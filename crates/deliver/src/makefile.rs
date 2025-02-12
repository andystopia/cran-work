//! Basic testing ground for generating makefiles which
//! can install R packages. I don't think there'd be a great
//! way to build a robust makefile for installation in general.
//! But makefile make -j8 gives instant parallelism, so I can
//! see how parallelism affects R install performance.

use std::collections::HashSet;

use miette::IntoDiagnostic;

use crate::{workspace_dep_tree, PackageAndDeps, ProgramCommand};

use super::Workspace;

pub(crate) struct MakefileTarget {
    pub(crate) prereqs: Vec<String>,
    pub(crate) target: String,
    pub(crate) commands: Vec<String>,
}

#[derive(Default)]
pub(crate) struct AbstractMakefile {
    pub(crate) variables: Vec<(String, String)>,
    pub(crate) targets: Vec<MakefileTarget>,
}

impl AbstractMakefile {
    pub fn push_target(&mut self, target: MakefileTarget) {
        self.targets.push(target);
    }
    pub fn push_variable(&mut self, varname: String, varvalue: String) {
        self.variables.push((varname, varvalue));
    }

    pub fn write<IO: std::io::Write>(&self, w: &mut IO) -> std::io::Result<()> {
        for (varname, varvalue) in &self.variables {
            writeln!(w, "{varname}={varvalue}")?;
        }

        write!(w, "\n\n")?;
        for MakefileTarget {
            prereqs,
            target,
            commands,
        } in &self.targets
        {
            writeln!(w, "{target}: {}", prereqs.join(" "))?;

            for command in commands {
                writeln!(w, "\t{command}")?;
            }

            writeln!(w)?;
        }

        Ok(())
    }
}

pub(crate) async fn generate_makefile(workspace: &Workspace) -> miette::Result<String> {
    let rattler_pkgs = [
        // we want gfortran and clang and some
        // compilation tools included in the environment
        // just to build a "fuzzy" build enviornment
        "compilers",
        // sed is a pretty common dependency, and it's
        // bog standard, so we might as well include it
        // in the distribution.
        "sed",
        // zlib, because well zlib is everywhere, and R
        // really likes to think that zlib is everywhere.
        "zlib",
        // pkg-config, well because building packages is hard without it
        "pkg-config",
        // building like it's 1979!
        "make",
        // R itself :)
        "r-base",
        // and openblas, gotta go fast!
        "openblas",
    ];

    let mut makefile = AbstractMakefile::default();

    makefile.push_variable(
        "PATH".to_owned(),
        "$(pwd)/.prefix/bin:/usr/bin/:/bin".to_owned(),
    );

    makefile.push_variable(
        "CPPFLAGS?".to_owned(),
        "CPPFLAGS=\"-Wl,-rpath,$(pwd)/.prefix/lib\"".to_owned(),
    );

    let mut all_dependencies = HashSet::new();
    let mut is_dependency = HashSet::new();
    let mut recommended_packages = HashSet::new();

    let rattler_target = MakefileTarget {
        prereqs: Vec::new(),
        target: "create-rattler-env".to_owned(),
        commands: vec![format!("rattler create {}", rattler_pkgs.join(" "))],
    };

    makefile.push_target(rattler_target);

    let package_and_deps = workspace_dep_tree(workspace).await?;

    // we really want to build recommended packages first.
    for PackageAndDeps {
        is_recommended,
        package_name,
        ..
    } in &package_and_deps
    {
        if *is_recommended {
            recommended_packages.insert(package_name.clone());
        }
    }

    for PackageAndDeps {
        package_name,
        package_deps,
        requires_build,
        is_recommended,
    } in package_and_deps
    {
        all_dependencies.insert(package_name.clone());

        is_dependency.extend(package_deps.clone());
        let target = MakefileTarget {
            // we don't want to depend *exactly* on a recommended package, during
            // building, rather let's depend on the whole set of recommended-packages always,
            // because so many R dependencies have a soft dependency on the recommended
            // set. So we will ensure that those are built and installed first and foremost, and then
            // we'll deal with installing the other packages later.
            prereqs: package_deps
                .into_iter()
                .filter(|v| {
                    !recommended_packages.contains(v)
                        || recommended_packages.contains(&package_name)
                })
                .chain(Some("recommended-packages".to_owned()).filter(|_| !is_recommended))
                .collect(),
            target: package_name.clone(),
            commands: vec![ProgramCommand::new("R")
                .with_args(&["CMD", "INSTALL", &format!("./{package_name}")])
                .bool_flag(requires_build, "--build")
                .value_flag("--library", "../../.builts/")
                .to_string()],
        };

        makefile.push_target(target)
    }

    makefile.push_target(MakefileTarget {
        prereqs: recommended_packages.into_iter().collect(),
        target: "recommended-packages".to_owned(),
        commands: vec![],
    });

    let no_reverse_deps = all_dependencies
        .difference(&is_dependency)
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    makefile.push_target(MakefileTarget {
        prereqs: no_reverse_deps,
        target: "install".to_owned(),
        commands: vec![],
    });

    makefile.push_target(MakefileTarget {
        prereqs: all_dependencies.into_iter().collect::<Vec<_>>(),
        target: ".PHONY".to_owned(),
        commands: Vec::new(),
    });

    let mut makefile_buffer = Vec::new();
    makefile.write(&mut makefile_buffer).into_diagnostic()?;

    Ok(String::from_utf8_lossy(&makefile_buffer).into_owned())
}
