//! Parses CRAN DESCRIPTION files into Rattler Recipes.
//! A great deal of this code was already written by the rattler
//! community over in github.com/prefix-dev/rattler-build, and I've
//! copied and pasted a great deal of it. However, I'm basically adapting
//! it to fit my input sources, as it's really handy being able to support
//! more than the r-universe package set, such as GitHub/Lab/other remotes,
//! and CRAN packages that haven't made their way into the r universe yet.
//!
//! This crate seeks to unify the R building world, and move away from
//! external dependencies, allowing for a more rust-y and contained build
//! ecosystem.
use std::collections::{HashMap, HashSet};

use clap::Parser;
use cran_description_file_parser::{strip_indents, RVersion, VersionConstraint};
use crancherry::{Cran, CranPackage};
use miette::{miette, IntoDiagnostic};
use rattler_digest::{compute_bytes_digest, Sha256};
use serialize::{ScriptTest, Test};

mod serialize;

fn map_license(license: &str) -> (Option<String>, Option<String>) {
    let license_replacements: HashMap<&str, &str> = [
        ("GPL-3", "GPL-3.0-only"),
        ("GPL-2", "GPL-2.0-only"),
        ("GPL (>= 3)", "GPL-3.0-or-later"),
        ("GPL (>= 3.0)", "GPL-3.0-or-later"),
        ("GPL (>= 2)", "GPL-2.0-or-later"),
        ("GPL (>= 2.0)", "GPL-2.0-or-later"),
        ("GPL (== 3)", "GPL-3.0-only"),
        ("GPL (== 2)", "GPL-2.0-only"),
        ("LGPL-3", "LGPL-3.0-only"),
        ("LGPL-2", "LGPL-2.0-only"),
        ("LGPL-2.1", "LGPL-2.1-only"),
        ("LGPL (>= 3)", "LGPL-3.0-or-later"),
        ("LGPL (>= 2)", "LGPL-2.0-or-later"),
        ("LGPL (>= 2.1)", "LGPL-2.1-or-later"),
        ("BSD_3_clause", "BSD-3-Clause"),
        ("BSD_2_clause", "BSD-2-Clause"),
        ("Apache License (== 2.0)", "Apache-2.0"),
        ("Apache License 2.0", "Apache-2.0"),
        ("MIT License", "MIT"),
        ("CC0", "CC0-1.0"),
        ("CC BY 4.0", "CC-BY-4.0"),
        ("CC BY-NC 4.0", "CC-BY-NC-4.0"),
        ("CC BY-SA 4.0", "CC-BY-SA-4.0"),
        ("AGPL-3", "AGPL-3.0-only"),
        ("AGPL (>= 3)", "AGPL-3.0-or-later"),
        ("EPL", "EPL-1.0"),
        ("EUPL", "EUPL-1.1"),
        ("Mozilla Public License 1.0", "MPL-1.0"),
        ("Mozilla Public License 2.0", "MPL-2.0"),
    ]
    .iter()
    .cloned()
    .collect();

    // Split the license string at '|' to separate licenses
    let parts: Vec<&str> = license.split(&['|', '+']).map(str::trim).collect();

    let mut final_licenses = Vec::new();
    let mut license_file = None;

    for part in parts {
        if part.to_lowercase().contains("file") {
            // This part contains the file specification
            license_file = part.split_whitespace().last().map(|s| s.to_string());
        } else {
            // This part is a license
            let mapped = license_replacements.get(part).map_or(part, |&s| s);
            final_licenses.push(mapped.to_string());
        }
    }

    let final_license = if final_licenses.is_empty() {
        None
    } else {
        Some(final_licenses.join(" OR "))
    };

    (final_license, license_file)
}

const R_BUILTINS: &[&str] = &[
    "base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "utils",
];

pub struct PackageFields<'a>(Vec<cran_description_file_parser::Field<'a>>);

impl PackageFields<'_> {
    pub fn get_field(&self, key: &str) -> Option<&&str> {
        self.0.iter().find_map(|f| match f {
            cran_description_file_parser::Field::Any { key: k, value } if k == &key => Some(value),
            _ => None,
        })
    }

    pub fn name(&self) -> Option<&&str> {
        self.0.iter().find_map(|el| match el {
            cran_description_file_parser::Field::Package(d) => Some(d),
            _ => None,
        })
    }

    pub fn description(&self) -> Option<&&str> {
        self.0.iter().find_map(|el| match el {
            cran_description_file_parser::Field::Description(d) => Some(d),
            _ => None,
        })
    }

    pub fn title(&self) -> Option<&&str> {
        self.0.iter().find_map(|el| match el {
            cran_description_file_parser::Field::Title(d) => Some(d),
            _ => None,
        })
    }

    pub fn version(&self) -> Option<&RVersion> {
        self.0.iter().find_map(|el| match el {
            cran_description_file_parser::Field::Version(d) => Some(d),
            _ => None,
        })
    }

    pub fn license(&self) -> Option<&&str> {
        self.0.iter().find_map(|el| match el {
            cran_description_file_parser::Field::License(d) => Some(d),
            _ => None,
        })
    }

    pub fn needs_compilation(&self) -> Option<bool> {
        self.0.iter().find_map(|el| match el {
            cran_description_file_parser::Field::Any {
                key: "NeedsCompilation",
                value: "yes",
            } => Some(true),
            cran_description_file_parser::Field::Any {
                key: "NeedsCompilation",
                value: "no",
            } => Some(false),
            _ => None,
        })
    }
}
pub enum DepRole {
    Depends,
    Imports,
    Suggests,
    LinkingTo,
}

pub struct Dep<'a> {
    dep: cran_description_file_parser::Dependency<'a>,
    role: DepRole,
}

fn make_unique(deps: Vec<String>) -> Vec<String> {
    let mut output_vec = Vec::new();
    let mut deps_seen: HashSet<String> = HashSet::new();

    for dep in deps {
        if deps_seen.contains(&dep) {
            continue;
        }

        deps_seen.insert(dep.clone());
        output_vec.push(dep);
    }

    output_vec
}

/// Generate rattler-build recipes straight from
/// the CRAN, by utilizing the DESCRIPTION file.
#[derive(Parser, Debug)]
pub enum Cli {
    /// Generate a rattler-build recipe from
    /// a CRAN package. Old versions supported too!
    GenerateCran {
        /// name of the package on the CRAN.
        package_name: String,
        /// (OPTIONAL) version of the package
        package_version: Option<String>,

        /// (OPTIONAL) URL to the CRAN itself, defaults to r-project.
        cran_url: Option<String>,
        #[clap(long)]
        /// write the recipe to disk in the forge format.
        export: bool,
    },

    /// print the DESCRIPTION file of a
    /// package on the CRAN.
    PrintDescription {
        package_name: String,
        package_version: Option<String>,
        /// (OPTIONAL) URL to the CRAN itself, defaults to r-project.
        cran_url: Option<String>,
    },
}

fn format_r_package(package_name: &str) -> String {
    format!("r-{}", package_name.to_lowercase())
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    match cli {
        Cli::GenerateCran {
            package_name,
            package_version,
            cran_url,
            export,
        } => {
            let final_recipe = generate_recipe(
                &package_name,
                package_version.as_deref(),
                cran_url.as_deref(),
            )?;

            if export {
                let package_dir = format!("r-{}", package_name.to_lowercase());
                fs_err::create_dir(&package_dir).into_diagnostic()?;
                fs_err::write(format!("{package_dir}/recipe.yaml"), final_recipe)
                    .into_diagnostic()?;
            } else {
                println!();
                println!("   Rattler Recipe for {}    ", package_name);
                println!("==================================");
                println!("{}", final_recipe);
            }
        }
        Cli::PrintDescription {
            package_name,
            package_version,
            cran_url,
        } => print_description(
            &package_name,
            package_version.as_deref(),
            cran_url.as_deref(),
        )?,
    }

    Ok(())
}

fn print_description(
    package: &str,
    version: Option<&str>,
    cran_url: Option<&str>,
) -> miette::Result<()> {
    let cran = match cran_url {
        Some(url) => Cran::with_url(url.to_owned()),
        None => Cran::new(),
    };

    let cran_package = match version {
        Some(version) => CranPackage {
            name: package.to_string().into(),
            version: crancherry::RVersion::from_str(version),
        },
        None => {
            eprintln!("Figuring out most recent version of {}", package);
            cran.most_recent_version_of(package)?
        }
    };

    eprintln!(
        "Most recent version of {} is {}",
        package, cran_package.version
    );

    eprintln!(
        "Figuring out download url for {}@{}",
        cran_package.name, cran_package.version
    );
    let pkg_download = cran.download_url_for(&cran_package.name, &cran_package.version)?;
    eprintln!("Downloading {}...", package);
    let description_file = pkg_download.download_decompressed()?.description_file()?;
    let description_file = description_file.contents();
    eprintln!("Finished downloading {}...", package);

    println!();

    println!("   DESCRIPTION file for {}    ", package);
    println!("==================================");
    println!("{}", description_file);
    Ok(())
}

fn generate_recipe(
    target_package: &str,
    version: Option<&str>,
    cran_url: Option<&str>,
) -> miette::Result<String> {
    let cran = match cran_url {
        Some(url) => Cran::with_url(url.to_owned()),
        None => Cran::new(),
    };

    let cran_package = match version {
        Some(version) => CranPackage {
            name: target_package.to_string().into(),
            version: crancherry::RVersion::from_str(version),
        },
        None => {
            eprintln!("Figuring out most recent version of {}", target_package);
            cran.most_recent_version_of(target_package)?
        }
    };

    eprintln!(
        "Most recent version of {} is {}",
        target_package, cran_package.version
    );

    eprintln!(
        "Figuring out download url for {}@{}",
        cran_package.name, cran_package.version
    );
    let pkg_download = cran.download_url_for(&cran_package.name, &cran_package.version)?;
    eprintln!("Downloading {}...", target_package);
    let downloaded_bytes = pkg_download.download_uncompressed_vec()?;
    eprintln!("Finished downloading {}...", target_package);
    let digest = compute_bytes_digest::<Sha256>(&downloaded_bytes.compressed);
    let mut pkg = downloaded_bytes.into_compressed_package()?;
    eprintln!("Parsing description field {}...", target_package);
    let res = pkg.description_file()?;
    let res = res.parse()?;
    let package = PackageFields(res);
    let mut recipe = serialize::Recipe::default();
    eprintln!("Generating recipe for {}...", target_package);
    recipe.package.name = format_r_package(
        package
            .name()
            .ok_or(miette!("No package name found in DESCRIPTION manifest"))?,
    );
    recipe.package.version = package
        .version()
        .ok_or(miette!("No package version found in DESCRIPTION manifest"))?
        .components
        .join(".");
    let source = serialize::SourceElement {
        url: pkg_download.url,
        sha256: Some(format!("{:x}", digest)),
        md5: None,
    };
    recipe.source.push(source);
    recipe.build.script = "R CMD INSTALL --build .".to_owned();
    let build_requirements = vec![
        "${{ compiler('c') }}".to_string(),
        "${{ compiler('cxx') }}".to_string(),
        "make".to_string(),
    ];
    let needs_compilation = package.needs_compilation().unwrap_or_default();
    if needs_compilation {
        recipe.requirements.build.extend(build_requirements.clone());
    }
    recipe.requirements.host = vec!["r-base".to_string()];
    recipe.requirements.run = vec!["r-base".to_string()];
    let dependencies = package
        .0
        .iter()
        .filter_map(|el| match el {
            cran_description_file_parser::Field::Depends(d) => Some(
                d.iter()
                    .map(|dep| Dep {
                        dep: dep.to_owned(),
                        role: DepRole::Depends,
                    })
                    .collect::<Vec<_>>(),
            ),
            cran_description_file_parser::Field::Imports(d) => Some(
                d.iter()
                    .map(|dep| Dep {
                        dep: dep.to_owned(),
                        role: DepRole::Imports,
                    })
                    .collect::<Vec<_>>(),
            ),
            cran_description_file_parser::Field::Suggests(d) => Some(
                d.iter()
                    .map(|dep| Dep {
                        dep: dep.to_owned(),
                        role: DepRole::Suggests,
                    })
                    .collect::<Vec<_>>(),
            ),
            cran_description_file_parser::Field::LinkingTo(d) => Some(
                d.iter()
                    .map(|dep| Dep {
                        dep: dep.to_owned(),
                        role: DepRole::LinkingTo,
                    })
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        })
        .flatten();
    let mut remaining_deps = HashSet::new();
    for Dep { dep, role } in dependencies {
        if R_BUILTINS.contains(&dep.name) {
            continue;
        }

        let dep_spec = match &dep.constraint {
            Some(VersionConstraint { ordering, version }) => {
                format!(
                    "{} {}{}",
                    format_r_package(dep.name),
                    ordering.as_str(),
                    version.components.join(".")
                )
            }
            None => format_r_package(dep.name),
        };

        if dep.name == "R" {
            let dep_spec = match &dep.constraint {
                Some(VersionConstraint { ordering, version }) => {
                    format!(
                        "r-base {}{}",
                        ordering.as_str(),
                        version.components.join(".")
                    )
                }
                None => format!("r-{}", dep.name),
            };

            recipe.requirements.host.push(dep_spec);
            continue;
        }

        match role {
            DepRole::Depends | DepRole::Imports => {
                recipe.requirements.run.push(dep_spec.clone());
                recipe.requirements.host.push(dep_spec.clone());
                remaining_deps.insert(dep_spec);
            }
            DepRole::Suggests => {
                recipe
                    .requirements
                    .run
                    .push(format!("SUGGEST {}", dep_spec.clone()));
            }
            DepRole::LinkingTo => {
                recipe.requirements.host.push(dep_spec);
                recipe.requirements.build.extend(build_requirements.clone());
            }
        }
    }
    recipe.requirements.host = make_unique(recipe.requirements.host);
    recipe.requirements.build = make_unique(recipe.requirements.build);
    recipe.requirements.run = make_unique(recipe.requirements.run);

    recipe.tests.push(Test::Script(ScriptTest {
        script: vec![format!("Rscript -e 'library(\"{}\")'", cran_package.name)],
    }));
    recipe.about.summary = package.title().map(|x| strip_indents(x));
    recipe.about.description = package.description().map(|x| strip_indents(x));
    (recipe.about.license, recipe.about.license_file) = package
        .license()
        .map(|x| map_license(x))
        .unwrap_or((None, None));
    // only take the first URL, as we'll 
    // assume that that is the homepage
    recipe.about.homepage = package
        .get_field("URL")
        .and_then(|&x| x.split(',').next().map(|x| x.to_string()));

    recipe.about.repository = Some(format!("https://github.com/cran/{}", cran_package.name));
    let recipe_str = format!("{}", recipe);
    let mut final_recipe = String::new();
    for line in recipe_str.lines() {
        if line.contains("SUGGEST") {
            final_recipe.push_str(&format!(
                "{}  # suggested\n",
                line.replace(" - SUGGEST", " # - ")
            ));
        } else {
            final_recipe.push_str(&format!("{}\n", line));
        }
    }
    Ok(final_recipe.trim().to_owned())
}
