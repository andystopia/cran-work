pub mod cran;
mod cran_index;
mod edit_project;
mod leveling_graph;
mod makefile;
mod parbuild;
mod toolchain;
mod tree;
mod version;

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf, MAIN_SEPARATOR},
    str::FromStr,
    sync::Arc,
};

use clap::Parser;
use cran::{
    CranDescriptionGrouped, CranPackagesFileDownload, CranRecommendedFileDownload,
    PackageRepoDownloader, PackageRepositoryUrl,
};
use cran_description_file_parser::{strip_indents, RVersion, VersionConstraint};
use cran_index::{load_cran_packages_file, save_cran_packages_file};
use leveling_graph::LevelingGraph;
use makefile::generate_makefile;
use miette::{bail, IntoDiagnostic};
use tokio::{sync::Semaphore, task::JoinSet};
use toolchain::ToolchainCli;

// to be nice to servers and to avoid a thundering
// herd problem, we're going to bottleneck how many
// concurrent requests to something reasonable. If
// a complex R project has 5000 deps, spawning 5000
// download requests to CRAN might not stress the server
// per se, but it'd be inefficient, and certainly
// could raise alarm bells over there. So go slow.
const MAX_CONCURRENT_DOWNLOAD_REQUESTS: usize = 16;
const STDLIB_PACKAGES: [&str; 16] = [
    "base",
    "compiler",
    "datasets",
    "grDevices",
    "graphics",
    "grid",
    "methods",
    "parallel",
    "profile",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "translations",
    "utils",
];

pub struct Workspace {
    path: std::path::PathBuf,
}

impl Workspace {
    pub fn manifest_mut(&self) -> miette::Result<toml_edit::DocumentMut> {
        let result = fs_err::read_to_string(self.manifest_path()).into_diagnostic()?;
        let doc = result.parse::<toml_edit::DocumentMut>().into_diagnostic()?;

        Ok(doc)
    }

    pub fn lockfile_path(&self) -> std::path::PathBuf {
        self.path().join("deliver.lock")
    }

    pub fn manifest_path(&self) -> std::path::PathBuf {
        self.path().join("deliver.lock")
    }

    pub fn lockfile_exists(&self) -> bool {
        self.lockfile_path().exists()
    }

    pub fn manifest_exists(&self) -> bool {
        self.manifest_path().exists()
    }

    pub fn lockfile_mut(&self) -> miette::Result<toml_edit::DocumentMut> {
        let result = fs_err::read_to_string(self.lockfile_path()).into_diagnostic()?;
        let doc = result.parse::<toml_edit::DocumentMut>().into_diagnostic()?;

        Ok(doc)
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        Self {
            path: path.as_ref().to_path_buf(),
        }
    }
}

async fn print_system_requirements(workspace_path: &Path) -> miette::Result<()> {
    let workspace = Workspace::from_path(workspace_path);

    if !workspace.lockfile_exists() {
        lock(workspace.path().to_owned()).await?;
    }
    let lockfile = workspace.lockfile_mut()?;
    for package in lockfile["packages"].as_array_of_tables().unwrap() {
        let package_name = package["name"].as_str().unwrap();

        // base packages aren't vendored in, so they don't have
        // accessible description files, so we can't really
        // tell ahead of time what dependencies they are intended
        // to be built with.
        if STDLIB_PACKAGES.contains(&package_name) {
            continue;
        }

        let package_description_file_path = workspace
            .path()
            .join("deliver-r")
            .join(".sources")
            .join("packages")
            .join(package_name)
            .join("DESCRIPTION");

        let description_file = CranPackagesFileDownload::from_cached(
            fs_err::read_to_string(package_description_file_path).into_diagnostic()?,
        );
        let parsed = description_file.parse()?;

        let system_requirements = parsed
            .assume_single()
            .system_requirements()
            .map(|x| strip_indents(x).replace("\n", ""))
            .unwrap_or_default();

        println!("{package_name}: {system_requirements}");
    }
    Ok(())
}
async fn print_build_order(workspace_path: &Path) -> miette::Result<()> {
    let workspace = Workspace::from_path(workspace_path);

    let (needs_build, leveled) = get_build_order(&workspace).await?;

    let leveled = leveled
        .into_iter()
        .map(|pkgs| {
            let mut pkgs = pkgs
                .into_iter()
                .map(|p| {
                    if needs_build.contains(&p) {
                        format!("{} [b]", p.to_owned())
                    } else {
                        p.to_owned()
                    }
                })
                .collect::<Vec<_>>();
            pkgs.sort_unstable_by_key(|v| v.to_lowercase());
            pkgs
        })
        .collect::<Vec<_>>();

    let largest_leveled_step = leveled.iter().map(|v| v.len()).max().unwrap();

    let layers_longest_names = leveled
        .iter()
        .map(|v| v.iter().map(|pkg_name| pkg_name.len()).max().unwrap())
        .collect::<Vec<_>>();

    for dep_idx in 0..largest_leveled_step {
        print!("│ ");
        for level in 0..leveled.len() {
            let lev = &leveled[level];

            let spaces = layers_longest_names[level];
            if dep_idx < lev.len() {
                let name = &lev[dep_idx];
                print!("{} {}│ ", name, " ".repeat(spaces - name.len()));
            } else {
                print!(" {}│ ", " ".repeat(spaces));
            }
        }
        println!();
    }
    Ok(())
}

pub struct ProgramCommand {
    command: String,
    arguments: Vec<String>,
}

impl ProgramCommand {
    pub fn new(command: &str) -> Self {
        Self {
            command: command.to_string(),
            arguments: Vec::new(),
        }
    }

    pub fn with_arg(&mut self, argument: &str) -> &mut Self {
        self.arguments.push(argument.to_owned());
        self
    }

    pub fn with_args<'s, S: AsRef<[&'s str]>>(&mut self, args: S) -> &mut Self {
        self.arguments
            .extend(args.as_ref().iter().map(|x| (*x).to_owned()));
        self
    }

    pub fn bool_flag(&mut self, cond: bool, argument: &str) -> &mut Self {
        if cond {
            self.with_arg(argument);
        }
        self
    }

    pub fn value_flag(&mut self, name: &str, argument: &str) -> &mut Self {
        self.with_arg(&format!("{name}={argument}"));
        self
    }

    pub fn to_string(&mut self) -> String {
        format!("{} {}", self.command, self.arguments.join(" "))
    }
}

async fn print_build_script(workspace_path: &Path) -> miette::Result<()> {
    let workspace = Workspace::from_path(workspace_path);

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

    // fail early, fail fast, we're not in the
    // business of half working sofware.
    println!("#!/usr/bin/env bash\n");
    println!("set -Eeuo pipefail");

    println!("rattler create {}", rattler_pkgs.join(" "));
    println!(r#"export PATH="$(pwd)/.prefix/bin:$PATH""#);

    let (requires_build, build_steps) = get_build_order(&workspace).await?;

    let build_steps = build_steps
        .into_iter()
        .map(|v| {
            let mut as_vec = v.into_iter().collect::<Vec<_>>();
            as_vec.sort_by_key(|v| v.to_lowercase());
            as_vec
        })
        .collect::<Vec<_>>();

    for (i, build_step) in build_steps.into_iter().enumerate() {
        println!(
            "\n# ~~~ BUILD WAVE {:<03} ({} {})~~~ ",
            i,
            build_step.len(),
            if build_step.len() == 1 {
                "package"
            } else {
                "packages"
            }
        );
        for pkg_build in build_step {
            let req_build = requires_build.contains(&pkg_build);

            println!(r#"echo """#);

            println!(
                r#"echo "$(tput bold)$(tput setab 255)$(tput setaf 0) {} $(tput setab 91)$(tput setaf 7) {pkg_build} $(tput sgr0)" "#,
                if req_build { "Compiling" } else { "Building" }
            );

            let prog_cmd = ProgramCommand::new("R")
                .with_args(&["CMD", "INSTALL", &format!("./{pkg_build}")])
                .bool_flag(req_build, "--build")
                .value_flag("--library", "../../.builts/")
                .to_string();
            println!("{}", prog_cmd);
        }
    }
    Ok(())
}

pub struct PackageAndDeps {
    package_name: String,
    package_deps: HashSet<String>,
    requires_build: bool,
    is_recommended: bool,
}

async fn workspace_dep_tree(workspace: &Workspace) -> miette::Result<Vec<PackageAndDeps>> {
    if !workspace.lockfile_exists() {
        lock(workspace.path().to_owned()).await?;
    }
    let lockfile = workspace.lockfile_mut()?;
    let mut packages_and_deps = Vec::new();
    for package in lockfile["packages"].as_array_of_tables().unwrap() {
        let package_name = package["name"].as_str().unwrap();
        let Some(package_dependencies) = package.get("dependencies") else {
            continue;
        };

        let package_dependencies = package_dependencies
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_owned())
            .filter(|v| !STDLIB_PACKAGES.contains(&v.as_str()))
            .collect();

        let requires_build = package
            .get("needs-compilation")
            .and_then(|v| v.as_bool())
            .unwrap_or_default();

        let is_recommended = package.get("recommended-for").is_some();

        packages_and_deps.push(PackageAndDeps {
            package_name: package_name.to_owned(),
            package_deps: package_dependencies,
            requires_build,
            is_recommended,
        })
    }

    Ok(packages_and_deps)
}

async fn get_build_order(
    workspace: &Workspace,
) -> miette::Result<(HashSet<String>, Vec<HashSet<String>>)> {
    if !workspace.lockfile_exists() {
        lock(workspace.path().to_owned()).await?;
    }
    let mut dependency_leveling_graph = LevelingGraph::new();
    let mut needs_build = HashSet::new();
    let lockfile = workspace.lockfile_mut()?;
    for package in lockfile["packages"].as_array_of_tables().unwrap() {
        let package_name = package["name"].as_str().unwrap();
        let Some(package_dependencies) = package.get("dependencies") else {
            continue;
        };

        let package_dependencies = package_dependencies
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_owned())
            .filter(|v| !STDLIB_PACKAGES.contains(&v.as_str()))
            .collect();

        let requires_build = package
            .get("needs-compilation")
            .and_then(|v| v.as_bool())
            .unwrap_or_default();

        if requires_build {
            needs_build.insert(package_name.to_owned());
        }

        dependency_leveling_graph.insert(package_name.to_owned(), package_dependencies);
    }
    let leveled = dependency_leveling_graph.into_leveled();
    Ok((needs_build, leveled))
}
async fn download_lockfile_packages(workspace_path: &Path) -> miette::Result<()> {
    let workspace = Workspace::from_path(workspace_path);

    // generate a lockfile if it doesn't already exist
    if !workspace.lockfile_exists() {
        lock(workspace_path.to_owned()).await?;
    }

    let lockfile = workspace.lockfile_mut()?;

    let downloader = PackageRepoDownloader::new();

    // start by getting the package repositories, which are
    // enumerated in the lockfile, so that we can fetch the
    // packages later
    let package_repos = lockfile["package-repositories"]
        .as_table()
        .expect("package-repositories in the lockfile should be table");

    let package_repos = package_repos
        .iter()
        .map(|(k, v)| {
            (
                k,
                PackageRepositoryUrl::new(
                    v.as_str()
                        .expect("package repository urls should be strings"),
                ),
            )
        })
        .collect::<HashMap<_, _>>();

    let package_index_files =
        fetch_repository_package_indexes(&package_repos, &workspace, downloader.clone()).await?;

    let mut parsed = HashMap::new();

    // we can't iterate over package_index_files, because
    // it's an elsa frozen collection, but we can iterate
    // over the package repos, to get at the keys, so we're
    // just going to do that of course.

    for name in package_repos.keys() {
        let index = &package_index_files[name.to_owned()];
        let parse = index.parse()?;

        let grouped = parse.group_by_package();

        parsed.insert(*name, grouped);
    }

    let mut download_objects = Vec::new();

    for package in lockfile["packages"]
        .as_array_of_tables()
        .expect("lockfile should have a packages section")
    {
        // skip packages which are part of the stdlib
        // we really wouldn't want to download those packages
        if package
            .get("stdlib")
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
        {
            continue;
        }
        // TODO: generate spanned errors that point to
        // the toml file in order to indicate exactly what
        // went wrong during resolution.
        let package_name = package["name"]
            .as_str()
            .expect("every package should have a name");
        let package_repo_name = package["repository"]
            .as_str()
            .expect("every package should have a repository key");

        let package_version = RVersion::from_string(
            package["version"]
                .as_str()
                .expect("every package should have a version key"),
        );

        let package_repository_url = package_repos[package_repo_name].clone();

        let index = &parsed[package_repo_name];

        download_objects.push(downloader.package_resource(
            &package_repository_url,
            package_name,
            &package_version,
            index,
        ));
    }

    download_all_packages(
        download_objects.clone(),
        &workspace,
        MAX_CONCURRENT_DOWNLOAD_REQUESTS,
    )
    .await?;

    Ok(())
}

/// downloads a collection of packages
///
/// attempts to do so concurrently
async fn download_all_packages(
    download_objects: Vec<cran::PackageDownload>,
    workspace: &Workspace,
    max_concurrent_download_requests: usize,
) -> Result<(), miette::Error> {
    let download_limit = Arc::new(Semaphore::new(max_concurrent_download_requests));
    let mut joinset = JoinSet::new();

    let mut recv_chans = Vec::new();
    for download_package in download_objects {
        let workspace_path = workspace.path().to_path_buf();
        let download_limit = download_limit.clone();

        let (send_chan, recv_chan) = tokio::sync::oneshot::channel();

        joinset.spawn(async move {
            let _sem = download_limit.acquire().await.into_diagnostic()?;
            println!("start downloading {}...", download_package.name());
            let res = download_package.download(&workspace_path).await;
            println!("finished downloading {}...", download_package.name());
            let res = match res {
                Ok(_md5) => {
                    rayon::spawn(move || {
                        println!("decompressing {}", download_package.name());
                        send_chan
                            .send(download_package.decompress(&workspace_path))
                            .unwrap();
                        println!("decompressed {}", download_package.name());
                    });

                    Ok(())
                }
                Err(err) => Err(err),
            };
            drop(_sem);

            res
        });

        recv_chans.push(recv_chan);
    }

    // aggregate errors from the downloads
    while let Some(download_res) = joinset.join_next().await {
        // throw up the errors, and we're done
        download_res.into_diagnostic()??;
    }

    // now aggregate the errors from the decompression
    for decompress_res in recv_chans.drain(..) {
        decompress_res.await.into_diagnostic()??;
    }

    Ok(())
}

/// Fetches the PACKAGES.gz file from a package archive
///
/// Will load from a cache if the etags returned from the server
/// indicate that the cache is still valid. The downloader
/// is just a client which stores state for how to interact with
/// servers and is shared amongst all the endpoints.
///
/// This function intends to concurrently download all the packages
/// files, so that 50 repositories will bottleneck on the incoming
/// datastream instead of being bottlenecked on any individual repositories
/// server speed.

async fn fetch_repository_package_indexes(
    package_repos: &HashMap<&str, PackageRepositoryUrl>,
    workspace: &Workspace,
    downloader: PackageRepoDownloader,
) -> miette::Result<elsa::FrozenMap<String, Box<CranPackagesFileDownload>>> {
    let mut joinset = JoinSet::new();
    for (name, repo_url) in package_repos {
        let workspace_path = workspace.path.clone();
        let downloader = downloader.clone();
        let repo_url = repo_url.clone();
        let name = (*name).to_owned();
        // we box the cran package index file becuase
        // for the cost of one single allocation we
        // can solve having to clone it a bunch of
        // times to parse it, this essentially just
        // resolves our lifetime headaches for the cost
        // of one indirection.
        joinset.spawn(async move {
            maybe_cached_cran_packages_file(&repo_url, &downloader, &workspace_path)
                .await
                .map(|v| (name, Box::new(v)))
        });
    }
    let all_joined = joinset.join_all().await;
    all_joined
        .into_iter()
        .collect::<Result<elsa::FrozenMap<_, _>, _>>()
}

fn print_dependency_tree(
    package_name: &str,
    cran_grouped: &cran::CranDescriptionGrouped,
    prefix: &str,
) {
    let Some(pkg) = cran_grouped.get_package(package_name) else {
        return;
    };

    let depends = pkg.required_dependencies().into_iter().collect::<Vec<_>>();

    let depends_len = depends.len();

    for (i, dep) in depends.into_iter().enumerate() {
        let infix = if i + 1 == depends_len { " " } else { "|" };
        println!("{}+- {}", prefix, dep.name);
        print_dependency_tree(&dep.name, cran_grouped, &format!("{}{}  ", prefix, infix));
    }
}

#[derive(Debug, Parser)]
pub enum Command {
    Init {
        #[clap(long)]
        /// create an RStudio project
        rstudio: bool,
    },
    New {
        package_name: String,
        rstudio: bool,
    },
    Add {
        dependency: String,
        workspace: Option<PathBuf>,
    },
    Tree {
        workspace: Option<PathBuf>,
    },
    Lock {
        workspace: Option<PathBuf>,
    },
    DownloadSinglePackage {
        package_name: String,
    },
    /// download all dependencies, and
    /// save them into the package.
    VendorDeps {
        workspace: Option<PathBuf>,
    },
    ShowBuildSteps {
        workspace: Option<PathBuf>,
    },
    GenerateIndex,
    PrintBuildScript {
        workspace: Option<PathBuf>,
    },
    PrintMakefile {
        workspace: Option<PathBuf>,
    },
    PrintSystemRequirements {
        workspace: Option<PathBuf>,
    },
    Toolchain {
        #[command(subcommand)]
        args: ToolchainCli,
    },
}

pub fn get_dependency_names<'a>(
    grouped: &CranDescriptionGrouped<'a>,
    name: &str,
    set: &mut HashSet<&'a str>,
) {
    let Some(pkg) = grouped.get_package(name) else {
        return;
    };

    let deps = pkg.required_dependencies();

    for dep in deps {
        set.insert(dep.name);
        get_dependency_names(grouped, dep.name, set);
    }
}

pub fn version_constraint_pretty(vc: &VersionConstraint) -> String {
    let prefix = vc.ordering.as_str();

    format!("{} {}", prefix, vc.version)
}
pub fn get_dependency_constraints<'a>(
    grouped: &CranDescriptionGrouped<'a>,
    name: &str,
    set: &mut HashMap<String, version::PackageBoundsConstraint<'a>>,
) {
    let Some(package) = grouped.get_package(name) else {
        return;
    };

    let dependencies = package.required_dependencies();

    for dep in dependencies {
        let item = set
            .entry(dep.name.to_owned())
            .or_insert_with(version::PackageBoundsConstraint::new);
        if let Some(constraint) = &dep.constraint {
            item.subset(constraint);
        }
        get_dependency_constraints(grouped, dep.name, set);
    }
}

pub async fn lock(workspace: PathBuf) -> miette::Result<()> {
    let manifest_file_path = workspace.join("deliver.toml");
    let manifest_file = fs_err::read_to_string(&manifest_file_path).into_diagnostic()?;
    let manifest = toml_edit::ImDocument::from_str(&manifest_file).into_diagnostic()?;

    let rversion = manifest["package"]["RVersion"]
        .as_str()
        .expect("deliver.toml must have an RVersion field");

    // see if a repository is listed in the workspace manifest
    // otherwise, we'll default to
    // using the r-project CRAN.
    let repository_url = manifest["package"]
        .get("default-repository")
        .and_then(|v| v.as_str())
        .unwrap_or("https://cran.r-project.org");

    let cran_url = PackageRepositoryUrl::new(&repository_url);
    let downloader = PackageRepoDownloader::new();

    let parsed = downloader
        .download_recommended_packages_page(&cran_url, rversion)
        .await
        .into_diagnostic()?
        .parsed();

    let recommended_package_versions = parsed.parse_package_listing();

    let decompressed = maybe_cached_cran_packages_file(&cran_url, &downloader, &workspace).await?;

    let parsed = decompressed.parse()?;
    let grouped = parsed.group_by_package();

    // download the recommended package versions to the
    // local machine, so that they can have their
    // MD5 hashes computed for the lockfile.

    // TODO: this is an embarrassingly parallel download
    //       and decompression operation, we should first
    //       download parallel and concurrently and then decompress
    //       in parallel.

    println!("Recommended {:#?}", recommended_package_versions);

    let mut recommended_package_hashes = HashMap::new();
    for (package_name, package_version) in &recommended_package_versions {
        println!("Downloading {package_name}");
        let download = downloader.from_recommended(
            &cran_url,
            rversion,
            (*package_name).to_owned(),
            package_version.to_string().as_str(),
        );

        let hash = download.download(&workspace).await?;
        println!("Downloaded {package_name}");

        recommended_package_hashes.insert(package_name.to_string(), hash);
        println!("Decompressing {package_name}");
        download.decompress(&workspace)?;
        println!("Decompressed {package_name}");
    }

    let table = manifest["dependencies"]
        .as_table()
        .expect("dependencies should be a table");

    let mut all_deps = HashSet::new();

    let mut dep_constraints = HashMap::<String, _>::new();

    for (dep_name, it) in table {
        let v = it.as_str().unwrap();

        let rver = RVersion::from_string(v);
        get_dependency_names(&grouped, dep_name, &mut all_deps);
        dep_constraints.insert(
            dep_name.to_owned(),
            version::PackageBoundsConstraint::create_only(rver),
        );
    }

    let downloads = recommended_package_versions
        .iter()
        .map(|(package_name, _)| {
            CranRecommendedFileDownload::from_downloaded(&workspace, package_name)
        })
        .collect::<std::io::Result<Vec<_>>>()
        .into_diagnostic()?;

    for (recommended, recommended_version) in &recommended_package_versions {
        dep_constraints.insert((*recommended).to_owned(), {
            let mut pbc = version::PackageBoundsConstraint::new();
            pbc.only(recommended_version.clone());
            pbc
        });
    }

    let parsed = downloads
        .iter()
        .map(|d| d.parse())
        .collect::<miette::Result<Vec<_>>>()?;

    for parse in &parsed {
        let single_package = parse.assume_single();

        let dependencies = single_package.required_dependencies();

        for dep in dependencies {
            if all_deps.contains(&dep.name) {
                dep_constraints.insert(dep.name.to_owned(), {
                    let mut pbc = version::PackageBoundsConstraint::new();
                    if let Some(constraint) = &dep.constraint {
                        pbc.subset(&constraint);
                    }
                    pbc
                });
            }
        }
    }

    let parsed_recommended_map = recommended_package_versions
        .iter()
        .map(|(package_name, _)| package_name)
        .zip(parsed)
        .collect::<HashMap<_, _>>();

    for (dep_name, _) in table {
        get_dependency_constraints(&grouped, dep_name, &mut dep_constraints);
    }

    let mut dependency_constraints = dep_constraints.into_iter().collect::<Vec<_>>();

    dependency_constraints.sort_by_key(|(k, _)| k.to_lowercase());

    let mut lockfile = toml_edit::DocumentMut::new();

    lockfile.insert("RVersion", toml_edit::value(rversion));

    lockfile["package-repositories"] = toml_edit::table();
    lockfile["package-repositories"]["cran"] = toml_edit::value(cran_url.as_str());

    lockfile.insert("version", toml_edit::value(1));
    lockfile.insert("packages", toml_edit::array());

    for (pkg_name, constraint) in dependency_constraints {
        let mut table = toml_edit::Table::new();

        let satis = match grouped.get_package_version(&pkg_name) {
            Some(current_cran_package_version) => {
                if let Some(pkg_version) = recommended_package_versions
                    .iter()
                    .find(|(name, _)| *name == pkg_name)
                    .map(|(_, ver)| ver)
                {
                    table["name"] = toml_edit::value(pkg_name.clone());
                    table["version"] = toml_edit::value(&pkg_version.to_string());
                    let mut deps = toml_edit::Array::new();

                    let pkg = parsed_recommended_map
                        .get(&pkg_name.as_str())
                        .expect("package should be in the recommended section");

                    let pkg = pkg.assume_single();

                    let mut imports = pkg
                        .required_dependencies()
                        .into_iter()
                        .map(|dep| dep.name.to_owned())
                        .collect::<Vec<_>>();

                    // sort the imports, hopefully
                    // reducing diff sizes
                    imports.sort_unstable();

                    deps.extend(imports);

                    deps.fmt();

                    table["dependencies"] = toml_edit::Item::Value(toml_edit::Value::Array(deps));
                    table["needs-compilation"] = toml_edit::value(pkg.needs_compilation());
                    table["recommended-for"] = toml_edit::value("4.3.2");

                    if let Some(hash) = recommended_package_hashes.get(&pkg_name) {
                        table["md5-hash"] = toml_edit::value(&hash.to_string());
                    }

                    format!("(satisfied, from recommended section: {pkg_version})")
                } else if constraint.is_satisfied_by(&current_cran_package_version) {
                    table["name"] = toml_edit::value(pkg_name.clone());
                    table["version"] = toml_edit::value(current_cran_package_version.to_string());
                    let mut deps = toml_edit::Array::new();

                    let pkg = grouped
                        .get_package(&pkg_name)
                        .expect("cran should have this package");

                    let mut imports = pkg
                        .required_dependencies()
                        .into_iter()
                        .map(|dep| dep.name.to_owned())
                        .collect::<Vec<_>>();

                    // sort the imports, hopefully
                    // reducing diff sizes
                    imports.sort_unstable();

                    deps.extend(imports);

                    deps.fmt();

                    table["dependencies"] = toml_edit::Item::Value(toml_edit::Value::Array(deps));
                    table["needs-compilation"] = toml_edit::value(pkg.needs_compilation());

                    if let Some(md5) = pkg.md5() {
                        table["md5-hash"] = toml_edit::value(md5);
                    }

                    format!("(satisfiable by latest: {current_cran_package_version})")
                } else {
                    format!("unsatisfiable, current is {current_cran_package_version}")
                }
            }
            None => {
                if STDLIB_PACKAGES.contains(&pkg_name.as_str()) {
                    table["name"] = toml_edit::value(pkg_name.clone());
                    table["stdlib"] = toml_edit::value(true);
                    "(stdlib)".to_owned()
                } else {
                    "(unsatisfiable, not found in cran)".to_owned()
                }
            }
        };
        table["repository"] = toml_edit::value("cran");
        lockfile["packages"]
            .as_array_of_tables_mut()
            .unwrap()
            .push(table);
        println!("{} = \"{}\" -- {satis}", pkg_name, constraint);
    }

    fs_err::write(workspace.join("deliver.lock"), lockfile.to_string()).into_diagnostic()?;
    Ok(())
}

async fn maybe_cached_cran_packages_file(
    pkg_url: &PackageRepositoryUrl,
    downloader: &PackageRepoDownloader,
    workspace: &Path,
) -> miette::Result<CranPackagesFileDownload> {
    eprintln!("Getting packages file HEAD");
    let etag = downloader
        .packages_gz_etag(pkg_url.clone())
        .await
        .into_diagnostic()?
        .unwrap();
    eprintln!("Finished getting packages file HEAD");

    let cached_contents =
        load_cran_packages_file(&etag, pkg_url.domain(), workspace).into_diagnostic()?;

    match cached_contents {
        Some(cached_contents) => {
            eprintln!("Using cached packages file CONTENTS");
            Ok(CranPackagesFileDownload::from_cached(cached_contents))
        }
        None => {
            eprintln!("Downloading packages file CONTENTS");
            let downloaded = downloader
                .download_packages_file(pkg_url)
                .await
                .into_diagnostic()?;
            eprintln!("Finished Downloading packages file CONTENTS");
            eprintln!("Decompressing packages file");
            let decompressed = downloaded.decompress().into_diagnostic()?;
            eprintln!("Finished decompressing packages file");

            save_cran_packages_file(&etag, pkg_url.domain(), decompressed.contents(), workspace)
                .into_diagnostic()?;

            Ok(decompressed)
        }
    }
}

pub async fn tree_workspace(workspace: &Path) -> miette::Result<()> {
    let manifest_file_path = workspace.join("deliver.toml");

    let manifest_file = fs_err::read_to_string(&manifest_file_path).into_diagnostic()?;

    let document = toml_edit::ImDocument::from_str(&manifest_file).into_diagnostic()?;

    let pkg_url = "cran.r-project.org";
    let cran_url = PackageRepositoryUrl::new(&format!("https://{pkg_url}"));
    let downloader = PackageRepoDownloader::new();

    let decompressed = maybe_cached_cran_packages_file(&cran_url, &downloader, &workspace).await?;

    let parsed = decompressed.parse()?;
    let grouped = parsed.group_by_package();

    let table = document["dependencies"]
        .as_table()
        .expect("dependencies should be a table");

    for (dep_name, dep_qual) in table {
        let dep_name = dep_name;
        let dep_qual = dep_qual.as_str().unwrap();
        println!("+- {}@{}", dep_name, dep_qual);
        print_dependency_tree(dep_name, &grouped, "|  ");
    }

    Ok(())
}

async fn deliver() -> miette::Result<()> {
    let cli = Command::parse();

    match cli {
        Command::DownloadSinglePackage { package_name } => {
            let url = PackageRepositoryUrl::from_domain("cran.r-project.org");

            let downloader = PackageRepoDownloader::new();

            let pkgs_file = downloader
                .download_packages_file(&url)
                .await
                .into_diagnostic()?;

            let decompressed = pkgs_file.decompress().into_diagnostic()?;
            let parsed = decompressed.parse()?;
            let grouped = parsed.group_by_package();

            let Some(package_version) = grouped.get_package_version(&package_name) else {
                bail!("No package `{package_name}` found in the CRAN.");
            };

            let pkg_download_url = downloader.from_main(&url, &package_name, package_version);
            pkg_download_url
                .download_to_path(
                    &Path::new(".").join(format!("{package_name}_{package_version}.tar.gz")),
                )
                .await?;

            Ok(())
        }
        Command::Lock { workspace } => {
            let workspace = workspace.unwrap_or("tests/first-example/".to_owned().into());
            lock(workspace).await
        }
        Command::Tree { workspace } => {
            let workspace = workspace.unwrap_or("tests/first-example/".to_owned().into());

            tree_workspace(workspace.as_path()).await?;
            Ok(())
        }

        Command::GenerateIndex => Ok(()),
        Command::Add {
            dependency,
            workspace,
        } => {
            let workspace = workspace.unwrap_or("tests/first-example/".to_owned().into());
            let manifest_file_path = workspace.join("deliver.toml");

            let manifest_file = fs_err::read_to_string(&manifest_file_path).into_diagnostic()?;

            let mut document =
                toml_edit::DocumentMut::from_str(&manifest_file).into_diagnostic()?;
            let (name, version) = dependency
                .split_once('@')
                .unwrap_or((dependency.as_str(), "latest"));

            let version = if version == "latest" {
                let pkg_url = "cran.r-project.org";

                let cran_url = PackageRepositoryUrl::new(&format!("https://{pkg_url}"));
                let downloader = PackageRepoDownloader::new();

                eprintln!("Getting packages file HEAD");
                let etag = downloader
                    .packages_gz_etag(cran_url.clone())
                    .await
                    .into_diagnostic()?
                    .unwrap();
                eprintln!("Finished getting packages file HEAD");

                let decompressed = {
                    let cached_contents =
                        load_cran_packages_file(&etag, pkg_url, &workspace).into_diagnostic()?;

                    match cached_contents {
                        Some(cached_contents) => {
                            eprintln!("Using cached packages file CONTENTS");
                            CranPackagesFileDownload::from_cached(cached_contents)
                        }
                        None => {
                            eprintln!("Downloading packages file CONTENTS");
                            let downloaded = downloader
                                .download_packages_file(&cran_url)
                                .await
                                .into_diagnostic()?;
                            eprintln!("Finished Downloading packages file CONTENTS");
                            eprintln!("Decompressing packages file");
                            let decompressed = downloaded.decompress().into_diagnostic()?;
                            eprintln!("Finished decompressing packages file");

                            save_cran_packages_file(
                                &etag,
                                pkg_url,
                                decompressed.contents(),
                                &workspace,
                            )
                            .into_diagnostic()?;
                            decompressed
                        }
                    }
                };

                let parsed = decompressed.parse()?;

                let version = parsed.package_version_from_name(name);

                match version {
                    Some(version) => version.to_string(),
                    None => {
                        bail!("Package not found");
                    }
                }
            } else {
                version.to_owned()
            };

            document
                .get_mut("dependencies")
                .unwrap()
                .as_table_mut()
                .unwrap()
                .insert(name, toml_edit::value(&version));

            std::fs::write(manifest_file_path, document.to_string()).into_diagnostic()?;

            println!("Added {}@{}", name, version);
            Ok(())
        }
        Command::VendorDeps { workspace } => {
            let workspace = workspace.unwrap_or("tests/first-example/".to_owned().into());

            download_lockfile_packages(&workspace).await?;

            Ok(())
        }
        Command::ShowBuildSteps { workspace } => {
            let workspace = workspace.unwrap_or("tests/first-example/".to_owned().into());

            print_build_order(&workspace).await?;
            Ok(())
        }
        Command::Init { rstudio } => {
            let cwd = std::env::current_dir().into_diagnostic()?;

            let project_name = cwd
                .components()
                .last()
                .expect("paths should have a last component")
                .as_os_str()
                .to_str()
                .expect("directory name must be UTF-8 to create a project");

            edit_project::init(project_name, rstudio)?;
            Ok(())
        }
        Command::New {
            package_name: _,
            rstudio: bool,
        } => todo!(),
        Command::PrintBuildScript { workspace } => {
            let workspace = workspace.unwrap_or("tests/first-example/".to_owned().into());

            print_build_script(&workspace).await?;
            Ok(())
        }
        Command::PrintMakefile { workspace } => {
            let workspace = workspace.unwrap_or("tests/first-example/".to_owned().into());
            let workspace = Workspace::from_path(workspace);

            println!("{}", generate_makefile(&workspace).await?);
            Ok(())
        }
        Command::PrintSystemRequirements { workspace } => {
            let workspace = workspace.unwrap_or("tests/first-example/".to_owned().into());

            print_system_requirements(&workspace).await?;
            Ok(())
        }

        Command::Toolchain { args } => toolchain::handle_toolchain(args).await,
    }
}

#[tokio::main]
async fn main() -> miette::Result<()> {
    let Some(first_arg) = std::env::args().next() else {
        bail!("argv[0] must be set for deliver to function properly")
    };

    let Some(program_name) = first_arg.as_str().split(MAIN_SEPARATOR).last() else {
        bail!("argv[0] must have a program name to function properly");
    };

    match program_name {
        "deliver" => {
            deliver().await?;
        }

        "R" | "Rscript" => {
            toolchain::invoke_r().await?;
        }

        a => {
            bail!("{a} is not a recognized proxy for deliver. It is recommended uninstalling and re-installing deliver.")
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::tree_workspace;

    #[tokio::test]
    async fn test_tree() -> miette::Result<()> {
        tree_workspace("tests/first-example/".as_ref()).await
    }
}
