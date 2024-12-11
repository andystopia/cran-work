mod cran;

use std::{
    cell::RefCell,
    collections::HashMap,
    io::{BufReader, Read},
};

use anyhow::{bail, Context};
use bytes::Bytes;
use clap::Parser;
use cran::RVersionEco;
use cran_description_file_parser::{Field, RVersionOrdering};
use ecow::EcoString;
use flate2::bufread::GzDecoder;
use pubgrub::{
    error::PubGrubError,
    range::Range,
    report::{DefaultStringReporter, Reporter},
    solver::{
        choose_package_with_fewest_versions, resolve, Dependencies, DependencyConstraints,
        DependencyProvider,
    },
    version::Version,
};
use scraper::Selector;

const BASE_PACKAGES: [&str; 15] = [
    "base",
    "compiler",
    "datasets",
    "grDevices",
    "graphics",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "translations",
    "utils",
];

pub struct CranIndexPage(String);
pub struct CranDescriptionFile(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct CranPackage {
    name: EcoString,
    version: RVersionEco,
}

fn cran_package_archive_index_blocking(package: &str) -> reqwest::Result<CranIndexPage> {
    let cran_archive_index = format!("https://cran.r-project.org/src/contrib/Archive/{package}/");
    reqwest::blocking::get(cran_archive_index)?
        .text()
        .map(CranIndexPage)
}

fn parse_cran_index(CranIndexPage(index): &CranIndexPage) -> anyhow::Result<Vec<CranPackage>> {
    let scraper = scraper::Html::parse_document(index);

    let tables = Selector::parse("tr").unwrap();
    let selected = scraper.select(&tables);

    let mut cran_pkgs = vec![];

    for element in selected {
        let img_selector = Selector::parse("img");
        let Some(img) = element.select(&img_selector.unwrap()).next() else {
            continue;
        };

        if img.attr("alt") != Some("[   ]") {
            // println!("{:#?}", element.inner_html());
            // println!("-------------------");
        } else {
            // now we know we have a real package, so we just
            // need to parse it as such.

            let td_select = Selector::parse("td").unwrap();
            let &[_, package_name, _, _, _] =
                element.select(&td_select).collect::<Vec<_>>().as_slice()
            else {
                continue;
            };

            let Some(package_name) = package_name.text().next() else {
                continue;
            };

            // we only want the comprssed archives
            if !package_name.ends_with(".tar.gz") {
                continue;
            }

            let package_name = package_name.trim_end_matches(".tar.gz");

            let (package_name, package_version) = match package_name.split_once("_") {
                Some(x) => x,
                None => panic!("couldn't split package with name {}", package_name),
            };

            cran_pkgs.push(CranPackage {
                name: package_name.into(),
                version: RVersionEco::from_str(package_version),
            });
        }
    }
    Ok(cran_pkgs)
}

pub enum CranPlace {
    Archive,
    Main,
}

fn get_cran_package_description_file(
    package: &str,
    version: &str,
    place: CranPlace,
) -> anyhow::Result<CranDescriptionFile> {
    let download_path = match place {
        CranPlace::Archive => {
            format!("https://cran.r-project.org/src/contrib/Archive/{package}/{package}_{version}.tar.gz")
        }
        CranPlace::Main => {
            format!("https://cran.r-project.org/src/contrib/{package}_{version}.tar.gz",)
        }
    };
    let downloaded = reqwest::blocking::get(download_path)?;

    let bytes = downloaded.bytes()?;
    let downloaded_bytes: &[u8] = bytes.as_ref();

    let tar = GzDecoder::new(BufReader::new(downloaded_bytes));

    let mut archive = tar::Archive::new(tar);

    let search_path = format!("{}/DESCRIPTION", package);
    for entry in archive.entries()? {
        let entry = entry?;
        let path = entry.path()?;
        let path = path.to_str().unwrap();

        if path == search_path {
            let mut reader = BufReader::new(entry);
            let mut contents = String::new();
            reader.read_to_string(&mut contents)?;
            return Ok(CranDescriptionFile(contents));
        }
    }
    bail!("No DESCRIPTION FILE FOUND");
}

#[derive(Debug, Parser)]
pub enum Cli {
    ViewCurrent {
        name: String,
    },
    LsPkg {
        name: String,
        version: Option<String>,
    },
    LsVersions {
        name: String,
    },
}

fn decode_packages_file(bytes: Bytes) -> std::io::Result<String> {
    let mut gz_decode = GzDecoder::new(BufReader::new(bytes.as_ref()));
    let mut index_buf = String::new();
    gz_decode.read_to_string(&mut index_buf)?;
    Ok(index_buf)
}

fn cran_packages_file_blocking() -> anyhow::Result<String> {
    let index =
        reqwest::blocking::get("https://cran.r-project.org/src/contrib/PACKAGES.gz")?.bytes()?;
    let index = decode_packages_file(index)?;
    Ok(index)
}

#[derive(Debug)]
pub struct PackageIndex<'a> {
    packages: Vec<Vec<Field<'a>>>,
    packages_by_name: HashMap<String, usize>,
}

impl<'a> PackageIndex<'a> {
    pub fn from_flat(items: Vec<Field<'a>>) -> PackageIndex<'a> {
        let mut packages_by_name = HashMap::new();
        let mut packages = vec![];

        let mut current_package = vec![];

        for item in items {
            match item {
                Field::Package(src) => {
                    let replaced = std::mem::take(&mut current_package);
                    packages.push(replaced);
                    packages_by_name.insert(src.to_owned(), packages.len());
                    current_package.push(item);
                }
                _ => {
                    current_package.push(item);
                }
            }
        }

        PackageIndex {
            packages,
            packages_by_name,
        }
    }
    pub fn get_package_by_name(&self, name: &str) -> Option<Vec<Field<'a>>> {
        let idx = self.packages_by_name.get(name)?;
        Some(self.packages[*idx].clone())
    }
}

#[allow(dead_code)]
fn pprint_fields(fields: Vec<Field<'_>>) {
    for field in fields {
        match field {
            Field::Package(name) => {
                println!("Package: {}", name);
            }
            Field::AuthorsR(authors) => {
                println!("Authors @ R: {:#?}", authors)
            }
            Field::Type(ty) => {
                println!("Type: {}", ty);
            }
            Field::Title(title) => {
                println!("Title: {}", title);
            }
            Field::Description(desc) => {
                println!("Description: {}", desc);
            }
            Field::License(license) => {
                println!("License: {}", license);
            }
            Field::Version(version) => {
                println!("Version: {}", version.components.join("."))
            }
            Field::RoxygenNote(version) => {
                println!("RoxygenNote: {}", version)
            }
            Field::Depends(dependencies) => {
                println!("Depends:");
                print_dependencies(dependencies);
            }
            Field::Imports(imports) => {
                println!("Imports:");
                print_dependencies(imports);
            }
            Field::LinkingTo(links_to) => {
                println!("LinkingTo:");
                print_dependencies(links_to);
            }
            Field::Suggests(suggests) => {
                println!("Suggests:");
                print_dependencies(suggests);
            }
            Field::Any { key, value } => {
                println!("{key}: {value}")
            }
            Field::ErrorLine(err_line) => {
                println!("ERROR @ {}", err_line);
            }
        }
    }
}

fn print_dependencies(dependencies: Vec<cran_description_file_parser::Dependency<'_>>) {
    for dep in dependencies {
        match dep.constraint {
            Some(constraint) => {
                let (bound, version) = (constraint.ordering, constraint.version);

                let bound_text = match bound {
                    RVersionOrdering::EQ => "=",
                    RVersionOrdering::GT => ">",
                    RVersionOrdering::GE => ">=",
                    RVersionOrdering::LT => "<",
                    RVersionOrdering::LE => "<=",
                };
                println!(
                    "  - {} ({} {})",
                    dep.name,
                    bound_text,
                    version.components.join(".")
                );
            }
            None => {
                println!("  - {} (*)", dep.name);
            }
        }
    }
}

fn parse_cran_desc_file(contents: &str) -> anyhow::Result<Vec<Field<'_>>> {
    match cran_description_file_parser::from_str(contents) {
        Ok(res) => Ok(res),
        Err(err) => {
            cran_description_file_parser::pretty_errors(contents, err);
            bail!("failed to parse description file")
        }
    }
}

pub fn cran_dependencies_to_grub_dependencies(
    dependencies: Vec<cran_description_file_parser::Dependency>,
) -> Dependencies<EcoString, RVersionEco> {
    let mut deps = DependencyConstraints::default();
    for dep in dependencies {
        let name = dep.name;
        let version_constraint = dep
            .constraint
            .map(|c| {
                let version = RVersionEco::from(c.version);
                match c.ordering {
                    RVersionOrdering::EQ => Range::exact(version),
                    RVersionOrdering::GT => Range::higher_than(version.bump()),
                    RVersionOrdering::GE => Range::higher_than(version),
                    RVersionOrdering::LT => Range::strictly_lower_than(version),
                    RVersionOrdering::LE => Range::strictly_lower_than(version.bump()),
                }
            })
            .unwrap_or(Range::any());
        if let Some(dep) = deps.get_mut(name) {
            dep.intersection(&version_constraint);
        } else {
            deps.insert(name.into(), version_constraint);
        }
    }
    Dependencies::Known(deps)
}

fn dependencies_from_description_file_fields_single(
    fields: Vec<Field>,
) -> (CranPackage, Dependencies<EcoString, RVersionEco>) {
    let mut current_package_name = None;
    let mut current_package_version = None;
    let mut current_dependencies = vec![];

    for item in fields {
        match item {
            Field::Package(new_pkg_name) => {
                current_package_name = Some(new_pkg_name);
            }
            Field::Version(version) => current_package_version = Some(RVersionEco::from(version)),
            Field::Imports(dep) => current_dependencies
                .extend(dep.into_iter().filter(|d| !BASE_PACKAGES.contains(&d.name))),
            _ => {}
        }
    }

    match (current_package_name, current_package_version) {
        (Some(name), Some(version)) => (
            CranPackage {
                name: EcoString::from(name),
                version,
            },
            cran_dependencies_to_grub_dependencies(current_dependencies),
        ),
        _ => panic!("invalid package description"),
    }
}

fn dependencies_from_description_file_fields(
    fields: Vec<Field>,
) -> Vec<(CranPackage, Dependencies<EcoString, RVersionEco>)> {
    let mut packages = vec![];

    let mut current_package_name = None;
    let mut current_package_version = None;
    let mut current_dependencies = vec![];

    for item in fields {
        match item {
            Field::Package(new_pkg_name) => {
                let name = current_package_name.take();
                let version = current_package_version.take();
                if let Some((name, version)) = name.zip(version) {
                    let deps = std::mem::take(&mut current_dependencies);
                    packages.push((
                        CranPackage {
                            name: EcoString::from(name),
                            version,
                        },
                        cran_dependencies_to_grub_dependencies(deps),
                    ))
                }
                current_package_name = Some(new_pkg_name);
            }
            Field::Version(version) => current_package_version = Some(RVersionEco::from(version)),
            Field::Imports(dep) => current_dependencies
                .extend(dep.into_iter().filter(|d| !BASE_PACKAGES.contains(&d.name))),
            _ => {}
        }
    }

    let name = current_package_name.take();
    let version = current_package_version.take();
    if let Some((name, version)) = name.zip(version) {
        let deps = std::mem::take(&mut current_dependencies);
        packages.push((
            CranPackage {
                name: EcoString::from(name),
                version,
            },
            cran_dependencies_to_grub_dependencies(deps),
        ))
    }

    packages
}

struct CRANResolver {
    /// r doesn't really have a defined versioning system
    /// which means people can do things like v1, v2, v3, v4, v5,
    /// instead of semver, and since v1, v2, v3, ... is bijective
    /// with the natural numbers, then NumberVersion captures the
    /// loose versioning system of R packages correctly.
    up_to_date: RefCell<HashMap<EcoString, RVersionEco>>,
    dependency_cache: RefCell<HashMap<CranPackage, Dependencies<EcoString, RVersionEco>>>,
    version_cache: RefCell<HashMap<EcoString, Vec<RVersionEco>>>,
    /// this is kinda not what you wanna do
    /// but until I can get an async form of
    /// pubgrub, this is what I'm going to do
    up_to_date_full_index: Vec<CranPackage>,
}

impl CRANResolver {
    pub fn new() -> anyhow::Result<Self> {
        eprintln!("Loading CRAN Packages file...");
        let packages_file = cran_packages_file_blocking()?;
        eprintln!("Finished Loading CRAN Packages file...");
        let fields = parse_cran_desc_file(&packages_file)?;
        let pkgs_info = dependencies_from_description_file_fields(fields);

        let mut up_to_date_parsed = Vec::new();
        let mut lookup = HashMap::new();
        let mut up_to_date = HashMap::new();

        for (pkg, deps) in pkgs_info {
            up_to_date.insert(pkg.name.clone(), pkg.version.clone());
            lookup.insert(pkg.clone(), deps);
            up_to_date_parsed.push(pkg);
        }

        // let up_to_date = cran_main_index().await?;
        // let up_to_date_parsed = parse_cran_index(&up_to_date)?;

        Ok(Self {
            up_to_date: RefCell::new(up_to_date),
            dependency_cache: RefCell::new(lookup),
            version_cache: Default::default(),
            up_to_date_full_index: up_to_date_parsed,
        })
    }

    pub fn most_recent_version_of(&self, package_name: &str) -> RVersionEco {
        self.up_to_date.borrow()[package_name].clone()
    }

    pub fn dependencies_of(
        &self,
        cran_package: &CranPackage,
    ) -> anyhow::Result<Dependencies<EcoString, RVersionEco>> {
        if !self.dependency_cache.borrow().contains_key(cran_package) {
            println!("missing dependency cache for {}", cran_package.name);
            self.populate_dependencies_of(
                cran_package.name.as_str(),
                cran_package.version.clone(),
            )?;
        }

        self.dependency_cache
            .borrow()
            .get(cran_package)
            .cloned()
            .context("failed to get dependendencies")
    }

    pub fn get_versions_of(&self, package_name: &str) -> anyhow::Result<Vec<RVersionEco>> {
        let contains_key = { self.version_cache.borrow().contains_key(package_name) };

        if contains_key {
            Ok(self.version_cache.borrow()[package_name].clone())
        } else {
            self.populate_versions_of(package_name)?;
            Ok(self.version_cache.borrow()[package_name].clone())
        }
    }

    pub fn populate_dependencies_of(
        &self,
        package_name: &str,
        package_version: RVersionEco,
    ) -> anyhow::Result<()> {
        eprintln!("populating dependencies of {package_name} {package_version}");
        let versions = self.get_versions_of(package_name)?;

        let place = if &package_version == versions.last().unwrap() {
            CranPlace::Main
        } else {
            CranPlace::Archive
        };

        let CranDescriptionFile(desc_file) = get_cran_package_description_file(
            package_name,
            &package_version.components.join("."),
            place,
        )?;

        let fields = parse_cran_desc_file(&desc_file)?;

        let (pkg, dependencies) = dependencies_from_description_file_fields_single(fields);
        assert!(
            pkg.version == package_version,
            "requested version / received version mismatch"
        );
        assert!(
            pkg.name == package_name,
            "requested name / received name mismatch"
        );

        self.dependency_cache.borrow_mut().insert(pkg, dependencies);

        Ok(())
    }

    pub fn populate_versions_of(&self, package_name: &str) -> anyhow::Result<()> {
        let archives = cran_package_archive_index_blocking(package_name)?;
        let archives_parsed = parse_cran_index(&archives)?;
        let up_to_date_version = self
            .up_to_date_full_index
            .iter()
            .find(|pkg| pkg.name == package_name)
            .cloned();
        // I can't imagine a situation where the CRAN wouldn't have
        // a package that the archives have, but eh, whatevs.

        let mut vc = self.version_cache.borrow_mut();
        let vc_entry = vc
            .entry(EcoString::from(package_name))
            .or_default();

        vc_entry.drain(..);

        for version in archives_parsed
            .into_iter()
            .chain(up_to_date_version.into_iter())
            .map(|CranPackage { name: _, version }| version)
        {
            vc_entry.push(version);
        }

        Ok(())
    }
}

impl DependencyProvider<EcoString, RVersionEco> for CRANResolver {
    fn choose_package_version<
        T: std::borrow::Borrow<EcoString>,
        U: std::borrow::Borrow<Range<RVersionEco>>,
    >(
        &self,
        potential_packages: impl Iterator<Item = (T, U)>,
    ) -> Result<(T, Option<RVersionEco>), Box<dyn std::error::Error>> {
        Ok(choose_package_with_fewest_versions(
            |f: &EcoString| self.get_versions_of(f).unwrap().into_iter().rev(),
            potential_packages,
        ))
    }

    fn get_dependencies(
        &self,
        package: &EcoString,
        version: &RVersionEco,
    ) -> Result<pubgrub::solver::Dependencies<EcoString, RVersionEco>, Box<dyn std::error::Error>>
    {
        // step 1, we need to convert whatever the version flag is to an
        // actual cran version.

        let deps = self.dependencies_of(&CranPackage {
            name: package.clone(),
            version: version.clone(),
        })?;

        Ok(deps)
    }
}

fn print_built_dependency_tree(
    root_packages: Vec<(EcoString, RVersionEco)>,
    sol: &HashMap<EcoString, RVersionEco>,
    resolve: &CRANResolver,
    indent: &str,
) -> anyhow::Result<()> {
    let root_packages_len = root_packages.len();
    for (i, (rname, rver)) in root_packages.into_iter().enumerate() {
        let last = i == (root_packages_len - 1);
        let bracket = if last { '└' } else { '├' };
        let spacer = if last { ' ' } else { '│' };
        println!("{}{} {} @ {}", indent, bracket, rname, rver);

        let res = resolve
            .get_dependencies(&rname, &rver)
            .map_err(|e| anyhow::anyhow!("{}", e))?;

        match res {
            Dependencies::Unknown => bail!("{rname} has unknown dependencies"),
            Dependencies::Known(known) => {
                let mut res = known.into_keys().collect::<Vec<_>>();

                res.sort_unstable();

                let parents = res
                    .into_iter()
                    .map(|res| {
                        let version = sol.get(&res).unwrap();

                        (res, version.clone())
                    })
                    .collect::<Vec<_>>();

                print_built_dependency_tree(parents, sol, resolve, &format!("{indent}{spacer} "))?;
            }
        }
    }

    Ok(())
}


fn main() -> anyhow::Result<()> {
    // println!("R packages for Version 4.3.2");
    // for pkg in fetch_recommended_packages("4.3.2")? {
    //     println!(" - {} @ {}", pkg.name, pkg.version);
    // }
    resolve_package("tidyverse")?;

    Ok(())
}

fn resolve_package(pkg: &str) -> Result<(), anyhow::Error> {
    let cran = CRANResolver::new()?;
    let resolution = resolve(&cran, pkg.into(), cran.most_recent_version_of(pkg));
    match resolution {
        Ok(sol) => {
            print_built_dependency_tree(
                vec![(EcoString::from(pkg), cran.most_recent_version_of(pkg))],
                &sol.into_iter().collect(),
                &cran,
                "",
            )?;
        }
        Err(PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();
            eprintln!("{}", DefaultStringReporter::report(&derivation_tree));
        }
        Err(err) => panic!("{:?}", err),
    };
    Ok(())
}
