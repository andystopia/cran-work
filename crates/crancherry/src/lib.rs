//! Crancherry is a crate designed
//! to interface with CRAN-like file repositories.
//! It provides an easier interface to ask the CRAN
//! for information and packages, and tries to abstract
//! away a few of the details of the package repository,
//! while still covering a great deal of it's functionality.
//!
//! ## What crancherry is *not*:
//!  - a version solver
//!  - dependency manager
//!  - a query engine
//!  - a package manager
//!
//! ## What crancherry is:
//!  - a client to request information
//!    on the cran in the same way that a human
//!    would be able to peruse the index themselves.

pub mod cherry;

use std::{
    collections::HashMap,
    io::{BufReader, Read},
};

use cran_description_file_parser::{rich_to_miette, Field};
use ecow::EcoString;
use flate2::bufread::GzDecoder;
use miette::Diagnostic;
use scraper::Selector;
use serde::Deserialize;
use tar::Archive;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum CranError {
    #[error("failed to parse url {url}: {error}")]
    UrlError { 
        url: String,
        error: url::ParseError
    },
    #[error("failed to get web resource: {error}")]
    RequestError {
        #[from]
        error: ureq::Error,
    },
    #[error("an io error occurred: {error}")]
    Io { error: std::io::Error },
    #[error("Failed Parsing PACKAGES file ({url}/PACKAGES): {errors:?}")]
    PackagesParseError {
        url: String,
        errors: Vec<miette::Report>,
    },
    #[error("Failed to parse the index of the cran archive:  ({url}/PACKAGES), because a name was missing")]
    MissingPackageNameInPackages { url: String },
    #[error("Failed to parse the index of the cran archive:  ({url}/PACKAGES), because a version was missing")]
    MissingPackageVersionInPackages { url: String },
    #[error(
        "Failed to get version history for {package_name}, because no current version was found"
    )]
    NoCurrentVersionFoundForPackage { package_name: String },

    #[error("Package {name} was not found in the CRAN")]
    PackageNotFound { name: String },

    #[error("CRAN: {name} does not have version {version} available.")]
    PackageFoundVersionNotFound { name: String, version: String },
    #[error("Failed to parse a yaml which {purpose}: {error}")]
    YamlDeserialize {
        error: serde_yaml::Error,
        purpose: String,
    },
}

#[derive(Debug)]
pub struct CranPackage {
    pub name: EcoString,
    pub version: RVersion,
}

fn archived_versions(url: &str, package: &str) -> Result<Vec<CranPackage>, CranError> {
    let cran_archive_index = format!("{url}/Archive/{package}/");
    let res = ureq::get(&cran_archive_index).call();

    match res {
        Ok(res) => parse_cran_index(
            res.into_string()
                .map_err(|err| CranError::Io { error: err })?,
        ),
        Err(ureq::Error::Status(404, _)) => {
            // it's okay if we don't have any archived
            // versions.  have seen this happen,
            // and it absolutely should not be a point
            // of failure for a version solving software.
            Ok(vec![])
        }
        Err(err) => Err(CranError::RequestError { error: err }),
    }
}

fn parse_cran_index(index: String) -> Result<Vec<CranPackage>, CranError> {
    let scraper = scraper::Html::parse_document(&index);

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
                version: RVersion::from_str(package_version),
            });
        }
    }
    Ok(cran_pkgs)
}

pub struct CranPackageCompressedDownload {
    pub compressed: Vec<u8>,
    pub name: String,
    pub version: RVersion,
}

impl CranPackageCompressedDownload {
    pub fn into_compressed_package(self) -> Result<CompressedRPackage, CranError> {
        let reader = GzDecoder::new(BufReader::new(
            Box::new(std::io::Cursor::new(self.compressed))
                as Box<dyn Read + Send + Sync + 'static>,
        ));

        Ok(CompressedRPackage {
            compressed: Archive::new(reader),
            name: self.name.into(),
        })
    }
}

pub struct CranPackageDownloadUrl {
    pub name: String,
    pub version: RVersion,
    pub url: String,
}

impl CranPackageDownloadUrl {
    fn from_main(url: &String, name: String, version: RVersion) -> Self {
        Self {
            url: format!("{}/{}_{}.tar.gz", url, name, version.to_string()),

            name,
            version,
        }
    }

    fn from_archive(url: &String, name: String, version: RVersion) -> Self {
        Self {
            url: format!(
                "{}/Archive/{name}/{name}_{}.tar.gz",
                url,
                version.to_string()
            ),

            name,
            version,
        }
    }

    /// downloads and decompresses the package, however,
    /// it is still in a tar archive (the structure which
    /// represents the package in in the public compressed
    /// field of the returned struct). The package
    /// is wrapped in a convenience struct for better error
    /// messages, and more readable access of files.
    pub fn download_decompressed(&self) -> Result<CompressedRPackage, CranError> {
        let compressed = ureq::get(&self.url).call()?.into_reader();

        let reader = GzDecoder::new(BufReader::new(compressed));

        Ok(CompressedRPackage {
            compressed: Archive::new(reader),
            name: self.name.clone().into(),
        })
    }

    /// downloads the archive into a structure
    /// backed by a Vec<u8>, which is accessible in
    /// the compressed field of the returned struct.
    /// this method is probably less efficient than
    /// download_decompressed, yet does less work, and
    /// has less features, nevertheless, this is useful
    /// if you need to compute the hash of the data, and can
    /// inevitably converted to the `CompressedRPackage` struct
    /// that the other function returns anyways. It is probably
    /// just a bit less simple/efficient. I would recommend
    /// converting this to a `CompressedRPackage` instead of
    /// redownloading the package using `download_decompressed`, by
    /// using the `CranPackageCompressedDownload::into_compressed_package` method.
    pub fn download_uncompressed_vec(&self) -> Result<CranPackageCompressedDownload, CranError> {
        let mut reader = ureq::get(&self.url).call()?.into_reader();

        let mut buf = Vec::new();
        std::io::copy(&mut reader, &mut buf).map_err(|err| CranError::Io { error: err })?;
        Ok(CranPackageCompressedDownload {
            compressed: buf,
            name: self.name.clone(),
            version: self.version.clone(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VersionSeparator {
    Dot,
    Dash,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RVersion {
    pub components: Vec<EcoString>,
    pub separators: Vec<VersionSeparator>,
}

impl PartialOrd for RVersion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // comparing two (in non-semver) is somewhat, tricky,
        // from the most forwards part of the version to the most
        // backwards, if two components are equal, then we move to the next component,
        // if two components are unequal, then the component with the longer
        // component is considered greater, if there is no component with a longer
        // component, then the component which sorts to a higher lexical order
        // is considered greater.

        let mut self_iter = self.components.iter();
        let mut other_iter = other.components.iter();

        loop {
            let self_comp = self_iter.next();
            let other_comp = other_iter.next();

            match (self_comp, other_comp) {
                (None, None) => return Some(std::cmp::Ordering::Equal),
                (None, Some(_)) => return Some(std::cmp::Ordering::Less),
                (Some(_), None) => return Some(std::cmp::Ordering::Greater),
                (Some(self_comp), Some(other_comp)) => {
                    if self_comp.len() == other_comp.len() {
                        match self_comp.cmp(other_comp) {
                            std::cmp::Ordering::Equal => continue,
                            x => return Some(x),
                        }
                    } else if self_comp.len() > other_comp.len() {
                        return Some(std::cmp::Ordering::Greater);
                    } else {
                        return Some(std::cmp::Ordering::Less);
                    }
                }
            }
        }
    }
}

impl Ord for RVersion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::fmt::Display for RVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (component, sep) in self.components.iter().zip(
            self.separators
                .iter()
                .map(Some)
                .chain(std::iter::once(None)),
        ) {
            f.write_str(component.as_str())?;
            if let Some(sep) = sep {
                match sep {
                    VersionSeparator::Dot => f.write_str(".")?,
                    VersionSeparator::Dash => f.write_str("-")?,
                }
            }
        }
        Ok(())
    }
}

impl RVersion {
    pub fn from_str(source: &str) -> RVersion {
        let source = source.trim();

        let split = source.split(&['.', '-']);

        let components = split.into_iter().map(EcoString::from).collect();
        let separators = source
            .chars()
            .filter_map(|c| match c {
                '-' => Some(VersionSeparator::Dash),
                '.' => Some(VersionSeparator::Dot),
                _ => None,
            })
            .collect();

        RVersion {
            components,
            separators,
        }
    }
}

impl<'a> From<cran_description_file_parser::RVersion<'a>> for RVersion {
    /// convert from the parser R version to this
    /// crates' R package version definition.
    fn from(
        cran_description_file_parser::RVersion {
            components,
            separators,
        }: cran_description_file_parser::RVersion,
    ) -> Self {
        Self {
            components: components.iter().map(|s| EcoString::from(s.as_ref())).collect(),
            separators: separators
                .iter()
                .map(|s| match s {
                    cran_description_file_parser::VersionSeperator::Dot => VersionSeparator::Dot,
                    cran_description_file_parser::VersionSeperator::Dash => VersionSeparator::Dash,
                })
                .collect(),
        }
    }
}

pub struct DescriptionFile(String);

impl DescriptionFile {
    pub fn contents(&self) -> &str {
        &self.0
    }

    pub fn parse(&self) -> Result<Vec<Field>, CranError> {
        cran_description_file_parser::from_str(&self.0)
            .map_err(|err| rich_to_miette(err, self.0.clone()))
            .map_err(|err| CranError::PackagesParseError {
                url: "<NA>".to_owned(),
                errors: err,
            })
    }
}

pub struct CompressedRPackage {
    pub compressed: Archive<GzDecoder<BufReader<Box<dyn Read + Send + Sync + 'static>>>>,
    pub name: EcoString,
}

impl CompressedRPackage {
    pub fn at_path<W: std::io::Write>(
        &mut self,
        path: &std::path::Path,
        w: &mut W,
    ) -> Result<(), std::io::Error> {
        let archive = &mut self.compressed;
        for file in archive.entries()? {
            let mut file = file?;
            let file_path = file.path()?;
            let file_path: &std::path::Path = file_path.as_ref();
            if file_path == path {
                std::io::copy(&mut file, w)?;
                return Ok(());
            }
        }
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!(
                "file at `{}` not found in package `{}`",
                path.display(),
                self.name
            ),
        ))
    }

    pub fn description_file(&mut self) -> Result<DescriptionFile, CranError> {
        let mut output_buf = Vec::new();
        let package = &self.name;
        self.at_path(format!("{package}/DESCRIPTION").as_ref(), &mut output_buf)
            .map_err(|error| CranError::Io { error })?;
        String::from_utf8(output_buf)
            .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err))
            .map_err(|error| CranError::Io { error: error })
            .map(DescriptionFile)
    }
}

#[derive(Deserialize, Debug)]
pub struct BioConductor {
    r_ver_for_bioc_ver: HashMap<String, String>,
    release_dates: HashMap<String, String>,
    release_version: String,
    r_version_associated_with_release: String,
}

impl BioConductor {
    pub fn new() -> Result<Self, CranError> {
        let req = ureq::get("https://bioconductor.org/config.yaml")
            .call()
            .map_err(|err| CranError::RequestError { error: err })?;

        serde_yaml::from_reader(req.into_reader()).map_err(|err| CranError::YamlDeserialize {
            error: err,
            purpose: "enumerates details about the BioConductor".to_owned(),
        })
    }

    pub fn releases(&self) -> Vec<String> {
        self.release_dates.keys().cloned().collect()
    }

    pub fn r_version_for_bioc_version(&self, bioc_version: &str) -> Option<&str> {
        self.r_ver_for_bioc_ver
            .get(bioc_version)
            .map(|x| x.as_str())
    }

    pub fn current_release(&self) -> &str {
        &self.release_version
    }

    pub fn current_r_version(&self) -> &str {
        &self.r_version_associated_with_release
    }

    pub fn into_bioc_repo(&self) -> Cran {
        Cran::with_url(format!(
            "https://bioconductor.org/packages/{}/bioc/src/contrib",
            self.current_release()
        ))
    }
    pub fn into_annotation_repo(&self) -> Cran {
        Cran::with_url(format!(
            "https://bioconductor.org/packages/{}/data/annotation/src/contrib",
            self.current_release()
        ))
    }
    pub fn into_experiment_repo(&self) -> Cran {
        Cran::with_url(format!(
            "https://bioconductor.org/packages/{}/data/experiment/src/contrib",
            self.current_release()
        ))
    }
}

pub struct Cran {
    url: String,
}

impl Cran {
    pub fn new() -> Self {
        Self {
            // hmm, is it fail to link this deeply into the CRAN?
            url: "https://cran.r-project.org/src/contrib".to_string(),
        }
    }

    /// assumes structure of active CRAN, but allows
    /// just changing out the host for a different mirror.
    pub fn with_host(host: String) -> Self {
        Self {
            url: format!("https://{}/src/contrib", host),
        }
    }

    /// create a new cran client with a given url
    /// this is optimal if you can pre-choose your
    /// cran mirror, instead of depending on r-project.org,
    /// for speed or if you have an alternative mirrror.
    ///
    /// I'm not sure how cran-compliant most mirrors are,
    /// so the default link points into the src/contrib directory,
    /// but this naturally excludes things like binary substitution,
    /// so we might have to generalize this a little bit.
    pub fn with_url(url: String) -> Self {
        Self { url }
    }

    /// returns the current version and name
    /// of every single package on a given index.
    pub fn available(&self) -> Result<Vec<CranPackage>, CranError> {
        let pkgs_file = self.packages_file()?;

        let package_fields = cran_description_file_parser::from_str(&pkgs_file)
            .map_err(|errs| rich_to_miette(errs, pkgs_file.clone()))
            .map_err(|err| CranError::PackagesParseError {
                url: self.url.clone(),
                errors: err,
            })?;

        let mut all_pkgs = Vec::new();

        let mut package_name = None;

        for field in package_fields {
            match field {
                cran_description_file_parser::Field::Package(pkg) => {
                    if package_name.is_some() {
                        return Err(CranError::MissingPackageVersionInPackages {
                            url: self.url.clone(),
                        });
                    }
                    package_name = Some(pkg.to_owned());
                }
                cran_description_file_parser::Field::Version(rversion) => {
                    let package_version = RVersion::from(rversion);
                    all_pkgs.push(CranPackage {
                        name: package_name
                            .take()
                            .ok_or_else(|| CranError::MissingPackageNameInPackages {
                                url: self.url.clone(),
                            })?
                            .into(),
                        version: package_version,
                    })
                }
                _ => {}
            }
        }

        Ok(all_pkgs)
    }

    /// get all the current packages as a vector
    /// of packages with their corresponding download urls
    pub fn available_with_url(&self) -> Result<Vec<CranPackageDownloadUrl>, CranError> {
        self.available()?
            .into_iter()
            .map(|pkg| {
                Ok(CranPackageDownloadUrl::from_main(
                    &self.url,
                    pkg.name.into(),
                    pkg.version,
                ))
            })
            .collect()
    }

    /// returns the historical versions of every package, does NOT
    /// include the most up to date version.
    /// This function exists for efficiency for filling in the
    /// case where getting the current version is efficient but
    /// the rest are not, we shouldn't need to fetch the current version
    /// again in order to get the history.
    pub fn package_history(&self, package_name: &str) -> Result<Vec<CranPackage>, CranError> {
        archived_versions(&self.url, package_name)
    }

    /// returns the all versions of a package, past and present
    pub fn package_versions(&self, package_name: &str) -> Result<Vec<CranPackage>, CranError> {
        let mut parsed = self.package_history(package_name)?;

        let current_package_version = self.most_recent_version_of(package_name)?;
        parsed.push(current_package_version);
        Ok(parsed)
    }

    pub fn most_recent_version_of(&self, package_name: &str) -> Result<CranPackage, CranError> {
        let available = self.available()?;
        let referenced_package = available.into_iter().find(|pkg| pkg.name == package_name);
        let current_package_version =
            referenced_package.ok_or_else(|| CranError::NoCurrentVersionFoundForPackage {
                package_name: package_name.to_string(),
            })?;
        Ok(current_package_version)
    }

    pub fn most_recent_version_of_url(
        &self,
        package_name: &str,
    ) -> Result<CranPackageDownloadUrl, CranError> {
        let available = self.available()?;
        let referenced_package = available.into_iter().find(|pkg| pkg.name == package_name);
        let current_package_version =
            referenced_package.ok_or_else(|| CranError::NoCurrentVersionFoundForPackage {
                package_name: package_name.to_string(),
            })?;
        
        let pkg = current_package_version;
        Ok(CranPackageDownloadUrl::from_main(
            &self.url,
            pkg.name.into(),
            pkg.version,
        ))
    }

    pub fn download_url_for_most_recent(&self, name: &str) -> Result<String, CranError> {
        let first_version = self.most_recent_version_of(name)?;

        return Ok(format!(
            "{}/{}_{}.tar.gz",
            self.url,
            name,
            first_version.version.to_string()
        ));
    }

    pub fn download_url_for(
        &self,
        name: &str,
        version: &RVersion,
    ) -> Result<CranPackageDownloadUrl, CranError> {
        let first_version = self.most_recent_version_of(name)?;

        if &first_version.version == version {
            return Ok(CranPackageDownloadUrl::from_main(
                &self.url,
                name.into(),
                version.clone(),
            ));
        }

        let versions = self.package_history(name)?;

        if versions.len() == 0 {
            return Err(CranError::PackageNotFound { name: name.into() });
        }

        versions
            .into_iter()
            .map(|version| version.version)
            .position(|x| &x == version)
            .ok_or_else(|| CranError::PackageFoundVersionNotFound {
                name: name.into(),
                version: version.to_string(),
            })?;

        Ok(CranPackageDownloadUrl::from_archive(
            &self.url,
            name.into(),
            version.clone(),
        ))
    }

    pub fn packages_file(&self) -> Result<String, CranError> {
        let contents = ureq::get(&format!("{}/PACKAGES.gz", self.url))
            .call()?
            .into_reader();

        let mut content_buf = String::new();
        let mut reader = GzDecoder::new(BufReader::new(contents));

        reader
            .read_to_string(&mut content_buf)
            .map_err(|err| CranError::Io { error: err })?;

        Ok(content_buf)
    }
}

#[cfg(test)]
mod test {

    #[test]
    pub fn test_available() -> miette::Result<()> {
        let cran = super::Cran::new();

        dbg!(cran.available()?);
        Ok(())
    }

    #[test]
    pub fn current_version() -> miette::Result<()> {
        let cran = super::Cran::new();

        dbg!(cran.most_recent_version_of("ggplot2")?);
        Ok(())
    }

    #[test]
    pub fn versions_test() -> miette::Result<()> {
        let cran = super::Cran::new();

        dbg!(cran.package_versions("ggplot2")?);
        Ok(())
    }

    #[test]
    pub fn test_bioc() -> miette::Result<()> {
        let bioc = super::BioConductor::new()?;

        let repo = bioc.into_bioc_repo();

        // check to ensure that the available packages
        // does not through an exception.
        let available = repo.available()?;

        // we absolutely should see some packages
        // in the repository
        for package in available.iter() {
            println!("{}@{}", package.name, package.version);
        }

        // let's make sure that we don't choke on versions
        // of packages, which do not exist in the archive.
        dbg!(repo.package_versions(&available.iter().next().unwrap().name)?);

        Ok(())
    }
}
