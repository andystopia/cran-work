use std::{
    borrow::Cow,
    io::{BufReader, Read},
};

use cran_description_file_parser::{rich_to_miette, Field};
use flate2::bufread::GzDecoder;
use url::Url;

use crate::CranError;

#[derive(Debug)]
struct RepoDownloadFileGz {
    url: Url,
}

impl RepoDownloadFileGz {
    fn new(url: Url) -> RepoDownloadFileGz {
        RepoDownloadFileGz { url }
    }

    pub fn download_string(&self) -> Result<String, CranError> {
        let compressed = ureq::get(&self.url.to_string()).call()?.into_reader();

        let mut reader = GzDecoder::new(BufReader::new(compressed));

        let mut contents = String::new();
        reader
            .read_to_string(&mut contents)
            .map_err(|err| CranError::Io { error: err })?;
        Ok(contents)
    }
}

pub struct RepoIndex<'r> {
    repo: &'r Repo,
    download: RepoDownloadFileGz,
}

impl<'r> RepoIndex<'r> {
    pub fn as_manifest_repo(&self) -> Result<ManifestRepo, CranError> {
        Ok(ManifestRepo {
            repo: self.repo,
            index: self.download.download_string()?,
        })
    }
}

#[derive(Debug)]
struct Repo {
    url: Url,
}

impl Repo {
    pub fn new(url: Url) -> Repo {
        Repo { url }
    }

    pub fn from_url_str(url: &str) -> Result<Repo, CranError> {
        Ok(Repo {
            url: Url::parse(url).map_err(|err| CranError::UrlError {
                url: url.to_owned(),
                error: err,
            })?,
        })
    }

    pub fn as_index(&self) -> RepoIndex {
        RepoIndex {
            repo: self,
            download: RepoDownloadFileGz::new(
                self.url
                    .join("/src/contrib/")
                    .unwrap()
                    .join("PACKAGES.gz")
                    .unwrap(),
            ),
        }
    }
}

#[derive(Debug)]
pub struct ManifestRepo<'r> {
    repo: &'r Repo,
    index: String,
}

impl<'r> ManifestRepo<'r> {
    pub fn as_parsed(&self) -> Result<ParsedManifestRepo, CranError> {
        let parsed_contents = cran_description_file_parser::from_str(&self.index)
            .map_err(|err| rich_to_miette(err, self.index.clone()))
            .map_err(|errs| CranError::PackagesParseError {
                url: self.repo.url.to_string(),
                errors: errs,
            })?;
        Ok(ParsedManifestRepo {
            manifest_repo: self,
            parsed_contents,
        })
    }
}

#[derive(Debug)]
pub struct ParsedManifestRepo<'m, 'r> {
    manifest_repo: &'m ManifestRepo<'r>,
    parsed_contents: Vec<Field<'m>>,
}

pub struct PackageName<'s> {
    name: Cow<'s, str>,
}

pub struct PackageVersion {
    version: String,
}

#[cfg(test)]
mod test {
    use super::Repo;

    #[test]
    pub fn testing() -> miette::Result<()> {
        let repo = Repo::from_url_str("https://cran.r-project.org")?;
        dbg!(repo.as_index().as_manifest_repo()?.as_parsed()?);
        Ok(())
    }
}
