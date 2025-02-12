use std::{
    cell::OnceCell,
    collections::HashMap,
    io::{BufReader, BufWriter, Read, Write},
    iter::{Chain, Filter, Flatten},
    option::IntoIter,
    path::Path,
};

use bytes::Bytes;
use cran_description_file_parser::{rich_to_miette, Dependency, Field, RVersion};
use flate2::bufread::GzDecoder;
use md5::Digest;
use miette::IntoDiagnostic;
use reqwest::{header, Url};
use scraper::{Html, Selector};

#[derive(Debug, Clone)]
pub struct PackageRepositoryUrl(Url);

impl PackageRepositoryUrl {
    pub fn new(url: &str) -> Self {
        PackageRepositoryUrl(
            Url::parse(url).expect("package repositories should always have valid urls"),
        )
    }
    pub fn domain(&self) -> &str {
        self.0.domain().unwrap()
    }
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
    pub fn from_domain(domain: &str) -> Self {
        PackageRepositoryUrl::new(&format!("https://{}", domain))
    }
}

#[test]
pub fn test_domain() {
    let url = PackageRepositoryUrl::new("https://cran.r-project.org/src/contrib/");
    assert_eq!(url.domain(), "cran.r-project.org");
}

#[derive(Clone)]
pub struct PackageDownload {
    name: String,
    url: Url,
    client: reqwest::Client,
}

impl PackageDownload {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub async fn download_to_path(&self, path: &Path) -> miette::Result<MD5Digest> {
        let mut request = self
            .client
            .get(self.url.clone())
            .send()
            .await
            .into_diagnostic()?;

        // TODO: I wonder if this or the bufwriter should be tokio async to reduce
        // blocking the thread. I would need to be convinced that this is
        // actually useful though, as in a single threaded environment, I think
        // the copy operation will saturate the core.
        let file = fs_err::OpenOptions::new()
            .write(true)
            .create(true)
            .open(path)
            .into_diagnostic()?;

        let mut md5_hasher = md5::Md5::new();
        let mut bufwriter = BufWriter::new(file);

        while let Some(chunk) = request.chunk().await.into_diagnostic()? {
            bufwriter.write_all(&chunk).into_diagnostic()?;
            md5_hasher.update(chunk);
        }

        let md5_hash = MD5Digest(md5_hasher.finalize().into());

        Ok(md5_hash)
    }

    pub fn tarball_dir(&self, workspace: &Path) -> std::path::PathBuf {
        workspace
            .join("deliver-r")
            .join("source-tarballs")
            .join("packages")
    }
    pub fn tarball_path(&self, workspace: &Path) -> std::path::PathBuf {
        self.tarball_dir(workspace)
            .join(format!("{}.tar.gz", self.name()))
    }

    pub fn source_dir(&self, workspace: &Path) -> std::path::PathBuf {
        workspace
            .join("deliver-r")
            .join(".sources")
            .join("packages")
    }

    pub async fn download(&self, workspace: &Path) -> miette::Result<MD5Digest> {
        fs_err::create_dir_all(self.tarball_dir(workspace)).into_diagnostic()?;

        let tarball_path = self.tarball_path(workspace);
        Ok(self.download_to_path(&tarball_path).await?)
    }

    pub fn decompress(&self, workspace: &Path) -> miette::Result<()> {
        let downloaded_path = self.tarball_path(workspace);

        let file = fs_err::File::open(downloaded_path).into_diagnostic()?;
        let tar_gz = BufReader::new(file);

        let tar = GzDecoder::new(tar_gz);

        let mut dir = tar::Archive::new(tar);

        dir.unpack(&self.source_dir(workspace)).into_diagnostic()?;

        Ok(())
    }
}

#[derive(Clone)]
pub struct PackageRepoDownloader {
    client: reqwest::Client,
}

impl PackageRepoDownloader {
    pub fn new() -> Self {
        PackageRepoDownloader {
            client: reqwest::Client::new(),
        }
    }

    pub async fn download_repo_html_page(
        &self,
        url: &PackageRepositoryUrl,
        path: &str,
    ) -> reqwest::Result<RepoHtmlPage> {
        let url = url.0.join(path).expect("this should be a valid url");
        let response = self.client.get(url).send().await?;
        let body = response.text().await?;
        Ok(RepoHtmlPage(body))
    }

    pub async fn download_recommended_packages_page(
        &self,
        url: &PackageRepositoryUrl,
        r_version: &str,
    ) -> reqwest::Result<RepoHtmlPage> {
        self.download_repo_html_page(url, &format!("/src/contrib/{}/Recommended/", r_version))
            .await
    }

    pub async fn packages_gz_etag(
        &self,
        url: PackageRepositoryUrl,
    ) -> reqwest::Result<Option<String>> {
        let url = url
            .0
            .join("src/contrib/PACKAGES.gz")
            .expect("this should be a valid url");

        let response = self.client.head(url).send().await?;
        let headers = response.headers();
        let etag = headers
            .get(header::ETAG)
            .map(|x| x.to_str().unwrap())
            .expect("HEAD request to CRAN's should return an etag");

        let etag_chars = etag.chars();

        // parse out the value in quotes
        let mut previous_slash = false;
        let mut lower = usize::MAX;
        let mut upper = usize::MIN;
        for (i, c) in etag_chars.enumerate() {
            if !previous_slash {
                if lower == usize::MAX {
                    if c == '"' {
                        lower = i;
                    }
                } else {
                    if c == '"' {
                        upper = i;
                        break;
                    }
                }
            }

            previous_slash = c == '\\';
        }

        if upper > lower {
            Ok(Some(etag[lower + 1..upper].to_string()))
        } else {
            Ok(None)
        }
    }

    pub async fn download_packages_file(
        &self,
        url: &PackageRepositoryUrl,
    ) -> reqwest::Result<CranPackagesFileDownloadObject> {
        let index = self
            .client
            .get(url.0.join("/src/contrib/PACKAGES.gz").unwrap())
            .send()
            .await?
            .bytes()
            .await?;
        Ok(CranPackagesFileDownloadObject(index))
    }
    pub fn from_recommended(
        &self,
        url: &PackageRepositoryUrl,
        r_version: &str,
        package_name: String,
        package_version: &str,
    ) -> PackageDownload {
        let url = url
            .0
            .join(&format!(
                "/src/contrib/{}/Recommended/{}_{}.tar.gz",
                r_version, package_name, package_version
            ))
            .unwrap();

        PackageDownload {
            name: package_name,
            url,
            client: self.client.clone(),
        }
    }

    pub fn from_main(
        &self,
        url: &PackageRepositoryUrl,
        name: &str,
        version: &RVersion,
    ) -> PackageDownload {
        PackageDownload {
            name: name.to_owned(),
            url: url
                .0
                .join(&format!(
                    "src/contrib/{}_{}.tar.gz",
                    name,
                    version.to_string()
                ))
                .expect("should be a valid URL"),
            client: self.client.clone(),
        }
    }
    pub fn from_archive(
        &self,
        url: &PackageRepositoryUrl,
        name: &str,
        version: &RVersion,
    ) -> PackageDownload {
        PackageDownload {
            name: name.to_owned(),
            url: url
                .0
                .join(&format!(
                    "src/contrib/Archive/{name}/{name}_{}.tar.gz",
                    version.to_string()
                ))
                .expect("should be a valid URL"),
            client: self.client.clone(),
        }
    }

    pub fn package_resource(
        &self,
        url: &PackageRepositoryUrl,
        name: &str,
        version: &RVersion,
        index: &CranDescriptionGrouped,
    ) -> PackageDownload {
        match index.get_package_version(name) {
            Some(ver) if ver == version => self.from_main(url, name, version),
            _ => self.from_archive(url, name, version),
        }
    }
}
pub struct CranPackagesFileDownloadObject(Bytes);

impl CranPackagesFileDownloadObject {
    pub fn decompress(self) -> std::io::Result<CranPackagesFileDownload> {
        let bytes = self.0;
        let mut gz_decode = GzDecoder::new(BufReader::new(bytes.as_ref()));
        let mut index_buf = String::new();
        gz_decode.read_to_string(&mut index_buf)?;
        Ok(CranPackagesFileDownload(index_buf))
    }
}

pub struct CranPackagesFileDownload(String);

impl CranPackagesFileDownload {
    pub fn parse(&self) -> miette::Result<DescriptionFields> {
        let out = cran_description_file_parser::from_str(&self.0);

        match out {
            Ok(res) => Ok(DescriptionFields(res)),
            Err(err) => {
                let result = rich_to_miette(err, self.0.clone());
                eprintln!("{:?}", result);
                panic!("an unexpected error occurred");
            }
        }
    }

    pub fn from_cached(contents: String) -> Self {
        CranPackagesFileDownload(contents)
    }

    pub fn contents(&self) -> &str {
        &self.0
    }
}

pub struct CranRecommendedFileDownload(String);

impl CranRecommendedFileDownload {
    pub fn from_downloaded(workspace: &Path, package_name: &str) -> std::io::Result<Self> {
        let path = workspace
            .join("deliver-r")
            .join(".sources")
            .join("packages")
            .join(package_name)
            .join("DESCRIPTION");

        let contents = fs_err::read_to_string(path)?;
        Ok(Self(contents))
    }

    pub fn parse(&self) -> miette::Result<DescriptionFields> {
        let out = cran_description_file_parser::from_str(&self.0);

        match out {
            Ok(res) => Ok(DescriptionFields(res)),
            Err(err) => {
                let result = rich_to_miette(err, self.0.clone());
                eprintln!("{:?}", result);
                panic!("an unexpected error occurred");
            }
        }
    }
}
pub struct DescriptionFields<'f>(pub Vec<Field<'f>>);

impl<'a> DescriptionFields<'a> {
    pub fn assume_single(&self) -> IndividualPackageFromGrouped<'a, '_> {
        IndividualPackageFromGrouped { fields: &self.0 }
    }
    pub fn package_version_from_name(&self, name: &str) -> Option<&RVersion> {
        let mut current_package = None;
        for field in &self.0 {
            match field {
                Field::Package(p) => {
                    current_package = Some(p);
                }
                Field::Version(v) => match current_package {
                    Some(&current_package) => {
                        if current_package == name {
                            return Some(v);
                        }
                    }
                    None => {}
                },
                _ => {}
            }
        }

        None
    }
    pub fn group_by_package(self) -> CranDescriptionGrouped<'a> {
        let mut packages = vec![];

        let mut current_package = vec![];

        for item in self.0 {
            match item {
                Field::Package(_) => {
                    let replaced = std::mem::take(&mut current_package);
                    packages.push(replaced);
                    current_package.push(item);
                }
                _ => {
                    current_package.push(item);
                }
            }
        }

        CranDescriptionGrouped {
            packages,
            name_lookup: OnceCell::new(),
        }
    }
}

fn filter_non_r(dep: &&Dependency) -> bool {
    dep.name != "R"
}

pub struct RequiredDependencies<'a, 'c> {
    depends: Option<&'c [Dependency<'a>]>,
    imports: Option<&'c [Dependency<'a>]>,
    linking_to: Option<&'c [Dependency<'a>]>,
}

impl<'a, 'c> IntoIterator for RequiredDependencies<'a, 'c> {
    type Item = &'c Dependency<'a>;
    type IntoIter = Chain<
        Chain<
            Filter<Flatten<IntoIter<&'c [Dependency<'a>]>>, fn(&&Dependency<'a>) -> bool>,
            Flatten<IntoIter<&'c [Dependency<'a>]>>,
        >,
        Flatten<IntoIter<&'c [Dependency<'a>]>>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.depends
            .into_iter()
            .flatten()
            .filter(filter_non_r as fn(&&Dependency) -> bool)
            .chain(self.imports.into_iter().flatten())
            .chain(self.linking_to.into_iter().flatten())
    }
}

pub struct IndividualPackageFromGrouped<'a, 'c> {
    pub fields: &'c [Field<'a>],
}

impl<'a, 'c> IndividualPackageFromGrouped<'a, 'c> {
    pub fn system_requirements(&self) -> Option<&'a str> {
        for field in self.fields {
            if let Field::Any {
                key: "SystemRequirements",
                value,
            } = field
            {
                return Some(value);
            }
        }
        None
    }
    pub fn version(&self) -> Option<&RVersion> {
        for field in self.fields {
            if let Field::Version(version) = field {
                return Some(version);
            }
        }
        None
    }
    pub fn imports(&self) -> Option<&'c [Dependency<'a>]> {
        for field in self.fields {
            if let Field::Imports(dep) = field {
                return Some(dep);
            }
        }
        None
    }

    pub fn depends(&self) -> Option<&'c [Dependency<'a>]> {
        for field in self.fields {
            if let Field::Depends(dep) = field {
                return Some(dep);
            }
        }
        None
    }

    pub fn linking_to(&self) -> Option<&'c [Dependency<'a>]> {
        for field in self.fields {
            if let Field::LinkingTo(dep) = field {
                return Some(dep);
            }
        }
        None
    }

    pub fn required_dependencies(&self) -> RequiredDependencies<'a, 'c> {
        RequiredDependencies {
            depends: self.depends(),
            imports: self.imports(),
            linking_to: self.linking_to(),
        }
    }

    pub fn needs_compilation(&self) -> bool {
        for field in self.fields {
            if let Field::NeedsCompilation(b) = field {
                return *b;
            }
        }
        false
    }

    pub fn md5(&self) -> Option<&'a str> {
        for field in self.fields {
            if let Field::Any {
                key: "MD5sum",
                value,
            } = field
            {
                return Some(value);
            }
        }
        None
    }
}

pub struct CranDescriptionGrouped<'a> {
    pub packages: Vec<Vec<Field<'a>>>,
    pub name_lookup: OnceCell<HashMap<String, usize>>,
}

impl<'a> CranDescriptionGrouped<'a> {
    pub fn name_lookup(&self) -> &HashMap<String, usize> {
        self.name_lookup.get_or_init(|| {
            let mut map = HashMap::new();
            for (i, package) in self.packages.iter().enumerate() {
                for field in package {
                    if let Field::Package(name) = field {
                        map.insert(name.to_string(), i);
                        break;
                    }
                }
            }
            map
        })
    }

    pub fn get_package_version(&self, name: &str) -> Option<&RVersion> {
        let index = self.name_lookup().get(name)?;
        let package = &self.packages[*index];

        for field in package {
            if let Field::Version(version) = field {
                return Some(version);
            }
        }

        None
    }

    pub fn get_package<'s>(&'s self, name: &str) -> Option<IndividualPackageFromGrouped<'a, 's>> {
        let index = self.name_lookup().get(name)?;
        Some(IndividualPackageFromGrouped {
            fields: &self.packages[*index],
        })
    }
}

pub struct RepoHtmlPage(String);

impl RepoHtmlPage {
    pub fn parsed(&self) -> ParsedRepoHtmlPage {
        ParsedRepoHtmlPage(Html::parse_document(&self.0))
    }
}

pub struct ParsedRepoHtmlPage(scraper::Html);

impl ParsedRepoHtmlPage {
    pub fn parse_package_listing(&self) -> Vec<(&str, RVersion)> {
        let doc = &self.0;
        // this unwrap is fine, because it, by necessity,
        // should pass a test, and because it's invariant with
        // respect to program data.
        let selector = Selector::parse("td > a").unwrap();
        let mut dests = Vec::new();

        for element in doc.select(&selector) {
            if let Some(href) = element.value().attr("href") {
                if href.ends_with(".tar.gz") {
                    let href = href.trim_end_matches(".tar.gz");

                    let (name, version) = href.split_once("_").unwrap();
                    dests.push((name, RVersion::from_string(version)));
                }
            }
        }

        dests
    }

    pub fn parse_tarballs(&self) -> Vec<String> {
        let doc = &self.0;
        // this unwrap is fine, because it, by necessity,
        // should pass a test, and because it's invariant with
        // respect to program data.
        let selector = Selector::parse("td > a").unwrap();
        let mut dests = Vec::new();

        for element in doc.select(&selector) {
            if let Some(href) = element.value().attr("href") {
                if href.ends_with(".tar.gz") {
                    dests.push(href.to_owned());
                }
            }
        }

        dests
    }
}

#[tokio::test]
pub async fn test_recommended() -> miette::Result<()> {
    let repo_url = PackageRepositoryUrl::new("https://cran.r-project.org");

    let downloader = PackageRepoDownloader::new();

    let page = downloader
        .download_repo_html_page(&repo_url, "/src/contrib/4.3.2/Recommended/")
        .await
        .into_diagnostic()?
        .parsed();

    dbg!(page.parse_package_listing());
    Ok(())
}

pub struct MD5Digest([u8; 16]);

impl MD5Digest {
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
    pub fn as_arr(&self) -> &[u8; 16] {
        &self.0
    }
    pub fn to_string(&self) -> String {
        hex_encode(&self.0)
    }

    pub fn from_hex(str: &str) -> MD5Digest {
        let mut out = [0; 16];
        let mut i = 0;
        let mut j = 0;
        while i < 32 {
            let byte = u8::from_str_radix(&str[i..i + 2], 16).unwrap();
            out[j] = byte;
            i += 2;
            j += 1;
        }
        MD5Digest(out)
    }
}

pub(self) fn hex_encode(slice: &[u8]) -> String {
    let mut str = String::new();

    for byte in slice {
        let suff = byte & 0b1111;
        let suff_char = match suff {
            0 => '0',
            1 => '1',
            2 => '2',
            3 => '3',
            4 => '4',
            5 => '5',
            6 => '6',
            7 => '7',
            8 => '8',
            9 => '9',
            10 => 'a',
            11 => 'b',
            12 => 'c',
            13 => 'd',
            14 => 'e',
            15 => 'f',
            _ => unreachable!(),
        };

        let pref = (byte >> 4) & 0b1111;
        let pref_char = match pref {
            0 => '0',
            1 => '1',
            2 => '2',
            3 => '3',
            4 => '4',
            5 => '5',
            6 => '6',
            7 => '7',
            8 => '8',
            9 => '9',
            10 => 'a',
            11 => 'b',
            12 => 'c',
            13 => 'd',
            14 => 'e',
            15 => 'f',
            _ => unreachable!(),
        };

        str.push(pref_char);
        str.push(suff_char);
    }
    return str;
}

#[cfg(test)]
mod test {
    #[test]
    fn test_hex_computer() {
        assert_eq!(super::hex_encode(&[255, 155, 0]), "ff9b00")
    }
}
