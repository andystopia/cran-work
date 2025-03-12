use color_eyre::eyre::ContextCompat;
use scraper::Selector;
use std::collections::{HashMap, HashSet};
use std::fs::OpenOptions;
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use tokio::sync::Semaphore;
use tokio::task::JoinSet;

use color_eyre::eyre::bail;
use tracing::{error, info};

const MAX_CRAN_CONCURRENT_DOWNLOAD: usize = 64;
const GITHUB_DOWNLOAD_WAIT_MS: u64 = 5;

pub async fn cran_scrape_package_file_names(
    client: reqwest::Client,
    url: &str,
    icon: &str,
) -> color_eyre::Result<Vec<String>> {
    let response = client.get(url).send().await?.text().await?;
    let document = scraper::Html::parse_document(&response);

    // Select the first table
    let table_selector = Selector::parse("table").unwrap();
    let Some(table) = document.select(&table_selector).next() else {
        error!(response);
        bail!("{url} didn't have a table");
    };

    // Select all rows in the table
    let row_selector = Selector::parse("tr").unwrap();
    let rows = table.select(&row_selector);

    // Prepare selector for columns td
    let cell_selector = Selector::parse("td").unwrap();

    let img_selector = Selector::parse("img").unwrap();

    // Collect the second column
    let mut second_column = Vec::new();
    for row in rows {
        let cells: Vec<_> = row.select(&cell_selector).collect();
        if cells.len() >= 2 {
            let first_cell = cells[0];

            let is_compressed = first_cell
                .select(&img_selector)
                .any(|img| img.value().attr("src").map_or(false, |src| src == icon));

            if is_compressed {
                let package_filename = cells[1].text().collect::<String>().trim().to_string();

                second_column.push(package_filename);
            }
        }
    }

    Ok(second_column)
}

#[derive(Debug)]
pub struct RCranPackageIndexInfo {
    package_name: String,
    package_versions: Vec<String>,
}

pub async fn scrape_archives(
    client: reqwest::Client,
    packages: &[String],
) -> color_eyre::Result<()> {
    let mut joinset: JoinSet<color_eyre::Result<RCranPackageIndexInfo>> = JoinSet::new();
    let semaphore = Arc::new(Semaphore::new(MAX_CRAN_CONCURRENT_DOWNLOAD));

    let packages_len = packages.len();
    for (i, package) in packages.into_iter().enumerate() {
        let i = i.clone();
        let client = client.clone();
        let semaphore = semaphore.clone();
        let package = package.clone();
        joinset.spawn(async move {
            let sem = semaphore.acquire().await.unwrap();
            info!("Getting versions of `{package}` [{i}/{}]", packages_len);
            let package = package.trim_end_matches("/").trim_start_matches("/");

            let scraped = cran_scrape_package_file_names(
                client.clone(),
                &format!("https://cran.r-project.org/src/contrib/Archive/{package}/"),
                "/icons/compressed.gif",
            )
            .await?;

            let mut versions = Vec::new();
            for entry in scraped {
                let version = package_version_from_full_name(&entry, package);
                versions.push(version.to_owned());
            }

            let info = RCranPackageIndexInfo {
                package_name: package.to_owned(),
                package_versions: versions,
            };

            drop(sem);
            Ok(info)
        });
    }

    while let Some(pkg_info) = joinset.join_next().await {
        write_package_info_to_index(&pkg_info??)?;
    }

    Ok(())
}

fn write_package_info_to_index(pkg: &RCranPackageIndexInfo) -> Result<(), color_eyre::eyre::Error> {
    let mut opts = OpenOptions::new()
        .append(true)
        .create(true)
        .open("package_index.txt")?;
    writeln!(
        opts,
        "{}_{}",
        pkg.package_name,
        pkg.package_versions.join("##")
    )?;
    Ok(())
}

fn package_version_from_full_name<'e>(filename: &'e str, packagename: &str) -> &'e str {
    filename
        .trim_start_matches(packagename)
        .trim_start_matches("_")
        .trim_end_matches(".tar.gz")
        .trim_end_matches(".tar.xz")
}

fn package_parsed_from_full_name<'e>(filename: &'e str) -> Option<(&'e str, &'e str)> {
    let (filename, version_and_ext) = filename.split_once('_')?;

    let version = version_and_ext
        .strip_suffix(".tar.gz")
        .or(version_and_ext.strip_suffix(".tar.xz"))?;

    Some((filename, version))
}

pub fn load_package_index() -> color_eyre::Result<Vec<RCranPackageIndexInfo>> {
    let file = std::fs::File::open("package_index.txt")?;
    let bufreader = BufReader::new(file);

    let mut packages_and_versions: HashMap<_, HashSet<_>> = HashMap::new();

    for line in bufreader.lines() {
        let line = line?;
        let (package_name, versions) = line.split_once('_').context("couldn't split line")?;
        for version in versions.split("##").map(|v| v.to_owned()) {
            packages_and_versions
                .entry(package_name.to_owned())
                .or_default()
                .insert(version.to_owned());
        }
    }

    let mut packages = packages_and_versions
        .into_iter()
        .map(|(name, versions)| RCranPackageIndexInfo {
            package_name: name,
            package_versions: versions.into_iter().collect::<Vec<_>>(),
        })
        .collect::<Vec<_>>();

    // sort by package name
    packages.sort_unstable_by(|a, b| a.package_name.cmp(&b.package_name));

    Ok(packages)
}
pub fn coalesce_index() -> color_eyre::Result<Vec<RCranPackageIndexInfo>> {
    let items = load_package_index()?;

    {
        OpenOptions::new()
            .write(true)
            .truncate(true)
            .open("package_index.txt")?;
    }

    for info in &items {
        write_package_info_to_index(info)?;
    }
    Ok(items)
}
// pub fn get_already_downloaded_information_full() -> color_eyre::Result<Vec<RCranPackageIndexInfo>> {
//     let already_got = std::fs::read_to_string("package_index_info.jsonld")?;

//     let mut all = Vec::new();
//     for line in already_got.lines() {
//         let info: RCranPackageIndexInfo = serde_json::from_str(line)?;
//         all.push(info);
//     }
//     Ok(all)
// }

pub async fn download_description_file_from_github(
    client: reqwest::Client,
    package: &str,
    version: &str,
) -> color_eyre::Result<String> {
    let req = client
        .get(&format!(
            "https://raw.githubusercontent.com/cran/{package}/refs/tags/{version}/DESCRIPTION"
        ))
        .send()
        .await?;
    let text = req.text().await?;
    Ok(text)
}

pub fn create_dir_if_not_exists(path: &std::path::Path) -> std::io::Result<()> {
    if !path.exists() {
        std::fs::create_dir(path)?;
    }
    Ok(())
}

async fn cran_scrape_latest(
    client: &reqwest::Client,
    cran_contrib_url: &str,
) -> Result<Vec<(String, String)>, color_eyre::eyre::Error> {
    let latest_packages =
        cran_scrape_package_file_names(client.clone(), cran_contrib_url, "/icons/compressed.gif")
            .await?;
    let mut latest_packages_parsed = Vec::new();
    for package in latest_packages {
        let Some((name, version)) = package_parsed_from_full_name(&package) else {
            error!("Couldn't parse {package} into a package name and version");
            continue;
        };

        latest_packages_parsed.push((name.to_owned(), version.to_owned()));
    }
    Ok(latest_packages_parsed)
}

pub fn retrieve_git_user_email() -> color_eyre::Result<String> {
    let output = Command::new("git")
        .args(&["config", "user.email"])
        .output()?;

    if !output.status.success() {
        bail!(
            "Failed to retrieve Git user email: git exited with status {}",
            output.status
        );
    }

    let email = String::from_utf8(output.stdout)?.trim().to_owned();

    if email.is_empty() {
        bail!("Git user email is not configured, but it should be for the sake of abuse.");
    }

    Ok(email)
}
pub async fn lib_main() -> color_eyre::Result<()> {
    tracing_subscriber::fmt().init();

    let git_email_addr = retrieve_git_user_email()?;

    let client = reqwest::ClientBuilder::new()
            .user_agent(format!("andystopia project trying to make R packaging better. Please contact {git_email_addr} for abuse. I hope / expect to make about 250,000K requests"))
            .build()?;

    info!("Scraping latest packages");
    let latest_packages =
        cran_scrape_latest(&client, "https://cran.r-project.org/src/contrib/").await?;

    for (package_name, version) in latest_packages {
        write_package_info_to_index(&RCranPackageIndexInfo {
            package_name,
            package_versions: vec![version],
        })?;
    }

    let archive_packages = cran_scrape_package_file_names(
        client.clone(),
        "https://cran.r-project.org/src/contrib/Archive/",
        "/icons/folder.gif",
    )
    .await?;

    info!("Downloading Package Information");
    scrape_archives(client.clone(), &archive_packages).await?;

    info!("Loading all version information");

    let infos = coalesce_index()?;

    info!("Downloading DESCRIPTION files from GitHub");
    create_dir_if_not_exists("cran_description_files".as_ref())?;

    for info in infos {
        create_dir_if_not_exists(
            format!("./cran_description_files/{}", info.package_name).as_ref(),
        )?;

        info!(
            "Downloading {} DESCRIPTIONS for {}...",
            info.package_versions.len(),
            info.package_name
        );

        for version in info.package_versions {
            let description_path: PathBuf = format!(
                "./cran_description_files/{}/{}/DESCRIPTION",
                info.package_name, version
            )
            .into();

            if description_path.exists() {
                continue;
            }

            create_dir_if_not_exists(
                format!("./cran_description_files/{}/{}", info.package_name, version).as_ref(),
            )?;

            let downloaded =
                download_description_file_from_github(client.clone(), &info.package_name, &version)
                    .await?;

            std::fs::write(description_path, downloaded)?;

            std::thread::sleep(std::time::Duration::from_millis(GITHUB_DOWNLOAD_WAIT_MS));
        }
    }

    Ok(())
}
