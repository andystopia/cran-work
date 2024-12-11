use std::collections::HashMap;

use cran_description_file_parser::{rich_to_miette, Field};
use crancherry::Cran;
use fs_err::PathExt;
use miette::{ensure, IntoDiagnostic};
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct Config {
    pub crater: Crater,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct Crater {
    pub host: String,
    pub description_file_dir: String,
    pub substituter: Substituter,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct Substituter {
    #[serde(rename = "description_file")]
    description_file: DescriptionFileSubsituter,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct DescriptionFileSubsituter {
    pub urls: Vec<String>,
}

pub fn get_description_file(
    cran_package: &crancherry::CranPackageDownloadUrl,
    config: &Config,
) -> miette::Result<String> {
    let urls = &config.crater.substituter.description_file.urls;

    for url in urls {
        let url = url.replace("{package_name}", &cran_package.name);

        println!(
            "Trying to get {} DESCRIPTION from {}",
            cran_package.name, url
        );
        let response = ureq::get(&url).call();

        match response {
            Ok(ok) => {
                let contents = ok.into_string().into_diagnostic()?;

                println!("Got the DESCRIPTION file from the substituter ({})", url);
                return Ok(contents);
            }
            Err(err) => match err {
                ureq::Error::Status(404, _) => continue,
                x => return Err(x).into_diagnostic()?,
            },
        }
    }

    println!("Falling back to requesting the DESCRIPTION file directly from the CRAN");

    Ok(cran_package
        .download_decompressed()?
        .description_file()?
        .contents()
        .to_owned())
}

pub fn main() -> miette::Result<()> {
    let crater_config = fs_err::read_to_string("crates/description-file-crater/crater-config.toml")
        .into_diagnostic()?;
    let config: Config = toml::from_str(&crater_config).into_diagnostic()?;

    // make sure the output directory exists
    ensure!(
        std::path::Path::new(&config.crater.description_file_dir)
            .fs_err_try_exists()
            .into_diagnostic()? == true,
        "Output DESCRIPTION file directory (`{}`) does not exist.", &config.crater.description_file_dir
    );

    let cran = Cran::with_host(config.crater.host.clone());

    let available = cran.available_with_url()?;

    
    for (i, package) in available.iter().enumerate() {
        let output_path = format!("{}/{}", config.crater.description_file_dir, package.name);
        if std::path::Path::new(&output_path)
            .fs_err_try_exists()
            .into_diagnostic()?
        {
            continue;
        }

        println!(
            "[{}/{}] Downloading {}, because it's not in the cache.",
            i + 1,
            available.len(),
            package.name
        );

        let description_file = get_description_file(package, &config)?;

        fs_err::write(&output_path, description_file).into_diagnostic()?;
    }


    println!("Let's start testing each file!");

    let mut errors = HashMap::new();
    let mut successes = 0;
    let mut failures = 0;
    let mut minor_failures = 0;
    for package in &available {
        let output_path = format!("{}/{}", config.crater.description_file_dir, package.name);

        let description_file = fs_err::read_to_string(&output_path).into_diagnostic()?;

        match cran_description_file_parser::from_str(&description_file) {
            Ok(ok) => {
                for field in ok { 
                    if let Field::ErrorLine(err) = field { 
                        eprintln!("Error in {}: failed to parse line: {}", package.name, err);
                        minor_failures += 1;
                    }
                }
                successes += 1;
            },
            Err(errs) => {
                failures += 1;
                errors.insert(package.name.clone(), rich_to_miette(errs, description_file.clone()));
            },
        }
    }

    println!("[{}/{}] packages parsed successfully (w/ {} minor failures)", successes, failures + successes, minor_failures);
    Ok(())
}
