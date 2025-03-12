use miette::{bail, IntoDiagnostic};
use toml_edit::table;

const R_PROJ_STR: &'static str = r#"Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX

AutoAppendNewline: Yes
StripTrailingWhitespace: Yes
LineEndingConversion: Posix"#;

pub(crate) fn init(name: &str, rstudio: bool) -> miette::Result<()> {
    if rstudio {
        let r_studio_manifest = std::path::PathBuf::from(&format!("{name}.Rproj"));

        if r_studio_manifest.exists() {
            bail!("can't create {name}.Rproj here, becuase it already exists. Not going to create this project");
        };

        std::fs::write(r_studio_manifest, R_PROJ_STR).into_diagnostic()?;
    }

    let manifest_location = std::path::PathBuf::from("deliver.toml");

    if manifest_location.exists() {
        bail!("Can't create a project here. deliver.toml already exists");
    };

    let mut deliver_manifest = toml_edit::DocumentMut::new();

    deliver_manifest.insert("package", table());

    deliver_manifest["package"]["name"] = name.into();
    deliver_manifest["package"]["version"] = "0.1.0".into();
    deliver_manifest["package"]["RVersion"] = "4.3.2".into();

    deliver_manifest.insert("dependencies", table());

    std::fs::write("deliver.toml", deliver_manifest.to_string()).into_diagnostic()?;
    Ok(())
}
