use std::str::FromStr;

pub fn save_cran_packages_file(
    etag: &str,
    index_url: &str,
    contents: &str,
    workspace: &std::path::Path,
) -> std::io::Result<()> {
    let mut doc = toml_edit::DocumentMut::new();

    doc.insert("repo", toml_edit::array());

    doc["repo"].as_array_of_tables_mut().unwrap().push({
        let mut table = toml_edit::Table::new();
        table["etag"] = etag.into();
        table["index_url"] = index_url.into();
        table["contents"] = contents.into();
        table
    });

    fs_err::create_dir_all(workspace.join("deliver-r/cache/"))?;
    fs_err::write(
        workspace.join("deliver-r/cache/").join("cran-index.toml"),
        doc.to_string(),
    )?;
    Ok(())
}

pub fn load_cran_packages_file(
    etag: &str,
    index_url: &str,
    workspace: &std::path::Path,
) -> std::io::Result<Option<String>> {
    let index_cache_file_path = workspace.join("deliver-r/cache/").join("cran-index.toml");

    if !index_cache_file_path.exists() {
        return Ok(None);
    }

    let doc = fs_err::read_to_string(index_cache_file_path)?;
    let doc = toml_edit::DocumentMut::from_str(&doc).unwrap();

    let repo = doc.get("repo").unwrap().as_array_of_tables().unwrap();

    for entry in repo {
        if entry.get("etag").unwrap().as_str().unwrap() == etag
            && entry.get("index_url").unwrap().as_str().unwrap() == index_url
        {
            return Ok(Some(
                entry.get("contents").unwrap().as_str().unwrap().to_string(),
            ));
        }
    }

    Ok(None)
}
