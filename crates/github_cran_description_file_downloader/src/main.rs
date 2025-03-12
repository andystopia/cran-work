use github_cran_description_file_downloader::lib_main;

#[tokio::main]
pub async fn main() -> color_eyre::Result<()> {
    lib_main().await?;
    Ok(())
}
