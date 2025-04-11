# install.packages("googledrive")
# install.packages("fs")
# install.packages("furrr")
# install.packages("future")
rm(list = ls())
gc()

library(googledrive)
library(fs)
library(dplyr)
library(purrr)
library(furrr)
library(future)

# 🧠 Use all cores (or set manually like workers = 4)
plan(multisession, workers = parallel::detectCores() - 1)

# 🔐 Authenticate with editable scope
drive_auth(scopes = "https://www.googleapis.com/auth/drive")

# 🗂️ Target Google Drive folder
folder_id <- as_id("1w3ywfQSS23JSUZFmVG6otpaq1VSjGyuw")

# 🔄 Recursively get all files from nested folders
get_all_files_recursive <- function(folder_id, parent_path = "") {
  folder <- as_id(folder_id)
  items <- drive_ls(folder)
  
  files <- items %>%
    filter(map_chr(drive_resource, "mimeType") != "application/vnd.google-apps.folder")
  
  folders <- items %>%
    filter(map_chr(drive_resource, "mimeType") == "application/vnd.google-apps.folder")
  
  files$name <- file.path(parent_path, files$name)
  
  for (i in seq_len(nrow(folders))) {
    subfolder <- folders[i, ]
    cat("📁 Entering subfolder:", file.path(parent_path, subfolder$name), "\n")
    
    subfiles <- get_all_files_recursive(subfolder$id, file.path(parent_path, subfolder$name))
    files <- bind_rows(files, subfiles)
  }
  
  return(files)
}

# 📦 List all files to download
all_files <- get_all_files_recursive("1w3ywfQSS23JSUZFmVG6otpaq1VSjGyuw")

# 📥 Download in parallel with resume support
download_files_parallel <- function(files_df, dest_dir = "C:/Data/Future Projections CPS SC-GAMs ROMS Domain") {
  failed <- list()
  
  future_walk(seq_len(nrow(files_df)), function(i) {
    file_row <- files_df[i, ]
    local_path <- fs::path(dest_dir, file_row$name)
    
    if (fs::file_exists(local_path)) {
      message("✅ Already downloaded: ", file_row$name)
      return(NULL)
    }
    
    fs::dir_create(fs::path_dir(local_path))
    
    tryCatch({
      googledrive::drive_download(file = file_row, path = local_path, overwrite = FALSE)
      message("⬇️ Downloaded: ", file_row$name)
    }, error = function(e) {
      message("❌ Failed to download: ", file_row$name)
      failed[[length(failed) + 1]] <<- list(name = file_row$name, id = file_row$id, error = e$message)
    })
  }, .options = furrr_options(seed = TRUE))
  
  return(failed)
}

# 🚀 Run the parallel downloader
failed_downloads <- download_files_parallel(all_files)

# 🛠️ Retry any failed downloads sequentially
if (length(failed_downloads) > 0) {
  for (f in failed_downloads) {
    message("🔁 Retrying: ", f$name)
    local_path <- path("C:/Data/Future Projections CPS SC-GAMs ROMS Domain", f$name)
    dir_create(path_dir(local_path))
    
    tryCatch({
      drive_download(file = as_id(f$id), path = local_path, overwrite = TRUE)
      Sys.sleep(1)
    }, error = function(e) {
      message("🚫 Still failed: ", f$name)
    })
  }
}
