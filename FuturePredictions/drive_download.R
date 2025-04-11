# install.packages("googledrive")
# install.packages("fs")
rm(list=ls())
gc()

library(googledrive)
library(fs)
library(dplyr)
library(purrr)

drive_auth(
  scopes = "https://www.googleapis.com/auth/drive"
)

folder_id <- as_id("1w3ywfQSS23JSUZFmVG6otpaq1VSjGyuw")

get_all_files_recursive <- function(folder_id, parent_path = "") {
  # Ensure ID is treated as such, not as a path string
  folder <- as_id(folder_id)
  items <- drive_ls(folder)
  
  # Separate files and folders
  files <- items %>% 
    filter(map_chr(drive_resource, "mimeType") != "application/vnd.google-apps.folder")
  
  folders <- items %>% 
    filter(map_chr(drive_resource, "mimeType") == "application/vnd.google-apps.folder")
  
  # Adjust file names to include full path
  files$name <- file.path(parent_path, files$name)
  
  # Recursively handle subfolders
  for (i in seq_len(nrow(folders))) {
    subfolder <- folders[i, ]
    cat("📁 Entering subfolder:", file.path(parent_path, subfolder$name), "\n")
    
    subfiles <- get_all_files_recursive(subfolder$id, file.path(parent_path, subfolder$name))
    files <- bind_rows(files, subfiles)
  }
  
  return(files)
}

all_files <- get_all_files_recursive("1w3ywfQSS23JSUZFmVG6otpaq1VSjGyuw")

failed_downloads <- list()

for (i in seq_len(nrow(all_files))) {
  local_path <- path("C:/Data/Future Projections CPS SC-GAMs ROMS Domain", all_files$name[i])
  dir_create(path_dir(local_path))
  
  message("⬇️ Downloading: ", all_files$name[i])
  
  tryCatch({
    drive_download(file = all_files[i, ], path = local_path, overwrite = TRUE)
    Sys.sleep(1)  # optional: wait 1 sec between files
  }, error = function(e) {
    message("❌ Failed to download: ", all_files$name[i])
    failed_downloads[[length(failed_downloads) + 1]] <- list(
      name = all_files$name[i],
      id = all_files$id[i],
      error = e$message
    )
  })
}

length(failed_downloads)
# To inspect:
failed_downloads[[1]]

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


