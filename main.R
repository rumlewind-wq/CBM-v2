
setwd("/Users/rumlewind/Desktop/CBS/Eksamensforberedelse/Commerical banking management/Probanker/R MAIN")
message("Working directory set to: ", getwd())

# --- Define your two inputs relative to the script location ---
# Save your files next to this script as:
#   - manifest.yml         (Wthe YAML manifest)
#   - report.txt       (the raw CSV text EXACTLY as pasted)
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
library(readr)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(tidyr)


manifest_path <- file.path(getwd(), "manifest.yml")
# Vælg nyeste .txt i arbejdsmappen
txt_files <- list.files(getwd(), pattern = "\\.txt$", full.names = TRUE)
stopifnot(length(txt_files) > 0)
csv_path <- txt_files[which.max(file.info(txt_files)$mtime)]
message("Indlæser nyeste .txt: ", csv_path)

manifest_yaml <- read_file(manifest_path)
csv_text      <- read_file(csv_path)

stopifnot(file.exists(manifest_path), file.exists(csv_path))

manifest_yaml <- read_file(manifest_path)  # full YAML as a single string
csv_text      <- read_file(csv_path)       # full CSV text as a single string

# --- Load and run the parser (assumes csv_parser.R sits next to these files) ---
# csv_parser.R must define parse_pbreport() (your previously saved parser script).
parser_path <- file.path(getwd(), "csv_parser.R")
stopifnot(file.exists(parser_path))
source(parser_path, encoding = "UTF-8")

res <- parse_pbreport(csv_text, manifest_yaml, strict = TRUE, return_long = TRUE)

# Quick sanity checks
print(names(res$tables))
source("pb_helpers_core.R", chdir = TRUE, encoding = "UTF-8")
cat("Nyeste .txt fil:", basename(csv_path), "\n")
