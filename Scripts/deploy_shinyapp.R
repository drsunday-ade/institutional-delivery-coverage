#!/usr/bin/env Rscript
# ============================================================
# deploy_shinyapp.R
# Reproducible end-to-end Shiny deployment to shinyapps.io
#
# What it does:
#  1) Validates the Shiny app folder structure and required files
#  2) (Optional) Runs app locally for a quick check
#  3) Sets shinyapps.io account credentials (rsconnect::setAccountInfo)
#  4) Deploys the app with rsconnect::deployApp
#     - If standard deploy fails, retries with explicit appFiles
#  5) Writes a deployment log + sessionInfo to output/logs/
#
# Requirements:
#  - Your app folder exists at: output/shiny_app/
#  - Inside it: app.R, data/policy_data.rds, www/style.css, www/custom.js, www/*.png
#
# Usage:
#  Rscript deploy_shinyapp.R
#
# Optional env vars:
#  APP_DIR        (default: output/shiny_app)
#  APP_NAME       (default: who-mmr-policy-explorer)
#  ACCOUNT_NAME   (default: setrojaboard)
#  RUN_LOCAL      (default: 0; set to 1 to run locally before deploy)
# ============================================================

options(stringsAsFactors = FALSE)

# ---------------------------
# 0) Resolve project root
# ---------------------------
get_script_dir <- function(){
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if(length(file_arg) == 1){
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  return(getwd())
}
ROOT <- get_script_dir()

APP_DIR      <- Sys.getenv("APP_DIR",      unset = file.path(ROOT, "output", "shiny_app"))
APP_NAME     <- Sys.getenv("APP_NAME",     unset = "who-mmr-policy-explorer")
ACCOUNT_NAME <- Sys.getenv("ACCOUNT_NAME", unset = "setrojaboard")
RUN_LOCAL    <- as.integer(Sys.getenv("RUN_LOCAL", unset = "0"))

# ---------------------------
# 1) Logging
# ---------------------------
LOG_DIR <- file.path(ROOT, "output", "logs")
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

log_path <- file.path(LOG_DIR, "deploy_shinyapp_log.txt")
sink_con <- file(log_path, open = "wt")
sink(sink_con, split = TRUE)
on.exit({
  cat("\nDeploy script ended:", as.character(Sys.time()), "\n")
  try(sink(), silent = TRUE)
  try(close(sink_con), silent = TRUE)
}, add = TRUE)

cat("Deploy script started:", as.character(Sys.time()), "\n")
cat("ROOT:", ROOT, "\n")
cat("APP_DIR:", APP_DIR, "\n")
cat("APP_NAME:", APP_NAME, "\n")
cat("ACCOUNT_NAME:", ACCOUNT_NAME, "\n")
cat("RUN_LOCAL:", RUN_LOCAL, "\n\n")

# ---------------------------
# 2) Packages
# ---------------------------
req <- c("rsconnect","shiny")
missing <- req[!req %in% rownames(installed.packages())]
if(length(missing)){
  install.packages(missing, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(library(rsconnect))
suppressPackageStartupMessages(library(shiny))

# ---------------------------
# 3) Validate app structure (Step 1)
# ---------------------------
stop_if_missing <- function(paths){
  missing <- paths[!file.exists(paths)]
  if(length(missing)){
    stop("Missing required file(s):\n- ", paste(missing, collapse = "\n- "))
  }
}

if(!dir.exists(APP_DIR)) stop("App directory does not exist: ", APP_DIR)

app_r   <- file.path(APP_DIR, "app.R")
rds     <- file.path(APP_DIR, "data", "policy_data.rds")
css     <- file.path(APP_DIR, "www", "style.css")
js      <- file.path(APP_DIR, "www", "custom.js")
pngs    <- list.files(file.path(APP_DIR, "www"), pattern = "\\.png$", full.names = TRUE)

stop_if_missing(c(app_r, rds, css, js))

if(length(pngs) < 1){
  stop("No PNG files found in: ", file.path(APP_DIR, "www"),
       "\nExpected your 5 figures copied into www/ for display.")
}

cat("App structure OK.\n")
cat("Found PNGs:", length(pngs), "\n\n")

# ---------------------------
# 4) Optional local run (Step 2)
# ---------------------------
if(RUN_LOCAL == 1){
  cat("RUN_LOCAL=1 -> launching app locally now.\n")
  cat("Close the app window / stop the session to continue to deployment.\n\n")
  shiny::runApp(APP_DIR, launch.browser = TRUE)
  cat("\nLocal run completed (or stopped). Continuing to deployment...\n\n")
}

# ---------------------------
# 5) Configure shinyapps.io account (Step 3)
# IMPORTANT: If you prefer NOT to hardcode token/secret in a file,
# set them as environment variables and read via Sys.getenv instead.
# ---------------------------
# Option A (hardcoded, as you provided):
TOKEN  <- Sys.getenv("SHINYAPPS_TOKEN", unset = "D4EECE2E740E4DAFC58AC17F2E7286C6")
SECRET <- Sys.getenv("SHINYAPPS_SECRET", unset = "nsrR31whjWdsmHy1Ek/exT/5oiYGr1srwkPxG/wJ")

# Minimal sanity checks
if(nchar(TOKEN) < 10 || nchar(SECRET) < 10){
  stop("Token/secret look invalid. Set env vars SHINYAPPS_TOKEN and SHINYAPPS_SECRET or edit this script.")
}

rsconnect::setAccountInfo(
  name   = ACCOUNT_NAME,
  token  = TOKEN,
  secret = SECRET
)

cat("Account info set for:", ACCOUNT_NAME, "\n\n")

# ---------------------------
# 6) Deploy (Step 4)
#    - First try standard deployApp
#    - If it fails, retry with explicit appFiles
# ---------------------------
deploy_try <- function(){
  rsconnect::deployApp(
    appDir = APP_DIR,
    appName = APP_NAME,
    account = ACCOUNT_NAME,
    forceUpdate = TRUE
  )
}

deploy_try_explicit <- function(){
  rsconnect::deployApp(
    appDir = APP_DIR,
    appName = APP_NAME,
    account = ACCOUNT_NAME,
    appFiles = c(
      "app.R",
      file.path("data","policy_data.rds"),
      file.path("www","style.css"),
      file.path("www","custom.js"),
      file.path("www", basename(pngs))
    ),
    forceUpdate = TRUE
  )
}

cat("Starting deployment to shinyapps.io...\n")
deploy_ok <- FALSE

res <- tryCatch({
  deploy_try()
  deploy_ok <<- TRUE
  "Standard deployment succeeded."
}, error = function(e){
  paste("Standard deployment failed:", conditionMessage(e))
})

cat(res, "\n\n")

if(!deploy_ok){
  cat("Retrying with explicit appFiles...\n")
  res2 <- tryCatch({
    deploy_try_explicit()
    deploy_ok <<- TRUE
    "Explicit-files deployment succeeded."
  }, error = function(e){
    paste("Explicit-files deployment failed:", conditionMessage(e))
  })
  cat(res2, "\n\n")
}

if(!deploy_ok){
  stop("Deployment failed. See log: ", log_path,
       "\nTip: Check that your account name is correct and that the appDir is the folder containing app.R.")
}

cat("Deployment finished successfully.\n")
cat("In the Console output above, rsconnect usually prints the live URL.\n\n")

# ---------------------------
# 7) Reproducibility: sessionInfo
# ---------------------------
si <- capture.output(sessionInfo())
writeLines(si, con = file.path(LOG_DIR, "deploy_sessionInfo.txt"))
cat("Saved sessionInfo to:", file.path(LOG_DIR, "deploy_sessionInfo.txt"), "\n")
cat("Saved deploy log to:", log_path, "\n")
cat("Done.\n")