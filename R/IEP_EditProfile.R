#' Edit or Create R Profile Configuration
#'
#' Opens the user's .Rprofile file for editing. If it doesn't exist, creates it
#' with helpful starter content. Provides guidance on common .Rprofile uses.
#'
#' @param scope Character. Either "user" for ~/.Rprofile or "project" for 
#'   project-specific .Rprofile in current directory. Default is "user".
#' @param create_backup Logical. Should a backup be created before editing? Default FALSE.
#' @param add_template Logical. If file doesn't exist, add helpful template? Default FALSE.
#'
#' @return Invisibly returns the path to the .Rprofile file
#' @export
#'
#' @examples
#' \dontrun{
#' # Edit user's global .Rprofile
#' IEP_EditProfile()
#' 
#' # Edit project-specific .Rprofile
#' IEP_EditProfile(scope = "project")
#' 
#' # Edit with backup
#' IEP_EditProfile(create_backup = TRUE)
#' 
#' # Create new file with template
#' IEP_EditProfile(add_template = TRUE)
#' }
IEP_EditProfile <- function(scope = c("user", "project"), 
                           create_backup = FALSE,
                           add_template = FALSE) {
  
  scope <- match.arg(scope)
  
  # Determine file path
  if (scope == "user") {
    profile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")
    profile_type <- "user"
  } else {
    profile_path <- file.path(getwd(), ".Rprofile")
    profile_type <- "project"
  }
  
  # Check if file exists
  file_exists <- file.exists(profile_path)
  
  # Create backup if requested and file exists
  if (create_backup && file_exists) {
    backup_path <- paste0(profile_path, ".backup_", format(Sys.Date(), "%Y%m%d"))
    file.copy(profile_path, backup_path, overwrite = FALSE)
    message("Backup created: ", backup_path)
  }
  
  # If file doesn't exist and template requested, create with starter content
  if (!file_exists && add_template) {
    template <- c(
      "# ========================================",
      paste0("# R Profile (", profile_type, ")"),
      paste0("# Created: ", Sys.Date()),
      "# ========================================",
      "",
      "# Load frequently used packages on startup",
      "if (interactive()) {",
      "  # suppressMessages({",
      "  #   library(tidyverse)",
      "  #   library(researchIEP)",
      "  # })",
      "}",
      "",
      "# Set CRAN mirror",
      "options(repos = c(CRAN = 'https://cran.rstudio.com/'))",
      "",
      "# Set default options",
      "options(",
      "  stringsAsFactors = FALSE,",
      "  max.print = 100,",
      "  scipen = 999,  # Avoid scientific notation",
      "  width = 120    # Wider console output",
      ")",
      "",
      "# Custom prompt (optional)",
      "# options(prompt = '> ', continue = '+ ')",
      "",
      "# Set working directory (project-specific only)",
      if (scope == "project") "# setwd(getwd())" else "# setwd('~/Projects')",
      "",
      "# Load custom functions",
      "# if (file.exists('~/R/custom_functions.R')) {",
      "#   source('~/R/custom_functions.R')",
      "# }",
      "",
      "# Welcome message",
      "if (interactive()) {",
      "  cat('\\n=============================================\\n')",
      "  cat('R version:', R.version.string, '\\n')",
      "  cat('Working directory:', getwd(), '\\n')",
      "  cat('Date:', format(Sys.Date(), '%A, %B %d, %Y'), '\\n')",
      "  cat('=============================================\\n\\n')",
      "}",
      "",
      "# IEP-specific settings",
      "# Sys.setenv(IEP_USERPATH = '~/IEP_Projects')",
      "",
      "# First-run message for new setup",
      "# if (interactive() && !file.exists('~/.Rprofile_configured')) {",
      "#   cat('Remember to customize your .Rprofile!\\n')",
      "#   cat('Run IEP_EditProfile() to edit this file.\\n')",
      "#   file.create('~/.Rprofile_configured')",
      "# }"
    )
    
    writeLines(template, profile_path)
    message("Created new .Rprofile with template at: ", profile_path)
  }
  
  # Provide information before opening
  cat("\n=== .Rprofile Information ===\n")
  cat("Opening:", profile_path, "\n")
  cat("Type:", profile_type, "profile\n")
  cat("Status:", ifelse(file_exists, "Existing file", "New file"), "\n")
  
  if (!file_exists && !add_template) {
    cat("\nNote: File will be created when you save.\n")
  }
  
  cat("\nCommon .Rprofile uses:\n")
  cat("• Set default CRAN mirror\n")
  cat("• Load frequently used packages\n")
  cat("• Set working directory\n")
  cat("• Define custom functions\n")
  cat("• Set R options (digits, width, etc.)\n")
  cat("• Set environment variables\n")
  
  cat("\nRemember: .Rprofile runs at R startup.\n")
  cat("Changes take effect after restarting R.\n")
  cat("===============================\n\n")
  
  # Open the file for editing
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(profile_path)
  } else {
    file.edit(profile_path)
  }
  
  # Return path invisibly
  invisible(profile_path)
}

#' Show Current R Profile Settings
#' 
#' Display the contents of the active .Rprofile without opening it
#' 
#' @param scope Character. Either "user" or "project"
#' @export
IEP_ShowProfile <- function(scope = c("user", "project")) {
  scope <- match.arg(scope)
  
  profile_path <- if (scope == "user") {
    file.path(Sys.getenv("HOME"), ".Rprofile")
  } else {
    file.path(getwd(), ".Rprofile")
  }
  
  if (file.exists(profile_path)) {
    cat("Contents of", profile_path, ":\n")
    cat("=====================================\n")
    cat(readLines(profile_path), sep = "\n")
    cat("=====================================\n")
  } else {
    message("No .Rprofile found at: ", profile_path)
    message("Run IEP_EditProfile() to create one.")
  }
}