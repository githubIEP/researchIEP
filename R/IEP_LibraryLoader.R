#' Load and Install R Packages (CRAN and GitHub)
#'
#' Checks if packages are installed and loaded. Installs missing packages
#' from CRAN or GitHub and loads all requested packages. Reports what was installed.
#'
#' @param ... Package names (unquoted or quoted). For GitHub packages, use "owner/repo" format
#' @param install_missing Logical. Should missing packages be installed? Default TRUE
#' @param quiet Logical. Suppress package startup messages? Default FALSE
#' @param repos Repository to use for CRAN installation
#' @param github_packages Character vector of package names that should be installed from GitHub
#'
#' @return Invisibly returns a list with installation and loading details
#' @export
#'
#' @examples
#' \dontrun{
#' # Mix of CRAN and GitHub packages
#' IEP_LibraryLoader(tidyverse, sf, scales, 
#'                   github_packages = c("githubIEP/researchIEP"))
#' 
#' # Specify GitHub packages inline
#' IEP_LibraryLoader(ggplot2, "coolbutuseless/ggpattern", dplyr)
#' }
IEP_LibraryLoader <- function(..., install_missing = TRUE, quiet = FALSE, 
                              repos = "https://cran.rstudio.com/",
                              github_packages = NULL) {
  
  # Capture arguments
  args <- substitute(list(...))[-1]
  package_specs <- sapply(args, function(arg) {
    if (is.character(arg)) {
      return(arg)
    } else {
      return(as.character(arg))
    }
  })
  
  # Combine with explicit github_packages if provided
  if (!is.null(github_packages)) {
    package_specs <- c(package_specs, github_packages)
  }
  
  # Track what happens
  already_loaded <- character()
  newly_loaded <- character()
  newly_installed <- character()
  failed_install <- character()
  installed_from_github <- character()
  
  # Check which packages are already loaded
  currently_loaded <- .packages()
  
  # Process each package
  for (spec in package_specs) {
    
    # Determine if it's a GitHub package (contains "/")
    is_github <- grepl("/", spec)
    
    # Extract package name (for GitHub: "owner/repo" -> "repo")
    if (is_github) {
      package_name <- basename(spec)
      github_spec <- spec
    } else {
      package_name <- spec
    }
    
    # Check if already loaded
    if (package_name %in% currently_loaded) {
      already_loaded <- c(already_loaded, package_name)
      next
    }
    
    # Check if installed
    if (!package_name %in% rownames(installed.packages())) {
      if (install_missing) {
        if (is_github) {
          # Install from GitHub
          message(paste("Installing", package_name, "from GitHub:", github_spec, "..."))
          
          # Check if devtools or remotes is available
          if (!requireNamespace("remotes", quietly = TRUE) && 
              !requireNamespace("devtools", quietly = TRUE)) {
            message("Installing 'remotes' package to enable GitHub installations...")
            install.packages("remotes", repos = repos, quiet = TRUE)
          }
          
          tryCatch({
            if (requireNamespace("remotes", quietly = TRUE)) {
              remotes::install_github(github_spec, quiet = quiet)
            } else {
              devtools::install_github(github_spec, quiet = quiet)
            }
            newly_installed <- c(newly_installed, package_name)
            installed_from_github <- c(installed_from_github, package_name)
          }, error = function(e) {
            failed_install <- c(failed_install, spec)
            warning(paste("Failed to install", spec, "from GitHub:", e$message))
            next
          })
        } else {
          # Install from CRAN
          message(paste("Installing", package_name, "from CRAN..."))
          tryCatch({
            install.packages(package_name, repos = repos, quiet = quiet)
            newly_installed <- c(newly_installed, package_name)
          }, error = function(e) {
            # If CRAN fails, provide helpful message
            failed_install <- c(failed_install, package_name)
            warning(paste("Failed to install", package_name, "from CRAN:", e$message,
                         "\nIf this is a GitHub package, specify it as 'owner/repo' or",
                         "use github_packages parameter"))
            next
          })
        }
      } else {
        failed_install <- c(failed_install, spec)
        warning(paste("Package not installed and install_missing = FALSE:", spec))
        next
      }
    }
    
    # Load the package
    if (!spec %in% failed_install) {
      tryCatch({
        if (quiet) {
          suppressPackageStartupMessages(library(package_name, character.only = TRUE))
        } else {
          library(package_name, character.only = TRUE)
        }
        newly_loaded <- c(newly_loaded, package_name)
      }, error = function(e) {
        warning(paste("Failed to load", package_name, ":", e$message))
      })
    }
  }
  
  # Create summary message
  cat("\n=== Package Loading Summary ===\n")
  
  if (length(newly_installed) > 0) {
    if (length(installed_from_github) > 0) {
      cran_installs <- setdiff(newly_installed, installed_from_github)
      if (length(cran_installs) > 0) {
        cat("✓ Newly INSTALLED from CRAN:", paste(cran_installs, collapse = ", "), "\n")
      }
      cat("✓ Newly INSTALLED from GitHub:", paste(installed_from_github, collapse = ", "), "\n")
    } else {
      cat("✓ Newly INSTALLED packages:", paste(newly_installed, collapse = ", "), "\n")
    }
  }
  
  if (length(newly_loaded) > 0) {
    cat("✓ Newly loaded packages:", paste(newly_loaded, collapse = ", "), "\n")
  }
  
  if (length(already_loaded) > 0) {
    cat("• Already loaded:", paste(already_loaded, collapse = ", "), "\n")
  }
  
  if (length(failed_install) > 0) {
    cat("✗ Failed to install/load:", paste(failed_install, collapse = ", "), "\n")
  }
  
  cat("===============================\n\n")
  
  # Return summary invisibly
  invisible(list(
    requested = package_specs,
    newly_installed = newly_installed,
    newly_loaded = newly_loaded,
    already_loaded = already_loaded,
    failed = failed_install,
    github_installs = installed_from_github
  ))
}