#' Create IEP Project Folder Structure
#'
#' Creates a standardized folder structure for IEP research projects with
#' template R scripts containing boilerplate code.
#'
#' @param base_path Character string. Path where the project structure should be created.
#'   Defaults to current working directory.
#' @param type Character string. Either "simple" or "complex" to determine folder structure.
#'   Defaults to "simple".
#' @param project_name Character string. Optional custom project name. If NULL, 
#'   uses the base_path folder name.
#'
#' @return Invisibly returns a list containing:
#'   \item{path}{The base path where structure was created}
#'   \item{type}{The type of structure created}
#'   \item{project_name}{The project name used}
#'   \item{folders_created}{Vector of created folder paths}
#'   \item{files_created}{Vector of created file paths}
#'   \item{structure}{Names of top-level folders}
#'
#' @export
#' @examples
#' \dontrun{
#' # Create simple structure in current directory
#' IEP_ProjectFolders()
#' 
#' # Create complex structure with custom name
#' IEP_ProjectFolders("~/Projects/Analysis", type = "complex", 
#'                    project_name = "Q1 Revenue Analysis")
#' }
IEP_ProjectFolders <- function(base_path = getwd(), type = "simple", project_name = NULL) {
  
  # Parameter validation
  if (!type %in% c("simple", "complex")) {
    stop("Type must be either 'simple' or 'complex'")
  }
  
  if (!dir.exists(base_path)) {
    stop("Base path does not exist: ", base_path)
  }
  
  # Define the directory structures - both with numbered folders
  complex_folders <- list(
    "01_documentation" = NULL,
    "02_data" = c("raw", "processed"),
    "03_scripts" = c("01_cleaning", "02_standardOutputs", "03_analysis", "04_outputs"),
    "04_outputs" = c("charts", "maps", "tables")
  )
  
  simple_folders <- list(
    "01_documentation" = NULL,
    "02_data" = c("raw", "processed"),
    "03_scripts" = c("01_cleaning", "02_analysis"),
    "04_outputs" = NULL
  )
  
  # Determine project name for dynamic content
  proj_name <- ifelse(is.null(project_name), basename(base_path), project_name)
  
  # Define default content for R files with dynamic naming
  file_templates <- list(
    "projectFunctions.R" = c(
      "# ----- Project Functions File: Custom functions for this project -----",
      paste0("# Project: ", proj_name),
      paste0("# Created: ", Sys.Date()),
      "",
      "library(researchIEP)",
      "PACKAGE_PRIORITY = c(filter = \"dplyr\", select = \"dplyr\", lag = \"dplyr\", first = \"dplyr\")",
      "",
      "# - f_PackagePriorities: Gives priority to tidyverse functions. Will remove issues with filter(), select() etc.",
      "f_ConflictPriorities <- function() {",
      "  # Check if 'conflicted' package is installed, install if not",
      "  if (!requireNamespace(\"conflicted\", quietly = TRUE)) {",
      "    install.packages(\"conflicted\")",
      "  }",
      "  library(conflicted)",
      "  ",
      "  # Set conflict preferences using the global PACKAGE_PRIORITY",
      "  for (conflict_func in names(PACKAGE_PRIORITY)) {",
      "    conflict_prefer(conflict_func, PACKAGE_PRIORITY[[conflict_func]])",
      "  }",
      "}",
      ""
    ),
    
    "projectVariables.R" = c(
      "# ----- Project Variables File: Universal variables for this project -----",
      paste0("# Project: ", proj_name),
      paste0("# Created: ", Sys.Date()),
      "",
      "### --- Libraries ------------------------- ###",
      "",
      "",
      "### --- Filepaths ------------------------- ###",
      "",
      "# PROJECT_PATH = \"/path to the folder on the shared drive\"",
      "ONEDRIVE = paste0(IEP_USERPATH, PROJECT_PATH)",
      "",
      "# Output Locations",
      "COLLATERAL_FILES = paste0(ONEDRIVE,\"/Collateral\")",
      "DASHBOARD_FILES = paste0(ONEDRIVE,\"/Dashboard\")",
      "CHART_FILES = paste0(ONEDRIVE,\"/Layout/Charts\")",
      "IMAGE_FILES = paste0(ONEDRIVE,\"/Layout/Images\")",
      "TABLE_FILES = paste0(ONEDRIVE,\"/Layout/Tables\")",
      "MAP_FILES = paste0(ONEDRIVE,\"/Layout/Maps\")",
      "ANALYSIS_FILES = paste0(ONEDRIVE,\"/Analysis\")",
      "",
      "### --- Variables ------------------------- ###",
      "",
      "# Chart and Map Colours",
      "IEP_COLOURS <- c(\"#770A1E\", \"#F37053\", \"#ED1D24\", \"#FDC16E\",\"#678696\", \"#F9B298\", \"#B7C9D3\", \"#2D4F5E\", \"#D1D3D4\", \"#A6A593\")",
      "",
      "IEP_COLOURS_SCALE <- c(\"#D1D3D4\",\"#B2DED1\", \"#f6e38d\", \"#FEC779\", \"#F37053\", \"#ed1c24\", \"#770A1E\")",
      "",
      "# Chart Line widths",
      "LINE_SMALL = 0.5",
      "LINE_LARGE = 0.72",
      "",
      "# Chart Sizes",
      "GPI_CHARTS <- list(",
      "  small = c(width = 8.45, height = 10),",
      "  medium = c(width = 12, height = 10),",
      "  large = c(width = 17.6, height = 10)",
      ")",
      "",
      "GPI_MAPS <- list(",
      "  small = c(width = 12, height = 8),",
      "  medium = c(width = 14, height = 10),",
      "  large = c(width = 28, height = 14)",
      ")",
      "",
      "CHART_UNIT = \"cm\"",
      "",
      "# Chart Fonts",
      "HEAVY_FONT = \"Arial Bold\"",
      "LIGHT_FONT = \"Arial\"",
      ""
    ),
    
    "projectControl.R" = c(
      "# ----- Project Control File: Main control script for this project -----",
      paste0("# Project: ", proj_name),
      paste0("# Created: ", Sys.Date()),
      "",
      "# This script allows for the entire project to be run from a single script",
      "# Use the source() function to run specific scripts, eg \"source(03_scripts/projectFunctions.R\")",
      "",
      ""
    ),
    
    "projectOutputs.R" = c(
      "# ----- Project Outputs File: Generate outputs for this project -----",
      paste0("# Project: ", proj_name),
      paste0("# Created: ", Sys.Date()),
      "",
      "# NOTE: for the export to work, the charts have to be created first",
      "",
      "# Libraries to export charts, add any that are required",
      "IEP_LibraryLoader(tidyverse, sf, scales, patchwork, extrafont, openxlsx)",
      "",
      "### --- Section 1 -------------------------------------- ###",
      "# Add extra sections as necessary",
      "",
      "# Create Workbook",
      "wbCHARTS_SECTION1 <- createWorkbook()",
      "",
      "# List",
      "SECTION1_EXPORT <- c(\"\")",
      "",
      "# Reset Counters",
      "figure_count = 0",
      "table_count = 0",
      "",
      "# Export Data",
      "IEP_ProjectExport(\"1\", wbCHARTS_SECTION1, CHARTBOOK_1, SECTION1_EXPORT)"
    )
  )
  
  # Select which folder structure to use
  folders <- if(type == "complex") complex_folders else simple_folders
  
  # Track what was created
  created_folders <- character()
  created_files <- character()
  
  # Create the directory structure
  for (main_folder in names(folders)) {
    main_folder_path <- file.path(base_path, main_folder)
    
    # Create main folder with recursive option
    if (!dir.exists(main_folder_path)) {
      dir.create(main_folder_path, recursive = TRUE, showWarnings = FALSE)
      created_folders <- c(created_folders, main_folder_path)
    }
    
    # Create .gitkeep in main folder
    gitkeep_path <- file.path(main_folder_path, ".gitkeep")
    if (!file.exists(gitkeep_path)) {
      file.create(gitkeep_path)
      created_files <- c(created_files, gitkeep_path)
    }
    
    # Add script files to the scripts folder (03_scripts in both versions)
    if (main_folder == "03_scripts") {
      for (script_name in names(file_templates)) {
        script_path <- file.path(main_folder_path, script_name)
        
        if (!file.exists(script_path)) {
          # Write the template content to the file
          writeLines(file_templates[[script_name]], script_path)
          created_files <- c(created_files, script_path)
        }
      }
    }
    
    # Create subfolders if they exist
    if (!is.null(folders[[main_folder]])) {
      for (sub_folder in folders[[main_folder]]) {
        sub_folder_path <- file.path(main_folder_path, sub_folder)
        
        # Create subfolder with recursive option
        if (!dir.exists(sub_folder_path)) {
          dir.create(sub_folder_path, recursive = TRUE, showWarnings = FALSE)
          created_folders <- c(created_folders, sub_folder_path)
        }
        
        # Create .gitkeep in subfolder
        gitkeep_sub_path <- file.path(sub_folder_path, ".gitkeep")
        if (!file.exists(gitkeep_sub_path)) {
          file.create(gitkeep_sub_path)
          created_files <- c(created_files, gitkeep_sub_path)
        }
      }
    }
  }
  
  # Success message
  cat("Folder structure created or updated successfully!\n")
  cat("Type:", type, "\n")
  cat("Location:", base_path, "\n")
  cat("Project Name:", proj_name, "\n")
  cat("Folders created:", length(created_folders), "\n")
  cat("Files created:", length(created_files), "\n")
  
  # Return useful information (invisibly)
  invisible(list(
    path = base_path,
    type = type,
    project_name = proj_name,
    folders_created = created_folders,
    files_created = created_files,
    structure = names(folders)
  ))
}