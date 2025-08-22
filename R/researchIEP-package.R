#' researchIEP: Research Tools for the Institute for Economics and Peace
#'
#' The researchIEP package provides standardized tools and functions
#' for IEP research projects, including project setup, data processing,
#' and visualization utilities.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{IEP_ProjectFolders}}: Create standardized project structure
#'   \item \code{\link{IEP_LibraryLoader}}: Load multiple packages efficiently  
#'   \item \code{\link{IEP_ChartTheme}}: Apply IEP chart styling
#'   \item \code{\link{IEP_EditProfile}}: Edit or create R profile configuration
#'   \item \code{\link{IEP_ShowProfile}}: Display current R profile settings
#'   \item \code{\link{IEP_ScatterLabels}}: Add directional labels to scatter plots
#'   \item \code{\link{IEP_SavePlots}}: Save plots in multiple formats and sizes
#'   \item \code{\link{IEP_Chartbook}}: Create Excel chartbook
#'   \item \code{\link{IEP_Sheet}}: Create formatted sheet in chartbook
#'   \item \code{\link{IEP_SheetData}}: Add data to chartbook sheet
#'   \item \code{\link{IEP_SheetImage}}: Add chart image to worksheet
#'   \item \code{\link{IEP_ProjectExport}}: Export project charts and tables
#' }
#'
#' @section Project Setup Functions:
#' \itemize{
#'   \item \code{\link{IEP_ProjectFolders}}: Create standardized project folder structure
#'   \item \code{\link{IEP_EditProfile}}: Edit or create R profile configuration
#'   \item \code{\link{IEP_ShowProfile}}: Display current R profile settings
#'   \item \code{\link{IEP_LibraryLoader}}: Load and install multiple packages efficiently
#' }
#'
#' @section Visualization Functions:
#' \itemize{
#'   \item \code{\link{IEP_ChartTheme}}: Apply standardized IEP chart styling
#'   \item \code{\link{IEP_ScatterLabels}}: Add directional labels to scatter plots
#'   \item \code{\link{IEP_SavePlots}}: Save plots in multiple formats and sizes
#' }
#'
#' @section Export Functions:
#' \itemize{
#'   \item \code{\link{IEP_Chartbook}}: Create Excel chartbook
#'   \item \code{\link{IEP_Sheet}}: Create formatted sheet in chartbook  
#'   \item \code{\link{IEP_SheetData}}: Add data to chartbook sheet
#'   \item \code{\link{IEP_SheetImage}}: Add chart image to worksheet
#'   \item \code{\link{IEP_ProjectExport}}: Export project charts and tables
#' }
#'
#' @section Getting Started:
#' To create a new project with IEP structure:
#' \preformatted{
#' library(researchIEP)
#' IEP_ProjectFolders("path/to/project", type = "simple")
#' }
#'
#' To set up your R environment:
#' \preformatted{
#' IEP_EditProfile()  # Edit your .Rprofile
#' IEP_LibraryLoader(tidyverse, ggplot2, scales)  # Load packages
#' }
#'
#' @docType package
#' @name researchIEP
#' @aliases researchIEP-package
#'
#' @author Institute for Economics and Peace
#' @references \url{https://www.economicsandpeace.org}
#' 
#' @keywords internal
"_PACKAGE"