#' Save Plots in Multiple Formats and Sizes
#'
#' Saves plots (charts, maps, or diagrams) in both PNG and SVG formats across 
#' three standard sizes (small, medium, large) to designated output directories.
#'
#' @param plot_info A list or named vector containing plot metadata:
#'   \itemize{
#'     \item type: "Chart", "Map", or "Diagram"
#'     \item sheet: Base filename for the saved files
#'   }
#' @param plot The plot object to save (ggplot, diagram, etc.)
#' @param sizes List of custom sizes. If NULL, uses global GPI_CHARTS/GPI_MAPS.
#' @param paths List of custom paths. If NULL, uses global paths.
#' @param formats Character vector of formats to save. Default c("png", "svg").
#' @param bg Background color for PNG files. Default "transparent".
#' @param verbose Logical. Print save locations? Default FALSE.
#'
#' @return Invisibly returns a list of saved file paths
#' @export
#' @importFrom ggplot2 ggsave
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Create a sample plot
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' 
#' # Define plot information
#' plot_info <- list(
#'   type = "Chart",
#'   sheet = "fuel_efficiency"
#' )
#' 
#' # Save in multiple sizes and formats
#' IEP_SavePlots(plot_info, p)
#' 
#' # Save with custom settings
#' IEP_SavePlots(plot_info, p, formats = "png", verbose = TRUE)
#' }
IEP_SavePlots <- function(plot_info, 
                         plot,
                         sizes = NULL,
                         paths = NULL,
                         formats = c("png", "svg"),
                         bg = "transparent",
                         verbose = FALSE) {
  
  # Validate plot_info
  if (!all(c("type", "sheet") %in% names(plot_info))) {
    stop("plot_info must contain 'type' and 'sheet' elements")
  }
  
  # Validate type
  valid_types <- c("Chart", "Map", "Diagram")
  if (!plot_info["type"] %in% valid_types) {
    stop("Type must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  # Set default global variables if not provided
  if (is.null(sizes)) {
    if (!exists("GPI_CHARTS") || !exists("GPI_MAPS")) {
      # Provide default sizes if globals don't exist
      GPI_CHARTS <- list(
        small = c(width = 8.45, height = 10),
        medium = c(width = 12, height = 10),
        large = c(width = 17.6, height = 10)
      )
      GPI_MAPS <- list(
        small = c(width = 12, height = 8),
        medium = c(width = 14, height = 10),
        large = c(width = 28, height = 14)
      )
      message("Note: Using default sizes. Define GPI_CHARTS and GPI_MAPS for custom sizes.")
    }
    sizes <- if (plot_info["type"] == "Map") GPI_MAPS else GPI_CHARTS
  }
  
  # Set default paths if not provided
  if (is.null(paths)) {
    if (!exists("IMAGE_FILES") || !exists("CHART_FILES") || !exists("MAP_FILES")) {
      # Use current directory if globals don't exist
      IMAGE_FILES <- getwd()
      CHART_FILES <- getwd()
      MAP_FILES <- getwd()
      message("Note: Using current directory. Define IMAGE_FILES, CHART_FILES, and MAP_FILES for custom paths.")
    }
    paths <- list(
      png_chart = IMAGE_FILES,
      svg_chart = CHART_FILES,
      png_map = IMAGE_FILES,
      svg_map = MAP_FILES
    )
  }
  
  # Set units
  if (!exists("CHART_UNIT")) {
    CHART_UNIT <- "cm"
  }
  
  # Track saved files
  saved_files <- character()
  
  # Process based on type
  if (plot_info["type"] == "Chart") {
    
    for (size_name in names(sizes)) {
      size <- sizes[[size_name]]
      file_base_name <- paste0(plot_info["sheet"], "_", size_name)
      
      # Save PNG if requested
      if ("png" %in% formats) {
        png_file <- file.path(paths$png_chart, paste0(file_base_name, ".png"))
        ggsave(png_file, plot, device = "png", 
               width = size["width"], height = size["height"], 
               units = CHART_UNIT, bg = bg)
        saved_files <- c(saved_files, png_file)
        if (verbose) cat("Saved:", png_file, "\n")
      }
      
      # Save SVG if requested
      if ("svg" %in% formats) {
        svg_file <- file.path(paths$svg_chart, paste0(file_base_name, ".svg"))
        ggsave(svg_file, plot, device = "svg", 
               width = size["width"], height = size["height"], 
               units = CHART_UNIT)
        saved_files <- c(saved_files, svg_file)
        if (verbose) cat("Saved:", svg_file, "\n")
      }
    }
    
  } else if (plot_info["type"] == "Map") {
    
    for (size_name in names(sizes)) {
      size <- sizes[[size_name]]
      file_base_name <- paste0(plot_info["sheet"], "_", size_name)
      
      # Save PNG if requested
      if ("png" %in% formats) {
        png_file <- file.path(paths$png_map, paste0(file_base_name, ".png"))
        ggsave(png_file, plot, device = "png", 
               width = size["width"], height = size["height"], 
               units = CHART_UNIT, bg = bg)
        saved_files <- c(saved_files, png_file)
        if (verbose) cat("Saved:", png_file, "\n")
      }
      
      # Save SVG if requested
      if ("svg" %in% formats) {
        svg_file <- file.path(paths$svg_map, paste0(file_base_name, ".svg"))
        ggsave(svg_file, plot, device = "svg", 
               width = size["width"], height = size["height"], 
               units = CHART_UNIT)
        saved_files <- c(saved_files, svg_file)
        if (verbose) cat("Saved:", svg_file, "\n")
      }
    }
    
  } else if (plot_info["type"] == "Diagram") {
    
    # Check for required packages
    if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
      stop("DiagrammeRsvg package is required for saving diagrams. Install with: install.packages('DiagrammeRsvg')")
    }
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      stop("rsvg package is required for saving diagrams. Install with: install.packages('rsvg')")
    }
    
    for (size_name in names(sizes)) {
      file_base_name <- paste0(plot_info["sheet"], "_", size_name)
      
      # Save SVG first (required for diagram conversion)
      svg_file <- file.path(paths$svg_chart, paste0(file_base_name, ".svg"))
      svg_temp <- DiagrammeRsvg::export_svg(plot)
      readr::write_lines(svg_temp, svg_file)
      saved_files <- c(saved_files, svg_file)
      if (verbose) cat("Saved:", svg_file, "\n")
      
      # Convert to PNG if requested
      if ("png" %in% formats) {
        png_file <- file.path(paths$png_chart, paste0(file_base_name, ".png"))
        rsvg::rsvg_png(svg_file, png_file)
        saved_files <- c(saved_files, png_file)
        if (verbose) cat("Saved:", png_file, "\n")
      }
    }
  }
  
  # Summary message
  if (!verbose && length(saved_files) > 0) {
    message(sprintf("Saved %d files (%d sizes × %d formats)", 
                   length(saved_files), 
                   length(sizes), 
                   length(formats)))
  }
  
  invisible(saved_files)
}