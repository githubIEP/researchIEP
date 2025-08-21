#' Apply IEP Chart Theme to ggplot
#'
#' Applies standardized IEP theme to ggplot objects with customizable elements
#' including titles, axes, gridlines, and source attribution.
#'
#' @param plot A ggplot object to apply the theme to
#' @param chart_info A list containing chart metadata with elements:
#'   \itemize{
#'     \item title: Chart title text
#'     \item xtext: X-axis label
#'     \item ytext: Y-axis label  
#'     \item source: Data source for caption
#'   }
#' @param plottitle Character. "Include" to show title, "Exclude" to hide. Default "Include".
#' @param xaxis Character. "Include" to show x-axis line, "Exclude" to hide. Default "Include".
#' @param yaxis Character. "Include" to show y-axis line, "Exclude" to hide. Default "Include".
#' @param xgridline Character. "Include" to show vertical gridlines, "Exclude" to hide. Default "Exclude".
#' @param ygridline Character. "Include" to show horizontal gridlines, "Exclude" to hide. Default "Include".
#' @param source Character. "Include" to show source caption, "Exclude" to hide. Default "Include".
#'
#' @return A ggplot object with IEP theme applied
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Create sample plot
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
#'   geom_point()
#' 
#' # Define chart information
#' chart_info <- list(
#'   title = "Fuel Efficiency vs Weight",
#'   xtext = "Weight (1000 lbs)",
#'   ytext = "Miles per Gallon",
#'   source = "Motor Trend Magazine, 1974"
#' )
#' 
#' # Apply IEP theme
#' p_themed <- IEP_ChartTheme(p, chart_info)
#' 
#' # Customize theme elements
#' p_custom <- IEP_ChartTheme(p, chart_info, 
#'                            xgridline = "Include",
#'                            yaxis = "Exclude")
#' }
IEP_ChartTheme <- function(plot, 
                          chart_info,
                          plottitle = "Include",
                          xaxis = "Include", 
                          yaxis = "Include",
                          xgridline = "Exclude",
                          ygridline = "Include",
                          source = "Include") {
  
  # Check for required global variables (fonts)
  if (!exists("HEAVY_FONT")) {
    HEAVY_FONT <- "Arial Bold"
    message("Note: HEAVY_FONT not found, using 'Arial Bold'")
  }
  if (!exists("LIGHT_FONT")) {
    LIGHT_FONT <- "Arial"
    message("Note: LIGHT_FONT not found, using 'Arial'")
  }
  
  # Validate chart_info
  required_fields <- c("title", "xtext", "ytext", "source")
  missing_fields <- setdiff(required_fields, names(chart_info))
  if (length(missing_fields) > 0) {
    stop("chart_info missing required fields: ", paste(missing_fields, collapse = ", "))
  }
  
  # Create caption with source
  finalcaption <- if (source == "Include" && !is.null(chart_info[["source"]])) {
    paste0("Source: ", chart_info[["source"]])
  } else {
    NULL
  }
  
  # Set up labels
  plot_labels <- labs(
    title = if (plottitle == "Include") chart_info[["title"]] else NULL,
    x = chart_info[["xtext"]],
    y = chart_info[["ytext"]],
    caption = finalcaption
  )
  
  # Base theme
  plot_theme <- theme_minimal() +
    theme(
      # Text settings
      text = element_text(family = HEAVY_FONT),
      plot.title = if (plottitle == "Include") {
        element_text(size = 13, family = HEAVY_FONT)
      } else {
        element_blank()
      },
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 9),
      
      # Caption settings
      plot.caption = if (source == "Include") {
        element_text(hjust = 0, colour = "#888686", size = 7)
      } else {
        element_blank()
      },
      plot.caption.position = "plot",
      
      # Axis settings
      axis.text = element_text(colour = "#444444", size = 6.5, family = LIGHT_FONT),
      axis.title = element_text(face = "bold", size = 7, family = HEAVY_FONT),
      axis.line.x = if (xaxis == "Include") {
        element_line(colour = "#444444")
      } else {
        element_blank()
      },
      axis.line.y = if (yaxis == "Include") {
        element_line(colour = "#444444")
      } else {
        element_blank()
      },
      
      # Grid settings
      panel.grid.minor = element_blank(),
      panel.grid.major.x = if (xgridline == "Include") {
        element_line(colour = "lightgrey")
      } else {
        element_blank()
      },
      panel.grid.major.y = if (ygridline == "Include") {
        element_line(colour = "lightgrey")
      } else {
        element_blank()
      },
      
      # Legend settings
      legend.title = element_blank(),
      legend.text = element_text(size = 7, family = LIGHT_FONT)
    )
  
  # Apply theme and labels to plot
  plot <- plot + plot_labels + plot_theme
  
  # Adjust y-axis expansion if x-axis line is shown
  if (xaxis == "Include") {
    plot <- plot + scale_y_continuous(expand = c(0, 0))
  }
  
  return(plot)
}