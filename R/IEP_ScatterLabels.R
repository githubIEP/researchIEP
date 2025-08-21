#' Add Directional Labels to Scatter Plots
#'
#' Adds arrow indicators and text labels to show directional meaning on scatter plot axes.
#' Useful for creating quadrant charts or showing the meaning of axis directions.
#'
#' @param plot A ggplot object (typically a scatter plot)
#' @param xaxis Character. "Include" to show horizontal arrows and labels, "Exclude" to hide. Default "Include".
#' @param yaxis Character. "Include" to show vertical arrows and labels, "Exclude" to hide. Default "Include".
#' @param left_text Character. Label text for left direction. Default "".
#' @param right_text Character. Label text for right direction. Default "".
#' @param up_text Character. Label text for up direction. Default "".
#' @param down_text Character. Label text for down direction. Default "".
#' @param xposition Numeric. Vertical position of x-axis labels (0-1). Default 0.07.
#' @param yposition Numeric. Horizontal position of y-axis labels (0-1). Default 0.045.
#'
#' @return A plot object with directional labels and arrows added
#' @export
#' @importFrom cowplot ggdraw draw_grob draw_label
#' @importFrom grid linesGrob arrow unit
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Create a scatter plot
#' p <- ggplot(mtcars, aes(wt, mpg)) + 
#'   geom_point() +
#'   theme_minimal()
#' 
#' # Add directional labels
#' p_labeled <- IEP_ScatterLabels(
#'   p,
#'   left_text = "Lighter", 
#'   right_text = "Heavier",
#'   up_text = "More Efficient", 
#'   down_text = "Less Efficient"
#' )
#' 
#' # Add only horizontal labels
#' p_horizontal <- IEP_ScatterLabels(
#'   p,
#'   left_text = "Low", 
#'   right_text = "High",
#'   yaxis = "Exclude"
#' )
#' }
IEP_ScatterLabels <- function(plot, 
                              xaxis = "Include", 
                              yaxis = "Include",
                              left_text = "", 
                              right_text = "", 
                              up_text = "", 
                              down_text = "",
                              xposition = 0.07, 
                              yposition = 0.045) {
  
  require(cowplot)
  
  # Check for required font variable
  if (!exists("HEAVY_FONT")) {
    HEAVY_FONT <- "Arial Bold"
    message("Note: HEAVY_FONT not found, using 'Arial Bold'")
  }
  
  # Arrow positioning constants
  ARROW_LENGTH <- 0.05
  LEFT_ARROW_START <- 0.12
  LEFT_ARROW_END <- LEFT_ARROW_START + ARROW_LENGTH
  RIGHT_ARROW_END <- 0.97
  RIGHT_ARROW_START <- RIGHT_ARROW_END - ARROW_LENGTH
  UP_ARROW_END <- 0.96
  UP_ARROW_START <- UP_ARROW_END - ARROW_LENGTH
  DOWN_ARROW_START <- 0.12
  DOWN_ARROW_END <- DOWN_ARROW_START + ARROW_LENGTH
  
  # Text positioning constants
  LEFT_TEXT <- LEFT_ARROW_END + 0.01
  RIGHT_TEXT <- RIGHT_ARROW_START - 0.01
  UP_TEXT <- UP_ARROW_START - 0.01
  DOWN_TEXT <- DOWN_ARROW_END + 0.01
  
  # Create drawing canvas from the plot
  p <- cowplot::ggdraw(plot)
  
  # Add horizontal axis labels and arrows if requested
  if (xaxis == "Include") {
    # Create arrow graphical objects
    right_arrow <- grid::linesGrob(
      x = grid::unit(c(RIGHT_ARROW_START, RIGHT_ARROW_END), "npc"), 
      y = grid::unit(c(xposition, xposition), "npc"),
      arrow = grid::arrow(ends = "last", type = "closed", length = grid::unit(2, "mm"))
    )
    
    left_arrow <- grid::linesGrob(
      x = grid::unit(c(LEFT_ARROW_START, LEFT_ARROW_END), "npc"), 
      y = grid::unit(c(xposition, xposition), "npc"),
      arrow = grid::arrow(ends = "first", type = "closed", length = grid::unit(2, "mm"))
    )
    
    # Add arrows and labels to plot
    p <- p +
      cowplot::draw_grob(right_arrow) +
      cowplot::draw_grob(left_arrow) +
      cowplot::draw_label(
        left_text, 
        x = LEFT_TEXT, 
        y = xposition, 
        hjust = 0, 
        vjust = 0.25, 
        fontface = "bold", 
        size = 6, 
        fontfamily = HEAVY_FONT
      ) +
      cowplot::draw_label(
        right_text, 
        x = RIGHT_TEXT, 
        y = xposition, 
        hjust = 1, 
        vjust = 0.25, 
        fontface = "bold", 
        size = 6, 
        fontfamily = HEAVY_FONT
      )
  }
  
  # Add vertical axis labels and arrows if requested
  if (yaxis == "Include") {
    # Create arrow graphical objects
    up_arrow <- grid::linesGrob(
      x = grid::unit(c(yposition, yposition), "npc"), 
      y = grid::unit(c(UP_ARROW_START, UP_ARROW_END), "npc"),
      arrow = grid::arrow(ends = "last", type = "closed", length = grid::unit(2, "mm"))
    )
    
    down_arrow <- grid::linesGrob(
      x = grid::unit(c(yposition, yposition), "npc"), 
      y = grid::unit(c(DOWN_ARROW_START, DOWN_ARROW_END), "npc"),
      arrow = grid::arrow(ends = "first", type = "closed", length = grid::unit(2, "mm"))
    )
    
    # Add arrows and labels to plot
    p <- p +
      cowplot::draw_grob(up_arrow) +
      cowplot::draw_grob(down_arrow) +
      cowplot::draw_label(
        down_text, 
        x = yposition, 
        y = DOWN_TEXT, 
        hjust = 0, 
        vjust = 0.25, 
        angle = 90, 
        fontface = "bold", 
        size = 6, 
        fontfamily = HEAVY_FONT
      ) +
      cowplot::draw_label(
        up_text, 
        x = yposition, 
        y = UP_TEXT, 
        hjust = 1, 
        vjust = 0.25, 
        angle = 90, 
        fontface = "bold", 
        size = 6, 
        fontfamily = HEAVY_FONT
      )
  }
  
  return(p)
}
