suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n = 9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size = 9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = color.background, color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(color = color.background)) +
    
    # Format the grid
    theme(panel.grid.major = element_line(color = color.grid.major, size = 0.25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position = "top") +
    theme(legend.background = element_rect(fill = color.background)) +
    theme(legend.key = element_rect(fill = color.background)) +
    theme(legend.text = element_text(size = 10, color = color.axis.title)) +
    theme(legend.title = element_text(colour = color.title, size = 12, face = "bold")) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color=color.title, size = 15, vjust = 1.25, face = "bold")) +
    theme(axis.text.x = element_text(size = 10, color = color.axis.text)) +
    theme(axis.text.y = element_text(size = 10, color = color.axis.text)) +
    theme(axis.title.x = element_text(size = 12, color = color.axis.title, vjust = 0)) +
    theme(axis.title.y = element_text(size = 12, color = color.axis.title, vjust = 1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}