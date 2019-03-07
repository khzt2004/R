library(tidyverse)
library(plotly)

# https://datatitian.com/how-to-turn-your-ggplot2-visualization-into-an-interactive-tweet/

static_plot <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point()
interactive_plot <- plotly::ggplotly(static_plot)
interactive_plot
