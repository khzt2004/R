library(tidyverse)
library(plotly)

# https://datatitian.com/how-to-turn-your-ggplot2-visualization-into-an-interactive-tweet/

# df <- read_csv("clusterable_embedding_df.csv")

static_plot <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point()

# static_plot <- ggplot(df, aes(x = `0`, y = `1`, color = Desc_00001)) +
#   geom_point()

interactive_plot <- plotly::ggplotly(static_plot)
interactive_plot
