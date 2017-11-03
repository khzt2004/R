library(ggiraph)
library(tidyverse)

g <- ggplot(mpg, aes( x = displ, y = cty, color = hwy) )
my_gg <- g + geom_point_interactive(
  aes(tooltip = model, data_id = model), size = 2) 
ggiraph(code = print(my_gg), hover_css = "cursor:pointer;fill:red;stroke:red;")


dataset <- mtcars
dataset$carname <- row.names(dataset)
gg_point_1 <- ggplot(dataset, aes(x = disp, y = qsec, tooltip = carname, data_id = carname, color= wt) ) + 
  geom_point_interactive(size=3)

# htmlwidget call
ggiraph(code = {print(gg_point_1)}, tooltip_offx = 20, tooltip_offy = -10 )
tooltip_css <- "background-color:gray;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
ggiraph(code = {print(gg_point_1)}, tooltip_extra_css = tooltip_css, tooltip_opacity = .75, zoom_max = 5)
