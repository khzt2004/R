devtools::install_github("mrjoh3/c3")

library(c3)

data = data.frame(a = abs(rnorm(20) * 10),
                  b = abs(rnorm(20) * 10),
                  date = seq(as.Date("2014-01-01"), by = "month", length.out = 20))

simple.plot <- c3(data)
simple.plot

simple.plot.date <- c3(data, x = 'date')
simple.plot.date

# piped plot
piped.plot <- data %>%
  c3()

piped.plot

# spline plot
spline.plot <- data %>%
  c3() %>%
  c3_line('spline')

spline.plot

# step plot
step.plot <- data %>%
  c3(x = 'date') %>%
  c3_line('area-step')

step.plot

# bar plot
bar.plot <- data[1:10, ] %>%
  c3() %>%
  c3_bar(stacked = TRUE, rotate = FALSE)

bar.plot

# mixed plot
data$c = abs(rnorm(20) *10)
data$d = abs(rnorm(20) *10)

mixed.plot <- data %>%
  c3() %>%
  c3_mixedGeom(type = 'bar',
               stacked = c('b','d'),
               types = list(a='area',
                            c='spline'))

mixed.plot

# scatter plot
scatter.plot <- iris %>%
  c3(x='Sepal_Length', y='Sepal_Width', group = 'Species') %>% 
  c3_scatter()

scatter.plot

# pie chart
pie.chart <- data.frame(sugar=20,fat=45,salt=10) %>% 
  c3() %>% 
  c3_pie()

pie.chart

# donut chart
donut.chart <- data.frame(red=82,green=33,blue=93) %>% 
  c3(colors=list(red='red',green='green',blue='blue')) %>% 
  c3_donut(title = 'Colors')

donut.chart

# gauge chart
gauge.chart <- data.frame(data = 80) %>% 
  c3() %>% 
  c3_gauge()

gauge.chart

# region plot
region.plot <- data %>%
  c3() %>%
  region(data.frame(axis = 'x',
                    start = 5,
                    end = 6))

region.plot

# sub-chart
subchart.plot <- data %>%
  c3(x = 'date') %>%
  subchart()

subchart.plot