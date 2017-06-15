require(datasets)
library(googleVis)
# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html

states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

# histogram

set.seed(123)
datHist=data.frame(A=rpois(100, 20),
                   B=rpois(100, 5),
                   C=rpois(100, 50))

Hist <- gvisHistogram(datHist, options=list(
  legend="{ position: 'top', maxLines: 2 }",
  colors="['#5C3292', '#1A8763', '#871B47']",
  width=1200, height=400))
plot(Hist)

# motion chart

Motion=gvisMotionChart(Fruits, 
                       idvar="Fruit", 
                       timevar="Year")
plot(Motion)

# stacked bar chart
df <- data.frame(country=c("US", "GB", "BR"), 
                 val1=c(1,3,4), 
                 val2=c(23,12,32))

## Bar chart
Bar1 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"))
plot(Bar1)

## Stacked bar chart
Bar2 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"),
                     options=list(isStacked=TRUE))
plot(Bar2)

## Stacked column chart
Col2 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"),
                        options=list(isStacked=TRUE))
plot(Col2)

