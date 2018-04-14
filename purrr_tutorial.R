library(purrr)
library(repurrrsive)
library(listviewer)
library(tidyverse)

# https://jennybc.github.io/purrr-tutorial/ls01_map-name-position-shortcuts.html

# provide “TEXT” to extract the element named “TEXT”
map(got_chars, "name")

got <- got_chars %>% {
  tibble(
    name = map_chr(., "name"),
    culture = map_chr(., "culture"),
    gender = map_chr(., "gender"),       
    id = map_int(., "id"),
    born = map_chr(., "born"),
    alive = map_lgl(., "alive")
  )
}

# map example
mtcars_map <- mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# open multiple files
files <- c("Book1.csv", "Book2.csv", "Book3.csv")
filedata <- rbind(map_df(files, read_csv))

# to open from file directory
# files <- list.files("../open-data/", pattern = "^2017", full.names = TRUE)
full <- rbind(map_df(files, read_csv))

X <- map(1:10000, ~ data.frame(x = .x))
X <- bind_rows(X)




         