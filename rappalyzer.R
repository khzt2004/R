library(hrbrthemes)
library(tidyverse)
library(curl)
library(httr)
library(rvest)
library(stringi)
library(urltools)
library(rappalyzer) # devtools::install_github("hrbrmstr/rappalyzer")
library(rprojroot)

# first, create a folder named "data" in the working directory and 
# place f1k.csv into the folder before running code

rt <- "C:/Users/User/Documents"

if (!file.exists(file.path(rt, "data", "f1k_gets.rds"))) {
  
  f1k <- read_csv(file.path(rt, "data", "f1k.csv"))
  
  targets <- pull(f1k, website)
  
  results <- list()
  errors <- list()
  
  OK <- function(res) {
    cat(".", sep="")
    results <<- c(results, list(res))
  }
  
  BAD <- function(err_msg) {
    cat("X", sep="")
    errors <<- c(errors, list(err_msg))
  }
  
  pool <- multi_set(total_con = 20)
  
  walk(targets, ~{
    multi_add(
      new_handle(url = .x, useragent = splashr::ua_macos_chrome, followlocation = TRUE, timeout = 60),
      OK, BAD
    )
  })
  
  multi_run(pool = pool)
  
  write_rds(results, file.path(rt, "data", "f1k_gets.rds"), compress = "xz")
  
} else {
  results <- read_rds(file.path(rt, "data", "f1k_gets.rds"))
}

if (!file.exists(file.path(rt, "data", "rapp_results.rds"))) {
  
  results <- keep(results, ~.x$status_code < 300)
  map(results, ~{
    list(
      url = .x$url,
      status_code = .x$status_code,
      content = .x$content,
      headers = httr:::parse_headers(.x$headers)
    ) -> res
    class(res) <- "response"
    res
  }) -> results
  
  # this takes a *while*
  pb <- progress_estimated(length(results))
  map_df(results, ~{
    pb$tick()$print()
    rap_df <- rappalyze(.x)
    if (nrow(rap_df) > 0) rap_df <- mutate(rap_df, url = .x$url)
  }) -> rapp_results
  
  write_rds(rapp_results, file.path(rt, "data", "rapp_results.rds"))
  
} else {
  rapp_results <- read_rds(file.path(rt, "data", "rapp_results.rds"))
}

# Let's join it back up with the original metadata

left_join(
  mutate(rapp_results, host = domain(rapp_results$url)) %>%
    bind_cols(suffix_extract(.$host))
  ,
  mutate(f1k, host = domain(website)) %>%
    bind_cols(suffix_extract(.$host)),
  by = c("domain", "suffix")
) %>%
  filter(!is.na(name)) -> rapp_results

length(unique(rapp_results$name))

# see how many categories we picked up

xdf <- distinct(rapp_results, name, sector, category, tech)

sort(unique(xdf$category))

# find most common tech component across the sites
count(xdf, tech, sort=TRUE)

# Let's see how broadly each tech stack category is used across the sectors

group_by(xdf, sector) %>%
  count(category) %>%
  ungroup() %>%
  arrange(category) %>%
  mutate(category = factor(category, levels=rev(unique(category)))) %>%
  ggplot(aes(category, n)) +
  geom_boxplot() +
  scale_y_comma() +
  coord_flip() +
  labs(x=NULL, y="Tech/Services Detected across Sectors",
       title="Usage of Tech Stack Categories by Sector") +
  theme_ipsum_rc(grid="X")

# interactive visualization that lets you explore the tech stacks by sector
devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)
library(data.tree)
library(treemap)

cpdf <- count(xdf, sector, tech)

cpdf$pathString <- paste("rapp", cpdf$sector, cpdf$tech, sep = "/")
stacks <- as.Node(cpdf)

circlepackeR(stacks, size = "n", color_min = "hsl(56,80%,80%)",
             color_max = "hsl(341,30%,40%)", width = "100%", height="800px")