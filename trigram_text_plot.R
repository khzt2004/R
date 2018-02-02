library(tidyverse)
library(tidytext)
library(purrrlyr)
library(stringr)

# https://www.hvitfeldt.me/2018/01/visualizing-trigrams-with-the-tidyverse/

n_word <- 20
n_top <- 150
n_gramming <- 3

trigrams <- tibble(text = janeaustenr::emma) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = n_gramming)

start_words <- c("he", "she")

pattern <- str_c("^", start_words, " ", collapse = "|")
top_words <- trigrams %>%
  filter(str_detect(trigram, pattern)) %>%
  count(trigram, sort = TRUE) %>%
  slice(seq_len(n_top)) %>%
  pull(trigram)

trigrams <- trigrams %>%
  filter(trigram %in% top_words)

str_nth_word <- function(x, n, sep = " ") {
  str_split(x, pattern = " ") %>%
    map_chr(~ .x[n])
}

nodes <- map_df(seq_len(n_gramming),
                ~ trigrams %>%
                  mutate(word = str_nth_word(trigram, .x)) %>%
                  count(word, sort = TRUE) %>%
                  slice(seq_len(n_word)) %>% 
                  mutate(y = seq(from = n_word + 1, to = 0, 
                                 length.out = n() + 2)[seq_len(n()) + 1],
                         x = .x))

nodes %>% 
  ggplot(aes(x, y, label = word)) +
  geom_text()

sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}

# function to create curved plotting lines
egde_lines <- function(trigram, from_word, to_word, scale = 5, n = 50, 
                       x_space = 0) {
  
  from_word <- from_word %>%
    select(-n) %>%
    set_names(c("from", "y_from", "x_from"))
  
  to_word <- to_word %>%
    select(-n) %>%
    set_names(c("to", "y_to", "x_to"))
  
  links <- crossing(from = from_word$from, 
                    to = to_word$to) %>%
    mutate(word_pair = paste(from, to),
           number = map_dbl(word_pair, 
                            ~ sum(str_detect(trigram$trigram, .x)))) %>%
    left_join(from_word, by = "from") %>%
    left_join(to_word, by = "to")
  
  links %>%
    by_row(~ sigmoid(x_from = .x$x_from + 0.2 + x_space,
                     x_to = .x$x_to - 0.05, 
                     y_from = .x$y_from, y_to = .x$y_to, 
                     scale = scale, n = n) %>%
             mutate(word_pair = .x$word_pair,
                    number = .x$number,
                    from = .x$from)) %>%
    pull(.out) %>%
    bind_rows()
}

# first plot
egde_lines(trigram = trigrams, 
           from_word = filter(nodes, x == 1), 
           to_word = filter(nodes, x == 2)) %>%
  filter(number > 0) %>%
  ggplot(aes(x, y, group = word_pair, alpha = number, color = from)) +
  geom_line()


# egdes between first and second column
egde1 <- egde_lines(trigram = trigrams, 
                    from_word = filter(nodes, x == 1), 
                    to_word = filter(nodes, x == 2), 
                    n = 50) %>%
  filter(number > 0) %>%
  mutate(id = word_pair)

# Words in second colunm
## That start with he
second_word_he <- nodes %>%
  filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with she
second_word_she <- nodes %>%
  filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

# Words in third colunm
## That start with he
third_word_he <- nodes %>%
  filter(x == 3) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 3)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with she
third_word_she <- nodes %>%
  filter(x == 3) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 3)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

# egdes between second and third column that starts with he
egde2_he <- egde_lines(filter(trigrams, 
                              str_detect(trigram, paste0("^", start_words[1], " "))), 
                       second_word_he, third_word_he, n = 50) %>%
  mutate(y = y + 0.05,
         from = start_words[1],
         id = str_c(from, word_pair, sep = " ")) %>%
  filter(number > 0)

# egdes between second and third column that starts with she
egde2_she <- egde_lines(filter(trigrams, 
                               str_detect(trigram, paste0("^", start_words[2], " "))), 
                        second_word_she, third_word_she, n = 50) %>%
  mutate(y = y - 0.05,
         from = start_words[2],
         id = str_c(from, word_pair, sep = " ")) %>%
  filter(number > 0)

# All edges
edges <- bind_rows(egde1, egde2_he, egde2_she)


p <- nodes %>% 
  ggplot(aes(x, y, label = word, size = n)) +
  geom_text(hjust = 0, color = "#DDDDDD") +
  theme_void() +
  geom_line(data = edges,
            aes(x, y, group = id, color = from, alpha = sqrt(number)),
            inherit.aes = FALSE) +
  theme(plot.background = element_rect(fill = "#666666", colour = 'black'),
        text = element_text(color = "#EEEEEE", size = 15)) +
  guides(alpha = "none", color = "none", size = "none") +
  xlim(c(0.9, 3.2)) +
  scale_color_manual(values = c("#5EF1F1", "#FA62D0")) +
  labs(title = " Vizualizing trigrams in Jane Austen's, Emma") + 
  scale_size(range = c(3, 8))
p

# extra vizzes
n_word <- 20
n_top <- 150
n_gramming <- 3

trigrams <- tibble(text = janeaustenr::emma) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = n_gramming)

start_words <- c("i", "you")

n_word <- 20
n_top <- 150
n_gramming <- 3

library(rvest)
sherlock_holmes <- read_html("https://sherlock-holm.es/stories/plain-text/cano.txt") %>%
  html_text() %>% 
  str_split("\n") %>%
  unlist()

trigrams <- tibble(text = sherlock_holmes) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = n_gramming)

start_words <- c("holmes", "watson")