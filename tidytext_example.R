library(tidytext)
library(janeaustenr)
library(stringr)
library(tidyverse)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)
