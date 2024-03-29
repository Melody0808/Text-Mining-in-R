library(tidyverse)
library(tidytext)

# Emily Dickinson poem
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

# convert the poem into a tibble
text_tibble <- tibble(line = 1:4, text = text)

#' ** Wait, what is a tibble? I thought we were working with
#' data frames?

# break into tokens
text_tibble_unnest <- text_tibble %>%
  unnest_tokens(word, text)

library(janeaustenr)

# let's run this one part at a time
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
#^chapter [\\divxlc] means find chapter and a space and any number(\\d) or letters of ivxlc

# by the way, which books are these?
unique(original_books$book)

# now let's unnest it
tidy_books <- original_books %>%
  unnest_tokens(word, text)

# these are words that don't add much meaning
stop_words

# https://cran.r-project.org/web/packages/stopwords/stopwords.pdf

# get rid of the stop words
tidy_books <- tidy_books %>%
  anti_join(stop_words)

# how many words do we have?
my_count <- tidy_books %>%
  count(word, sort = TRUE, name = "aren")
# sort =TRUE means sequence by n, if omitted, then sequence alphabetically.
# name defines the name of the column. by default is "n".

# let's make a plot!

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>% # later see what happens when you don't do this
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# start some word counts, a common task in text mining

# explore Project Gutenberg (https://www.gutenberg.org/)
library(gutenbergr)

# let's get some H G Wells texts
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# create a tidy format
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# find most common words
tidy_hgwells %>%
  count(word, sort = TRUE)

# now some Charlotte Bront? texts
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

# Comapre top 10 words. Which are common to Bronte and Wells?

# comparing Jane Austen to the other authors
frequency <- bind_rows(mutate(tidy_bronte, author = "Bront? Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word, sort=TRUE) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Bront? Sisters`:`H.G. Wells`)

# let's graph it
library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# correlation test between Bronte and Austen
cor.test(data = frequency[frequency$author == "Bront? Sisters",],
         ~ proportion + `Jane Austen`)

# correlation test between Wells and Austen
cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)
