library(tidytext)

# can pull various sentiments lexicons
# will need to download them the first time
View(get_sentiments("afinn")) # http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
  # repository: https://github.com/fnielsen/afinn
  # includes emoticons? https://github.com/fnielsen/afinn/blob/master/afinn/data/AFINN-emoticon-8.txt
View(get_sentiments("bing")) # https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
View(get_sentiments("nrc")) # http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
View(get_sentiments("loughran")) # https://sraf.nd.edu/textual-analysis/resources/
                           # scroll down on that page to the author, currency, and names
                           # stop words!

# look at the superfluous words at the bottom
View(get_sentiments("loughran"))

# load the Jane Austen books again
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
#' Note: Sentiment lexicons and stop word datasets also use "word"
#' as name of their columns containing words. "word" was used as
#' column name for convenience to make other functions easier to use
#' since they will match relevant columns on name.

# get the joy words from NRC
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# inner join with tidy_books
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# get the net sentiment per 80-line section
library(tidyr)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  # What is %/%? Also check out %%.
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
# %/% test that!

# pretty plots
library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

#' note about tilde: a ~ b + c + d is a common style of notation
#' a is response variable; bcd are explanatory variables
#' a is dependent variable; bcd are independent variables
#' a depends on b, c, and d

# test one book against all lexicons

# filter down just to Pride & Prejudice
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

# summarize by afinn lexicon
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarize(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")
# why did we not do ungroup? (Hint: summarize)

# bing and nrc both use words, not numbers
# so they are approached a bit differently
bing_rows <- pride_prejudice %>% 
  inner_join(get_sentiments("bing")) %>%
  mutate(method = "Bing et al.")

nrc_rows <- pride_prejudice %>% 
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment %in% c("positive", 
                                       "negative"))) %>%
  mutate(method = "NRC")

bing_and_nrc <- bind_rows(bing_rows,
                          nrc_rows) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



# new pretty graph!!!
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# note differences between the graphs

# compare positive vs. negative in NRC, Bing
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

# lets get the most common positive or negative words
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

# let's make a visualization!
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% # why do we have this??
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#' Note the presence of "miss" before? We should add it
#' as a stop word that we don't want to analyze. Our
#' sentiment analysis treats it as a negative, but it's
#' actually a neutral term for a young woman.
custom_stop_words <- bind_rows(tibble(word = c("miss"), 
                                          lexicon = c("custom")), 
                               stop_words)

# Wait, why didn't we use the stop words previously?
# Hint: inner_join with lexicon

# word clouds
library(wordcloud)

# find the most common words in Jane Austen's
# novels
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
# note that it looks different every time you run it


# comparison word cloud
# negative at top, trending towards positive at bottom
# due to a bug or confguration issue, this is working weirdly
# export to PDF to see whole thing
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  # Why do we use acast? Look at the function help and then ...
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  # ... check the first argument of comparision.cloud in the help.
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# start looking at more than unigrams
PandP_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

# look at some interesting sentences
PandP_sentences$sentence[2]
PandP_sentences$sentence[3]

# let's split by chapters
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

# how many chapters per book?
austen_chapters %>% 
  group_by(book) %>% 
  summarize(chapters = n())

#let's find the most negative chapters
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()