# all examples based on https://www.tidytextmining.com/topicmodeling.html

library(topicmodels)

data("AssociatedPress")
AssociatedPress

# what is this data? https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf

library(dplyr)
library(tidytext)

# try to view AssociatedPress
View(AssociatedPress)
# this is a good time to explain the list!

# convert to dataframe and view it
View(tidy(AssociatedPress))

# latent Dirichlet allocation
# https://www.youtube.com/watch?v=jXnu3P5uBBs
# Two key principles:
# 1. Every document is a mixture of topics.
# 2. Every topic is a mixture of words.

# this will take some time
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

# why seed?

# note the order of the words
library(tm)
Terms(AssociatedPress)
# Where to start? With first word every time? That would bias the results.
# Specifying a random seed, but the same random seed, can make results
# more predictable.

# if seed not specified, would use as.integer(Sys.time())
# may produce slightly different results each time
# that is not _wrong_, but we want the same results for this exercise

# β = per-topic-per-word probabilities
# "probability of that term being generated from that topic"
# γ = per-document-per-topic (used later on)
ap_topics <- tidy(ap_lda, matrix = "beta")

# picture time!
library(ggplot2)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# let's see words in each topic
ap_top_terms %>%
  # why again is next line needed? We covrered this in
  # an earlier class.
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# which words had the greatest difference between the topics?
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>% # We may run into an error here. Why?
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# why log ratio? makes analysis easier!
# if x = y, then log(x/y) = 0
# suppose one is a ratio of the other:
# if kx = y, then log(x/y) = j
# if x = ky, then log(x/y) = -j

# now let's do document-topic probabilities
# γ = per-document-per-topic probabilities
# "estimated proportion of words from that document that are generated from that topic"
ap_documents <- tidy(ap_lda, matrix = "gamma")

# notice how little of document 6 came from topic 1

# let's look at document 6 more closely
tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

# the great chapter heist!

# the books we'll use
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

# get the books by title instead of ID
library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

# we are going to be the vandal and divide the books into chapters
library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE)

# we have to cast as a DocumentTermMatrix before we can run LDA on it
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n) # each chapter is a separate document

# now let's run LDA on all of these documents
# note that we are looking for four topics this time
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))

# now look at per-topic, per-word probabilities
chapter_topics <- tidy(chapters_lda, matrix = "beta")

# let's look at the terms most likely to appear in each of the topics
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# gamma = percent of words generated from that topic
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

# separate out the title and chapter
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

#' note: about to make Tukey boxplot
#' parts:
#'   -median
#'   -box: straddles median and extends to 25% and 75% percentile values
#'   -fences (extremes): lines that are this far away from median: (75% percentile
#'    value - 25% percentile value) * 1.5. If most extreme value is inside the fence, then
#'    draw the fence at that value.
#'   -whiskers: connects the ends of the box to the whiskers
#'   -dots: shows all observations outside the fences

# reorder titles in order of topic 1, topic 2, etc before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) + # why were x and y not specified here?
  geom_boxplot() +
  facet_wrap(~ title)

# which topic was associated with each chapter?
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

#' compare consensus topic for each book to see which were
#' most often misindentified
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  # why is the "by" parameter needed on next command?
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

# regular expressions
# https://r4ds.had.co.nz/strings.html#matching-patterns-with-regular-expressions

# Trump insider op-ed
# http://varianceexplained.org/r/op-ed-text-analysis/
# https://www.nytimes.com/2018/09/05/opinion/trump-white-house-anonymous-resistance.html