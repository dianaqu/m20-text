# Exercise-2
# What are informatics courses about?

# Set up
# install.packages('tidytext')
# install.packages('dplyr')
# install.packages('stringr')
# install.packages('ggplot2')
# install.packages('rvest')
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(rvest)

# Read in web page
page <- read_html('https://www.washington.edu/students/crscat/info.html')

# Extract descriptions of each course into a dataframe (may take multiple steps)
course.titles <- page %>% html_nodes('p b') %>% html_text() 
descriptions <- page %>% html_nodes('p') %>% html_text()
classes <- data.frame(title = course.titles, description = descriptions[2:length(descriptions)], stringsAsFactors = FALSE)

# How many courses are in the catalogue?
length(unique(classes$title))

# Create a tidytext sturcture of all words
all.words <- classes %>% unnest_tokens(word, description)

# Which words do we use to describe our courses?
word.count <- all.words %>% group_by(word) %>% summarize(count = n()) %>% arrange(-count)

# Create a set of stop words by adding (more) irrelevant words to the stop_words dataframe
more.stop.words <- data.frame(
  word = c("course", "info", "information"),
  lexicon = "custom"
)
all.stop.words <- cbind(stop_words, more.stop.words)

# Remove stop words by performing an anti_join with the stop_words dataframe
no.stop.words <- all.words %>% anti_join(all.stop.words, by="word")

# Which non stop-words are most common?
non.stop.count <- no.stop.words %>% group_by(word) %>% summarize(count = n()) %>% arrange(-count)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words

no.stop.words %>% 
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()