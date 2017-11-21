# Exercise-1
# Developed from: http://tidytextmining.com/

# Set up (install packages that you don't have)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# Load Jane Austen books into a dataframe using the austen_books() function
book.data <- austen_books() %>% 
  mutate(book = as.character(book))

# How many books are in the dataset?
book.count <- book.data$book %>% unique() %>% length()

# Which book has the most lines?
longest.book <- book.data %>%
  group_by(book) %>% 
  summarize(lines = n()) %>% 
  arrange(-lines) %>% 
  filter(lines == max(lines)) %>% 
  select(book)

# Use the unnest_tokens function to generate the full list of words
word.list <- book.data %>%
  unnest_tokens(word, text)

# Which words are most common (regardless of which book them come from)?
word.freq <- word.list %>% 
  group_by(word) %>%
  summarize(count = n()) %>% 
  arrange(-count)

# Remove stop words by performing an anti_join with the stop_words dataframe
word.freq.clean <- word.freq %>% 
  anti_join(stop_words, by = 'word')

# Which non stop-words are most common?
common.words <- word.freq.clean %>% 
  arrange(-count) %>% 
  head(n = 15)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
common.words$word.factor <- factor(common.words$word, levels = common.words$word) # set word as a factor to ensure 
ggplot(common.words) +
  geom_bar(mapping = aes(x = word.factor, y = count), stat = 'identity')
