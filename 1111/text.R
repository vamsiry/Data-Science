
install.packages("tidytext")

library("gutenbergr")
library("tidyverse")
library("tidytext")
library("zoo")


macbeth = gutenberg_works(title == "Macbeth") %>%gutenberg_download()

head(macbeth)


macbeth %>%
  unnest_tokens(word, text) %>% # Make text tidy
  count(word, sort = TRUE) %>% # Count occurances
  anti_join(stop_words, by = "word") %>% # Remove stop words
  head(10) %>% # Select top 10
  ggplot(aes(word, n)) + # Plot
  geom_col() 




speaker_words = macbeth %>%
  mutate(is_speaker = str_detect(text, "^[A-Z ]+\\.$"), # Detect capital letters
         speaker = ifelse(is_speaker, text, NA),
         speaker = na.locf(speaker, na.rm = FALSE))


speaker_words = speaker_words %>%
  filter(!is_speaker, !is.na(speaker)) %>%
  select(-is_speaker, -gutenberg_id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")


speaker_words %>%
  count(speaker, word, sort = TRUE) %>%
  bind_tf_idf(word, speaker, n) %>%
  arrange(desc(tf_idf)) %>%
  filter(n >= 5)
















