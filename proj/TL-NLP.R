
library(data.table)
library(ggplot2)
library(tm)

# read in data ======
data_path <- 'C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\nlp\\'
clean = ''
list.files(data_path)

dat_bio <- read.csv(paste0(data_path, 'biology',clean,'.csv'), stringsAsFactors = F)
dat_cook <- read.csv(paste0(data_path, 'cooking',clean,'.csv'), stringsAsFactors = F)
dat_crypt <- read.csv(paste0(data_path, 'crypto',clean,'.csv'), stringsAsFactors = F)
dat_diy <- read.csv(paste0(data_path, 'diy',clean,'.csv'), stringsAsFactors = F)
dat_robot <- read.csv(paste0(data_path, 'robotics',clean,'.csv'), stringsAsFactors = F)
dat_travel <- read.csv(paste0(data_path, 'travel',clean,'.csv'), stringsAsFactors = F)
dat_physic <- read.csv(paste0(data_path, 'test',clean,'.csv'), stringsAsFactors = F)

# attach a category label
dat_bio$category <- 'biology'
dat_cook$category <- 'cooking'
dat_crypt$category <- 'crypto'
dat_diy$category <- 'diy'
dat_robot$category <- 'robotics'
dat_travel$category <- 'travel'

dat_physic$tags <- NA
dat_physic$tags <- as.character(dat_physic$tags)
dat_physic$category <- 'physic'

# combine and remove from environment
dat_all <- rbind(dat_bio, dat_cook, dat_crypt, dat_diy, dat_robot, dat_travel,dat_physic)
rm(dat_bio, dat_cook, dat_crypt, dat_diy, dat_robot, dat_travel, dat_physic)


# duplicate id values across categories, concat id to category and we're good
print(sum(duplicated(dat_all$id))) 



dat_all$id_cat <- paste0(dat_all$id, '|', dat_all$category)
print(sum(duplicated(dat_all$id_cat)))  # 0 perrrfect


#Declaring My Custom Functions
#----------------------------------
#remove_html_tags removes all html tags from a string - this stack overflow

#custom_tokenizer takes in a string and makes it lowercase, removes punctuation,
#removes new line and return characters, removes any extra whitespace beyond a
#single space, then splits the string by spaces - here is the link for removing
#extra white space


# removes all html tags
remove_html_tags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


# custom tokenizer
custom_tokenizer <- function(param_big_string) {
  #' lower-cases, removes punctuation, new line and return characters, 
  #' and removes unnecessary whitespace, then strsplits 
  split_content <- sapply(param_big_string, removePunctuation, preserve_intra_word_dashes=T)
  split_content <- sapply(split_content, function(y) gsub("[\r\n]", " ", y))
  split_content <- sapply(split_content, tolower)
  split_content <- sapply(split_content, function(y) gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", y, perl=TRUE))
  return(split_content)
  #return(split_content <- (sapply(split_content, strsplit, " ")))
}


#Clean, Process
----------------
# quick clean on content column
dat_all$content <- remove_html_tags(dat_all$content)

# tokenize
dat_all$content <- custom_tokenizer(dat_all$content)
dat_all$tags <- custom_tokenizer(dat_all$tags)
dat_all$title <- custom_tokenizer(dat_all$title)

names(dat_all)

#Text Mining
#TF-IDF method
install.packages("tidytext")
library(dplyr)
library(stringr)
library(tidytext)

original_books <- data_frame(line = 1:length(dat_all$content), text = dat_all$content, category = dat_all$category)

names(original_books)

stack_words <- original_books %>%
  unnest_tokens(word, text) %>%
  count(category, word, sort = TRUE) %>%
  ungroup()

head(stack_words)

stack_words <- stack_words %>%
  bind_tf_idf(word, category, n) 

stack_words


#plots
plot_stack <- stack_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(category = factor(category, levels = c("cooking","diy",
                                                "biology","crypto",
                                                "robotics","travel","physic")))

ggplot(plot_stack[1:20,], aes(word, tf_idf, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

plot_stack <- plot_stack %>% 
  group_by(category) %>% 
  top_n(15, tf_idf) %>% 
  mutate(word = reorder(word, tf_idf))


ggplot(plot_stack, aes(word, tf_idf, fill = category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~category, ncol = 2, scales = "free") +
  ggtitle("TF-IDF on content") + 
  coord_flip()


#We will compute the same code for the title in order to compare the result on title and content
original_books <- data_frame(line = 1:length(dat_all$content), text = dat_all$title, category = dat_all$category)

stack_words <- original_books %>%
  unnest_tokens(word, text) %>%
  count(category, word, sort = TRUE) %>%
  ungroup()

stack_words <- stack_words %>%
  bind_tf_idf(word, category, n) 

plot_stack <- stack_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(category = factor(category, levels = c("cooking","diy",
                                                "biology","crypto",
                                                "robotics","travel","physic")))

ggplot(plot_stack[1:20,], aes(word, tf_idf, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

#Wordcloud tag

#Now, let's do some wordcloud with the tags. We would like to compare the best tag with the top word found in the part TF-IDF.

#You can see below a "simple" wordcloud of the cooking catgeory

library(wordcloud)
original_books <- data_frame(line = 1:length(dat_all$content), text = dat_all$tags, category = dat_all$category)

stack_words <- original_books %>%
  ungroup() %>%
  unnest_tokens(word, text)

dark2 <- brewer.pal(6, "Dark2")  # Used for the color in wordcloud
stack_words %>%
  filter(category=="cooking")  %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,colors = dark2,rot.per = 0.2))


#To avoid doing 6 wordcloud (one for each catgeory) we'll try to combine all of the wordcloud :
library(reshape2)

stack_words %>%
  filter(category != "physic")  %>%
  count(word, category, sort = TRUE) %>%
  acast(word ~ category, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4",
                              "#00B2FF", "red", 
                              "#FF0099", "#6600CC"),
                   max.words = 100)


#Predict the tag
#In this part, we will predict the tag on the test data. We perform in 2 steps : general and local.
#The general part is done before. Indeed, we keep the best word found in the part tf-idf. For each question that we have to predict the tag, we will use one of these tags named "potential_tag".

potential_tag <- plot_stack %>%
filter(category=="physic")
potential_tag = as.character(potential_tag$word)
potential_tag


#These words allow to understand that we are in the category "physic".

#The second step is based on create a tf idf split on each question, and not 
#split on each category. It allows to precise the tag for each question.

dat_physic = dat_all %>%
  filter(category=="physic")
corpus <- data_frame(line = 1:length(dat_physic$content), text = dat_physic$content,
                     id = factor(dat_physic$id))

stack_words <- corpus %>%
  unnest_tokens(word, text) %>%
  count(id,word, sort = TRUE) %>%
  ungroup()

stack_words <- stack_words %>%
  bind_tf_idf(word,id, n) 

stack_words


#Now, we'll keep the 3 best word order by tf-idf for each question.
df <- stack_words %>% 
  group_by(id) %>% 
  arrange(desc(tf_idf)) %>%
  top_n(3, tf_idf)








