########################################################## This file generate NGrams and save them in files for the next module #############################################
#############################################################################################################################################################################

#Set the working directory
setwd("C:/Users/gehad/Desktop/final/en_US")

# loading required packages

library(stringr)
library(tau)
library(dplyr)
##########################

# read the raw data
twitter_raw_data   <- readLines("en_US.twitter.txt", encoding = "UTF-8")
news_raw_data      <- readLines("en_US.news.txt", encoding = "UTF-8")
blog_raw_data      <- readLines("en_US.blogs.txt", encoding = "UTF-8")

##### The data is too large so let's sample it 
# sample the data

set.seed(100)
sample_data_twitter <- sample(twitter_raw_data, 150000)
sample_data_news    <- sample(news_raw_data, 1000)
sample_data_blog    <- sample(blog_raw_data, 1000)
dirty_corpus        <- c(sample_data_twitter, sample_data_news, sample_data_blog)
dirty_corpus <- iconv(dirty_corpus, "UTF-8", "ASCII", "?")
dirty_corpus <- unlist(strsplit(dirty_corpus , "[\\.\\!\\?\\:]+"))



clean_text <- function(input_text){
  
  output_text <- ""
  input_text <- tolower(input_text)
  input_text <- str_replace_all(input_text, "\\S+@\\S+", "") 
  input_text <- str_replace_all(input_text, "http[[:alnum:]]*", "")
  input_text <- str_replace_all(input_text, "#[[:alnum:]]*", "")
  input_text <- str_replace_all(input_text, "# [[:alnum:]]*", "")
  input_text <- str_replace_all(input_text, "@[[:alnum:]]*", "")
  input_text <- str_replace_all(input_text, "@ [[:alnum:]]*", "")
  input_text <- str_replace_all(input_text, "RT", "")
  input_text <- str_replace_all(input_text, "PM", "")
  input_text <- str_replace_all(input_text, "rt", "")
  input_text <- str_replace_all(input_text, "pm", "")
  input_text <- str_replace_all(input_text, "'ll", " will")
  input_text <- str_replace_all(input_text, "'d", " would")
  input_text <- str_replace_all(input_text, "can't", "cannot")
  input_text <- str_replace_all(input_text, "n't", " not")
  input_text <- str_replace_all(input_text, "'re", " are")
  input_text <- str_replace_all(input_text, "'m", " am")
  input_text <- str_replace_all(input_text, "n'", " and")
  input_text <- str_replace_all(input_text, "'s", " ")
  input_text <- str_replace_all(input_text, "s'", " ")
  input_text <- str_replace_all(input_text, "[^[:alnum:]]", " ")
  input_text <- str_replace_all(input_text, "[:digit:]", "")
  input_text <- str_replace_all(input_text, "\\s+", " ")
  input_text <- str_trim(input_text, side = c("both"))
  input_text <- str_replace_all(input_text, "don t", "do not")
  input_text <- str_replace_all(input_text, "dont", "do not")
  input_text <- str_replace_all(input_text, "u s", "usa")
  
  output_text <- input_text
  
  return(output_text)
}
# clean the corpus
clean_corpus <- clean_text(dirty_corpus) 

# create N-grams
unigram_trie  <- textcnt(clean_corpus, n = 1L, method = "string", split = " ")
bigram_trie   <- textcnt(clean_corpus, n = 2L, method = "string", split = " ")
trigram_trie  <- textcnt(clean_corpus, n = 3L, method = "string", split = " ")
fourgram_trie <- textcnt(clean_corpus, n = 4L, method = "string", split = " ")
fivegram_trie <- textcnt(clean_corpus, n = 5L, method = "string", split = " ")

# convert unigram_trie to dataframe
unigram_df <- data.frame(counts = unclass(unigram_trie), size = nchar(names(unigram_trie)))
unigram_df$n.gram <- rownames(unigram_df)
rownames(unigram_df) <- NULL

# convert bigram_trie to dataframe
bigram_df <- data.frame(counts = unclass(bigram_trie), size = nchar(names(bigram_trie)))
bigram_df$n.gram <- rownames(bigram_df)
rownames(bigram_df) <- NULL

# convert trigram_trie to dataframe
trigram_df <- data.frame(counts = unclass(trigram_trie), size = nchar(names(trigram_trie)))
trigram_df$n.gram <- rownames(trigram_df)
rownames(trigram_df) <- NULL

# convert fourgram_trie to dataframe
fourgram_df <- data.frame(counts = unclass(fourgram_trie), size = nchar(names(fourgram_trie)))
fourgram_df$n.gram <- rownames(fourgram_df)
rownames(fourgram_df) <- NULL

# convert fivegram_trie to dataframe
fivegram_df <- data.frame(counts = unclass(fivegram_trie), size = nchar(names(fivegram_trie)))
fivegram_df$n.gram <- rownames(fivegram_df)
rownames(fivegram_df) <- NULL


# drop only once N gram

################ unigram ###########################

counts = unclass(unigram_trie)
unigram_short  <- filter(unigram_df, counts > 1) %>%
  select(n.gram, counts)

save(unigram_short, file="unigram_final.Rda")

####################################################

################# bigram ###########################
counts = unclass(bigram_trie)
bigram_short   <- filter(bigram_df, counts  > 1) %>%
  select(n.gram, counts)

save(bigram_short, file="bigram_final.Rda")

####################################################

################ trigram ###########################
counts = unclass(trigram_trie)
trigram_short   <- filter(trigram_df, counts  > 1) %>%
  select(n.gram, counts)

save(trigram_short, file="trigram_final.Rda")

#####################################################

############### fourgram ############################
counts = unclass(fourgram_trie)
fourgram_short   <- filter(fourgram_df, counts  > 1) %>%
  select(n.gram, counts)

save(fourgram_short, file="fourgram_final.Rda")

#####################################################

############## fivegram #############################
counts = unclass(fivegram_trie)
fivegram_short   <- filter(fivegram_df, counts  > 1) %>%
  select(n.gram, counts)

save(fivegram_short, file="fivegram_final.Rda")

######################################################
