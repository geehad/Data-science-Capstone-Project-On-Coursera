##### load the required packages

library("dplyr")
library("stringr")

################################

##### load the final generated NGrams from NGRam generator module

load("unigram_final.Rda")
load("bigram_final.Rda")
load("trigram_final.Rda")
load("fourgram_final.Rda")
load("fivegram_final.Rda")

################################

##### get last N last words from input text 

GetLastWords <- function(text, last) {
  split_Patt <- paste("[a-z']+( [a-z']+){", last - 1, "}$", sep="")
  return(substring(text, str_locate(text, split_Patt)[,1]))
}


##### implementation of Stupid Backoff Algorithm

predict_next_word <- function(text_string){
  
  last_four_words <- GetLastWords(text_string, 4)
  last_three_words <- GetLastWords(last_four_words, 3)
  last_two_words <- GetLastWords(last_three_words, 2)
  last_word <- GetLastWords(last_two_words, 1)
  
  # Keep top 3 predicted words from  5 Gram Table
  Predict_5_Gram <- fivegram_short %>%
    
    filter(grepl(paste0("^", last_four_words," "), n.gram)) %>%
    mutate(word_predict = GetLastWords(n.gram, 1))  %>%
    mutate(prop = counts/fourgram_short$counts[fourgram_short$n.gram == last_four_words]) %>%
    arrange(desc(prop)) %>%
    select(word_predict, prop) %>%
    top_n(3) 
  
  # Keep top 3 predicted words from  4 Gram Table
  Predict_4_Gram <- fourgram_short %>%  
    filter(grepl(paste0("^", last_three_words," "), n.gram)) %>%
    mutate(word_predict = GetLastWords(n.gram, 1))  %>% 
    mutate(prop = (counts*0.4)/trigram_short$counts[trigram_short$n.gram == last_three_words]) %>%
    arrange(desc(prop)) %>%
    select(word_predict, prop) %>%
    top_n(3) 
  
  # Keep top 3 predicted words from  3 Gram Table
  Predict_3_Gram <- trigram_short %>%  
    filter(grepl(paste0("^", last_two_words," "), n.gram)) %>%
    mutate(word_predict = GetLastWords(n.gram, 1))  %>% 
    mutate(prop = (counts*0.4*0.4)/bigram_short$counts[bigram_short$n.gram == last_two_words]) %>%
    arrange(desc(prop)) %>%
    select(word_predict, prop) %>%
    top_n(3) 
  
  # Keep top 3 predicted words from  2 Gram Table
  Predict_2_Gram <- bigram_short %>%  
    filter(grepl(paste0("^", last_word," "), n.gram)) %>%
    mutate(word_predict = GetLastWords(n.gram, 1))  %>% 
    mutate(prop = (counts*0.4*0.4*0.4)/unigram_short$counts[unigram_short$n.gram == last_word]) %>%
    arrange(desc(prop)) %>%
    select(word_predict, prop) %>%
    top_n(3) 
  
  # Keep top 3 predicted words from  1 Gram Table
  Predict_1_Gram <- unigram_short %>%  
    arrange(desc(counts)) %>% 
    mutate(word_predict = n.gram)  %>% 
    mutate(prop = (counts*0.4*0.4*0.4*0.4)/sum(unigram_short$counts)) %>%
    arrange(desc(prop)) %>%
    select(word_predict, prop) %>%
    top_n(3) 
  
  
  Table_Probablity <- rbind(Predict_5_Gram,Predict_4_Gram,Predict_3_Gram,Predict_2_Gram, Predict_1_Gram) %>%
    arrange(desc(prop))  
  

  Table_Probablity <- Table_Probablity[!duplicated(Table_Probablity$word_predict),]
  
  Table_Probablity <- Table_Probablity %>%
    top_n(3)
  
  
  return(Table_Probablity[1,"word_predict"])
}

