# Text mining

# This is the second half of the project, here will be used the trackname column of the dataset
# in order to extract useful infos from it for the classification task

library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions


# creating a copy of dataset in order to make analysis w/out changing the original one
df <- dataframe.num

#
# 
# library(keras)
# library(dplyr)
# library(ggplot2)
# library(purrr)
# 
# 
# 
# # the text must be converted to tensors before fed into the neural network
# # First, we create a dictionary and represent each of the 10,000 most common words by an integer. 
# # In this case, every track_name will be represented by a sequence of integers.
# 
# # We can pad the arrays so they all have the same length, then create an integer tensor of shape num_examples * max_length. 
# # We can use an embedding layer capable of handling this shape as the first layer in our network.
# 
# # For python env
# library(reticulate)
# use_condaenv('r-reticulate')
# tf <- import("tensorflow")
# # and check
# tf$constant("Hellow Tensorflow")
# ## Metal device set to: Apple M1 Max
# 
# ## systemMemory: 32.00 GB
# ## maxCacheSize: 10.67 GB
# 
# num_words <- 10000
# max_length <- 50
# text_vectorization <- layer_text_vectorization(
#   max_tokens = num_words, 
#   output_sequence_length = max_length, 
# )
# 
# ### No, tensorflow non funziona sui mac con architettura arm, 
# ### devo usare python nativo senza r-reticulate con la versione di tensorflow compatibile con metal
# 
#


set.seed(42)
library(caTools)
# adding tracknames and resplitting using same seed
df$track_name <- dataframe$track_name
# summary about len of track_name
df$track_name %>%
  strsplit(" ") %>%
  sapply(length) %>%
  summary()





# Writing df for later
write.csv(df,"./df.csv", row.names = FALSE)



df$music_genre <- dataframe$music_genre

split = sample.split(df, SplitRatio = 0.75)
train.t = subset(df, split == TRUE)
test.t = subset(df, split == FALSE)



library(tm)


clean <- function(text){
  # cleaning, numbers left as they are
  co <- VCorpus(VectorSource(text))
  c <- tm_map(co, content_transformer(tolower))
  c <- tm_map(c, removePunctuation)
  c <- tm_map(c, removeWords, stopwords())
  c <- tm_map(c, stripWhitespace)
  # c <- tm_map(c, PlainTextDocument)
  return(c)
}


# Wordcloud
library(wordcloud)




datas <- list()
for (i in seq_along(levels(df$music_genre))){
  datas[[i]] <- df[df$music_genre == levels(df$music_genre)[i], ]
}

corpuses <- list()
for (i in seq_along(datas)){
  corpuses[[i]] <- clean(datas[[i]]$track_name)
}



wordcloud(corpuses[[1]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Alternative")
wordcloud(corpuses[[2]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Anime")
wordcloud(corpuses[[3]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Blues")
wordcloud(corpuses[[4]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Classical")
wordcloud(corpuses[[5]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Country")
wordcloud(corpuses[[6]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Electronic")
wordcloud(corpuses[[7]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Hip-Hop")
wordcloud(corpuses[[8]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Jazz")
wordcloud(corpuses[[9]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Rap")
wordcloud(corpuses[[10]], min.freq=10, random.order = FALSE, colors=brewer.pal(8, "Dark2"), max.words=150, main = "Rock")


#####################
# Writing corpus
# corpuses[[i]][[j]][["content"]]


for (i in seq_along(corpuses)){
  tmp_df <- list()
  for (j in seq_along(corpuses[[i]])){
    tmp_df <- append(tmp_df, corpuses[[i]][[j]][["content"]])
  }
  write.csv(do.call(rbind.data.frame, tmp_df), sprintf("./corpuses/df_%d.csv", i), row.names = FALSE)
  tmp_df <- list()
}



#####################

# building a corpus for train and test
corpus.train <- clean(train.t$track_name)
corpus.test <- clean(test.t$track_name)


# Generating sparse matrix where columns: words; rows: each track_name
dtm.train <- DocumentTermMatrix(corpus.train)
dtm.test <- DocumentTermMatrix(corpus.test)



find_freq_terms <- function(corpus_in){
  doc_term_mat <- TermDocumentMatrix(corpus_in)
  freq_terms <- findFreqTerms(doc_term_mat)[1:max(doc_term_mat$nrow)]
  terms_grouped <- doc_term_mat[freq_terms,] %>%
    as.matrix() %>%
    rowSums() %>%
    data.frame(Term=freq_terms, Frequency = .) %>%
    arrange(desc(Frequency)) %>%7
    mutate(prop_term_to_total_terms=Frequency/nrow(.))
  return(data.frame(terms_grouped))
}



# picking top 6 words for every class
bow <- c(find_freq_terms(corpuses[[1]])[1:6,]$Term,  # Alternative
         find_freq_terms(corpuses[[2]])[1:6,]$Term,  # Anime
         find_freq_terms(corpuses[[3]])[1:6,]$Term,  # Blues
         find_freq_terms(corpuses[[4]])[1:6,]$Term,  # Classical
         find_freq_terms(corpuses[[5]])[1:6,]$Term,  # Country
         find_freq_terms(corpuses[[6]])[1:6,]$Term,  # Electronic
         find_freq_terms(corpuses[[7]])[1:6,]$Term,  # Hip-Hop
         find_freq_terms(corpuses[[8]])[1:6,]$Term,  # Jazz
         find_freq_terms(corpuses[[9]])[1:6,]$Term,  # Rap
         find_freq_terms(corpuses[[10]])[1:6,]$Term) # Rock



for (i in seq_along(corpuses)){
  # for ele in bow
    # if bow in corpuses[[i]]:
      # df[bow] = 1
}

# for (i in )
#   for ele in bow
#     if bowcorpuses[[i]] contains 





























