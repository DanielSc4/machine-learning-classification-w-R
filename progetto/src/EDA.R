# EDA


dataframe <- read.csv(file = 'music_genre.csv')


# ---------------------------------------
source('./src/functions.R')
# Libraries
library(plyr)
library(ggplot2)
library(GGally)
library(dplyr)
library(ggcorrplot)
library(ggstatsplot)
library(fastDummies)
# ---------------------------------------





# Displayng first six values
head(dataframe)

# Columns:
ncol(dataframe)

# Rows:
nrow(dataframe)

# Na values:
colSums(is.na(dataframe))
# There are some na values
# Dropping na values (only five rows)
dataframe <- na.omit(dataframe)


# Types of attributes:
sapply(dataframe, class)


dataframe$music_genre <- as.factor(dataframe$music_genre)
dataframe$instance_id <- NULL
dataframe$obtained_date <- NULL
dataframe$obtained_date <- NULL

# Basic descriptive statistics for evey column
summary(dataframe)



# -------------------------- #
# Exploring every attribute  #
# -------------------------- #

##############################
# track_name

# adding a column containing length of track_name, may it be useful?
library(stringr)
dataframe$length_title <- str_count(dataframe$track_name)
dataframe$length_title <- as.numeric(dataframe$length_title)

summary(dataframe$length_title)

ggplot(dataframe, aes(x=music_genre, y=length_title, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

# Yess, calssical music has significant longer tiles


### HIST ###
ggplot(dataframe, aes( x=length_title, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("length_title")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###


##############################
# popularity

summary(dataframe$popularity)

ggplot(dataframe, aes(x=music_genre, y=popularity, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=popularity, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("popularity")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###




##############################
# acousticness

summary(dataframe$acousticness)

ggplot(dataframe, aes(x=music_genre, y=acousticness, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=acousticness, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("acousticness")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###




##############################
# danceability

summary(dataframe$danceability)

ggplot(dataframe, aes(x=music_genre, y=danceability, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")


### HIST ###
ggplot(dataframe, aes( x=valence, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("danceability")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###





##############################
# duration_ms

summary(dataframe$duration_ms)

# -1 obv is not a valid value
length(dataframe$duration_ms[dataframe$duration_ms == -1])/nrow(dataframe) * 100
# About 10% of missing values
# replacing with median
dataframe$duration_ms[dataframe$duration_ms == -1] <- NA
dataframe$duration_ms[is.na(dataframe$duration_ms)] <- median(dataframe$duration_ms, na.rm = T)

ggplot(dataframe, aes(x=music_genre, y=duration_ms, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

# replacing outliers with median
outliers <- boxplot(dataframe$duration_ms, plot = FALSE)$out
dataframe[dataframe$duration_ms %in% outliers, "duration_ms"] <- median(dataframe$duration_ms, na.rm = T)

ggplot(dataframe, aes(x=music_genre, y=duration_ms, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=duration_ms, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("duration_ms")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###




##############################
# energy

summary(dataframe$energy)

ggplot(dataframe, aes(x=music_genre, y=energy, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=energy, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("energy")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###




##############################
# instrumentalness, will we discard this feature?


summary(dataframe$instrumentalness)


ggplot(dataframe, aes(x=music_genre, y=instrumentalness, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")


### HIST ###
ggplot(dataframe, aes( x=instrumentalness, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("instrumentalness")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###

# 0 value seems a sus value
length(dataframe$instrumentalness[dataframe$instrumentalness == 0])/nrow(dataframe) * 100
# About 30% of missing values
# Make no sense to use median so dropping this attribute, goodbye my old friend
dataframe$instrumentalness <- NULL






##############################
# Key, categorical value

#ggplot(dataframe, aes(x=music_genre, y=acousticness, fill=music_genre)) + 
#  geom_boxplot(alpha=0.9) +
#  theme(legend.position="none") +
#  scale_fill_brewer(palette="Set3")



##############################
# Liveness

ggplot(dataframe, aes(x=music_genre, y=liveness, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=liveness, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("liveness")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###





##############################
# Loudness

ggplot(dataframe, aes(x=music_genre, y=loudness, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=loudness, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("loudness")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###






##############################
# mode, categorical value

#ggplot(dataframe, aes(x=music_genre, y=liveness, fill=music_genre)) + 
#  geom_boxplot(alpha=0.9) +
#  theme(legend.position="none") +
#  scale_fill_brewer(palette="Set3")




##############################
# Speechiness

ggplot(dataframe, aes(x=music_genre, y=speechiness, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

# replacing outliers with median
outliers <- boxplot(dataframe$speechiness, plot = FALSE)$out
dataframe[dataframe$speechiness %in% outliers, "speechiness"] <- median(dataframe$speechiness, na.rm = T)

ggplot(dataframe, aes(x=music_genre, y=speechiness, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=speechiness, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("speechiness")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###




##############################
# Tempo

summary(dataframe$tempo)

# In tempo there are ? values, obv missing values (about 10% of the entire dataset)
dataframe$tempo[dataframe$tempo == '?'] <- NA
dataframe$tempo <- as.numeric(dataframe$tempo)
# replacing missing values in tempo with the median
dataframe$tempo[is.na(dataframe$tempo)] <- median(dataframe$tempo, na.rm = T)

summary(dataframe$tempo)

ggplot(dataframe, aes(x=music_genre, y=tempo, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=tempo, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("tempo")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###





##############################
# Valence


ggplot(dataframe, aes(x=music_genre, y=valence, fill=music_genre)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

### HIST ###
ggplot(dataframe, aes( x=valence, fill=music_genre))+
  geom_histogram(alpha=0.4,position="identity")+
  xlab("valence")+
  scale_y_continuous("Number of Observations")+
  scale_fill_brewer(palette="Set3")
### .... ###







# Quick recap about what's been done:
# - TODO


#for (i in 1:length(dataframe)){
#  p = ggplot(dataframe, aes_string(x="music_genre", y=names(dataframe)[i]) , fill="music_genre") + geom_boxplot(alpha=0.9)
#}
#p









# -------------------------- #
# Pairs plots                #
# -------------------------- #

# scatterplot matrix
ggpairs(select_if(dataframe, is.numeric), 
        aes(colour = dataframe$music_genre))


