library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(ggplot2)

setwd("C:/Users/wheff/Documents/UMD/INST447/Final Group Project")
data <- read.csv("spotify_top_songs_audio_features.csv")

# Correlation test of danceability and number of streams
dance_streams_corr = cor.test(data$danceability, data$streams)
dance_streams_corr

#linear regression analysis
model1 <- lm(danceability ~ streams, data = data)

# Summary of the regression
summary(model1)

#  results of relationship
library(ggplot2)
ggplot(data, aes(x = danceability, y = streams)) +
  geom_point(alpha = 0.5) +  # Using semi-transparent points
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Linear Regression of Danceability and Number of Streams",
       x = "Danceability",
       y = "Number of Streams",
       caption = "Data Source: Spotify")

# Correlation test of tempo and number of streams
tempo_streams_corr = cor.test(data$tempo, data$streams)
tempo_streams_corr

# linear regression analysis
model2 <- lm(streams ~ tempo, data = data)

# Summary of the regression
summary(model2)

#  results of relationship
library(ggplot2)
ggplot(data, aes(x = tempo, y = streams)) +
  geom_point(alpha = 0.5) +  # Using semi-transparent points
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Linear Regression of Tempo and Number of Streams",
       x = "Tempo (BPM)",
       y = "Number of Streams",
       caption = "Data Source: Spotify")

# Correlation test of acousticness and danceability
acoustic_dance_corr = cor.test(data$acousticness, data$danceability)
acoustic_dance_corr

# linear regression analysis
model3 <- lm(acousticness ~ danceability, data = data)

# Summary of the regression
summary(model3)

#  results of relationship
library(ggplot2)
ggplot(data, aes(x = acousticness, y = danceability)) +
  geom_point(alpha = 0.5) +  # Using semi-transparent points
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Linear Regression of Acousticness and Danceability",
       x = "Acousticness",
       y = "Danceability",
       caption = "Data Source: Spotify")
