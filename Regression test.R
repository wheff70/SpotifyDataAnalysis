library(dplyr)
data <- read.csv("spotify_top_songs_audio_features.csv")

# Ensure correct format for 'streams' and 'tempo' for conversion 
data$streams <- as.numeric(data$streams)
data$tempo <- as.numeric(data$tempo)

#linear regression analysis
model <- lm(streams ~ tempo, data = data)

# Summary of the regression
summary(model)

#  results of relationship
library(ggplot2)
ggplot(data, aes(x = tempo, y = streams)) +
  geom_point(alpha = 0.5) +  # Using semi-transparent points
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Linear Regression of Streams on Tempo",
       x = "Tempo (BPM)",
       y = "Number of Streams",
       caption = "Data Source: Spotify")
