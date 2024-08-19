library(ggplot2)
library(readr)
library(dplyr)
data <- read.csv("spotify_top_songs_audio_features.csv")
str(data)
head(data)
summary(data[, c('danceability', 'energy', 'speechiness', 'acousticness', 
                 'instrumentalness', 'liveness', 'valence', 'loudness', 
                 'tempo', 'duration_ms', 'weeks_on_chart', 'streams')])

artist_streams <- data %>%
  group_by(artist_names) %>%
  summarise(total_streams = sum(streams)) %>%
  arrange(desc(total_streams)) %>%
  slice_head(n = 10)


# Bar Plot of Top 10 Artists by Stream Count
ggplot(artist_streams, aes(x = reorder(artist_names, -total_streams), y = total_streams, fill = artist_names)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Artists by Stream Count") +
  xlab("Artist Names") + ylab("Total Streams") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = danceability)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Histogram of Danceability") +
  xlab("Danceability") + ylab("Frequency")

ggplot(data, aes(x = danceability, y = energy)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatter Plot of Danceability vs Energy") +
  xlab("Danceability") + ylab("Energy")


ggplot(data, aes(x = streams)) +
  geom_histogram(bins = 50, fill = "green", color = "black") +
  ggtitle("Histogram of Streams") +
  xlab("Streams") + ylab("Frequency") +
  scale_x_log10() + # Using log scale due to wide range of stream counts
  theme_minimal()


