library(spotifyr)
library(tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = '68e637a306ec4508a0fdaa026b141147')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '14092029658947babc08f75cc6ada847')

access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))



# dataframe of audio features by artist
singer <- "Westlife"
artist_name <- get_artist_audio_features(singer)

# analyse artist tracks by key
musicKey_count <- count(artist_name, key_mode, sort = T)

# most joyful artist songs by quantile
artist_valence <- artist_name %>%
  filter(valence, valence > quantile(valence, 0.95)) %>%
  arrange(-valence)


# function to correlate audio feature against track popularity
audio_feature_popularity_correlations <- function(artist) {
  artist_name <- get_artist_audio_features(artist)
  artist_correlations <- c(cor(artist_name$danceability, artist_name$track_popularity),
                           cor(artist_name$energy, artist_name$track_popularity),
                           cor(artist_name$loudness, artist_name$track_popularity),
                           cor(artist_name$speechiness, artist_name$track_popularity),
                           cor(artist_name$acousticness, artist_name$track_popularity),
                           cor(artist_name$instrumentalness, artist_name$track_popularity),
                           cor(artist_name$liveness, artist_name$track_popularity),
                           cor(artist_name$valence, artist_name$track_popularity),
                           cor(artist_name$tempo, artist_name$track_popularity))
  
  names(artist_correlations) = c("danceability", "energy", "loudness", "speechiness",
                                 "acousticness",
                                 "instrumentalness",
                                 "liveness",
                                 "valence",
                                 "tempo") 
  
  artist_correlations <- artist_correlations %>% 
    map(~ as_tibble(.x)) %>% 
    map2(names(.), ~ add_column(.x, Audio_Feature = rep(.y, nrow(.x)), .before = "value")) %>% 
    bind_rows() %>%
    add_column(Artiste_Name = artist, .before = "Audio_Feature")
}

popularity_correlation <- audio_feature_popularity_correlations('Michael Learns To Rock')






