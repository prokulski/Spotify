setwd("~/RProjects/Spotify")
rm(list= ls())

library(tidyverse)
library(stringr)
library(randomForest)

source("spotify_functions.R")
load("spotify_cred.rda") # zmienne client_id i client_secret

spotify_token <- getSpotifyToken()

# parametry list:
## piosenki, które lubię:
### HARD ROCK 80's 90's 2000
user_like <- "migueljusan"
playlist_like <- "66zMURup6R6yWFVDBTdDaj"
## piosenki, których nie lubię:
### Discoteque
user_dislike <- "meemeesiko"
playlist_dislike <- "3Ld8PkpgMGQQEWPH3sr3Xj"
## piosenki testowe:
### Chillout / Mellow / Soft Rock - 70's 80's 90's
user_test <-  "centralia"
playlist_test <- "5KKBIKSTKg6rp6KiOyobOo"


# pobranie utworów z playlisty lubię
tracks_like <- getSpotifyUserPlaylist(user_like, playlist_like)

# pobranie utworów z playlisty nie lubię
tracks_dislike <- getSpotifyUserPlaylist(user_dislike, playlist_dislike)

# łączymy listy z jednoczesnym oznaczeniem lubię/nielubię
tracks <- bind_rows(tracks_like %>% mutate(liked = TRUE),
                    tracks_dislike %>% mutate(liked = FALSE)) %>%
   distinct(track_id, .keep_all = TRUE)

# pobieramy cechy utworów
tracks_features <- tracks$track_id %>% map_df(getSporifyTrackFeatures)

# dodajemy informacje o tytułach itp do cech
tracks_all <- left_join(tracks_features,
                        tracks %>% select(track_id, track, artist, album, liked),
                        by = c("id" = "track_id")) %>%
   select(-time_signature) %>%
   mutate(liked = as.factor(liked))


# jak różnią się cechy?
tracks_all %>%
   select(-track, -album, -artist) %>%
   gather(feature, value, -id, -liked) %>%
   mutate(like = case_when(.$liked == TRUE ~ "liked",
                           .$liked == FALSE ~ "disliked")) %>%
   select(-liked) %>%
   ggplot() +
   geom_density(aes(value, color = like)) +
   facet_wrap(~feature, scales = "free")



# model
model_rf <- randomForest(liked ~ .,
                         data = tracks_all[, -c(1, 12:14)],
                         importance=TRUE,  proximity=TRUE)

model_rf

#  OOB estimate of  error rate: 9.35%

plot(model_rf)
varImpPlot(model_rf)
round(importance(model_rf), 2)

plot(cmdscale(1 - model_rf$proximity, eig=TRUE)$points, col=tracks_all$liked)



# bierzemy listę testową
tracks_test <- getSpotifyUserPlaylist(user_test, playlist_test)

# pobieramy cechy utworów z listy testowej
tracks_test_features <- tracks_test$track_id %>% map_df(getSporifyTrackFeatures)


# przewidujemy czy się podoba czy nie
tracks_test_features$like <- predict(model_rf, newdata = tracks_test_features %>% select(-time_signature))

# łaczymy przewidywania z tytułami itp
tracks_predicted_to_like <- left_join(tracks_test_features %>% select(id, like),
                                      tracks_test %>% select(track_id, track, artist, album),
                                      by = c("id" = "track_id"))

View(tracks_predicted_to_like)

table(tracks_predicted_to_like$like)

# co powinno się podobać?
tracks_predicted_to_like %>%
   filter(like == TRUE) %>%
   arrange(artist, track) %>%
   select(-id, -like) %>%
   select(artist, track, album)

# co raczej się nie spodoba?
tracks_predicted_to_like %>%
   filter(like == FALSE) %>%
   arrange(artist, track) %>%
   select(-id, -like) %>%
   select(artist, track, album)

bi_artist <- tracks_predicted_to_like %>%
   count(like, artist, sort = TRUE) %>%
   ungroup() %>%
   spread(like, n) %>%
   mutate(s = `FALSE` + `TRUE`) %>%
   filter(!is.na(s)) %>%
   .$artist

# wykonawcy ktorych trochę się podoba, a trochę nie:
tracks_predicted_to_like %>%
   filter(artist %in% bi_artist) %>%
   arrange(artist, like)


tracks_all_long <- bind_rows(
   tracks_all %>%
      select(-track, -album, -artist) %>%
      gather(feature, value, -id, -liked) %>%
      mutate(like = case_when(.$liked == TRUE ~ "liked",
                              .$liked == FALSE ~ "disliked")) %>%
      select(-liked),
   tracks_test_features %>%
      select(-time_signature) %>%
      gather(feature, value, -id, -like) %>%
      mutate(like = case_when(.$like == TRUE ~ "pred like",
                              .$like == FALSE ~ "pred dislike"))
)


tracks_all_long %>%
   mutate(like = factor(like, levels = c("liked","pred like", "disliked", "pred dislike"))) %>%
   ggplot() +
   geom_violin(aes(like, value, fill = like)) +
   facet_wrap(~feature, scales = "free") +
   theme(legend.position = "bottom")



tracks_all_long %>%
   mutate(like = factor(like, levels = c("liked","pred like", "disliked", "pred dislike"))) %>%
   group_by(feature, like) %>%
   summarise(m_val = mean(value)) %>%
   ungroup() %>%
   ggplot() +
   geom_point(aes(like, m_val, color = like), size = 5) +
   facet_wrap(~feature, scales = "free") +
   theme(legend.position = "bottom")

