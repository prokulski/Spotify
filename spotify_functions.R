library(tidyverse)
library(httr)
library(stringr)
library(jsonlite)
library(RCurl)


# funkcja zwraca token
getSpotifyToken <- function() {
   res <- POST("https://accounts.spotify.com/api/token",
      body = list(grant_type = "client_credentials"),
      config = add_headers("Authorization" = paste0("Basic ", base64(paste(client_id, client_secret, sep = ":"))[[1]])),
      encode = "form"
   )

   if (res$status_code == 200) {
      return(content(res)$access_token)
   } else {
      return(NA)
   }
}



# funkcja pobiera listę utworów z biblioteki użytkownika
getMySporifyLibrary <- function() {
   library_tracks <- tibble()

   is_next <- TRUE

   query_url <- "https://api.spotify.com/v1/me/tracks?limit=50"

   while (is_next) {
      response <- GET(
         query_url,
         add_headers(
            "Authorization:" = paste("Bearer", spotify_token),
            "Content-Type:" = "application/json",
            "Accept:" = "application/json"
         )
      )

      if (response$status_code == 200) {
         response_json <- rawToChar(response$content) %>% iconv(from = "UTF-8", to = Sys.getenv("CHARSET"))
         query_results <- fromJSON(response_json, flatten = TRUE)

         library_tracks_tmp <- query_results$items %>%
            select(added_at,
               track_id = track.id,
               track = track.name,
               popularity = track.popularity,
               duration_s = track.duration_ms,
               explicit = track.explicit,
               album = track.album.name
            ) %>%
            mutate(duration_s = duration_s / 1000)


         library_tracks_tmp$artist <- map_chr(.f = function(x) {
            x$name %>%
               unlist() %>%
               str_c(collapse = ", ")
         }, query_results$items$track.artists)
         library_tracks_tmp$artist_id <- map_chr(.f = function(x) {
            x$id %>%
               unlist() %>%
               str_c(collapse = ", ")
         }, query_results$items$track.artists)

         library_tracks <- bind_rows(library_tracks, library_tracks_tmp)

         query_url <- query_results$`next`
         is_next <- !is.null(query_url)
      } else {
         return(response$status_code)
      }
   }

   return(library_tracks)
}



# Funkcja pobiera cechy audio utworu track_id
getSporifyTrackFeatures <- function(track_id) {
   response <- GET(
      paste0("https://api.spotify.com/v1/audio-features/", track_id),
      add_headers(
         "Authorization:" = paste("Bearer", spotify_token),
         "Content-Type:" = "application/json",
         "Accept:" = "application/json"
      )
   )

   if (response$status_code == 200) {
      response_json <- rawToChar(response$content) %>% iconv(from = "UTF-8", to = Sys.getenv("CHARSET"))
      query_results <- fromJSON(response_json, flatten = TRUE)

      track_features <- tibble(
         id = query_results$id,
         danceability = query_results$danceability,
         energy = query_results$energy,
         loudness = query_results$loudness,
         mode = query_results$mode,
         speechiness = query_results$speechiness,
         acousticness = query_results$acousticness,
         instrumentalness = query_results$instrumentalness,
         liveness = query_results$liveness,
         valence = query_results$valence,
         tempo = query_results$tempo,
         time_signature = query_results$time_signature
      )
   } else {
      return(tibble())
   }

   return(track_features)
}



# Funkcja pobiera playlistę playlist_id użytkownika user_id
getSpotifyUserPlaylist <- function(user_id, playlist_id) {
   playlist_tracks <- tibble()

   is_next <- TRUE

   query_url <- paste0("https://api.spotify.com/v1/users/", user_id, "/playlists/", playlist_id, "?limit=100")

   while (is_next) {
      response <- GET(
         query_url,
         add_headers(
            "Authorization:" = paste("Bearer", spotify_token),
            "Content-Type:" = "application/json",
            "Accept:" = "application/json"
         )
      )

      if (response$status_code == 200) {
         response_json <- rawToChar(response$content)
         query_results <- fromJSON(response_json, flatten = TRUE)

         if (!is.null(query_results$tracks)) {
            playlist_tracks_tmp <- query_results$tracks$items %>%
               select(added_at,
                  track_id = track.id,
                  track = track.name,
                  popularity = track.popularity,
                  duration_s = track.duration_ms,
                  explicit = track.explicit,
                  album_id = track.album.id,
                  album = track.album.name
               ) %>%
               mutate(duration_s = duration_s / 1000)


            playlist_tracks_tmp$artist <- map_chr(.f = function(x) {
               x$name %>%
                  unlist() %>%
                  str_c(collapse = ", ")
            }, query_results$tracks$items$track.artists)
            playlist_tracks_tmp$artist_id <- map_chr(.f = function(x) {
               x$id %>%
                  unlist() %>%
                  str_c(collapse = ", ")
            }, query_results$tracks$items$track.artists)

            query_url <- query_results$tracks$`next`
         } else {
            playlist_tracks_tmp <- query_results$items %>%
               select(added_at,
                  track_id = track.id,
                  track = track.name,
                  popularity = track.popularity,
                  duration_s = track.duration_ms,
                  explicit = track.explicit,
                  album_id = track.album.id,
                  album = track.album.name
               ) %>%
               mutate(duration_s = duration_s / 1000)


            playlist_tracks_tmp$artist <- map_chr(.f = function(x) {
               x$name %>%
                  unlist() %>%
                  str_c(collapse = ", ")
            }, query_results$items$track.artists)
            playlist_tracks_tmp$artist_id <- map_chr(.f = function(x) {
               x$id %>%
                  unlist() %>%
                  str_c(collapse = ", ")
            }, query_results$items$track.artists)

            query_url <- query_results$`next`
         }

         playlist_tracks <- bind_rows(playlist_tracks, playlist_tracks_tmp)

         is_next <- !is.null(query_url)
      } else {
         return(response$status_code)
      }
   }

   return(playlist_tracks)
}



# Get an Artist
# https://api.spotify.com/v1/artists/{id}
getSpotifyArtist <- function(artist_id) {
   response <- GET(
      paste0("https://api.spotify.com/v1/artists/", artist_id),
      add_headers(
         "Authorization:" = paste("Bearer", spotify_token),
         "Content-Type:" = "application/json",
         "Accept:" = "application/json"
      )
   )

   if (response$status_code == 200) {
      response_json <- rawToChar(response$content)
      query_results <- fromJSON(response_json, flatten = TRUE)

      if (length(query_results$genres) != 0) {
         df <- tibble(genres = query_results$genres)
      } else {
         df <- tibble(genres = NA)
      }

      df$artist_name <- query_results$name
      df$popularity <- query_results$popularity
      df$followers <- query_results$followers$total

      return(df)
   } else {
      return(tibble())
   }
}


# Get an Artist's Albums
# https://api.spotify.com/v1/artists/{id}/albums
# https://api.spotify.com/v1/artists/0k17h0D3J5VfsdmQ1iZtE9/albums


# Get an Artist's Top Tracks
# https://api.spotify.com/v1/artists/{id}/top-tracks
# https://api.spotify.com/v1/artists/0k17h0D3J5VfsdmQ1iZtE9/top-tracks?country=PL


# Get an Artist's Related Artists
# https://api.spotify.com/v1/artists/{id}/related-artists
# https://api.spotify.com/v1/artists/0k17h0D3J5VfsdmQ1iZtE9/related-artists


# Get an Album
# https://api.spotify.com/v1/albums/{id}
# https://api.spotify.com/v1/albums/0bCAjiUamIFqKJsekOYuRw


# Get an Album's Tracks
# https://api.spotify.com/v1/albums/{id}/tracks
# https://api.spotify.com/v1/albums/0bCAjiUamIFqKJsekOYuRw/tracks