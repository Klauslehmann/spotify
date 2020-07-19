library(spotifyr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(httr)


id <- "d42df380447347c2a5946a38b1e24132"
secret <- "4acc065f735642a99ff653efd065d984"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()


my_plists <- get_user_playlists( user_id = "klaus_lehmann" )

beatles <- get_artist_audio_features('the beatles')
joy <- get_artist_audio_features('metallica')


chile <- spotifyr::get_artist_top_tracks("2ye2Wgw4gimLv2eAKyk1NB", market = "cl")



d <- map(c(1500), get_genre_artists, genre = "rock", market = "cl", limit = 50) %>% 
  reduce(bind_rows) 

d2 <- d %>% 
  distinct()

album_info <-  map(d2$id[1:2], get_artist_audio_features, include_groups = "album") 

album_info[[1]] %>% View()
album_info[[2]] %>% View()

View(d[[3]])

mercado1 <- get_genre_artists(genre = "rock", market = "cl", limit = 50, offset = 0)

mercado2 <- get_genre_artists(genre = "rock", market = "cl", limit = 50, offset = 1)

mercado3 <- get_genre_artists(genre = "rock", market = "cl", limit = 50, offset = 10000)


argentina <- get_playlist_tracks('1hsAb7SpmIeoOSHh0VLqD2' )

my_plists2 <- my_plists %>%
  filter(playlist_name %in% c('Taiwan Top 50', 'France Top 50', 'Bolivia Top 50', 'U.S. Top 50'))

tracks <- get_playlist_tracks(my_plists2)
features <- get_track_audio_features(tracks)


chile <- spotifyr::search_spotify("Chile Top 50", type = "playlist", limit = 50 )
chile <- spotifyr::search_spotify("Chile", limit = 50 )


lista <-  get_playlist_tracks(chile$id[1])
cancion <- get_track_audio_features(lista$track.id[1])

z <- get_featured_playlists(chile$id[1])
get_artist(lista$track.id[1])
library(lubridate)

get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at)