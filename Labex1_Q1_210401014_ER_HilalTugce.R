
library(httr)

spotify_token <- function() {
  token_url <- "https://accounts.spotify.com/api/token"
  client_id <- (Sys.getenv("SPOTIFY_ID"))
  client_secret <- (Sys.getenv("SPOTIFY_SECRET"))
  
  body <- list(
    grant_type = "client_credentials",
    client_id = client_id,
    client_secret = client_secret
  )
  
  response <- httr::POST(
    url = token_url,
    body = body,
    encode = "form",
    add_headers("Content-Type" = "application/x-www-form-urlencoded")
  )
  
  status_code <- status_code(response)
  token <- content(response)$access_token
  bearer_token <- paste("Bearer",token)
  
  result <- list(
    status_code = status_code,
    token = bearer_token
  )
  
  return(result)
}

#Özellik 2:

spotify_search_artist <- function(artist_name) {
  if (!is.character(artist_name)) stop("Artist name must be character type.")
  
  token <- spotify_token()
  search_url <- paste0(
    "https://api.spotify.com/v1/search?q=", URLencode(artist_name),
    "&type=artist&limit=", 5
  )
  
  response <- httr::GET(
    url = search_url,
    add_headers("Authorization" = token[[2]])
  )
  
  search_result <- httr::content(response, type = "application/json")
  status_code <- status_code(response)
  
  # İlk num_results sanatçısını al
  artists <- search_result$artists$items[seq_len(5)]
  
  search_results <- data.frame(
    artist = sapply(artists, function(x) x$name),
    id = sapply(artists, function(x) x$id)
  )
  
  result <- list(
    status_code = status_code,
    search_results = search_results
  )
  
  return(result)
}

print(spotify_search_artist("The Doors"))