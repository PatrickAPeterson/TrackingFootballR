#' Get Player List
#'
#' @description Retrieve Tracking Football player details in a structured list since a given date.
#' @param api_key A character element containing an API key obtained from Tracking Football
#' @param base_url Tracking Football APi base URL: "https://tfb2-api.trackingfootball.com/api/v1/api/"
#' @param page_size Number of players per page
#' @param verbose Print Progress messages
#' @param sleep_sec Sleep Timer for API call
#' @param updated_since Date parameter for oldest date to query API in format "YYYY-MM-DD"
#' @param player_type Character, call in type of player. Options: "high-school", "college", "nfl". Default to "college"
#' @return A large list with nested dataframes for individual player measurables and performances.
#' @examples
#' \dontrun{
#' ncaa_historical <- get_player_list(api_key, updated_since = "2020-01-01", player_type = "college")
#' }
#' @export get_player_list get_all_player_details
#' @aliases get_all_player_details
#'
#' @import httr
#' @import jsonlite
#' @import dplyr


get_player_list <- function(api_key,
                                   base_url = "https://tfb2-api.trackingfootball.com/api/v1/api/",
                                   page_size = 500,     # Number of players per page
                                   verbose = TRUE,     # Progress messages
                                   sleep_sec = 0.005,
                                   updated_since = as.character(Sys.Date() - 1), # Default pull yesterday's data
                                   player_type = "college") {    # Default pull NCAA data

  all_player_details <- list()     # List of all player details

  # 1. Get total count of updated players using the count endpoint
  count_resp <- httr::GET(
    url = paste0(base_url, "count-players-updated-since"),     # Count players updated since endpoint
    query = list(
      date = updated_since,     # Players updated since this date WRITE INTO THE FUNCTION
      playerType = player_type     # Filter for player types
    ),
    httr::add_headers("api-key" = api_key)     # Attach api key as a header
  )
  httr::stop_for_status(count_resp)     # Stop and print error is API call fails
  count_json <- jsonlite::fromJSON(
    httr::content(count_resp, as = "text", encoding = "UTF-8"))
  total_players <- count_json$count # correctly extract 'count' field!
  total_pages <- ceiling(total_players / page_size)
  if (verbose) message("Total players: ", total_players, ", total pages: ", total_pages)

  # 2. Proceed with paging, Loop through each page of player slugs
  for (page in seq_len(total_pages)) { # Loop through X number of pages
    resp <- httr::GET(
      url = paste0(base_url, "players-updated-since"),     # API endpoint for player list
      query = list(
        date = updated_since,
        playerType = player_type, # Filter for player types
        page = page,
        pageSize = page_size
      ),
      httr::add_headers("api-key" = api_key)
    )
    httr::stop_for_status(resp)     # Stop if a request fails
    player_page <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8")) # Parse JSON; returns a huge fucking list or data.frame

    if (length(player_page) == 0) break # If no data/page is returned, break loop early

    # Extract player slugs for this page
    slugs <- if (is.data.frame(player_page)) player_page$slug
    else if (is.list(player_page)) vapply(player_page, function(x) x$slug, FUN.VALUE = character(1))
    else character(0)    # Fallback to empty

    # 3. For each slug, retrieve full player JSON using player-details endpoint. If there are errors increase timeout and sleep system for 10s. Bad idea? Probably I don't fucking care.
    for (slug in slugs) {
      detail_resp <- NULL
      for (attempt in 1:3) {
        detail_resp <- try(
          httr::GET(
            url = paste0(base_url, "player-details"),
            query = list(type = "slug", value = slug),
            httr::add_headers("api-key" = api_key),
            httr::timeout(60) # Increase timeout to 60s
        ),
        silent = TRUE)
        if (!inherits(detail_resp, "try-error")) break
        Sys.sleep(10) # Wait 10 seconds before next try
      }
      if (is.null(detail_resp) || inherits(detail_resp, "try-error")) next # Skip if still failed

      if (httr::status_code(detail_resp) == 200) {
        detail_json <- httr::content(detail_resp, as = "text", encoding = "UTF-8")
        detail <- jsonlite::fromJSON(detail_json, flatten = TRUE)
        all_player_details[[length(all_player_details)+1]] <- detail
        if (verbose) message("Fetched details for: ", slug)
      }
      Sys.sleep(sleep_sec)
    }
  }
  # 4. Return all player details as a list of JSON-decoded R objects
  return(all_player_details)
}
