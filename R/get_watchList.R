#' Parse College Watch List data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#' @returns all players listed on College all-star game watch lists
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' df <- get_watchList(player_list)
#' }
#' @export
get_watchList <- function(player_list) {
  purrr::imap_dfr(
    .x = player_list,
    .f = function(player, idx) {

      # extract nested watchList
      wl <- player[["watchList"]]

      # skip if missing or not a data.frame
      if (is.null(wl) || !is.data.frame(wl)) return(NULL)

      # get the player id stored at first level
      tf_playerId <- player[["id"]]

      # if for some reason id is missing, force NA
      if (is.null(tf_playerId )) tf_playerId  <- NA_character_

      wl %>%
        dplyr::mutate(
          id = tf_playerId        # keep first-level id in final df
        ) %>%
        dplyr::rename(tf_playerId = id,
                      tf_watchList = name,
                      tf_watchListSeason = year)  # sloppy, but rename it again idk
    }
  )
}
