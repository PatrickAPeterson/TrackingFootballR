#' Parse Multi Sport data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#' @returns High school sports played by all players listed
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' df <- get_multiSport(player_list)
#' }
#' @export
get_multiSport <- function(player_list) {
  imap_dfr(
    .x = player_list,
    .f = function(player, idx) {

      ms <- player[["multiSport"]]   # expected to be character vector

      # skip if missing or not character
      if (is.null(ms) || !is.character(ms)) return(NULL)

      # if for some reason id is missing, force NA
      tf_playerId <- player[["id"]]
      if (is.null(tf_playerId)) tf_playerId <- NA_character_

      # create one row per sport
      tibble(
        tf_playerId,
        tf_multiSport = ms
      )
    }
  )
}
