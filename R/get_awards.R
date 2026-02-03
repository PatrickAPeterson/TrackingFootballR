#' Parse Award List data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#' @returns all players listed on College or NFL awards lists
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' #' @examples
#' \dontrun{
#' df <- get_awards(player_list)
#' }
#' @export
get_awards <- function(player_list) {
  purrr::imap_dfr(
    .x = player_list,
    .f = function(player, idx) {

      # first-level id
      this_id <- player[["id"]]
      if (is.null(this_id)) this_id <- idx

      # each of these is a character vector of years (or NULL)
      all_conf <- player[["allConference"]]
      all_amer <- player[["allAmerican"]]
      all_pro  <- player[["allPro"]]
      pro_bowl <- player[["proBowl"]]

      # list of award-type -> character vector of years
      awards_list <- list(
        allConference = all_conf,
        allAmerican   = all_amer,
        allPro        = all_pro,
        proBowl       = pro_bowl
      )

      # drop NULLs and non-character safely
      awards_list <- awards_list[
        vapply(awards_list, function(x) !is.null(x) && is.character(x) && length(x) > 0L,
               logical(1))
      ]

      # if no awards, skip this player
      if (length(awards_list) == 0L) return(NULL)

      # build long df: one row per (award type, year)
      dplyr::tibble(
        tf_awardName = rep(names(awards_list),
                           times = vapply(awards_list, length, integer(1))),
        tf_awardSeason = unlist(awards_list, use.names = FALSE),
        tf_playerId   = this_id
      ) %>%
        dplyr::select(tf_playerId, tf_awardSeason, tf_awardName)
    }
  )
}
