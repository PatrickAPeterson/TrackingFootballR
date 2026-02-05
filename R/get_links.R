#' Parse links data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#'
#' @returns all player links details as a list object
#' @export
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr


get_links <- function(player_list) {

  purrr::imap_dfr(
    player_list,
    function(player, idx) {

      # IF bestTrackEvent does not exist (is NULL) OR (i.e. '||')
      # IF bestTrackEvent exists and is length == 0 (i.e. the '||' operator here)
      # If the NULL is returned here then no playerId is contributed in the function
      if (is.null(player$links) || length(player$links) == 0) {
        return(NULL)
      }

      # playerId from first level; just a cleaner If Else statement to take playerId if its listed or just id if not
      # The first level playerId is just listed as id but in other tables (not bestTrackEvent)
      pid <- player$playerId %||% player$id

      # bestTrackEvent map the 2nd level list, do not change the 'ev' or 'ev-name' conventions
      # the date neets to be fixed, not sure how
      df <- data.frame(tf_playerId = pid,
                       tf_links_key = player$links$key,
                       tf_links_value = player$links$value
      )
    }
  )

}
