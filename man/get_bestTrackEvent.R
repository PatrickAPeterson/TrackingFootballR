#' Parse bestTrackEvent data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#'
#' @returns all player bestTrackEvent details as a list object
#' @export
#'
#' @import httr
#' @import jsonlite
#' @import dplyr


get_bestTrackEvent <- function(player_list) {

  imap_dfr(
    player_list,
    function(player, idx) {

      # IF bestTrackEvent does not exist (is NULL) OR (i.e. '||')
      # IF bestTrackEvent exists and is length == 0 (i.e. the '||' operator here)
      # If the NULL is returned here then no playerId is contributed in the function
      if (is.null(player$bestTrackEvent) || length(player$bestTrackEvent) == 0) {
        return(NULL)
      }

      # playerId from first level; just a cleaner If Else statement to take playerId if its listed or just id if not
      # The first level playerId is just listed as id but in other tables (not bestTrackEvent)
      pid <- player$playerId %||% player$id

      # bestTrackEvent map the 2nd level list, do not change the 'ev' or 'ev-name' conventions
      # the date neets to be fixed, not sure how
      imap_dfr(
        player$bestTrackEvent,
        function(ev, ev_name) {
          tibble(
            tf_playerId   = pid,
            tf_bestTrackEvent_event      = ev_name,
            tf_bestTrackEvent_date       = as.character(ev$date),
            tf_bestTrackEvent_meet       = as.character(ev$meet),
            tf_bestTrackEvent_percentile = as.character(ev$percentile),
            tf_bestTrackEvent_performance = as.character(ev$performance)
          )
        }
      )
    }
  )

}
