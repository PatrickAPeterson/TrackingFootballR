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
#' @import tibble


# Pulls one event field, returning NA rather than failing when the event is not a
# list or the field is absent. Keeps every column length-1 so tibble() can bind.
tf_event_field <- function(ev, field) {
  if (!is.list(ev)) return(NA_character_)
  val <- ev[[field]]
  if (is.null(val) || length(val) == 0) return(NA_character_)
  as.character(val)[1]
}

get_bestTrackEvent <- function(player_list) {

  purrr::imap_dfr(
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

      events <- player$bestTrackEvent
      if (!is.list(events)) events <- as.list(events)

      # Normally a named map of event objects ("event_100m" = list(date = , meet = , ...)).
      # Some players come back with one event's fields spliced into the top level
      # instead of nested under an event name, either on their own or alongside
      # properly nested events. Re-wrap those loose scalars as a single unnamed
      # event so they aren't iterated as if each field were its own event.
      is_event <- vapply(events, is.list, logical(1))
      loose <- events[!is_event]
      events <- events[is_event]
      if (length(loose) > 0) {
        events <- c(events, stats::setNames(list(loose), NA_character_))
      }

      # bestTrackEvent map the 2nd level list, do not change the 'ev' or 'ev-name' conventions
      # the date neets to be fixed, not sure how
      purrr::imap_dfr(
        events,
        function(ev, ev_name) {
          tibble::tibble(
            tf_playerId   = pid,
            tf_bestTrackEvent_event      = if (is.na(ev_name)) NA_character_ else as.character(ev_name),
            tf_bestTrackEvent_date       = tf_event_field(ev, "date"),
            tf_bestTrackEvent_meet       = tf_event_field(ev, "meet"),
            tf_bestTrackEvent_percentile = tf_event_field(ev, "percentile"),
            tf_bestTrackEvent_performance = tf_event_field(ev, "performance")
          )
        }
      )
    }
  )

}
