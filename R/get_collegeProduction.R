#' Parse College Production data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#' @returns all player collegeProduction details as a list object
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' df <- get_collegeProduction(player_list)
#' }
#' @export

get_collegeProduction <- function(player_list) {
  purrr::map_dfr(
    # List of row IDs
    .x = seq_along(player_list),

    # Function to run through row IDs
    .f = function(i) {
      x <- player_list[[i]][["collegeProductionData"]]
      # collegeProductionData can be missing or NOT a dataframe, skip and return NULL
      if (is.null(x) || !is.data.frame(x)) return(NULL)

      # Dress up resulting dataframe
      x %>%
        dplyr::mutate(
          player_index = i
        ) %>%
        dplyr::select(-"id") %>%
        dplyr::rename_with(~ paste0("tf_collegeProductionData_", .), .cols = -c(playerId)) %>%
        dplyr::rename("tf_playerId" = playerId)
    }
  )
}
