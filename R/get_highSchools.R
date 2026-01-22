#' Parse highSchools data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#'
#' @returns all player highSchools details as a list object
#' @export
#'
#' @import httr
#' @import jsonlite
#' @import dplyr


get_highSchools <- function(player_list) {

  map_dfr(NEW_hs_player_details, "highSchools") %>%
    filter(isPrimary == TRUE) %>%
    group_by_all() %>%
    distinct(playerId, .keep_all = TRUE) %>%
    ungroup() %>%
    unnest("positions", names_sep = "_") %>%
    group_by(playerId) %>%
    mutate(positions_all = paste(positions_code, collapse = ", "))  %>%
    filter(positions_isPrimary == TRUE) %>%
    select(-c("id")) %>%
    distinct(playerId, .keep_all = TRUE) %>%
    unnest("heightPerc", names_sep = "_") %>%
    unnest("weightPerc", names_sep = "_") %>%
    filter(heightPerc_code == positions_code & weightPerc_code == positions_code) %>%
    select(-c(isPrimary, positions_id, positions_playerId, positions_createdAt,
              positions_updatedAt, weightPerc_code, heightPerc_code)) %>%
    rename_with(~ paste0("tf_highSchools_", .), .cols = -c(playerId)) %>%
    rename("tf_playerId" = playerId)

}
