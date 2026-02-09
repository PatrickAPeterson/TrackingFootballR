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
#' @import purrr
#' @import tidyr


get_highSchools <- function(player_list) {

  purrr::map_dfr(player_list, "highSchools") %>%
    dplyr::filter(isPrimary == TRUE) %>%
    dplyr::group_by_all() %>%
    dplyr::distinct(playerId, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    tidyr::unnest("positions", names_sep = "_") %>%
    dplyr::group_by(playerId) %>%
    dplyr::mutate(positions_all = paste(positions_code, collapse = ", "))  %>%
    dplyr::filter(positions_isPrimary == TRUE) %>%
    dplyr::select(-c("id")) %>%
    dplyr::distinct(playerId, .keep_all = TRUE) %>%
    tidyr::unnest("heightPerc", names_sep = "_") %>%
    tidyr::unnest("weightPerc", names_sep = "_") %>%
    dplyr::filter(heightPerc_code == positions_code & weightPerc_code == positions_code) %>%
    dplyr::select(-c(isPrimary, positions_id, positions_playerId, positions_createdAt,
              positions_updatedAt, weightPerc_code, heightPerc_code)) %>%
    dplyr::rename_with(~ paste0("tf_highSchools_", .), .cols = -c(playerId)) %>%
    dplyr::rename("tf_playerId" = playerId)

}
