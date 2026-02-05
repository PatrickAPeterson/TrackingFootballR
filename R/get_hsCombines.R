#' Parse hsCombines data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#'
#' @returns all player hsCombines details as a list object
#' @export
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import stringr


get_hsCombines <- function(player_list) {

  purrr::map_dfr(player_list, "hsCombines") %>%
    tidyr::unnest("combine", names_sep = "_", keep_empty = TRUE) %>%
    dplyr::select(-one_of("combine")) %>%
    dplyr::group_by_all() %>%
    dplyr::distinct(playerId, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(playerId) %>%
    dplyr::mutate(

      # 1. If there is no combine_code, then create positions_all string as NA
      positions_all = if ("combine_code" %in% names(.)) {
        stringr::str_replace(toString(unique(unlist(strsplit(combine_code, ",\\s*")))), "NA, |, NA", "")}
      else {NA_character_},

      # 2. If there is no combine_code, then create combine_perc as NA
      combine_perc = if ("combine_combine" %in% names(.)) {combine_combine}
      else {NA_character_},

      # 3. If there is no combine_code, then create combine_code as NA
      combine_code = if ("combine_code" %in% names(.)) {combine_code}
      else {NA_character_},

      # 4. If there is no combine_isPrimary, then create combine_isPrimary as NA
      combine_isPrimary = if ("combine_isPrimary" %in% names(.)) {combine_isPrimary}
      else {NA_character_},

      createdAt  = as.character(createdAt),
      updatedAt  = as.character(updatedAt),
      combineDate = as.character(combineDate)) %>%
    dplyr::select(-one_of("combine_combine")) %>% # Remove combine_combine if persists
    dplyr::mutate(across(everything(), ~ ifelse(trimws(.) == "", NA, .))) %>%
    dplyr::rename_with(~ paste0("tf_hsCombines_", .), .cols = -c(playerId)) %>%
    dplyr::rename("tf_playerId" = playerId)
}
