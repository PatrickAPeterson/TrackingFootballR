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
    dplyr::mutate(
      armPerc = purrr::map(armPerc, function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        if (is.data.frame(x)) return(x)
        if (is.list(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
        return(NULL)
      }),
      handPerc = purrr::map(handPerc, function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        if (is.data.frame(x)) return(x)
        if (is.list(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
        return(NULL)
      }),
      heightPerc = purrr::map(heightPerc, function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        if (is.data.frame(x)) return(x)
        if (is.list(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
        return(NULL)
      }),
      weightPerc = purrr::map(weightPerc, function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        if (is.data.frame(x)) return(x)
        if (is.list(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
        return(NULL)
      }),
      wingspanPerc = purrr::map(wingspanPerc, function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        if (is.data.frame(x)) return(x)
        if (is.list(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
        return(NULL)
      })
    ) %>%
    tidyr::unnest("armPerc", names_sep = "_", keep_empty = TRUE) %>%
    tidyr::unnest("handPerc", names_sep = "_", keep_empty = TRUE) %>%
    tidyr::unnest("heightPerc", names_sep = "_", keep_empty = TRUE) %>%
    tidyr::unnest("weightPerc", names_sep = "_", keep_empty = TRUE) %>%
    tidyr::unnest("wingspanPerc", names_sep = "_", keep_empty = TRUE) %>%
    dplyr::rowwise() %>%
    # Fixing the issue of unnesting and creating unwanted duplicates using the "_code" columns
    dplyr::filter({
      vals   <- dplyr::c_across(dplyr::ends_with("_code"))
      non_na <- vals[!is.na(vals)]
      length(unique(non_na)) <= 1
    }) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::group_by(playerId) %>%
    dplyr::mutate(

      # 1. If there is no combine_code, then create positions_all string as NA
      positions_all = if ("combine_code" %in% names(.)) {
        stringr::str_replace(toString(unique(unlist(strsplit(combine_code, ",\\s*")))), "NA, |, NA", "")}
      else {NA_character_},

      # 2. If there is no combine_combine, then create combine_perc as NA
      combine_perc = if ("combine_combine" %in% names(.)) {combine_combine}
      else {NA_character_},

      # 3. If there is no combine_code, then create combine_code as NA
      combine_code = if ("combine_code" %in% names(.)) {as.character(combine_code)}
      else {NA_character_},

      # 4. If there is no combine_isPrimary, then create combine_isPrimary as NA
      combine_isPrimary = if ("combine_isPrimary" %in% names(.)) {combine_isPrimary}
      else {NA_character_},

      # 5. If there is no armPerc_code, then create arm_code as NA
      arm_code = if ("armPerc_code" %in% names(.)) {as.character(armPerc_code)}
      else {NA_character_},

      # 6. If there is no armPerc_percentile, then create arm_perc as NA
      arm_perc = if ("armPerc_percentile" %in% names(.)) {armPerc_percentile}
      else {NA_character_},

      # 7. If there is no handPerc_code, then create hand_code as NA
      hand_code = if ("handPerc_code" %in% names(.)) {as.character(handPerc_code)}
      else {NA_character_},

      # 8. If there is no handPerc_percentile, then create hand_perc as NA
      hand_perc = if ("handPerc_percentile" %in% names(.)) {handPerc_percentile}
      else {NA_character_},

      # 9. If there is no heightPerc_code, then create height_code as NA
      height_code = if ("heightPerc_code" %in% names(.)) {as.character(heightPerc_code)}
      else {NA_character_},

      # 10. If there is no heightPerc_percentile, then create height_perc as NA
      height_perc = if ("heightPerc_percentile" %in% names(.)) {heightPerc_percentile}
      else {NA_character_},

      # 11. If there is no weightPerc_code, then create weight_code as NA
      weight_code = if ("weightPerc_code" %in% names(.)) {as.character(weightPerc_code)}
      else {NA_character_},

      # 12. If there is no weightPerc_percentile, then create weight_perc as NA
      weight_perc = if ("weightPerc_percentile" %in% names(.)) {weightPerc_percentile}
      else {NA_character_},

      # 13. If there is no wingspanPerc_code, then create wingspan_code as NA
      wingspan_code = if ("wingspanPerc_code" %in% names(.)) {as.character(wingspanPerc_code)}
      else {NA_character_},

      # 14. If there is no wingspanPerc_percentile, then create wingspan_perc as NA
      wingspan_perc = if ("wingspanPerc_percentile" %in% names(.)) {wingspanPerc_percentile}
      else {NA_character_},

      createdAt   = as.character(createdAt),
      updatedAt   = as.character(updatedAt),
      combineDate = as.character(combineDate)) %>%
    dplyr::select(-any_of(c("combine_combine", "armPerc_code",
                            "armPerc_percentile", "handPerc_code",
                            "handPerc_percentile", "heightPerc_code",
                            "heightPerc_percentile", "weightPerc_code",
                            "weightPerc_percentile", "wingspanPerc_code",
                            "wingspanPerc_percentile", "armPerc", "handPerc",
                            "heightPerc", "weightPerc",  "wingspanPerc"))) %>%
    dplyr::mutate(across(everything(), ~ ifelse(trimws(.) == "", NA, .))) %>%
    dplyr::rename_with(~ paste0("tf_hsCombines_", .), .cols = -c(playerId)) %>%
    dplyr::rename("tf_playerId" = playerId) %>%
    mutate(across(contains("code"), as.character))
}
