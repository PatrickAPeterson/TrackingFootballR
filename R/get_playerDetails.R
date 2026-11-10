#' Parse playerDetails data from Tracking Football Player List
#'
#' @param player_list all player details list JSON-decoded object obtained through one of the get_all_xyz_player_details functions
#'
#' @returns all player playerDetails details as a list object
#' @export
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr


get_playerDetails <- function(player_list) {

  player_detail_fields <- c("id", "pffId", "247Key", "ncaaId", "ryzerId",
                 "ssaId", "uniqueId", "firstName", "lastName", "slug",
                 "photoUrl", "dob", "locationFlag", "transferStatus", "transferYear",
                 "gpa", "sat", "act", "compStar", "247Comp",
                 "247Star", "committedToTeamId", "committedYear", "hasEverCommitted", "about",
                 "isSpeedRecruit", "isPowerRecruit", "combineHasFreshmanData", "paiHasFreshmanData", "hasHighSchool",
                 "hasCollegeTeam", "hasNFLTeam", "cellPhone", "email", "address",
                 "cityStateZip", "parentContact", "parentNames", "headCoach", "headCoachContact",
                 "createdAt", "updatedAt", "on3Id", "collegeGSIS", "nflGSIS", "ryzerUrl",
                 "ppi", "ppiDate", "ppiDiv1Percentile", "bestPPI", "bestPPIYear")

  purrr::map_dfr(player_list,
          ~ {
            # Subsetting by name can return a bare NULL for a field that is
            # JSON null. If every player in a batch has NULL for the same
            # field, map_dfr/bind_rows has no length-1 value anywhere to
            # anchor that column, so it silently vanishes instead of coming
            # back as NA. Normalise NULL -> NA first so the column shape
            # never depends on which fields happen to be populated.
            vals <- .x[player_detail_fields]
            vals[vapply(vals, is.null, logical(1))] <- NA
            vals
          }) %>%
    dplyr::rename(playerId = "id") %>%
    dplyr::distinct(playerId, .keep_all = TRUE) %>%
    dplyr::rename_with(~ paste0("tf_playerDetails_", .), .cols = -c(playerId)) %>%
    dplyr::rename("tf_playerId" = playerId) %>%
    (\(x) if ("tf_playerDetails_pffId" %in% names(x)) dplyr::mutate(x,
                                                    tf_playerDetails_pffId = dplyr::case_when(tf_playerDetails_pffId == "0" ~ NA_character_,
                                                                                              TRUE ~ tf_playerDetails_pffId)) else x)() %>%
    # Replace all occurrences of null with NA across all columns
    mutate(across(everything(), ~ replace(., . == "null", NA)))


}
