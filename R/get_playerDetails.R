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

  purrr::map_dfr(player_list,
          ~ .x[c("id", "pffId", "247Key", "ncaaId", "ryzerId",
                 "ssaId", "uniqueId", "firstName", "lastName", "slug",
                 "photoUrl", "dob", "locationFlag", "transferStatus", "transferYear",
                 "gpa", "sat", "act", "compStar", "247Comp",
                 "247Star", "committedToTeamId", "committedYear", "hasEverCommitted", "about",
                 "isSpeedRecruit", "isPowerRecruit", "combineHasFreshmanData", "paiHasFreshmanData", "hasHighSchool",
                 "hasCollegeTeam", "hasNFLTeam", "cellPhone", "email", "address",
                 "cityStateZip", "parentContact", "parentNames", "headCoach", "headCoachContact",
                 "createdAt", "updatedAt", "on3Id", "collegeGSIS", "nflGSIS", "ryzerUrl")]) %>%
    dplyr::rename(playerId = "id") %>%
    dplyr::distinct(playerId, .keep_all = TRUE) %>%
    dplyr::rename_with(~ paste0("tf_playerDetails_", .), .cols = -c(playerId)) %>%
    dplyr::rename("tf_playerId" = playerId)

}
