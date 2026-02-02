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


get_hsCombines <- function(player_list) {

  map_dfr(player_list, "hsCombines") %>%
    select(-one_of("combine")) %>%
    rename_with(~ paste0("tf_hsCombines_", .), .cols = -c(playerId)) %>%
    rename("tf_playerId" = playerId) %>%
    mutate(tf_hsCombines_isHandTime = ifelse(grepl("hand timed", tf_hsCombines_city, ignore.case = TRUE), 1, 0),
           tf_hsCombines_isLaserTime = ifelse(grepl("laser", tf_hsCombines_city, ignore.case = TRUE), 1, 0),
           .after = 3) %>%
    group_by(tf_playerId) %>%
    mutate(combineDate_rufb = as.Date(tf_hsCombines_combineDate),
           daysSinceCombineDate_rufb = as.numeric(as.Date(today()) - combineDate_rufb),
           mostRecentCombine_rufb = min(daysSinceCombineDate_rufb)) %>%
    summarise(

      tf_hsCombines_40mDash_recent = if(any(tf_hsCombines_isLaserTime == 1 & !is.na(tf_hsCombines_40mDash))) {
        tf_hsCombines_40mDash[which.min(ifelse(tf_hsCombines_isLaserTime == 1 & !is.na(tf_hsCombines_40mDash), daysSinceCombineDate_rufb, NA))]
      } else if(all(is.na(tf_hsCombines_40mDash))) {
        NA_real_
      } else {
        min(tf_hsCombines_40mDash, na.rm = TRUE)
      },


      tf_hsCombines_10Split_recent = if(any(tf_hsCombines_isLaserTime == 1 & !is.na(tf_hsCombines_10Split))) {
        tf_hsCombines_10split[which.min(ifelse(tf_hsCombines_isLaserTime == 1 & !is.na(tf_hsCombines_10split), daysSinceCombineDate_rufb, NA))]
      } else if(all(is.na(tf_hsCombines_10Split))) {
        NA_real_
      } else {
        min(tf_hsCombines_10Split, na.rm = TRUE)
      },


      tf_hsCombines_height_recent =
        if(all(is.na(tf_hsCombines_height))) {
          NA_real_
        } else {
          tf_hsCombines_height[which.min(daysSinceCombineDate_rufb)]
        },


      tf_hsCombines_weight_recent =
        if(all(is.na(tf_hsCombines_weight))) {
          NA_real_
        } else {
          tf_hsCombines_weight[which.min(daysSinceCombineDate_rufb)]
        },

      tf_hsCombines_wingspan_recent =
        if(all(is.na(tf_hsCombines_wingspan))) {
          NA_real_
        } else {
          tf_hsCombines_wingspan[which.min(daysSinceCombineDate_rufb)]
        },

      tf_hsCombines_arm_recent =
        if(all(is.na(tf_hsCombines_arm))) {
          NA_real_
        } else {
          tf_hsCombines_arm[which.min(daysSinceCombineDate_rufb)]
        },

      tf_hsCombines_hand_recent =
        if(all(is.na(tf_hsCombines_hand))) {
          NA_real_
        } else {
          tf_hsCombines_hand[which.min(daysSinceCombineDate_rufb)]
        },

      tf_hsCombines_3Cone_recent =
        if(all(is.na(tf_hsCombines_3Cone))) {
          NA_real_
        } else {
          tf_hsCombines_3Cone[which.min(daysSinceCombineDate_rufb)]
        },

      tf_hsCombines_shuttle_recent =
        if(all(is.na(tf_hsCombines_shuttle))) {
          NA_real_
        } else {
          tf_hsCombines_shuttle[which.min(daysSinceCombineDate_rufb)]
        },


      tf_hsCombines_verticalJump_recent =
        if(all(is.na(tf_hsCombines_verticalJump))) {
          NA_real_
        } else {
          tf_hsCombines_verticalJump[which.min(daysSinceCombineDate_rufb)]
        },

      tf_hsCombines_broadJump_recent =
        if(all(is.na(tf_hsCombines_broadJump))) {
          NA_real_
        } else {
          tf_hsCombines_broadJump[which.min(daysSinceCombineDate_rufb)]
        },

      .groups = "drop"
    ) %>%
    mutate(across(everything(), na_if, 0))
}
