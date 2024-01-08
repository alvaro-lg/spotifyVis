date_to_season <- function(year, month, day) {
  is_leap_year <- (year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0  
  if (is_leap_year) {
    days_in_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else {
    days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  day_of_year = sum(days_in_month[1:month-1]) + day
  # 21.03 -> 80/81
  # 22.06 -> 173/174
  # 23.09 -> 266/267
  # 22.12 -> 356/357
  if (is_leap_year) {
    day_of_year <- day_of_year - 1
  }
  if (day_of_year < 80 | day_of_year >= 356)
    return("winter")
  if (day_of_year >= 80 & day_of_year < 173)
    return("spring")
  if (day_of_year >= 173 & day_of_year < 266)
    return("summer")
  if (day_of_year >= 266 & day_of_year < 356)
    return("autumn")
}

date_to_season_row <- function(x) {
  return(date_to_season(strtoi(x[["released_year"]]), strtoi(x[["released_month"]]), strtoi(x[["released_day"]])))
}
