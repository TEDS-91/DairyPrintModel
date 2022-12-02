#' Vector to determine the manure storage emptying days.
#'
#' @param empty_time Season of manure storage emptying (i.e., fall, spring).
#'
#' @return Binary vector where 1 means days of emptying.
#'
empty_day <- function(empty_time) {

  yday <- seq(1, 730, 1)

  empty_day <- vector(length = length(yday))

  if(empty_time == "Fall") {

    "fall" <- c(200, 565)

    for ( i in 1:length(yday)) {

      empty_day[i] <- ifelse(yday[i] == fall[1] | yday[i] == fall[2], 1, 0)
    }

  } else if (empty_time == "Spring") {

    "spring" <- c(120, 485)

    for ( i in 1:length(yday)) {

      empty_day[i] <- ifelse(yday[i] == spring[1] | yday[i] == spring[2], 1, 0)
    }

  } else {

    fall_and_spring <- c(120, 200, 485, 565)

    for ( i in 1:length(yday)) {

      empty_day[i] <- ifelse(yday[i] == fall_and_spring[1] | yday[i] == fall_and_spring[2] | yday[i] == fall_and_spring[3] | yday[i] == fall_and_spring[4], 1, 0)
    }
  }

  return(empty_day)

}
