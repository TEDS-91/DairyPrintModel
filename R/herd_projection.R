#' her Projection
#'
#' @param time_first_calv Time at first calving.
#' @param heifer_calf_born Heifer calf born.
#' @param stillbirth_rate Stillbirth rate.
#' @param calves_heifers_cul Culling rate for heifers and calves.
#' @param cow_rep_rate Culling rate of adult cows.
#' @param cow_calving_int Calving interval.
#' @param n_adult_cows Number of adult cows.
#' @param months_to_project Months to project.
#'
#' @return A list.
#' @export
#'
herd_projection <- function(time_first_calv,
                            heifer_calf_born,
                            stillbirth_rate,
                            calves_heifers_cul,
                            cow_rep_rate,
                            cow_calving_int,
                            n_adult_cows,
                            months_to_project){

  # support variables

  #month_preg_cow <- cow_calving_int - 8

  #month_prg_heifer <- time_first_calv - 8

  cull_rate_month_heifer <- (calves_heifers_cul / time_first_calv) / 100

  cull_rate_month_cows <- (cow_rep_rate / 12) / 100

  herdMatrix <- herd_stabilization(time_first_calv,
                                   heifer_calf_born,
                                   stillbirth_rate,
                                   calves_heifers_cul,
                                   cow_rep_rate,
                                   cow_calving_int,
                                   n_adult_cows)

  replac_target <- herdMatrix[(time_first_calv + 1)]

  replac_target

  required_replac <- vector()

  for (i in 1:(time_first_calv - 1)){

    required_replac[1] <- replac_target / (1 - (calves_heifers_cul / 100) / time_first_calv)

    required_replac[i + 1] <- required_replac[i] / (1 - (calves_heifers_cul / 100) / time_first_calv)

  }

  required_replac <- sort(required_replac, decreasing = TRUE)

  required_replac

  length(required_replac)

  # creating the herd matrix

  number_rows <- months_to_project

  number_cols <- time_first_calv + 9 * cow_calving_int

  # Matriz to project

  herdMatrixProjected <- matrix(ncol = number_cols, nrow = number_rows)

  replacemProduced <- matrix(ncol = 1, nrow = months_to_project)

  # initiating the projected matrix

  for(i in 2:nrow(herdMatrixProjected)){

    for(j in ncol(herdMatrixProjected)){

      # first step

      herdMatrixProjected[1, 1:time_first_calv] <- required_replac # month 1 - heifers

      herdMatrixProjected[1, ((time_first_calv + 1):number_cols)] <- herdMatrix[((time_first_calv + 1):number_cols)] # month 1 - heifers

      herdMatrixProjected[i, 2:time_first_calv] <- herdMatrixProjected[i - 1, 1:(time_first_calv - 1)] * (1 - cull_rate_month_heifer)

      # second step

      herdMatrixProjected[i, (time_first_calv + 2):number_cols] <- herdMatrixProjected[i - 1, (time_first_calv + 1):(number_cols - 1)] * (1 - cull_rate_month_cows)

      herdMatrixProjected[i, (time_first_calv + 1)] <- n_adult_cows - sum(herdMatrixProjected[ i, (time_first_calv + 1):number_cols], na.rm = TRUE) # cows to replace

      herdMatrixProjected[i, 1] <- required_replac[1]

      replacemProduced[i, 1] <- (sum(herdMatrixProjected[i - 1, time_first_calv],
                                     herdMatrixProjected[i - 1, time_first_calv + (1 * cow_calving_int)],
                                     herdMatrixProjected[i - 1, time_first_calv + (2 * cow_calving_int)],
                                     herdMatrixProjected[i - 1, time_first_calv + (3 * cow_calving_int)],
                                     herdMatrixProjected[i - 1, time_first_calv + (4 * cow_calving_int)],
                                     herdMatrixProjected[i - 1, time_first_calv + (5 * cow_calving_int)],
                                     herdMatrixProjected[i - 1, time_first_calv + (6 * cow_calving_int)],
                                     herdMatrixProjected[i - 1, time_first_calv + (7 * cow_calving_int)],
                                     herdMatrixProjected[i - 1, time_first_calv + (8 * cow_calving_int)],
                                     herdMatrixProjected[i - 1, time_first_calv + (9 * cow_calving_int)],
                                     na.rm = TRUE) * (heifer_calf_born / 100 - (stillbirth_rate / 2 / 100)) * (1 - cull_rate_month_heifer))
    }

    # rename the columns of the matrix

    # heifer names

    month_growth <- seq(1, time_first_calv)

    heifer_categories <- paste0(c(rep("HeiOpen", time_first_calv - 9), rep("HeiPreg", 9)), "GrowMonth", month_growth)

    # cow names

    cow_categories <- rep(c(rep("CowOpen", cow_calving_int - 2 - 7), rep("CowPreg", 7), rep("DryPreg", 2)), 9)

    parity <- sort(rep(1:9, cow_calving_int))

    month_after_calv <- rep(seq(1, cow_calving_int), 9)

    cow_categories_lac <- paste0(cow_categories, "Lac", parity, "Month", month_after_calv)

    # renaming the matrix columns

    colnames(herdMatrixProjected) <- c(heifer_categories, cow_categories_lac)

  }

  # calculating number of springers to replace

  initial_value_calves <- mean(replacemProduced - herdMatrixProjected[ , 1], na.rm = TRUE)

  rep_vector_calves <- c()

  for(i in 2:time_first_calv){

    monthly_cul_rate <- (calves_heifers_cul / time_first_calv) / 100

    rep_vector_calves[1] <- initial_value_calves

    rep_vector_calves[i] <- rep_vector_calves[i - 1] * (1 - monthly_cul_rate)

  }

  number_springers <- round(rep_vector_calves[time_first_calv], 2) # grabbing the number of springers

  return(list("HerdProjected" = round(herdMatrixProjected, 3),
              "ReplacProducedCalves" = round(mean(replacemProduced, na.rm = TRUE), 2),
              "ReplacRequiredCalves" = round(mean(herdMatrixProjected[ , 1], na.rm = TRUE), 2),
              "Calves to sell (+) or purchase (-)" = round(mean(replacemProduced - herdMatrixProjected[ , 1], na.rm = TRUE), 2),
              "Or Springers to sell (+) or purchase (-)" = number_springers,
              "Vector of Calves Inventory Reduction" = round(rep_vector_calves, 2),
              "Note:" = "The producer can define the strategy that works best for him/her: purchase calves or springers!"))

}
