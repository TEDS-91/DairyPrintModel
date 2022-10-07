#' Herd Stabilization
#'
#' @param time_first_calv Time at first calving.
#' @param heifer_calf_born Heifer calf born.
#' @param stillbirth_rate Stillbirth rate.
#' @param calves_heifers_cul Culling rate for heifers and calves.
#' @param cow_rep_rate Culling rate of adult cows.
#' @param cow_calving_int Calving interval.
#' @param n_adult_cows Number of adult cows.
#'
#' @return A matrix with the demographics.
#'
herd_stabilization <- function(time_first_calv,
                               heifer_calf_born,
                               stillbirth_rate,
                               calves_heifers_cul,
                               cow_rep_rate,
                               cow_calving_int,
                               n_adult_cows){

  # support variables calculated based on the inputs

  # month_preg_cow <- cow_calving_int - 8

  #month_prg_heifer <- time_first_calv - 8

  cull_rate_month_heifer <- (calves_heifers_cul / time_first_calv) / 100

  cull_rate_month_cows <- (cow_rep_rate / 12) / 100

  # creating the herd matrix

  number_rows <- 1000 # maximum number of rows for the herdMatrix

  number_cols <- time_first_calv + 9 * cow_calving_int # calculating the number of cols

  herdMatrix <- matrix(ncol = number_cols, nrow = number_rows) # setting up the matrix to be filled

  tol <- 0.0001 # level of tolerance till achieve the convergence

  criteria <- 1 # starting value for the criteria of convergence

  # nested for loop to fill up the herdMatrix

  for(i in 2:nrow(herdMatrix)){

    for(j in ncol(herdMatrix)){

      # first step

      herdMatrix[1, 2] <- n_adult_cows * (1 - cull_rate_month_heifer) # month 1 - heifers

      herdMatrix[i, 2:time_first_calv] <- herdMatrix[i - 1, 1:(time_first_calv - 1)] * (1 - cull_rate_month_heifer)

      herdMatrix[1, time_first_calv + 2] <- n_adult_cows * (1 - cull_rate_month_cows) # month 1 - cows 2 month

      herdMatrix[1, time_first_calv + 1] <- n_adult_cows - sum(herdMatrix[1, (time_first_calv + 2):number_cols], na.rm = TRUE) # month 1 - cows to replace

      # second step

      herdMatrix[i, (time_first_calv + 2):number_cols] <- herdMatrix[i - 1, (time_first_calv + 1):(number_cols - 1)] * (1 - cull_rate_month_cows)

      herdMatrix[i, (time_first_calv + 1)] <- n_adult_cows - sum(herdMatrix[ i, (time_first_calv + 1):number_cols], na.rm = TRUE) # cows to replace

      herdMatrix[2:i, 1] <- sum(herdMatrix[i, time_first_calv], herdMatrix[i, time_first_calv + (1 * cow_calving_int)],
                                herdMatrix[i, time_first_calv + (2 * cow_calving_int)],
                                herdMatrix[i, time_first_calv + (3 * cow_calving_int)],
                                herdMatrix[i, time_first_calv + (4 * cow_calving_int)],
                                herdMatrix[i, time_first_calv + (5 * cow_calving_int)],
                                herdMatrix[i, time_first_calv + (6 * cow_calving_int)],
                                herdMatrix[i, time_first_calv + (7 * cow_calving_int)],
                                herdMatrix[i, time_first_calv + (8 * cow_calving_int)],
                                herdMatrix[i, time_first_calv + (9 * cow_calving_int)],
                                na.rm = TRUE) * (heifer_calf_born / 100 - (stillbirth_rate / 2 / 100)) * (1 - cull_rate_month_heifer)

      criteria <- abs(sum(herdMatrix[i, ], na.rm = TRUE) - sum(herdMatrix[i - 1, ], na.rm = TRUE))

    }

    # if to stop once the criterion was achieved

    if (criteria < tol){
      break
    }

  }

  # removing all NA's from the herdMatrix

  herdMatrix <- stats::na.omit(herdMatrix)

  # rename the columns of the matrix

  # heifer names

  heifer_categories <- c(rep("HeiOpen", time_first_calv - 9), rep("HeiPreg", 9))

  # cow names

  cow_categories <- rep(c(rep("CowOpen", cow_calving_int - 2 - 7), rep("CowPreg", 7), rep("DryPreg", 2)), 9)

  parity <- sort(rep(1:9, cow_calving_int))

  cow_categories_lac <- paste0(cow_categories, "Lac", parity)

  # renaming the matrix

  colnames(herdMatrix) <- c(heifer_categories, cow_categories_lac)

  # getting the last row from the herdMatrix

  last_row <- dim(herdMatrix)[1]

  # returning just the last row with the stable herd

  herdStable <- herdMatrix[last_row, ]

  return(herdStable)
}
