#' pirst_sc
#'
#' @param data A Dataframe in Form c(X,   - numeric
#'                                   Y,   - numeric
#'                                   Condition, - 1 or -1)
#'
#' @param nperms Number of permutations you want to draw in the PIRST-algorithm
#'
#' @return A List with: - a vector of the SSE's of all permutations
#'                      - Proportions of greater equal and smaller SSE's
#'                      - the original SSE
#'                      - the input Dataframe with additional isoreg-fitted values
#' @export
#'
#' @examples pirst_sc(data,1000)
pirst_sc <- function(data, nperms) {
  data <- find_eligible_points(data)
  data <- isoreg(data)
  SSE_compare <- SSE_calc(data)
  possible_permutations <- factorial(length(data$eligibility[data$eligibility == 1])) /
    (factorial(length(data$Condition[data$eligibility == 1 & data$Condition == 1])) *
       factorial(length(data$Condition[data$eligibility == 1 & data$Condition == -1])))
  if (possible_permutations == 1) {
    SSE <- NaN
    message("No permutations are possible, this subject will not have impact on the Test-statistic.")
  } else if (possible_permutations < 100 || (possible_permutations < nperms && nperms < 100 )) {
    SSE <- permutate_small(data)
    message("Number of possible Permutations is only ", possible_permutations,
            ".\nTo boost performance just ", possible_permutations, " Permutations are drawn!")
  } else {
    SSE <- permutate_big(data, nperms)
    message("Number of possible Permutations is ", possible_permutations, "\n",
            nperms, " Permutations are drawn randomly!")
  }
  Proportion_greater <- sum(SSE > SSE_compare) / length(SSE)
  Proportion_equal <- sum(SSE == SSE_compare) / length(SSE)
  Proportion_smaller <- sum(SSE < SSE_compare) / length(SSE)
  Data_full <- data

  Statistic <- list(Proportion_greater, Proportion_equal, Proportion_smaller,
                    SSE, SSE_compare, Data_full)
  names(Statistic) <- c("Proportion greater", "Proportion equal", "Proportion smaller", "All SSE's", "SSE original", "Data")
  Statistic
}

#Get points eligible for permuting
find_eligible_points <- function(data) {
  names(data) <- c("X", "Y", "Condition")
  data$eligibility <- rep(1, nrow(data))
  data <- data[order(data$Condition, decreasing = TRUE), ]#needed for next function
  for (i in 1:(nrow(data))) {
    if (data$Condition[i] == 1) {
      if ((data$X[i] >= max(data$X[data$Condition == -1]) &&
           data$Y[i] >= max(data$Y[data$Condition == -1])) ||
          data$X[i] <= min(data$X[data$Condition == -1]) &&
          data$Y[i] <= min(data$Y[data$Condition == -1])) {
        data$eligibility[i] <- 0
      }
    } else {
      if ((data$X[i] >= max(data$X[data$Condition == 1]) &&
           data$Y[i] >= max(data$Y[data$Condition == 1])) ||
          data$X[i] <= min(data$X[data$Condition == 1]) &&
          data$Y[i] <= min(data$Y[data$Condition == 1])) {
        data$eligibility[i] <- 0
      }
    }
  }
  data
}

#Fit isotonic regressions and add y-Values to the Dataframe
isoreg <- function(data) {
  iso_reg1 <- isotone::gpava(data$X[data$Condition == 1],
                             data$Y[data$Condition == 1])
  iso_reg2 <- isotone::gpava(data$X[data$Condition == -1],
                             data$Y[data$Condition == -1])
  data$Y_isoreg <- c(iso_reg1[["x"]], iso_reg2[["x"]])
  data
}

#get Sum Squares of Error
SSE_calc <- function(data) {
  SSE1 <- sum((data$Y_isoreg[data$Condition == 1] -
                 data$Y[data$Condition == 1])^2)
  SSE2 <- sum((data$Y_isoreg[data$Condition == -1] -
                 data$Y[data$Condition == -1])^2)
  SSE <- SSE1 + SSE2
  SSE
}

#get Permuations for Cases with less possible permutations than 100
getpermutations <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(unique(res))
  }
}

#needed for permutation big and permutation small to fit regressions and get SSE's without changing dataframe
isoreg_ssepermcalc <- function(data){
  iso_reg1 = isotone::gpava(data$X[data$Condition == 1],data$Y[data$Condition == 1])
  iso_reg2 = isotone::gpava(data$X[data$Condition == -1],data$Y[data$Condition == -1])
  SSE = sum((iso_reg1[["x"]]-iso_reg1[["y"]])^2)+sum((iso_reg2[["x"]]-iso_reg2[["y"]])^2)
}

#get SSE's of all permutations if there are less than 100 possible permutations
permutate_small <- function(data) {
  cont_shuffle <- data$Condition[data$eligibility == 1] #for shuffling again if permutation = original
  Permutations <- getpermutations(cont_shuffle)
  SSE_all <- c(rep(0, nrow(Permutations)))
  for (i in 1:nrow(Permutations)) {
    shuffle <- Permutations[i, ]
    data$Condition[data$eligibility == 1] <- shuffle
    SSE_all[i] <- isoreg_ssepermcalc(data)
  }
  SSE_all <- SSE_all[-1] # original SSE
  SSE_all
}

#get SSE's of random nperm permutations
permutate_big <- function(data, nperms) {
  cont_shuffle <- data$Condition[data$eligibility == 1] #for shuffling again if permutation = original
  SSE_all <- c(rep(0, nperms))
  for (i in 1:nperms) {
    shuffle <- sample(data$Condition[data$eligibility == 1])
    while (all(shuffle == cont_shuffle) == TRUE) {
      shuffle <- sample(data$Condition[data$eligibility == 1])
    }
    data$Condition[data$eligibility == 1] <- shuffle
    SSE_all[i] <- isoreg_ssepermcalc(data)
  }
  SSE_all
}
