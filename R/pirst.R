#' pirst
#'
#' @param data A Dataframe in Form c(X,   - numeric
#'                                   Y,   - numeric
#'                                   Condition, - 1 or -1
#'                                   Person     - number of subject)
#'
#' @param nperms Number of permutations you want to draw in the PIRST-algorithm
#'
#' @return A List with: - the pirst effectsize
#'                      - a voctor with the proportion of SSE's greater than the original SSE
#'
#' @export
#'
#' @examples pirst_sc(data,1000)
pirst <- function(data, nperms) {
  names(data) <- c("X", "Y", "Condition", "Person")
  SSE_greater <- c(rep(0, max(data$Person)))
  for (i in 1:max(data$Person)) {
    calc_simdata <- data[data$Person == i, ]
    message("For subject ", i)
    get_SSE_greater <- pirst_sc(calc_simdata, nperms)
    SSE_greater[i] <- get_SSE_greater[["Proportion greater"]]
  }
  SSE_greater = na.omit(SSE_greater)
  Prop_greater <- sum(SSE_greater > 0.5) / length(SSE_greater)
  Statistic <- list(Prop_greater, SSE_greater)
  names(Statistic) <- c("Effectsize", "List of Proportion of SSE's greater 0.5")
  Statistic
}
