#'pirstsim
#'
#' @param nhalf Number of datapoints per condition
#' @param overlap Number of overlapping points to the sides if there is no noise
#' @param interaction_constant 0 = Single process. >0 = dual process.
#' @param noise Gaussian noise added to all datapoints.
#' @param curve 0 -linear , 1  -concave up, -1 - concave down
#' @param cases number of single cases you want to draw
#' @return a Dataframe in Form c(X, Y, Condition, Person)
#' @export
#'
#' @examples
#' pirstsim(nhalf = 4,
#'          overlap = 1,
#'          interaction_constant = 0.2,
#'          noise = 0,
#'          curve = 1,
#'          cases = 100)
pirstsim <- function(nhalf,
                     overlap,
                     interaction_constant,
                     noise,
                     curve,
                     cases) {
  exp_simdata <- pirstsim_sc(nhalf, overlap, interaction_constant, noise, curve)
  exp_simdata$Person <- c(rep(1, nhalf * 2))
  for (i in 2:cases) {
    simdata_part <- pirstsim_sc(nhalf, overlap, interaction_constant, noise, curve)
    simdata_part$Person <- c(rep(i, nhalf * 2))
    exp_simdata <- rbind(exp_simdata, simdata_part)
  }
  exp_simdata
}
