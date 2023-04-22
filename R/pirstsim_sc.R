### Simulation Data for PIRST (Benjamin, Griffin, & Douglas, 2019) ###

### R-Version by Lukas Rommel

###Settings for Simulation####
################################################################################
# nhalf                         Points per Condition = 50, 10, or 4
# overlap                       Overlap or better said: How much points are "free" on the left and right side
# interaction_constant          0 = Single process model. 0.1 , 0.15 or 0.2 = dual process model
# noise                         0, 0.1 , 0.2 , 0.4
# curve                         0 -linear , 1  -concave up, -1 - concave down
# cases                         If you run an experiment, how many cases do you want?
################################################################################

###FUNCTIONS###

##Single Case##
#' pirstsim_sc
#'
#' @param nhalf Number of datapoints per condition
#' @param overlap Number of overlapping points to the sides if there is no noise
#' @param interaction_constant 0 = Single process. >0 = dual process.
#' @param noise Gaussian noise added to all datapoints.
#' @param curve 0 -linear , 1  -concave up, -1 - concave down
#'
#' @return a Dataframe in Form c(X, Y, Condition)
#' @export
#'
#' @examples
#' pirstsim_sc(nhalf = 4,
#'             overlap = 1,
#'             interaction_constant = 0.2,
#'             noise = 0,
#'             curve = 1,)
#'
#'             Use the st_plot function on the dataframe for better understanding.
pirstsim_sc <- function(nhalf,
                        overlap,
                        interaction_constant,
                        noise,
                        curve) {
  ###producing the linear funtion###
  npoints <- nhalf * 2
  Condition <- c(rep(1, nhalf), rep(-1, nhalf))
  p <- overlap
  X1 <- rep(0, nhalf)
  X2 <- rep(0, nhalf)

  # producing the points
  for (i in 1:nhalf) {
    X1[i] <- 0.7 * (i / nhalf) + 0.15
    X2[i] <- 0.7 * (i / nhalf) + 0.15
  }
  X3 <- c(X1, X2)

  # evenly strechtch points out
  for (i in 1:npoints) {
    if (Condition[i] == -1) {
      X3[i] <- X3[i] + 0.35 * ((p - 0.5) / nhalf)
    } else {
      X3[i] <- X3[i] - 0.35 * ((p - 0.5) / nhalf)
    }
  }

  #Linear, Curve uo or Curve down cases
  if (curve == 0) {
    #Linear
    Y <- X3
    X <- X3
  } else if (curve == 1) {
    # transfroming to Concave Up
    Y <- sin((90 * X3 + 270) * pi / 180) + 1
    X <- cos((90 * X3 + 270) * pi / 180)
  } else if (curve == -1) {
    # transforming to Concave Down
    Y <- cos((90 * X3 + 270) * pi / 180)
    X <- sin((90 * X3 + 270) * pi / 180) + 1
  } else {
    warning("invalid Curve input")
  }


  #Adding the Interaction constant to Condition 2
  for (i in 1:npoints) {
    if (Condition[i] == -1) {
      Y[i] <- Y[i] + interaction_constant
    }
  }

  #adding noise
  Y <- Y + rnorm(npoints, 0, noise)
  X <- X + rnorm(npoints, 0, noise)

  #Combine to a simdataframe
  simdata <- as.data.frame(cbind(X, Y, Condition))
  simdata
}
