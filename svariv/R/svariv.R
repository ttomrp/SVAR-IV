#' Identify the impulse response for a VAR (using the VAR estimated from the vars package), using an external instrument.
#'
#' @param var A varest var, or a dataframe of reduced form residuals.
#' @param instrument A list containing the data for the instrument. Should be same length as the estimation sample.
#' @param dependent (String) Which variable in your var are you instrumenting.
#' @param p (Integer) How many lags does your var have (only needed if supplying a dataframe instead of a varest).
#'
#' @export
svariv <- function(var, instrument, dependent, p)
  UseMethod("svariv")

#' @export
svariv.est <- function(var, instrument, dependent, p) {
  res <- data.frame(stats::residuals(var))
  p <- var$p
  return(svariv(res, instrument[(p+1):length(instrument)], dependent, p))
}

#' @export
svariv.data.frame <- function(var, instrument, dependent, p) {
  seriesnames <- colnames(var)
  origorder <- seriesnames
  if (dependent %in% seriesnames) {
    # order dependent first
    seriesnames <- seriesnames[seriesnames != dependent]
    seriesnames <- c(dependent, seriesnames) # Order the dependent variable first
  } else {
    stop(paste("The series you are trying to instrument (", dependent, ") is not a series in the residual dataframe.", sep =""))
  }
  # Merge the instrument into the data frame
  var[, "instrument"] <- instrument

  # put together matrix of residuals
  u <- as.matrix(var[, seriesnames])

  # Now restrict to just the sample for the instrument (if necessary)
  u <- u[!is.na(var[, "instrument"]), ]

  # Useful constants
  T <- nrow(u)
  k <- ncol(u)

  # Some necessary parts of the covariance matrix
  gamma <- (1 / (T - k*p - 1)) * t(u) %*% u
  gamma_11 <- gamma[1,1]
  gamma_21 <- matrix(gamma[2:nrow(gamma), 1], c(k-1,1))
  gamma_22 <- matrix(gamma[2:nrow(gamma), 2:nrow(gamma)], c(k-1,k-1))

  # First stage regression
  firststage <- stats::lm(stats::as.formula(paste(dependent, " ~ instrument", sep = "")), var)
  var[names(stats::predict(firststage)), "fs"] <- stats::predict(firststage)

  # Now get the second-stage coefficients - this becomes the column (though we need to scale it)
  coefs <- rep(0, k)
  names(coefs) <- seriesnames
  for (i in 1:k) {
    s <- seriesnames[i]
    if (s != dependent) {
      secondstage <- stats::lm(stats::as.formula(paste(s, " ~ fs", sep = "")), var)
      coefs[i] <- secondstage$coefficients["fs"]
    } else {
      coefs[i] <- firststage$coefficients["instrument"]
    }
  }
  s21_on_s11 <- matrix(coefs[2:k], c(k-1,1))

  Q <- (s21_on_s11 * gamma_11) %*% t(s21_on_s11) - (gamma_21 %*% t(s21_on_s11) + s21_on_s11 %*% t(gamma_21)) + gamma_22

  s12s12 <- t(gamma_21 - s21_on_s11 * gamma_11) %*% solve(Q) %*% (gamma_21 - s21_on_s11 * gamma_11)

  s11_squared <- gamma_11 - s12s12

  sp <- as.numeric(sqrt(s11_squared))

  # finally, scale the coefs (the colnames are used to reorder to the original ordering)
  return(sp * coefs[origorder])
}
