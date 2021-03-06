#' Interpolate plant properties using the 'b90' method.
#'
#' Creates a daily sequence for one year from parameters
#'
#' @param minval Minimum value.
#' @param maxval Maximum value.
#' @param doy.incr Day of year when increasing from \code{minval} to
#'   \code{maxval} begins.
#' @param incr.dur Duration (number of days) since  \code{doy.incr} until
#'   \code{maxval} is reached.
#' @param doy.decr Day of year when decreasing to \code{minval} begins.
#' @param decr.dur Duration (number of days) since \code{doy.incr} until
#'   \code{minval} is reached.
#' @param maxdoy Length of the year, 366 for leap years, 365 for normal years.
#'
#' @return A numeric vector of length \code{maxdoy}.
#'
#' @examples
#' plot(plant_b90(minval = 0,maxval=1,
#' doy.incr = 121,incr.dur = 28,
#' doy.decr = 280, decr.dur = 50,
#' maxdoy = 365))
#' @export
plant_b90 <- function(minval, maxval,
                      doy.incr,incr.dur,
                      doy.decr, decr.dur,
                      maxdoy) {
  stopifnot(doy.incr > 0,
            doy.decr > (doy.incr+incr.dur))


  if ((doy.decr+decr.dur) > maxdoy) {
    decr.dur <- maxdoy - doy.decr
    warning("shortened decr.dur, due to doy.decr+decr.dur > maxdoy")
  }

  ind <- c(1,doy.incr, doy.incr + incr.dur,
           doy.decr, doy.decr + decr.dur,
           maxdoy)
  values <- c(minval,minval,maxval,maxval,minval,minval)
  stats::approx(x = ind, y = values,method = "linear", xout = 1:maxdoy)$y
}
