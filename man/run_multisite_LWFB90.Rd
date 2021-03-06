% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msiterunLWFB90.R
\name{run_multisite_LWFB90}
\alias{run_multisite_LWFB90}
\title{Make a multi-site simulation using lists of climate, soil, and
parameter input objects.}
\usage{
run_multisite_LWFB90(
  options_b90,
  param_b90,
  soil = NULL,
  climate,
  climate_args = NULL,
  all_combinations = FALSE,
  cores = 2,
  show_progress = TRUE,
  ...
)
}
\arguments{
\item{options_b90}{Named list of model control options to be used in all
simulations}

\item{param_b90}{Named list of parameters to be used in all simulations, or a
list of multiple parameter sets.}

\item{soil}{Data.frame with soil properties to be used in all simulations, or
a list of data.frames with different soil profiles.}

\item{climate}{Data.frame with climate data, or a list of climate
data.frames. Alternatively, a function can be supplied that returns a
data.frame. Arguments to the function can be passed via
\code{climate_args}.}

\item{climate_args}{List of named lists of arguments passed to
\code{climate}, if this is a function.}

\item{all_combinations}{Logical: Set up and run all possible combinations of
individual \code{param_b90}, \code{climate} and \code{soil} objects?
Default is \code{FALSE}, running one object or the list of \code{param_b90}
objects for a series of climate/soil combinations.}

\item{cores}{Number of cores to use for parallel processing.}

\item{show_progress}{Logical: Show progress bar? Default is \code{TRUE}. See
also section \code{Progress bar} below.}

\item{...}{Further arguments passed to \code{\link{run_LWFB90}}.}
}
\value{
A named list with the results of the single runs as returned by
  \code{\link{run_LWFB90}}. Simulation or processing errors are passed on. The
  names of the returned list entries are concatenated from the names of the
  input list entries in the following form: <climate> <soil> <param_b90>. If
  \code{climate} is a function, the names for <climate> are taken from the
  names of \code{climate_args}.
}
\description{
Wrapper function for \code{\link{run_LWFB90}} to make multiple parallel
simulations of one or several parameter sets, for a series of sites with
individual climate and soil, or individual parameter sets for each
climate/soil combinations.
}
\section{Data management}{

The returned list of single run results can become very large, if many
simulations are performed and the selected output contains daily resolution
data sets, especially daily layer-wise soil moisture data. To not overload
memory, it is advised to reduce the returned simulation results to a minimum,
by carefully selecting the output, and make use of the option to pass a list
of functions to \code{\link{run_LWFB90}} via argument \code{output_fun}.
These functions perform directly on the output of a single run simulation,
and can be used for aggregating model output on-the-fly, or for writing
results to a file or database. The regular output of
\code{\link{run_LWFB90}} can be suppressed by setting \code{rtrn.output =
FALSE}, for exclusively returning the output of such functions.
To provide full flexibility, the names of the current \code{soil},
\code{param_b90}, and \code{climate} are automatically passed as additional
arguments (\code{soil_nm}, \code{param_nm},\code{clim_nm}) to
\code{\link{run_LWFB90}} and in this way become available to functions passed
via \code{output_fun}. In order to not overload the memory with climate input
data, it is advised to provide a function instead of a list of \code{climate}
data.frames, and specify its arguments for individual sites in
\code{climate_args}, in case many sites with individual climates will be
simulated.
}

\section{Progress bar}{

This function provides a progress bar via the package \CRANpkg{progressr} if
\code{show_progress=TRUE}. The parallel computation is then wrapped with
\code{progressr::with_progress()} to enable progress reporting from
distributed calculations. The appearance of the progress bar (including
audible notification) can be customized by the user for the entire session
using \code{progressr::handlers()} (see \code{vignette('progressr-intro')}).
}

\examples{
\donttest{
data("slb1_meteo")
data("slb1_soil")

opts <- set_optionsLWFB90(budburst_method = "Menzel", enddate = as.Date("2002-12-31"))

# define parameter sets
param_l <- list(spruce = set_paramLWFB90(maxlai = 5,
                                         budburst_species = "Picea abies (frueh)",
                                         winlaifrac = 0.8),
                beech = set_paramLWFB90(maxlai = 6,
                                        budburst_species = "Fagus sylvatica",
                                        winlaifrac = 0))

soil <- cbind(slb1_soil, hydpar_wessolek_tab(slb1_soil$texture))

# define list of soil objects
soils <- list(soil1 = soil, soil2 = soil)

# define list of climate objects
climates <- list(clim1 = slb1_meteo, clim2 = slb1_meteo)

# run two parameter sets on a series of climate and soil-objects
res <- run_multisite_LWFB90(param_b90 = param_l,
                      options_b90 = opts,
                      soil = soils,
                      climate = climates)
names(res)

# set up and run individual parameter sets for individual locations

# set up location parameters
loc_parm <- data.frame(loc_id = names(climates),
                       coords_y = c(48.0, 54.0),
                       eslope = c(30,0),
                       aspect = c(180,0))

# create input list of multiple param_b90 list objects
param_l <- lapply(names(climates), function(x, loc_parms) {
  parms <- set_paramLWFB90()
  parms[match(names(loc_parm),names(parms), nomatch = 0)] <-
    loc_parm[loc_parm$loc_id == x, which(names(loc_parm) \%in\% names(parms))]
  parms
}, loc_parm = loc_parm)

names(param_l) <- c("locpar1", "locpar2")

res <- run_multisite_LWFB90(param_b90 = param_l,
                      options_b90 = opts,
                      soil = soils,
                      climate = climates)
names(res)
}
}
