% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MakeStand.R
\name{make_standprop}
\alias{make_standprop}
\title{Create daily plant characteristics from parameters and options}
\usage{
make_standprop(options_b90, param_b90, out_yrs)
}
\arguments{
\item{options_b90}{A list of model control options.}

\item{param_b90}{A parameter list-object.}

\item{out_yrs}{Years for which values are returned.}
}
\value{
A data.frame containing daily sequences of 'age', 'height', 'sai',
  'densef', and 'lai'.
}
\description{
Creates daily sequences of 'age', 'height', 'sai', 'densef', and 'lai' from
parameters and options using \code{\link{approx_standprop}} and
\code{\link{make_seasLAI}}.
}
\examples{
options_b90 <- set_optionsLWFB90()
param_b90 <- set_paramLWFB90()

standprop <- make_standprop(options_b90,
                            param_b90,
                            out_yrs = 2002:2004)
plot(standprop$dates, standprop$lai, type = "l")
}
