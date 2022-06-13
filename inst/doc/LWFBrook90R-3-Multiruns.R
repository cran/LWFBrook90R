## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## ---- warning = FALSE, message = FALSE, eval = TRUE---------------------------
library(LWFBrook90R)
library(data.table)

## -----------------------------------------------------------------------------
output_function <- function(x, tolayer) {
  # aggregate SWAT
  swat_tran <- x$layer_output[which(nl <= tolayer),
                              list(swat = sum(swati)),
                              by  = list(yr, doy)]
  #add transpiration from EVAPDAY.ASC
  swat_tran$tran <- x$output$tran

  # get beginning and end of growing season from input parameters
  vpstart <- x$model_input$param_b90$budburstdoy
  vpend <- x$model_input$param_b90$leaffalldoy
  swat_tran <- merge(swat_tran,
                     data.frame(yr = unique(swat_tran$yr),
                                vpstart, vpend), by = "yr")
  # mean swat and tran sum
  swat_tran[doy >= vpstart & doy <= vpend,
            list(swat_vp_mean = mean(swat), tran_vp_sum = sum(tran)), by = yr]
}

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
data("slb1_meteo")
data("slb1_soil")
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
b90res <- LWFBrook90R:::b90res

## ---- eval = FALSE------------------------------------------------------------
#  data("slb1_meteo")
#  data("slb1_soil")
#  soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
#  b90res <- run_LWFB90(options_b90 = set_optionsLWFB90(),
#                       param_b90 = set_paramLWFB90(),
#                       climate = slb1_meteo,
#                       soil = soil)

## -----------------------------------------------------------------------------
output_function(b90res, tolayer = 15)

## -----------------------------------------------------------------------------
set.seed(2021)
N=50
paramvar <- data.frame(maxlai = runif(N, 4,7),
                       glmax = runif(N,0.003, 0.01))

## ----eval = FALSE-------------------------------------------------------------
#  mrun_res <- run_multi_LWFB90(paramvar = paramvar,
#                               param_b90 = set_paramLWFB90(),
#                               cores = 2, # arguments below are passed to run_LWFB90()
#                               options_b90 = set_optionsLWFB90(),
#                               climate = slb1_meteo,
#                               soil = soil,
#                               rtrn_input = FALSE, rtrn_output = FALSE,
#                               output_fun = output_function,
#                               tolayer = 15) # argument to output_fun

## ---- eval = FALSE------------------------------------------------------------
#  mrun_dt <- rbindlist(lapply(mrun_res, function(x) x$output_fun[[1]]),
#                       idcol = "singlerun")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
mrun_dt <- LWFBrook90R:::mrun_dt

## ----boxplots, fig.width=7, fig.height = 5, echo =FALSE, fig.cap = "Growing season soil water storage and transpiration of 50 simulations, with random variation of parameters 'maxlai' and 'glmax'"----

oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
boxplot(swat_vp_mean~yr, data = mrun_dt, col = "blue")
boxplot(tran_vp_sum~yr, data = mrun_dt, col = "green")
par(oldpar)

