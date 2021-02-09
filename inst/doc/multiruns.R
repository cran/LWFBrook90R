## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ---- warning = FALSE, message = FALSE, eval = TRUE---------------------------
library(LWFBrook90R)
library(data.table)

## -----------------------------------------------------------------------------
#  output_function <- function(x, tolayer) {
#    # aggregate SWAT
#    swat_tran <- x$SWATDAY.ASC[which(nl <= tolayer),
#                               list(swat = sum(swati)),
#                               by  = list(yr, doy)]
#    #add transpiration from EVAPDAY.ASC
#    swat_tran$tran <- x$EVAPDAY.ASC$tran
#  
#    # get beginning and end of growing season from input parameters
#    vpstart <- x$model_input$param_b90$budburstdoy
#    vpend <- x$model_input$param_b90$leaffalldoy
#    swat_tran <- merge(swat_tran,
#                       data.frame(yr = unique(swat_tran$yr),
#                                  vpstart, vpend), by = "yr")
#    # mean swat and tran sum
#    swat_tran[doy >= vpstart & doy <= vpend,
#              list(swat_vp_mean = mean(swat), tran_vp_sum = sum(tran)), by = yr]
#  }

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
data("slb1_meteo")
data("slb1_soil")
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
b90res <- LWFBrook90R:::b90res

## -----------------------------------------------------------------------------
#  data("slb1_meteo")
#  data("slb1_soil")
#  soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))
#  b90res <- run_LWFB90(options_b90 = set_optionsLWFB90(),
#                       param_b90 = set_paramLWFB90(),
#                       climate = slb1_meteo,
#                       soil = soil,
#                       output = set_outputLWFB90())

## -----------------------------------------------------------------------------
#  output_function(b90res, tolayer = 15)

## -----------------------------------------------------------------------------
#  set.seed(2021)
#  N=50
#  paramvar <- data.frame(maxlai = runif(N, 4,7),
#                         glmax = runif(N,0.003, 0.01))

## -----------------------------------------------------------------------------
#  mrun_res <- run_multi_LWFB90(paramvar = paramvar,
#                               param_b90 = set_paramLWFB90(),
#                               cores = 5, # arguments below are passed to run_LWFB90()
#                               options_b90 = set_optionsLWFB90(),
#                               climate = slb1_meteo,
#                               soil = soil,
#                               output = set_outputLWFB90(),
#                               rtrn_input = FALSE, rtrn_output = FALSE,
#                               output_fun = output_function,
#                               tolayer = 15) # argument to output_fun

## -----------------------------------------------------------------------------
#  mrun_dt <- rbindlist(lapply(mrun_res, function(x) x$output_fun[[1]]),
#                       idcol = "singlerun")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
mrun_dt <- LWFBrook90R:::mrun_dt

## ----results='hide', fig.height=5, fig.width=7, echo =FALSE, fig.cap = "Growing season soil water storage and transpiration of 50 simulations, with random variation of parameters 'maxlai' and 'glmax'"----
#  oldpar <- par(no.readonly = TRUE)
#  par(mfrow = c(1,2))
#  boxplot(swat_vp_mean~yr, data = mrun_dt, col = "blue")
#  boxplot(tran_vp_sum~yr, data = mrun_dt, col = "green")
#  par(oldpar)

## -----------------------------------------------------------------------------
#  parms_beech <- set_paramLWFB90(maxlai = 6)
#  parms_spruce <- set_paramLWFB90(maxlai = 4.5, winlaifrac = 0.8)
#  parms_l <- list(beech = parms_beech, spruce = parms_spruce)

## -----------------------------------------------------------------------------
#  soils_l <- list(soil1 = soil, soil2 = soil, soil3 = soil)
#  climates_l <- list(clim1 = slb1_meteo, clim2 = slb1_meteo, clim3 = slb1_meteo)

## -----------------------------------------------------------------------------
#  msite_run1 <- run_multisite_LWFB90(
#    options_b90 = set_optionsLWFB90(startdate = as.Date("2002-06-01"),
#                                    enddate = as.Date("2002-06-30")),
#    param_b90 = parms_l,
#    climate = climates_l,
#    soil = soils_l,
#    cores = 3)
#  str(msite_run1, max.level = 1)

## -----------------------------------------------------------------------------
#  tdir <- tempdir()
#  fnames <- paste0(tdir, "/clim", 1:3, ".csv")
#  write.csv(slb1_meteo[year(slb1_meteo$dates) %in% c(2002,2003),],
#            file = fnames[1], row.names = FALSE)
#  write.csv(slb1_meteo[year(slb1_meteo$dates) %in% c(2002,2003),],
#            file = fnames[2], row.names = FALSE)
#  write.csv(slb1_meteo[year(slb1_meteo$dates) %in% c(2002,2003),],
#            file = fnames[3], row.names = FALSE)

## -----------------------------------------------------------------------------
#  srun <- run_LWFB90(
#    options_b90 = set_optionsLWFB90(),
#    param_b90 = set_paramLWFB90(),
#    soil = soil,
#    output = set_outputLWFB90(),
#    climate = fread,
#    file = fnames[1],
#    rtrn.input = FALSE)

## -----------------------------------------------------------------------------
#  clim_args <- list(climfromfile1 = list(file = fnames[1]),
#                    climfromfile2 = list(file = fnames[2]),
#                    climfromfile3 = list(file = fnames[3]))

## -----------------------------------------------------------------------------
#  msite_run2 <- run_multisite_LWFB90(
#    options_b90 = set_optionsLWFB90(),
#    param_b90 = parms_l,
#    soil = soils_l,
#    climate = fread,
#    climate_args = clim_args,
#    cores = 3)
#  
#  str(msite_run2, max.level = 1)

## -----------------------------------------------------------------------------
#  output_function2 <- function(x, tolayer, basedir = getwd(),
#                              soil_nm, clim_nm, param_nm ) {
#    # file-name
#    filenm = file.path(basedir, paste(soil_nm, clim_nm, param_nm, sep = "_"))
#  
#    # aggregate SWAT
#    swat_tran <- x$SWATDAY.ASC[which(nl <= tolayer),
#                               list(swat = sum(swati)),
#                               by  = list(yr, doy)]
#    #add transpiration from EVAPDAY.ASC
#    swat_tran$tran <- x$EVAPDAY.ASC$tran
#  
#    # get beginning and end of growing season from input parameters
#    vpstart <- x$model_input$param_b90$budburstdoy
#    vpend <- x$model_input$param_b90$leaffalldoy
#    swat_tran <- merge(swat_tran,
#                       data.frame(yr = unique(swat_tran$yr),
#                                  vpstart, vpend), by = "yr")
#    # mean swat and tran sum
#    swattran_vp <- swat_tran[doy >= vpstart & doy <= vpend,
#              list(swat_vp_mean = mean(swat), tran_vp_sum = sum(tran)), by = yr]
#  
#    write.csv(swattran_vp, file = paste0(filenm, ".csv"))
#  }

## -----------------------------------------------------------------------------
#  msite_run3 <- run_multisite_LWFB90(
#    options_b90 = set_optionsLWFB90(),
#    param_b90 = parms_l,
#    soil = soils_l,
#    climate = fread,
#    climate_args = clim_args,
#    output = set_outputLWFB90(),
#    rtrn_input = FALSE, rtrn_output = FALSE,
#    output_fun = output_function2,
#    tolayer = 15,
#    basedir = tdir,
#    cores = 3)

## -----------------------------------------------------------------------------
#  list.files(tdir, pattern = "csv")

## ---- echo = FALSE, results='hide'--------------------------------------------
#  file.remove(list.files(tdir, pattern = "csv", full.names = T))

