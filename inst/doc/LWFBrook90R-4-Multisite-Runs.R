## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE, eval = TRUE---------------------------
library(LWFBrook90R)
library(data.table)
data("slb1_meteo")
data("slb1_soil")
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

## -----------------------------------------------------------------------------
parms_beech <- set_paramLWFB90(maxlai = 6)
parms_spruce <- set_paramLWFB90(maxlai = 4.5, winlaifrac = 0.8)
parms_l <- list(beech = parms_beech, spruce = parms_spruce)

## -----------------------------------------------------------------------------
soils_l <- list(soil1 = soil, soil2 = soil, soil3 = soil)
climates_l <- list(clim1 = slb1_meteo, clim2 = slb1_meteo, clim3 = slb1_meteo)

## -----------------------------------------------------------------------------
startdate <- as.Date("2002-06-01")
enddate <- as.Date("2002-06-30")

msite_run1 <- run_multisite_LWFB90(
  options_b90 = set_optionsLWFB90(startdate = startdate, enddate = enddate),
  param_b90 = parms_l,
  climate = climates_l,
  soil = soils_l,
  cores = 2)

## -----------------------------------------------------------------------------
str(msite_run1, max.level = 1)

## ---- result='hide'-----------------------------------------------------------
tdir <- tempdir()
fnames <- paste0(tdir, "/clim", 1:3, ".csv")
lapply(fnames, function(x) {
  write.csv(slb1_meteo[year(slb1_meteo$dates) == 2002,], 
            file = x, row.names = FALSE)
})


## -----------------------------------------------------------------------------
srun <- run_LWFB90(
  options_b90 = set_optionsLWFB90(startdate = startdate, enddate = enddate),
  param_b90 = set_paramLWFB90(),
  soil = soil,
  climate = fread,
  file = fnames[1],
  rtrn.input = FALSE)

## -----------------------------------------------------------------------------
clim_args <- list(climfromfile1 = list(file = fnames[1]),
                  climfromfile2 = list(file = fnames[2]),
                  climfromfile3 = list(file = fnames[3]))

## -----------------------------------------------------------------------------
msite_run2 <- run_multisite_LWFB90(
  options_b90 = set_optionsLWFB90(startdate = startdate, enddate = enddate),
  param_b90 = parms_l,
  soil = soils_l,
  climate = fread,
  climate_args = clim_args,
  cores = 2)

## -----------------------------------------------------------------------------
str(msite_run2, max.level = 1)

## -----------------------------------------------------------------------------
output_function <- function(x, tolayer, basedir = getwd(),
                            soil_nm, clim_nm, param_nm ) {
  # file-name
  filenm = file.path(basedir, paste(soil_nm, clim_nm, param_nm, sep = "_"))
  
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
  swattran_vp <- swat_tran[doy >= vpstart & doy <= vpend, 
            list(swat_vp_mean = mean(swat), tran_vp_sum = sum(tran)), by = yr]
  
  write.csv(swattran_vp, file = paste0(filenm, ".csv"))
}

## -----------------------------------------------------------------------------
msite_run3 <- run_multisite_LWFB90(
  options_b90 = set_optionsLWFB90(startdate = startdate, enddate = enddate),
  param_b90 = parms_l,
  soil = soils_l,
  climate = fread,
  climate_args = clim_args,
  rtrn_input = FALSE, rtrn_output = FALSE,
  output_fun = output_function,
  tolayer = 15,
  basedir = tdir,
  cores = 2)

## -----------------------------------------------------------------------------
list.files(tdir, pattern = "csv")

## ---- echo = FALSE, results='hide'--------------------------------------------
file.remove(list.files(tdir, pattern = "csv", full.names = T))

