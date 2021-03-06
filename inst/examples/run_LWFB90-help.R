# Set up lists containing model control options and model parameters:
param_b90 <- set_paramLWFB90()
options_b90 <- set_optionsLWFB90()

# Set start and end Dates for the simulation
options_b90$startdate <- as.Date("2003-06-01")
options_b90$enddate <- as.Date("2003-06-30")

# Derive soil hydraulic properties from soil physical properties
# using pedotransfer functions
soil <- cbind(slb1_soil, hydpar_wessolek_tab(slb1_soil$texture))

# Run LWF-Brook90
b90.result <- run_LWFB90(options_b90 = options_b90,
                        param_b90 = param_b90,
                        climate = slb1_meteo,
                        soil = soil)

# use a function to be performed on the output:
# aggregate soil water storage down to a specific layer
agg_swat <- function(x, layer) {
  out <- aggregate(swati~yr+doy,
                   x$SWATDAY.ASC,
                   FUN = sum,
                   subset = nl <= layer)
  out[order(out$yr, out$doy),]}

# run model without returning the selected output.
b90.aggswat <- run_LWFB90(options_b90 = options_b90,
                         param_b90 = param_b90,
                         climate = slb1_meteo,
                         soil = soil,
                         output_fun = list(swat = agg_swat),
                         rtrn_output = FALSE,
                         layer = 10)  # passed to output_fun
str(b90.aggswat$output_fun$swat)
