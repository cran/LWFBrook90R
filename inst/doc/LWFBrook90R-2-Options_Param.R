## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE----------------------------------------
library(LWFBrook90R)
library(data.table)

options_b90 <- set_optionsLWFB90()
param_b90 <- set_paramLWFB90()

## -----------------------------------------------------------------------------
LAI_b90 <-  make_seasLAI(method = options_b90$lai_method,
                         year = 2003,
                         maxlai = param_b90$maxlai,
                         winlaifrac = param_b90$winlaifrac,
                         budburst_doy = param_b90$budburstdoy,
                         leaffall_doy = param_b90$leaffalldoy,
                         emerge_dur = param_b90$emergedur,
                         leaffall_dur = param_b90$leaffalldur)

## -----------------------------------------------------------------------------
options_b90$lai_method <- "linear"
param_b90$lai_doy <- c(1,110,117,135,175,220,250,290,365)
param_b90$lai_frac <- c(0.1,0.1,0.5,0.7,1.2,1.2,1.0,0.1,0.1)
LAI_linear <-  make_seasLAI(method = options_b90$lai_method,
                            year = 2003,
                            maxlai = param_b90$maxlai,
                            lai_doy = param_b90$lai_doy ,
                            lai_frac = param_b90$lai_frac)

## -----------------------------------------------------------------------------
options_b90$lai_method <- "Coupmodel"
param_b90$shp_budburst <- 0.5
param_b90$shp_leaffall <- 5
param_b90$shp_optdoy <- 180
LAI_coupmodel <-  make_seasLAI(method = options_b90$lai_method,
                               year = 2003,
                               maxlai = param_b90$maxlai,
                               budburst_doy = param_b90$budburstdoy,
                               leaffall_doy = param_b90$leaffalldoy,
                               shp_budburst = param_b90$shp_budburst,
                               shp_leaffall = param_b90$shp_leaffall,
                               shp_optdoy = param_b90$shp_optdoy)

## ---- echo = FALSE, fig.height=5, fig.width=7, fig.cap = "Methods featured by make_seasLAI()"----
oldpar <- par(no.readonly = T)
par(xpd = TRUE, mar = c(5.1,4.1,2.1,2.1), oma = c(1,1,1,1))

plot(LAI_b90, type = "n", xlab = "doy", ylab = "lai [m²/m²]", ylim = c(0,6))
with(param_b90, abline(v = c(budburstdoy,budburstdoy+emergedur,
                             leaffalldoy, leaffalldoy+leaffalldur), lty = 2, xpd = FALSE))
lines(LAI_b90, col ="green", lwd = 2)
lines(LAI_linear, col ="red", lwd = 2)
lines(LAI_coupmodel, col ="blue", lwd = 2)

with(param_b90, arrows(x0 = c(budburstdoy,leaffalldoy,shp_optdoy,budburstdoy,leaffalldoy),
                       x1 = c(budburstdoy,leaffalldoy, shp_optdoy,
                              budburstdoy+emergedur, leaffalldoy + leaffalldur),
                       y0 = c(-1.6,-1.6,3.5,2.5,2.5),y1 = c(-0.3,-0.3,4.8,2.5,2.5), 
                       length = 0.15, code = 2))
with(param_b90, arrows(x0 = c(budburstdoy,leaffalldoy),
                       x1 = c(budburstdoy+emergedur, leaffalldoy + leaffalldur),
                       y0 = c(2.5,2.5),y1 = c(2.5,2.5), 
                       length = 0.15, code = 3))

with(param_b90, text(x = c(budburstdoy, leaffalldoy, shp_optdoy), y = c(-1.9,-1.9,3.2),
                     c("budburstdoy", "leaffalldoy", "shp_optdoy")))
with(param_b90, text(x = c(budburstdoy+0.5*emergedur,leaffalldoy+0.5*leaffalldur),
                     y = 2, c("emergedur","leaffalldur")))
with(param_b90, text(
  x = c(budburstdoy/2, (leaffalldoy - budburstdoy - emergedur)/2 + budburstdoy + emergedur),
  y = c(winlaifrac*maxlai+0.4,maxlai+0.4), c("winlaifrac * maxlai", "maxlai")))

legend("topleft",c("'b90'","'linear'", "'Coupmodel'"), pch = NULL, lwd =2, col = c("green", "red", "blue"), bty = "n")
par(oldpar)

## ----echo = 1:6, results = 'hide',fig.height=5, fig.width=7, fig.cap = "Options and parameters affecting interannual variation of leaf area index."----
years <- 2001:2003
param_b90$maxlai <- c(4,6,5)
param_b90$shp_optdoy <- c(210,180,240)
param_b90$shp_budburst <- c(3,1,0.3)
param_b90$budburstdoy <- c(100,135,121) 
lai_variation <- make_seasLAI(method = options_b90$lai_method,
                              year = years,
                              maxlai = param_b90$maxlai,
                              budburst_doy = param_b90$budburstdoy,
                              leaffall_doy = param_b90$leaffalldoy,
                              shp_budburst = param_b90$shp_budburst,
                              shp_leaffall = param_b90$shp_leaffall,
                              shp_optdoy = param_b90$shp_optdoy)
par(mar=c(4.1,4.1,1.1,4.1), oma = c(1,1,1,1))
plot(seq.Date(as.Date("2001-01-01"), as.Date("2003-12-31"), by = "day"),
     lai_variation, col = "green", ylab = "lai [m²/m²]",
     type ="l", xlab = "", lwd = 2)
arrows( x0 = as.Date(paste(param_b90$budburstdoy, years),format = "%j %Y"),
        y0 = -1.0, y1 = -0.3, length = 0.15, code = 2, xpd =TRUE)
text(x = as.Date(paste(param_b90$budburstdoy, years),format = "%j %Y"),
     y = -1.3, paste("doy", param_b90$budburstdoy), xpd = TRUE)
par(oldpar)

## -----------------------------------------------------------------------------
# constant 'interpolation'
options_b90$standprop_interp <- 'constant'
param_b90$height <- c(20.2,20.8,21.3)
simyears <- 2002:2003
height_c <- approx_standprop(x_yrs=years,
                             y = param_b90$height,
                             approx.method = options_b90$standprop_interp)

# linear interpolation       
options_b90$standprop_interp <- 'linear'
param_b90$height_ini <- 19.1
height_l <- approx_standprop(x_yrs=years,
                             y = param_b90$height,
                             y_ini = param_b90$height_ini,
                             approx.method = options_b90$standprop_interp)

## -----------------------------------------------------------------------------
options_b90$use_growthperiod <- TRUE
height_l_gp <- approx_standprop(x_yrs = years,
                                y = param_b90$height,
                                y_ini = param_b90$height_ini,
                                use_growthperiod = options_b90$use_growthperiod,
                                startdoy = param_b90$budburstdoy,
                                enddoy = param_b90$leaffalldoy,
                                approx.method = options_b90$standprop_interp)

## ----echo = FALSE, fig.height=5, fig.width=7, fig.cap="Interpolated stand height derived from parameters using approx_standprop()"----
dates <- seq.Date(from = as.Date(paste0(min(years),"-01-01")),
                  to = as.Date(paste0(max(years),"-12-31")),
                  by = "day")
par(mar=c(4.1,4.1,1.1,4.1))
plot(dates, height_c,
     type = "l", lwd = 2, col = "black", 
     ylim = c(19,22), ylab = "height [m]", xlab = "", xpd = TRUE)
lines(dates, height_l, 
      col = "blue", lwd = 2)
lines(dates, height_l_gp, 
      col = "green", lwd = 2)
legend("topleft", legend = c("approx.method = 'constant'", "approx.method = 'linear'", "approx.method = 'linear', use_growthperiod = TRUE"), 
       col  = c("black", "blue", "green"),  lwd = 2, pch = NULL,
       bty = "n")
arrows( x0 = as.Date(paste(param_b90$budburstdoy, years),format = "%j %Y"),
        y0 = c(param_b90$height_ini,param_b90$height[1:2])-0.5, y1 = c(param_b90$height_ini,param_b90$height[1:2])-0.1, length = 0.15, code = 2, xpd =TRUE)
text(x = as.Date(paste(param_b90$budburstdoy, years), format = "%j %Y"),
     y = c(param_b90$height_ini,param_b90$height[1:2])-0.7, paste("doy", param_b90$budburstdoy), xpd = TRUE)
arrows( x0 = as.Date(paste(param_b90$leaffalldoy, years),format = "%j %Y"),
        y0 = param_b90$height-0.5, y1 = param_b90$height-0.1, length = 0.15, code = 2, xpd =TRUE)
text(x = as.Date(paste(param_b90$leaffalldoy, years), format = "%j %Y"),
     y = param_b90$height-0.7, paste("doy", param_b90$leaffalldoy), xpd = TRUE)
par(oldpar)

## ---- messages = FALSE--------------------------------------------------------
#Extend simulation period
options_b90$startdate <- as.Date("1980-01-01")
options_b90$enddate <- as.Date("1999-12-31")

soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))


#set up options for table input 
options_b90$standprop_input <- 'table'
param_b90$standprop_table <- slb1_standprop

# Set up dynamic budburst and leaf fall
options_b90$budburst_method <- "Menzel"
options_b90$leaffall_method <- "vonWilpert"
param_b90$budburst_species <- "Fagus sylvatica"

#run LWF-Brook90 without simulation
standprop_daily <- run_LWFB90(options_b90 = options_b90,
                              param_b90 = param_b90,
                              climate = slb1_meteo,
                              soil = soil,
                              output = output, 
                              run = FALSE)$standprop_daily

## ---- echo = FALSE, fig.height=5, fig.width=7, fig.cap="Stand properties generated using table input of annual stand characteristics"----
par(mar=c(4.1,4.1,1.1,4.1), oma = c(1,1,1,1))
with(standprop_daily,{
  plot(dates, lai, 
       type = "l", lwd = 2, col = "green", 
       ylab = "lai, sai m²/m²", xlab = "")
  lines(dates, sai, lwd = 2, col = "brown")
  par(new = TRUE) 
  plot(dates, height,
       type = "l", lwd = 2, col = "blue", 
       ylim = c(27, 32),
       ylab = "", xlab = "",
       xaxt = "n", yaxt = "n")
})
axis(4, at = seq(27,32, by = 1))
mtext("height [m]", side = 4, line = 3)
legend("bottom",horiz = TRUE, bty = "n",inset = -0.25, xpd =TRUE,
       legend = c("lai", "sai", "height"), 
       col  = c("green","brown", "blue"), lwd = 2, pch = NULL)
par(oldpar)

## ---- echo = FALSE, fig.height=4, fig.width=4---------------------------------
plot(make_rootden(soilnodes = seq(0,-2, by = -0.01), 
                  method = "betamodel", 
                  beta = 0.93),
     seq(-0.01,-2, by = -0.01), ylim = c(-2,0),
     type="l", lwd = 1.5,
     xlab = "relative root density", ylab = "soil depth [m]")
lines(make_rootden(soilnodes = seq(0,-2, by = -0.01), 
                   method = "betamodel", 
                   beta = 0.96),
      seq(-0.01,-2, by = -0.01), 
      lty = 2, lwd = 1.5)
lines(make_rootden(soilnodes = seq(0,-2, by = -0.01), 
                   method = "betamodel", 
                   beta = 0.98),
      seq(-0.01,-2, by = -0.01), 
      lty = 3, lwd = 1.5)
lines(make_rootden(soilnodes = seq(0,-2, by = -0.01), 
                   method = "betamodel", 
                   beta = 0.99),
      seq(-0.01,-2, by = -0.01), 
      lty = 4, lwd = 1.5)
legend("bottomright",legend = c(expression(paste(beta, "= 0.93")),expression(paste(beta, "= 0.96")),
                                expression(paste(beta, "= 0.98")),expression(paste(beta, "= 0.99"))),
       lwd = 1.5, pch = NULL, lty = 1:4, bty = "n")

## -----------------------------------------------------------------------------
param_b90$maxrootdepth <- -1.4
options_b90$root_method <- "betamodel" 
roots_beta <- make_rootden(soilnodes = c(max(slb1_soil$upper), slb1_soil$lower), 
                           maxrootdepth = param_b90$maxrootdepth,
                           beta = param_b90$betaroot,
                           method = options_b90$root_method)

## -----------------------------------------------------------------------------
options_b90$root_method <- 'table'
param_b90$rootden_table <- data.frame(
  upper = c(0.03,0,-0.02, -0.15, -0.35, -0.5, -0.65,-0.9,-1.1,-1.3),
  lower = c(0,-0.02, -0.15, -0.35, -0.5, -0.65,-0.9,-1.1,-1.3,-1.6),
  rootden = c(10,15, 35, 15, 7.5, 4, 12, 2, 2, 0))
roots_table <- make_rootden(soilnodes = c(max(slb1_soil$upper), slb1_soil$lower), 
                            method = options_b90$root_method, 
                            rootdat = param_b90$rootden_table)

## ---- echo = 1:4, fig.height=4, fig.width=4-----------------------------------
options_b90$root_method <- 'linear'
roots_linear <- make_rootden(soilnodes = c(max(slb1_soil$upper), slb1_soil$lower), 
                             maxrootdepth = param_b90$maxrootdepth,
                             method = options_b90$root_method)
options_b90$root_method <- 'const'
roots_constant <- make_rootden(soilnodes = c(max(slb1_soil$upper), slb1_soil$lower), 
                               maxrootdepth = param_b90$maxrootdepth,
                               method = options_b90$root_method)

plot(roots_constant, slb1_soil$lower,
     type = 's', lwd = 1.5,ylab = "soil depth [m]",xlab = "relative root density",
     col = "red")
lines(roots_linear, slb1_soil$lower,
      type = 's', col = "blue", lwd = 1.5)
lines(roots_table/100, slb1_soil$lower,
      type = 's', col = "green", lwd = 1.5)

lines(roots_beta*10, slb1_soil$lower, type = 's', col = "brown", lwd = 1.5)

legend("bottomright", c("'betamodel'","'table'","'linear'", "'constant'"),seg.len = 1.5,
       pch = NULL, lwd =1.5, col = c("brown", "green", "blue", "red"), bty = "n")


