## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE----------------------------------------
library(LWFBrook90R)
library(data.table)

## -----------------------------------------------------------------------------
options_b90 <- set_optionsLWFB90()
param_b90 <- set_paramLWFB90()

## -----------------------------------------------------------------------------
soil <- cbind(slb1_soil, hydpar_wessolek_tab(texture = slb1_soil$texture))

## ---- eval = FALSE------------------------------------------------------------
#  data("slb1_meteo")
#  b90res <- run_LWFB90(options_b90 = options_b90,
#                       param_b90 = param_b90,
#                       climate = slb1_meteo,
#                       soil = soil)

## ---- echo = FALSE------------------------------------------------------------
b90res <- LWFBrook90R:::b90res

## -----------------------------------------------------------------------------
str(b90res, max.level = 1)

## -----------------------------------------------------------------------------
b90res$output[, dates := as.Date(paste(yr, mo, da, sep = "-"))]

## -----------------------------------------------------------------------------
b90res$layer_output[, dates := as.Date(paste(yr, mo, da, sep = "-"))]
swat100cm <- b90res$layer_output[which(nl <= 15), list(swat100cm = sum(swati)),
                                by  = dates]

## ---- fig.height=5, fig.width=7, echo =FALSE, fig.cap="Simulation results for sample data"----
oldpar <- par(no.readonly = T)
par(mar=c(4.1,4.1,1.1,4.1), oma = c(1,1,1,1))
plot(b90res$output$dates, 
     b90res$output$tran, type ='l',
     col = "green", ylab = "tran [mm]", xlab = "")
par(new =TRUE)
plot(swat100cm$dates,
     swat100cm$swat100cm, 
     ylim=c(100, 350), type ='l', col = "blue",
     xaxt = "n", yaxt ="n", xlab= "", ylab = "")
axis(4,pretty(c(100,350)))
mtext("swat_100cm [mm]", side = 4, line =3)
legend("bottom",inset = -0.25,
       legend = c("tran", "swat100cm"),
       col = c("green", "blue"),  lty = 1, 
       bty = "n", xpd = TRUE,  horiz = TRUE,  text.width = 100)
par(oldpar)

