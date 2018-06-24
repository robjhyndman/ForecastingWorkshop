## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ----gasolinedata--------------------------------------------------------
autoplot(gasoline) +
  xlab("Year") + ylab("Thousands of barrels per day") +
  ggtitle("Weekly US finished motor gasoline products")

## ----callsdata, message=FALSE, warning=FALSE, echo=FALSE,fig.height=5----
p1 <- autoplot(calls) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(breaks=seq(1,33,by=2)) +
  ggtitle("5 minute call volume at North American bank")
p2 <- autoplot(window(calls, end=4)) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(minor_breaks = seq(1,4,by=0.2))
gridExtra::grid.arrange(p1,p2)

## ----turk, echo=FALSE----------------------------------------------------
telec <- read.csv("../data/turkey_elec.csv")
telec <- msts(telec/1e3, start=2000, seasonal.periods = c(7,354.37,365.25))
autoplot(telec) +
  ggtitle("Turkish daily electricity demand") +
  xlab("Year") + ylab("Electricity Demand (GW)")

## ----callsmstl, fig.height=4.6-------------------------------------------
calls %>% mstl() %>% autoplot() + xlab("Week")

## ----callsmstlf----------------------------------------------------------
calls %>%  stlf() %>% autoplot() + xlab("Week")

## ----callsmstlf2---------------------------------------------------------
calls %>%  stlf() %>% autoplot(include=5*169) + xlab("Week")

## ----callsharmonics, echo=TRUE, fig.asp=0.45-----------------------------
fit <- auto.arima(calls, seasonal=FALSE, lambda=0,
         xreg=fourier(calls, K=c(10,10)))
fit %>%
  forecast(xreg=fourier(calls, K=c(10,10), h=2*169)) %>%
  autoplot(include=5*169) +
    ylab("Call volume") + xlab("Weeks")

## ----callsorder, echo=FALSE----------------------------------------------
ncoef <- length(coef(fit))
arma <- sum(arimaorder(fit)[-2])
nf169 <- sum(grepl("169",names(coef(fit))))
nf845 <- sum(grepl("845",names(coef(fit))))
if(ncoef != (arma+nf169+nf845))
  stop("Coefficients don't add up")

## ----gasoline, echo=TRUE, fig.height=4-----------------------------------
gasoline %>% tbats() %>% forecast() %>% autoplot()

## ----callcentref, echo=TRUE, fig.height=4--------------------------------
calls %>% tbats() %>% forecast() %>% autoplot()

## ----telecf, echo=TRUE, fig.height=4-------------------------------------
telec %>% tbats() %>% forecast() %>% autoplot()

