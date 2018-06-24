## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ----usconsump, fig.height=5, fig.width=8.5------------------------------
autoplot(uschange[,1:2], facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumption and personal income")

## ---- fig.height=5, fig.width=8.5----------------------------------------
qplot(Income,Consumption, data=as.data.frame(uschange)) +
  ggtitle("Quarterly changes in US consumption and personal income")

## ----usconsump2, echo=TRUE, fig.height=3---------------------------------
(fit <- auto.arima(uschange[,1], xreg=uschange[,2]))

## ---- echo=TRUE, fig.height=3.7------------------------------------------
checkresiduals(fit, test=FALSE)

## ---- echo=TRUE, fig.height=3.7------------------------------------------
checkresiduals(fit, plot=FALSE)

## ----usconsump3, echo=TRUE, fig.height=3.--------------------------------
fcast <- forecast(fit,
  xreg=rep(mean(uschange[,2]),8), h=8)
autoplot(fcast) + xlab("Year") +
  ylab("Percentage change") +
  ggtitle("Forecasts from regression with ARIMA(1,0,2) errors")

## ---- echo=TRUE, fig.height=3.6------------------------------------------
qplot(elecdaily[,"Temperature"], elecdaily[,"Demand"]) +
  xlab("Temperature") + ylab("Demand")

## ---- echo=TRUE, fig.height=4--------------------------------------------
autoplot(elecdaily, facets = TRUE)

## ---- echo=TRUE, fig.height=3.6------------------------------------------
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
checkresiduals(fit)

## ---- echo=FALSE---------------------------------------------------------
checkresiduals(fit, plot=FALSE)

## ---- echo=TRUE----------------------------------------------------------
# Forecast one day ahead
forecast(fit, xreg = cbind(26, 26^2, 1))

## ----echo=TRUE, fig.height=3.8-------------------------------------------
fcast <- forecast(fit,
  xreg = cbind(rep(26,14), rep(26^2,14),
    c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
autoplot(fcast) + ylab("Electicity demand (GW)")

## ----cafe, echo=TRUE, fig.height=4.6, fig.width=8------------------------
cafe04 <- window(auscafe, start=2004)
autoplot(cafe04)
cafefit <- function(K)
{
  require(latex2exp)
  fit <- auto.arima(cafe04, xreg=fourier(cafe04, K=K),
                  seasonal = FALSE, lambda = 0)
  reg <- log(cafe04) - residuals(fit, type='regression')
  reg <- exp(reg - mean(reg) + mean(log(cafe04)))
  fc <- fit %>%
    forecast(xreg=fourier(cafe04, K=K, h=24))
  autoplot(cafe04, series="Data") +
    autolayer(fc) + ggtitle(TeX(paste(fc$method,"and $\\lambda = 0$"))) +
    autolayer(reg, series="Regression fit") +
    xlab(paste("K=",K,"   AICC=",round(fit$aicc,2))) +
    ylab("") + ylim(1.5,4.7)
}

## ----cafe1, dependson='cafe', fig.height=5, echo=FALSE-------------------
cafefit(1)

## ----cafe2, dependson='cafe', fig.height=5, echo=FALSE-------------------
cafefit(2)

## ----cafe3, dependson='cafe', fig.height=5, echo=FALSE-------------------
cafefit(3)

## ----cafe4, dependson='cafe', fig.height=5, echo=FALSE-------------------
cafefit(4)

## ----cafe5, dependson='cafe', fig.height=5, echo=FALSE-------------------
cafefit(5)

## ----cafe6, dependson='cafe', fig.height=5, echo=FALSE-------------------
cafefit(6)

## ----cafe7, fig.height=4-------------------------------------------------
fit <- auto.arima(cafe04, xreg=fourier(cafe04, K=5),
                  seasonal = FALSE, lambda = 0)
fc <- forecast(fit, xreg=fourier(cafe04, K=5, h=24))
autoplot(fc)

## ----gasmodel, echo=TRUE-------------------------------------------------
harmonics <- fourier(gasoline, K = 13)
(fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE))

## ----gasres1, echo=TRUE, dependson='gasmodel'----------------------------
checkresiduals(fit, test=FALSE)

## ----gasres2, echo=TRUE, dependson='gasmodel'----------------------------
checkresiduals(fit, plot=FALSE)

## ----gasf, echo=TRUE, fig.height=3.5-------------------------------------
newharmonics <- fourier(gasoline, K = 13, h = 156)
fc <- forecast(fit, xreg = newharmonics)
autoplot(fc)

## ----calls, echo=TRUE, fig.height=4--------------------------------------
autoplot(calls)

## ----callsmodel, echo=TRUE-----------------------------------------------
xreg <- fourier(calls, K = c(10,0))
(fit <- auto.arima(calls, xreg=xreg, seasonal=FALSE, stationary=TRUE))

## ----callsres, echo=TRUE-------------------------------------------------
checkresiduals(fit, test=FALSE)

## ----callsf, echo=TRUE, fig.height=4-------------------------------------
fc <- forecast(fit, xreg = fourier(calls, c(10,0), 1690))
autoplot(fc)

## ----tvadvert, fig.height=3.5--------------------------------------------
autoplot(insurance, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Insurance advertising and quotations")

## ---- echo=TRUE----------------------------------------------------------
Advert <- cbind(
    AdLag0 = insurance[,"TV.advert"],
    AdLag1 = lag(insurance[,"TV.advert"],-1),
    AdLag2 = lag(insurance[,"TV.advert"],-2),
    AdLag3 = lag(insurance[,"TV.advert"],-3)) %>%
  head(NROW(insurance))

# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1],
  stationary=TRUE)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2],
  stationary=TRUE)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3],
  stationary=TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4],
  stationary=TRUE)
c(fit1$aicc,fit2$aicc,fit3$aicc,fit4$aicc)

## ----tvadvertagain, echo=TRUE--------------------------------------------
(fit <- auto.arima(insurance[,1], xreg=Advert[,1:2],
  stationary=TRUE))

## ----tvadvertparam, echo=FALSE-------------------------------------------
# Store coefficients
phi1 <- coef(fit)['ar1']
phi2 <- coef(fit)['ar2']
phi3 <- coef(fit)['ar3']
intercept <- coef(fit)['intercept']
gamma0 <- coef(fit)['AdLag0']
gamma1 <- coef(fit)['AdLag1']

## ---- echo=TRUE, fig.height=3.3------------------------------------------
fc <- forecast(fit, h=20,
  xreg=cbind(c(Advert[40,1],rep(10,19)), rep(10,20)))
autoplot(fc)

## ---- echo=TRUE, fig.height=3.3------------------------------------------
fc <- forecast(fit, h=20,
  xreg=cbind(c(Advert[40,1],rep(8,19)), rep(8,20)))
autoplot(fc)

## ---- echo=TRUE, fig.height=3.3------------------------------------------
fc <- forecast(fit, h=20,
  xreg=cbind(c(Advert[40,1],rep(6,19)), rep(6,20)))
autoplot(fc)

