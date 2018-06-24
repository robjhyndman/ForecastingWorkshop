## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ---- echo=TRUE, fig.height=3.6------------------------------------------
autoplot(euretail) +
  xlab("Year") + ylab("Retail index")

## ---- echo=TRUE, fig.height=4--------------------------------------------
euretail %>% diff(lag=4) %>% autoplot()

## ---- echo=TRUE, fig.height=3.8------------------------------------------
euretail %>% diff(lag=4) %>% diff() %>%
  autoplot()

## ----euretail, echo=TRUE-------------------------------------------------
(fit <- auto.arima(euretail))

## ----euretail2, echo=TRUE------------------------------------------------
(fit <- auto.arima(euretail, stepwise=TRUE,
  approximation=FALSE))

## ---- dependson='euretail2'----------------------------------------------
checkresiduals(fit, test=FALSE)

## ---- dependson='euretail2'----------------------------------------------
checkresiduals(fit, plot=FALSE)

## ---- dependson='euretail2'----------------------------------------------
forecast(fit, h=36) %>% autoplot()

## ----h02, echo=FALSE-----------------------------------------------------
lh02 <- log(h02)
tmp <- cbind("H02 sales (million scripts)" = h02,
             "Log H02 sales"=lh02)
autoplot(tmp, facets=TRUE) + xlab("Year") + ylab("")

## ----h02b----------------------------------------------------------------
autoplot(diff(log(h02),12), xlab="Year",
  main="Seasonally differenced H02 scripts")

## ----h02tryharder, echo=TRUE, fig.height=3.6-----------------------------
(fit <- auto.arima(h02, lambda=0, max.order=9,
  stepwise=FALSE, approximation=FALSE))

## ---- echo=TRUE, fig.height=4, dependson='h02tryharder'------------------
checkresiduals(fit)

## ---- echo=FALSE, dependson='h02tryharder'-------------------------------
checkresiduals(fit, plot=FALSE)

## ---- cache=TRUE, echo=FALSE---------------------------------------------
models <- rbind(
  c(3,0,0,2,1,0),
  c(3,0,1,2,1,0),
  c(3,0,2,2,1,0),
  c(3,0,1,1,1,0),
  c(3,0,1,0,1,1),
  c(3,0,1,0,1,2),
  c(3,0,1,1,1,1),
  c(4,0,3,0,1,1),
  c(3,0,3,0,1,1),
  c(4,0,2,0,1,1),
  c(3,0,2,0,1,1),
  c(2,1,3,0,1,1),
  c(4,1,1,2,1,2),
  c(4,1,2,2,1,2),
  c(3,1,2,2,1,2),
  c(4,1,2,1,1,2),
  c(4,1,2,2,1,1),
  c(2,1,4,0,1,1),
  c(2,1,5,0,1,1))
h <- 24
train.end <- time(h02)[length(h02)-h]
test.start <- time(h02)[length(h02)-h+1]
train <- window(h02,end=train.end)
test <- window(h02,start=test.start)

rmse <- numeric(NROW(models))
modelname <- character(NROW(models))
for(i in seq(length(rmse)))
{
  fit <- Arima(train, order=models[i,1:3],
          seasonal=models[i,4:6], lambda=0)
  fc <- forecast(fit,h=h)
  rmse[i] <- accuracy(fc, test)[2,"RMSE"]
  modelname[i] <- as.character(fit)
}
k <- order(rmse)
knitr::kable(data.frame(Model=modelname[k],RMSE=rmse[k]),
             digits=4)

## ----h02fa, echo=TRUE, fig.height=3.4------------------------------------
fit <- Arima(h02, order=c(4,1,1), seasonal=c(2,1,2),
  lambda=0)
autoplot(forecast(fit)) + xlab("Year") +
  ylab("H02 sales (million scripts)") + ylim(0.3,1.8)

## ----h02fb, echo=TRUE, fig.height=3.4------------------------------------
fit <- Arima(h02, order=c(4,1,2), seasonal=c(2,1,2),
  lambda=0)
autoplot(forecast(fit)) + xlab("Year") +
  ylab("H02 sales (million scripts)") + ylim(0.3,1.8)

## ----h02fc, echo=TRUE, fig.height=3.4------------------------------------
fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2),
  lambda=0)
autoplot(forecast(fit)) + xlab("Year") +
  ylab("H02 sales (million scripts)") + ylim(0.3,1.8)

