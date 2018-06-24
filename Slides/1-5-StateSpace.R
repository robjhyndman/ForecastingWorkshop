## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ---- echo=TRUE----------------------------------------------------------
ets(h02)

## ---- echo=TRUE----------------------------------------------------------
ets(h02, model="AAA", damped=FALSE)

## ---- echo=TRUE, fig.height=4--------------------------------------------
h02 %>% ets() %>% autoplot()

## ---- echo=TRUE, fig.height=4--------------------------------------------
h02 %>% ets() %>% forecast() %>% autoplot()

## ---- echo=TRUE----------------------------------------------------------
h02 %>% ets() %>% accuracy()

h02 %>% ets(model="AAA", damped=FALSE) %>% accuracy()

## ---- echo=TRUE----------------------------------------------------------
train <- window(h02, end=c(2004,12))
test <- window(h02, start=2005)
fit1 <- ets(train)
fit2 <- ets(test, model = fit1)
accuracy(fit2)
accuracy(forecast(fit1,10), test)

