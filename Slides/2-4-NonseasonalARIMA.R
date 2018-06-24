## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ----arp, echo=FALSE, fig.height=3---------------------------------------
set.seed(1)
p1 <- autoplot(10 + arima.sim(list(ar = -0.8), n = 100)) +
  ylab("") + ggtitle("AR(1)")
p2 <- autoplot(20 + arima.sim(list(ar = c(1.3, -0.7)), n = 100)) +
  ylab("") + ggtitle("AR(2)")
gridExtra::grid.arrange(p1,p2,nrow=1)

## ---- echo=FALSE, out.width="50%", fig.height=2.2, fig.width=2.2---------
p1

## ---- fig.height=2.2, fig.width=2.2, out.width="50%", echo=FALSE---------
p2

## ----maq, fig.height=2.5, echo=FALSE-------------------------------------
set.seed(2)
p1 <- autoplot(20 + arima.sim(list(ma = 0.8), n = 100)) +
  ylab("") + ggtitle("MA(1)")
p2 <- autoplot(arima.sim(list(ma = c(-1, +0.8)), n = 100)) +
  ylab("") + ggtitle("MA(2)")
gridExtra::grid.arrange(p1,p2,nrow=1)

## ---- fig.height=2.2, fig.width=2.2, out.width="50%", echo=FALSE---------
p1

## ---- fig.height=2.2, fig.width=2.2, out.width="50%", echo=FALSE---------
p2

## ---- fig.height=3.8-----------------------------------------------------
autoplot(uschange[,"Consumption"]) +
  xlab("Year") + ylab("Quarterly percentage change") +
  ggtitle("US consumption")

## ---- echo=TRUE----------------------------------------------------------
(fit <- auto.arima(uschange[,"Consumption"]))

## ----usconsumptioncoefs, echo=FALSE--------------------------------------
coef <- coefficients(fit)
intercept <- coef['intercept'] * (1-coef['ar1'] - coef['ar2'])

## ---- include=FALSE------------------------------------------------------
# Following line assumes forecast v8.4+
if(!identical(arimaorder(fit),c(p=2L,d=0L,q=2L)))
  stop("Different model from expected")

## ---- echo=TRUE, fig.height=4--------------------------------------------
fit %>% forecast(h=10) %>% autoplot(include=80)

## ---- echo=TRUE, fig.height=4--------------------------------------------
autoplot(internet)

## ---- echo=TRUE, fig.height=4--------------------------------------------
internet %>% diff() %>% autoplot()

## ---- echo=TRUE, fig.height=4--------------------------------------------
(fit <- auto.arima(internet))

## ---- echo=TRUE, fig.height=4--------------------------------------------
(fit <- auto.arima(internet, stepwise=FALSE,
  approximation=FALSE))

## ---- echo=TRUE, fig.height=4--------------------------------------------
checkresiduals(fit, plot=TRUE)

## ---- echo=TRUE, fig.height=4--------------------------------------------
checkresiduals(fit, plot=FALSE)

## ---- echo=TRUE, fig.height=4--------------------------------------------
fit %>% forecast() %>% autoplot()

## ----ee1, fig.height=3.3, echo=TRUE--------------------------------------
eeadj <- seasadj(stl(elecequip, s.window="periodic"))
autoplot(eeadj) + xlab("Year") +
  ylab("Seasonally adjusted new orders index")

## ----ee2, echo=TRUE, fig.height=4----------------------------------------
eeadj %>% diff() %>% autoplot()

## ---- echo=TRUE----------------------------------------------------------
fit <- auto.arima(eeadj, stepwise=FALSE, approximation=FALSE)
summary(fit)

## ---- echo=TRUE, fig.height=2.5------------------------------------------
ggAcf(residuals(fit))

## ---- echo=TRUE----------------------------------------------------------
checkresiduals(fit, plot=FALSE)

## ---- echo=TRUE----------------------------------------------------------
fit %>% forecast() %>% autoplot()

## ---- echo=TRUE, fig.height=3.7------------------------------------------
fit %>% forecast(bootstrap=TRUE) %>% autoplot()

