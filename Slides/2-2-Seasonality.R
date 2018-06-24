## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ----mstl, fig.height=4--------------------------------------------------
fit <- mstl(elecequip)
autoplot(fit)

## ----mstl3, dependson='mstl', fig.height=3.5-----------------------------
autoplot(elecequip, series="Data") +
 autolayer(trendcycle(fit), series="Trend") +
 ylab("New orders index") + xlab("") +
 ggtitle("Electrical equipment manufacturing (Euro area)")

## ----mstl2, dependson='mstl'---------------------------------------------
ggmonthplot(seasonal(fit)) + ylab("Seasonal")

## ----fig.height=3.3, dependson='mstl'------------------------------------
autoplot(elecequip, series="Data") +
  autolayer(seasadj(fit), series="SeasAdjust") +
  ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing")

## ---- echo=TRUE, fig.height=4--------------------------------------------
elecequip %>%
  mstl(s.window=5) %>%
  autoplot()

## ---- echo=TRUE, fig.height=4--------------------------------------------
elecequip %>%
  mstl(t.window=15, s.window='periodic', robust=TRUE) %>%
  autoplot()

## ---- fig.height=3.5-----------------------------------------------------
mstl(elecequip, t.window=15, s.window="periodic") %>%
  seasadj() %>% naive(h=24) %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

## ---- fig.height=3.5-----------------------------------------------------
mstl(elecequip, t.window=15, s.window="periodic") %>%
  forecast(method="naive", h=24) %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Reseasonalized naive forecasts")

