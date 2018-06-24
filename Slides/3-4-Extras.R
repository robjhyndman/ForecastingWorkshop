## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ----baggedets1, echo=FALSE----------------------------------------------
library(Mcomp)
df <- cbind(
  'M495' = M3[[1896]]$x,
  'Box-Cox transformed' = BoxCox(M3[[1896]]$x, BoxCox.lambda(M3[[1896]]$x))
)
autoplot(df, facets=TRUE)

## ----baggedets2, echo=FALSE----------------------------------------------
df[,2] %>% stl(s.window='periodic') %>% autoplot

## ----baggedets3, echo=FALSE----------------------------------------------
fit <- bld.mbb.bootstrap(Mcomp::M3[[1896]]$x, 11)
lambda <- BoxCox.lambda(Mcomp::M3[[1896]]$x)
as.data.frame(fit) %>% as.ts -> z
tsp(z) <- tsp(Mcomp::M3[[1896]]$x)
colnames(z) <- c("Data",paste("B",1:10,sep=""))

## ----baggedets4, echo=FALSE----------------------------------------------
stlb <- stl(df[,2], s.window='periodic')
stlb$time.series[,3] <- BoxCox(z[,2],lambda) - stlb$time.series[,1] - stlb$time.series[,2]
autoplot(stlb)

## ----baggedets5, echo=FALSE----------------------------------------------
autoplot(Mcomp::M3[[1896]]$x) +
  xlab("Year") + ylab("M495") +
  ylim(min(z),max(z))

## ----baggedets6, echo=FALSE----------------------------------------------
autoplot(Mcomp::M3[[1896]]$x) +
  forecast::autolayer(z[,2]) +
  xlab("Year") + ylab("M495") +
  ylim(min(z),max(z)) +
  theme(legend.position="none")

## ----baggedets7, echo=FALSE----------------------------------------------
autoplot(Mcomp::M3[[1896]]$x) +
  forecast::autolayer(z[,2:11]) +
  xlab("Year") + ylab("M495") +
  ylim(min(z),max(z)) +
  theme(legend.position="none")

## ----baggedets8, echo=TRUE, cache=TRUE, fig.height=4---------------------
baggedETS(Mcomp::M3[[1896]]$x) %>%
  forecast %>% autoplot +
  xlab("Year") + ylab("M495")

## ----baggedets9, echo=FALSE, cache=TRUE, fig.height=4--------------------
baggedETS(Mcomp::M3[[1896]]$x) %>%
  forecast %>% autoplot +
  xlab("Year") + ylab("M495")

## ---- fig.height=3.8-----------------------------------------------------
eggs %>%
  ets(model="AAN", damped=FALSE, lambda=0) %>%
  forecast(h=50, biasadj=TRUE) %>%
  autoplot()

## ----constrained, echo=FALSE---------------------------------------------
    # Bounds
    a <- 50
    b <- 400
    # Transform data and fit model
    fit <- log((eggs-a)/(b-eggs)) %>%
      ets(model="AAN", damped=FALSE)
    fc <- forecast(fit, h=50)
    # Back-transform forecasts
    fc[["mean"]] <- (b-a)*exp(fc[["mean"]]) /
      (1+exp(fc[["mean"]])) + a
    fc[["lower"]] <- (b-a)*exp(fc[["lower"]]) /
     (1+exp(fc[["lower"]])) + a
    fc[["upper"]] <- (b-a)*exp(fc[["upper"]]) /
     (1+exp(fc[["upper"]])) + a
    fc[["x"]] <- eggs
    # Plot result on original scale
    autoplot(fc)

## ----combine1, message=FALSE, warning=FALSE, echo=FALSE------------------
train <- window(auscafe, end=c(2012,9))
h <- length(auscafe) - length(train)
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),
  h=h)
STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

## ----combineplot, dependson="combine1", echo=FALSE, fig.height=4.8-------
autoplot(auscafe) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Year") + ylab("$ billion") +
  ggtitle("Australian monthly expenditure on eating out")

## ----combineaccuracy, dependson="combine1"-------------------------------
c(ETS = accuracy(ETS, auscafe)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, auscafe)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, auscafe)["Test set","RMSE"],
  NNAR = accuracy(NNAR, auscafe)["Test set","RMSE"],
  TBATS = accuracy(TBATS, auscafe)["Test set","RMSE"],
  Combination =
    accuracy(Combination, auscafe)["Test set","RMSE"])

## ----aggregates----------------------------------------------------------
# First fit a model to the data
fit <- ets(gas/1000)
# Forecast six months ahead
fc <- forecast(fit, h=6)
sum(fc[["mean"]][1:6])
# Simulate 10000 future sample paths
nsim <- 10000
h <- 6
sim <- numeric(nsim)
for(i in seq_len(nsim))
  sim[i] <- sum(simulate(fit, future=TRUE, nsim=h))
mean(sim)

## ----aggregates3, dependson="aggregates"---------------------------------
#80% interval:
quantile(sim, prob=c(0.1, 0.9))
#95% interval:
quantile(sim, prob=c(0.025, 0.975))

## ----backcasting_functions-----------------------------------------------
# Function to reverse time
reverse_ts <- function(y)
{
  ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}
# Function to reverse a forecast
reverse_forecast <- function(object)
{
  h <- length(object[["mean"]])
  f <- frequency(object[["mean"]])
  object[["x"]] <- reverse_ts(object[["x"]])
  object[["mean"]] <- ts(rev(object[["mean"]]),
    end=tsp(object[["x"]])[1L]-1/f, frequency=f)
  object[["lower"]] <- object[["lower"]][h:1L,]
  object[["upper"]] <- object[["upper"]][h:1L,]
  return(object)
}

## ----backcasting, dependson="backcasting_functions", echo=FALSE, fig.height=3.5----
# Backcast example
euretail %>%
  reverse_ts() %>%
  auto.arima() %>%
  forecast() %>%
  reverse_forecast() -> bc
autoplot(bc) +
  ggtitle(paste("Backcasts from",bc[["method"]]))

## ------------------------------------------------------------------------
autoplot(gold)

## ---- fig.height=3-------------------------------------------------------
gold %>% na.interp() %>%
  autoplot(series="Interpolated") +
    autolayer(gold, series="Original") +
    scale_color_manual(
      values=c(`Interpolated`="red",`Original`="gray"))

## ---- fig.height=3.4-----------------------------------------------------
autoplot(gold)

## ---- fig.height=3.4-----------------------------------------------------
tsoutliers(gold)

## ---- fig.height=3.4-----------------------------------------------------
gold %>% tsclean() %>% autoplot()

