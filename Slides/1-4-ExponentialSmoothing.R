## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  cache=TRUE,
  warning=FALSE,
  message=FALSE)
library(fpp2)
options(digits=4, width=55)

## ----sesfit, echo=TRUE, cache=TRUE---------------------------------------
fc <- ses(oil, h=5)
summary(fc[["model"]])

## ----sesparam, echo=FALSE, cache=TRUE------------------------------------
#tmp <- accuracy(fc)
#print(round(c(tmp[,c("MAE","RMSE","MAPE")],SSE=sum(residuals(fc)^2)),1))
alpha <- fc$model$par[1]
l0 <- fc$model$par[2]

## ---- echo=TRUE, fig.height=4--------------------------------------------
oil %>% ses(PI=FALSE) %>% autoplot

## ---- fig.height=3.6, echo=TRUE------------------------------------------
window(ausair, start=1990, end=2004) %>%
  holt(h=5, PI=FALSE) %>%
  autoplot()

## ---- echo=TRUE----------------------------------------------------------
livestock2 <- window(livestock, start=1970, end=2000)
fc1 <- ses(livestock2)
fc2 <- holt(livestock2)
fc3 <- holt(livestock2, damped = TRUE)

## ----echo=FALSE----------------------------------------------------------
tab <- matrix(NA, ncol=3,nrow=10)
colnames(tab) <- c("SES","Linear trend","Damped trend")
rownames(tab) <- c("$\\alpha$","$\\beta^*$","$\\phi$","$\\ell_0$","$b_0$",
                   "Training RMSE","Test RMSE","Test MAE","Test MAPE","Test MASE")
# SSE
tab[1,1] <- fc1$model$par["alpha"]
tab[4,1] <- fc1$model$par["l"]
tab[6,1] <- sqrt(fc1$model$mse)
tab[c(7:10),1] <- accuracy(fc1,livestock)["Test set",c("RMSE","MAE","MAPE","MASE")]
# Holt
tab[1,2] <- fc2$model$par["alpha"]
tab[2,2] <- fc2$model$par["beta"]/fc1$model$par["alpha"]
tab[4,2] <- fc2$model$par["l"]
tab[5,2] <- fc2$model$par["b"]
tab[6,2] <- sqrt(fc2$model$mse)
tab[c(7:10),2] <- accuracy(fc2,livestock)["Test set",c("RMSE","MAE","MAPE","MASE")]
# Damped trend
tab[1,3] <- fc3$model$par["alpha"]
tab[2,3] <- fc3$model$par["beta"]/fc1$model$par["alpha"]
tab[3,3] <- fc3$model$par["phi"]
tab[4,3] <- fc3$model$par["l"]
tab[5,3] <- fc3$model$par["b"]
tab[6,3] <- sqrt(fc3$model$mse)
tab[c(7:10),3] <- accuracy(fc3,livestock)["Test set",c("RMSE","MAE","MAPE","MASE")]
# Convert to characters
tab <- as.data.frame(formatC(tab, format="f", digits=2))
# Remove missing values
tab <- apply(tab, 2, function(x){j <- grep("[ ]*NA",x); x[j] <- ""; return(x)})
# Show table
knitr::kable(tab[-(1:6),], booktabs=TRUE)

## ----fig-7-comp, echo=TRUE, fig.height=3.6-------------------------------
autoplot(window(livestock, start=1970)) +
  autolayer(fc1, series="SES", PI=FALSE) +
  autolayer(fc2, series="Linear trend", PI=FALSE) +
  autolayer(fc3, series="Damped trend", PI=FALSE) +
  ylab("Livestock, sheep in Asia (millions)")

## ----7-HW, echo=TRUE-----------------------------------------------------
aust <- window(austourists,start=2005)
fc1 <- hw(aust,seasonal="additive")
fc2 <- hw(aust,seasonal="multiplicative")

## ---- fig.height=3.2, echo=FALSE-----------------------------------------
tmp <- cbind(Data=aust,
  "HW additive forecasts" = fc1[["mean"]],
  "HW multiplicative forecasts" = fc2[["mean"]])

autoplot(tmp) + xlab("Year") +
  ylab("International visitor night in Australia (millions)") +
  scale_color_manual(name="",
    values=c('#000000','#1b9e77','#d95f02'),
    breaks=c("Data","HW additive forecasts","HW multiplicative forecasts"))

## ----fig-7-LevelTrendSeas, echo=FALSE------------------------------------
addstates <- fc1$model$states[,1:3]
multstates <- fc2$model$states[,1:3]
colnames(addstates) <- colnames(multstates) <-
  c("level","slope","season")
p1 <- autoplot(addstates, facets=TRUE) + xlab("Year") +
  ylab("") + ggtitle("Additive states")
p2 <- autoplot(multstates, facets=TRUE) + xlab("Year") +
  ylab("") + ggtitle("Multiplicative states")
gridExtra::grid.arrange(p1,p2,ncol=2)

