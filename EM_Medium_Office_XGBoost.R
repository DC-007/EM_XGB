rm(list = ls())
# install.packages('package name') Use this to install the following packages
require(xgboost)
library(readr)
library(ggplot2)
library(dplyr)
library(Matrix)
library(caTools)
library(DataCombine)
library(astsa)
library(lubridate)
library(plotly)
set.seed(0)
ptm <- proc.time()
### Read data from csv ####
#rawData <- read_csv('Data_Office_Medium_Chicago.csv', col_names = T, col_types = cols(.default = "d", DATE = col_date("%m/%d/%y")))
#rawData <- read_csv('Data_Office_Medium_Phoenix.csv', col_names = T, col_types = cols(.default = "d", DATE = col_date("%m/%d/%y")))
rawData <- read_csv('Data_Office_Medium_Miami.csv', col_names = T, col_types = cols(.default = "d", DATE = col_date("%m/%d/%y")))
### Preliminary EDA ####
#ggplot(rawData,aes(x=1:17520,y=CoolingEnergy)) + geom_point(aes(color = CoolingEnergy,alpha = DBT)) +geom_line()
#acf(rawData$CoolingEnergy, lag.max = 72, type = "correlation",
#        plot = TRUE, na.action = na.fail, demean = TRUE)
#lag2.plot (rawData$DBT,rawData$CoolingEnergy, 10, corr = TRUE, smooth = TRUE)
#lag2.plot (rawData$DPT,rawData$CoolingEnergy, 25, corr = TRUE, smooth = TRUE)
#lag2.plot (rawData$GHR,rawData$CoolingEnergy, 10, corr = TRUE, smooth = TRUE)
#lag2.plot (rawData$DNR,rawData$CoolingEnergy, 10, corr = TRUE, smooth = TRUE)
#lag2.plot (rawData$DHR,rawData$CoolingEnergy, 10, corr = TRUE, smooth = TRUE)
##############
lagpad <- function(x, k) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(x)) 
    stop('x must be numeric')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  c(rep(NA, k), x)[1 : length(x)] 
}
rawData$DBTLag1 <- lagpad(rawData$DBT,1)
rawData$DPTLag1 <- lagpad(rawData$DPT,1)
rawData$GHRLag1 <- lagpad(rawData$GHR,1)
rawData$DNRLag1 <- lagpad(rawData$DNR,1)
rawData$DHRLag1 <- lagpad(rawData$DHR,1)
rawData <- rawData[-1,]
# Using data for years 2011, 2012 for training
Training.Data <- rawData[(nrow(rawData)-(4*8760)+1):(nrow(rawData)-2*8760),]
Training.Data$CoolingEnergy <- (Training.Data$CoolingEnergy)/3600000
# Using data for year 2013 & 2014 for testing
Testing.Data <- rawData[(nrow(rawData)-(2*8760)+1):nrow(rawData),]
Testing.Data$CoolingEnergy <- (Testing.Data$CoolingEnergy)/3600000

### Preparing the building schedule ####
# Weekday hourly cooling setpoints [80	80	80	80	80	78	77	75	75	75	75	75	75	75	75	75	75	75	75	75	75	75	80	80]
# Saturday hourly cooling setpoints [80	80	80	80	80	78	77	75	75	75	75	75	75	75	75	75	75	75	75	75	75	75	80	80]
# Sunday hourly cooling setpoints [80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80	80]
# CSP 80 -> 0; CSP 78 -> 1; CSP 77 -> 2; CSP 75 -> 3
# Weekday Occupancy Schedule [0	0	0	0	0	0	0.1	0.2	0.95	0.95	0.95	0.95	0.5	0.95	0.95	0.95	0.95	0.3	0.1	0.1	0.1	0.1	0.05	0.05]
# Saturday Occupancy Schedule [0	0	0	0	0	0	0.1	0.1	0.3	0.3	0.3	0.3	0.1	0.1	0.1	0.1	0.1	0.05	0.05	0	0	0	0	0]
# Sunday Occupancy Schedule [0	0	0	0	0	0	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0	0	0	0	0	0]
# Weekday OA_DAMPER [0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	0	0]
# Saturday OA_DAMPER [0	0	0	0	0	0	0	1	1	1	1	1	1	1	1	1	1	1	0	0	0	0	0	0]
# SUNDAY OA_DAMPER [0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0]
# WEEKDAY INFILTRATION [1	1	1	1	1	1	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	1	1]
# SATURDAY INFILTRATION [1	1	1	1	1	1	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	0.25	1	1	1	1	1	1]
# SUNDAYINFILTRATION [1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1]

wCSP <-   c(80,80,80,80,80,78,77,75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,80,80)
satCSP <- c(80,80,80,80,80,78,77,75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,80,80)
sunCSP <- c(80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80)

wOCC <-   c(0,0,0,0,0,0,0.1,0.2,0.95,0.95,0.95,0.95,0.5,0.95,0.95,0.95,0.95,0.3,0.1,0.1,0.1,0.1,0.05,0.05)
satOCC <- c(0,0,0,0,0,0,0.1,0.1,0.3,0.3,0.3,0.3,0.1,0.1,0.1,0.1,0.1,0.05,0.05,0,0,0,0,0)
sunOCC <- c(0,0,0,0,0,0,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0,0,0,0,0,0)

wOADAM <-   c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0)
satOADAM <- c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0)
sunOADAM <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

wINFIL <-   c(1,1,1,1,1,1,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,1,1)
satINFIL <- c(1,1,1,1,1,1,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,1,1,1,1,1,1)
sunINFIL <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

### Preparing Training data ######
Months <- month(Training.Data$DATE)
Day.Of.Week <- wday(Training.Data$DATE)
Day.Of.Month <- mday(Training.Data$DATE)
Day.Of.Year <- yday(Training.Data$DATE)
DaySavingsInd <- which(Training.Data$DATE >= as.Date("2012-03-11") & 
                              Training.Data$DATE < as.Date("2012-11-05"))
DS <-numeric(nrow(Training.Data))
DS[DaySavingsInd] <- 1
Hour <- Training.Data$HOUR
D.B.Temp <- Training.Data$DBT
D.P.Temp <- Training.Data$DPT
R.H <- Training.Data$RH
G.H.R <- Training.Data$GHR
D.N.R <- Training.Data$DNR
D.H.R <- Training.Data$DHR
DBTLag1 <- Training.Data$DBTLag1
DPTLag1 <- Training.Data$DPTLag1
GHRLag1 <- Training.Data$GHRLag1
DNRLag1 <- Training.Data$DNRLag1
DHRLag1 <- Training.Data$DHRLag1
Energy <- Training.Data$CoolingEnergy
TrainingSet <- as.data.frame(cbind(Months,Day.Of.Week,Day.Of.Month,Day.Of.Year,
                                   Hour,D.B.Temp,D.P.Temp,R.H,G.H.R,D.N.R,D.H.R,
                                   DS,DBTLag1,DPTLag1,GHRLag1,DNRLag1,DHRLag1))

for (i in (1:nrow(TrainingSet))) {
  ind = TrainingSet$Hour[i]
  if (TrainingSet$Day.Of.Week[i] == 1) {
    TrainingSet$CSP[i] <- sunCSP[ind]
    TrainingSet$OCC[i] <- sunOCC[ind]
    TrainingSet$DAMP[i] <- sunOADAM[ind]
    TrainingSet$INFIL[i] <- sunINFIL[ind]
  }else if (TrainingSet$Day.Of.Week[i] == 7) {
    TrainingSet$CSP[i] <- satCSP[ind]
    TrainingSet$OCC[i] <- satOCC[ind]
    TrainingSet$DAMP[i] <- satOADAM[ind]
    TrainingSet$INFIL[i] <- satINFIL[ind]
  }else {
    TrainingSet$CSP[i] <- wCSP[ind]
    TrainingSet$OCC[i] <- wOCC[ind]
    TrainingSet$DAMP[i] <- wOADAM[ind]
    TrainingSet$INFIL[i] <- wINFIL[ind]
  }
}
TrainingSet$Energy <- Energy
##### Preparing Testing data #####
Months <- month(Testing.Data$DATE)
Day.Of.Week <- wday(Testing.Data$DATE)
Day.Of.Month <- mday(Testing.Data$DATE)
Day.Of.Year <- yday(Testing.Data$DATE)
DaySavingsInd <- which(Testing.Data$DATE >= as.Date("2012-03-11") & 
                              Testing.Data$DATE < as.Date("2012-11-05"))
DS <-numeric(nrow(Testing.Data))
DS[DaySavingsInd] <- 1
Hour <- Testing.Data$HOUR
D.B.Temp <- Testing.Data$DBT
D.P.Temp <- Testing.Data$DPT
R.H <- Testing.Data$RH
G.H.R <- Testing.Data$GHR
D.N.R <- Testing.Data$DNR
D.H.R <- Testing.Data$DHR
DBTLag1 <- Testing.Data$DBTLag1
DPTLag1 <- Testing.Data$DPTLag1
GHRLag1 <- Testing.Data$GHRLag1
DNRLag1 <- Testing.Data$DNRLag1
DHRLag1 <- Testing.Data$DHRLag1
TestingFeatures <- as.data.frame(cbind(Months,Day.Of.Week,Day.Of.Month,Day.Of.Year,
                                       Hour,D.B.Temp,D.P.Temp,R.H,G.H.R,D.N.R,D.H.R,
                                       DS,DBTLag1,DPTLag1,GHRLag1,DNRLag1,DHRLag1))

for (j in (1:nrow(TestingFeatures))) {
  ind = TestingFeatures$Hour[j]
  if (TestingFeatures$Day.Of.Week[j] == 1) {
    TestingFeatures$CSP[j] <- sunCSP[ind]
    TestingFeatures$OCC[j] <- sunOCC[ind]
    TestingFeatures$DAMP[j] <- sunOADAM[ind]
    TestingFeatures$INFIL[j] <- sunINFIL[ind]
  }else if (TestingFeatures$Day.Of.Week[j] == 7) {
    TestingFeatures$CSP[j] <- satCSP[ind]
    TestingFeatures$OCC[j] <- satOCC[ind]
    TestingFeatures$DAMP[j] <- satOADAM[ind]
    TestingFeatures$INFIL[j] <- satINFIL[ind]
  }else {
    TestingFeatures$CSP[j] <- wCSP[ind]
    TestingFeatures$OCC[j] <- wOCC[ind]
    TestingFeatures$DAMP[j] <- wOADAM[ind]
    TestingFeatures$INFIL[j] <- wINFIL[ind]
  }
}

TestingTarget <- Testing.Data$CoolingEnergy
### EDA ####
# ggplot(TrainingSet,aes(x=Months,y=Energy)) + geom_boxplot(aes(fill=factor(Months)))
# ggplot(TrainingSet,aes(x=Hour,y=Energy)) + geom_boxplot(aes(fill=factor(Hour)))
# ggplot(TrainingSet,aes(x=Day.Of.Week,y=Energy)) + geom_boxplot(aes(fill=factor(Day.Of.Week)))
### EDA ####
Train.Temp <- TrainingSet[1:(nrow(TrainingSet)-8760),]
TrainingFeatures <- Train.Temp[,-ncol(Train.Temp)]
TrainingTarget <-Train.Temp[,ncol(Train.Temp)]

Val.Temp <- TrainingSet[(nrow(TrainingSet)-8759):nrow(TrainingSet),]
ValFeatures <- Val.Temp[,-ncol(Train.Temp)]
ValTarget <-Val.Temp[,ncol(Train.Temp)]

CombinedFeatures <- rbind(TrainingFeatures,ValFeatures)
CombinedTargets <- c(TrainingTarget,ValTarget)

#### Preparing data for XGBoost matrix format ####
dtrain <- xgb.DMatrix(data=as.matrix(TrainingFeatures), label=TrainingTarget)
dval <- xgb.DMatrix(data=as.matrix(ValFeatures), label=ValTarget)
dcomb <- xgb.DMatrix(data=as.matrix(CombinedFeatures), label=CombinedTargets)
dtest <- xgb.DMatrix(data=as.matrix(TestingFeatures), label=TestingTarget)
watchlist <- list(train=dtrain, test=dval)

Temp.Frame <- data.frame('ETA' = double(),'GAMMA' = double(),
                         'MAXDEPTH' = integer(), 'COLUMNS' = double(),
                         'ROUNDS' = integer(), 'EARLY_STOP' = integer(),
                         'RMSE' = double())
niters <- 0
## Random Search ###
while (niters < 1000) { # Increase iteration if more exploration is desirable
  ETA <- runif(1,0.001,0.3)
  GAMMA <- runif(1,0.1,1)
  MAXDEPTH <- sample(6:20,1)
  COLUMNS <- runif(1,0.1,1)
  ROUNDS <- ifelse(ETA<0.1,sample(1000:5000,1),sample(100:999,1))
  EARLY_STOP <- ifelse(ETA<0.1,round(0.07*ROUNDS),round(0.05*ROUNDS))
  model1 <- xgb.train(data = dtrain, eta=ETA, gamma = GAMMA, max.depth=MAXDEPTH, 
                      colsample_bytree = COLUMNS, nrounds = ROUNDS,
                      early_stopping_rounds = EARLY_STOP,
                      nthread=4, eval_metric = list("rmse"),seed = 0,
                      maximize=FALSE,verbose=1,watchlist=watchlist,
                      objective="reg:linear")
  preds <- predict(model1,dval)
  RMSE <- sqrt(mean((preds - ValTarget)^2))
  Temp.Frame <- rbind(Temp.Frame,cbind(ETA,GAMMA,MAXDEPTH,COLUMNS,ROUNDS,EARLY_STOP,RMSE))
  niters <- niters + 1
}
index <- which(Temp.Frame$RMSE == min(Temp.Frame$RMSE))
choices <- Temp.Frame[index,]
best <- filter(choices,choices$COLUMNS == min(choices$COLUMNS)) # Only if more than one choice found
best.eta <- best$ETA
best.gamma <- best$GAMMA
best.depth <- best$MAXDEPTH
best.col <- best$COLUMNS
best.rounds <- best$ROUNDS
best.earlystop <- best$EARLY_STOP
bst <- xgb.train(data = dcomb, eta=best.eta, gamma = best.gamma, 
                 max.depth=best.depth, colsample_bytree = best.col, 
                 nrounds = best.rounds, 
                 early_stopping_rounds = best.earlystop,
                 nthread=4, eval_metric = list("rmse"),
                 seed = 0, maximize=FALSE,verbose=1,watchlist=list(train=dcomb),
                 objective="reg:linear")
########## Evaluating the trained model on the training data ##################
Predicted.Training <- predict(bst,dcomb)
results.train <- as.data.frame(cbind(Predicted.Training,CombinedTargets))

######### Predicting on the test dataset ##### 
Predicted.Testing <- predict(bst,dtest)
Predicted.Testing <- ifelse(Predicted.Testing<1,0,Predicted.Testing)
result.test <- as.data.frame(cbind(Predicted.Testing,TestingTarget,Months,Day.Of.Week,Hour,D.B.Temp))

#### Analysis of model testing #####
in.rmse <- sqrt(mean((Predicted.Training - CombinedTargets)^2))
print(paste("In sample error: ",in.rmse,"kWh"))
final.rmse <- sqrt(mean((Predicted.Testing - TestingTarget)^2))
print(paste("Out sample error: ",round(final.rmse,3),"kWh"))
Yhat <- scale(Predicted.Testing, center = TRUE, scale = TRUE)
Yactual <- scale(TestingTarget, center = TRUE, scale = TRUE)
Scaled.Residuals <- Yhat - Yactual
RMDN_RMSE <- 100 * (sqrt(mean((Yhat - Yactual)^2)))/(max(Yactual) - min(Yactual))
print(paste("Scaled normalized error (RMDN_RMSE) is:",round(RMDN_RMSE,3),"%"))
Total.Error <- sum(result.test$TestingTarget) - sum(result.test$Predicted.Testing)
print(paste("Approximate Cost of predictive difference: ",round(abs(Total.Error)*0.12),"$"))

time.elapsed <- proc.time() - ptm
### Export Results #####
# ExportData <- as.data.frame(cbind(TestingFeatures,TestingTarget,Predicted.Testing))
# write.csv(ExportData,"ExportedResults_CHI_MED_EM_XGB.csv")
###########
importance_matrix <- xgb.importance(colnames(CombinedFeatures),model = bst)
gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = TRUE)
print(gg + ggplot2::ylab("Importance"))

