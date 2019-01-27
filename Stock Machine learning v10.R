#http://www.tradinggeeks.net/2014/07/technical-analysis-with-r/
#followed by ML: https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.FPGodtU

library(gtrendsR)
library(ggplot2)
library(reshape2)
library(xts)
library(zoo)
library(quantmod) # data, plotting, quant modelling
library(TTR)
library(rattle) #https://rstudio-pubs-static.s3.amazonaws.com/64455_df98186f15a64e0ba37177de8b4191fa.html
library(rpart.plot)
library(rpart)

###########parameters##########
initDate <- Sys.Date()-365 #last x days
finDate <- Sys.Date()
Ticker1<- "BTCUSD=X" #^GDAXI #^FCHI-PXI in IG ì" #from yahoo finance
Nshifts<-1 #number o f days for future price shift
searchbestMethod<-FALSE #if FALSE: my_method is:
my_method<-'lvq' #OR 'knn'  - 'ctree2' - 'ctree' - 'rpart' -'lvq' 
                    #- 'rf' (random forest)
                    #- 'gbm'(boosting) 
                    #- 'treebag' (bagging)
myfactors<-c("pctB", "HOnL", "HCOnOL","macd","ADX","signal",  "fastK", "fastD", "slowD", "Close.1","cci", "atr")


#all parameters to try:
#("pctB","COOnHL", "HOnL", "HCOnOL","macd","ADX","signal",  "fastK", "fastD", "slowD", "Close.1","cci", "atr")


#rpart BEST RESULTS:
#( "HCOnOL","macd","signal")   --->51% on ^GDAXI with 'rpart'

################################

allData<-NULL

#####################################
## EXTRACTION OF FINANCIAL DATA   ##
#####################################
Sys.setenv(TZ = "GMT")
financeData1<-NULL
financeData1 = getSymbols(Ticker1, 
                         src = "yahoo", 
                         from=initDate, to=finDate,
                         auto.assign=FALSE)
dc_hlc <- HLC(financeData1)
dc_hlc <-na.omit(dc_hlc)
colnames(dc_hlc) = c("High","Low","Close")

#FINANCIAL ANALISYS PARAMETERS
bb20 = BBands(dc_hlc, n = 10, sd = 2, maType=EMA)
adx = ADX(dc_hlc, n = 10)
rsi14 = RSI(dc_hlc[,"Close"], n=14)
sma20 = SMA(dc_hlc[,"Close"],n=20)
macd = MACD(dc_hlc[,"Close"], nFast=12, nSlow=26, nSig=9, maType=SMA, percent = TRUE)
stoc = stoch(dc_hlc[,"Close"], nFastK = 14, nFastD = 3, nSlowD = 3, maType=SMA)
Aroon = aroon(dc_hlc[,"Close"], n = 10)
Atr=ATR(dc_hlc, n = 14)
Cci=CCI(dc_hlc, n = 10, c = 0.015)
vol <- chaikinVolatility(dc_hlc)
DCC<-DonchianChannel(dc_hlc[,"Close"], n = 10, include.lag = FALSE)
roc <- ROC(dc_hlc[,"Close"])
mom <- momentum(dc_hlc[,"Close"])

StartDate<-as.Date(index(dc_hlc))

#merging all data in single file for strategy test
dataPlusBB = data.frame(StartDate,dc_hlc,bb20,roc, adx)
rownames(dataPlusBB) <- NULL

head(dataPlusBB)

#plot(StartDate,dataPlusBB$Close.1)

plot(StartDate,dataPlusBB$Close)

lines(StartDate,dataPlusBB$Close, col = "red")
lines(StartDate,dataPlusBB$up, col = "purple")
lines(StartDate,dataPlusBB$dn, col = "brown")
lines(StartDate,dataPlusBB$mavg, col = "blue")


allData = data.frame(StartDate,dc_hlc,sma20,bb20,rsi14,macd, stoc, Cci, Atr, roc, adx)
head(allData)

# ML INPUT PARAMETERS DEFINITION #####
######################################
# Close_ price / SMA ratio: CloseOnSMA
allData$CloseOnSMA=allData$Close/allData$SMA

# (C-C1) / (H-L) ratio: COOnHL
mylen<-length(allData$Close)
allData$DeltCO[2:mylen]=allData$Close[2:mylen]-allData$Close[1:mylen-1]
allData$DeltHL=allData$High-allData$Low
allData$COOnHL=allData$DeltCO/allData$DeltHL

# (H-C) / (C1-L) ratio: HCOnCL
allData$DeltOL[2:mylen]=allData$Close[1:mylen-1]-allData$Low[2:mylen]
allData$DeltHC=allData$High-allData$Close
allData$HCOnOL=allData$DeltHC/allData$DeltOL

# H/L ratio: HOnL
allData$HOnL=allData$High/allData$Low

head(allData)

## ML OUPUT DEFINITION ##################
####################################
#shifted price function
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

Future_Close=shift(allData$Close,Nshifts)

allData<- data.frame(allData,Future_Close)

#extracting future price ratio
datalen=length(Future_Close)

#Return
allData$Return_Futur[2:datalen]=allData$Future_Close[2:datalen]/allData$Future_Close[1:datalen-1]

# define Observed variation:
allData$Obs_var <- ifelse(allData$Return_Futur > 1.0005, "UP",
                      ifelse(allData$Return_Futur < 0.9995, "DOWN","STABLE"))



#we want to see if there is any correlation between two variables.

# Load in `ggvis`
#library(ggvis)

#  scatter plot
#allData %>% ggvis(~COOnHL, ~CloseOnSMA, fill = ~Obs_var) %>% layer_points()

#allData %>% ggvis(~pctB, ~CloseOnSMA, fill = ~Obs_var) %>% layer_points()

#allData %>% ggvis(~pctB, ~COOnHL, fill = ~Obs_var) %>% layer_points()


# Overall correlation 
cor(allData$pctB, allData$CloseOnSMA, use = "na.or.complete")

# Print UP level correlation matrix
cor(allData[allData$Obs_var=="UP",myfactors], use = "na.or.complete")
# Print DOWN level correlation matrix
cor(allData[allData$Obs_var=="DOWN",myfactors], use = "na.or.complete")


#Remove NAs
allData_all<-allData
allData<-allData[complete.cases(allData), ]

#Summary(allData)

#dividing Obs
# Division of `Obs_var`
table(allData$Obs_var) 

tail(allData_all)


#####################################

###Machine Learning in R with caret############

###############################################
#any(grepl("caret", installed.packages()))
library(caret)
# Create index to split based on labels  
index <- createDataPartition(allData$Obs_var, p=0.75, list=FALSE)
# Subset training set with index
allData.training <- allData[index,]
head(allData.training)
# Subset test set with index
allData.test <- allData[-index,]
head(allData.test)
# Train a model
my_model <-NULL


if (searchbestMethod==TRUE) {
  
  #Tree rpart
  set.seed(123) 
  Grid <- expand.grid(cp=seq(0, 0.05, 0.005))
  Mod0 <- train(allData.training[, myfactors],
                    allData.training$Obs_var, 
                    method = 'rpart', 
                    maximize=FALSE, tuneGrid = Grid)
  pred0 <- predict.train(object=Mod0,allData.test[,myfactors], type="raw")
  cm0<-confusionMatrix(pred0,allData.test$Obs_var)
  
  #Random forest
  Mod1<-train(allData.training[, myfactors],
                  allData.training$Obs_var,
                  method="rf",
                  trControl=trainControl(method="cv",number=5),
                  prox=TRUE,allowParallel=TRUE)
  pred1 <- predict.train(object=Mod1,allData.test[,myfactors], type="raw")
  cm1<-confusionMatrix(pred1,allData.test$Obs_var)
  
  #Boosting
  set.seed(2)
  Mod2 <- train(allData.training[, myfactors],
                    allData.training$Obs_var, 
                    method = "gbm", 
                    verbose = F, 
                    trControl = trainControl(method = "cv", number = 3))
  pred2 <- predict.train(object=Mod2,allData.test[,myfactors], type="raw")
  cm2<-confusionMatrix(pred2,allData.test$Obs_var)  
  
  
  #Bagging
  
  Mod3 <- train(allData.training[, myfactors],
                    allData.training$Obs_var, 
                    method="treebag")
  pred3 <- predict.train(object=Mod3,allData.test[,myfactors], type="raw")
  cm3<-confusionMatrix(pred3,allData.test$Obs_var)    
  
  ### CONCLUSION ###
  re <- data.frame(Tree=cm0$overall[1], 
                   rf=cm1$overall[1], 
                   boosting=cm2$overall[1],
                   bagging=cm3$overall[1])
  library(knitr)
  re
  
  
} else {


if (my_method=='knn')  {
my_model <- train(allData.training[, myfactors],
                   allData.training$Obs_var, 
                   method=my_method, 
                   preProcess=c("center", "scale"))
}

if (my_method=='rpart')  { 
  set.seed(123) 
Grid <- expand.grid(cp=seq(0, 0.05, 0.005))
my_model <- train(allData.training[, myfactors],
                  allData.training$Obs_var, 
                  method = 'rpart', 
                  maximize=FALSE, tuneGrid = Grid)
fancyRpartPlot(my_model$finalModel) #https://rstudio-pubs-static.s3.amazonaws.com/64455_df98186f15a64e0ba37177de8b4191fa.html
}

if (my_method=='ctree2')  {
  fitControl <- trainControl(method = 'cv', number=6,summaryFunction=defaultSummary)
  set.seed(123)
  Grid <- expand.grid(maxdepth = seq(15, 50,5))
  my_model <- train(allData.training[, myfactors],
                        allData.training$Obs_var, 
                        method = 'ctree2', trControl=fitControl)
}
if (my_method=='lvq')  {
  set.seed(7)
  # prepare training scheme
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  # train the model
  my_model <- train(allData.training[, myfactors],
                    allData.training$Obs_var,
                    method="lvq", trControl=control, 
                    preProcess="scale",tuneLength=5) 
}
if (my_method=='rf')  { #RANDOM FOREST
  #http://bigcomputing.blogspot.it/2014/10/an-example-of-using-random-forest-in.html
  my_model<-train(allData.training[, myfactors],
                  allData.training$Obs_var,
                  method="rf",
                  trControl=trainControl(method="cv",number=5),
                  prox=TRUE,allowParallel=TRUE)
  }
if (my_method=='gbm')  { #Boosting
  #http://bigcomputing.blogspot.it/2014/10/an-example-of-using-random-forest-in.html
  set.seed(2)
  my_model <- train(allData.training[, myfactors],
                    allData.training$Obs_var, 
                     method = "gbm", 
                     verbose = F, 
                     trControl = trainControl(method = "cv", number = 3))
}
if (my_method=='treebag')  { #Bagging
  #http://bigcomputing.blogspot.it/2014/10/an-example-of-using-random-forest-in.html
  my_model <- train(allData.training[, myfactors],
                    allData.training$Obs_var, 
                    method="treebag")
}

}
###
ctreeVarImp = NULL
ctreeVarImp = varImp(my_model)


# Predict values
predictions<-NULL
predictions<-predict.train(object=my_model,allData.test[,myfactors], type="raw")


#subset data to predict
tutti<-length(allData_all$StartDate)
allData_all.current <- allData_all[(tutti-10):tutti,]


# make a predictions on "new data" using the final model
final_predictions <- predict.train(object=my_model,
                                   allData_all.current[,myfactors])
allData_all.current$Pred_var<-final_predictions

my_model$finalModel
# Confusion matrix
cm<-confusionMatrix(predictions,allData.test$Obs_var)
cm
cm$overall
tail(allData_all.current)
#plot(my_model)
plot(ctreeVarImp)

