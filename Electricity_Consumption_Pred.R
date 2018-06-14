# BHATIA PREDICTIVE MODELLING FINAL 

#
#Data Preprocessing--------------------------------------------------------------------------------------------------------------------------
#

df <- read.csv("final.csv")

#removing the final-weights as we are not going to use the weights for modelling 
df <- df[,-c(851:1048)]
#removing the public ID as it is unnecessary for the data frame
df <- df[,-1]
#choosing those rows from the pacific region - sicne that is the region whose electricity consumption is to be analysed 
df <- df[which(df$CENDIV==9),]

#removing all impute variables
df <- df[,-c(445:849)]

#removing most YES/NO variables
df <- df[,-c(225:421)]
df <- df[,-c(152:215)] #yes no variables for 

#variables to remove 
#REGION and CENDIV because the donot change
#SQFTC, YRCONC  repeating 
#MONCON , RENADD
#Unnecessary to have % of each act ACT1PCT,ACT2PCT,ACT3PCT
# FEDFAC  
#removing from FLUORP to OTLTP as they are percentages of buidling lit by certain type of light
variables_to_remove <- c(1,2,7,24,25,27,46,47,48,161:167)
df <- df[,-variables_to_remove]

#setting a variable electric for total electricity consumption 
df$ELECTRIC <- df$ELWTBTU + df$ELHTBTU + df$ELCLBTU
#remove those rows which have NA value for target variable as they  will not contribute to the model 
df <- df[,-c(213,214,216)]
df <- df[complete.cases(df$ELECTRIC),]
#removing more imputed variables 
df <- df[,-c(193:202)]

# "SQFTC", "EQGLSS","ELEVTR","ESCLTR","YRCONC","RENOV"-"ONEACT","PBAPLUS"
remove_values <- match(c("SQFTC","ELEVTR","ESCLTR","PBAPLUS"),names(df))
df <- df[,-remove_values]
df <- df[,-c(17,32)]
remove_values <- match(c("PBSEAT","EDSEAT","FDSEAT","FDSEAT","HCBED","NRSBED","LODGRM","COURT",
                         "FACIL","FEDFAC","GOVOWN","COOK","MANU","CAPGEN","GENR","ANYEGY","ELUSED",
                         "NGUSED","FKUSED"),names(df))


df <- df[,-remove_values]
df <- df[,-c(185:199)]

remove_values <- match(c("PUBCLIM","OWNOCC","OWNOPR","OWNPPR","NWNPPR","NWNOPR","WHOPPR","MONUSE","LODOCCP",
                         "PRUSED","STUSED","CWUSED","WOUSED",
                         "COUSED","SOUSED","OTUSED"),names(df))
df <- df[,-remove_values]
remove_values <- match(c("FREESTN","ATTIC","RENCOS","RENRDC","RENINT","RENWLL","RENWIN","RENHVC","RENLGT",
                         "RENPLB","RENELC","RENINS","RENSAF","RENSTR", "CUBE","CUBEC","CUBELOC",
                         "OWNTYPE","GOVTYP","FACACT","MANIND","PLANT","FACDST","FACDCW","BLDPLT",
                         "NOCC","NOCCAT","OCCUPYP","WKHRS","WKHRSC","NWKER","NWKERC"),names(df))
df <- df[,-remove_values]
remove_values <- match(c("DRYCL","VACANT","RWSEAT","FACDHW","WOHT1","COHT1","SOHT1","OTHT1","NGHT2",
                         "NGHT2","FKHT2","PRHT2","STHT2","HWHT2","WOHT1","COHT1","SOHT1","OTHT1",
                         "ELHT2","NGHT2","FKHT2","PRHT2"),names(df))
df <- df[,-remove_values]
remove_values <- match(c("HWHT1","WOHT2","COHT2","SOHT2","OTHT2","HEATP","HTLS50","PKGHT","STHW",
                         "NGHT1"  ,  "FKHT1",    "PRHT1",    "STHT1",
                         "PKGHP"  ,  "BOILP",    "STHWP",    "HTPHP",    "SLFCNP",   "OTHTP",
                         "RCACP" ,    "CHILP",    "CHWTP"  ,  "HTPCP",    "ACWNWP",   "EVAPP",    "OTCLP",
                         "EMCSLT"  ,"SCHED"  ,"OCSN" ,    "DIM",      "DAYHARV",  "TRIM",     "PLGCTRL" , "DRLGHT", 
                         "LTEXPC" ,  "PKLT",     "WINTYP",   "TINT",     "REFL",  "AWN" , "SKYLT", "DAYLTP","MFUSED",
                         "MFBTU","MFEXP","ELBTU","ELCNS","NGCNS"  ,  "NGBTU"  ,  "NGEXP"  ,  "FKCNS"  ,  "FKBTU"  ,  "FKEXP",
                         "DHUSED" ,  "DHHT1"  ,  "DHHT2"  ,  "DHCOOL",   "DHCOOK"  ,
                         "DHWATR"   ,"DHMANU"   ,"DHOTH"  ,  "DHCNS"   , "DHBTU" ,   "DHEXP",
                         "MFHTBTU" , "MFCLBTU"  ,"MFVNBTU"  ,"MFWTBTU"  ,"MFLTBTU", 
                          "MFCKBTU"  ,"MFRFBTU" , "MFOFBTU", "ELVNBTU" , "ELLTBTU"  ,"ELCKBTU",  "ELRFBTU",  "ELOFBTU"  ,"ELPCBTU" 
                         ),names(df))
df <- df[,-remove_values]
#removing the variables with too many NA values 
remove_values <- match(c("SUNGLS","BASEMNT","NELVTR","NESLTR","ACT1","ACT2","ACT3","FACELC","FKTYPE","FURNP","PKGCP","DHOTBTU","MFPCBTU","MFOTBTU"),names(df))
df <- df[,-remove_values]

final.df <- df[complete.cases(df),]

#converting to factor 
 final.df$RFCNS <-  as.factor(final.df$RFCNS)   
 final.df$RFCOOL <- as.factor(final.df$RFCOOL)
 final.df$RFTILT <-as.factor(final.df$ RFTILT)
 final.df$BLDSHP <- as.factor(final.df$ BLDSHP)
 final.df$GLSSPC <- as.factor(final.df$ GLSSPC)
 final.df$EQGLSS <- as.factor(final.df$ EQGLSS) 
 final.df$RENOV <- as.factor(final.df$ RENOV)
 final.df$RENRFF <- as.factor(final.df$ RENRFF  )
 final.df$ONEACT <- as.factor(final.df$ ONEACT  )
 final.df$OPEN24 <- as.factor(final.df$ OPEN24  )
 final.df$OPNMF <- as.factor(final.df$ OPNMF   )
 final.df$OPNWE <- as.factor(final.df$ OPNWE   )
 final.df$HT1 <- as.factor(final.df$ HT1     )
 final.df$HT2 <- as.factor(final.df$ HT2     )
final.df$COOL <- as.factor(final.df$ COOL    )
 final.df$WATR <- as.factor(final.df$ WATR    )
 final.df$HWUSED <- as.factor(final.df$ HWUSED  )
 final.df$ELHT1 <- as.factor(final.df$ ELHT1  )
 final.df$FURNAC <- as.factor(final.df$ FURNAC )
 final.df$BOILER <- as.factor(final.df$ BOILER  )
 final.df$HTPMPH <- as.factor(final.df$ HTPMPH  )
 final.df$SLFCON <- as.factor(final.df$ SLFCON  )
 final.df$OTHTEQ <- as.factor(final.df$ OTHTEQ )
 final.df$PBA <- as.factor(final.df$PBA)

str(final.df)
summary(final.df$ELECTRIC)
#removing HT1 and COOL because they have only 1 level 
remove_values <- match(c("COOL","HT1"),names(final.df))
final.df <- final.df[,-remove_values]

#removing 90 % of the outlier data 
quantile(final.df$ELECTRIC, c(0.9,0.95,0.98))
final.df <- final.df[which(final.df$ELECTRIC<=3485151),]
#moreover removing those buildings with 0 electricity usage because they are either abandoned or not contributing 
#to the predictive power of our model 
final.df <- final.df[which(final.df$ELECTRIC != 0),]

#T_log = log()
library(rcompanion)
boxplot(final.df$ELECTRIC)

#Exploratory data analysis-------------------------------------------------------------------------------------------------------------
final.df$ELECTRIC <- log(final.df$ELECTRIC)

#PCA-----------------------------------------------------------------------------------------------------------------------------------
#setting parameters before applying models
k <- 10
folds <- cut(seq(1,nrow(final.df)),breaks=k,labels=FALSE)

error.model1.OS <- c(1:k)
error.model1.IS <- c(1:k)

error.model2.OS <- c(1:k)
error.model2.IS <- c(1:k)

error.model3.OS <- c(1:k)
error.model3.IS <- c(1:k)

error.model4.OS <- c(1:k)
error.model4.IS <- c(1:k)

error.model5.OS <- c(1:k)
error.model5.IS <- c(1:k)

error.model6.OS <- c(1:k)
error.model6.IS <- c(1:k)

error.model7.OS <- c(1:k)
error.model7.IS <- c(1:k)

error.model8.OS <- c(1:k)
error.model8.IS <- c(1:k)

rmse.model5.OS <- c(1:k)

library(ModelMetrics)
library(rpart)
library(rpart.plot)

#Linear Model=------------------------------------------------------------------------------------------------------------------------
for(i in 1:k){
  
  set.seed(100)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]

  lm.model3 <- lm(ELECTRIC ~ ELEXP + HWUSED + OPEN24 + NFLOOR + SQFT, data = trainData)
  model3.predicted <- predict(lm.model3, newdata = testData)
  
  model3.pred.OS <- predict(lm.model3,newdata = testData)
  model3.pred.IS <- predict(lm.model3,newdata = trainData)
  
  error.model3.OS[i] <- rmse(actual = testData$ELECTRIC,predicted = model3.pred.OS)
  error.model3.IS[i] <- rmse(actual = trainData$ELECTRIC,predicted = model3.pred.IS)
}  
#CART----------------------------------------------------------------------------------------------------------------------------------
for(i in 1:k){
  
  set.seed(100)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ] 
  
  cart.model1 <- rpart(ELECTRIC ~ . , data=trainData)
  model1.predicted <- predict(cart.model1,newdata=testData)
  
  model1.pred.OS <- predict(cart.model1,newdata = testData)
  model1.pred.IS <- predict(cart.model1,newdata = trainData)
  
  error.model1.OS[i] <- rmse(actual = testData$ELECTRIC,predicted = model1.pred.OS)
  error.model1.IS[i] <- rmse(actual = trainData$ELECTRIC,predicted = model1.pred.IS)
}

#RANDOM FORESTS-------------------------------------------------------------------------------------------------------------------------
for(i in 1:k){
  
  set.seed(100)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]
  
  library(randomForest)
  rf.model2 <- randomForest(ELECTRIC ~ .,data=trainData)
  varImpPlot(rf.model2)
  rf.model2 <- randomForest(ELECTRIC ~ ELEXP + SQFT  + CDD65 + HDD65 + GLSSPC + PBA + MAINCL + NFLOOR + MAINHT + 
                              FLCEILHT, data = trainData,ntree = 5000)
  
  model2.pred.OS <- predict(rf.model2,newdata = testData)
  model2.pred.IS <- predict(rf.model2,newdata = trainData)
  
  error.model2.OS[i] <- rmse(actual = testData$ELECTRIC,predicted = model2.pred.OS)
  error.model2.IS[i] <- rmse(actual = trainData$ELECTRIC,predicted = model2.pred.IS)
}

#GAM--------------------------------------------------------------------------------------------------------------------------------------
final.df$RFCNS <- as.numeric(final.df$RFCNS)
for(i in 1:k){
  
  set.seed(100)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]
  library(gam)
  gam.model4 <- gam.object <- gam(ELECTRIC ~ ., family = gaussian(), data=trainData)
  
  model4.pred.OS <- predict(gam.model4,newdata=testData)
  model4.pred.IS <- predict(gam.model4,newdata = trainData)
  
  error.model4.OS[i] <- rmse(actual = testData$ELECTRIC,predicted = model4.pred.OS)
  error.model4.IS[i] <- rmse(actual = trainData$ELECTRIC,predicted = model4.pred.IS)
}

#MARS----------------------------------------------------------------------------------------------------------------------------------
for(i in 1:k){
  set.seed(100)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]
 
  library(earth)
  mars.model5 <- earth(ELECTRIC ~ . , data = trainData, degree = 3, penalty = 1) 
  mars.model5.unpruned <- earth(ELECTRIC ~ . , data = trainData, degree = 3, penalty = 1, pmethod=c("none")) 
  model5.pred.OS <- predict(mars.model5,newdata=testData)
  model5.pred.IS <- predict(mars.model5,newdata = trainData)
  
  model5.unpruned.pred.OS <- predict(mars.model5.unpruned,newdata=testData)
  model5.unpruned.pred.IS <- predict(mars.model5.unpruned,newdata = trainData)
  plot(evimp(mars.model5))
  plot(evimp(mars.model5.unpruned))
  
  error.model5.OS[i] <- rmse(actual = testData$ELECTRIC,predicted = model5.pred.OS)
  error.model5.IS[i] <- rmse(actual = trainData$ELECTRIC,predicted = model5.pred.IS)
  
}

#BART----------------------------------------------------------------------------------------------------------------------------------
library(rJava)
library(bartMachine)

for(i in 1:k){
  paste0(c("BART Iteration no : ",i))
  set.seed(100)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]
  
  df.train.covariates <- trainData[,-ncol(trainData)]
  df.train.response <- trainData[,ncol(trainData)]
  df.test.covariates <- testData[,-ncol(trainData)]
  
  bart.model6 <- bartMachine(X=df.train.covariates,y=df.train.response)
  
  model6.pred.OS <- predict(bart.model6,df.test.covariates)
  model6.pred.IS <-predict(bart.model6,df.train.covariates)
  
  error.model6.OS[i] <- rmse(actual = testData$ELECTRIC, predicted= model6.pred.OS)
  error.model6.IS[i] <- rmse(actual = testData$ELECTRIC, predicted= model6.pred.IS)
  
  
  
}


#SVM-----------------------------------------------------------------------------------------------------------------------------------
library(e1071)
for(i in 1:k){
  paste0(c("SVM Iteration no : ",i))
  set.seed(100)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]
  
  svm.model7 <- svm(ELECTRIC ~ . , trainData)
  model7.pred.OS <- predict(svm.model7, testData)
  model7.pred.IS <- predict(svm.model7, trainData)
  
  error.model7.OS[i] <- rmse(actual = testData$ELECTRIC, predicted= model7.pred.OS)
  error.model7.IS[i] <- rmse(actual = trainData$ELECTRIC, predicted= model7.pred.IS)
  
  points(testData$ELECTRIC, model7.pred.OS, col = "red", pch=4)
}
svm.rmse.IS <- mean(error.model7.IS)
svm.rmse.OS <- mean(error.model7.OS)


#Neural Networks-----------------------------------------------------------------------------------------------------------------------
library(nnet)
library(neuralnet)

set.seed(100)
for (i in 1:k){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  
  
  #we use the min max normalization to scale the data 
  scaled.df <- final.df
  #creating boolean variables for the NN 
  for (j in 1:ncol(final.df)){
    if(is.numeric(final.df[,j])==TRUE){
      max  <- max(final.df[,j])
      min <-  min(final.df[,j])
      scaled.col <- scale(final.df[,j], center = TRUE, scale = max - min)
      cbind(scaled.df, scaled.col)
    }
      
    else{
      print("not numeric")
      cbind(scaled.df,final.df[,j])
    }
  }
  #taking the trainng and test data from the scaled index 
  testData <- scaled.df[testIndexes, ]
  trainData <- scaled.df[-testIndexes, ]
  
  #fit the nueral network using the scaled data 
  #reference: https://www.youtube.com/watch?v=LTg-qP9iGFY
  
  variable <- colnames(scaled.df)
  predictor.variables <- variable[!variable%in%"ELECTRIC"]
  predictor.variables <- paste(predictor.variables,collapse = "+")
  form <- as.formula(paste("ELECTRIC~",predictor.variables,collapse = "+"))
  
  model8.NN <- neuralnet(ELECTRIC ~ SQFT + ELEXP  ,hidden = 10 , rep = 3, linear.output = T,  data =  trainData, stepmax = 1e+06)
  
  #predictions using NN
  model8.pred.OS <- compute(model8.NN, trainData)
  model8.pred.IS <- compute(model8.NN, trainData)$net.result
  
  error.model8.OS[i] <- rmse(actual = testData$ELECTRIC, predicted= model8.pred.OS) 
  error.model8.IS[i] <- rmse(actual = trainData$ELECTRIC, predicted= model8.pred.IS)

}


#Model interpretations---------------------------------------------------------------------------------------------------------------
errors.IS <- c(mean(error.model1.IS),mean(error.model2.IS)
                     ,mean(error.model3.IS),mean(error.model4.IS),mean(error.model5.IS),
                     mean(error.model6.IS),mean(error.model7.IS))

errors.OS <- c(mean(error.model1.OS),mean(error.model2.OS)
               ,mean(error.model3.OS),mean(error.model4.OS),mean(error.model5.OS),
               mean(error.model6.OS),mean(error.model7.OS))

rmse.table <- data.frame()
rmse.table <- cbind(errors.IS,errors.OS)

rownames(rmse.table) <- c("cart","RF","linear","GAM","MARS","BART","SVM")
rmse.table
