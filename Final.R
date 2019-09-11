library(car)
library(psych)
library(lmSupport)
library(reshape2)
library(outliers)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


set.seed(3)

df <- read.csv('DataProject.csv')
head(df)
data <- data[,-1]

data$Testcase = data$ï..TestCase
summary(df)
na.omit(df)#Remove any N/A

df <- unique(df) #Remove any duplication 
#44804.

summary(df$Status)

ggplot(df, aes(Sector, Patents)) +
  geom_bar(stat = "identity") + 
  labs(y = "Sector", x = "Patents")


df.cor.1 = cor(df[,c(9:18)])
corrplot(df.cor.1)

tab <- as.data.frame(data[,c(9:18)])
data <- cbind(data, Total.Investment = rowSums(tab)) #Calculate total investment

df$PPI <- with(df, Patents/(Total.Investment/100)) #PPI: Patents per 100 Investments

df<-df[!(df$Total.Investment==0),] # Remove zero investment
df<-df[!(df$Status=="Inactive"),] # Remove Inactive
df<-df[!(df$Status=="Not Available"),] # Remove zero investment
df<-df[!(df$Sector=="D3" & df$Industry=="C"),]

ggplot(data = melt(df), mapping = aes(x = value)) + 
  geom_histogram(bins=50) + facet_wrap(~variable, scales = 'free_x')

plot(df$Investment.Human.Capital, df$Patents)
abline(lm(df$Patents~df$Investment.Human.Capital))

hist(log(df$PPI))#Transform Log
pairs(df)
plot(df$SecARat)
df<-df[!(df$SecARat>200),] 

plot(df$Size)
df<-df[!(df$Size>30000),] 

plot(df$Investment.Human.Capital.B)
df<-df[!(df$Investment.Human.Capital.B>60),]


plot(df$Investment.Human.Capital.C)
df<-df[!(df$Investment.Human.Capital.C>200),]

plot(df$Investment.Human.Capital.D)

plot(df$Investment.Human.Capital.E)

plot(df$Investment.Human.Capital.F)

plot(df$Investment.Lab.Infrastructure)

plot(df$Investment.in.HR.Services)
df<-df[!(df$Investment.in.HR.Services>2500),]

plot(df$Investment.Building)
df<-df[!(df$Investment.Building>700),]

plot(df$PPI)
df<-df[!(df$PPI>2000),]


hist(df$Region)
df<-df[!(df$PPI>2000),]

cor(df$SecARat, df$PPI)


data$Quarter <- as.factor(data$Quarter)
data$Region <- as.factor(data$Region)

names(data)[9:18] =
  c("IHC.F","IHC.E","IHC.D","IHC.A","IHC.B","ILI","IHC.C","ICI","IHRS","IB")

df$IsZero <- (df$PPI == 0)
df$IsZero <- as.factor(df$IsZero)


library(mlbench)
library(caret)
library(caretEnsemble)


df$Sector <- car::recode(df$Sector, "c('B1', 'B5','B6','B11','B20')='B0';c('C10', 'C18', 'C19', 'C28','C34') = 'C0'")



m.pca <- prcomp(df[,c(9:18)], center = TRUE,scale. = TRUE)
summary(m.pca)

df <- cbind(df,m.pca$x[,1:7])

data$Sector <- car::recode(data$Sector, "c('B4','B12','B15','B16','B19','B22')='Bx';c('C1', 'C12', 'C14', 'C16','C21','C22','C23','C26','C3','C30','C37') = 'Cx'")
testdata <- predict(m.pca, newdata = data[,c(9:18)])
testdata <- as.data.frame(testdata)
data <- cbind(data,testdata)


mydata_numcols <- df[, sapply(df, is.numeric)]

mydata_faccols <- df[, sapply(df, is.factor)]

mydata_numcols <- as.data.frame(scale(mydata_numcols, center = TRUE, scale = TRUE))

mydata_numcols <- mydata_numcols[,-15]

depend <- as.data.frame(df[,"PPI"])

df <- as.data.frame(c(mydata_numcols,mydata_faccols,depend))

rm(mydata_numcols,mydata_faccols)

names(df)[29] ="PPI"

names(df)[28] ="IsZero"

f$Region <- car::recode(df$Region, "c('2','3','4','5','6','8','9','10','11','12','13','16','18','20,'21','22','23','24','25','26','27','28')='Bx';c('C1', 'C12', 'C14', 'C16','C21','C22','C23','C26','C3','C30','C37') = 'Cx'")

require(data.table)

write.csv(df,"data_cleaned.csv")


set.seed(3)

smp_size <- floor(0.8 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

df0 <- rbind(train,test)
train <- df[1:33443,]
test <- df[33444:41804,]
data <- df0[!(df0$Testcase==c(99999))&!(df0$Testcase==c(11111)),]
train <- df0[df0$Testcase==c(11111),]
test<- df0[df0$Testcase==c(99999),]

x <- data.frame(train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])
xtest <- data.frame(test[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])


y <- as.numeric(train[,"IsZero"])-1
ytest <- as.numeric(test[,"IsZero"])-1

train_data <- data.frame(train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","IsZero")])

new_tr <- model.matrix(~.+0,data = x)
new_ts <- model.matrix(~.+0,data = xtest)

dtrain <- xgb.DMatrix(data = new_tr,label = y)
dtest <- xgb.DMatrix(data = new_ts,label=ytest)

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1, scale_pos_weight = 0.5)

xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 81, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "error")

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

confusionMatrix (as.factor(xgbpred), as.factor(ytest))
importance <- xgb.importance(feature_names = NULL, model = xgb1)
head(importance)


library(randomForest)
rf <- randomForest( IsZero ~., data = train_data, mtry = 10, ntree=40, importance=TRUE)

##Logit Model

fit.logit <- glm(IsZero~., data = train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","IsZero")],family = "binomial")
logistic_probs_1 <- predict(fit.logit, test[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","IsZero")], type = "response")
logistic_pred_1  <- rep(FALSE, length(logistic_probs_1))
logistic_pred_1[logistic_probs_1>0.5] <- TRUE
confusionMatrix(as.factor(logistic_pred_1), test$IsZero)


library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology

svmfit <- svm(IsZero~., data = train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","IsZero")], kernel = "radial", gamma = 1, cost = 1)

svm_pred <- predict(svmfit, test[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","IsZero")])

confusionMatrix (svm_pred, test[,40])

tune.out <- tune(svm, IsZero~., data = train[,c(1,14,27:34,36:40)], kernel = "radial",
                 ranges = list(cost = c(0.1,1,10,100),
                               gamma = c(0.5,1,2,3,4)))

train1<-train[!(train$IsZero=="TRUE"),]
test1<-test[!(test$IsZero=="TRUE"),]

x1 <- data.frame(train1[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])
xtest1 <- data.frame(test1[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])


y1 <- train1[,"Patents"]
ytest1 <- test1[,"Patents"]


new_tr1 <- model.matrix(~.+0,data = x1)
new_ts1 <- model.matrix(~.+0,data = xtest1)

dtrain1 <- xgb.DMatrix(data = new_tr1,label = y1)
dtest1 <- xgb.DMatrix(data = new_ts1,label=ytest1)

params1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.1, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv1 <- xgb.cv(params = params1, data = dtrain1, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb2 <- xgb.train (params = params1, data = dtrain1, nrounds =36, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "error")

pred.xgb.1 <- predict(xgb2, new_ts1)

# results
caret::RMSE(pred.xgb.1, ytest1)


fit.lm <- lm(PPI~., data = train1[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","PPI")])
fit.lm.pred <- predict(fit.lm, test1[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","PPI")])
caret::RMSE(fit.lm.pred, ytest1)

fit.lm0 <- lm(Patents~., data = train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","Patents")])
fit.lm.pred0 <- predict(fit.lm0, test[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","Patents")])
caret::RMSE(fit.lm.pred0, ytest0)


library(glmnet)
# maintain the same folds across all models
fold_id <- sample(1:10, size = length(y1), replace=TRUE)

# search across a range of alphas
tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)

for(i in seq_along(tuning_grid$alpha)) {
  
  # fit CV model for each alpha value
  fit <- cv.glmnet(new_tr1, y1, alpha = tuning_grid$alpha[i], foldid = fold_id)
  
  # extract MSE and lambda values
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

tuning_grid
tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
  ggtitle("MSE ± one standard error")


elastic   <- cv.glmnet(new_tr1, y1, alpha = 0.1)
min(elastic$cvm)

elastic.pred <- predict(elastic, s = elastic$lambda.min, testa.test)
caret::RMSE(elastic.pred, ytest1)


xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse(xgbpred > 0.5,TRUE,FALSE)

test0 <- test
data$IsZero <- xgbpred
data$IsZero <- as.factor(data$IsZero)

testa <- data[!(data$IsZero=="TRUE"),]


testa.x <- data.frame(testa[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])
testa.y <- testa[,"PPI"]

testa.test <- model.matrix(~.+0,data = testa.x)

testa.matrix<- xgb.DMatrix(data = testa.test,label=testa.y)
pred.xgb.testa <- predict(xgb2, testa.matrix)

testa <- cbind(testa,pred.xgb.testa)
names(testa)[30] = "elastic.pred"

testb <- data[!(data$IsZero=="FALSE"),]
testb$pred.xgb.testa = rep(0,nrow(testb))
data <- rbind(testa, testb)
write.csv(data,"data2.csv")
sqrt(mean((test0$PPI - test0$elastic.pred)^2))
caret::RMSE(test0$pred.xgb.testa, test0$PPI)


fit.alt <- lm(PPI~., data = train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","PPI")])
fit.alt.pred <- predict(fit.alt, test[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","PPI")])
caret::RMSE(fit.alt.pred, test[,"PPI"])

droplevels(train)
droplevels(test)
















train1 <- train[!(train$IsZero == "TRUE"),]
test1 <- test[!(test$IsZero == "TRUE"),]




x1 <- data.frame(train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","IsZero")])
xtest1 <- data.frame(test[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","IsZero")])


y.alt <- train[,"PPI"]
ytest.alt <- test[,"PPI"]

dtrain.alt <- xgb.DMatrix(data = new_tr1,label = y.alt)
dtest.alt <- xgb.DMatrix(data = new_ts1,label=ytest.alt)

params1 <- list(booster = "gbtree", objective = "reg:linear", eta=0.1, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv.alt <- xgb.cv(params = params1, data = dtrain.alt, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb.alt <- xgb.train (params = params1, data = dtest.alt, nrounds = 25, watchlist = list(val=dtest.alt,train=dtrain.alt), print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "error")

pred.xgb.alt <- predict(xgb.alt, dtest.alt)

# results
caret::RMSE(pred.xgb.alt, ytest.alt)



x <- data.frame(train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])
xtest <- data.frame(data[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])


y <- d[,"PPI"]
ytest <- data[,"PPI"]


new_tr <- model.matrix(~.+0,data = x)
new_ts <- model.matrix(~.+0,data = xtest)

dtrain <- xgb.DMatrix(data = new_tr,label = y)
dtest <- xgb.DMatrix(data = new_ts,label=ytest)

params <- list(booster = "gbtree", objective = "reg:linear", eta=0.05, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 13, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "error")

s <- predict (xgb1,dtest)
caret::RMSE(xgbpred, ytest)



## Elastic net

elastic   <- cv.glmnet(new_tr, y, alpha = 0.1)
min(elastic$cvm)

elastic.pred <- predict(elastic, s = elastic$lambda.min, new_ts)
caret::RMSE(elastic.pred, ytest)

library(neuralnet)
x0 <- data.frame(train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status","PPI")])
train_matrix <- model.matrix(~.+0,data = x0)
f <- as.formula(PPI ~ SecARat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + Total.Investment + Quarter + Sector + Type + Region + Status)
nn_1 <- neuralnet(f, data = train_matrix, hidden=2, linear.output=TRUE)
summary(nn_1)
colnames

new_df <- read.cs('DataProjectTest.csv')



df0 <- as.table(as.matrix(df))

scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

df0 <- df0 %>%
  mutate_all(scale01)

# Split into test and train sets
set.seed(12345)
df_train <- sample_frac(tbl = df0, replace = FALSE, size = 0.80)
df_test <- anti_join(df0, train)


set.seed(12321)
nn1 <- neuralnet(PPI ~ SecARat + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + Total.Investment + Quarter + Sector + Type + Region + Status, data = df_train)





# Install the package
install.packages("SuperLearner")

# Load the package
library("SuperLearner")



x <- data.frame(train[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])
xtest <- data.frame(test[,c("SecARat","PC1","PC2","PC3","PC4","PC5","PC6","PC7","Total.Investment","Quarter","Sector","Type","Region","Status")])


y <- as.numeric(train[,"IsZero"])-1
ytest <- as.numeric(test[,"IsZero"])-1





# Set the seed
set.seed(150)

# Fit the ensemble model
model <- SuperLearner(y,
                      x,
                      family=binomial(),
                      SL.library=list("SL.ranger",
                                      "SL.ksvm",
                                      "SL.ipredbagg",
                                      "SL.bayesglm"))

# Return the model
model\


data <- read.csv("DataProjectTest.csv")
data$PPI <- rep(0,nrow(data))
data$IsZero <- rep(0,nrow(data))
data$IsZero <- as.factor(data$IsZero)
data$Patents <- rep(0,nrow(data))

test$Testcase <- rep(99999,nrow(test))
train$Testcase <- rep(11111,nrow(train))
data <- data[names(test)]

test <- rbind(test,data)



















library(car)
library(psych)
library(lmSupport)
library(reshape2)
library(outliers)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


set.seed(3)

df <- read.csv('DataProject.csv')
head(df)
data <- data[,-1]

data$Testcase = data$ï..TestCase
summary(df)
na.omit(df)#Remove any N/A

df <- unique(df) #Remove any duplication 
#44804.

summary(df$Status)

ggplot(df, aes(Sector, Patents)) +
  geom_bar(stat = "identity") + 
  labs(y = "Sector", x = "Patents")


df.cor.1 = cor(df[,c(9:18)])
corrplot(df.cor.1)

tab <- as.data.frame(data[,c(9:18)])
data <- cbind(data, Total.Investment = rowSums(tab)) #Calculate total investment

df$PPI <- with(df, Patents/(Total.Investment/100)) #PPI: Patents per 100 Investments

df<-df[!(df$Total.Investment==0),] # Remove zero investment
df<-df[!(df$Status=="Inactive"),] # Remove Inactive
df<-df[!(df$Status=="Not Available"),] # Remove zero investment
df<-df[!(df$Sector=="D3" & df$Industry=="C"),]

ggplot(data = melt(df), mapping = aes(x = value)) + 
  geom_histogram(bins=50) + facet_wrap(~variable, scales = 'free_x')

plot(df$Investment.Human.Capital, df$Patents)
abline(lm(df$Patents~df$Investment.Human.Capital))

hist(log(df$PPI))#Transform Log
pairs(df)
plot(df$SecARat)
df<-df[!(df$SecARat>200),] 

plot(df$Size)
df<-df[!(df$Size>30000),] 

plot(df$Investment.Human.Capital.B)
df<-df[!(df$Investment.Human.Capital.B>60),]


plot(df$Investment.Human.Capital.C)
df<-df[!(df$Investment.Human.Capital.C>200),]

plot(df$Investment.Human.Capital.D)

plot(df$Investment.Human.Capital.E)

plot(df$Investment.Human.Capital.F)

plot(df$Investment.Lab.Infrastructure)

plot(df$Investment.in.HR.Services)
df<-df[!(df$Investment.in.HR.Services>2500),]

plot(df$Investment.Building)
df<-df[!(df$Investment.Building>700),]

plot(df$PPI)
df<-df[!(df$PPI>2000),]


hist(df$Region)
df<-df[!(df$PPI>2000),]

cor(df$SecARat, df$PPI)


data$Quarter <- as.factor(data$Quarter)
data$Region <- as.factor(data$Region)

names(data)[9:18] =
  c("IHC.F","IHC.E","IHC.D","IHC.A","IHC.B","ILI","IHC.C","ICI","IHRS","IB")

df$IsZero <- (df$PPI == 0)
df$IsZero <- as.factor(df$IsZero)


library(mlbench)
library(caret)
library(caretEnsemble)


data$Sector <- car::recode(data$Sector, "c('B1', 'B5','B6','B11','B20')='B0';c('C10', 'C18', 'C19', 'C28','C34') = 'C0'")



m.pca <- prcomp(df[,c(9:18)], center = TRUE,scale. = TRUE)
summary(m.pca)

df <- cbind(df,m.pca$x[,1:7])

data$Sector <- car::recode(data$Sector, "c('B4','B12','B15','B16','B19','B22')='Bx';c('C1', 'C12', 'C14', 'C16','C21','C22','C23','C26','C3','C30','C37') = 'Cx'")
testdata <- predict(m.pca, newdata = data[,c(9:18)])
testdata <- as.data.frame(testdata)
data <- cbind(data,testdata)


mydata_numcols <- data[, sapply(data, is.numeric)]

mydata_faccols <- data[, sapply(data, is.factor)]

mydata_numcols <- as.data.frame(scale(mydata_numcols, center = TRUE, scale = TRUE))

mydata_numcols <- mydata_numcols[,-15]

depend <- as.data.frame(df[,"PPI"])

data <- as.data.frame(c(mydata_numcols,mydata_faccols))

rm(mydata_numcols,mydata_faccols)

names(df)[29] ="PPI"

names(df)[28] ="IsZero"

f$Region <- car::recode(df$Region, "c('2','3','4','5','6','8','9','10','11','12','13','16','18','20,'21','22','23','24','25','26','27','28')='Bx';c('C1', 'C12', 'C14', 'C16','C21','C22','C23','C26','C3','C30','C37') = 'Cx'")

require(data.table)

write.csv(df,"data_cleaned.csv"