library(lmSupport)
library(car)
library(ggplot2)
library(psych)

#### Section III ####

#### Question 1 ####

d <- read.csv("CA1_DataA.csv") #Reading the data
varDescribe(d)
head(d)
#Exploring the data

#### Question 2 ####

d <- transform(d,
BMI = 703 * Weight/((12 * Height)^2)
) #Create BMI

#### Question 3 ####

d <- transform(d,
RunTime = RunTimeMin + RunTimeSec/60) #Create RunTime
d$RunTime <- round(d$RunTime, digits = 3) #Rounding up

#### Question 4 ####

db <- c('db1','db2','db3','db4','db5','db6')
alpha(d[,db])
#a. alpha = 0.62 so the current scale is not really reliable
#b. raw_alpha would increase to 0.74 if we drop 'db4' from the scale
alpha(d[,c('db1','db2','db3','db5','db6')])
#c. alpha increases to 0.74 after dropping 'db4'

#d. The removal of 'db4' is justified, for the following reasons:
#It helps increased cronbach's alpha by a significant amount (0.62 -> 0.74)
#'db4' has the lowest raw.r of 0.42, much lower than other items
#This is not an established scale

d$dbM <- varScore(d,
                  Forward= c('db1','db2','db3','db5','db6'),
                  Prorate=TRUE,
                  MaxMiss=0.4     # exclude missing more than 2 items
) / 5 #Create dbM

#### Question 5 ####

varDescribe(d[,c('he1','he2','he3','he4','he5','he6')])
#a. As we can see, the distributions of each items in this scale vary a lot
#the reverse-coded items don't center around 3 like other items
#This may be due to social desirability bias, or alcohol and sweet simply aren't consumed that much normally, comparing to other items.
#So by standardizing, we can make sure all items (even the reverse-coded) have the same weight.

#b. Standardization
attach(d)
d$he1S <- (he1- mean(he1))/sd(he1)
d$he2S <- (he2- mean(he2))/sd(he2)
d$he3S <- (he3- mean(he3))/sd(he3)
d$he4S <- -(he4- mean(he4))/sd(he4)#negative for the reverse-coded
d$he5S <- -(he5- mean(he5))/sd(he5)#negative for the reverse-coded
d$he6S <- (he6- mean(he6))/sd(he6)
detach(d)

heS <- c('he1S','he2S','he3S','he4S','he5S','he6S')
alpha(d[,heS])
#c. Alpha = 0.83, which means the scale is reliable
#d.No item would lead to an improvement in reliablity if dropped.
#e. No dropping so no need to re-check

d$heM <- varScore(d,
                  Forward = heS,
                  Prorate = TRUE,
                  MaxMiss = 0.34)/6
#f. Create heM

#### Question 6 ####

#a. Calcuating Correlation
corr.test(d[,c('heM','dbM')],use='complete')
#Correlation coefficient: 0.22 
# p-value ~ 0-> there is correlation
#b. Plot:
ggplot(d, aes(heM, dbM)) + geom_point() + 
  geom_smooth(method='lm') #Option 1
spm(d[,c('heM','dbM')]) #Option 2
#c. There is a moderate, positive relationship between an individual's belief about how healthy their diet is,
#and their actual healthy eating behavior. This relationship makes sense since an individual with a healthy diet would
#be more likely to believe their diet is healthy.

#### Question 7 ####

#a.
mod1 <- lm(BMI ~ heM, data = d) #Create the model
modelSummary(mod1) #View the model result
# Coeff: -1.2274, pval = 0.0009037, R^2 = 0.05, F = 11.36, df = 195, SSE = 2718.4

#b. From the model, we can see that for each unit of health eating habits scale, BMI decreases by 1.2274.
#This means healthy eating habit does have an negative relationship with a person BMI score
#however, the explanation power is not large (5% of variance) and healthy eating habits alone will not accurately predict BMI.

#c.
mod2 <- lm(RunTime ~ heM, data = d) #Create the model
modelSummary(mod2)
# Coeff: -0.6025, pval = 0.036, R^2 = 0.02, F = 4.448, df = 195, SSE = 1672.8

#d. From the model, we can see that for each unit of Health eating habits scale, Runtime decreases by 0.6025 minute.
#This means healthy eating habit does lower with a person one-mile run time.
#However, the explanation power is not large (2% of variance), and healthy eating habits alone will not accurately predict run time.

#e. PRE is Proportional Reduction of Error, which represents the proportion of one model's errors that is reduced or eliminated
#when we replace it with a more complex model

mod3 <- lm(RunTime ~ 1, data = d) #Create mean-only model
modelSummary(mod3) #SSE = 1711
SSE_A<- 1672.8
SSE_C<- 1711
PRE <- (SSE_C - SSE_A)/SSE_A #PRE = 0.0228
#PRE is equivalent to R quared

#### Question 8 ####

#a. Model BMI ~ dbM

mod4 <- lm(BMI ~ dbM, data = d)
modelSummary(mod4)
#Coeff: -0.5285, R^2 = 0.0103, SSE = 2847.2, pval = 0.156, df = 195

#b. From our model, we cannot say that there is a significant relationship between one individual's belief in how healthy their diet was
#and their BMI score.

#c. Model RunTime ~ dbM

mod5 <- lm(RunTime ~ dbM, data = d)
modelSummary(mod5)
#Coeff: -0.6809, R^2 = 0.0287, SSE = 1661.8, p-val = 0.0172

#d.From our model, we can say that there is a negative relationship between dbM and Runtime, with each unit of dbM reduces RunTime by 0.68 minute
#However, the explanation power is not large (2.3% of variance), so one individual's belief in how healthy their diet was alone cannot accurately
#predict their RunTime.


#f. Step C:
require('ggthemes')
plot1 <- ggplot(d, aes(x = dbM, y = RunTime)) +
          geom_point() +
          stat_smooth(method = "lm", col = "blue") +
          ylab('One-mile running time (minutes)') + xlab('Dietary Beliefs') + ggtitle("Running Time by Dietary Beliefs")
#Create plot

plot1 <- plot1 + theme_pubr() + annotate("text",x = 5.5, y = 20, size = 4, 
                                  label = paste("R^2 = ",signif(summary(mod5)$r.squared, 5),
                                                "\nIntercept =",signif(mod5$coef[[1]],5 ),
                                                " \nSlope =",signif(mod5$coef[[2]], 5),
                                                " \nP-val =",signif(summary(mod5)$coef[2,4], 5)))
plot1
#Add annotation

#### Section IV ####

#### Question 1 ####

rm(list = ls()) #Clear work environment

d <- read.csv("CA1_DataB.csv") #Read the data
summary(d)
varDescribe(d)
head(d) #Explore the data

#### Question 2 ####

d$catnipS <- varRecode(d$catnip, c(0,1), c('control','catnip')) #Create CatnipS
#### Question 3 ####

varDescribeBy(d$mice, d$catnipS)
#Mean of Catnip group: 3; mean of Control: 1.78

#### Question 4 ####

t.test(d$mice) #T-test
#mean of d$mice: 2.39
#confidence interval: (1.955406,2.827203)
#t = 11.049, df = 45, p-value = 2.084e-14

#### Question 5 ####

#a. Create model
mod1 <- lm(mice ~ catnip, data = d)
modelSummary(mod1)
#R^2 = 0.1758; SSE = 79.9, p-value = 0.004
#b. intercept coefficient: 1.7826 -> The average number of decoy mice found and attacked by cats in "Control" group is 1.7826
#catnip coefficient: 1.2174 -> On average, cats exposed to catnip found and attacked 1.2174 more mice, or a total of avg. 3 mice.
#c. The hypothesis is that cats exposed to catnip will find and attack more of the decoy mice cannot be rejected by the observed data.
#The exposure to catnip can explain 17.6% variance in number of mice found and attacked.
#d. Plot:
pX <- data.frame(catnip=c(0,1))
pY <- modelPredictions(mod1, pX)

require('ggthemes')
plot1 <- ggplot(pY, aes(x = as.factor(catnip), y = Predicted)) + 
  geom_bar(mapping = aes(fill = as.factor(catnip)), 
           stat = "identity", 
           width = 0.5) +
  geom_point(data = d, aes(y = mice, x = as.factor(catnip)),
               colour='darkgrey',
               position = position_jitter(width = 0.1, height=0)) +
  geom_errorbar(width=.25,
                aes(ymin = CILo,
                    ymax = CIHi), stat="identity") +
  xlab('Catnip Exposure') + ylab('Mice found & attacked') +
  theme_pubr() +  
  theme(legend.position="none")
  
plot1






