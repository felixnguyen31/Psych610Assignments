library(psych)
library(car)
library(lmSupport)

#### Question 1 ####

d <- read.csv('iq_brain.csv') #Read the data
summary(d) #Summary
varDescribe(d)
head(d)

#### Question 2 ####

scatterplotMatrix(d[, c("Gender","Weight","Height","MRI_Count","FSIQ_grouped")]) #Scatterplot

#### Question 3 ####

cor(d[, c("Gender","Weight","Height","MRI_Count","FSIQ_grouped")], use="pairwise.complete") #Correlation

#### Question 4 ####

#a, FSIQ_grouped
mod1 <- lm(MRI_Count ~ FSIQ_grouped, data = d)
summary(mod1)
#There is no statistically significant relationship between Dichotomous FSIQ and Voxel count when not controlling for any other variables.
# R squared = .058, pEta^2 =.058

#b. Gender
mod2 <- lm(MRI_Count ~ Gender, data = d)
summary(mod2)
#Gender is statistically significant in predicting voxel count, with male having 92.2 more voxel than female on average, beta = 92.2, t(38) = 5.216, p < 7e-06. 
#It also explain a significant proportion of variance of voxel count, R^2 = .4172

#c. Weight
mod3 <- lm(MRI_Count ~ Weight, data = d)
summary(mod3)
#Weight is statistically significant in predicting voxel count, with each pound of weight increases voxel count by 1.587, beta = 1.587, t(36) = 3.589, p < 0.001. 
#It also explain a significant proportion of variance of voxel count, R^2 = .2636

#d. Height
mod4 <- lm(MRI_Count ~ Height, data = d)
summary(mod4)
#Each inch of height increases voxel count by 11.023, beta = 11.023, t(37) = 4.582, p< 5.09e-05.
#It also explain a significant proportion of variance of voxel count, R^2 = .3621

#### Question 5 ####

mod5 <- lm(MRI_Count ~ Gender + Weight + Height, data = d)
summary(mod5)
#The 3-predictors model of Gender, Weight, Height is statistically significant in predicting Voxel count, F(3,24) = 9.555, R^2 = .4754, p <0.00011
#However, only Gender is staitsically significant, p <.023, while Weight and Height are not statistically significant.

#### Question 6 ####

modelEffectSizes(mod5) #Model effect size

#Gender: pEta^2 = .1436, dR^2 = .0910
#Weight: .006; .0033
#Weight: .032, .0179
#pEta^2 and dR^2 is much smaller than the 2-param model

#### Question 7 ####

#R^2 of the 3 predictors model is higher than any of the single predictors model
#This doesn't really tell us anything, since adding variables would always increase R^2

#### Question 8 ####

mod6 <- lm(MRI_Count ~ FSIQ_grouped + Gender + Weight + Height, data = d)
summary(mod6)
vif(mod6) #CHeck multicollinearity 
#all VIF << 5 -> No concerned multicollinearity

#### Question 9 ####
modelEffectSizes(mod6)

#Yes, FSIQ_grouped is now statistically significant (p < .018), and pBeta^2 increases as well (peta^2 = .1436).

#### Question 11 ####
pX = data.frame(FSIQ_grouped = c(0,1),
                Gender = 0.5, Height = mean(d$Height, na.rm = TRUE), Weight = mean(d$Weight, na.rm = TRUE))
pY = modelPredictions(mod6, pX) 
library(ggplot2)
library(ggpubr)

plot1 = ggplot(data=pY, aes(x = as.factor(FSIQ_grouped), y = Predicted)) +
  geom_bar(mapping = aes(fill=as.factor(FSIQ_grouped)), stat='identity', width=.5) +
  geom_point(data=d, aes(x = as.factor(FSIQ_grouped), y = MRI_Count), position = position_jitter(w=.12, h=.1)) +
  geom_errorbar(width = .25, aes(ymin = CILo, ymax = CIHi), stat='identity') +
  scale_x_discrete(labels=c("Average IQ", "High IQ")) +
  xlab('IQ Group') + ylab('Voxel count (thousands)') + ggtitle('Relationship between IQ and Brain Size') +
  theme_pubr() +  
  theme(legend.position="none")
plot1

#### Question 12 ####

time <- '2.5 hours'