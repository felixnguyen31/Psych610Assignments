library(car)
library(psych)
library(lmSupport)
library(gvlma)

#### QUestion 1 ####
d <- UN
describe(UN)
hist(d$infantMortality)
hist(d$ppgdp)
#No anomaly

#### Question 2 ####

mod1 <- lm(infantMortality ~ ppgdp, data = d)
summary(mod1)
#b1 = -0.0008656
#p = 0
#F(1,191) = 69.08
#R^2 = .2656

#### Question 3 ####

mod1_hat <- modelCaseAnalysis(mod1, Type='HATVALUES')
mod1_hat #Luxembourg, Norway, Qatar, Australia & Denmark have highest leverages, respectively. Luxembourg has by far the highest.
d[mod1_hat$Rownames,]

mod1_resids <- modelCaseAnalysis(mod1, Type='RESIDUALS') #There is no significant regression outlier here, Afghanistan is the most outlying one
d[mod1_resids$Rownames,]
outlierTest(mod1) #check again - "No Studentized residuals with Bonferonni p < 0.05"

mod1_cook<- modelCaseAnalysis(mod1, Type='COOKSD')
mod1_cook
d[mod1_cook$Rownames,] #Luxembourg, Norway, Qatar, Afghanistan, Chad have the highest influence, respectively. Luxembourg is outstandingly high.


modelCaseAnalysis(mod1, Type='DFBETAS')
mod1_dfbeta <- modelCaseAnalysis(mod1, Type="DFBETAS", 'ppgdp') #This returned error when I clicked "Finish" for some reason
mod1_dfbeta <- dfbetas(mod1) #Get dfbetas for ppgdp
mod1_dfbeta <- mod1_dfbeta[,'ppgdp']

plot(mod1_dfbeta, pch="*", cex=2, main="Influential Obs by dfbetas")  # plot dfbetas
text(x=1:length(mod1_dfbeta)+1, y=mod1_dfbeta, labels=ifelse(mod1_dfbeta>0.2, names(mod1_dfbeta),""), col="red")  # add labels
#Luxembourg, Norway, Qatar, Switzerland have the highest influence on GDP

# All in all, I will remove Luxembourg, Norway, Qatar
d <- dfRemoveCases(d, c('Luxembourg','Norway','Qatar'))

#### QUestion 4 ####

mod2 <- lm(infantMortality ~ ppgdp, data = d)
summary(mod2)
#b1 = -0.001
#p = 0
#F(1,188) = 80.14

#### Question 5 ####

modelAssumptions(mod2, "NORMAL") #Normality check
#Distribution is skewed heavy left
#Assumption not satisfied
modelAssumptions(mod2, "CONSTANT") #Heteroskedasticity check
#Not Satisfied
modelAssumptions(mod2, "LINEAR") #Linearity check
#Not Satisfied

#### QUestion 6 ####

d <- UN #Reset the dataset
modelBoxCox(mod1) #Suggested Lamba = -0.18
#I would use a log transformation here since it is ~ 0

#### Question 7 ####

d$IM_t <- log(d$infantMortality)

mod3 <- lm(IM_t ~ ppgdp, data = d) #Refit the model
modelAssumptions(mod3, "NORMAL") #Acceptable
modelAssumptions(mod3, "CONSTANT") #No significant heteroskedasticity
modelAssumptions(mod3, "LINEAR") # still not linear
#Transforming InfantMortality improved the model assumptions, but it still doesn't satisfy the linearity assumption

#### Question 8 ####

d$gdp_log <- log(d$ppgdp)
mod4 <- lm (infantMortality ~ gdp_log, data = d)
modelAssumptions(mod4, "NORMAL") #Not satisfied
modelAssumptions(mod4, "CONSTANT") #Not satisfied
modelAssumptions(mod4, "LINEAR") #Not satisfied
#The transformation of ppgdp alone doesn't improve model assumptions

#### Question 9 ####

mod5 <- lm(IM_t ~ gdp_log, data = d)
modelAssumptions(mod5, "NORMAL") #Normal enough accept for Equatorial Guinea
modelAssumptions(mod5, "CONSTANT") #Satisfied
modelAssumptions(mod5, "LINEAR") #Linear

#This model does the best in satisfying model assumptions.

#### Question 10 ####

mod5_hat <- modelCaseAnalysis(mod5, Type='HATVALUES') 
mod5_hat #Somalia has the most leverage

mod5_resids <- modelCaseAnalysis(mod5, Type='RESIDUALS')
mod5_resids #Equatorial Guinea is the regression outlier
outlierTest(mod5)

mod5_cook<- modelCaseAnalysis(mod5, Type='COOKSD')
mod5_cook #Equatorial Guinea has the highest influences

mod5_dfbeta <- dfbetas(mod5) #Get dfbetas for ppgdp
mod5_dfbeta <- mod5_dfbeta[,'gdp_log']

plot(mod5_dfbeta, pch="*", cex=2, main="Influential Obs by dfbetas")  # plot dfbetas
text(x=1:length(mod5_dfbeta)+1, y=mod5_dfbeta, labels=ifelse(abs(mod5_dfbeta)>0.2, names(mod5_dfbeta),""), col="red")  # add labels
#Equatorial Guinea hasthe highest influence on GDP_log

#### Question 11 ####
#Remove Equatorial Guinea

d <- dfRemoveCases(d, 'Equatorial Guinea')

mod6 <- lm(IM_t ~ gdp_log, data = d) #Refit
summary(mod6)
#b1 = -0.624: For each change of 1 unit of ln(ppgdp), ln(InfantMortality) changes by -0.624

#### Question 12 ####

mod6b <- lm(infantMortality ~ ppgdp, data = d)
modelSummary(mod6b)#SSE = 116113
modelSummary(mod6)#SSE = 47.3
#These differ since SSE of the raw model is in term of raw residual of infantMortality, while SSE of transformed model is in term of residual
#of log transformed mortality rate, so much smaller.

#### Question 13 ####
require('ggpubr')
require('ggplot2')
plot1 <- ggplot(d, aes(x = gdp_log, y = IM_t)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  ylab('Infant Mortality Rate (log-transformed)') + xlab('GDP per capita (log-transformed)') + ggtitle("Infant Mortality Rate by GDP per capita")
#Create plot

plot1 <- plot1 + theme_pubr() + annotate("text",x = 5.5, y = 7, size = 4, 
                                         label = paste("R^2 = ",signif(summary(mod5)$r.squared, 5),
                                                       "\nIntercept =",signif(mod5$coef[[1]],5 ),
                                                       " \nSlope =",signif(mod5$coef[[2]], 5),
                                                       " \nP-val =",signif(summary(mod5)$coef[2,4], 5)))
plot1
modelEffectSizes(mod6)
summary(mod6)
modelSummary
confint(mod6)