d <- read.csv('population.csv')

plot(d$MET, d$Ex)
describe(d)

#### Question 1 ####
d$MET_C <- d$MET - mean(d$MET)

m1 <- lm(Ex ~ MET_C, data = d)
modelSummary(m1)

m2 <- lm(Ex ~ MET_C + I(MET_C^2), data = d )
modelSummary(m2)

#Choose m2 since m2 has predictor with signficant (MET_C^2), much higher R^2, and the plot of MET and Ex seems to show a quadratic relationship

#### Question 2 ####

library("gvlma")
modelAssumptions(m2, "NORMAL") #Normality confirmed
modelAssumptions(m2, "CONSTANT") #Heteroskedastic, assumption violated
modelAssumptions(m2, "LINEAR") #OK

# Heteroskedasticity may be fixed by logarithmized the DV (ln(Ex)), but then this would violate the normality assumption
# Or we can use Weighted Least Square, I guess
# or adding another factor

#### Question 3 ####

summary(m1)
summary(m2)
modelCompare(m1,m2) 
#R^2 increases since m2 explained much more variance than m1
#SSE m2 < SSE m1 for similar reason
#b0 decrease since effect of MET increase

#### Question 4 ####

m3 <-lm(Ex ~ (MET_C + I(MET_C^2))*grp,data=d)
summary(m3)
# The interaction term between MET_C^2 and grp is significant (b = .41, p < .005), while MET_C:grp is not significant
# => the non-linear relationship between MET_C and Ex is moderated by grp
#Also, for grp = 0.5, the non-linear effect is almost 0

#### Question 5 ####
m3b <- lm(Ex ~ (MET + I(MET^2))*grp,data=d)

pY <- expand.grid(MET = seq(min(d$MET), max(d$MET), length = 80), grp = c(-0.5,0.5))
pY <- modelPredictions(m3b, pY)

labels <- c("-0.5" = "Low cost Areas", "0.5" = "High cost Areas")

plot <- ggplot(d, aes(x = MET, y = Ex)) + 
  geom_point(color = "black") +
  geom_smooth(data = pY, aes(ymin = CILo, ymax = CIHi, x = MET, y = Predicted), 
              stat = "identity", color="red") +
  facet_grid(. ~ grp, labeller=labeller(grp = labels)) +
  ylab('Per capita state and local public expenditures ($)') +
  xlab('Urban population Percentage')

plot

#### Question 6 ####
#MET_C range from -46.17 to 40.33
# With grp = -.5 
f <- function(x, b) 419.33039 + 4.36467*x + 0.20238*(x^2) - 147.03967*b + 1.85471*x*b + 0.41198 *(x^2)*b

min_1 <- optimize(f, c(-46.16,40.33), maximum = FALSE, b = -0.5)
#~ zero % (0.1%)

min_2 <- optimize(f, c(-46.16,40.33), maximum = FALSE, b = 0.5)
# MET_C =  -6.479449 -> MET = 39.7%


#### PART B ####

library(mediation)
library(lavaan)

df <- dfReadDat('polymorphism.dat')

#### Question 1 ####

pathmodel <- ' 
Dopamine ~ 1 + A*Motivation
CogCtrl ~ 1 + CPRIME*Motivation + B*Dopamine
AB:= A*B
Total := CPRIME + A*B
'
fit <- sem(pathmodel,data=df)
summary(fit)

#Recheck

m1 <- lm(CogCtrl ~ Motivation, data=df)
modelSummary(m1)

m2 <- lm(Dopamine ~ Motivation, data=df)
modelSummary(m2)

m3 <- lm(CogCtrl ~ Motivation + Dopamine, data=df)
modelSummary(m3)

med <- mediation::mediate(m2,m3,treat="Motivation",mediator="Dopamine",boot=TRUE)
summary(med)
plot(med)

# Data is consistent with the simple mediation model

#### Question 2 ####

df$DopamineC <- df$Dopamine - mean(df$Dopamine)
mod1 <- lm(CogCtrl ~ Motivation * Genes, data = df)
summary(mod1)
confint(mod1)
mod2 <- lm(DopamineC ~ Motivation * Genes, data = df)
summary(mod2)
confint(mod2)
mod3 <- lm(CogCtrl ~ DopamineC * Genes + Motivation * Genes, data = df)
summary(mod3)
confint(mod3)

#only path a since interaction term in path a has p < 0.001, while path b has p >0.6

####QUestion 3####

med <- mediate(mod2, mod3, treat = "Motivation", mediator = 
                   "DopamineC", covariates = list(Genes = 0))
summary(med)

med2 <- mediate(mod2, mod3, treat = "Motivation", mediator = 
                 "DopamineC", covariates = list(Genes = -0.5))
summary(med2)

med3 <- mediate(mod2, mod3, treat = "Motivation", mediator = 
                  "DopamineC", covariates = list(Genes = 0.5))
summary(med3)

test.modmed(med, covariates.1 = list(Genes = .5), covariates.2 =
              list(Genes = -.5))


#The Indirect effect is higher and significant at Genes = 0.5, lower and insignificant at genes = -0.5
#CI(0.003398713 0.055956770), p < 0.04 => significant difference between Genes = -.5 and .5
#So the indirect effect is moderated by genes

#### Question 5 ####
df$CogCtrlC <- df$CogCtrl - mean(df$CogCtrl)

mod1b <- lm(DopamineC ~ Motivation * Genes, data = df)
summary(mod1b)
mod2b <- lm(CogCtrlC ~ Motivation * Genes, data = df)
summary(mod2b)
mod3b <- lm(DopamineC ~ Motivation * Genes + CogCtrlC * Genes, data = df)
summary(mod3b)

#only a could be moderated

medb <- mediate(mod2b, mod3b, treat = "Motivation", mediator = 
                 "CogCtrlC", covariates = list(Genes = 0))
summary(medb)
med2b <- mediate(mod2b, mod3b, treat = "Motivation", mediator = 
                  "CogCtrlC", covariates = list(Genes = -0.5))
summary(med2b)

med3b <- mediate(mod2b, mod3b, treat = "Motivation", mediator = 
                  "CogCtrlC", covariates = list(Genes = 0.5))
summary(med3b)


test.modmed(medb, covariates.1 = list(Genes = .5), covariates.2 =
              list(Genes = -.5))
#The different is not significant

#### Question 6 ####
#The reverse mediation model is consistent with the data, however, this mediation effect is not moderated by Genes
#This reinforces the hypothesis that polymorphism moderates the mediation power of Dopamine level
# on the effect of motivation on cognitive control
# People with polymorphism generates more dopamine when motivated, thus have better cognitive control

