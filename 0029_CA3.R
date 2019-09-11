
#### Study A ####

#### Question 1 ####
d <- read.csv('StudyA.csv')
head(d)
varDescribe(d)
#att3 and att4 have very high kurtosis
#se5 has an observation with score of 0, which is out of range (1-7). I'm gonna drop that
d <- d[!(d$se5=="0"),] 
varDescribeBy(d, d$sclnorm)
#att3 and att4 have higher skewness and kurtosis in experimental group
#=> There seems to be a convergent of score 
corr.test(d)
#for spc, all items are correlated with at least one other item in the group
#for se, se1 and se2 correlated, se4-5 correlated, and both groups correlate wirh se6. se3 is not correlated with others
#att items is not correlated with each others

#### Question 2 ####
psych::alpha(d[,c('spc1','spc2','spc3','spc4','spc5','spc6')], keys=c('spc5','spc6'))
#alpha = 0.69, which is marginally acceptable.
#dropping spc1 could result in a marginal increase of alpha to 0.71, but this is not significant
#Therefore we shouldn't drop any item
#Creating composite score:
d$spcM <- varScore(d, Forward= c('spc1','spc2','spc3','spc4'), Reverse = c('spc5','spc6'), Range=c(1, 7),
                    Prorate=TRUE, MaxMiss=0.33) / 6

#### Question 3 ####
#Hypothesis I is only partially correct, since the self-presentation concerns scale, with cronbach's alpha = .69, has a marginally "acceptable"
#reliability, not really a "good" one.
#Jasmine can perform a exploratory factor analysis (EFA), or using a Multi Trait Multi Method approach, using different methods to measure the same
#constructs. She can also use a nomological net to measure validity.

#### Question 4 ####
psych::alpha(d[,c('se1','se2','se3','se4','se5','se6')])
#alpha = 0.68, which is marginally acceptable.
#Dropping se3 can bring alpha to 0.76, and se3 is also not really correlated with other items.
#However, this is an established scale, so we are not removing se3.
d$seM <- varScore(d, Forward= c('se1','se2','se3','se4','se5','se6'), Range=c(1, 7),
                  Prorate=TRUE, MaxMiss=0.33) / 6

#### Question 5 ####

#difference score for each attitude item
for(i in 1:8) { 
  dif <- paste("dif", i, sep = "")
  att <- paste("att", i, sep = "")
  d[,dif] <- abs(d[,att] - mean(d[,att]))
}
describe(d[,c(24:31)]) # check OK

#Create composite score
d$difM<-varScore(d, Forward =c('dif1','dif2','dif3','dif4','dif5','dif6','dif7','dif8'), Range = c(0,100), Prorate = TRUE, MaxMiss = 0.25)/8

#### Question 6 ####

m1 <- lm(difM ~ sclnorm, data = d)
summary(m1)

#Students exposed to their peers’ opinions only express opinions marginally more similar to their peers (not statistically significant).
#b1 = -1.97, t(124) = -1.877, p = 0.06
#Can reject H2

#### Question 7 ####

#Case Analysis
hats <- modelCaseAnalysis(m1, Type='HATVALUES')
#Dichotomous, so no case with high leverage

resids <- modelCaseAnalysis(m1, Type='RESIDUALS')
resids
d[resids$Rownames,]
#Case number #3, #77, and #117 are regression outliers, since they have extreme attitude scores on most items

cooks <- modelCaseAnalysis(m1, Type='COOKSD')
cooks
d[cooks$Rownames,]
#Case #3,77,117 have high influence, since they have extreme attitude scores on most items

#I'll remove all three cases since they are both outliers and highly influential, and there is no specific intention to test reaction of 
#people with extreme opinions.

d1 <- dfRemoveCases(d, c(3,77,117))

#### Question 8 ####

m2 <- lm(difM ~ sclnorm, data = d1)
summary(m2)
modelEffectSizes(m2)
#Now there is a siginficant effect on social normal manipulation on opinion score difference. 
#b1 = -1.79: people who are exposed to peer's opinions express more similar opinions to their peers, with 1.97 unit smaller in opinion difference.
#b1 = -1.79, t(121) = -4.323, p<0.001, pEta^2 = .1338.
#The effect became much more significant, R^2 is higher (0.1338 vs 0.02764).

#### Question 9 ####
d1$sclnormC <- varRecode(d1$sclnorm, c(0,1), c(-0.5,0.5))
d1$spcMC <- d1$spcM - mean(d1$spcM)

m3 <- lm(difM ~ sclnormC*spcMC, data = d1)
summary(m3)
modelEffectSizes(m3)
#There is a significant interaction effect between self-presentation concerns and social norms manipulation on opinion score difference.
#b = -1.276: The effect of social norms manipulation increases 1.2758 unit in magnitude for one unit increase in self-presentation concerns score.
#b3 = -1.276, t(119) = -2.482, p<.02, pEta^2 = 0.05. Cannot reject H3.

#### Question 10 ####

d1$spcML <- d1$spcM - (mean(d1$spcM) - sd(d1$spcM))
m3b <- lm(difM ~ sclnormC*spcML, data = d1)
summary(m3b)
#social norms manipulation doesn't have a significant effect for individuals one standard deviation below the mean
#on the self-presentation concerns scale, p = 0.36

d1$spcMH <- d1$spcM - (mean(d1$spcM) + sd(d1$spcM))
m3c <- lm(difM ~ sclnormC*spcMH, data = d1)
summary(m3c)
#social norms manipulation have a significant effect for individuals one standard deviation below the mean
#on the self-presentation concerns scale, b1 = -2.35, t(119) = -4.51, p < 0.001, pEta^2 = 0.146

#### Question 11 ###
d1$seMC <- d1$seM - mean(d1$seM)
m4 <- lm(difM ~ sclnormC*spcMC + seMC*sclnormC, data = d1)
summary(m4)
modelEffectSizes(m4)
#When account for self-esteem, the interaction effect between etween self-presentation concerns and social norms manipulation
#on opinion score difference increases in magnitude and statistical power
#b = -1.66, t(117) = -2.96, p<.004, pEta^2 = 0.07. Cannot reject H3.

#### Question 12 ####
library(cowplot)
d1$sclnormF <- varRecode(d1$sclnorm, c(0,1), c('Control','Manipulated'))
m4b <- lm(difM ~ sclnormF*spcM + seM*sclnormF, data = d1)

pY <- expand.grid(sclnormF = c('Control','Manipulated'), spcM = seq(min(d$spcM), max(d$spcM), length = 200), seM = mean(d$seM))
pY <- modelPredictions(m4b, pY)


plot <- ggplot(d1, aes(x = spcM, y = difM, group= sclnormF, color = sclnormF)) + 
  geom_point() +
  geom_smooth(data = pY, aes(ymin = CILo, ymax = CIHi, x = spcM, y = Predicted), 
              stat = "identity") +
  ylab('Difference in Opinions from Peers') +
  xlab('Self-presentation concerns') +
  theme(legend.title=element_blank())

plot

#### Study B ####

#### Question 1 ####

df <- read.csv('StudyB.csv')
head(df)
varDescribe(df)
varPlot(df$hours)
varPlot(df$percent)
plot(df$percent,df$hours)
#There are more undergrad than grad participants, but otherwise OK
#There seems to be a relationship between percentage and hours
varDescribeBy(df,df$student)
#Undergrad volunteers much more than grad student
corr.test(df)
#number of hours volunteered is correlated with both student status and the percentage

#### Question 2 ####

mod1 <- lm(hours ~ percent, data = df)
summary(mod1)
modelEffectSizes(mod1)
#There is a significant relationship between percentage reported on poster and number of hours volunteered. 
#b = 0.066: Each percent increase leads to 0.066 hour increase. 
#b1 = 0.066, F(1,106) = 36.85, p < 0.001, pEta^2 = 0.258. We cannot reject H1

#### Question 3 ####

df$percentC <- df$percent - mean(df$percent)
df$studentC <- varRecode(df$student, c(0,1), c(-0.5,0.5))

mod2 <- lm(hours ~ percentC*studentC, data = df)
summary(mod2)
modelEffectSizes(mod2)
#There is a significant interaction effect between student status and percentage reported on poster on number of hours volunteered
#b = -0.048: the relationship between percentage reported and poster on number of hours volunteered is 0.048 unit stronger for undergraduate student
#in comparison with graduate students.
#b = -0.048, t(104) = -2.418, p < 0.02, pEta^2 = 0.05. We cannot reject H2.

#### Question 4 ####

mod2b <- lm(hours ~ percentC*student, data = df)
summary(mod2b)
modelEffectSizes(mod2b)
#There is a significant effect of percentage reported on poster on number of hours volunteered for undergraduate students.
#b = 0.084, t(104) = 6.9, p < 0.001, pEta^2 = 0.314

df$studentGrad <- varRecode(df$student, c(0,1), c(-1,0))

mod2c <- lm(hours ~ percentC*studentGrad, data = df)
summary(mod2c)
modelEffectSizes(mod2c)
#There is a significant effect of percentage reported on poster on number of hours volunteered for graduate students.
#b = 0.0366, t(104) = 2.369, p<0.02, pEta^2 = 0.208

#The relationship is 0.048 unit stronger in undergraduate student in comparison with graduate student.

#### Question 5 ####

library("gvlma")

modelAssumptions(mod2, "NORMAL")
#Normality is not satisfied since residuals is skewed left
modelAssumptions(mod2, "CONSTANT")
#Heteroskedasticity detected, constant variance assumption is not satisfied
df$inter <- df$percentC*df$studentC
mod2d <- lm(hours ~ percentC + studentC + inter, data = df)
modelAssumptions(mod2d, "LINEAR")
#Percent and interaction are not linear, linearity assumption is not satisfied
df$hours1 <- df$hours + 0.000001 #adding a very small positive value to Y so BoxCox could work
mod2e <- lm(hours1 ~ percentC + studentC + inter, data = df)
modelBoxCox(mod2e)
#lambda = 0.18
#so recommended BoXCox Transformation would be either fifth root or log transformation of hours
#This does not work since y contains 0

#### Question 6 ####

mod3 <- lm(hours ~ percentC + I(percentC^2), data = df)
summary(mod3)


#There is a significant quadratic effect of percent reported on hours volunteered, so the relationship is not linear
#b2 = -0.0014358: For each unit increase in percentage, the effect of percentage on hours volunteered decrease by 0.0014358*2 = 0.0028716 unit
#b2 = -0.0014358, t(105)=-2.579, p<0.02. Cannot reject hypothesis IIIa

#Check if it can go further?
mod3_2 <- lm(hours ~ percentC + I(percentC^2) + I(percentC^3), data = df)
summary(mod3_2) #No

#Test assumption again

modelAssumptions(mod3, "NORMAL") #Look better

modelAssumptions(mod3, "CONSTANT") #Still heteroskedastic

modelAssumptions(mod3, "LINEAR") #This is better

#### Question 7 ####

mod4 <- lm(hours ~ (percentC + I(percentC^2))*studentC, data = df)
summary(mod4)
#There is NO significant moderation effect of student status on the (non-linear) quadratic relationship of percentage and hours volunteer
#b5 = 0.0002, p = 0.826
#We can reject H3b

#### Question 8 ####

#percentC range from 30-68.333 = -38.333 to 100 - 68.333 = 31.666

f <- function(x, b) 3.9440794 + 0.0531029*x - 0.0014428*(x^2) - 2.3796868*b -0.0536100*x*b + 0.0002236*(x^2)*b

max_1 <- optimize(f, c(-38.333,31.666), maximum = TRUE, b = -0.5)
max_1
#For undergraduate, effect maximized at 6.16 hours with percentc = 25.7 => raw percentage should be showed is 25.7 + 68.333 ~ 94%

max_1 <- optimize(f, c(-38.333,31.666), maximum = TRUE, b = 0.5)
max_1

#For graduate, effect maximized at 2.88 hours with percentc = 9.88 => raw percentage should be showed is 9.88 + 68.333 ~ 78.2%

#### Question 9 ####
df$student_f <- varRecode(df$student, c(0,1), c('Undergraduate','Graduate'))
mod4b <-  lm(hours ~ (percent + I(percent^2))*student_f, data = df)

pY <- expand.grid(percent = seq(min(df$percent), max(df$percent), length = 100), student_f = c('Undergraduate','Graduate'))
pY <- modelPredictions(mod4b, pY)


plot <- ggplot(df, aes(x = percent, y = hours, group=as.factor(student_f), color = as.factor(student_f))) + 
  geom_point() +
  geom_smooth(data = pY, aes(ymin = CILo, ymax = CIHi, x = percent, y = Predicted), 
              stat = "identity") +
  ylab('Hours volunteered') +
  xlab('Percentage displayed') +
  theme(legend.title=element_blank())
plot

#### Study C ####

#### Question 1 ####

dat <- read.csv('StudyC.csv')

head(dat)
varDescribe(dat)
#There is a case with Excitement A = 0, which is out of bound.
#I'm gonna remove that
dat <- dat[!(dat$excitementA=="0"),]
varDescribeBy(dat[,c('excitementA','excitementB', 'time')], list(pose = dat$posetype, toy = dat$toytype))
#Nearly equal cell size. Used toys seems to result in lower excitement but higher time, and attack pose seems to result in higher time
varPlot(dat$excitementA)
varPlot(dat$excitementB)
#Look similar enough
corr.test(dat)
#posetype is highly correlated with time, and toy type is correlated with time and excitementA and B
#excitement A and B is highly correlated

#### Question 2 ####

corr.test(dat[,c('excitementA','excitementB')])
#Highly correlated (coef = 0.76, p < 0.001)
#Check again with t-test
t.test(dat$excitementA,dat$excitementB, paired = TRUE)
#95%CI [-0.796 -0.077], t(70) = -2.42, p<0.02
#So Excitement A and B is pretty similar

dat$excitement <- varScore(dat, Forward= c('excitementA', 'excitementB'), Range=c(1, 10),
                           Prorate=TRUE, MaxMiss=0.5) / 2

#### Question 3 ####

lm1 <- lm(time ~ posetype, data = dat)
summary(lm1)
#There is a significant effect of pose type on number of minutes cat played with the toys
#b1 = 2.8938: cat engaged in attack pose play with the toys 2.8938 hours more on average.
#b1 = 2.8938, t(69) = 3.116, p<0.003. We cannot reject hypothesis I.

#### Question 4 ####

dat$posetypeC <- varRecode(dat$posetype, c(0,1), c(-0.5,0.5))
dat$toytypeC <- varRecode(dat$toytype, c(0,1), c(-0.5,0.5))
lm2 <- lm(time ~ posetypeC*toytypeC, data = dat)
summary(lm2)
#There is NO statistically significant interaction effect between toy type and pose type
#b = 1.84, p = 0.29. We can reject hypothesis II.

#### Question 5 ####
dat$posetypeT <- varRecode(dat$posetype, c(0,1), c('Relax','Attack'))
dat$toytypeT <- varRecode(dat$toytype, c(0,1), c('New','Use'))
dat$posetypeT <- as.factor(dat$posetypeT)
dat$toytypeT <- as.factor(dat$toytypeT)

lm2b <- lm(time ~ posetypeT*toytypeT, data = dat)

pY <- expand.grid(posetypeT = c('Relax','Attack'), toytypeT = c('New','Use'))
pY <- modelPredictions(lm2b, pY)


bplot <- ggplot(pY, aes(x = posetypeT, y = Predicted, fill = toytypeT)) +
  geom_bar(stat='identity', position = position_dodge(.9)) + 
  geom_errorbar(aes(ymin = CILo, ymax = CIHi), position=position_dodge(.9), width=.25) +
  geom_point(data=dat, aes(y = time), position=position_jitterdodge(jitter.width = .2,dodge.width = .9), color='darkgrey') +
  xlab('Pose') +
  ylab('Hours played with toys') + coord_cartesian(ylim=c(0, 14)) +
  scale_y_continuous(expand = c(0,0)) + scale_fill_brewer(palette = 'Paired') +
  theme(legend.title = element_blank())
bplot

#### Question 6 ####
lm2c <- lm(time ~ posetypeC*toytype, data = dat)
summary(lm2c)
#For cat with new toy, there is no significant effect of pose type on time cat spent with the toys
#b = 1.94, p = 0.11
dat$toytypeH <- varRecode(dat$toytype, c(0,1), c(-1,0))

lm2d<- lm(time ~ posetypeC*toytypeH, data = dat)
summary(lm2d)
#For cat with used toy, there is a significant effeect of pose type on time cat spent with the toys
#b = 3.79, t(67) = 3.063, p < 0.004

#The broader point about interaction this tells us is that we cannot intepret an interaction just by comparing simple effects.

#### Question 7 ####
library(lavaan)

model1  <- ' 
excitement ~ 1 + A*toytype
time ~ 1 + CPRIME*toytype + B*excitement
AB:= A*B 
Total := CPRIME + A*B
'
fit <- sem(model1,data=dat)
summary(fit)

#We CANNOT detect significant mediating effect of excitement on the relationship between toy type and time the cat played with toys
#A*B = 0.516, p = 0.18
#There is a significant total effect of toy type on the time cat played with toys (c), though (we have not tested this before)
# c  = 2.891, z  = 3.162, 95% CI = [1.12, 4.71], p <0.002

#### Question 8 ####

dat$excitementC <- dat$excitement - mean(dat$excitement)
model2 <- lm(time ~ toytypeC*posetypeC, data = dat)
model2b <- lm(excitementC ~ toytypeC*posetypeC, data = dat)
model2c <- lm(time ~ toytypeC*posetypeC + excitementC*posetypeC, data = dat)
med <- mediate(model2b, model2c, treat = "toytypeC", mediator = "excitementC", covariates = list(posetypeC = 0))
summary(med)
test.modmed(med, covariates.1 = list(posetypeC = .5), covariates.2 = list(posetypeC = -.5))
#We CANNOT detect significant moderation effect of cat's pose on the mediatiing effect of excitement.
#ACME(covariates.1) - ACME(covariates.2) = 0.1631, p-value = 0.848
#95%CI [-1.44, 1.94]
#There is also no moderation effect of cat's pose on the direct effect.
#ADE(covariates.1) - ADE(covariates.2) = 1.6937, p-value = 0.352
#95%CI [ -1.96, 5.13]