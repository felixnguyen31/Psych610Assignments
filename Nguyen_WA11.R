library(psych)
library(car)
library(lmSupport)
library(ggplot2)
library(cowplot)
#### Question 1 ####

d <- dfReadDat("HW11_Darts.dat")
varDescribe(d)
head(d)
varDescribeBy(d[c("Experience","BAC","Coordination","Score")],d$Condition)
scatterplotMatrix(d)
spm(d)
corr.test(d)

#### Question 2 ####

#I'm not really sure about my understanding of this question
#I can write a function to put the M and SD in a 2 X 2 Matrix but it would be too long and may not be what you want?

suma <- function(var){
  result = paste0("M = ",round(mean(var, na.rm=TRUE),2),"; SD = ",round(sd(var, na.rm=TRUE),2))
  return(result)
}

aggregate(d[c("BAC","Coordination","Score")],by=list(Condition=d$Condition, Experience=d$Experience), suma)

#### Question 3 ####

d$BAC_C <- d$BAC - mean(d$BAC)
d$ConditionC <- varRecode(d$Condition, c(0,1), c(-.5,.5))
d$ExperienceC <- varRecode(d$Experience, c(0,1), c(-.5,.5))
d$CoordinationC <- d$Coordination - mean(d$Coordination)

#### Question 4 ####

m1 <- lm(Score ~ ConditionC*ExperienceC, data = d)
summary(m1)
#Intercept = 80.583 means the average score average across experience and test groups 
#b1 = -12.567, p > 0.07, there is no significant difference between test groups, average across experience level
#b2 = 1.5, p > 0.8, there is no significant difference between experience levels, average across test groups
#b3 = 40.333, p<0.005, there is a significant effect of Experience level on score differs by Condition.
#The experience effect is 40.33 unit higher for someone in alcoholic group, in comparison with placebo group

d$ConditionStr <- varRecode(d$ConditionC, c(-0.5, 0.5), c("Placebo","Alcoholic"))
d$ExperienceStr <- varRecode(d$ExperienceC, c(-0.5, 0.5), c("Novice","Experience"))
d$ConditionStr <- factor(d$ConditionStr)
d$ExperienceStr <- factor(d$ExperienceStr)
m1b <- lm(Score ~ ConditionStr*ExperienceStr, data = d)

pY = expand.grid(ConditionStr = c("Placebo","Alcoholic"), ExperienceStr = c("Novice","Experience"))
pY = modelPredictions(m1b, pY)
plota <- ggplot(pY, aes(x = ConditionStr, y = Predicted, fill = ExperienceStr)) +
  geom_bar(stat='identity', position = position_dodge(.9)) + 
  geom_errorbar(aes(ymin = CILo, ymax = CIHi), position=position_dodge(.9), width=.25) +
  geom_point(data=d, aes(y = Score), position=position_jitterdodge(jitter.width = .2,dodge.width = .9), color='darkgrey') +
  labs(x = 'Test Group', y = 'Score') +
  scale_y_continuous(expand = c(0,0)) + scale_fill_brewer(palette = 'Paired')
plota
  
#### Question 5 ####

m2 <- lm(Score ~ ConditionC*CoordinationC, data = d)
summary(m2)
#Intercept = 79 means the average score across Coordination rating and test groups 
#b1 = -11.769, p > 0.1, there is no significant difference in score between test groups, controlling for Coordination rating
#b2 = 0.6837, p > 0.6, there is no significant difference in score between people with different coordination raing, controlling for test group
#b3 = -5.19, p > 0.07, there is no significant effect of Coordination on score differs by Condition.
#CoordinationC = SD = 2.63 -> model: Score = 79 + 0.6837*2.63 - 11.77*ConditionC -5.19*2.63*ConditionC 
# Score = 80.8 - 25.42*ConditionC
# For someone who has Coordination 1 sd above the mean, alholic effect decreases score by 25.42, however this is not statistically significant.

m2b <- lm(Score ~ ConditionStr*Coordination, data = d)

pY <- expand.grid(Coordination = seq(min(d$Coordination), max(d$Coordination), length=80), ConditionStr=c("Placebo","Alcoholic"))
pY <- modelPredictions(m2b, pY)

plot2 <- ggplot(data=d, aes(x = Coordination, y = Score, color=ConditionStr)) + 
  geom_point(aes(shape=ConditionStr)) +
  geom_smooth(data = pY, aes(ymin = CILo, ymax = CIHi, y=Predicted),
              stat = "identity") +
 labs(x = 'Coordination', y= 'Score')
plot2

#### Question 6 ####

m3 <- lm(Score ~ BAC_C*CoordinationC, data = d)
summary(m3)
#Intercept = 79.2 means the average score across Coordination rating and BAC level
#b1 = -5.6345, p < 0.01, there is a significant difference in score between BAC level, controlling for Coordination rating, each BAC level increase decrease score by 5.63
#b2 = -0.3790, p > 0.7, there is no significant difference in score between people with different coordination raing, controlling for BAC level
#b3 = -0.9437, p < 0.02, there is a significant effect of Coordination on score differs by BAC level.
# BAC_C = 5.08333 -> model: Score = 79.18  -5.6345*5.083 -0.3790*CoordinationC -0.9437*5.083*CoordinationC 
# Score = 50.54 - 5.176*CoordinationC
# For someone who has BAC as maximum legal level, coordination effect decreases score by 5.176 per one unit decrease.

m3b <- lm(Score ~ BAC*Coordination, data = d)

XLow = expand.grid(Coordination = seq(min(d$Coordination),max(d$Coordination),length=80), 
                   BAC = mean(d$BAC)-sd(d$BAC))
XHi = expand.grid(Coordination = seq(min(d$Coordination),max(d$Coordination),length=80), 
                  BAC = mean(d$BAC)+sd(d$BAC))
YLow = modelPredictions(m3b, XLow)
YHi = modelPredictions(m3b, XHi)
plot3 <- ggplot(data=d, aes(x=Coordination, y = Score, color=BAC)) +
  geom_point() +
  geom_smooth(data=YLow, aes(y = Predicted, ymin = CILo, ymax = CIHi), stat='identity') +
  geom_smooth(data=YHi, aes(y = Predicted, ymin = CILo, ymax = CIHi), stat='identity') +
  coord_cartesian(ylim = c(0,150)) + 
  labs(x = 'Coordination', y = 'Score')
plot3


#### Question 7 ####

mod4 <- lm(Score~Condition, data = d)
summary(mod4)

mod5 <- lm(Score~BAC, data = d)
summary(mod5)