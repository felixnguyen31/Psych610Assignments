setwd("Courses/Psych610Lab/Homework")
library(lmSupport)
library(car)
library(psych)

#### Question 1 ####

d <- dfReadDat("Darts.dat")
varDescribe(d)
varDescribeBy(d, d$Condition)
varDescribeBy(d, d$Experience)
scatterplotMatrix(d)
corr.test(d)
#SoberScore and FinalScore is highly correlated. This makes sense since SoberScore represents the inherent skill in the game, which obviously
#should influence FinalScore as well

#### Question 2 ####

corr.test(d[(d$Condition=="0"),]) #Correlation of Placebo Condition
corr.test(d[(d$Condition=="1"),]) #Correlation of Alcohol Condition
#The correlation does differ a little, but not much (0.69 vs 0.72)
#This means the relationship between SoberSCore and FinalScore is not moderated by Condition

#### Question 3 ####

d$ConditionC <- varRecode(d$Condition, c(0, 1), c(-0.5,0.5))
d$ExperienceC <- varRecode(d$Experience, c(0, 1), c(-0.5,0.5))

#### Question 4 ####

m1 <- lm(FinalScore ~ ConditionC*ExperienceC, data = d)
summary(m1)

#### Question 5 ####

m2 <- lm(FinalScore ~ ConditionC*ExperienceC + SoberScore, data = d)
summary(m2)
#This is not the correct analysis for this situation, according to Yzerbyt et al., since Experience is a measured IV, and Condition is a manipulated IV.
#This can lead to biased estimates of the effect of the interaction between the two IVs.
#Note: But in this case SoberScore doesn't significantly correlate with Experience or Condition, so I'm not really sure.

#### Question 6 ####

d$SoberScoreC <- d$SoberScore - mean(d$SoberScore)

m3 <- lm(FinalScore ~ ConditionC*ExperienceC+ ConditionC*SoberScoreC, data = d)
summary(m3)
#b0 = 94.51, the score of a person with Condition between Placebo & Alcohol, Experience between Novice and Expert, and average SoberScore.
#b1 = 5.1336, the conditon effect of a person with experience between Novice & Expert, and average SoberScore.
#b2 = 1.1548, the Experience effect of a person with neutral Condition
#b3 = 1.0138, the effect of soberscore in a person with Condition between Placebo & Alcohol
#b4 = 13.1415, the extent to which the condition effect changes based on experience (and vice versa), controlling for SoberScore
#and its interaction with Condition
#b5 = 0.1817, the extent to which the condition effect changes based on SoberScore (and vice versa), controlling for Experience
#and its interaction with Condition

#### Question 7 ####

#It is related to the correlation between SoberSCore and FinalScore by each Condition group, seen we are accounting for interaction effect between
#Condition and SoberScore

#### Question 8 ####
d$ConditionS <- varRecode(d$ConditionC, c(-0.5, 0.5), c("Placebo","Alcohol"))
d$ExperienceS <- varRecode(d$ExperienceC, c(-0.5, 0.5), c("Novice","Expert"))
d$ConditionS <- as.factor(d$ConditionS)
d$ExperienceS <- as.factor(d$ExperienceS)
m3b <- lm(FinalScore ~ ConditionS*ExperienceS + ConditionS*SoberScore, data = d)
summary(m3b)
pY <- expand.grid(ConditionS = c("Placebo","Alcohol"), ExperienceS =  c("Novice","Expert"), SoberScore = mean(d$SoberScore))
pY <- modelPredictions(m3b, pY)

p1 <- ggplot(pY, aes(x = ConditionS, y = Predicted, fill = ExperienceS)) +
  geom_bar(stat='identity', position=position_dodge(.9)) +
  geom_errorbar(aes(ymax=CIHi, ymin=CILo), position=position_dodge(.9), width=0.25) +
  theme(legend.title=element_blank())+
  ylab('Score') +
  xlab('Condition Group')
p1

######## Secion B ########

#### Question 1 ####

df <- dfReadDat("Castro.dat")
str(df) #The first 2 are "char"
df$essayC <- varRecode(df$essay, c("anti Castro","pro Castro"), c(-0.5, 0.5))
df$essayC <- as.numeric(df$essayC)
df$choiceC <- varRecode(df$choice, c("no choice","choice"), c(-0.5, 0.5))
df$choiceC <- as.numeric(df$choiceC)
df$essay <- as.factor(df$essay)
df$choice <- as.factor(df$choice)
varDescribe(df)
scatterplotMatrix(df[c('empathy','attitude','essayC','choiceC')])
corr.test(df[c('empathy','attitude','essayC','choiceC')]) #Essay & Attitude is correlated

#### Question 2 ####

mod1 <- lm(attitude ~ essayC*choiceC, data = df)
summary(mod1)
modelEffectSizes(mod1)
#The model indicate that if essay writer was desribed as having a choice, the attitude effect would be stronger. 

#### Question 3 ####
df$empathyC <- df$empathy - mean(df$empathy)
mod2 <- lm(attitude ~ essayC*choiceC*empathyC, data = df)
summary(mod2)
modelEffectSizes(mod2)

#### Question 4 ####

df$empathyL <- df$empathy - 1.5

mod2a <- lm(attitude ~ essayC*choiceC*empathyL, data = df)
summary(mod2a)
modelEffectSizes(mod2a) #Essay by choice effect is insignificant (p>0.24)

df$empathyH <- df$empathy - 5

mod2b <- lm(attitude ~ essayC*choiceC*empathyH, data = df)
summary(mod2b)
modelEffectSizes(mod2b) #Essay by choice effect is significant (p<.01, p-Eta^2 = 0.6447)

#### Question 5 ####

mPlot <- lm(attitude ~ essay*choice*empathy, data = df)
modelSummary(mPlot, t=F)

pY <- expand.grid(empathy = seq(min(df$empathy), max(df$empathy), length=10),
                 choice = c('choice','no choice'),
                 essay = c('anti Castro','pro Castro'))
pY <- modelPredictions(mPlot, pY)


p2 <- ggplot(data=df, aes(x = empathy, y = attitude, color=essay, group=essay)) + 
  geom_point() + 
  geom_smooth(data=pY, aes(ymin = CILo, ymax = CIHi, y = Predicted), stat = "identity") +
  facet_wrap("choice") + scale_x_continuous("empathy", breaks = seq(0, 6, by=1)) + 
  scale_y_continuous("attiude", breaks = seq(0, 6, by=1)) 
p2

#### Question 6 ####

pY <- expand.grid(choice = c('choice','no choice'), essay = c('anti Castro','pro Castro'), empathy = c(1.5,5))
pY <- modelPredictions(mPlot, pY)

p3 <- ggplot(pY, aes(x = choice, y = Predicted, fill = essay)) +
  geom_bar(stat='identity', position=position_dodge(.9)) +
  geom_errorbar(aes(ymax=CIHi, ymin=CILo), position=position_dodge(.9), width=0.25) +
  facet_wrap("empathy") +
  theme(legend.title=element_blank())+
  ylab('attitude') +
  xlab('Low Empathy vs High Empathy')
p3

