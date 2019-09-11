library(psych)
library(car)
library(lmSupport)
library(mediation)

#### Question 1 ####
d = dfReadDat('Outgroup.dat')
varDescribe(d)
summary(d)
cor(d[, c("condition", "heterog", "individ", "subgr", "like")], use="pairwise.complete")
scatterplotMatrix(d[, c("condition", "heterog", "individ", "subgr", "like")]) 

#### Question 2 ####

d$conditionC <- varRecode(d$condition, c(1, 2), c(-.5, .5)) #Mean Centering Condition variable

#### Question 3 ####

m1 <- lm(heterog ~ conditionC, data = d) #Model of research question
summary(m1)
modelEffectSizes(m1)
#Group membership has a statistically significant effect on people's perceived heterogeneity of a group, and people believe their own group is more variable
#b = -2.6, F(1,18) = 2.34, p < .006, pEta-sqr = .3538
#The intercept here (4.9) is the mean perceived group heterogeneity of the whole sample

#### Question 4 ####

#a. Individ
m2a <- lm(individ ~ conditionC, data = d)
summary(m2a) #There is a signficant relationship between number of individual known and group membership

m2b <- lm(heterog ~ conditionC + individ, data = d)
summary(m2b)
#There is no signficant relationship between individ and heterog when control for conditionC

meda <- mediate(m2a, m2b, treat = "conditionC", mediator = "individ", boot=T)
summary(meda) #path a*b is not significant -> individ doesn't have a mediating effect

#b. subgr

m3a <- lm(subgr ~ conditionC, data = d)
summary(m3a) #There is a signficant relationship between number of subgroup known and group membership

m3b <- lm(heterog ~ conditionC + subgr, data = d)
summary(m3b)
#There is a relationship between subgr and heterog when control for conditionC

medb <- mediate(m3a, m3b, treat = "conditionC", mediator = "subgr", boot=T)
summary(medb) #path a*b is signficant -> Subgr has a mediating effect

#c.like

m4a <- lm(like ~ conditionC, data = d)
summary(m4a) #There is a signficant relationship between number of liking and group membership

m4b <- lm(heterog ~ conditionC + like, data = d)
summary(m4b)
#There is no significant relationship between like and heterog when control for conditionC

medc <- mediate(m4a, m4b, treat = "conditionC", mediator = "like", boot=T)
summary(medc) #path a*b is not signficant -> Like doesn't have a mediating effect

#### Question 5 ####

#for individ

m5 <- lm(individ ~ heterog + conditionC, data = d)
summary(m5)
revmed <- mediate(m1, m5, treat = "conditionC", mediator = "heterog", boot=T)
summary(revmed)
#People who are outgroup know less people in the target group, however the perceived heterogeneity of the group doesn't have any mediating effect.

#### Question 7 ####

writeUp =function(myModel){
  mod =myModel
  coeffs=modelSummary(mod,t=F)
  confs = confint(mod)
  effects =modelEffectSizes(mod)
  myVars=c() 
  for (i in attributes(mod$terms)["term.labels"]){
    myVars =append(myVars,i)
  }
  fstart=(length(myVars)+1)*2+1 
  pstart=(length(myVars)+1)*3+1
  cstart=1
  for (variable in myVars){
    fstart=fstart+1
    pstart =pstart+1
    cstart =cstart+1
    thisOutput =paste0(variable,": b = ",round(mod$coefficients[[variable]],3)," [",round(confs[cstart,1],2),"; ",
                       round(confs[cstart,2],2),"], F(1, ",mod$df.residual,") = ",
                       round(coeffs$coefficients[fstart],3),", p = ", round(coeffs$coefficients[pstart],3),
                       ", partial Î·Â² = ", round(effects$Effects[fstart],3))
    thisOutput =gsub("p = 0,", "p < .001,", thisOutput)
    print(thisOutput)
  }}
writeUp(m2b)

#### Question 8 ####
time <- "3 hours"