# Assignment 2 question 3

d.race = c(rep(c("white", "white", "black", "black"),2))
v.race = c(rep(c("white", "black"),4))
sentence = c(rep("yes",4), rep("no",4))
count = c(19,0,11,6,132,9,52,97)


death.dat = data.frame(d.race,v.race,sentence,count)
death.dat

# Test 1: all three factors are mutually independent 
fit.1 = glm(count ~ d.race + v.race + sentence, family = poisson, data = death.dat)
summary(fit.1)
deviance(fit.1)

# Test 2: sentence is independent of both the defendant and the victims race
fit.2 = glm(count ~ sentence + d.race*v.race, family = poisson, data = death.dat)
summary(fit.2)
deviance(fit.2)

# Test 3: given d.race, sentence is independent of v.race
fit.3 = glm(count~ sentence*d.race + v.race*d.race, family = poisson, data = death.dat)
summary(fit.3)
deviance(fit.3)

# Test 4: given v.race, sentence is independent of d.race
fit.4 = glm(count~ sentence*v.race + d.race*v.race, family = poisson, data = death.dat)
summary(fit.4)
deviance(fit.4)

# b 
d.race.logit = factor(c("white","white","black","black"))
v.race.logit = factor(c("white", "black"))
yes = c(19,0,11,6)
no = c(132,9,52,97)
total = yes+no
death.dat.logit = data.frame(d.race.logit,v.race.logit,yes,total)

# all three factors are mutually independent 
# means they are purely additive, no interaction term
fit.5 = glm(yes/total~d.race.logit*v.race.logit, family = binomial, weight = total, data = death.dat.logit)
anova(fit.5, test = "Chi")

# sentence is independent of both the dependent and the victim's race 
# means it is the null (intercept only) model 
fit.6 = glm(yes/total~1, family = binomial, weight = total, data = death.dat.logit)
anova(fit.6, test = "Chi")

# given defendents race, sentence is independent of the victims race
# means a model with only the defendent variable 
fit.7 = glm(yes/total~d.race.logit, family = binomial, weight = total, data = death.dat.logit)
anova(fit.7, test = "Chi")

# given victims race, sentence is independent of the dependents race
# means a model with only the victim variable
fit.8 = glm(yes/total~v.race.logit, family = binomial, weight = total, data = death.dat.logit)
anova(fit.8, test = "Chi")

