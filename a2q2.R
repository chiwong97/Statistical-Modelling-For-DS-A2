# Assignment 2 Q2 
sex = c(rep("male",12),rep("female",12))
education = c(rep(6:17), rep(6:17))
agree = c(25,27,75,29,32,36,115,31,28,9,15,3,17,26,91,30,55,50,190,17,18,7,13,3)
disagree = c(9,15,49,29,45,59,245,70,79,23,110,29,5,16,36,35,67,62,403,92,81,34,115,28)
total = c(34,42,124,58,77,95,360,101,107,32,125,32,22,42,127,65,122,112,593,109,99,41,128,31)
attitude.dat = data.frame(sex,education,agree,disagree,total)

# a
sex.f = factor(sex)
attitude.dat$sex.f = sex.f
education.f = factor(education)
attitude.dat$education.f = education.f

fit.1 = glm(agree/total ~ sex.f + education.f, family = binomial, weight = total, data = attitude.dat)
summary(fit.1)
anova(fit.1, test = "Chi")

# switching the two variables around
fit.2 = glm(agree/total ~ education.f + sex.f, family = binomial, weight = total, data = attitude.dat)
anova(fit.2, test = "Chi")

# testing to see if the variables are strictly additive (i.e. no interaction term)
fit.3 = glm(agree/total ~ education.f*sex.f, family = binomial, weight = total, data = attitude.dat)
anova(fit.3, test = "Chi")


# b 
fit.4 = glm(agree/total ~ sex.f + education, family = binomial, weight = total, data = attitude.dat)
anova(fit.4, test = "Chi")

fit.5 = glm(agree/total ~ education + sex.f, family = binomial, weight = total, data = attitude.dat)
anova(fit.5, test = "Chi")

fit.6 = glm(agree/total ~ education + factor(education), family = binomial, weight = total, data = attitude.dat)
anova(fit.6, test = "Chi")

fit.7 = glm(agree/total ~ education + I(education^2), family = binomial, weight = total, data = attitude.dat)
anova(fit.7, test = "Chi")

fit.8 = glm(agree/total ~ education, family = binomial, weight = total, data = attitude.dat)
anova(fit.8, test = "Chi")
summary(fit.8)

plot(education, agree/total, xlab = "education", ylab = "proportion agree")

