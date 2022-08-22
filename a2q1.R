# Assignment 2 R Code Question 1 
# a
x = c(1.69,1.72,1.76,1.78,1.81,1.84,1.86,1.88)
n = c(59,60,62,56,63,59,62,60)
y = c(6,13,18,28,52,53,61,60)


plot(x, log((y+0.5)/(n-y+0.5)), xlab = "Dosage", ylab = "Empirical Logits")


# b
fit.1 = glm(y/n ~ x, family = binomial, weight = n)
summary(fit.1)

# f
summary(fit.1)$cov.scaled

# g
c.val.95 = qchisq(0.95,6)
c.val.95
# Residual deviance 
deviance(fit.1)
# Pearson deviance 
sum(resid(fit.1, type = "pearson")^2)

# h
fit.1.d.res = residuals(fit.1, type = "deviance")
plot(x, fit.1.d.res, xlab = "Dosage", ylab = "Devaince residuals")

# i
fit.2 = glm(y/n ~ x + I(x^2), family = binomial, weight = n)
anova(fit.2, test = "Chi")

