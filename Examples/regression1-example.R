
#######################
## Linear regression ##
#######################

help(lm)
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)

anova(lm.D9)
summary(lm.D9)

## diagnostic plots
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, 1:4, las = 1)      # Residuals, Fitted, ...
par(opar)

## compare with two-sample t-test
t.test(ctl, trt, var.equal=TRUE)
plot(group, weight)


## create an artificial treatment group
trt2 <- ctl + rnorm(length(ctl),-1,sd(weight))
new.group <- rep(c(0,1,2), each=length(ctl)) #numerical categories: 0="Ctl", 1="Trt", 2="Trt2"
new.weight <- c(ctl, trt, trt2)

## use as.factor() for numerially coded categorical covariates
lm.new <- lm(new.weight ~ as.factor(new.group))
summary(lm.new)
anova(lm.new)

## compare with one-way ANOVA
aa <- aov(new.weight ~ as.factor(new.group))
summary(aa)
plot(as.factor(new.group), new.weight)

## leave covariates as numeric
lm.new1 <- lm(new.weight ~ new.group)
summary(lm.new1)
anova(lm.new1)



#########################
## Logistic regression ##
#########################

## load data
data("iris")
str(iris)
levels(iris$Species)
## subset data for two species
mydat <- iris[iris$Species!="setosa",]
plot(mydat$Sepal.Length, mydat$Sepal.Width, col=as.numeric(mydat$Species), pch=as.numeric(mydat$Species))
legend("topleft", levels(mydat$Species)[2:3], col=2:3, pch=2:3)

## fitting logistic regression model
mod <- glm(Species~Sepal.Length+Sepal.Width, data=mydat, family=binomial)
summary(mod)

## plot decision boundary, i.e. decision boundary is where p=0.5
mycoef <- coef(summary(mod))
intercept <- -mycoef[1]/mycoef[3]
slope <- -mycoef[2]/mycoef[3]
abline(intercept, slope, col="blue", lwd=3)

