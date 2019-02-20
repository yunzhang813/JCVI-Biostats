## rnorm
x <- rnorm(10)
x
x <- rnorm(10, 20, 2)
x
summary(x)
sd(x)

## Always set a random seed when conducting simulation!
set.seed(1)
rnorm(5)
rnorm(5)
set.seed(1)
rnorm(5)

## Random sampling
set.seed(1)
sample(1:10, 4)
sample(1:10, 4)
sample(letters, 5)
sample(1:10) #permutation
sample(1:10)
sample(1:10, replace=TRUE) #sample w. replacement

#########################
## logistic regression ##
#########################
library(gtools) #required for inv.logit()

## simulate data
set.seed(1)
nsamp <- 10
x <- runif(nsamp, -1, 1)
logit.p <- 0.6 + 2.8*x
p <- inv.logit(logit.p)
y <- rbinom(nsamp, 1, p)
plot(x,y)

## fit glm and find predicted scores
mod <- glm(y~x, family=binomial(link = "logit"))
predicted <- mod$fitted.values
cbind(predicted, y)[order(predicted, decreasing = T),] #decreasing order

## ROC plot
pred <- prediction(predicted, y)
perf <- performance(pred, "tpr", "fpr")
plot(perf, lwd=3, colorize=TRUE,
     print.cutoffs.at=predicted, 
     cutoff.label.function=function(x){NULL}, 
     text.adj=c(1.3,1.3),
     main="ROC plot for simulated logistic regresion")


########################
## hypothesis testing ##
########################

## data generation
set.seed(123)
n <- 30
Niter <- 2000

XX0 <- matrix(rnorm(n*Niter, 0, 1), nrow=n)
YY0 <- matrix(rnorm(n*Niter, 0, 1), nrow=n)
XX1 <- matrix(rnorm(n*Niter, 0, 1), nrow=n)
YY1 <- matrix(rnorm(n*Niter, 0.5, 1), nrow=n)
Grp <- as.factor(rep(c("X","Y"), each=n)) #two groups

mydat0 <- rbind(XX0,YY0) #null data
mydat1 <- rbind(XX1,YY1) #alternative data

## t-test
library(genefilter)
pval.t0 <- colttests(mydat0, Grp)[,"p.value"]
hist(pval.t0)
pval.t1 <- colttests(mydat1, Grp)[,"p.value"]
hist(pval.t1)
pval.t <- c(pval.t0, pval.t1)

## Wilcoxon test
pval.w0 <- apply(mydat0, 2, function(z) wilcox.test(z~Grp)$p.value)
hist(pval.w0)
pval.w1 <- apply(mydat1, 2, function(z) wilcox.test(z~Grp)$p.value)
hist(pval.w1)
pval.w <- c(pval.w0, pval.w1)

## plot ROC curves and calculate AUC values
library(ROCR)
mylab <- rep(1:0, each=Niter) #set true labels

pred.t <- prediction(pval.t, mylab)
perf.t <- performance(pred.t, "tnr", "fnr")
auc.t <- performance(pred.t, "auc")@y.values[[1]] #get AUC
plot(perf.t, main="ROC curve for t-test",
     xlab="False positive rate", ylab="True positive rate",
     colorize=TRUE, print.cutoffs.at=c(0.05))
legend("bottom", paste0("AUC = ", round(auc.t,2)), bty = "n")
abline(0,1,lty=2)

pred.w <- prediction(pval.w, mylab)
perf.w <- performance(pred.w, "tnr", "fnr")
auc.w <- performance(pred.w, "auc")@y.values[[1]] #get AUC
plot(perf.w, main="ROC curve for Wilcoxon test",
     xlab="False positive rate", ylab="True prositive rate",
     colorize=TRUE, print.cutoffs.at=c(0.05))
legend("bottom", paste0("AUC = ", round(auc.w,2)), bty = "n")
abline(0,1,lty=2)

## overlay ROC curves
plot(perf.t, lwd=2,
     main=paste("ROC curves for", Niter, "iterations"),
     xlab="False positive rate", ylab="True positive rate")
plot(perf.w, col=2, lwd=2, add=TRUE)
legend("bottomright", paste(c("t-test", "Wilcoxon"), "=", round(c(auc.t,auc.w),2)),
       title = "AUC", col=1:2, lty=1, lwd=3)
abline(0,1,lty=2)







