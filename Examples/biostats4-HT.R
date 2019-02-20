library(dplyr)
library(ggplot2)
library(easyGgplot2)

### parametric test vs. non-parametric test ###

Nsim <- 10000
n <- 20

XX <- matrix(rnorm(n*Nsim, mean=0, sd=1), ncol=Nsim)
YY <- matrix(rnorm(n*Nsim, mean=1, sd=1), ncol=Nsim)

pval.t <- sapply(1:Nsim, function(i) t.test(XX[,i],YY[,i], var.equal = TRUE)$p.value)
pval.wilcox <- sapply(1:Nsim, function(i) wilcox.test(XX[,i],YY[,i])$p.value)

sum(pval.t < 0.05)/Nsim
sum(pval.wilcox < 0.05)/Nsim


### distribution of p-value under the null ###

pval.t.onesample <- sapply(1:Nsim, function(i) t.test(XX[,i])$p.value)
hist(pval.t.onesample, xlab="p-value", main="P-values of 10000 null hypotheses")
abline(v=0.05, col="red")

### FWER ###

alpha <- 0.05
m <- 1:100
plot(m,1-(1-alpha)^m, ylim=c(0,1),main="Probablity of at least one false positive")
abline(h=0.05, col="red")


### sample size ###

n1 <- 10; n2 <- 50; n3 <- 100; n4 <- 1000
dt0 <- tibble(value=rnorm(1000,0,1), density=dnorm(value,0,1))
g0 <- ggplot(dt0) + geom_line(aes(x=value, y=density), color ="red") + 
  xlim(-4,4) + ylim(0,0.75) +
  ggtitle("Population distribution: N(0,1)") +
  theme(plot.title=element_text(hjust=0.5))
plot(g0)

dt1 <- tibble(value=rnorm(n1,0,1))
g1 <- ggplot(dt1) + geom_histogram(aes(x=value, y = ..density..)) + 
  xlim(-4,4) + ylim(0,0.75) +
  geom_rug(aes(x=value), sides="b", colour="blue") +
  geom_line(data=dt0, aes(x=value, y=density), color ="red") +
  ggtitle(paste(n1,"random draws")) +
  theme(plot.title=element_text(hjust=0.5))

dt2 <- tibble(value=rnorm(n2,0,1))
g2 <- ggplot(dt2) + geom_histogram(aes(x=value, y = ..density..)) + 
  xlim(-4,4) + ylim(0,0.75) +
  geom_rug(aes(x=value), sides="b", colour="blue") +
  geom_line(data=dt0, aes(x=value, y=density), color ="red") +
  ggtitle(paste(n2,"random draws")) +
  theme(plot.title=element_text(hjust=0.5))

dt3 <- tibble(value=rnorm(n3,0,1))
g3 <- ggplot(dt3) + geom_histogram(aes(x=value, y = ..density..)) + 
  xlim(-4,4) + ylim(0,0.75) +
  geom_rug(aes(x=value), sides="b", colour="blue") +
  geom_line(data=dt0, aes(x=value, y=density), color ="red") +
  ggtitle(paste(n3,"random draws")) +
  theme(plot.title=element_text(hjust=0.5))

dt4 <- tibble(value=rnorm(n4,0,1))
g4 <- ggplot(dt4) + geom_histogram(aes(x=value, y = ..density..)) + 
  xlim(-4,4) + ylim(0,0.75) +
  geom_rug(aes(x=value), sides="b", colour="blue") +
  geom_line(data=dt0, aes(x=value, y=density), color ="red") +
  ggtitle(paste(n4,"random draws")) +
  theme(plot.title=element_text(hjust=0.5))

ggplot2.multiplot(g1, g2, g3, g4, cols=2)


### CLT: sampling distribution of sample mean ###

n1 <- 10; n2 <- 50; n3 <- 100; n4 <- 1000
Niter <- 200

xx1 <- matrix(rnorm(n1*Niter,0,1), ncol=Niter)
dt1.xbar <- tibble(xbar=colMeans(xx1))
g1.xbar <- ggplot(dt1.xbar, aes(x=xbar)) + geom_histogram(aes(y = ..density..)) + 
  xlim(-1,1) +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle(paste(n1,"random draws each sample")) +
  theme(plot.title=element_text(hjust=0.5))
# plot(g1.xbar)

xx2 <- matrix(rnorm(n2*Niter,0,1), ncol=Niter)
dt2.xbar <- tibble(xbar=colMeans(xx2))
g2.xbar <- ggplot(dt2.xbar, aes(x=xbar)) + geom_histogram(aes(y = ..density..)) + 
  xlim(-1,1) +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle(paste(n2,"random draws each sample")) +
  theme(plot.title=element_text(hjust=0.5))
# plot(g2.xbar)

xx3 <- matrix(rnorm(n3*Niter,0,1), ncol=Niter)
dt3.xbar <- tibble(xbar=colMeans(xx3))
g3.xbar <- ggplot(dt3.xbar, aes(x=xbar)) + geom_histogram(aes(y = ..density..)) + 
  xlim(-1,1) +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle(paste(n3,"random draws each sample")) +
  theme(plot.title=element_text(hjust=0.5))
# plot(g3.xbar)

xx4 <- matrix(rnorm(n4*Niter,0,1), ncol=Niter)
dt4.xbar <- tibble(xbar=colMeans(xx4))
g4.xbar <- ggplot(dt4.xbar, aes(x=xbar)) + geom_histogram(aes(y = ..density..)) + 
  xlim(-1,1) +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle(paste(n4,"random draws each sample")) +
  theme(plot.title=element_text(hjust=0.5))
# plot(g4.xbar)

ggplot2.multiplot(g1.xbar, g2.xbar, g3.xbar, g4.xbar, cols=2)
