rm(list=ls())

library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(lattice)

####################################################################

## data
dat <- read.table("alpha_diversity_values.txt")
dat <- within(dat, flight <- relevel(flight, ref = "pre"))
dat$timepoint <- factor(dat$timepoint, levels=dat$timepoint[1:10])
dat <- dat %>% select(group, flight, timepoint, richness, shannon)

####################################################################
## model fitting
####################################################################

## richness
mod.richness <- lmer(richness ~ flight + (1|group), data = dat)
summary(mod.richness)
anova(mod.richness)

mod2.richness <- lmer(richness ~ flight + (flight|group), data = dat)
summary(mod2.richness)
anova(mod2.richness)

lm.richness <- lm(richness ~ flight, data = dat)
summary(lm.richness)
anova(lm.richness)

anova(mod.richness, mod2.richness)
anova(mod.richness, lm.richness)

## shannon
mod.shannon <- lmer(shannon ~ flight + (1|group), data = dat)
summary(mod.shannon)
anova(mod.shannon)

lm.shannon <- lm(shannon ~ flight, data = dat)
summary(lm.shannon)
anova(lm.shannon)

####################################################################
## plot
####################################################################

## richness
dat %>% 
  ggplot(aes(timepoint, y=richness, group=group, color=group)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=c(4.5,7.5), linetype=2)

## fitted values
dat %>% 
  # save predicted values
  mutate(pred_richness = fitted(mod.richness), fixef_richness = predict(mod.richness, re.form=NA)) %>% 
  # graph
  ggplot(aes(x=timepoint, y=pred_richness, group=group, color=group)) + theme_bw() +
  geom_line() + geom_point() +
  geom_line(aes(x=timepoint, y=fixef_richness), color = "darkgrey", size=1) +
  geom_vline(xintercept=c(4.5,7.5), linetype="longdash")

## random effect dotplot
ranef(mod.richness)
dotplot(ranef(mod.richness, condVar = T))$group

## fitted values
dat %>% 
  # save predicted values
  mutate(pred_richness = fitted(mod2.richness), fixef_richness = predict(mod2.richness, re.form=NA)) %>% 
  # graph
  ggplot(aes(x=timepoint, y=pred_richness, group=group, color=group)) + theme_bw() +
  geom_line() + geom_point() +
  geom_line(aes(x=timepoint, y=fixef_richness), color = "darkgrey", size=1) +
  geom_vline(xintercept=c(4.5,7.5), linetype="longdash")

## random effect dotplot
ranef(mod2.richness)
dotplot(ranef(mod2.richness, condVar = T))$group


## shannon
dat %>% 
  ggplot(aes(timepoint, y=shannon, group=group, color=group)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=c(4.5,7.5), linetype=2)

## fitted values
dat %>% 
  # save predicted values
  mutate(pred_shannon = fitted(mod.shannon), fixef_shannon = predict(mod.shannon, re.form=NA)) %>% 
  # graph
  ggplot(aes(x=timepoint, y=pred_shannon, group=group, color=group)) + theme_bw() +
  geom_line() + geom_point() +
  geom_line(aes(x=timepoint, y=fixef_shannon), color = "darkgrey", size=1) +
  geom_vline(xintercept=c(4.5,7.5), linetype="longdash")

## random effect dotplot
dotplot(ranef(mod.shannon, condVar = T), strip = T, scales=list(relation='free'))$group

