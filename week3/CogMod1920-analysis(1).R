## ---
##
## CogMod 1920 Experiment
##
## Analysis script: Hedderik van Rijn
##
## v2, 191129@15:15
## ---

library("data.table")
library("bit64")
library("ggplot2")
library(Rmisc)


short <- fread('jatos_short.csv')
long <- fread('jatos_long.csv')

dat <- rbind(short,long)

## Only include participants who participated on Friday the 15th:
dat <- dat[dat[,grep("Fri Nov 15",datetime)],]

unique(dat$datetime)

## To increase the likelihood of presenting stimuli at multiples of 100ms (given a 100Hz monitor), 
## we asked OpenSesame for slide presentations durations in multiples of (n*100)-5 ms. Correct this 
## here.
dat$dur <- dat$duration+5

dat[, rt := response_time_TimeEnd]

## Remove data that is unlikely 
dat[,plot(density(rt))]
dat <- dat[rt<2500 & rt > 600,]

table(dat$workerId)

dat <- dat[workerId != 488,]

dat[,rt2 := rt - mean(rt), by=.(workerId,block)]

dat$rt2 <- NULL
dat[,rt2 := (rt)-mean(rt[dur==1400])+1400L, by=.(workerId,block,title)]

pd <- position_dodge(width = 25)

## Plot slopes for all participants individually to checl for potentially odd performance
plotdat <- dat[,.(RT=mean(rt),rep=.N),by=list(dur,block,title, workerId)]
setkey(plotdat,"dur")
plotdat[,groupblock:=interaction(title,block)]
ggplot(plotdat, aes(x=dur, y=RT, color=as.factor(workerId), group=workerId)) + 
    geom_line(position=pd, show.legend = FALSE) +
    geom_abline(slope=1,intercept=0, col="darkgrey", lty=2) +
    facet_grid(cols = vars(title), rows = vars(block))  + 
    coord_fixed(ratio = 1)

## use ggsave to save plots for including in reports. Note that the plots included
## here are can definitely be improved.

## Make basic plot:

plotdat <- dat[,.(RT=mean(rt),rep=.N),by=list(dur,block,title)]

plotdat[,groupblock:=interaction(title,block)]
ggplot(plotdat, aes(x=dur, y=RT, color=as.factor(block), group=groupblock)) + 
    geom_line(position=pd) +
    geom_point(position=pd) + 
    geom_abline(slope=1,intercept=0, col="darkgrey", lty=2) +
    facet_grid(cols = vars(title)) + 
    coord_fixed(ratio = 1)


## Make basic plot:

plotdat <- dat[,.(RT=mean(rt),rep=.N),by=list(dur,block,title, workerId)]

## Create within subject CI (Morey, 2008)
plotdat <- data.table(summarySEwithin(plotdat,"RT",withinvars=c("dur","block"),betweenvars=c("title")))
plotdat[,dur:=as.numeric(as.character(dur))]          
plotdat[,groupblock:=interaction(title,block)]
ggplot(plotdat, aes(x=dur, y=RT, color=as.factor(block), group=groupblock)) + 
    geom_line() +
    geom_point() + 
    geom_errorbar(mapping=aes(x=dur, ymin=RT-ci, ymax=RT+ci), width=0.2, size=.5,) + 
    geom_abline(slope=1,intercept=0, col="darkgrey", lty=2) +
    facet_grid(cols = vars(title)) + 
    coord_fixed(ratio = 1)

## Each block consisted of 80 trials, do we see changes within these blocks?
table(trunc(dat$count_Trial/40))
dat[,ExpQuart := as.factor(trunc(dat$count_Trial/40))]

## Plot slopes for both conditions, each block split out in 2 subblocks
plotdat <- dat[,.(RT=mean(rt),rep=.N),by=list(dur,ExpQuart,title)]

plotdat[,groupblock:=interaction(title,ExpQuart)]
ggplot(plotdat, aes(x=dur, y=RT, color=ExpQuart, group=groupblock)) + 
    geom_line(position=pd) +
    geom_point(position=pd) + 
    geom_abline(slope=1,intercept=0, col="darkgrey", lty=2) +
    facet_grid(cols = vars(title)) + 
    coord_fixed(ratio = 1)

## Was the manipulation implemented correctly?
plotdat <- dat[,.(RT=mean(rt),rep=.N),by=list(dur,block,title)]
setkey(plotdat,"dur")
plotdat[,groupblock:=interaction(title,block)]
ggplot(plotdat, aes(x=dur, y=rep, color=as.factor(block), group=groupblock)) + 
    geom_line(position=pd) +
    geom_point(position=pd) + 
    facet_grid(cols = vars(title))


## What is the mean reproduced duration for each group of 10 trials?
dat[,trialBin:=trunc(count_Trial/10)]
plotdat <- dat[,.(devRT=mean(rt-dur),RT=mean(rt),rep=.N),by=list(trialBin,title)]

## Actual reproduced duration
ggplot(plotdat, aes(x=trialBin, y=RT, color=title, group=title)) + 
    geom_line() +
    geom_point() 

## Deviation from expected duration
ggplot(plotdat, aes(x=trialBin, y=devRT, color=title, group=title)) + 
    geom_line() +
    geom_point() 

## Ergo, participants indeed responded overall later/longer when in the long-block
## than in the short block

## Statistical Analyses ----

library(lme4)
library(lmerTest)

anadat <- dat

## "Baseline" the reproduced duration (rt) and the actual duration, so that the mean
## duration is coded as 0.
anadat[,dur:=(dur-1400)/1000,]
anadat[,rt:=(rt-1400)/1000,]
## Create effect coding for "short/long" bias
anadat[,list:=ifelse(title=="TimingShort",-.5,.5)]
## Code the first block as 0, and the second block as 1. This way, all interaction
## effects describe the difference compared to the baseline block, and all main 
## effects just describe the performance in the "uniform" block.
anadat[,block:=block-1]

anadat[,subj := as.factor(workerId)]


## To test whether the reproduced durations deviate from the veridical duration, we 
## calculate a rtDiff score. If their is a relation between dur and rtDiff, there is
## a central tendency effect:
anadat[,rtDiff := rt - dur]

## This could be a main analysis to report in a result section (even though much more 
## is of course possible ;-)):
summary(anadat[,lmer(rtDiff ~ dur * block * list + (1 | subj))])
## This analysis shows that:
## 1. Overall reproductions are shorter than standard
## 2. Duration predicts "rt difference" - so central tendency
## 3. Block effect, reproductions are longer in second block
## 4. In the second block, the duration effect is stronger - so more central tendency
## 5. In the second block, there is a list effect - so the long block is associated with longer productions

## Importantly, there is *no* effect of list on dur. 

## Additional analyses, not updated/commented yet

m1 <- anadat[block==-.5,lmer(rt ~ dur + (1 | workerId))]
m2 <- anadat[block==-.5,lmer(rt ~ dur + list + (1 | workerId))]
m3 <- anadat[block==-.5,lmer(rt ~ dur * list + (1 | workerId))]

anova(m1,m2)
anova(m1,m3)
anova(m2,m3)


m1 <- anadat[block==.5,lmer(rt ~ dur + (1 | workerId))]
m2 <- anadat[block==.5,lmer(rt ~ dur + list + (1 | workerId))]
m3 <- anadat[block==.5,lmer(rt ~ dur * list + (1 | workerId))]

anova(m1,m2)
anova(m1,m3)
anova(m2,m3)

m1 <- anadat[,lmer(rt ~ dur * block + (1 | workerId))]
m2 <- anadat[,lmer(rt ~ (dur + block + list)^2 + (1 | workerId))]
m3 <- anadat[,lmer(rt ~ dur * block * list + (1 | workerId))]

anova(m1,m2)
anova(m2,m3)

summary(m2)

m1 <- anadat[,lmer(rt ~ dur * block * + (1 | workerId))]
m2 <- anadat[,lmer(rt ~ (dur + block + list)^2 + (1 | workerId))]
m3 <- anadat[,lmer(rt ~ dur * block * list + (1 | workerId))]

anadat[,trl := count_Trial %% 80 ]
m1 <- anadat[,lmer(rt ~ dur * block + (1 | workerId))]
m2 <- anadat[,lmer(rt ~ dur * block * trl + (1 | workerId))]
m3 <- anadat[,lmer(rt ~ dur * block * list * trl + (1 | workerId))]
m4 <- anadat[,lmer(rt ~ (dur + block + list + trl)^2 +(1 | workerId))]
m4b <- anadat[,lmer(rt ~ (dur + block + list + trl)^2 + dur:block:list +(1 | workerId))]

anova(m1,m2)
anova(m1,m3)
anova(m2,m3)
anova(m3,m4)
anova(m4,m4b)

summary(m4)