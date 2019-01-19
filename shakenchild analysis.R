rm(list=ls()) 
shakenchild<-read.csv("shaken_child.csv", header=T)
str(shakenchild)
attach(shakenchild)
library(lme4)
library(readr)
library(tidyverse)
library(sjPlot)
install.packages('TMB', type='source')
library(ggthemes)
library(gridExtra)
library(reshape2)
library(car)

#preprocessing of data, scale categorical variables and define others as factor
z.trialno<-as.vector(scale(trialno))
z.age=as.vector(scale(age))
id<-as.factor(id)

agecat=relevel(agecat, ref="three")
summary(shakenchild)

#coding dummy variables before centering the slopes
sex.m<-as.numeric(sex==levels(sex)[2])
agecat.four<-as.numeric(agecat==levels(agecat)[3])
agecat.five<-as.numeric(agecat==levels(agecat)[2])

#centering the slopes
sex.m.c<-sex.m -mean(sex.m)
agecat.four.c=agecat.four-mean(agecat.four)
agecat.five.c=agecat.five-mean(agecat.five)

# running the control and then the full model

contr<-glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))
full=glmer(correct ~ agecat+z.trialno+sex+(1|id)+(1|boxtype)+(0+z.trialno|id),data=shakenchild, family=binomial, control=contr)
null=glmer(correct~ 1+(1|id)+(1|boxtype)+(0+z.trialno|id),data=shakenchild, family=binomial, control=contr)
summary(full)
anova(null,full,test="Chisq")

modeldrop=drop1(full, test="Chisq",control=contr)
round(modeldrop,3) #this is the results I'll report right? Age has a significant effect.

#posthoc analysis for age categories
install.packages("multcomp")
library("multcomp")
xx=glht(full, linfct = mcp(agecat = c("five-three = 0", "four-three = 0", "five-four =0")))
summary(xx)

#making the figure using: https://datascienceplus.com/building-barplots-with-error-bars/
# also this: http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
shakenaggregate <- aggregate(shakenchild$correct, by = list(age = shakenchild$age), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
shakenaggregate <- do.call(data.frame, shakenaggregate)
shakenaggregate$se <- shakenaggregate$x.sd / sqrt(shakenaggregate$x.n)

colnames(shakenaggregate) <- c("age", "mean", "sd", "n", "se")

shakenaggregate$names <- c(paste(shakenaggregate$age, "age"))

limits <- aes(ymax = shakenaggregate$mean + shakenaggregate$se,
              ymin = shakenaggregate$mean - shakenaggregate$se)
p <- ggplot(data = shakenaggregate, aes(x = factor(age), y = mean))
p + geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.8) + geom_errorbar(limits, position = position_dodge(0.9), width = 0.10) + labs(x = "Age groups", y = "Mean number of correct choices") + ggtitle("Performance in shaken boxes by age groups") + geom_hline(yintercept=0.50, linetype="dashed", color="red", size=1)  + theme(legend.text = element_text(size = 8)) + ylim(0.00,1.00) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())

#attempts for one sample t test
shapiro.test(shakenaggregate$mean)
#data is normal

install.packages("ggpubr")
library("ggpubr")
ggqqplot(shakenaggregate$mean, ylab = "average performance", ggtheme = theme_minimal())

#one sample t-test across age groups and phases
t3testdata <- subset(shakenchild, age=="3", select=c(id,trialno:correct))
t.test(t3testdata$correct, mu=0.5, alternative = "two.sided")
#3 year olds aren't significantly different from chance

t4testdata <- subset(shakenchild, age=="4", select=c(id,trialno:correct))
t.test(t4testdata$correct, mu=0.5, alternative = "two.sided")
#4 year olds are sig above chance

t5testdata <- subset(shakenchild, age=="5", select=c(id,trialno:correct))
t.test(t5testdata$correct, mu=0.5, alternative = "two.sided")
#5 year olds are sig above chance.

detach(shakenchild)
