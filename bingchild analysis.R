rm(list=ls()) 

bingchild<-read.csv("bingdataR.csv", header=T)
str(bingchild)
attach(bingchild)

library(lme4)
library(readr)
library(tidyverse)
library(sjPlot)
library(ggthemes)
library(gridExtra)
library(reshape2)
library(car)


#preprocessing of data, scale categorical variables and define others as factor
z.condition<-as.vector(scale(condition2))
z.trialno<-as.vector(scale(trialno))
z.age=as.vector(scale(age))
id=as.factor(bingchild$id)

agecat=relevel(agecat, ref="three")
condition=relevel(condition, ref="causal")
summary(bingchild)

#coding dummy variables before centering the slopes
sex.m<-as.numeric(sex==levels(sex)[2])
agecat.four<-as.numeric(agecat==levels(agecat)[3])
agecat.five<-as.numeric(agecat==levels(agecat)[2])
agecat.six<-as.numeric(agecat==levels(agecat)[4]) #is this correct?# CJV: yes

#centering the slopes
sex.m.c<-sex.m -mean(sex.m)
agecat.four.c=agecat.four-mean(agecat.four)
agecat.five.c=agecat.five-mean(agecat.five)
agecat.six.c=agecat.four-mean(agecat.four)

# running the control and then the full model: NOTE: I'm using data with some missing trials (5 of them)

contr<-glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))
full=glmer(correct ~ agecat*z.condition + z.trialno + sex + (1|id) + (1|boxtype) + (0+z.trialno|id),data=bingchild, family=binomial, control=contr) ## CV: does  condition and trial number vary within boxtype?
null=glmer(correct~ 1+(1|id)+(1|boxtype)+(0+z.trialno|id),data=bingchild, family=binomial, control=contr)
anova(null,full,test="Chisq")

modeldrop=drop1(full, test="Chisq",control=contr)
round(modeldrop,3)
#is this the final model I report? It's basically the same as the model I have above 
#but when I look at the summary(full), I see the interaction term for every level of agecat. This is confusing


#And trial no is significant. This is not very surprising as it took children ages to learn the rule if they ever did.
#How shall I explore this further? do I need to aggregate data for each trial no (so have 20 means for each trial) and then plot them?

#check means
#omit missing data? 
bingchild2 <- na.omit(bingchild) #no, there is a problem with varianle length of trialno in the analysis and it excluded one subject entirely.
bingaggregate <- aggregate(bingchild2$correct, by = list(age = bingchild2$age, condition = bingchild2$condition), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
shapiro.test(bingchild$correct)
#the data is non-normal

#This is my amateur way of doing one sample t-test/wilcoxon test!
#performance against chance level for all age groups in two different conditions
w3causal <- subset(bingchild, age=="3" & condition2=="1", select=c(id,trialno:correct))
wilcox.test(t3causal$correct, mu = 0.5, alternative = "two.sided", conf.int=TRUE, conf.level=0.95)
w3arbitrary <- subset(bingchild, age=="3" & condition2=="2", select=c(id,trialno:correct))
wilcox.test(w3arbitrary$correct, mu=0.5, alternative = "two.sided", conf.int=TRUE, conf.level=0.95)
#3 year olds are at chance level in both causal and arbitrary orders
#do I just report the p-values here? The V doesn't make sense?

w4causal <- subset(bingchild, age=="4" & condition2=="1", select=c(id,trialno:correct))
wilcox.test(w4causal$correct, mu=0.5, alternative = "two.sided", conf.int=TRUE, conf.level=0.95)
w4arbitrary <- subset(bingchild, age=="4" & condition2=="2", select=c(id,trialno:correct))
wilcox.test(w4arbitrary$correct, mu=0.5, alternative = "two.sided", conf.int=TRUE, conf.level=0.95)
#4 year olds are above chance in causal but not in arbitrary

w5causal <- subset(bingchild, age=="5" & condition2=="1", select=c(id,trialno:correct))
wilcox.test(w5causal$correct, mu=0.5, alternative = "two.sided", conf.int=TRUE, conf.level=0.95)
w5arbitrary <- subset(bingchild, age=="5" & condition2=="2", select=c(id,trialno:correct))
wilcox.test(w5arbitrary$correct, mu=0.5, alternative = "two.sided", conf.int=TRUE, conf.level=0.95)
#5 year olds are above chance in causal but not in arbitrary

w6causal <- subset(bingchild, age=="6" & condition2=="1", select=c(id,trialno:correct))
wilcox.test(w6causal$correct, mu=0.5, alternative = "two.sided", conf.int=TRUE, conf.level=0.95)
w6arbitrary <- subset(bingchild, age=="6" & condition2=="2", select=c(id,trialno:correct))
wilcox.test(w6arbitrary$correct, mu=0.5, alternative = "two.sided", conf.int=TRUE, conf.level=0.95)
#6 year olds are above chance in both causal and arbitrary

#differences between causal and arbitrary conditions across groups
wilcox.test(w3causal$correct, w3arbitrary$correct, paired = TRUE, alternative = "two.sided")
wilcox.test(w4causal$correct, w4arbitrary$correct, paired = TRUE, alternative = "two.sided")
wilcox.test(w5causal$correct, w5arbitrary$correct, paired = TRUE, alternative = "two.sided")
wilcox.test(w6causal$correct, w6arbitrary$correct, paired = TRUE, alternative = "two.sided")
#no differences for 3 year olds, sig diff for 4 and 5 year olds. Error for 6 year olds..


##ggplot but need to use omitted NA data
bingaggregate <- aggregate(bingchild2$correct, by = list(age = bingchild2$age, condition = bingchild2$condition), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
bingaggregate <- do.call(data.frame, bingaggregate)
bingaggregate$se <- bingaggregate$x.sd / sqrt(bingaggregate$x.n)

colnames(bingaggregate) <- c("age", "condition", "mean", "sd", "n", "se")

bingaggregate$names <- c(paste(bingaggregate$age, "age",
                              bingaggregate$condition, "condition"))

limits <- aes(ymax = bingaggregate$mean + bingaggregate$se,
              ymin = bingaggregate$mean - bingaggregate$se)
p <- ggplot(data = bingaggregate, aes(x = factor(age), y = mean, fill = factor(condition)))
p + geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.8) + geom_errorbar(limits, position = position_dodge(0.9), width = 0.10) + labs(x = "Age groups", y = "Mean number of correct choices") + ggtitle("Performance in causal and arbitrary conditions across age groups") + scale_fill_grey(name = "Condition") + geom_hline(yintercept=0.50, linetype="dashed", color="red", size=1)  + theme(legend.text = element_text(size = 8)) + ylim(0.00,1.00) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())

#detach(bingchild)
