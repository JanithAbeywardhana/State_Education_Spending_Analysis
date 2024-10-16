
library(mvtnorm, pos=17)
library(survival, pos=17)
library(MASS, pos=17)
library(TH.data, pos=17)
library(multcomp, pos=17)
library(abind, pos=22)
AnovaModel.1 <- aov(dollars ~ region, data = EduStates_New)
summary(AnovaModel.1)
with(EduStates_New, numSummary(dollars, groups = region, 
  statistics=c('mean', 'sd')))
AnovaModel.2 <- aov(dollars ~ region, data = EduStates_New)
summary(AnovaModel.2)
with(EduStates_New, numSummary(dollars, groups = region, 
  statistics=c('mean', 'sd')))
Boxplot( ~ dollars, data=EduStates_New, id=list(method="y"))
Boxplot( ~ dollars, data=EduStates_New, id=list(method="y"))
AnovaModel.4 <- lm(dollars ~ region, data=EduStates_New, contrasts=list(region ="contr.Sum"))
Anova(AnovaModel.4)
Tapply(dollars ~ region, mean, na.action=na.omit, data=EduStates_New) # means
Tapply(dollars ~ region, sd, na.action=na.omit, data=EduStates_New) # std. deviations
xtabs(~ region, data=EduStates_New) # counts
AnovaModel.5 <- lm(dollars ~ region, data=EduStates_New, contrasts=list(region ="contr.Sum"))
Anova(AnovaModel.5)
Tapply(dollars ~ region, mean, na.action=na.omit, data=EduStates_New) # means
Tapply(dollars ~ region, sd, na.action=na.omit, data=EduStates_New) # std. deviations
xtabs(~ region, data=EduStates_New) # counts
AnovaModel.7 <- aov(dollars ~ X, data = EduStates_New)
summary(AnovaModel.7)
with(EduStates_New, numSummary(dollars, groups = X, statistics=c('mean', 'sd')))
Boxplot( ~ dollars, data=EduStates_New, id=list(method="y"), ylab="dollars")
Boxplot( ~ dollars, data=EduStates_New, id=list(method="y"), ylab="dollars")
Boxplot( ~ dollars, data=EduStates_New, id=list(method="y"), ylab="dollars")

