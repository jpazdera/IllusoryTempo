library(rstatix)  # For anova_test
library(tidyverse)
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/IllusoryTempo/E3/analysis/")

###
# DATA PREPARATION
###

# Load dataset
data0 <- read.csv("../../E1/data/response_data.csv", fileEncoding="UTF-8-BOM")
data1 <- read.csv("../../E2/data/response_data.csv", fileEncoding="UTF-8-BOM")
data2 <- read.csv("../data/response_data.csv", fileEncoding="UTF-8-BOM")

# Filter excluded subjects
excluded0 <- read_lines("../../E1/data/excluded.txt")
excluded1 <- read_lines("../../E2/data/excluded.txt")
excluded2 <- read_lines("../data/excluded.txt")
data0 <- filter(data0, !(subject %in% excluded0))
data1 <- filter(data1, !(subject %in% excluded1))
data2 <- filter(data2, !(subject %in% excluded2))

# Adjust subject IDs to be unique by experiment
data1$subject <- data1$subject + 1000
data2$subject <- data2$subject + 2000

# Loudness codes were -19, -16, -13 in E1 but 0, 1, 2 in E2 and E3
data0$loudness[data0$loudness == -19] <- 0
data0$loudness[data0$loudness == -16] <- 1
data0$loudness[data0$loudness == -13] <- 2

# Combine data into one table
full_data <- rbind(data0, data1, data2)

# Recode variables
full_data <- mutate_if(full_data, is.character, as.factor)
full_data$subject <- as.factor(full_data$subject)
full_data$pitch <- as.numeric(full_data$pitch)
full_data$tempo <- as.factor(full_data$tempo)
full_data$loudness <- as.factor(full_data$loudness)

# Filter out outlier trials
data <- filter(full_data, !(cooks > 4/90))
n_dropped <- count(full_data) - count(data)
perc_dropped <- n_dropped / count(full_data) * 100
print(n_dropped)  # 932 dropped
print(perc_dropped)  # 5.37% dropped

###
# RAW TEMPO RATINGS
###

# Derive theoretical ground-truth regression line
ioi_levels <- c(1000, 918, 843, 774, 710, 652, 599, 550, 504, 463, 425, 390, 358, 329, 302)
score <- 100 * log(1100/ioi_levels) / log(1100/275)
true_model <- lm(score ~ log(ioi_levels))
true_slope <- true_model$coefficients[2]
true_intercept <- true_model$coefficients[1]

# Test subject fits against ground truth
subj_avgs <- group_by(data, subject) %>%
  summarize(slope = mean(slope), intercept = mean(intercept))
# Slope (n.s.)
# t(192)=0.40, p=.693, d=0.029
t.test(subj_avgs$slope, mu=true_slope, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$slope) - true_slope) / sd(subj_avgs$slope)
# Intercept (n.s.)
# t(192)=-0.31, p=.756, d=-0.022
t.test(subj_avgs$intercept, mu=true_intercept, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$intercept) - true_intercept) / sd(subj_avgs$intercept)

###
# LOUDNESS CONTROL
###

# Get subject averages
subj_avgs <- group_by(data, subject, loudness) %>%
  summarize(residual = mean(residual, na.rm=T))
# Descriptive stats by condition -3:M=0.21, SD=0.13; +0:M=-0.09, SD=0.11; +3:M=-0.11, SD=0.13
group_means <- aggregate(subj_avgs$residual, list(subj_avgs$loudness), mean)$x
group_sds <- aggregate(subj_avgs$residual, list(subj_avgs$loudness), sd)$x / sqrt(length(unique(subj_avgs$subject)))
group_sems <- group_sds / sqrt(length(unique(subj_avgs$subject)))
subj_avgs <- ungroup(subj_avgs)

# Repeated measures ANOVA (n.s.)
# Sphericity violated (p=.043; GG epsilon=.968)
# GG-corrected rmANOVA: F(1.96, 375.63)=1.47, p=.232, pes=.008
anova_test(data=subj_avgs, dv=residual, wid=subject, within=loudness, effect.size="pes", type=3)

###
# EFFECT OF PITCH
###

# Fifth-order polynomial regression [ORIGINAL ANALYSIS]
#data$pitch <- as.numeric(data$pitch)
#data$tempo <- factor(data$tempo, ordered=F)
#data$tap_type <- factor(data$tap_type, ordered=F)
#model <- lm(residual ~ 1 + poly(pitch, 5) * (tap_type + tempo), data=data)
#summary(model)
#anova_stats(model)

s <- c()
b1 <- c()
b2 <- c()
b3 <- c()
b4 <- c()
b5 <- c()
for (subj in unique(data$subject)) {
  mask <- (data$subject==subj)
  model <- lm(residual ~ 0 + poly(pitch, 5), data=data[mask,])
  s <- append(s, subj)
  b1 <- append(b1, model$coefficients[1])
  b2 <- append(b2, model$coefficients[2])
  b3 <- append(b3, model$coefficients[3])
  b4 <- append(b4, model$coefficients[4])
  b5 <- append(b5, model$coefficients[5])
}
fits <- data.frame(s, b1, b2, b3, b4, b5)
fits$s <- as.factor(fits$s)
# Intercepts are fixed at 0 due to already regressing out subject intercepts
# during calculation of residual tempo ratings
# Linear Slope (**)
# t(192)=3.17, p=.002, d=0.228, M=2.39, SD=10.49
boxplot(fits$b1)
t.test(fits$b1, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1) / sd(fits$b1)
# Quadratic Slope (***)
# t(192)=-6.87, p<.001, d=-0.494, M=-5.36, SD=10.84
boxplot(fits$b2)
t.test(fits$b2, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b2) / sd(fits$b2)
# Cubic Slope (n.s.)
# t(192)=1.10, p=.275, d=0.079, M=0.82, SD=10.36
boxplot(fits$b3)
t.test(fits$b3, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b3) / sd(fits$b3)
# Quartic Slope (n.s.)
# t(192)=0.49, p=.622, d=0.036, M=0.37, SD=10.40
boxplot(fits$b4)
t.test(fits$b4, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b4) / sd(fits$b4)
# Quintic Slope (n.s.)
# t(192)=-1.62, p=.106, d=-0.117, M=-1.14, SD=9.71
boxplot(fits$b5)
t.test(fits$b5, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b5) / sd(fits$b5)

###
# EFFECT OF TEMPO RANGE
###

s <- c()
t <- c()
a <- c()
b1 <- c()
b2 <- c()
for (subj in unique(data$subject)) {
  for (temp in c(1:5)) {
    mask <- (data$subject==subj) & (data$tempo == temp)
    model <- lm(residual ~ 1 + poly(pitch, 2), data=data[mask,])
    s <- append(s, subj)
    t <- append(t, temp)
    a <- append(a, model$coefficients[1])
    b1 <- append(b1, model$coefficients[2])
    b2 <- append(b2, model$coefficients[3])
  }
}
fits <- data.frame(s, t, a, b1, b2)
fits$s <- as.factor(fits$s)
fits$t <- as.factor(fits$t)
# Intercept (Main Effect of Tempo Range) (***)
# Sphericity violated (p<.001, GG epsilon=0.695)
# GG-corrected rmANOVA: F(2.78, 533.72)=6.76, p<.001, pes=.034
boxplot(a ~ t, data=fits)
anova_test(data=fits, dv=a, wid=s, within=t, effect.size="pes", type=3)
# Linear Slope x Tempo Interaction (n.s.)
# Sphericity violated (p =.019, GG epsilon=0.946)
# GG-corrected rmANOVA: F(3.79, 726.91)=2.02, p=.094, pes=.010
boxplot(b1 ~ t, data=fits)
anova_test(data=fits, dv=b1, wid=s, within=t, effect.size="pes", type=3)
# Quadratic Slope x Tempo Interaction (n.s.)
# Sphericity violated (p=.003, GG epsilon=0.937)
# GG-corrected rmANOVA: F(3.75, 719.54)=1.56, p=.186, pes=.008
boxplot(b2 ~ t, data=fits)
anova_test(data=fits, dv=b2, wid=s, within=t, effect.size="pes", type=3)

# Pairwise comparisons for main effect of tempo
# 3 is lower than all except 2; 5 is greater than both 2 and 3
pairwise.t.test(fits$a, fits$t, paired=T, p.adjust.method='bonferroni', alternative='two.sided')

###
# EFFECT OF TAPPING
###

s <- c()
tap <- c()
a <- c()
b1 <- c()
b2 <- c()
for (subj in unique(data$subject)) {
  for (tap_cond in c(0, 2)) {
    mask <- (data$subject==subj) & (data$tap_type == tap_cond)
    if (sum(mask) > 2) {
      model <- lm(residual ~ 1 + poly(pitch, 2), data=data[mask,])
      s <- append(s, subj)
      tap <- append(tap, tap_cond)
      a <- append(a, model$coefficients[1])
      b1 <- append(b1, model$coefficients[2])
      b2 <- append(b2, model$coefficients[3])
    }
  }
}
fits <- data.frame(s, tap, a, b1, b2)
fits$s <- as.factor(fits$s)
fits$tap <- as.factor(fits$tap)
# Intercept (Main effect of tapping) (n.s)
# Intercept is fixed at zero within-subject due to regressing out intercept during
# residual tempo rating calculation. NTI condition's average intercept is guaranteed
# to be 0; TI-YT condition's intercept may not equal 0 if the participant didn't tap
# on every trial.
# t(87)=-0.17, p=.869, d=-0.012, M=-0.03, SD=1.02
boxplot(a ~ tap, data=fits)
t.test(fits$a[tap==2], mu=0, conf.level=.95, alternative="two.sided")
mean(fits$a) / sd(fits$a)
# Linear Slope x Tapping Interaction (n.s.)
# t(182)=-0.24, p=.812, d=-0.035, M=1.80|2.14, SD=9.01|10.00
boxplot(b1 ~ tap, data=fits)
t.test(fits$b1[tap==0], fits$b1[tap==2], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b1[tap==0]) - mean(fits$b1[tap==2])) / sd(fits$b1)
# Quadratic Slope x Tapping Interaction (.)
# t(182)=-1.94, p=.054, d=-0.284, M=-6.80 | -3.63, SD=11.26|10.86
boxplot(b2 ~ tap, data=fits)
t.test(fits$b2[tap==0], fits$b2[tap==2], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b2[tap==0]) - mean(fits$b2[tap==2])) / sd(fits$b2)
