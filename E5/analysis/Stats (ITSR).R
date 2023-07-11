library(rstatix)  # For anova_test
library(tidyverse)
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/IllusoryTempo/E5/analysis")

###
# DATA PREPARATION
###

# Load dataset
full_data <- read.csv("../data/response_data.csv", fileEncoding="UTF-8-BOM")

# Filter excluded subjects
excluded <- read_lines("../data/excluded.txt")
full_data <- filter(full_data, !(subject %in% excluded))

# Recode variables
full_data <- mutate_if(full_data, is.character, as.factor)
full_data$subject <- as.factor(full_data$subject)
full_data$pitch <- as.numeric(full_data$pitch)
full_data$tempo <- as.factor(full_data$tempo)
full_data$loudness <- as.factor(full_data$loudness)
full_data$range <- as.factor(full_data$range)
str(full_data)

# Filter out outlier trials
data <- filter(full_data, !(cooks > 4/180))
n_dropped <- count(full_data) - count(data)
perc_dropped <- n_dropped / count(full_data) * 100
print(n_dropped)  # 664 dropped
print(perc_dropped)  # 4.85% dropped

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
# t(75)=0.71, p=.479, d=0.082
t.test(subj_avgs$slope, mu=true_slope, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$slope) - true_slope) / sd(subj_avgs$slope)
# Intercept (n.s.)
# t(75)=-0.57, p=.571, d=-0.065
t.test(subj_avgs$intercept, mu=true_intercept, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$intercept) - true_intercept) / sd(subj_avgs$intercept)

###
# LOUDNESS CONTROL
###

# Get subject averages
subj_avgs <- group_by(data, subject, loudness) %>%
  summarize(residual = mean(residual, na.rm=T))
# Descriptive stats by condition -3:M=0.15, SD=0.12; +0:M=-0.03, SD=0.10; +3:M=-0.12, SD=0.11
group_means <- aggregate(subj_avgs$residual, list(subj_avgs$loudness), mean)$x  # Means by condition
group_sds <- aggregate(subj_avgs$residual, list(subj_avgs$loudness), sd)$x / sqrt(length(unique(subj_avgs$subject)))  # Standard deviations by condition
group_sems <- group_sds / sqrt(length(unique(subj_avgs$subject)))  # Standard errors by condition
subj_avgs <- ungroup(subj_avgs)

# Repeated measures ANOVA (n.s.)
# F(2, 150)=0.98, p=.377, pes=.013
anova_test(data=subj_avgs, dv=residual, wid=subject, within=loudness, effect.size="pes", type=3)

###
# EFFECT OF PITCH AND REGISTER
###

s <- c()
r <- c()
b1 <- c()
b2 <- c()
for (subj in unique(data$subject)) {
  for (reg in c(0, 1)) {
    mask <- (data$subject==subj) & (data$range == reg)
    if (sum(mask) > 2) {
      model <- lm(residual ~ 0 + poly(pitch, 2), data=data[mask,])
      s <- append(s, subj)
      r <- append(r, reg)
      b1 <- append(b1, model$coefficients[1])
      b2 <- append(b2, model$coefficients[2])
    }
  }
}
fits <- data.frame(s, r, b1, b2)
fits$s <- as.factor(fits$s)
fits$r <- as.factor(fits$r)

# MAIN EFFECT OF PITCH
# Intercepts are fixed at 0 due to already regressing out subject intercepts
# during calculation of residual tempo ratings
# Linear Slope (***)
# t(75)=6.05, p<.001, d=0.694, M=11.15, SD=16.06
boxplot(fits$b1)
t.test(fits$b1, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1) / sd(fits$b1)
# Quadratic Slope (n.s.)
# t(75)=-0.950, p=.345, d=-0.109, M=-0.90, SD=8.22
boxplot(fits$b2)
t.test(fits$b2, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b2) / sd(fits$b2)

# PITCH x REGISTER INTERACTION
# Linear Slope (*)
# t(74)=2.07, p=.042, d=0.465, M=14.98|7.51, SD=16.81|14.62
boxplot(b1 ~ r, data=fits)
t.test(fits$b1[fits$r==0], fits$b1[fits$r==1], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b1[fits$r==0]) - mean(fits$b1[fits$r==1])) / sd(fits$b1)
# Quadratic Slope (n.s.)
# t(74)=-1.71, p=.091, d=-0.389, M=-2.53|0.66, SD=9.16|6.98
boxplot(b2 ~ r, data=fits)
t.test(fits$b2[fits$r==0], fits$b2[fits$r==1], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b2[fits$r==0]) - mean(fits$b2[fits$r==1])) / sd(fits$b2)

# Linear slope significantly differs by condition, so check whether slopes are nonzero in each condition
# Lower Register (***): t(36)=5.42, p<.001, d=0.891, M=14.98, SD=16.81
# Upper Register (**): t(38)=3.21, p=.003, d=0.514, M=7.51, SD=14.62
t.test(fits$b1[fits$r==0], mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1[fits$r==0]) / sd(fits$b1[fits$r==0])
t.test(fits$b1[fits$r==1], mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1[fits$r==1]) / sd(fits$b1[fits$r==1])
