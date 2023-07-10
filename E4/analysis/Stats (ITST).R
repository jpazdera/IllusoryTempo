library(rstatix)  # For anova_test
library(sjstats)  # For anova_stats
library(tidyverse)
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/IllusoryTempo/E4/analysis")

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
full_data$type <- as.factor(full_data$type)
str(full_data)

# Filter out outlier trials
data <- filter(full_data, !(cooks > 4/180))
n_dropped <- count(full_data) - count(data)
perc_dropped <- n_dropped / count(full_data) * 100
print(n_dropped)
print(perc_dropped)

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
t.test(subj_avgs$slope, mu=true_slope, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$slope) - true_slope) / sd(subj_avgs$slope)
t.test(subj_avgs$intercept, mu=true_intercept, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$intercept) - true_intercept) / sd(subj_avgs$intercept)

###
# LOUDNESS CONTROL
###

# Get subject averages
subj_avgs <- group_by(data, subject, loudness) %>%
  summarize(residual = mean(residual, na.rm=T))
group_means <- aggregate(subj_avgs$residual, list(subj_avgs$loudness), mean)$x  # Means by condition
group_sds <- aggregate(subj_avgs$residual, list(subj_avgs$loudness), sd)$x / sqrt(length(unique(subj_avgs$subject)))  # Standard deviations by condition
group_sems <- group_sds / sqrt(length(unique(subj_avgs$subject)))  # Standard errors by condition
subj_avgs <- ungroup(subj_avgs)

# Repeated measures ANOVA
anova_test(data=subj_avgs, dv=residual, wid=subject, within=loudness, effect.size="pes", type=3)

###
# EFFECT OF PITCH
###

s <- c()
b1 <- c()
b2 <- c()
for (subj in unique(data$subject)) {
  mask <- (data$subject==subj)
  model <- lm(residual ~ 0 + poly(pitch, 2), data=data[mask,])
  s <- append(s, subj)
  b1 <- append(b1, model$coefficients[1])
  b2 <- append(b2, model$coefficients[2])
}
fits <- data.frame(s, b1, b2)
fits$s <- as.factor(fits$s)
# Intercepts are fixed at 0 due to already regressing out subject intercepts
# during calculation of residual tempo ratings
# Linear Slope
boxplot(fits$b1)
t.test(fits$b1, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1) / sd(fits$b1)
# Quadratic Slope
boxplot(fits$b2)
t.test(fits$b2, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b2) / sd(fits$b2)

###
# EFFECT OF PITCH BY TONE TYPE
###

s <- c()
type <- c()
a <- c()
b1 <- c()
b2 <- c()
for (subj in unique(data$subject)) {
  for (tone_type in c('p', 'a')) {
    mask <- (data$subject==subj) & (data$type == tone_type)
    if (sum(mask) > 2) {
      model <- lm(residual ~ 1 + poly(pitch, 2), data=data[mask,])
      s <- append(s, subj)
      type <- append(type, tone_type)
      a <- append(a, model$coefficients[1])
      b1 <- append(b1, model$coefficients[2])
      b2 <- append(b2, model$coefficients[3])
    }
  }
}
fits <- data.frame(s, type, a, b1, b2)
fits$s <- as.factor(fits$s)
fits$type <- as.factor(fits$type)
# Intercept
boxplot(a ~ type, data=fits)
t.test(fits$a[fits$type=='p'], fits$a[fits$type=='a'], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$a[fits$type=='p']) - mean(fits$a[fits$type=='a'])) / sd(fits$a)
# Linear Slope
boxplot(b1 ~ type, data=fits)
t.test(fits$b1[fits$type=='p'], fits$b1[fits$type=='a'], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b1[fits$type=='p']) - mean(fits$b1[fits$type=='a'])) / sd(fits$b1)
# Quadratic Slope
boxplot(b2 ~ type, data=fits)
t.test(fits$b2[fits$type=='p'], fits$b2[fits$type=='a'], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b2[fits$type=='p']) - mean(fits$b2[fits$type=='a'])) / sd(fits$b2)

# If slopes significantly differ, check whether slopes are nonzero in each conditions
# Linear Slope
t.test(fits$b1[fits$type=='p'], mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1[fits$type=='p']) / sd(fits$b1[fits$type=='p'])
t.test(fits$b1[fits$type=='a'], mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1[fits$type=='a']) / sd(fits$b1[fits$type=='a'])
# Quadratic Slope
t.test(fits$b2[fits$type=='p'], mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b2[fits$type=='p']) / sd(fits$b2[fits$type=='p'])
t.test(fits$b2[fits$type=='a'], mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b2[fits$type=='a']) / sd(fits$b2[fits$type=='a'])
