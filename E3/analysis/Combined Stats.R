library(lme4)
library(lmerTest)
library(sjstats)
library(tidyverse)
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/IllusoryTempo/E3/analysis/")


### DATA PREPARATION ###

# Load dataset
data0 = read.csv("../../E1/data/response_data.csv", fileEncoding="UTF-8-BOM")
data1 = read.csv("../../E2/data/response_data.csv", fileEncoding="UTF-8-BOM")
data2 = read.csv("../data/response_data.csv", fileEncoding="UTF-8-BOM")

# Filter excluded subjects
excluded0 = read_lines("../../E1/data/excluded.txt")
excluded1 = read_lines("../../E2/data/excluded.txt")
excluded2 = read_lines("../data/excluded.txt")
data0 = filter(data0, !(subject %in% excluded0))
data1 = filter(data1, !(subject %in% excluded1))
data2 = filter(data2, !(subject %in% excluded2))

# Adjust subject IDs to be unique by experiment
data1$subject = data1$subject + 1000
data2$subject = data2$subject + 2000

# Loudness codes were -19, -16, -13 in E1 but 0, 1, 2 in E2 and E3
data0$loudness[data0$loudness == -19] = 0
data0$loudness[data0$loudness == -16] = 1
data0$loudness[data0$loudness == -13] = 2

# Combine data into one table
full_data = rbind(data0, data1, data2)

# Recode variables
full_data = mutate_if(full_data, is.character, as.factor)
full_data$subject = as.factor(full_data$subject)

# Filter out outlier trials
data = filter(full_data, !(cooks > 4/90))


### RAW TEMPO RATINGS ###

# Derive theoretical ground-truth regression line
ioi_levels = c(1000, 918, 843, 774, 710, 652, 599, 550, 504, 463, 425, 390, 358, 329, 302)
score = 100 * log(1100/ioi_levels) / log(1100/275)
true_model = lm(score ~ log(ioi_levels))
true_slope = true_model$coefficients[2]
true_intercept = true_model$coefficients[1]

# Test subject fits against ground truth
subj_avgs = group_by(data, subject) %>%
  summarize(slope = mean(slope), intercept = mean(intercept))
t.test(subj_avgs$slope, mu=true_slope, var.equal=T, conf.level=.95)
(mean(subj_avgs$slope) - true_slope) / sd(subj_avgs$slope)  # Cohen's d
t.test(subj_avgs$intercept, mu=true_intercept, var.equal=T, conf.level=.95)
(mean(subj_avgs$intercept) - true_intercept) / sd(subj_avgs$intercept)  # Cohen's d

### LOUDNESS CONTROL ###

# Set loudness as a categorical factor
data$loudness = factor(data$loudness, ordered=F)

# Get subject averages
subj_avgs = group_by(data, subject, loudness) %>% 
  summarize(residual = mean(residual, na.rm=T))

# Descriptive stats
group_means = aggregate(subj_avgs$residual, list(subj_avgs$loudness), mean)$x  # Means by condition
group_sds = aggregate(subj_avgs$residual, list(subj_avgs$loudness), sd)$x / sqrt(length(unique(subj_avgs$subject)))  # Standard deviations by condition
group_sems = group_sds / sqrt(length(unique(subj_avgs$subject)))  # Standard errors by condition

# Repeated measures ANOVA
model = aov(residual ~ 1 + loudness + Error(subject/(loudness)), data=subj_avgs)
anova_stats(model)


### PITCH, TEMPO, & TAPPING ###

data$pitch = as.numeric(data$pitch)
data$tempo = factor(data$tempo, ordered=F)
data$tap_type = factor(data$tap_type, ordered=F)

# Fifth-order polynomial regression
model = lm(residual ~ 1 + poly(pitch, 5) * (tap_type + tempo), data=data)
summary(model)
anova_stats(model)

# Restricted second-order model
model = lm(residual ~ 1 + poly(pitch, 2) * (tap_type + tempo), data=data)
summary(model)
anova_stats(model)

# Pairwise t-tests for tempo ranges
subj_avgs = group_by(data, subject, tempo) %>% 
  summarize(residual = mean(residual, na.rm=T))
pairwise.t.test(subj_avgs$residual, subj_avgs$tempo, p.adj="fdr", paired=T, alternative="two.sided")

# Pairwise t-tests for pitches
subj_avgs = group_by(data, subject, pitch) %>% 
  summarize(residual = mean(residual, na.rm=T))
pairwise.t.test(subj_avgs$residual, subj_avgs$pitch, p.adj="fdr", paired=T, alternative="two.sided")

