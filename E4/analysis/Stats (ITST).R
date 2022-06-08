library(lme4)
library(lmerTest)
library(sjstats)
library(tidyverse)
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/IllusoryTempo/E4/analysis")

### DATA PREPARATION ###

# Load dataset
data = read.csv("../data/response_data.csv", fileEncoding="UTF-8-BOM")

# Filter excluded subjects
excluded = read_lines("../data/excluded.txt")
data = filter(data, !(subject %in% excluded))

# Recode variables
data = mutate_if(data, is.character, as.factor)
data$subject = as.factor(data$subject)
str(data)

# Filter out outlier trials
data = filter(data, !(cooks > 4/180))



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
t.test(subj_avgs$intercept, mu=true_intercept, var.equal=T, conf.level=.95)



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



### EFFECT OF PITCH ###

# Polynomial regression (Pitch x Tone Type)
data$pitch = as.numeric(data$pitch)
data$type = factor(data$type, ordered=F)
subj_avgs = group_by(data, subject, pitch, type) %>% 
  summarize(residual = mean(residual, na.rm=T))
model = lm(residual ~ 1 + poly(pitch, 5) * type, subj_avgs)
summary(model)
anova_stats(model)

