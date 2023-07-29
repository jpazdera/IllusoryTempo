library(ICSNP)  # For HotellingsT2
library(rstatix)  # For anova_test
library(ggpubr)  # For ggqqplot
library(tidyverse)  # For all things tidy
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
full_data$tempo_range <- as.factor(full_data$tempo_range)
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

# Test subject fits against ground truth
subj_avgs <- group_by(data, subject) %>%
  summarize(slope = mean(slope), intercept = mean(intercept))

# Overall Average Model (**)
# F(2, 74)=6.91, p=.002, pes=0.157
T2 <- HotellingsT2(subj_avgs[, c('slope', 'intercept')], mu=c(50, 50), na.action=drop, test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])

# Post-hoc univariate t-testing - note that all post-hoc testing is Holm-Bonferroni corrected to determine significance
# Slope (n.s.)
# t(75)=-0.71, p=.479 (.479), d=0.082, M=49.10, CI=[46.57, 51.62]
t.test(subj_avgs$slope, mu=50, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$slope) - 50) / sd(subj_avgs$slope)
# Intercept (***)
# t(75)=3.72, p<.001 (<.001), d=0.427, M=51.66, CI=[50.77, 52.55]
t.test(subj_avgs$intercept, mu=50, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$intercept) - 50) / sd(subj_avgs$intercept)

###
# LOUDNESS CONTROL
###

# Get subject averages
subj_avgs <- group_by(data, subject, loudness) %>%
  summarize(residual = mean(residual, na.rm=T), illusory_tempo = mean(illusory_tempo, na.rm=T))
# Descriptive stats by condition -3:M=0.19, SD=2.07; +0:M=-0.02, SD=1.72; +3:M=-0.17, SD=4.04
group_means <- aggregate(subj_avgs$illusory_tempo, list(subj_avgs$loudness), mean)
group_sds <- aggregate(subj_avgs$illusory_tempo, list(subj_avgs$loudness), sd)
subj_avgs <- ungroup(subj_avgs)

# Repeated measures ANOVA (n.s.)
# Sphericity not violated (p=.174)
# rmANOVA: F(2, 150)=0.45, p=.639, pes=.006
anova_test(data=subj_avgs, dv=illusory_tempo, wid=subject, within=loudness, effect.size="pes", type=3)

###
# EFFECT OF PITCH AND REGISTER
###

s <- c()
r <- c()
a <- c()
b1 <- c()
b2 <- c()
for (subj in unique(data$subject)) {
  for (reg in c(0, 1)) {
    mask <- (data$subject==subj) & (data$range == reg)
    if (sum(mask) > 2) {
      model <- lm(illusory_tempo ~ 1 + poly(pitch, 2), data=data[mask,])
      s <- append(s, subj)
      r <- append(r, reg)
      a <- append(a, model$coefficients[1])
      b1 <- append(b1, model$coefficients[2])
      b2 <- append(b2, model$coefficients[3])
    }
  }
}
fits <- data.frame(s, r, a, b1, b2)
fits$s <- as.factor(fits$s)
fits$r <- as.factor(fits$r)
plot(fits$b1, fits$b2)
write.csv(fits, '../data/pitch_fits.csv', row.names=F)

# MAIN EFFECT OF PITCH
# Intercepts are all approximately 0 due to already regressing out subject intercepts during calculation of
# residual tempo ratings. Compare the model slopes to 0 with Hotelling's T-squared.
# F(2, 74) = 17.08, p < .001, pes = 0.316
T2 <- HotellingsT2(fits[, c('b1', 'b2')], mu=rep(0, 2), na.action=drop, test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])

# Post-hoc one sample t-tests on slopes of each order
# Linear Slope (***)
# t(75)=5.87, p<.001 (<.001), d=0.673, M=26.46, CI=[17.47, 35.45]
ggqqplot(fits$b1)
t.test(fits$b1, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1) / sd(fits$b1)
# Quadratic Slope (n.s.)
# t(75)=-1.11, p=.270 (.270), d=0.128, M=-2.38, CI=[-6.65, 1.89]
ggqqplot(fits$b2)
t.test(fits$b2, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b2) / sd(fits$b2)

# PITCH x REGISTER INTERACTION

# Hotellings T-squared test for Pitch x Register interaction (n.s.)
# F(2, 73)=2.62, p=.080, pes = 0.067
T2 <- HotellingsT2(fits[fits$r==0, c('b1', 'b2')], fits[fits$r==1, c('b1', 'b2')], test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])