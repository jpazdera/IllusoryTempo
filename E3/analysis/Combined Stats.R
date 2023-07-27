library(ICSNP)  # For HotellingsT2
library(rstatix)  # For anova_test
library(ggpubr)  # For ggqqplot
library(tidyverse)  # For all things tidy
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
full_data$tempo_range <- as.factor(full_data$tempo_range)
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

# Test subject fits against ground truth
subj_avgs <- group_by(data, subject) %>%
  summarize(slope = mean(slope), intercept = mean(intercept))

# F(2, 191)=3.50, p=.032, pes=0.035
T2 <- HotellingsT2(subj_avgs[, c('slope', 'intercept')], mu=c(50, 50), na.action=drop, test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])

# Slope (n.s.)
# t(192)=-0.40, p=.693, d=0.029, M=49.70, CI=[48.20, 51.20]
t.test(subj_avgs$slope, mu=50, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$slope) - 50) / sd(subj_avgs$slope)
# Intercept (**)
# t(192)=2.62, p=.009, d=0.189, M=50.59, CI=[50.15, 51.04]
t.test(subj_avgs$intercept, mu=50, conf.level=.95, alternative='two.sided')
(mean(subj_avgs$intercept) - 50) / sd(subj_avgs$intercept)

###
# LOUDNESS CONTROL
###

# Get subject averages
subj_avgs <- group_by(data, subject, loudness) %>%
  summarize(residual = mean(residual, na.rm=T), illusory_tempo = mean(illusory_tempo, na.rm=T))
# Descriptive stats by condition -3:M=0.34, SD=3.88; +0:M=-0.17, SD=3.29; +3:M=-0.17, SD=4.04
group_means <- aggregate(subj_avgs$illusory_tempo, list(subj_avgs$loudness), mean)
group_sds <- aggregate(subj_avgs$illusory_tempo, list(subj_avgs$loudness), sd)
subj_avgs <- ungroup(subj_avgs)

# Repeated measures ANOVA (n.s.)
# Sphericity violated (p=.005; HF epsilon=.957)
# HF-corrected rmANOVA: F(1.91, 367.66)=0.79, p=.449, pes=.004
anova_test(data=subj_avgs, dv=illusory_tempo, wid=subject, within=loudness, effect.size="pes", type=3)

###
# EFFECT OF PITCH
###

s <- c()
a <- c()
b1 <- c()
b2 <- c()
b3 <- c()
b4 <- c()
b5 <- c()
for (subj in unique(data$subject)) {
  mask <- (data$subject==subj)
  model <- lm(illusory_tempo ~ 1 + poly(pitch, 5), data=data[mask,])
  s <- append(s, subj)
  a <- append(a, model$coefficients[1])
  b1 <- append(b1, model$coefficients[2])
  b2 <- append(b2, model$coefficients[3])
  b3 <- append(b3, model$coefficients[4])
  b4 <- append(b4, model$coefficients[5])
  b5 <- append(b5, model$coefficients[6])
}
fits <- data.frame(s, a, b1, b2, b3, b4, b5)
fits$s <- as.factor(fits$s)
write.csv(fits, 'pitch_fits.csv', row.names=F)

# Intercepts are all approximately 0 due to already regressing out subject intercepts during calculation of
# residual tempo ratings. Compare the five slopes to 0 with Hotelling's T-squared. These pages have great explanations:
# https://real-statistics.com/multivariate-statistics/hotellings-t-square-statistic/one-sample-hotellings-t-square/
# https://biotoolbox.binghamton.edu/Multivariate%20Methods/Multivariate%20Hypothesis%20Testing/pdf%20files/MHT%20010.pdf
# F(5, 188) = 10.51, p < .001, pes = 0.218
T2 <- HotellingsT2(fits[, c('b1', 'b2', 'b3', 'b4', 'b5')], mu=rep(0, 5), na.action=drop, test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])

# Post-hoc one sample t-tests on slopes of each order
# Linear Slope (*)
# t(192)=2.40, p=.017, d=0.173, M=4.36, CI=[0.78, 7.94]
ggqqplot(fits$b1)
t.test(fits$b1, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1) / sd(fits$b1)
# Quadratic Slope (***)
# t(192)=-6.70, p<.001, d=0.483, M=-11.55, CI=[-14.95  -8.15]
ggqqplot(fits$b2)
t.test(fits$b2, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b2) / sd(fits$b2)
# Cubic Slope (n.s.)
# t(192)=1.16, p=.249, d=0.083, M=1.94, CI=[-1.37, 5.24]
ggqqplot(fits$b3)
t.test(fits$b3, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b3) / sd(fits$b3)
# Quartic Slope (n.s.)
# t(192)=0.88, p=.381, d=0.063, M=1.41, CI=[-1.76, 4.58]
ggqqplot(fits$b4)
t.test(fits$b4, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b4) / sd(fits$b4)
# Quintic Slope (n.s.)
# t(192)=-1.62, p=.106, d=0.117, M=-1.14, CI=[-4.89  0.75]
ggqqplot(fits$b5)
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
  for (tr in c(1:5)) {
    mask <- (data$subject==subj) & (data$tempo_range == tr)
    model <- lm(illusory_tempo ~ 1 + poly(pitch, 2), data=data[mask,])
    s <- append(s, subj)
    t <- append(t, tr)
    a <- append(a, model$coefficients[1])
    b1 <- append(b1, model$coefficients[2])
    b2 <- append(b2, model$coefficients[3])
  }
}
fits <- data.frame(s, t, a, b1, b2)
fits$s <- as.factor(fits$s)
fits$t <- as.factor(fits$t)

# MANOVA for Pitch x Tempo interaction (n.s.)
# Normality: Univariate QQplots suggest higher than expected kurtosis, without much skew - probably okay for MANOVA
# Multivariate distribution seems okay, maybe 2 or 3 notable outliers
ggqqplot(fits, 'b1', facet.by='t')
ggqqplot(fits, 'b2', facet.by='t')
plot(fits$b1, fits$b2)
# Slopes are not significantly correlated, no multicollinearity
# No apparent nonlinear relation between slopes
cor.test(fits$b1, fits$b2)
# Homogeneity of covariances: Not homogeneous, but should be okay due to balanced design
box_m(fits[, c('b1', 'b2')], fits$t)
# Wilks = 0.983, F(8, 1534) = 1.65, p = .107, pes = .009
model <- manova(cbind(b1, b2) ~ t + Error(s / t), data=fits)
summary(model, test='Wilks')
8 * 1.6465 / (8 * 1.6465 + 1534)  # Partial eta squared

# Intercept (Main Effect of Tempo Range) (***)
# Sphericity violated (p<.001, GG epsilon=0.715)
# GG-corrected rmANOVA: F(2.86, 549.02)=5.55, p<.001, pes=.028
boxplot(a ~ t, data=fits)
anova_test(data=fits, dv=a, wid=s, within=t, effect.size="pes", type=3)
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
  for (tap_cond in c(0, 1, 2)) {
    mask <- (data$subject==subj) & (data$tap_type == tap_cond)
    s <- append(s, subj)
    tap <- append(tap, tap_cond)
    if (sum(mask) > 2) {
      model <- lm(illusory_tempo ~ 1 + poly(pitch, 2), data=data[mask,])
      a <- append(a, model$coefficients[1])
      b1 <- append(b1, model$coefficients[2])
      b2 <- append(b2, model$coefficients[3])
    } else {
      a <- append(a, NaN)
      b1 <- append(b1, NaN)
      b2 <- append(b2, NaN)
    }
  }
}
fits <- data.frame(s, tap, a, b1, b2)
fits <- drop_na(fits)
fits$s <- as.factor(fits$s)
fits$tap <- as.factor(fits$tap)
write.csv(fits, 'tap_fits.csv', row.names=F)

ggqqplot(fits, 'a', facet.by='tap')
ggqqplot(fits, 'b1', facet.by='tap')
ggqqplot(fits, 'b2', facet.by='tap')
plot(fits$b1, fits$b2)

# Hotellings T-squared test for Pitch x Tapping interaction (*)
# F(2, 181) = 3.28, p = .040, pes = 0.035
T2 <- HotellingsT2(fits[fits$tap==0, c('b1', 'b2')], fits[fits$tap==2, c('b1', 'b2')], test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])

# Post-hoc t-test for effect of tapping on each slope order
# Linear Slope (n.s.)
# t(182)=-0.64, p=.526, d=0.089, M=2.83 | 4.98, CIdiff=[-8.82, 4.52]
boxplot(b1 ~ tap, data=fits)
t.test(fits$b1[fits$tap==0], fits$b1[fits$tap==2], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b1[fits$tap==0]) - mean(fits$b1[fits$tap==2])) / sd(fits$b1)
# Quadratic Slope (*)
# t(182)=-2.29, p=.023, d=0.330, diff=2.27, M=-15.21 | -6.90, CIdiff=[-15.49, -1.14]
boxplot(b2 ~ tap, data=fits)
t.test(fits$b2[fits$tap==0], fits$b2[fits$tap==2], paired=F, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b2[fits$tap==0]) - mean(fits$b2[fits$tap==2])) / sd(fits$b2)

# Intercept (Main effect of tapping) (n.s)
# NTI condition's average intercept is guaranteed to be approximately 0; TI-YT condition's intercept may not equal 0
# if the participant didn't tap on every trial.
# t(76)=-0.59, p=.556, d=0.067, M=-0.30, CI=[-1.29, 0.70]
boxplot(a ~ tap, data=fits)
t.test(fits$a[tap==2], mu=0, conf.level=.95, alternative="two.sided")
mean(fits$a[tap==2], na.rm=T) / sd(fits$a[tap==2], na.rm=T)