library(ICSNP)  # For HotellingsT2
library(rstatix)  # For anova_test
library(ggpubr)  # For ggqqplot
library(tidyverse)  # For all things tidy
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
full_data$tempo_range <- as.factor(full_data$tempo_range)
full_data$loudness <- as.factor(full_data$loudness)
full_data$type <- as.factor(full_data$type)
str(full_data)

# Filter out outlier trials
data <- filter(full_data, !(cooks > 4/180))
n_dropped <- count(full_data) - count(data)
perc_dropped <- n_dropped / count(full_data) * 100
print(n_dropped)  # 709 dropped
print(perc_dropped)  # 5.12% dropped

###
# RAW TEMPO RATINGS
###

# Test subject fits against ground truth
subj_avgs <- group_by(data, subject) %>%
  summarize(slope = mean(slope), intercept = mean(intercept))
plot(subj_avgs$intercept, subj_avgs$slope)

# F(2, 75)=2.13, p=.126, pes=0.054
T2 <- HotellingsT2(subj_avgs[, c('slope', 'intercept')], mu=c(50, 50), na.action=drop, test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])

###
# LOUDNESS CONTROL
###

# Get subject averages
subj_avgs <- group_by(data, subject, loudness) %>%
  summarize(residual = mean(residual, na.rm=T), illusory_tempo = mean(illusory_tempo, na.rm=T))
# Descriptive stats by condition -3:M=-.13, SD=2.96; +0:M=0.36, SD=3.20; +3:M=-0.23, SD=2.72
group_means <- aggregate(subj_avgs$illusory_tempo, list(subj_avgs$loudness), mean)
group_sds <- aggregate(subj_avgs$illusory_tempo, list(subj_avgs$loudness), sd)
subj_avgs <- ungroup(subj_avgs)

# Repeated measures ANOVA (n.s.)
# Sphericity not violated (p=.265)
# rmANOVA: F(2, 152)=0.57, p=.567, pes=.007
anova_test(data=subj_avgs, dv=illusory_tempo, wid=subject, within=loudness, effect.size="pes", type=3)

###
# EFFECT OF PITCH
###

s <- c()
a <- c()
b1 <- c()
b2 <- c()
for (subj in unique(data$subject)) {
  mask <- (data$subject==subj)
  model <- lm(illusory_tempo ~ 1 + poly(pitch, 2), data=data[mask,])
  s <- append(s, subj)
  a <- append(a, model$coefficients[1])
  b1 <- append(b1, model$coefficients[2])
  b2 <- append(b2, model$coefficients[3])
}
fits <- data.frame(s, a, b1, b2)
fits$s <- as.factor(fits$s)
write.csv(fits, '../data/pitch_fits.csv', row.names=F)
plot(fits$b1, fits$b2)

# Intercepts are all approximately 0 due to already regressing out subject intercepts during calculation of
# residual tempo ratings. Compare the model slopes to 0 with Hotelling's T-squared.
# F(2, 75) = 14.74, p < .001, pes = 0.282
T2 <- HotellingsT2(fits[, c('b1', 'b2')], mu=rep(0, 2), na.action=drop, test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])

# Post-hoc one sample t-tests on slopes of each order - testing is Holm-Bonferroni corrected to determine significance
# Linear Slope (*)
# t(76)=2.12, p=.037 (.037), d=0.242, M=6.82, CI=[0.42, 13.21]
ggqqplot(fits$b1)
t.test(fits$b1, mu=0, conf.level=.95, alternative="two.sided")
mean(fits$b1) / sd(fits$b1)
# Quadratic Slope (***)
# t(76)=-4.87, p<.001 (<.001), d=0.555, M=-14.39, CI=[-20.28, -8.50]
ggqqplot(fits$b2)
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
      model <- lm(illusory_tempo ~ 1 + poly(pitch, 2), data=data[mask,])
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
write.csv(fits, '../data/timbre_fits.csv', row.names=F)
plot(fits$a, fits$b1)
plot(fits$a, fits$b2)
plot(fits$b1, fits$b2)

# Effect of Tone Type on Models (**)
# F(3, 74)=5.09, p=.003 pes=0.282
diff <- fits[fits$type=='p', c('a', 'b1', 'b2')] - fits[fits$type=='a', c('a', 'b1', 'b2')]
T2 <- HotellingsT2(diff, mu=rep(0, 3), na.action=drop, test='f')
print(T2)
T2$parameter[['df1']] * T2$statistic[[1]] / (T2$parameter[['df1']] * T2$statistic[[1]] + T2$parameter[['df2']])

# Intercept (Main effect of tone type) (**)
# t(76)=-3.38, p=.001 (.003), d=0.720, diff=-1.60, CIdiff=[-2.54, -0.66]
boxplot(a ~ type, data=fits)
t.test(fits$a[fits$type=='p'], fits$a[fits$type=='a'], paired=T, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$a[fits$type=='p']) - mean(fits$a[fits$type=='a'])) / sd(fits$a)

# Linear Slope (.)
# t(76)=-2.20, p=.031 (.061), d=0.361, diff=-10.64, CIdiff=[-20.27, -1.01]
boxplot(b1 ~ type, data=fits)
t.test(fits$b1[fits$type=='p'], fits$b1[fits$type=='a'], paired=T, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b1[fits$type=='p']) - mean(fits$b1[fits$type=='a'])) / sd(fits$b1)

# Quadratic Slope (n.s.)
# t(76)=1.00, p=.320, d=0.153, diff=3.82, CIdiff=[-3.78, 11.42]
boxplot(b2 ~ type, data=fits)
t.test(fits$b2[fits$type=='p'], fits$b2[fits$type=='a'], paired=T, var.equal=T, conf.level=.95, alternative="two.sided")
(mean(fits$b2[fits$type=='p']) - mean(fits$b2[fits$type=='a'])) / sd(fits$b2)