require(ggplot2)
require(readxl)
require(tidyverse) 
require(lmerTest)
require(segmented)
require(effectsize)
require(emmeans)


# November 2022
# clear workspace
rm(list = ls())

# Template
theme_template<-theme(plot.title = element_text(size = 20, family = "Helvetica", face = "bold", hjust = 0.5), 
                      text = element_text(size = 20, family = "Helvetica", face = "bold"), 
                      axis.title = element_text(face = "bold"), 
                      axis.text.x = element_text(size = 20, face = "bold"), 
                      panel.border = element_blank(), legend.title = element_blank(), legend.position = "none")

# Are there outliers at the group level ?
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/ULB/Projects/19_AttenTrack/data/")
d <- read_xlsx("19_AttenTracking.xlsx")
outliers <- boxplot(baseline ~ audiometrie, data = d, range = 3)$out
outliers <- boxplot(TwoVoices ~ audiometrie, data = d, range = 3)$out
outliers <- boxplot(SiQ ~ audiometrie, data = d, range = 3)$out
outliers <- boxplot(SSN ~ audiometrie, data = d, range = 3)$out
outliers <- boxplot(Babble ~ audiometrie, data = d, range = 3)$out

# Outliers were manually removed and stored in a new Excel spreadsheet
d <- read_xlsx("19_AttenTracking_outliersRemoved.xlsx")
d <- subset(d, Music == "Nonmusician")
d <- subset(d, Age >= 8)
d <- subset(d, Age <= 22.9)

# Is the data normally distributed?
ks.test(d$baseline, "pnorm", mean = mean(d$baseline), sd = sd(d$baseline))
ks.test(d$TwoVoices, "pnorm", mean = mean(d$TwoVoices), sd = sd(d$TwoVoices))
ks.test(d$SiQ, "pnorm", mean = mean(d$SiQ, na.rm = TRUE), sd = sd(d$SiQ, na.rm = TRUE))
ks.test(d$SSN, "pnorm", mean = mean(d$SSN, na.rm = TRUE), sd = sd(d$SSN, na.rm = TRUE))
ks.test(d$Babble, "pnorm", mean = mean(d$Babble, na.rm = TRUE), sd = sd(d$Babble, na.rm = TRUE))

##############
### ATTENTIVE TRACKING
##############
# Check chance level
tt_baseline <- t.test(d$baseline, alternative = c("greater"), mu = 50)
tt_TwoVoices <- t.test(d$TwoVoices, alternative = c("greater"), mu = 50)
tt <- t.test(d$baseline, d$TwoVoices)

# Build dataframe for Attentive Tracking
AttenTrack <- data.frame("code" = d$code_sujet, "Gender" = d$Gender, "Age" = d$Age, 
                         "baseline" = d$baseline, "twoVoices" = d$TwoVoices)
AttenTrack <- gather(AttenTrack, key = "Condition", value = "Score", baseline, twoVoices)

# LME
m1 <- lm(TwoVoices ~ Age, data = d)
anova(m1)
F_to_eta2(f = c(27.87), df = c(1), df_error = c(193))

# Broken stick regression  (see: https://www.statology.org/piecewise-regression-in-r/)
df <- data.frame(Age = d$Age, TwoVoices = d$TwoVoices)
df <- df[with(df, order(Age)), ]
fit <- lm(TwoVoices ~ Age, data=df)
segmented.fit <- segmented(fit, seg.Z = ~Age, psi=16)
summary(segmented.fit)
anova(fit, segmented.fit)

F_to_eta2(f = c(4.89), df = c(2), df_error = c(191))

plot(df$Age, df$TwoVoices, pch=16, col='steelblue')
plot(segmented.fit, add=T)

##############
### SiN
##############
# Check chance level
tt_Quiet <- t.test(d$SiQ, alternative = c("less"), mu = 100)
tt_SSN <- t.test(d$SSN, alternative = c("less"), mu = 100)
tt_babble <- t.test(d$Babble, alternative = c("less"), mu = 100)

# Developmental effect on SiQ/SSN/Babble ?
Noise <- gather(d, key = "Condition", value = "Score", SiQ, SSN, Babble)
m3 <- lmer(Score ~ Condition*Age*TwoVoices + (1|code_sujet), data = Noise)
anova(m3)
# model reduction 
sm <- step(m3, reduce.random = F)
fm <- get_model(sm)
anova(fm)

# Get the eta squares
F_to_eta2(f = c(48.51, 83.7, 19.80, 4.07), df = c(1, 2, 2, 1), df_error = c(175, 362, 362, 176))

# Probing the Age x Condition interaction (p < 0.001)
emtrends(m3, pairwise ~ Condition, var = "Age")

