# This script performs the econometric analyses for the NBA hand check rule paper

# Loading libraries

library(stargazer)
library(margins)
library(sandwich)
library(tidyverse)
library(ggplot2)
library(AER)

# Reading in the raw data :: update the username / filepath variables accordingly!

username <- ''
filepath <- paste('C:/Users/', username, '/Documents/Data/NBA/', sep = '')
nba <- read.csv(paste(filepath, 'NBA.csv', sep = ''))
nba <- na.omit(nba)

# Running LPMs

lpm00 <- lm(Injured ~ Guard*Post, data = nba)
lpm0 <- lm(Injured ~ Guard*Post + Height + Weight + Age, data = nba)
lpm1 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience | . - Experience + Priors, data = nba)
lpm2 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) | . - Experience + Priors, data = nba)
lpm3 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) | . - Experience + Priors, data = nba)
lpm4 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) + factor(College) | . - Experience + Priors, data = nba)

cov00 <- vcovHC(lpm00, type = 'HC1')
cov0 <- vcovHC(lpm0, type = 'HC1')
cov1 <- vcovHC(lpm1, type = 'HC1')
cov2 <- vcovHC(lpm2, type = 'HC1')
cov3 <- vcovHC(lpm3, type = 'HC1')
cov4 <- vcovHC(lpm4, type = 'HC1')

rse00 <- sqrt(diag(cov00))
rse0 <- sqrt(diag(cov0))
rse1 <- sqrt(diag(cov1))
rse2 <- sqrt(diag(cov2))
rse3 <- sqrt(diag(cov3))
rse4 <- sqrt(diag(cov4))

write.csv(stargazer(lpm00, lpm0, lpm1, lpm2, lpm3, lpm4, se = list(rse00, rse0, rse1, rse2, rse3, rse4), omit = c('Season', 'Player', 'Country', 'College')),
          paste(filepath, 'LPM_results_tex.txt'), row.names = FALSE)
write.csv(stargazer(lpm00, lpm0, lpm1, lpm2, lpm3, lpm4, se = list(rse00, rse0, rse1, rse2, rse3, rse4), type = 'text', omit = c('Season', 'Player', 'Country', 'College')),
          paste(filepath, 'LPM_results.txt'), row.names = FALSE)

# Checking the instrument

summary(lpm1, diagnostics = TRUE) # This holds for all of the IV models

# Repeating for the lane-penetration specific injuries

# Creating a new variable

nba$Other.Injury <- nba$Injured - nba$Injured2

# Running LPMs

lpm002 <- lm(Injured2 ~ Guard*Post, data = nba)
lpm02 <- lm(Injured2 ~ Guard*Post + Height + Weight + Age + Other.Injury, data = nba)
lpm12 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Other.Injury + Experience | . - Experience + Priors, data = nba)
lpm22 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Other.Injury + Experience + factor(Season) | . - Experience + Priors, data = nba)
lpm32 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Other.Injury + Experience + factor(Season) + factor(Country) | . - Experience + Priors, data = nba)
lpm42 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Other.Injury + Experience + factor(Season) + factor(Country) + factor(College) | . - Experience + Priors, data = nba)

cov002 <- vcovHC(lpm002, type = 'HC1')
cov02 <- vcovHC(lpm02, type = 'HC1')
cov12 <- vcovHC(lpm12, type = 'HC1')
cov22 <- vcovHC(lpm22, type = 'HC1')
cov32 <- vcovHC(lpm32, type = 'HC1')
cov42 <- vcovHC(lpm42, type = 'HC1')

rse002 <- sqrt(diag(cov002))
rse02 <- sqrt(diag(cov02))
rse12 <- sqrt(diag(cov12))
rse22 <- sqrt(diag(cov22))
rse32 <- sqrt(diag(cov32))
rse42 <- sqrt(diag(cov42))

write.csv(stargazer(lpm002, lpm02, lpm12, lpm22, lpm32, lpm42, se = list(rse002, rse02, rse12, rse22, rse32, rse42), omit = c('Season', 'Player', 'Country', 'College')),
          paste(filepath, 'LPM_results2_tex.txt'), row.names = FALSE)
write.csv(stargazer(lpm002, lpm02, lpm12, lpm22, lpm32, lpm42, se = list(rse002, rse02, rse12, rse22, rse32, rse42), type = 'text', omit = c('Season', 'Player', 'Country', 'College')),
          paste(filepath, 'LPM_results2.txt'), row.names = FALSE)

# Make some pretty plots

# Pre/post trend analysis

sumdat <- nba %>% 
  group_by(Season, Guard) %>% 
  summarize(Injured = mean(Injured))

a <- sumdat[which(sumdat$Guard == 1),]
b <- sumdat[which(sumdat$Guard == 0),]
c <- as.numeric(substr(a$Season,0,4))
did <- data.frame(Guard = as.numeric(a$Injured), Other = as.numeric(b$Injured), Season = as.numeric(c))

ggplot() + geom_line(data = did[6:30,], aes(x = Season, y = Guard, color = 'red')) +
  geom_line(data = did[6:30,], aes(x = Season, y = Other, color = 'blue')) +
  ggtitle('Injury Rate for Guards v. Forwards & Centers by Year') +
  ylab('Injury Rate') +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0.35,.65) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 2004)

dev.copy(png, paste(filepath, 'NBA.png', sep = ''))
dev.off()

# Pre/post trend analysis for specific injuries

sumdat2 <- nba %>% 
  group_by(Season, Guard) %>% 
  summarize(Injured2 = mean(Injured2))

a2 <- sumdat2[which(sumdat2$Guard == 1),]
b2 <- sumdat2[which(sumdat2$Guard == 0),]
c2 <- as.numeric(substr(a2$Season,0,4))
did2 <- data.frame(Guard = as.numeric(a2$Injured2), Other = as.numeric(b2$Injured2), Season = as.numeric(c2))

ggplot() + geom_line(data = did2[6:30,], aes(x = Season, y = Guard, color = 'red')) +
  geom_line(data = did2[6:30,], aes(x = Season, y = Other, color = 'blue')) +
  ggtitle('Injury Rate for Guards v. Forwards & Centers by Year') +
  ylab('Injury Rate') +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0.15,.45) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 2004)

dev.copy(png, paste(filepath, 'NBA2.png', sep = ''))
dev.off()

# Robustness checks

# Running logit models

log00 <- glm(Injured ~ Guard*Post, data = nba, family = binomial(link = 'logit'))
log0 <- glm(Injured ~ Guard*Post + Height + Weight + Age, data = nba, family = binomial(link = 'logit'))
log1 <- glm(Injured ~ Guard*Post + Height + Weight + Age + Experience, data = nba, family = binomial(link = 'logit'))
log2 <- glm(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season), data = nba, family = binomial(link = 'logit'))
log3 <- glm(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country), data = nba, family = binomial(link = 'logit'))
log4 <- glm(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) + factor(College), data = nba, family = binomial(link = 'logit'))


co00 <- vcovHC(log00, type = 'HC1')
co0 <- vcovHC(log0, type = 'HC1')
co1 <- vcovHC(log1, type = 'HC1')
co2 <- vcovHC(log2, type = 'HC1')
co3 <- vcovHC(log3, type = 'HC1')
co4 <- vcovHC(log4, type = 'HC1')

rs00 <- sqrt(diag(co00))
rs0 <- sqrt(diag(co0))
rs1 <- sqrt(diag(co1))
rs2 <- sqrt(diag(co2))
rs3 <- sqrt(diag(co3))
rs4 <- sqrt(diag(co4))

stargazer(log00, log0, log1, log2, log3, log4, se = list(rs00, rs0, rs1, rs2, rs3, rs4), type = 'text', omit = c('Player', 'Country', 'College'))

# Additional robustness checks included running on a window of \pm 10 years rather than 15









