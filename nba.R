# This script performs the econometric analyses for the NBA hand check rule paper

# Loading libraries

library(stargazer)
library(margins)
library(sandwich)
library(tidyverse)
library(ggplot2)
library(AER)
library(lmtest)

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

lpm00c <- coeftest(lpm00, vcov = vcovCL, cluster = ~Season)
lpm0c <- coeftest(lpm0, vcov = vcovCL, cluster = ~Season)
lpm1c <- coeftest(lpm1, vcov = vcovCL, cluster = ~Season)
lpm2c <- coeftest(lpm2, vcov = vcovCL, cluster = ~Season)
lpm3c <- coeftest(lpm3, vcov = vcovCL, cluster = ~Season)
lpm4c <- coeftest(lpm4, vcov = vcovCL, cluster = ~Season)

# Repeat for 2 year window

blpm00 <- lm(Injured ~ Guard*Post, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm0 <- lm(Injured ~ Guard*Post + Height + Weight + Age, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm1 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience | . - Experience + Priors, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm2 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm3 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm4 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) + factor(College) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])

blpm00c <- coeftest(blpm00, vcov = vcovCL, cluster = ~Season)
blpm0c <- coeftest(blpm0, vcov = vcovCL, cluster = ~Season)
blpm1c <- coeftest(blpm1, vcov = vcovCL, cluster = ~Season)
blpm2c <- coeftest(blpm2, vcov = vcovCL, cluster = ~Season)
blpm3c <- coeftest(blpm3, vcov = vcovCL, cluster = ~Season)
blpm4c <- coeftest(blpm4, vcov = vcovCL, cluster = ~Season)

# Repeat for 1 year window

alpm00 <- lm(Injured ~ Guard*Post, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm0 <- lm(Injured ~ Guard*Post + Height + Weight + Age, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm1 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience | . - Experience + Priors, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm2 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm3 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm4 <- ivreg(Injured ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) + factor(College) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])

alpm00c <- coeftest(alpm00, vcov = vcovCL, cluster = ~Season)
alpm0c <- coeftest(alpm0, vcov = vcovCL, cluster = ~Season)
alpm1c <- coeftest(alpm1, vcov = vcovCL, cluster = ~Season)
alpm2c <- coeftest(alpm2, vcov = vcovCL, cluster = ~Season)
alpm3c <- coeftest(alpm3, vcov = vcovCL, cluster = ~Season)
alpm4c <- coeftest(alpm4, vcov = vcovCL, cluster = ~Season)

# Checking the instrument

summary(lpm1, diagnostics = TRUE) # This holds for all of the IV models

# Repeating for the lane-penetration specific injuries

# Creating a new variable

nba$Other.Injury <- nba$Injured - nba$Injured2

# Running LPMs

lpm002 <- lm(Injured2 ~ Guard*Post, data = nba)
lpm02 <- lm(Injured2 ~ Guard*Post + Height + Weight + Age, data = nba)
lpm12 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience | . - Experience + Priors, data = nba)
lpm22 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) | . - Experience + Priors, data = nba)
lpm32 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) | . - Experience + Priors, data = nba)
lpm42 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) + factor(College) | . - Experience + Priors, data = nba)

lpm002c <- coeftest(lpm002, vcov = vcovCL, cluster = ~Season)
lpm02c <- coeftest(lpm02, vcov = vcovCL, cluster = ~Season)
lpm12c <- coeftest(lpm12, vcov = vcovCL, cluster = ~Season)
lpm22c <- coeftest(lpm22, vcov = vcovCL, cluster = ~Season)
lpm32c <- coeftest(lpm32, vcov = vcovCL, cluster = ~Season)
lpm42c <- coeftest(lpm42, vcov = vcovCL, cluster = ~Season)

# Repeat for 2 year window

blpm002 <- lm(Injured2 ~ Guard*Post, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm02 <- lm(Injured2 ~ Guard*Post + Height + Weight + Age, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm12 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience | . - Experience + Priors, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm22 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm32 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])
blpm42 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) + factor(College) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2002-2003', '2003-2004', '2004-2005', '2005-2006')),])

blpm002c <- coeftest(blpm002, vcov = vcovCL, cluster = ~Season)
blpm02c <- coeftest(blpm02, vcov = vcovCL, cluster = ~Season)
blpm12c <- coeftest(blpm12, vcov = vcovCL, cluster = ~Season)
blpm22c <- coeftest(blpm22, vcov = vcovCL, cluster = ~Season)
blpm32c <- coeftest(blpm32, vcov = vcovCL, cluster = ~Season)
blpm42c <- coeftest(blpm42, vcov = vcovCL, cluster = ~Season)

# Repeat for 1 year window

alpm002 <- lm(Injured2 ~ Guard*Post, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm02 <- lm(Injured2 ~ Guard*Post + Height + Weight + Age, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm12 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience | . - Experience + Priors, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm22 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm32 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])
alpm42 <- ivreg(Injured2 ~ Guard*Post + Height + Weight + Age + Experience + factor(Season) + factor(Country) + factor(College) | . - Experience + Priors, data = nba[which(nba$Season %in% c('2003-2004', '2004-2005')),])

alpm002c <- coeftest(alpm002, vcov = vcovCL, cluster = ~Season)
alpm02c <- coeftest(alpm02, vcov = vcovCL, cluster = ~Season)
alpm12c <- coeftest(alpm12, vcov = vcovCL, cluster = ~Season)
alpm22c <- coeftest(alpm22, vcov = vcovCL, cluster = ~Season)
alpm32c <- coeftest(alpm32, vcov = vcovCL, cluster = ~Season)
alpm42c <- coeftest(alpm42, vcov = vcovCL, cluster = ~Season)

# Make some pretty plots

# Pre/post trend analysis

sumdat <- nba %>% 
  group_by(Season, Guard) %>% 
  summarize(Injured = mean(Injured))

a <- sumdat[which(sumdat$Guard == 1),]
b <- sumdat[which(sumdat$Guard == 0),]
c <- as.numeric(substr(a$Season,0,4))
did <- data.frame(Guard = as.numeric(a$Injured), Other = as.numeric(b$Injured), Season = as.numeric(c))

ggplot() + geom_line(data = did[14:17,], aes(x = Season, y = Guard, color = 'red')) +
  geom_line(data = did[14:17,], aes(x = Season, y = Other, color = 'blue')) +
  ggtitle('Injury Rate for Guards v. Forwards & Centers by Year') +
  ylab('Injury Rate') +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0.35,.65) + scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  geom_vline(xintercept = 2003)

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

ggplot() + geom_line(data = did2[14:17,], aes(x = Season, y = Guard, color = 'red')) +
  geom_line(data = did2[14:17,], aes(x = Season, y = Other, color = 'blue')) +
  ggtitle('Specific Injury Rate for Guards v. Forwards & Centers by Year') +
  ylab('Injury Rate') +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0.25,.45) + scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  geom_vline(xintercept = 2003)

dev.copy(png, paste(filepath, 'NBA2.png', sep = ''))
dev.off()

# A histogram of the Prior Injury variable

hist(nba$Priors, main = 'A Histogram of Prior Injury Seasons for NBA Players', xlab = 'Prior Injury Seasons')
dev.copy(png, paste(filepath, 'priors_hist.png', sep = ''))
dev.off()

# New models from peer review

# Injuries to Guards 

# One year window

base <- nba[which(nba$Guard == 1),]
pre <- base[which(base$Season == '2003-2004'),]
post <- base[which(base$Season == '2004-2005'),]

ng.pre <- length(unique(pre$Player))
ng.post <- length(unique(post$Player))

pre.inj <- sum(pre$Injured)
pre.inj2 <- sum(pre$Injured2)

post.inj <- sum(post$Injured)
post.inj2 <- sum(post$Injured2)

diff <- post.inj - pre.inj
diff2 <- post.inj2 - pre.inj2

weighted_diff <- (post.inj / ng.post) - (pre.inj / ng.pre)

# Two year window

base2 <- nba[which(nba$Guard == 1),]
pre2 <- base2[which(base2$Season %in% c('2002-2003', '2003-2004')),]
post2 <- base2[which(base2$Season %in% c('2004-2005', '2005-2006')),]

ng.pre2 <- length(unique(pre2$Player))
ng.post2 <- length(unique(post2$Player))

xpre.inj <- sum(pre2$Injured)
xpre.inj2 <- sum(pre2$Injured2)

xpost.inj <- sum(post2$Injured)
xpost.inj2 <- sum(post2$Injured2)

xdiff <- xpost.inj - xpre.inj
xdiff2 <- xpost.inj2 - xpre.inj2

weighted_xdiff <- (xpost.inj / ng.post2) - (xpre.inj / ng.pre2)

# Proportion of Injuries to Guards

# One year window

base <- nba[which(nba$Guard == 1),]
cf <- nba[which(nba$Guard == 0),]
pre <- base[which(base$Season == '2003-2004'),]
post <- base[which(base$Season == '2004-2005'),]
precf <- cf[which(base$Season == '2003-2004'),]
postcf <- cf[which(base$Season == '2004-2005'),]

ppre.inj <- sum(pre$Injured) / sum(precf$Injured)
ppre.inj2 <- sum(pre$Injured2) / sum(precf$Injured2)

ppost.inj <- sum(post$Injured)  / sum(postcf$Injured)
ppost.inj2 <- sum(post$Injured2) / sum(postcf$Injured2)

pdiff <- ppost.inj - ppre.inj
pdiff2 <- ppost.inj2 - ppre.inj2

# Two year window

base2 <- nba[which(nba$Guard == 1),]
cf2 <- nba[which(nba$Guard == 0),]
pre2 <- base2[which(base2$Season %in% c('2002-2003', '2003-2004')),]
post2 <- base2[which(base2$Season %in% c('2004-2005', '2005-2006')),]
precf2 <- cf2[which(cf2$Season %in% c('2002-2003', '2003-2004')),]
postcf2 <- cf2[which(cf2$Season %in% c('2004-2005', '2005-2006')),]

pxpre.inj <- sum(pre2$Injured) / sum(precf2$Injured)
pxpre.inj2 <- sum(pre2$Injured2) / sum(precf2$Injured2)

pxpost.inj <- sum(post2$Injured) / sum(postcf2$Injured)
pxpost.inj2 <- sum(post2$Injured2) / sum(postcf2$Injured2)

pxdiff <- pxpost.inj - pxpre.inj
pxdiff2 <- pxpost.inj2 - pxpre.inj2

# Read in scoring data as a means of identifying a causal mechanism

scor <- read.csv(paste(filepath, 'shooting_data.csv', sep = ''))

# Looking at changes in scoring due to the hand check rule

gs <- scor[which(scor$Guard == 1),]
os <- scor[which(scor$Guard == 0),]

# 1 year window effect on shot type (in the paint or not)

a <- gs[which(gs$SHOT_ZONE_BASIC %in% c('In The Paint (Non-RA)', 'Restricted Area')),]
a <- a[which(a$Season == '2003-04'),]
acf <- gs[which(gs$Season == '2003-04'),]
ax <- length(unique(acf$PLAYER_NAME))

b <- gs[which(gs$SHOT_ZONE_BASIC %in% c('In The Paint (Non-RA)', 'Restricted Area')),]
b <- b[which(b$Season == '2004-05'),]
bcf <- gs[which(gs$Season == '2004-05'),]
bx <- length(unique(bcf$PLAYER_NAME))

g.paint.diff <- (dim(b)[1] / dim(bcf)[1]) - (dim(a)[1] / dim(acf)[1])
g.paint.diffx <- dim(b)[1] - dim(a)[1]

g.paint.diffxx <- (dim(b)[1] / ng.post) - (dim(a)[1] / ng.pre)

# 2 year window effect on shot type (in the paint or not)

a2 <- gs[which(gs$SHOT_ZONE_BASIC %in% c('In The Paint (Non-RA)', 'Restricted Area')),]
a2 <- a2[which(a2$Season %in% c('2002-03', '2003-04')),]
acf2 <- gs[which(gs$Season %in% c('2002-03', '2003-04')),]

b2 <- gs[which(gs$SHOT_ZONE_BASIC %in% c('In The Paint (Non-RA)', 'Restricted Area')),]
b2 <- b2[which(b2$Season %in% c('2004-05', '2005-06')),]
bcf2 <- gs[which(gs$Season %in% c('2004-05', '2005-06')),]

g.paint.diff2 <- (dim(b2)[1] / dim(bcf2)[1]) - (dim(a2)[1] / dim(acf2)[1])
g.paint.diff2x <- dim(b2)[1] - dim(a2)[1]

g.paint.diffxx2 <- (dim(b2)[1] / ng.post2) - (dim(a2)[1] / ng.pre2)

# 1 year window effect on shot type (2 v 3) as pct

g2pre <- gs[which(gs$SHOT_TYPE == '2PT Field Goal'),]
g2pre <- g2pre[which(g2pre$Season == '2003-04'),]
g3pre <- gs[which(gs$SHOT_TYPE == '3PT Field Goal'),]
g3pre <- g3pre[which(g3pre$Season == '2003-04'),]

g2post <- gs[which(gs$SHOT_TYPE == '2PT Field Goal'),]
g2post <- g2post[which(g2post$Season == '2004-05'),]
g3post <- gs[which(gs$SHOT_TYPE == '3PT Field Goal'),]
g3post <- g3post[which(g3post$Season == '2004-05'),]

g2.diff <- (dim(g2post)[1] / (dim(g2post)[1] + dim(g3post)[1])) - (dim(g2pre)[1] / (dim(g2pre)[1] + dim(g3pre)[1]))

# 2 year window effect on shot type (2 v 3) as pct

g2prex <- gs[which(gs$SHOT_TYPE == '2PT Field Goal'),]
g2prex <- g2prex[which(g2prex$Season %in% c('2002-03', '2003-04')),]
g3prex <- gs[which(gs$SHOT_TYPE == '3PT Field Goal'),]
g3prex <- g3prex[which(g3prex$Season %in% c('2002-03', '2003-04')),]

g2postx <- gs[which(gs$SHOT_TYPE == '2PT Field Goal'),]
g2postx <- g2postx[which(g2postx$Season %in% c('2004-05', '2005-06')),]
g3postx <- gs[which(gs$SHOT_TYPE == '3PT Field Goal'),]
g3postx <- g3postx[which(g3postx$Season %in% c('2004-05', '2005-06')),]

g2.diffx <- (dim(g2postx)[1] / (dim(g2postx)[1] + dim(g3postx)[1])) - (dim(g2prex)[1] / (dim(g2prex)[1] + dim(g3prex)[1]))

# Percentage of shots by guards

pre1 <- scor[which(scor$Season %in% c('2003-04')),]
post1 <- scor[which(scor$Season %in% c('2004-05')),]
pre2 <- scor[which(scor$Season %in% c('2002-03', '2003-04')),]
post2 <- scor[which(scor$Season %in% c('2004-05', '2005-06')),]

g.pre.1 <- dim(pre1[which(pre1$Guard == 1),])[1] / (dim(pre1[which(pre1$Guard == 1),])[1] + dim(pre1[which(pre1$Guard == 0),])[1])
g.pre.2 <- dim(pre2[which(pre2$Guard == 1),])[1] / (dim(pre2[which(pre2$Guard == 1),])[1] + dim(pre2[which(pre2$Guard == 0),])[1])

g.post.1 <- dim(post1[which(post1$Guard == 1),])[1] / (dim(post1[which(post1$Guard == 1),])[1] + dim(post1[which(post1$Guard == 0),])[1])
g.post.2 <- dim(post2[which(post2$Guard == 1),])[1] / (dim(post2[which(post2$Guard == 1),])[1] + dim(post2[which(post2$Guard == 0),])[1])

diff.scor.1 <- g.post.1 - g.pre.1
diff.scor.2 <- g.post.2 - g.pre.2

g.shots.pre <- dim(pre1[which(pre1$Guard == 1),])[1] / ng.pre
g.shots.post <- dim(post1[which(post1$Guard == 1),])[1] / ng.post
g.shots.pre2 <- dim(pre2[which(pre2$Guard == 1),])[1] / ng.pre2
g.shots.post2 <- dim(post2[which(post2$Guard == 1),])[1] / ng.post2

g.shots <- g.shots.post - g.shots.pre
g.shots2 <- g.shots.post2 - g.shots.pre2

# Percentage of shots in the paint by guards

scor2 <- scor[which(scor$SHOT_ZONE_BASIC %in% c('In The Paint (Non-RA)', 'Restricted Area')),]

pre1 <- scor2[which(scor2$Season %in% c('2003-04')),]
post1 <- scor2[which(scor2$Season %in% c('2004-05')),]
pre2 <- scor2[which(scor2$Season %in% c('2002-03', '2003-04')),]
post2 <- scor2[which(scor2$Season %in% c('2004-05', '2005-06')),]

gp.pre.1 <- dim(pre1[which(pre1$Guard == 1),])[1] / (dim(pre1[which(pre1$Guard == 1),])[1] + dim(pre1[which(pre1$Guard == 0),])[1])
gp.pre.2 <- dim(pre2[which(pre2$Guard == 1),])[1] / (dim(pre2[which(pre2$Guard == 1),])[1] + dim(pre2[which(pre2$Guard == 0),])[1])

gp.post.1 <- dim(post1[which(post1$Guard == 1),])[1] / (dim(post1[which(post1$Guard == 1),])[1] + dim(post1[which(post1$Guard == 0),])[1])
gp.post.2 <- dim(post2[which(post2$Guard == 1),])[1] / (dim(post2[which(post2$Guard == 1),])[1] + dim(post2[which(post2$Guard == 0),])[1])

diff.paint.1 <- gp.post.1 - gp.pre.1
diff.paint.2 <- gp.post.2 - gp.pre.2

#############################################################

###################### MAIN RESULTS #########################

#############################################################

weighted_diff # Proportion of guards injured - 1 year window (+ 4.46%)
weighted_xdiff # Proportion of guards injured - 2 year window (+ 8.86%)

g.paint.diffxx # Shot attempts in the paint per guard - 1 year window (+ 12.07 shots/guard)
g.paint.diffxx2 # Shot attempts in the paint per guard - 2 year window ( + 17.51 shots/guard)

g.shots # Shot attempts per guard - 1 year window (+ 37.85 shots/guard)
g.shots2 # Shot attempts per guard - 2 year window (+ 39.65 shots/guard)

diff.paint.1 # Percentage of shots in the paint by guards - 1 year window (+ 0.70%)
diff.paint.2 # Percentage of shots in the paint by guards - 2 year window (+ 1.09%)

# Additional test - regression with 1 year window

players <- unique(scor$PLAYER_NAME)
players_check <- unique(nba$Player)
seasons <- unique(scor$Season)
seasons_check <- unique(nba$Season)[15:16]
injured <- c()
shots.in.paint <- c()
other.shots <- c()
seas <- c()
plays <- c()

for (p in players) {
  
  for (s in seasons) {
    
    if (p %in% players_check) {
      
      tmp <- scor[which(scor$PLAYER_NAME == p),]
      tmp <- tmp[which(tmp$Season == s),]
      tmpx <- tmp[which(tmp$SHOT_ZONE_BASIC %in% c('In The Paint (Non-RA)', 'Restricted Area')),]
      
      tmp2 <- nba[which(nba$Player == p),]
      tmp2 <- tmp2[which(tmp2$Season == seasons_check[which(seasons == s)]),]
      
      if (dim(tmp2)[1] > 0) {
        
        injured <- c(injured, max(tmp2$Injured))
        shots.in.paint <- c(shots.in.paint, dim(tmpx)[1])
        v <- dim(tmp)[1] - dim(tmpx)[1]
        other.shots <- c(other.shots, v)
        seas <- c(seas, s)
        plays <- c(plays, p)
        
      }
      
    }
    
  }
  
}

test1 <- lm(injured ~ shots.in.paint)
test1x <- lm(injured ~ shots.in.paint + other.shots)
test1xx <- lm(injured ~ shots.in.paint + other.shots + factor(seas))
test1xxx <- lm(injured ~ shots.in.paint + other.shots + factor(seas) + factor(plays))

# Additional test - regression with 2 year window

players <- unique(scor$PLAYER_NAME)
players_check <- unique(nba$Player)
seasons <- unique(scor$Season)
seasons_check <- unique(nba$Season)[14:17]
injured2 <- c()
shots.in.paint2 <- c()
other.shots2 <- c()
seas2 <- c()
plays2 <- c()

for (p in players) {
  
  for (s in seasons) {
    
    if (p %in% players_check) {
        
      tmp <- scor[which(scor$PLAYER_NAME == p),]
      tmp <- tmp[which(tmp$Season == s),]
      tmpx <- tmp[which(tmp$SHOT_ZONE_BASIC %in% c('In The Paint (Non-RA)', 'Restricted Area')),]
      
      tmp2 <- nba[which(nba$Player == p),]
      tmp2 <- tmp2[which(tmp2$Season == seasons_check[which(seasons == s)]),]
      
      if (dim(tmp2)[1] > 0) {
        
        injured2 <- c(injured2, max(tmp2$Injured))
        shots.in.paint2 <- c(shots.in.paint2, dim(tmpx)[1])
        v <- dim(tmp)[1] - dim(tmpx)[1]
        other.shots2 <- c(other.shots2, v)
        seas2 <- c(seas2, s)
        plays2 <- c(plays2, p)
        
      }
      
    }
    
  }
  
}

test2 <- lm(injured2 ~ shots.in.paint2)
test2x <- lm(injured2 ~ shots.in.paint2 + other.shots2)
test2xx <- lm(injured2 ~ shots.in.paint2 + other.shots2 + factor(seas2))
test2xxx <- lm(injured2 ~ shots.in.paint2 + other.shots2 + factor(seas2) + factor(plays2))

write.csv(stargazer(test1, test1x, test1xx, test1xxx, omit = c('plays', 'plays2')), paste(filepath, 'CM.txt', sep = ''))
stargazer(test1, test1x, test1xx, test1xxx, type = 'text', omit = c('plays', 'plays2'))

# Summary stats for shooring data

scorsum <- scor %>%
  group_by(PLAYER_NAME, Season) %>%
  count(PLAYER_ID)

scor2 <- scor[which(scor$SHOT_ZONE_BASIC %in% c('In The Paint (Non-RA)', 'Restricted Area')),]

scorsum2 <- scor2 %>%
  group_by(PLAYER_NAME, Season) %>%
  count(PLAYER_ID)












# Merge the two data sets + create vars

to.merge <- scor %>%
  group_by(PLAYER_NAME, Season) %>%
  summarize(across(c('SHOT_DISTANCE', 'SHOT_ATTEMPTED_FLAG', 'SHOT_MADE_FLAG'), c(mean, sum, mean)))

seas <- c()

for (i in 1:dim(to.merge)[1]) {
  
  if (to.merge$Season[i] == '2002-03')
    
    {seas <- c(seas, '2002-2003')} else
    
      if (to.merge$Season[i] == '2003-04') 
        
        {seas <- c(seas, '2003-2004')} else
          
          if (to.merge$Season[i] == '2004-05')
            
            {seas <- c(seas, '2004-2005')} else
              
              if (to.merge$Season[i] == '2005-06')
              
              {seas <- c(seas, '2005-2006')} else
                
                if (to.merge$Season[i] == '2006-07')
                  
                {seas <- c(seas, '2006-2007')} else
                  
                {seas <- c(seas, '2001-2002')}
                
  }

to.merge$Season2 <- seas
to.merge$PLAYER_YEAR <- paste(to.merge$PLAYER_NAME, to.merge$Season2, sep = ' ')
nba$Player_Year <- paste(nba$Player, nba$Season, sep = ' ')
merged <- merge(x = nba, y = to.merge, by.x = 'Player_Year', by.y = 'PLAYER_YEAR')
merged$Y12 <- as.numeric(merged$Season.x == '2001-2002') * -2
merged$Y23 <- as.numeric(merged$Season.x == '2002-2003') * -1
merged$Y34 <- as.numeric(merged$Season.x == '2003-2004') * 0
merged$Y45 <- as.numeric(merged$Season.x == '2004-2005') * 1
merged$Y56 <- as.numeric(merged$Season.x == '2005-2006') * 2
merged$Y67 <- as.numeric(merged$Season.x == '2006-2007') * 3
merged$BMI <- 703 * merged$Weight / (merged$Height * merged$Height)

