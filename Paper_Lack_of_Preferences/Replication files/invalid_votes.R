#######################################################################
## Replication files for "Invalid Votes, Political Efficacy          ##
## and Lack of Preferences in Brazilian Elections" (2015)            ##
## by Guilherme Arbache, Danilo Freire and Pietro Rodrigues          ##
## This script was written by Danilo Freire (danilofreire@gmail.com) ##
#######################################################################

# Clear the workspace
rm(list=ls())

# Load necessary packages
library(MCMCpack) # Bayesian inference
library(RCurl)    # download data

# Download data set from GitHub (file SHA: 7c38c37d8340d2f6616db301a30b5696ea17dd6d)
github.url <- getURL("https://raw.githubusercontent.com/danilofreire/replication-files/master/2015/invalid-votes/data.csv")
df <- read.csv(text = github.url, header = TRUE)

# Remove facultative votes (below 18 and above 70 years old under the Brazilian legislation)
df <- subset(df, age >= 18 & age <= 70)

# Table 1: Descriptive statistics
summary(df)

# Table 2: Invalid Votes for President, 1st and 2nd rounds. 
# A weakly informative multivariate normal prior was specified for all parameters
summary(model1 <- MCMClogit(inv_pres1 ~ houseincome + education + efficacyvote + partyeval + knowledge, 
                            b0 = 0, B0 = 0.001, verbose = FALSE, data = df))

summary(model2 <- MCMClogit(inv_pres2 ~ houseincome + education + efficacyvote + partyeval + knowledge,
                            b0 = 0, B0 = 0.001, verbose = FALSE, data = df))

summary(model3 <- MCMClogit(inv_gov ~ houseincome + education + efficacyvote + partyeval + knowledge,
                            b0 = 0, B0 = 0.001, verbose = FALSE, data = df))

summary(model4 <- MCMClogit(inv_federal ~ houseincome + education + efficacyvote + partyeval + knowledge,
                            b0 = 0, B0 = 0.001, verbose = FALSE, data = df))

summary(model5 <- MCMClogit(inv_estadual ~ houseincome + education + efficacyvote + partyeval + knowledge,
                            b0 = 0, B0 = 0.001, verbose = FALSE, data = df))

# Table 3: Different specifications for the evaluation of institutions
summary(model6 <- MCMClogit(inv_pres1 ~ houseincome + education + efficacyvote + govevaluation + knowledge + pref_part,
                            b0 = 0, B0 = 0.001, verbose = FALSE, data = df))

summary(model7 <- MCMClogit(inv_pres2 ~ houseincome + education + efficacyvote + govevaluation + knowledge + pref_cand,
                            b0 = 0, B0 = 0.001, verbose = FALSE, data = df))

summary(model8 <- MCMClogit(inv_federal ~ houseincome + education + efficacyvote + congressev + knowledge + pref_part,
                            b0 = 0, B0 = 0.001, verbose = FALSE, data = df))
