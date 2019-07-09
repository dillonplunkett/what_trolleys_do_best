# Reanalysis code for "Overlooked Evidence and a Misunderstanding of What
# Trolley Dilemmas Do Best: A Comment on Bostyn, Sevenhant, & Roets (2018)" by
# Dillon Plunkett and Joshua D. Greene

# Lines 30-71 are excerpts of the relevant parts of the code used by Bostyn,
# Sevenhant, & Roets (2018). Lines 77-88 are our reanalysis code.

# Bostyn et al.'s data can be downloaded from https://osf.io/42ptk/ and must
# be placed in your working directory to run this script.

# To reproduce our analyses exactly, run this script with R version 3.3.2 and
# brms 2.3.1. More recent versions of R and the brms package can produce
# slightly different output values for the Bayesian analyses (for both Bostyn
# et al.'s analyses and for ours).

# One way to obtain version 2.3.1 of brms is to use checkpoint:
# https://github.com/RevolutionAnalytics/checkpoint. To do so, ensure that
# your working directory contains only this file and the data file, then run
# the commented-out code below.
# if (!require(checkpoint, quietly = TRUE))  {
#   install.packages("checkpoint")
#   library(checkpoint)
# }
# checkpoint("2018-07-09", R.version = "3.3.2")


##### Excerpts from Bostyn, Sevenhant, & Roets (2018) #####


#Setting options
options(contrasts=c("contr.sum","contr.poly"))

#Please make sure the correct data file is in your working directory
data <- read.table("Data.txt", sep = "\t", header = TRUE)

#Factorizing the relevant variables
data$STUDY <- factor(data$STUDY, levels = c("1","2"), labels = c("Real", "Hypothetical"))
data$GENDER <- factor(data$GENDER, levels = c("0","1"), labels = c("Female","Male"))

data$DECISION <- factor(data$DECISION, levels = c("0","1"), labels = c("DEO","UTI"))

#Splitting the two datafiles into seperate dataframes per Study
#Do note that study1 includes some subjects that did not complete the 'real life' experiment and thus contains some NA values
study1 <- data[data$STUDY == "Real",]
study2 <- data[data$STUDY == "Hypothetical",]

#Main Analysis: Do moral preferences predict judgment on the 'real life' dilemma?

  #Controlled for Gender & Age differences (reported in the manuscript)
  fit.main3 <- glm(DECISION ~ CON + DEO + GENDER + AGE, family = binomial("logit"), data = study1)
  summary(fit.main3)

#Supplementary Analyses#

  #Bayes Factor
  library(brms)  #Do note that to replicate these analyses some additional programs might need to be installed
                 #Please check the online documentation of the brms package for assistance.

  set.seed(0)    #Setting seed for replicability

  prior <- set_prior("student_t(3,0,2.5)", class = "b")   #prior was chose as per the current recommendations
                                                          #of the Stan team

  bayes.fit1 <- brm(DECISION ~ CON + DEO + GENDER + AGE, family="bernoulli", data= study1, prior = prior, sample_prior = TRUE)
  bayes.fit1
  plot(bayes.fit1)

  hyp1 <- hypothesis(bayes.fit1, "CON = 0")
  hyp2 <- hypothesis(bayes.fit1, "DEO = 0")
  hyp1
  hyp2


##### Reanalysis #####


# Repeat main analysis using a difference score.
study1$diff <- study1$CON - study1$DEO
diff_lm <- glm(DECISION ~ diff, family = binomial("logit"), data = study1)
summary(diff_lm)
diff_lm_gender_age <- glm(DECISION ~ diff + GENDER + AGE, family = binomial("logit"), data = study1)
summary(diff_lm_gender_age)

# Perform Bayesian analysis with the difference score.
set.seed(0)
study1$diff_scaled <- (study1$diff - mean(study1$diff)) / (2 * sd(study1$diff))  # Scale per Gelman et al. (2008).
bayes_diff <- brm(DECISION ~ diff_scaled + GENDER + AGE, family="bernoulli", data= study1, prior = prior, sample_prior = TRUE)
hypothesis(bayes_diff, "diff_scaled = 0")
