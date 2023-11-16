
library(data.table)
library(lmerTest)
library(lme4)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(forcats)
library(ggsci)
library(RColorBrewer)
library(optimx)
library(minqa)
library(dfoptim)
library(survey)
library(lavaan.survey)
library(Polychrome)
library(scales)
library(ggnewscale)
library(ggpubr)
library(gplots)
library(psych)
library(rtrim)

withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
} 

prs_baseline <- read_excel('/Users/aaron/Downloads/6_28_complete_ANOREXIA_OFF.xlsx') 

prs_baseline$site_id <- ifelse(prs_baseline$site_id == 22, 21, prs_baseline$site_id)
prs_baseline[prs_baseline == "NA"] <- NA
prs_baseline[prs_baseline == ""] <- NA


#Have to change here to the column names for the ordered categorical variables, this code changes to factor
prs_baseline[,c(1:4,20,182:194)] <- lapply(prs_baseline[,c(1:4,20,182:194)],as.factor)
prs_baseline[,c(5:19,21:181)] <- lapply(prs_baseline[,c(5:19,21:181)],as.numeric)
ncol(prs_baseline)
#Scale the numeric variables
prs_baseline <- prs_baseline %>%
  mutate_if(is.numeric, scale)

prs_baseline <- as.data.frame(prs_baseline)

attach(prs_baseline)


#non_global
First <- 27 
Last <- 94
#creates objects for the loop
Beta <- NULL
STE <- NULL
Area <- NULL
Pval <- NULL
i <-  NULL
Warn <- NULL
Results <- NULL
Sigs <- NULL
LMEOut <- NULL
a <- NULL
STE2 <- NULL
Beta2 <- NULL
Pval2 <- NULL
CIl <- NULL
CIu <- NULL
CIl_2 <- NULL
CIu_2 <- NULL
#Runs the for loop
for (i in First:Last) {
  print(i)
  LMEOut <- withWarnings( a <- lmer(prs_baseline[,i] ~ non_cog_score + cog_score +  C1 + C2 + C3 + 
  C4 + C5 + C6 + C7 + C8 + C9 + C10 + sex + interview_age + 
  smri_vol_scs_intracranialv + mri_info_manufacturer + 
(1|mri_info_deviceserialnumber) + (1|rel_family_id), control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))))
  Beta[i] <- summary(a)$`coefficients`[2]
  Beta[i] <- summary(a)$`coefficients`[2]
  STE[i] <- summary(a)$`coefficients`[2,2]
  Pval[i] <-  summary(a)$`coefficients`[2,5]
  Beta2[i] <- summary(a)$`coefficients`[3]
  STE2[i] <- summary(a)$`coefficients`[3,2]
  Pval2[i] <-  summary(a)$`coefficients`[3,5]
  Area[i] <- colnames(prs_baseline)[i]
  Warn[i] <- LMEOut$warnings
  CIl[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
  CIl_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
}

#Store results in a dataframe
Results4_CI <- cbind.data.frame(Area[First:length(Area)], Beta[First:length(Beta)], STE[First:length(STE)], Pval[First:length(Pval)],  Beta2[First:length(Beta2)], STE2[First:length(STE2)], Pval2[First:length(Pval2)], CIl[First:length(CIl)], CIu[First:length(CIu)], CIl_2[First:length(CIl_2)], CIu_2[First:length(CIu_2)])
#Relabel Results
colnames(Results4_CI) <- c("Variable", "Beta_non_cog", "STE_non_cog", "Pval_non_cog", "Beta_cog", "STE_cog", "Pval_cog", "lower_non_cog", "upper_non_cog", "lower_cog", "upper_cog")

First <- 104 
Last <- 140
#creates objects for the loop
Beta <- NULL
STE <- NULL
Area <- NULL
Pval <- NULL
i <-  NULL
Warn <- NULL
Results <- NULL
Sigs <- NULL
LMEOut <- NULL
a <- NULL
STE2 <- NULL
Beta2 <- NULL
Pval2 <- NULL
CIl <- NULL
CIu <- NULL
CIl_2 <- NULL
CIu_2 <- NULL
#Runs the for loop
for (i in First:Last) {
  print(i)
  LMEOut <- withWarnings( a <- lmer(prs_baseline[,i] ~ non_cog_score + cog_score +  C1 + C2 + C3 + C4 + C5
  + C6 + C7 + C8 + C9 + C10 + sex + interview_age 
  + dmri_dtimd_fiberat_allfibers + mri_info_manufacturer + 
    dmri_dti_meanmotion + (1|mri_info_deviceserialnumber) + 
    (1|rel_family_id), control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))))
  Beta[i] <- summary(a)$`coefficients`[2]
  STE[i] <- summary(a)$`coefficients`[2,2]
  Pval[i] <-  summary(a)$`coefficients`[2,5]
  Beta2[i] <- summary(a)$`coefficients`[3]
  STE2[i] <- summary(a)$`coefficients`[3,2]
  Pval2[i] <-  summary(a)$`coefficients`[3,5]
  Area[i] <- colnames(prs_baseline)[i]
  Warn[i] <- LMEOut$warnings
  CIl[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
  CIl_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
}

#Store results in a dataframe
Results3_CI <- cbind.data.frame(Area[First:length(Area)], Beta[First:length(Beta)], STE[First:length(STE)], Pval[First:length(Pval)],  Beta2[First:length(Beta2)], STE2[First:length(STE2)], Pval2[First:length(Pval2)], CIl[First:length(CIl)], CIu[First:length(CIu)], CIl_2[First:length(CIl_2)], CIu_2[First:length(CIu_2)])
#Relabel Results
colnames(Results3_CI) <- c("Variable", "Beta_non_cog", "STE_non_cog", "Pval_non_cog", "Beta_cog", "STE_cog", "Pval_cog", "lower_non_cog", "upper_non_cog", "lower_cog", "upper_cog")

Results <- rbind(Results4_CI, Results3_CI)

First <- 141 
Last <- 177
#creates objects for the loop
Beta <- NULL
STE <- NULL
Area <- NULL
Pval <- NULL
i <-  NULL
Warn <- NULL
Results <- NULL
Sigs <- NULL
LMEOut <- NULL
a <- NULL
STE2 <- NULL
Beta2 <- NULL
Pval2 <- NULL
CIl <- NULL
CIu <- NULL
CIl_2 <- NULL
CIu_2 <- NULL
#Runs the for loop
for (i in First:Last) {
  print(i)
  LMEOut <- withWarnings( a <- lmer(prs_baseline[,i] ~ non_cog_score + cog_score +  C1 + C2 + C3 + C4 + C5
                                    + C6 + C7 + C8 + C9 + C10 + sex + interview_age 
                                    + dmri_dtifa_fiberat_allfibers + mri_info_manufacturer + 
                                      dmri_dti_meanmotion + (1|mri_info_deviceserialnumber) + 
                                      (1|rel_family_id), control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))))
  Beta[i] <- summary(a)$`coefficients`[2]
  STE[i] <- summary(a)$`coefficients`[2,2]
  Pval[i] <-  summary(a)$`coefficients`[2,5]
  Beta2[i] <- summary(a)$`coefficients`[3]
  STE2[i] <- summary(a)$`coefficients`[3,2]
  Pval2[i] <-  summary(a)$`coefficients`[3,5]
  Area[i] <- colnames(prs_baseline)[i]
  Warn[i] <- LMEOut$warnings
  CIl[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
  CIl_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
}
Results5_CI <- cbind.data.frame(Area[First:length(Area)], Beta[First:length(Beta)], STE[First:length(STE)], Pval[First:length(Pval)],  Beta2[First:length(Beta2)], STE2[First:length(STE2)], Pval2[First:length(Pval2)], CIl[First:length(CIl)], CIu[First:length(CIu)], CIl_2[First:length(CIl_2)], CIu_2[First:length(CIu_2)])
#Relabel Results
colnames(Results5_CI) <- c("Variable", "Beta_non_cog", "STE_non_cog", "Pval_non_cog", "Beta_cog", "STE_cog", "Pval_cog", "lower_non_cog", "upper_non_cog", "lower_cog", "upper_cog")



Resultsd <- rbind(Results, Results5_CI)
fwrite(Resultsd, "CI_imaging_non_globals.csv")

##global 

First <- 95 
Last <- 103
#creates objects for the loop
Beta <- NULL
STE <- NULL
Area <- NULL
Pval <- NULL
i <-  NULL
Warn <- NULL
Results <- NULL
Sigs <- NULL
LMEOut <- NULL
a <- NULL
STE2 <- NULL
Beta2 <- NULL
Pval2 <- NULL
CIl <- NULL
CIu <- NULL
CIl_2 <- NULL
CIu_2 <- NULL
#Runs the for loop
for (i in First:Last) {
  print(i)
  LMEOut <- withWarnings( a <- lmer(prs_baseline[,i] ~ non_cog_score + cog_score +  C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + sex + interview_age + mri_info_manufacturer + (1|mri_info_deviceserialnumber) + (1|rel_family_id), control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))))
  Beta[i] <- summary(a)$`coefficients`[2]
  STE[i] <- summary(a)$`coefficients`[2,2]
  Pval[i] <-  summary(a)$`coefficients`[2,5]
  Beta2[i] <- summary(a)$`coefficients`[3]
  STE2[i] <- summary(a)$`coefficients`[3,2]
  Pval2[i] <-  summary(a)$`coefficients`[3,5]
  Area[i] <- colnames(prs_baseline)[i]
  Warn[i] <- LMEOut$warnings
  CIl[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
  CIl_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
}

#Store results in a dataframe
Results14_CI <- cbind.data.frame(Area[First:length(Area)], Beta[First:length(Beta)], STE[First:length(STE)], Pval[First:length(Pval)],  Beta2[First:length(Beta2)], STE2[First:length(STE2)], Pval2[First:length(Pval2)], CIl[First:length(CIl)], CIu[First:length(CIu)], CIl_2[First:length(CIl_2)], CIu_2[First:length(CIu_2)])
#Relabel Results
colnames(Results14_CI) <- c("Variable", "Beta_non_cog", "STE_non_cog", "Pval_non_cog", "Beta_cog", "STE_cog", "Pval_cog", "lower_non_cog", "upper_non_cog", "lower_cog", "upper_cog")


First <- 178 
Last <- 179
#creates objects for the loop
Beta <- NULL
STE <- NULL
Area <- NULL
Pval <- NULL
i <-  NULL
Warn <- NULL
Results <- NULL
Sigs <- NULL
LMEOut <- NULL
a <- NULL
STE2 <- NULL
Beta2 <- NULL
Pval2 <- NULL
CIl <- NULL
CIu <- NULL
CIl_2 <- NULL
CIu_2 <- NULL
#Runs the for loop
for (i in First:Last) {
  print(i)
  LMEOut <- withWarnings( a <- lmer(prs_baseline[,i] ~ non_cog_score + cog_score +  C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + sex + interview_age + mri_info_manufacturer + dmri_dti_meanmotion + (1|mri_info_deviceserialnumber) + (1|rel_family_id), control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))))
  Beta[i] <- summary(a)$`coefficients`[2]
  STE[i] <- summary(a)$`coefficients`[2,2]
  Pval[i] <-  summary(a)$`coefficients`[2,5]
  Beta2[i] <- summary(a)$`coefficients`[3]
  STE2[i] <- summary(a)$`coefficients`[3,2]
  Pval2[i] <-  summary(a)$`coefficients`[3,5]
  Area[i] <- colnames(prs_baseline)[i]
  Warn[i] <- LMEOut$warnings
  CIl[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu[i] <- confint(a, "non_cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
  CIl_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[1]
  CIu_2[i] <- confint(a, "cog_score", method = "boot", nsim = 200, parallel = "multicore", ncpus = 8)[2]
}
Results16_CI <- cbind.data.frame(Area[First:length(Area)], Beta[First:length(Beta)], STE[First:length(STE)], Pval[First:length(Pval)],  Beta2[First:length(Beta2)], STE2[First:length(STE2)], Pval2[First:length(Pval2)], CIl[First:length(CIl)], CIu[First:length(CIu)], CIl_2[First:length(CIl_2)], CIu_2[First:length(CIu_2)])
#Relabel Results
colnames(Results16_CI) <- c("Variable", "Beta_non_cog", "STE_non_cog", "Pval_non_cog", "Beta_cog", "STE_cog", "Pval_cog", "lower_non_cog", "upper_non_cog", "lower_cog", "upper_cog")

Resultw <- rbind(Results14_CI, Results16_CI)

fwrite(Resultw, "CI_global.csv")



