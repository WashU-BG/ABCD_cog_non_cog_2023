library(lme4)
library(lmerTest)
library(dplyr)

setwd("/scratch/aalab/emmaj/Cog_noncog_resampling")

## read in ABCD dataset from Aaron
abcd <- read.csv("/scratch/aalab/emmaj/Cog_noncog_resampling/cog_non_cog_within_brain.csv",header=T)
head(abcd)
dim(abcd)

### dmri md
## initialize empty dataframe for average values across all outcomes
avgresults <- as.data.frame(matrix(nrow=2,ncol=7))
names(avgresults) <- c("Outcome","CogBeta","CogSE","CogP","NonCogBeta","NonCogSE","NonCogP")
avgresults$Outcome <- c("dmri_dtimd_fiberat_cstlh","dmri_dtimd_fiberat_cstrh")


k <- 0

### outer loop - through outcomes
for (i in c("dmri_dtimd_fiberat_cstlh","dmri_dtimd_fiberat_cstrh")) {
  
  k <- k+1
  print(i)
  
  ## pull out outcome i and covariates
  abcd2 <- abcd[,c(i,"id_redcap","rel_family_id","cog_score","non_cog_score","site_id","C1","C2","C3","C4","C5","C6",
                   "C7","C8","C9","C10","sex","interview_age","dmri_dtimd_fiberat_allfibers", "mri_info_manufacturer", "dmri_dti_meanmotion",
                   "mri_info_deviceserialnumber")]
  print(dim(abcd2))
  
  ## remove NAs
  abcd2 <- na.omit(abcd2)
  print(dim(abcd2))
  
  ## initialize empty matrix
  results <- as.data.frame(matrix(nrow=5000,ncol=6))
  names(results) <- c("CogBeta","CogSE","CogP","NonCogBeta","NonCogSE","NonCogP")
    
  ## inner loop: sampling 5,000 times
  for (j in 1:5000) { 
    
    
    abcd3 <- sample_n(abcd2, 1702, replace=FALSE)
    
    ## run regression 
    try(linmod <- lmer(scale(abcd3[,c(1)]) ~ scale(cog_score) + scale(non_cog_score) + sex + scale(interview_age) + scale(C1) + 
           scale(C2) + scale(C3) + scale(C4) + scale(C5) + scale(C6) + scale(C7) + scale(C8) + 
           scale(C9) + scale(C10) + (1|site_id) + dmri_dtimd_fiberat_allfibers + mri_info_manufacturer + dmri_dti_meanmotion + 
             (1|mri_info_deviceserialnumber) + (1|rel_family_id), data = abcd3, control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))),silent=TRUE)
    
    ## fill in results from regression model
    results$CogBeta[j] <- summary(linmod)$coefficients[2,1]
    results$CogSE[j] <- summary(linmod)$coefficients[2,2]
    results$CogP[j] <- summary(linmod)$coefficients[2,5]
    results$NonCogBeta[j] <- summary(linmod)$coefficients[3,1]
    results$NonCogSE[j] <- summary(linmod)$coefficients[3,2]
    results$NonCogP[j] <- summary(linmod)$coefficients[3,5]
  } 
  
  ## write out outcome-specific results table
  write.csv(results,file=paste0("sampled_results_for_",i),quote=FALSE,row.names=FALSE)
  
  ## calculate average estimates and fill in overall results table
  avgresults$CogBeta[k] <- mean(results$CogBeta)
  avgresults$CogSE[k] <- sd(results$CogBeta)
  cogz <- avgresults$CogBeta[k]/avgresults$CogSE[k]
  avgresults$CogP[k] <- 2*pnorm(abs(cogz),lower.tail=FALSE)
  
  avgresults$NonCogBeta[k] <- mean(results$NonCogBeta)
  avgresults$NonCogSE[k] <- sd(results$NonCogBeta)
  noncogz <- avgresults$NonCogBeta[k]/avgresults$NonCogSE[k]
  avgresults$NonCogP[k] <- 2*pnorm(abs(noncogz),lower.tail=FALSE)
  
}

write.csv(avgresults,file="sampled_overall_results_for_regional_md",quote=FALSE,row.names=FALSE)


