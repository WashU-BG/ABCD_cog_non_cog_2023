library(lme4)
library(lmerTest)
library(dplyr)

setwd("/scratch/aalab/emmaj/Cog_noncog")

## read in ABCD dataset from Aaron
abcd <- read.csv("/scratch/aalab/emmaj/Cog_noncog/cog_non_cog_within_brain.csv",header=T)
head(abcd)
dim(abcd)

## read in siblings file from Sarah
sibs <- read.table("/scratch/aalab/emmaj/Cog_noncog/abcd_sibs.txt",header=T)
head(sibs)
dim(sibs)

abcd_sibs <- merge(abcd[,-c(2,3)],sibs[,-c(2)],by.x="id_redcap",by.y="subjectkey")
dim(abcd_sibs)
head(abcd_sibs)
table(abcd_sibs$rel_family_id)

abcd_sibs$cogscale <- scale(abcd_sibs$cog_score)
abcd_sibs$noncogscale <- scale(abcd_sibs$non_cog_score)

## create variable that is family mean PGS
abcd2 <- as.data.frame(abcd_sibs %>% group_by(rel_family_id) %>% 
                         mutate(cog_family = mean(cogscale, na.rm=T),non_cog_family = mean(noncogscale,na.rm=T)) %>% 
                         arrange(rel_family_id)) %>%
                         mutate(cog_person = cogscale - cog_family, non_cog_person = noncogscale - non_cog_family)

######### smri outcomes
results <- as.data.frame(matrix(nrow=13,ncol=13))

names(results) <- c("Outcome","CogWSibBeta","CogWSibSE","CogWSibP","CogBwFamBeta","CogBwFamSE","CogBwFamP",
                    "NonCogWSibBeta","NonCogWSibSE","NonCogWSibP","NonCogBwFamBeta","NonCogBwFamSE","NonCogBwFamP")
j <- 0

for (i in c("smri_vol_cdk_banksstsrh","smri_vol_cdk_cdacaterh","smri_vol_cdk_ifplrh",
            "smri_vol_cdk_iftmlh","smri_vol_cdk_iftmrh","smri_vol_cdk_mdtmrh",
            "smri_vol_cdk_parsobisrh","smri_vol_cdk_precnlh","smri_vol_cdk_precnrh",
            "smri_vol_cdk_rracatelh","smri_vol_cdk_sutmlh","smri_vol_cdk_sutmrh",
            "smri_vol_cdk_tmpolerh")) {
j <- j+1
print(i)
print(j)

## pull out outcome i and covariates
abcd3 <- abcd2[,c(i,"id_redcap","rel_family_id","cog_person","cog_family","non_cog_person","non_cog_family",
                  "site_id","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","sex","interview_age",
                  "smri_vol_scs_intracranialv","mri_info_manufacturer", "mri_info_deviceserialnumber")]

print(dim(abcd3))

linmod <- lmer(scale(abcd3[,c(1)]) ~ scale(cog_person) + scale(cog_family) + scale(non_cog_person) + scale(non_cog_family) + 
                         sex + scale(interview_age) + scale(C1) + scale(C2) + scale(C3) + scale(C4) + 
                         scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + 
                 scale(smri_vol_scs_intracranialv) + mri_info_manufacturer + (1|mri_info_deviceserialnumber) + 
                 (1|rel_family_id), data = abcd3, control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
    
    ## fill in results from regression model
    results$Outcome[j] <- i
    results$CogWSibBeta[j] <- summary(linmod)$coefficients[2,1]
    results$CogWSibSE[j] <- summary(linmod)$coefficients[2,2]
    results$CogWSibP[j] <- summary(linmod)$coefficients[2,5]
    results$CogBwFamBeta[j] <- summary(linmod)$coefficients[3,1]
    results$CogBwFamSE[j] <- summary(linmod)$coefficients[3,2]
    results$CogBwFamP[j] <- summary(linmod)$coefficients[3,5]
    results$NonCogWSibBeta[j] <- summary(linmod)$coefficients[4,1]
    results$NonCogWSibSE[j] <- summary(linmod)$coefficients[4,2]
    results$NonCogWSibP[j] <- summary(linmod)$coefficients[4,5]
    results$NonCogBwFamBeta[j] <- summary(linmod)$coefficients[5,1]
    results$NonCogBwFamSE[j] <- summary(linmod)$coefficients[5,2]
    results$NonCogBwFamP[j] <- summary(linmod)$coefficients[5,5]
  } 
  
write.csv(results,file="smri_vol_wsibs.csv",quote=F,row.names=F)


######### md outcomes
results <- as.data.frame(matrix(nrow=2,ncol=13))

names(results) <- c("Outcome","CogWSibBeta","CogWSibSE","CogWSibP","CogBwFamBeta","CogBwFamSE","CogBwFamP",
                    "NonCogWSibBeta","NonCogWSibSE","NonCogWSibP","NonCogBwFamBeta","NonCogBwFamSE","NonCogBwFamP")
j <- 0


for (i in c("dmri_dtimd_fiberat_cstlh","dmri_dtimd_fiberat_cstrh")) {
  
  j <- j+1
  print(i)
  print(j)
  
  ## pull out outcome i and covariates
  abcd3 <- abcd2[,c(i,"id_redcap","rel_family_id","cog_person","cog_family","non_cog_person","non_cog_family",
                    "site_id","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","sex","interview_age",
                    "dmri_dtimd_fiberat_allfibers", "mri_info_manufacturer", "dmri_dti_meanmotion",
                    "mri_info_deviceserialnumber")]
  
  print(dim(abcd3))
  
  linmod <- lmer(scale(abcd3[,c(1)]) ~ scale(cog_person) + scale(cog_family) + scale(non_cog_person) + scale(non_cog_family) + 
                   sex + scale(interview_age) + scale(C1) + scale(C2) + scale(C3) + scale(C4) + 
                   scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + 
                   dmri_dtimd_fiberat_allfibers + mri_info_manufacturer + dmri_dti_meanmotion + 
                   (1|mri_info_deviceserialnumber) + (1|rel_family_id), data = abcd3, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
  
  ## fill in results from regression model
  results$Outcome[j] <- i
  results$CogWSibBeta[j] <- summary(linmod)$coefficients[2,1]
  results$CogWSibSE[j] <- summary(linmod)$coefficients[2,2]
  results$CogWSibP[j] <- summary(linmod)$coefficients[2,5]
  results$CogBwFamBeta[j] <- summary(linmod)$coefficients[3,1]
  results$CogBwFamSE[j] <- summary(linmod)$coefficients[3,2]
  results$CogBwFamP[j] <- summary(linmod)$coefficients[3,5]
  results$NonCogWSibBeta[j] <- summary(linmod)$coefficients[4,1]
  results$NonCogWSibSE[j] <- summary(linmod)$coefficients[4,2]
  results$NonCogWSibP[j] <- summary(linmod)$coefficients[4,5]
  results$NonCogBwFamBeta[j] <- summary(linmod)$coefficients[5,1]
  results$NonCogBwFamSE[j] <- summary(linmod)$coefficients[5,2]
  results$NonCogBwFamP[j] <- summary(linmod)$coefficients[5,5]
} 

write.csv(results,file="dmri_md_wsibs.csv",quote=F,row.names=F)


######### fa outcomes
results <- as.data.frame(matrix(nrow=10,ncol=13))

names(results) <- c("Outcome","CogWSibBeta","CogWSibSE","CogWSibP","CogBwFamBeta","CogBwFamSE","CogBwFamP",
                    "NonCogWSibBeta","NonCogWSibSE","NonCogWSibP","NonCogBwFamBeta","NonCogBwFamSE","NonCogBwFamP")
j <- 0


for (i in c("dmri_dtifa_fiberat_cc","dmri_dtifa_fiberat_cstlh","dmri_dtifa_fiberat_cstrh",
            "dmri_dtifa_fiberat_fmin","dmri_dtifa_fiberat_fscslh","dmri_dtifa_fiberat_fscsrh",
            "dmri_dtifa_fiberat_pscslh","dmri_dtifa_fiberat_pscsrh","dmri_dtifa_fiberat_scslh",
            "dmri_dtifa_fiberat_scsrh")) {
  
  j <- j+1
  print(i)
  print(j)
  
  ## pull out outcome i and covariates
  abcd3 <- abcd2[,c(i,"id_redcap","rel_family_id","cog_person","cog_family","non_cog_person","non_cog_family",
                    "site_id","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","sex","interview_age",
                    "dmri_dtifa_fiberat_allfibers", "mri_info_manufacturer", "dmri_dti_meanmotion",
                    "mri_info_deviceserialnumber")]
  
  print(dim(abcd3))
  
  linmod <- lmer(scale(abcd3[,c(1)]) ~ scale(cog_person) + scale(cog_family) + scale(non_cog_person) + scale(non_cog_family) + 
                   sex + scale(interview_age) + scale(C1) + scale(C2) + scale(C3) + scale(C4) + 
                   scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + 
                   dmri_dtifa_fiberat_allfibers + mri_info_manufacturer + dmri_dti_meanmotion + 
                   (1|mri_info_deviceserialnumber) + (1|rel_family_id), data = abcd3, 
                 control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
  
  ## fill in results from regression model
  results$Outcome[j] <- i
  results$CogWSibBeta[j] <- summary(linmod)$coefficients[2,1]
  results$CogWSibSE[j] <- summary(linmod)$coefficients[2,2]
  results$CogWSibP[j] <- summary(linmod)$coefficients[2,5]
  results$CogBwFamBeta[j] <- summary(linmod)$coefficients[3,1]
  results$CogBwFamSE[j] <- summary(linmod)$coefficients[3,2]
  results$CogBwFamP[j] <- summary(linmod)$coefficients[3,5]
  results$NonCogWSibBeta[j] <- summary(linmod)$coefficients[4,1]
  results$NonCogWSibSE[j] <- summary(linmod)$coefficients[4,2]
  results$NonCogWSibP[j] <- summary(linmod)$coefficients[4,5]
  results$NonCogBwFamBeta[j] <- summary(linmod)$coefficients[5,1]
  results$NonCogBwFamSE[j] <- summary(linmod)$coefficients[5,2]
  results$NonCogBwFamP[j] <- summary(linmod)$coefficients[5,5]
} 

write.csv(results,file="dmri_fa_wsibs.csv",quote=F,row.names=F)



######### global md outcomes
results <- as.data.frame(matrix(nrow=1,ncol=13))

names(results) <- c("Outcome","CogWSibBeta","CogWSibSE","CogWSibP","CogBwFamBeta","CogBwFamSE","CogBwFamP",
                    "NonCogWSibBeta","NonCogWSibSE","NonCogWSibP","NonCogBwFamBeta","NonCogBwFamSE","NonCogBwFamP")
j <- 0


for (i in c("dmri_dtimd_fiberat_allfibers")) {
  
  j <- j+1
  print(i)
  print(j)
  
  ## pull out outcome i and covariates
  abcd3 <- abcd2[,c(i,"id_redcap","rel_family_id","cog_person","cog_family","non_cog_person","non_cog_family",
                    "site_id","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","sex","interview_age",
                     "mri_info_manufacturer", "dmri_dti_meanmotion",
                    "mri_info_deviceserialnumber")]
  
  print(dim(abcd3))
  
  linmod <- lmer(scale(abcd3[,c(1)]) ~ scale(cog_person) + scale(cog_family) + scale(non_cog_person) + scale(non_cog_family) + 
                   sex + scale(interview_age) + scale(C1) + scale(C2) + scale(C3) + scale(C4) + 
                   scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + 
                    mri_info_manufacturer + dmri_dti_meanmotion + 
                   (1|mri_info_deviceserialnumber) + (1|rel_family_id), data = abcd3, 
                 control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
  
  ## fill in results from regression model
  results$Outcome[j] <- i
  results$CogWSibBeta[j] <- summary(linmod)$coefficients[2,1]
  results$CogWSibSE[j] <- summary(linmod)$coefficients[2,2]
  results$CogWSibP[j] <- summary(linmod)$coefficients[2,5]
  results$CogBwFamBeta[j] <- summary(linmod)$coefficients[3,1]
  results$CogBwFamSE[j] <- summary(linmod)$coefficients[3,2]
  results$CogBwFamP[j] <- summary(linmod)$coefficients[3,5]
  results$NonCogWSibBeta[j] <- summary(linmod)$coefficients[4,1]
  results$NonCogWSibSE[j] <- summary(linmod)$coefficients[4,2]
  results$NonCogWSibP[j] <- summary(linmod)$coefficients[4,5]
  results$NonCogBwFamBeta[j] <- summary(linmod)$coefficients[5,1]
  results$NonCogBwFamSE[j] <- summary(linmod)$coefficients[5,2]
  results$NonCogBwFamP[j] <- summary(linmod)$coefficients[5,5]
} 

write.csv(results,file="global_md_wsibs.csv",quote=F,row.names=F)


######### global fa outcomes
results <- as.data.frame(matrix(nrow=1,ncol=13))

names(results) <- c("Outcome","CogWSibBeta","CogWSibSE","CogWSibP","CogBwFamBeta","CogBwFamSE","CogBwFamP",
                    "NonCogWSibBeta","NonCogWSibSE","NonCogWSibP","NonCogBwFamBeta","NonCogBwFamSE","NonCogBwFamP")
j <- 0


for (i in c("dmri_dtifa_fiberat_allfibers")) {
  
  j <- j+1
  print(i)
  print(j)
  
  ## pull out outcome i and covariates
  abcd3 <- abcd2[,c(i,"id_redcap","rel_family_id","cog_person","cog_family","non_cog_person","non_cog_family",
                    "site_id","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","sex","interview_age",
                    "mri_info_manufacturer", "dmri_dti_meanmotion",
                    "mri_info_deviceserialnumber")]
  
  print(dim(abcd3))
  
  linmod <- lmer(scale(abcd3[,c(1)]) ~ scale(cog_person) + scale(cog_family) + scale(non_cog_person) + scale(non_cog_family) + 
                   sex + scale(interview_age) + scale(C1) + scale(C2) + scale(C3) + scale(C4) + 
                   scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + 
                   mri_info_manufacturer + dmri_dti_meanmotion + 
                   (1|mri_info_deviceserialnumber) + (1|rel_family_id), data = abcd3, 
                 control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
  
  ## fill in results from regression model
  results$Outcome[j] <- i
  results$CogWSibBeta[j] <- summary(linmod)$coefficients[2,1]
  results$CogWSibSE[j] <- summary(linmod)$coefficients[2,2]
  results$CogWSibP[j] <- summary(linmod)$coefficients[2,5]
  results$CogBwFamBeta[j] <- summary(linmod)$coefficients[3,1]
  results$CogBwFamSE[j] <- summary(linmod)$coefficients[3,2]
  results$CogBwFamP[j] <- summary(linmod)$coefficients[3,5]
  results$NonCogWSibBeta[j] <- summary(linmod)$coefficients[4,1]
  results$NonCogWSibSE[j] <- summary(linmod)$coefficients[4,2]
  results$NonCogWSibP[j] <- summary(linmod)$coefficients[4,5]
  results$NonCogBwFamBeta[j] <- summary(linmod)$coefficients[5,1]
  results$NonCogBwFamSE[j] <- summary(linmod)$coefficients[5,2]
  results$NonCogBwFamP[j] <- summary(linmod)$coefficients[5,5]
} 

write.csv(results,file="global_fa_wsibs.csv",quote=F,row.names=F)




######### global vol outcomes
results <- as.data.frame(matrix(nrow=9,ncol=13))

names(results) <- c("Outcome","CogWSibBeta","CogWSibSE","CogWSibP","CogBwFamBeta","CogBwFamSE","CogBwFamP",
                    "NonCogWSibBeta","NonCogWSibSE","NonCogWSibP","NonCogBwFamBeta","NonCogBwFamSE","NonCogBwFamP")
j <- 0


for (i in c("smri_vol_scs_cbwmatterlh","smri_vol_scs_cbwmatterrh","smri_vol_scs_subcorticalgv",
            "smri_vol_scs_suprateialv","smri_vol_scs_intracranialv","smri_vol_cdk_total",
            "smri_vol_cdk_totallh","smri_vol_cdk_totalrh","smri_vol_scs_wholeb")) {
  

  j <- j+1
  print(i)
  print(j)
  
  ## pull out outcome i and covariates
  abcd3 <- abcd2[,c(i,"id_redcap","rel_family_id","cog_person","cog_family","non_cog_person","non_cog_family",
                    "site_id","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","sex","interview_age",
                    "mri_info_manufacturer", 
                    "mri_info_deviceserialnumber")]
  
  print(dim(abcd3))
  
  linmod <- lmer(scale(abcd3[,c(1)]) ~ scale(cog_person) + scale(cog_family) + scale(non_cog_person) + scale(non_cog_family) + 
                   sex + scale(interview_age) + scale(C1) + scale(C2) + scale(C3) + scale(C4) + 
                   scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) + 
                   mri_info_manufacturer +  
                   (1|mri_info_deviceserialnumber) + (1|rel_family_id), data = abcd3, 
                 control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
  
  ## fill in results from regression model
  results$Outcome[j] <- i
  results$CogWSibBeta[j] <- summary(linmod)$coefficients[2,1]
  results$CogWSibSE[j] <- summary(linmod)$coefficients[2,2]
  results$CogWSibP[j] <- summary(linmod)$coefficients[2,5]
  results$CogBwFamBeta[j] <- summary(linmod)$coefficients[3,1]
  results$CogBwFamSE[j] <- summary(linmod)$coefficients[3,2]
  results$CogBwFamP[j] <- summary(linmod)$coefficients[3,5]
  results$NonCogWSibBeta[j] <- summary(linmod)$coefficients[4,1]
  results$NonCogWSibSE[j] <- summary(linmod)$coefficients[4,2]
  results$NonCogWSibP[j] <- summary(linmod)$coefficients[4,5]
  results$NonCogBwFamBeta[j] <- summary(linmod)$coefficients[5,1]
  results$NonCogBwFamSE[j] <- summary(linmod)$coefficients[5,2]
  results$NonCogBwFamP[j] <- summary(linmod)$coefficients[5,5]
} 

write.csv(results,file="global_vol_wsibs.csv",quote=F,row.names=F)



#######  psychosocial outcomes

## read in ABCD dataset from Aaron
abcd <- read.csv("/scratch/aalab/emmaj/Cog_noncog/6_28_complete_data_v2.csv",header=T)
head(abcd)
dim(abcd)


## 8/17 - add in nicotine data
nic <- read.csv("/scratch/aalab/emmaj/Cog_noncog/7_31_complete_nicdata.csv",header=T)
head(nic)
dim(nic)


abcd_sibs <- merge(abcd[,-c(2,3)],sibs[,-c(2)],by.x="id_redcap",by.y="subjectkey")
dim(abcd_sibs)
head(abcd_sibs)
table(abcd_sibs$rel_family_id)

abcd_sibs$cogscale <- scale(abcd_sibs$cog_score)
abcd_sibs$noncogscale <- scale(abcd_sibs$non_cog_score)

abcd_sibs2 <- merge(abcd_sibs,nic[,c(1,19)],by="id_redcap")


## create variable that is family mean PGS
abcd2 <- as.data.frame(abcd_sibs2 %>% group_by(rel_family_id) %>% 
                         mutate(cog_family = mean(cogscale, na.rm=T),non_cog_family = mean(noncogscale,na.rm=T)) %>% 
                         arrange(rel_family_id)) %>%
  mutate(cog_person = cogscale - cog_family, non_cog_person = noncogscale - non_cog_family)

## change cash choice task to binary
abcd2$cashchoice <- as.numeric(abcd2$cash_choice_task - 1)
table(abcd2$cashchoice)

abcd2$nic[abcd2$nic_any_woc == 1] <- 1
abcd2$nic[abcd2$nic_any_woc == 0] <- 0
abcd2$nic[abcd2$nic_any_woc == "#N/A"] <- NA
abcd2$nic <- as.numeric(abcd2$nic)


## continuous outcomes - lmer models
results <- as.data.frame(matrix(nrow=5,ncol=13))

names(results) <- c("Outcome","CogWSibBeta","CogWSibSE","CogWSibP","CogBwFamBeta","CogBwFamSE","CogBwFamP",
                    "NonCogWSibBeta","NonCogWSibSE","NonCogWSibP","NonCogBwFamBeta","NonCogBwFamSE","NonCogBwFamP")
j <- 0


for (i in c("nihtbx_fluidcomp_uncorrected","nihtbx_cryst_uncorrected","nihtbx_totalcomp_uncorrected",
            "upps12_y","pps_y_ss_severity_score")) {
  
  
  j <- j+1
  print(i)
  print(j)
  
  ## pull out outcome i and covariates
  abcd3 <- abcd2[,c(i,"id_redcap","rel_family_id","cog_person","cog_family","non_cog_person","non_cog_family",
                    "site_id","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","sex","interview_age")]
  
  print(dim(abcd3))
  
  linmod <- lmer(scale(abcd3[,c(1)]) ~ scale(cog_person) + scale(cog_family) + scale(non_cog_person) + scale(non_cog_family) + 
                   sex + scale(interview_age) + scale(C1) + scale(C2) + scale(C3) + scale(C4) + 
                   scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) +
                   (1|rel_family_id), data = abcd3, control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
  
  ## fill in results from regression model
  results$Outcome[j] <- i
  results$CogWSibBeta[j] <- summary(linmod)$coefficients[2,1]
  results$CogWSibSE[j] <- summary(linmod)$coefficients[2,2]
  results$CogWSibP[j] <- summary(linmod)$coefficients[2,5]
  results$CogBwFamBeta[j] <- summary(linmod)$coefficients[3,1]
  results$CogBwFamSE[j] <- summary(linmod)$coefficients[3,2]
  results$CogBwFamP[j] <- summary(linmod)$coefficients[3,5]
  results$NonCogWSibBeta[j] <- summary(linmod)$coefficients[4,1]
  results$NonCogWSibSE[j] <- summary(linmod)$coefficients[4,2]
  results$NonCogWSibP[j] <- summary(linmod)$coefficients[4,5]
  results$NonCogBwFamBeta[j] <- summary(linmod)$coefficients[5,1]
  results$NonCogBwFamSE[j] <- summary(linmod)$coefficients[5,2]
  results$NonCogBwFamP[j] <- summary(linmod)$coefficients[5,5]
} 

write.csv(results,file="psychosocial_linear_wsibs.csv",quote=F,row.names=F)


## binary outcomes - glmer models
results <- as.data.frame(matrix(nrow=4,ncol=13))

names(results) <- c("Outcome","CogWSibBeta","CogWSibSE","CogWSibP","CogBwFamBeta","CogBwFamSE","CogBwFamP",
                    "NonCogWSibBeta","NonCogWSibSE","NonCogWSibP","NonCogBwFamBeta","NonCogBwFamSE","NonCogBwFamP")
j <- 0


for (i in c("cashchoice","nic","ever_ADHD","ever_anorexia")) {
  
  
  j <- j+1
  print(i)
  print(j)
  
  ## pull out outcome i and covariates
  abcd3 <- abcd2[,c(i,"id_redcap","rel_family_id","cog_person","cog_family","non_cog_person","non_cog_family",
                    "site_id","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","sex","interview_age")]
  
  print(dim(abcd3))
  
  glmod <- glmer(abcd3[,c(1)] ~ scale(cog_person) + scale(cog_family) + scale(non_cog_person) + scale(non_cog_family) + 
                   sex + scale(interview_age) + scale(C1) + scale(C2) + scale(C3) + scale(C4) + 
                   scale(C5) + scale(C6) + scale(C7) + scale(C8) + scale(C9) + scale(C10) +
                   (1|rel_family_id), data = abcd3, family = "binomial", nAGQ = 0,
                  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
  
  ## fill in results from regression model
  results$Outcome[j] <- i
  results$CogWSibBeta[j] <- summary(glmod)$coefficients[2,1]
  results$CogWSibSE[j] <- summary(glmod)$coefficients[2,2]
  results$CogWSibP[j] <- summary(glmod)$coefficients[2,4]
  results$CogBwFamBeta[j] <- summary(glmod)$coefficients[3,1]
  results$CogBwFamSE[j] <- summary(glmod)$coefficients[3,2]
  results$CogBwFamP[j] <- summary(glmod)$coefficients[3,4]
  results$NonCogWSibBeta[j] <- summary(glmod)$coefficients[4,1]
  results$NonCogWSibSE[j] <- summary(glmod)$coefficients[4,2]
  results$NonCogWSibP[j] <- summary(glmod)$coefficients[4,4]
  results$NonCogBwFamBeta[j] <- summary(glmod)$coefficients[5,1]
  results$NonCogBwFamSE[j] <- summary(glmod)$coefficients[5,2]
  results$NonCogBwFamP[j] <- summary(glmod)$coefficients[5,4]
} 

write.csv(results,file="psychosocial_binary_wsibs.csv",quote=F,row.names=F)
