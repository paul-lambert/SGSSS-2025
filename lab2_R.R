

### Title and contents

#### SGSSS Summer School 2025

## Short course: Multilevel Models for Applied Social Research

## Materials by Paul Lambert and Kate O'Hara (Univ. Stirling)

##  LAB 2 - R EXERCISES - 'RESPONDING TO COMPLEX DATA AND TO COMPLEX ANALYTICAL OPTIONS'

###   Linked code, data and metadata: at https://github.com/paul-lambert/SGSSS-2025/

##    Version: 16/Jun/2025, written for R 4.4.3



################################### 


#### Contents listing

###   Title and contents 
###   Paths and preliminaries
###   Ex 2.1: Recaps on scenarios where random effects models might and might not help  
###   Ex 2.2: Random effects examples with 'cross-classified' data structures
###   Ex 2.3: Longitudinal data preparation and analysis examples 
###   Ex 2.4: An observation on trying random slopes models with low average group size in lmer   
###   Ex 2.5: Random effects for modelling intersectionality    

********************************


 


 



######################################






######################################


###   Paths and preliminaries


#################

### Context: 
###  Most of these exercises are modified examples of exercises used in a previous short course taught by Paul Lambert
##     and Richard Zijdeman. They use the 'base R' style (e.g. using the 'get' symbol <- , as in "mod1 <- [model code]"),
##     and a fairly minimal set of libraries. Many people that work regularly with R will use other conventions for 
##     working with objects, and other libraries. These examples are pretty minimalist (they were mainly designed to 
##     make points of comparison with examples given in more detail using Stata). We hope that they do enough 
##     to demonstrate relevant functionalities, but we'd encourage anyone planing to do more work in these areas 
##     to be open to searching for new or alternative code to work with multilevel models, such as by finding code via 
##     online resources like stack exchange and ChatGPT. 


#################




#######
# Making some settings changes

# Setting the number of digits for output
options(digits=4)

## Setting the default directory (to a location on your machine - edit text below as needed
setwd("C:/1/sgsss-2025/work/")



######

### File shortcuts: 
### This code sets shortcut names for specific data files that are used in the exercise. 
##   You'll typically need to modify the file path to one that suits your computer. 


library_space <- as.character('C:/1/sgsss-2025/work/Rlibs') 
## Use in preliminary section: refers to a location available on your machine where you can install R libraries to



soc_case_study_1 <- as.character('C:/1/sgsss-2025/data/soc_case_study_1.dta')
aindresp_extract8 <- as.character('C:/1/sgsss-2025/data/aindresp_extract8.dta')
bindresp_extract8 <- as.character('C:/1/sgsss-2025/data/bindresp_extract8.dta')
bhps_ghq_extract1 <- as.character('C:/1/sgsss-2025/data/bhps_ghq_extract1.dta')
bhps_soc_3level  <- as.character('C:/1/sgsss-2025/data/bhps_soc_3level.dta')
bhps_w15_health_extract1 <- as.character('C:/1/sgsss-2025/data/bhps_w15_health_extract1.dta')
## BHPS extract datasets : Various subsets of survey data with different multilevel structures
## These extract files are supplied in class but are not for onward distribution. 
## The original data from which the extracts are derived can be accessed from the UK Data Service, study number 5151



pupcross_data <- as.character('C:/1/sgsss-2025/data/pupcross.dta') 
## The cross-classified pupil experiences dataset as used in the Hox textbook (Hox 2010/Hox et al. 2017) (in a Stata format file)  
## You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
## Provenance: This dataset is used in many multilevel modelling textbooks, originating in research undertaken by 
#     Plewis and Paterson on educational inequalities. This version is as used in in the Hox textbook on multilevel models.
##  The data scenario is students in education where both their primary and secondary school is known 
##   (and nesting is not neatly hierarchical, i.e. it is 'cross-classified')  




#######

### Libraries: 
### The code below identifies and installs libraries used in Lab 1 (in a resource-heavy way)
##    (you might prefer to modify the installation code for your machine, e.g. if you don't need all of these or 
##      wish to include other installs)


# If necessary, add a space to your machine where you will be fine to install libraries to:
.libPaths() # shows current libary paths
.libPaths(c(.libPaths(), library_space)) # adds new library path to current library path(s)

pkgs <- c(   "foreign",  "readstata13", "lme4", "ggplot2", "nlme" , "ordinal", "Rcpp", "lmtest", "lattice" )

## Packages description
# foreign: For reading in data in other sofware formas
# readstata13: Same as foreign, but for more recent Stata format data 
# lme4: For mixed models with random effects
# ordinal: Facilitates analysis of ordinal (ordered categorical data) via cumulative link models (CLMs) and cumulative link mixed models (CLMMs).
# ggplot2: For nice graphical displays
# Rcpp: required by lme4
# lmtest: For post-estimation model tests
# lattice: for post-estimation visualisations, for the 'qqmath' function

## Library install code follows: 
remove.packages(pkgs)  
#(heavyweight cleanup: deletes any existing packages with these names (sometimes legacy out of date copies can cause conflict - e.g. Rcpp with lme4))
install.packages(pkgs, repos="https://www.stats.bris.ac.uk/R/", lib=library_space)
# comment: downloads the files for all these packages to a nominated  location 
# (also specifies a repository mirror for download, to avoid having to select one interactively)
lapply(pkgs, library, character.only = TRUE) 
# i.e. this commmand 'activates' the packages that are listed in the object 'pkgs') 
## (to save time, you don't always need to re-install packages if they are already downloaded, but 
#     you do always need to apply, them such as with lapply, in any given session)  

#####
##################################################################################################









#####################################################
#     2) Recaps on scenarios where random effects models might and might not help  
##################################






## Read in the plain text data created in the SPSS or Stata labs 6: 


bhps_soc <- read.dta(paste(soc_case_study_1),    convert.factors=F)
names(bhps_soc); hist(bhps_soc$lfimnl)

### (a) Scenarios where the hierarchical structure is interesting and random effects help describe it

names(bhps_soc)
rc_inc <- lmer(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage 
        + (1 + c_ajbhrs | ajbsoc ) , data=bhps_soc,  REML=FALSE   ) 
summary(rc_inc, corr=FALSE)
fixef(rc_inc)

# For plotting the residuals of interest
rc_inc_res <- data.frame(ranef(rc_inc)[1], fixef(rc_inc)[1], fixef(rc_inc)[2])
names(rc_inc_res); head(rc_inc_res) # i.e. the residuals plus original coefficients
rc_inc_res$int_tot <- rc_inc_res$ajbsoc..Intercept. + rc_inc_res$fixef.rc_inc..1.
rc_inc_res$jbhrs_tot <- rc_inc_res$ajbsoc.c_ajbhrs + rc_inc_res$fixef.rc_inc..2.
names(rc_inc_res); head(rc_inc_res) # i.e. the totals give the actual values of the coefficients plus adjustments

par(mfrow = c(1, 2)); plot(rc_inc_res$int_tot); plot(rc_inc_res$jbhrs_tot)
# i.e. shows job to job residuals by SOC 




### (b) Scenarios where the hierarchical structure is minimally important so payoff to random effects
#        is questionable

names(bhps_soc)
table(bhps_soc$c_ahlstat) # a poor health outcomes measure 

lin_hlth <- lm(c_ahlstat ~ c_ajbhrs  + fem + c_age + c_age2 
        + s_mcam + cohab + degdip + vocq + noqual + femage  , data=bhps_soc) 
summary(lin_hlth)
coef(lin_hlth)
logLik(lin_hlth)

ri_hlth <- lmer(formula = c_ahlstat ~ c_ajbhrs  + fem + c_age + c_age2 
       + s_mcam  + cohab + degdip + vocq + noqual + femage  
   + (1  | ajbsoc ) , data=bhps_soc,  REML=FALSE  ) 
summary(ri_hlth, corr=FALSE)
logLik(ri_hlth)

lrtest(lin_hlth, ri_hlth)
# No substantial improvement to model fit with RI so conventionally reject it; 


# No examples here, but for online resources for robust standard errors:
## https://www.r-bloggers.com/2021/05/clustered-standard-errors-with-r/ 
## https://economictheoryblog.com/2016/12/13/clustered-standard-errors-in-r/

###############



### (c) Further exploration of higher-level explanatory variables with and without random effects:  


# Models with no higher level (occupation-based) effects

# Single level
lin1 <- lm(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage, data=bhps_soc    ) 
summary(lin1)

# Occupation unit random effects (random intercepts) 
ri1 <- lmer(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage 
        + (1 | ajbsoc ) , data=bhps_soc , REML=FALSE   ) 
summary(ri1)



# Adding in higher level fixed-part variables (i.e. occupation-based measures)
#  and comparing with and without occupation-based random intercepts



##  (using CAMSIS, a scale for occupational advantage)

ri2 <- lmer(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage + s_bonus + c_smcam  
        + (1 | ajbsoc ) ,  data=bhps_soc , REML=FALSE   ) 
summary(ri2)

# (compare to single level) 
lin2 <- lm(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage + s_bonus + c_smcam  , data=bhps_soc ) 
summary(lin2)
anova(ri2, lin2) 
# => deviance comparison favours random effects, so re model should give more info
# and better fixed part estimates - yet in practice, single-level model might tell much same story)



## (using RGSC, a social class categorisation based on occupations)

ri3 <- lmer(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage + s_bonus  
        + rgsc_1 + rgsc_3 + rgsc_4 + rgsc_5 + rgsc_6 + rgsc_7   
        + (1 | ajbsoc ) , data=bhps_soc , REML=FALSE   ) 
summary(ri3)

# (compare to single level) 
lin3 <- lm(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage + s_bonus  
        + rgsc_1 + rgsc_3 + rgsc_4 + rgsc_5 + rgsc_6 + rgsc_7  , data=bhps_soc ) 
summary(lin3)
anova(ri3, lin3) 
# (same conclusion from this comparison)



## (using EGP, an alternative social class categorisation based on occupations)

ri4 <- lmer(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage + s_bonus  
        + gold_1 + gold_3 + gold_4 + gold_5 + gold_6 + gold_7 + gold_8 + gold_9   
        + (1 | ajbsoc ) , data=bhps_soc , REML=FALSE   ) 
summary(ri4)

# (compare to single level) 
lin4 <- lm(formula = lfimnl ~ c_ajbhrs + c_ahlstat + fem + c_age + c_age2 
        + cohab + degdip + vocq + noqual + femage + s_bonus  
       + gold_1 + gold_3 + gold_4 + gold_5 + gold_6 + gold_7 + gold_8 + gold_9  , data=bhps_soc   ) 
summary(lin4)
anova(ri4, lin4)
# (same conclusion again)


# Observation: The various available higher level fixed part predictors are doing the 
#    bulk (but not all) of the work of the occupation level random effects; it is therefore 
#    _plausible_ to run the same model without the random effects, as, indeed, has been 
#    standard practice in social sciecnes for many years, yet it might be sub-optimal (e.g. 
#    inefficient standard errors; underplaying extend of occuaptional influence)  

###################
###################



######################################################################
####  3) Fixed versus random effects comparisons
######################################################################


# (i) Random effects only in a simple model with just two individual level x variables
ref1 <- lmer(formula = lfimnl ~ ajbhrs + fem 
             + (1 | ajbsoc),  data=bhps_soc , REML=FALSE  ) 
summary(ref1)


 
        
# (ii) Ways of gettting the fixed effects estimates out of R 
#       (for discussion see Gelman and Hill 2007: 244ff) 


# (a)  Direct indicator coding usually works fine but may stretch memory: 
fe1 <- lm(formula = lfimnl ~ ajbhrs + fem 
             + as.factor(ajbsoc)  ,  data=bhps_soc   ) 
summary(fe1)



# (b) The same results can be obtained just by fitting the cluster group means of x vars 
#        (i.e. the 'mean deviation' method)



fe2 <- lm(formula = lfimnl ~ ajbhrs + fem + occ_hrs + occ_fem , data=bhps_soc ) 
summary(fe2)



# (c) Adding both random effects and cluster group means of x vars leads to equivalent results to fixed effects model
#        (i.e. the REWB or 'hybrid' model) 
ref2 <- lmer(formula = lfimnl ~ ajbhrs + fem + occ_hrs + occ_fem
             + (1 | ajbsoc),  data=bhps_soc , REML=FALSE  ) 
summary(ref2)



#  (for discussion of these ways of getting the within effects parameters - see Allison 2009: 18) 


########################################






################################################



### 2) Cross-classified models 

# Example following Hox 2010: p173ff
pupcros <- read.dta(paste(pupcross_data))
summary(pupcros)
pupcros$cons <- 1


# Outcome variable:
hist(pupcros$achiev)

## Linear models
lin0 <- lm(achiev ~ cons, data = pupcros)
summary(lin0)
lin1 <- lm(achiev ~ pupsex + pupses + pdenom + sdenom, data = pupcros)
summary(lin1)


## Two alternative two-level null models:
l2nulla <- lmer(formula = achiev ~  + (1 | pschool), data = pupcros  )
summary(l2nulla) 

l2nullb <- lmer(formula = achiev ~  + (1 | sschool), data = pupcros  )
summary(l2nullb) 


## Cross-classified null model at three levels (cf col 1 of Hox 2010:Table 9.1)
l3nulla <- lmer(formula = achiev ~ 
           + (1 | pschool) + (1 | sschool), data = pupcros)
summary(l3nulla)
# Note -  R has automatically recognised the 
#  shared variance in the shared values of the pschool / sschool identifiers

# Compare the model which treats each sschools within pschool as different:
summary(pupcros$pschool)
summary(pupcros$sschool)

pupcros$sschool2 <- (pupcros$pschool*100) + pupcros$sschool
summary(pupcros)

## cf. the erroneous 3-level model (not recognising shared sschools) 
l3_nullb <- lmer(formula = achiev ~ 
               + (1 | sschool2) + (1 | pschool), data = pupcros)
summary(l3_nullb)

## Cross-classified model at three levels with x vars (cf col 3 of Hox 2010:Table 9.1)
col3 <- lmer(formula = achiev ~ pupsex + pupses + pdenom + sdenom +
           (1 | pschool) + (1 | sschool), data = pupcros)
summary(col3)
######################################





######################################

# Example using BHPS data (following Gelman and Hill 2007: 289ff)


#Warning: prepared in the Stata file, so may be only available after running Stata

bhps.soc3 <- read.dta(paste(bhps_soc_3level))
summary(bhps.soc3)

# dependent variable: 
hist(bhps.soc3$lfimn)
lin0 <- lm(lfimn ~ cons, data = bhps.soc3) 
lin1 <- lm(lfimn ~ c_ajbhrs + fem + c_age + c_age2 + degdip + 
             vocq + noqual + degage, data = bhps.soc3 )

summary(lin0)
summary(lin1)

## Multilevel linear regressions: 

# SOC only clustering
l2a <- lmer(lfimn ~ c_ajbhrs + fem + c_age + c_age2 
               + degdip + vocq + noqual + degage 
           + (1 | ajbsoc), data = bhps.soc3)
summary(l2a) 

# PSU only clustering
l2b <- lmer(formula = lfimn ~ c_ajbhrs + fem + c_age + c_age2 
               + degdip + vocq + noqual + degage 
           + (1 | apsu), data = bhps.soc3)
summary(l2b)

## The model isn't hierarchically nested (it's 'cross-classified') but we 
#  can readily specify and fit the cross-classified system

l3a <- lmer(formula = lfimn ~ c_ajbhrs + fem + c_age + c_age2 
               + degdip + vocq + noqual + degage 
           + (1 | ajbsoc) + (1 | apsu), data = bhps.soc3)
summary(l3a) 



#########################################################
#########################################################





####################################
##  3) Longitudinal data preparation and analysis examples 

## 3a)
##
##  The moodle site for the online course 'Introduction to Understanding Society using R'
##  features examples of data preparation and analysis for this complex panel dataset - 
##    see https://www.understandingsociety.ac.uk/help/training/online/introduction-course
##   (we recommend joining that course and looking at those materials)
##

## 
## Thereafter, here are a couple of really basic examples of BHPS data linkage and analysis 


####################
##  3b) Panel transitions: comparing responses from a few different waves 


## Here are some examples of linking BHPS data across waves (for the same people)
##

## (i) Wide format: Data is linked by a shared identifier

# Open, and if relevant refine, extract from first time point
bhps_a <- read.dta13(paste(aindresp_extract8), convert.factors=F)
names(bhps_a)
keep_vars <- c("pid", "asex", "aage", "ahlstat") # (to select vars if desired, tho not impactful here)
dim(bhps_a); bhps_a_s <- bhps_a[keep_vars]; dim(bhps_a_s);  head(bhps_a_s)

# Open, and if relevant refine, extract from second time point
bhps_b <- read.dta13(paste(bindresp_extract8), convert.factors=F)
names(bhps_b)
keep_vars <- c("pid", "bsex", "bage", "bhlstat") # (to select vars if desired, tho not impactful here)
dim(bhps_b); bhps_b_s <- bhps_b[keep_vars]; dim(bhps_b_s);  head(bhps_b_s)
# (note we want names of variables to be different between the time points, which 
#    in this case they already are) 


# Match-merge the data frames using the consistent identifier variable pid: 

# This implements the merge: 
bhps_2 <- merge(bhps_a_s, bhps_b_s, by.x="pid",  by.y="pid",
               all.x=T, all.y=T, sort=F, suffixes = c(".x",".y") )
summary(bhps_2); dim(bhps_2);   head(bhps_2); bhps_2[9000:9005,] ; bhps_2[11000:11005,] 

# i.e. the new dataset has 11139 rows, those have data from people who are either 
#    sampled in year 1 or year 2 or both 

# A typical analysis of transitions simply looks at the link between answers for the same
#   people in different years (when non-missing), e.g. 

table(bhps_2$ahlstat); table(bhps_2$bhlstat); 
bhps_2$ahlstat[bhps_2$ahlstat == -9] <- NA
bhps_2$ahlstat[bhps_2$ahlstat == -1] <- NA
bhps_2$bhlstat[bhps_2$bhlstat == -9] <- NA
bhps_2$bhlstat[bhps_2$bhlstat == -1] <- NA  # long-hand way to code missing values 

res <- with(bhps_2, table(ahlstat, bhlstat, useNA="no"))

res; prop.table(res,1) ; prop.table(res,2) 
chisq.test(res)

# i.e. 57% of responding people who were very healthy in wave A were also very healthy in wave B, etc





## (ii)  Long format: Data is stacked, and recognised by a shared identifier

# (In long format, we need variable names to be the same across years, plus we
#    want extra variables to indicate the time point)


# Open, and if relevant refine, extract from first time point
bhps_a <- read.dta13(paste(aindresp_extract8), convert.factors=F)
names(bhps_a)
keep_vars <- c("pid", "asex", "aage", "ahlstat") # (to select vars if desired, tho not impactful here)
dim(bhps_a); bhps_a_s <- bhps_a[keep_vars]; dim(bhps_a_s)
names(bhps_a_s)
colnames(bhps_a_s) <- c("pid", "sex", "age", "hlstat"); names(bhps_a_s)
bhps_a_s$year <- 1991; names(bhps_a_s); head(bhps_a_s)

# Open, and if relevant refine, extract from second time point
bhps_b <- read.dta13(paste(bindresp_extract8), convert.factors=F)
names(bhps_b)
keep_vars <- c("pid", "bsex", "bage", "bhlstat") # (to select vars if desired, tho not impactful here)
dim(bhps_b); bhps_b_s <- bhps_b[keep_vars]; dim(bhps_b_s)
names(bhps_b_s)
colnames(bhps_b_s) <- c("pid", "sex", "age", "hlstat"); names(bhps_b_s)
bhps_b_s$year <- 1992; names(bhps_b_s); head(bhps_b_s)


# In this format, we can push the two data files together (stacking them) using 'cbind'

bhps_long <- rbind(bhps_a_s, bhps_b_s); dim(bhps_a_s); dim(bhps_b_s); dim(bhps_long)


## Conventional descriptive analyses make particular use of 'year', e.g. 

table(bhps_long$hlstat, bhps_long$year) 
bhps_long$hlstat[bhps_long$hlstat == -9] <- NA
bhps_long$hlstat[bhps_long$hlstat == -1] <- NA

table(bhps_long$hlstat, bhps_long$year) 


## Conventional modelling analyses make particular use of 'pid', e.g. 

names(bhps_long); table(bhps_long$sex); hist(bhps_long$age); summary(bhps_long)
bhps_long$fem <- as.numeric(bhps_long$sex==2); table(bhps_long$fem)
bhps_long$y1991 <- as.numeric(bhps_long$year==1991); table(bhps_long$y1991)
table(bhps_long$hlstat) # (missing data already addressed via e.g. above)

# Random effects panel model:

hl_ranef <- lmer(hlstat ~ fem + age + y1991  + (1 | pid), data = bhps_long)
summary(hl_ranef, corr=F)


# Fixed effects panel model

# (using the mean deviation method - for discussion see e.g. 
#  http://karthur.org/2019/implementing-fixed-effects-panel-models-in-r.html ) 

summary(bhps_long)

## Calculate group means for time varying variables and re-attach them to the data frame:

year_means <- aggregate(bhps_long$y1991, list(bhps_long$pid), FUN=mean)
colnames(year_means) <- c("pid", "year_mean"); head(year_means)
age_means <- aggregate(bhps_long$age, list(bhps_long$pid), FUN=mean)
colnames(age_means) <- c("pid", "age_mean"); head(age_means)

bhps_long_2 <- merge(bhps_long, year_means, by.x="pid",  by.y="pid",
               all.x=T, all.y=T, sort=F, suffixes = c(".x",".y") )
bhps_long_3 <- merge(bhps_long_2, age_means, by.x="pid",  by.y="pid",
               all.x=T, all.y=T, sort=F, suffixes = c(".x",".y") )
dim(bhps_long_3); names(bhps_long_3)

# Add the deviation from mean variables: 
bhps_long_3$year_dev <- bhps_long_3$y1991  - bhps_long_3$year_mean  
bhps_long_3$age_dev <- bhps_long_3$age  - bhps_long_3$age_mean  
dim(bhps_long_3); names(bhps_long_3)


## So the fixed effects model will be: 

hl_fixef <- lm(hlstat ~ age_mean + age_dev + year_mean + year_dev, data = bhps_long_3)
summary(hl_fixef)


## Which ought to give the same results as the excessively parameterised model:

hl_fixef2 <- lm(hlstat ~ fem + age + y1991 + as.factor(pid), data=bhps_long) #warning - memory use 
summary(hl_fixef2)


#############################################










############################################
## Ex 2.5) An observation on trying random slopes models with low average group size in lmer 
##############

# BHPS GHQ (individuals in households) example: 
bhq <- read.dta(paste(bhps_ghq_extract1))

names(bhq); hist(bhq$ghq)
use_vars <- c("ohid" , "ghq", "fem", "age", "age2", "cohab", "degdip", "vocq", "noqual", 
                       "emp_10hrs", "spghq2", "gdn", "lnkids") 
bhq_2 <- bhq[use_vars];  summary(bhq_2); names(bhq_2); dim(bhq_2) # are NA's, but no numeric missing codes
bhq_2$c_age <- bhq_2$age - 40 ; bhq_2$c_age2 <- bhq_2$age^2 - 40^2 # for centred age & age2

## Linear regression
(lin2 <- lm(formula = ghq ~   fem + c_age + c_age2 + cohab + degdip + vocq + noqual + emp_10hrs 
      + spghq2 + gdn + lnkids,      data = bhq_2) )


## Random intercepts multilevel model:

(ghq_ri <- lmer(formula = ghq ~ fem + c_age + c_age2 + cohab + degdip + vocq + noqual + emp_10hrs 
      + spghq2 + gdn + lnkids    + (1 | ohid  ),    data = bhq_2 ))

## Random slopes multilevel model...?

(ghq_rs <- lmer(formula = ghq ~ fem + c_age + c_age2 + cohab + degdip + vocq + noqual + emp_10hrs 
      + spghq2 + gdn + lnkids    + (1  + c_age  | ohid  ),    data = bhq_2 ) )
ghq_rs

## Not the expected behaviour?

## So, ghq_rs is a theoretically reasonable model to consider (allowing for intercept variance and c_age variance
#    at the household level), however lmer has prevented the model from running because it observes
#    that the low average group size means that the number of random effects terms (i.e. household level intercepts
#    plus household level slopes) will be greater than the total number of cases. 
## This model can work successfully in other packages, but is not available in R. 


####################################








###########################################
## ii)  Random effects for modelling intersectionality  
###################



## Data prep stuff...

bh_w15h <- read.dta13(paste(bhps_w15_health_extract1), convert.factors=F)
summary(bh_w15h); dim(bh_w15h); names(bh_w15h)
table(bh_w15h$hlstat) # doc: https://www.iser.essex.ac.uk/bhps/documentation/volb/wave15/oindresp6#ohlstat

bh_w15h <- subset(bh_w15h, (bh_w15h$hlstat >= 1 & bh_w15h$hlstat <= 5 & bh_w15h$jbcssm >= 1 
           & bh_w15h$age >= 1 & bh_w15h$qfedhi >= 1 & bh_w15h$qfedhi <= 12))  # listwise deletes missing data 
bh_w15h$good_health <- 6 - bh_w15h$hlstat; table(bh_w15h$good_health) # reverse coding outcome for convenience

educ_means <- aggregate(bh_w15h$jbcssm, list(bh_w15h$qfedhi), FUN=mean)
colnames(educ_means) <- c("qfedhi", "educ_sc"); head(educ_means) # i.e. a database of education scale scores 
bh_w15h <- merge(bh_w15h, educ_means, by.x="qfedhi",  by.y="qfedhi",
               all.x=T, all.y=T, sort=F, suffixes = c(".x",".y") ) ; names(bh_w15h)
# i.e. educ_sc can be used as a scale of relative educational attainment 
ed_mean <- mean(bh_w15h$educ_sc) # mean value of that
bh_w15h$educ_sc2 <- bh_w15h$educ_sc - ed_mean # centred version of the score 

table(bh_w15h$racel) # documentation: https://www.iser.essex.ac.uk/bhps/documentation/volb/wave15/oindresp2#oracel 
bh_w15h$race2 <- bh_w15h$racel ; table(bh_w15h$race2)
bh_w15h$race2[bh_w15h$racel==-8] <- NA ; table(bh_w15h$race2)
bh_w15h$race2[bh_w15h$racel >= 1 & bh_w15h$racel <= 4] <- 1 ; table(bh_w15h$race2)
bh_w15h$race2[bh_w15h$racel >= 6 & bh_w15h$racel <= 9] <- 9 ; table(bh_w15h$race2)
bh_w15h$race2[bh_w15h$racel >= 11 & bh_w15h$racel <= 13] <- 11 ; table(bh_w15h$race2)
bh_w15h$race2[bh_w15h$racel >= 14 & bh_w15h$racel <= 16] <- 16 ; table(bh_w15h$race2)
bh_w15h$race2[bh_w15h$racel >= 17 & bh_w15h$racel <= 18] <- 18 ; table(bh_w15h$race2) # i.e. substantive recodes 

hist(bh_w15h$doby) # doby distribution
bh_w15h$bir_coh <- NA
bh_w15h$bir_coh[bh_w15h$doby >= 1900 & bh_w15h$doby <=1944] <- 1; table(bh_w15h$bir_coh)
bh_w15h$bir_coh[bh_w15h$doby >= 1945 & bh_w15h$doby <=1964] <- 2; table(bh_w15h$bir_coh)
bh_w15h$bir_coh[bh_w15h$doby >= 1965 & bh_w15h$doby <=1979] <- 3; table(bh_w15h$bir_coh)
bh_w15h$bir_coh[bh_w15h$doby >= 1980 & bh_w15h$doby <=1990] <- 4; table(bh_w15h$bir_coh) # i.e. makes synthetic cohorts

table(bh_w15h$sex); bh_w15h$fem <- as.numeric(bh_w15h$sex==2); table(bh_w15h$fem)


## Intersectional inequalities?

table(bh_w15h$fem); table(bh_w15h$race2);  table(bh_w15h$bir_coh)


## Question will be about exploring intersectional inequalities of gender, birth cohort and ethnicity, in relation to health inequalities


## Health inequalities: health is better for the socially advantaged:
hlth_1 <- lm(good_health ~  educ_sc2, data=bh_w15h); summary(hlth_1)


## Intersectional health inequalities: do health inequalities play out differently by different intersectional identities?

## Lots of different model possibilities...?

##...Via conventional fixed part model terms: main effects only
hlth_2 <- lm(good_health ~  educ_sc2 + fem + as.factor(race2) + as.factor(bir_coh), data=bh_w15h); summary(hlth_2)

## ...Via conventional fixed part model terms: main effects and educ interactions only
hlth_3 <- lm(good_health ~  educ_sc2 + fem + as.factor(race2) + as.factor(bir_coh) + 
          fem:educ_sc2 + as.factor(race2):educ_sc2 + as.factor(bir_coh):educ_sc2, data=bh_w15h); summary(hlth_3)
# (Suggests gender and birth cohort but not ethnicity patterns in this format)


## ...via conventional fixed part model terms: all theoretically plausible interactions
hlth_4 <- lm(good_health ~  educ_sc2 + fem + as.factor(race2) + as.factor(bir_coh) + 
          fem:educ_sc2 + as.factor(race2):educ_sc2 + as.factor(bir_coh):educ_sc2 + 
          as.factor(race2):educ_sc2:fem + as.factor(bir_coh):educ_sc2:fem + 
          as.factor(bir_coh):as.factor(race2):educ_sc2:fem , data=bh_w15h); summary(hlth_4)

# In principle, this is a decent model that allows for all intersectional inequalities and suggests
#   some interesting, complex empirical patterns. However, it's not parsimonious, and the 
#   estimates for many groups are contingent on only a few cases  





### Would random effects help...?

## ...Use dplyr to get a grouping index variable: 
library(dplyr)
bh_w15h <- bh_w15h %>% group_by(fem, bir_coh, race2) %>% mutate(id_xx = cur_group_id())
names(bh_w15h); table(bh_w15h$id_xx)
## 59 different permutations of our 3 social categories, as defined 


# 'Main effects' model
(hlth_5 <- lmer(formula = good_health ~  educ_sc2   + (1 | id_xx  ),    data = bh_w15h ))

ranef(hlth_5); hist(ranef(hlth_5))
var_comp <- as.data.frame(VarCorr(hlth_5))$vcov; var_comp 
icc_hlth_5 <- var_comp[1] / (var_comp[1] + var_comp[2]) ; icc_hlth_5
# i.e. net of education, the intersectional groups have (modestly) different health experiences


# 'interaction patterns' model
(hlth_6 <- lmer(formula = good_health ~  educ_sc2   + (1 + educ_sc2 | id_xx  ),    data = bh_w15h ))

# i.e. net of education, the intersectional groups have different health experiences and there is variation 
#    from group to group in the impact of education on health 

# Observe: there are variations in slopes, but the variations in intercepts are much larger



## However, whilst the formulation above seems useful, the 2-level structure means that there is no 
#    recognition in the model of shared memberships within each structure. 

## In principle, the ideal model is cross-classified with main effects and interactions (and, possibly, with 
#    random slopes that differ at each level)

# For example: 
(hlth_7 <- lmer(formula = good_health ~  educ_sc2   
     + (1 | fem  ) + (1 | bir_coh  ) + (1 | race2  ) ,    data = bh_w15h ))

# Comment: this example works ok, with only a few intersections, but in many datasets, 
#  a substantial cross-classified model like this would not be   expected to be easily identified


# -> The 'MAIDHA' approach in general seeks to analyse intersectionality by defining large numbers of 
#   groups according to social intersections then fitting them as a level in a random effects format


###############################################################
###############################################################




