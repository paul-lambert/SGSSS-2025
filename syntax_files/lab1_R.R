

### Title and contents

#### SGSSS Summer School 2025

## Short course: Multilevel Models for Applied Social Research

## Materials by Paul Lambert and Kate O'Hara (Univ. Stirling), and Richard Zijdeman (International Institute for Social History)

##  LAB 1 - R EXERCISES - 'SELECTED POPULAR MODELS AND FUNCTIONS FOR MULTILEVEL MODELLING IN APPLIED SOCIAL RESEARCH'

###   Linked code, data and metadata: at https://github.com/paul-lambert/SGSSS-2025/

##    Version: 16/Jun/2025, written for R 4.4.3



################################### 


#### Contents listing

###   Title and contents 
###   Paths and preliminaries
###   Ex 1.1: Popular random effects models on the London Reading Test datasest 
###   Ex 1.2: Illustrating models and higher level residuals using the Pupil Popularity dataset (Hox et al. 2017) 
###   Ex 1.3: Illustrating random effects models for linear and non-linear outcomes 
###   Ex 1.4: Illustrating random effects models with more than 2 levels  


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


gcse_data <- as.character('C:/1/sgsss-2025/data/gcse.dta') 
## Used in Ex 1.1-3
## The 'London Reading Test' dataset (in a Stata format file)  
## You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
## Provenance: This dataset is used in many training materails on multilevel modelling in the UK. It was used in Harvey Goldsteins influential 
#     book 'Multilevel Statistical Models'. This version of the dataset was accessed from the website for Rabe-Hesketh & Skrondal's textbook 'Multilevel
#     and Longitudinal Modeling using Stata'


popularity_data <- as.character('C:/1/sgsss-2025/data/popular2.dta')
## Used in Ex 1.2-4
## The 'Pupil popularity' dataset from the Hox textbook (Hox 2010/Hox et al. 2017) (in a Stata format file)  
## You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
## Provenance: This dataset is used extensively in the Hox textbook on multilevel models. It's simulated data, the scenario is of 
##  data on students in classes, where students have been asked to rate their own popularity and other features of the class. 


nurses_data  <- as.character('C:/1/sgsss-2025/data/nurses.dta')
## Used in Ex 1.3
## The 'Nurses stress at work' dataset from the Hox textbook (Hox 2010/Hox et al. 2017) (in a Stata format file)  
## You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
## Provenance: This dataset is used in the Hox textbook on multilevel models. It's simulated data, the scenario is of 
##  data on the stress experienced by nurses, who are nested both in wards (level 2) and hospitals (level 3) 




#######

### Libraries: 
### The code below identifies and installs libraries used in Lab 1 (in a resource-heavy way)
##    (you might prefer to modify the installation code for your machine, e.g. if you don't need all of these or 
##      wish to include other installs)


# If necessary, add a space to your machine where you will be fine to install libraries to:
.libPaths() # shows current libary paths
.libPaths(c(.libPaths(), library_space)) # adds new library path to current library path(s)

pkgs <- c(   "foreign",  "lme4", "ggplot2", "nlme" , "ordinal", "Rcpp",  "lmtest" , "lattice" )

## Packages description
# foreign: For reading in data in other sofware formas
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








##################################################################################################
##################################################################################################


#########################################

###   Ex 1.1: Popular random effects models on the London Reading Test datasest 



gcse <- read.dta(paste(gcse_data), convert.factors=F)
# Reads in the GCSE dataset used in lectures 

names(gcse); hist(gcse$gcse)

## Linear regression
lin1 <- (lm(formula = gcse ~ lrt + girl + as.factor(schgend), data=gcse) ) 
summary(lin1)

## Random intercepts multilevel model with 2-levels, clustering by class: 

## Null model: 
mlm0a <- (lmer(formula = gcse ~  + (1 | school), data=gcse) ) 
summary(mlm0a)


# Aside - the default estimation algorithm uses 'REML', but we could switch it to MLE with extra spec: 
mlm0b <- (lmer(formula = gcse ~  + (1 | school), data=gcse, REML = FALSE) ) 
summary(mlm0b)



## Random intercepts multilevel model with 2-levels, clustering by class, with additional covariates: 

mlm1a <- (lmer(formula = gcse ~  + lrt + girl + as.factor(schgend) + (1 | school), data=gcse) ) 
summary(mlm1a)



## More on random effects outputs: 
# Look at the model estimates:
summary(mlm1a)

# A list of the fixed part coefficients can be pulled out with:
fixef(mlm1a)

# A list of the random part coefficients, i.e. the cluster level residuals, can be pulled out with
ranef(mlm1a)

# The model parameters as they would apply to each school is visible by: 
coef(mlm1a)
# i.e. the model has an average intercept of -1.6844, but this output has the adjusted intercept for each 
#    school, i.e. after adding the school-specific cluster residual to the global intercept



# The lowest level residuals can also be retrieved, here e.g. just showing some of them:
head(resid(mlm1a))
var(resid(mlm1a)); hist(resid(mlm1a)) # i.e. summarises the lower level error variance and it's distribution
var(ranef(mlm1a)$school); hist(as.matrix(ranef(mlm1a)$school)) # i.e. summarises the higher level residuals 

## To summarize the variance proportions and ICC:
summary(mlm1a)
# We can see that the ICC will equal:
icc <- 8.58  / (8.58 + 56.25) ; icc

## To get the ICC in an automated way:

VarCorr(mlm1a) # i.e. these are the relevant stored results 
var_comp <- as.data.frame(VarCorr(mlm1a))$vcov; var_comp # i.e. these store the original values as variances not sd's
icc_mlm1a <- var_comp[1] / (var_comp[1] + var_comp[2]) ; icc_mlm1a 


## Note we would also try to get the ICC via the residuals at each level, however the 
#    figures aren't exactly the same 
icc_approx <- (var(ranef(mlm1a)$school)) / (var(resid(mlm1a)) + var(ranef(mlm1a)$school)); icc_approx
##




### Random slopes model examples


mlm2a <- (lmer(formula = gcse ~  + lrt + girl + as.factor(schgend) + (1 + lrt | school), data=gcse) ) 
summary(mlm2a, corr=FALSE)

# Observe: no standard error/p-values for random part variance estimates 
#   (unlike other software) - the lmer programmers argue that it isn't necessary 
# (I've also used 'corr=FALSE' to suppress default display of 'correlation of fixed effects', for simplicity) 


# The standard test for whether a random slope is beneficial is an LR comparison of 
#    model fit against the equivalent model without the slope: 

summary(mlm1a,  corr=FALSE); summary(mlm2a, corr=FALSE); lrtest(mlm2a, mlm1a)

# In this example, model fit is substantially improved by allowing for the random slope
#    (evident form the small p-value); we'd conventionally favour the random intercepts model




## In lmer the standard random slopes formulation does not constrain covariances between random effects: 
summary(mlm2a, corr=FALSE)
# (here, the slope and intercept have an estimated correlation of 0.57: schools with a 
#    higher intercept also tend to have a higher slope for LRT) 

## In random effects literatures it's common to consider setting constraints on potential correlations, 
#     such as to fit the model which constrains the correlation to be zero. To do this in R we could use: 
 
mlm2a_c2 <- (lmer(formula = gcse ~  + lrt + girl + as.factor(schgend) + 
          (1 | school) + (lrt - 1 | school), data=gcse) ) 
summary(mlm2a_c2, corr=FALSE)
# (i.e. we specify two different random error sections, one for the intercept and another separate (uncorrelated) 
#   one for the random slope minus intercept, having the effect of setting these as uncorrelated)
#   ...In this example, this isn't a good model and in fact it doesn't converge





#############################################################################################







#########################################

###   Ex 1.2: Illustrating models and higher level residuals using the Pupil Popularity dataset (Hox et al. 2017)
#########################################



pop.df <- read.dta(paste(popularity_data), convert.factors=F) # pop.df for popular.dataframe
# Reads in the data from Hox c2  

names(pop.df)
summary(pop.df$popular)
qplot(pop.df$popular, binwidth = 0.1)


## Random intercepts multilevel null model with 2-levels, clustering by class: 
(fm1.ML <- lmer(formula = popular ~  1 + (1 | class  ), pop.df , REML=FALSE))
(fm1.REML <- lmer(formula = popular ~  1 + (1 | class  ), pop.df , REML=TRUE))
# By putting the above line in redundant parentheses the output is printed in addition

## Alternative display of the model estimates:
summary(fm1.REML)

## Note that the model has an average intercept, with higher level residuals around the 
#    intercept leading to group specific intercepts: 
coef(fm1.REML) # provides the intercepts (based on residuals) for the 100 classes
cf <- coef(fm1.REML)
par(mfrow = c(1, 2)); hist(as.matrix(cf$class)); plot(cf$class, pch = '|') 
# (just a way of visualising the distribution of class-specific intercepts)

# For the ICC:
var_comp <- as.data.frame(VarCorr(fm1.REML))$vcov; 
icc_fm1.REML <- var_comp[1] / (var_comp[1] + var_comp[2]) ; icc_fm1.REML 


######



#### Now some random intercepts models with covariates (all with reml)

## With just the one explanatory variables 
fm2 <- lmer(formula = popular ~ extrav + (1 |class )  ,    data = pop.df ) 
summary(fm2)


# with multiple fixed covariates
names(pop.df)
pop.df$ext_exp <- pop.df$extrav*pop.df$texp
summary(pop.df$ext_exp)
# (creates the interaction term between extraversion and teacher's experience)


fm3 <- lmer(formula = popular ~ sex + extrav + texp + 
                   ext_exp + (1 | class  ), data = pop.df, REML = TRUE )
summary(fm3)

# (An usual feature of the summary function for lmer output is the default display of the 'correlation of 
#    fixed effects'. These are summary statistics related to the 'variance-covariance matrix of the 
#    estimates'. Broadly speaking, they indicate the extent to which multiple sample-based estimates of  
#    the relevant coefficients would tend to be correlated to each other - e.g. in this model: 
#    when the estimate of the texp coefficient is higher, the estimate of the intercept is lower. 
#    In my experience it is rare to use these figures in any substantive interpretation, but
#    they are nevertheless provided by default in lmer outputs but not in other outputs). 
#     (the can be turned off from the summary display with 'corr=FALSE')
#     (see e.g. https://data.library.virginia.edu/correlation-of-fixed-effects-in-lme4/)

summary(fm3, corr=FALSE)

###########




#### Some random slopes model examples (random coefficients) 

# 1 explanatory variable and a random slope for it
fm4 <- lmer(formula = popular ~ extrav + (1 + extrav |class )  , 
            data = pop.df ) 
summary(fm4, corr=FALSE)


# Several explanatory variables and 1 random slope

fm5 <- lmer(formula = popular ~ sex + extrav + texp + 
                   ext_exp + (1 + extrav | class  ), data = pop.df, REML =TRUE )
summary(fm5, corr=FALSE)

# Observe: no standard error/p-values for random part variance estimates 
#   (unlike other software) - the lmer programmers argue that it isn't necessary 


# The standard test for whether a random slope is beneficial is an LR comparison of 
#    model fit against the equivalent model without the slope: 

fm5_ri <- lmer(formula = popular ~ sex + extrav + texp + 
                   ext_exp + (1 | class  ), data = pop.df, REML =TRUE )
fm5 <- lmer(formula = popular ~ sex + extrav + texp + 
                   ext_exp + (1 + extrav | class  ), data = pop.df, REML =TRUE )
summary(fm5, corr=FALSE); lrtest(fm5, fm5_ri)

# In this example, model fit is not substantially improved by allowing for the random slope; 
#    we'd conventionally revert back to the random intercepts model



# Several random slopes..?
fm6 <- lmer(formula = popular ~ sex + extrav + texp + 
                   ext_exp + (1 + extrav + sex | class  ), data = pop.df, REML =TRUE )
summary(fm6, corr=FALSE)
# This model doens't converge smoothly - we would conventionally abandon it from analysis 



########  

## One important limitation in the above is that the explanatory variables have not been 
#   centred, but it would generally be desirable to centre them, to ensure the estimated intercept 
#   variance terms are being calculated at sensible values


fm5_ri <- lmer(formula = popular ~ sex + extrav + texp + 
                   ext_exp + (1 | class  ), data = pop.df, REML =TRUE )
fm5 <- lmer(formula = popular ~ sex + extrav + texp + 
                   ext_exp + (1 + extrav | class  ), data = pop.df, REML =TRUE )

table(pop.df$sex) # this variable is 0/1 coded, so no need to centre (0 is already sensible)
extrav_mean <- mean(pop.df$extrav); texp_mean <- mean(pop.df$texp)
par(mfrow = c(1, 2)); hist(pop.df$extrav); hist(pop.df$texp); extrav_mean ; texp_mean 
# for both these measures, the value 0 is not in the normal range of data so centring 
#    is appropriate 
names(pop.df)
# The data actually already has centred versions of them, though we could construct them if we preferred)

pop.df$Cen_extrav <- pop.df$extrav - extrav_mean
pop.df$Cen_texp <- pop.df$texp - texp_mean
pop.df$Cen_ext_exp <- pop.df$Cen_extrav*pop.df$Cen_texp
# (i.e. makes two centred variables, then re-calculates the interaction term to use them)

fm5_ri_c <- lmer(formula = popular ~ sex + Cen_extrav + Cen_texp + 
                   Cen_ext_exp + (1 | class  ), data = pop.df, REML =TRUE )
fm5_c <- lmer(formula = popular ~ sex + Cen_extrav + Cen_texp + 
                   Cen_ext_exp + (1 + extrav | class  ), data = pop.df, REML =TRUE )

summary(fm5, corr=FALSE); summary(fm5_c, corr=FALSE)
lrtest(fm5, fm5_ri); lrtest(fm5_c, fm5_ri_c)
# i.e. no important differences in results between centred and uncentred, but some different paramaters


## Examples of evaluating if models improve upon each other 

# comparing random intercepts and random slopes models with 1 X var 
summary(fm4, corr=FALSE); summary(fm2, corr=FALSE)
# using anova:
anova(fm4, fm2)
# i.e. fm4 is a significant inprovement in fit 

# using likelihood statistics in detail: 
like.diff = logLik(fm3) - logLik(fm2)
df.diff = attr(logLik(fm4), 'df') - attr(logLik(fm2), 'df')
pchisq(as.numeric(like.diff) * 2, df=df.diff, lower.tail=F)

# Using the 'lrtest' tool from the package 'lmtest', as deployed above:
lrtest(fm4, fm2)

# Repeat for other relevant nested model comparisons , e.g. 
anova(fm6.REML, fm5.REML)
# The extra random slope in fm6, compared to fm5, does not 
#   significantly improve the model 


###################

## Plotting model results 



#####
# plotting residuals with ggplot2

# http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot
randoms<-ranef(fm1.REML, condVar = TRUE)
qq <- attr(ranef(fm1.REML, condVar = TRUE)[[1]], "postVar")
rand.interc<-randoms$class


fm1.df<-data.frame(Intercepts=randoms$class[,1],
               sd.interc=2*sqrt(qq[,,1:length(qq)]),
               lev.names=rownames(rand.interc))
fm1.df <- fm1.df[with(fm1.df, order(Intercepts)), ]
fm1.df$order <- reorder(fm1.df$lev.names, fm1.df$Intercepts)

# All objects put in one data frame. For error intervals sd.interc is calculated
# as 2 times square root of variance.
p1 <- ggplot(fm1.df,aes(order,Intercepts)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("group id")
p1.1 <- p1 + geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin=Intercepts - sd.interc, 
                    ymax=Intercepts + sd.interc), 
                width=0,color="black") + 
  geom_point(color = "blue") +
  labs(title = "Caterpillar plot of popularity on extaversion")
p1.1

######
# now let's plot intercept lines for an intercept model
fm2 <- lmer(formula = popular ~ extrav + (1 |class )  , 
                 data = pop.df ) 

fm2.df <- data.frame(ranef(fm2)[1], fixef(fm2)[1], fixef(fm2)[2])
names(fm2.df) <- c("int.ran", "int.fix", "extrav.fix")
fm2.df$int.tot <- fm2.df$int.fix + fm2.df$int.ran

p2 <- ggplot(aes(x = extrav, y = popular, group = class), data = pop.df) +
  geom_point(colour = "blue") +
  geom_abline(aes(intercept = int.tot, slope = extrav.fix), data = fm2.df,
              size = .1) +
  geom_abline(aes(intercept = int.fix, slope = extrav.fix), data = fm2.df,
              size = 1.5, colour = "red", alpha = .5) +
  theme_bw() + scale_x_continuous(breaks = c(1:10)) + 
  scale_y_continuous(breaks = c(0:10)) +
  labs(title = "Random intercept model of popularity on extaversion")
p2
# size sets the size of the object, here the linewidth
# alpha determines the level of transparency





#####
# now let's plot intercept lines for a random coefficients model
summary(fm4) 
fm4.df <- data.frame(ranef(fm4)[1], fixef(fm4)[1], fixef(fm4)[2])
names(fm4.df) <- c("int.ran", "extrav.ran", "int.fix", "extrav.fix")
fm4.df$int.tot <- fm4.df$int.fix + fm4.df$int.ran
fm4.df$extrav.tot <- fm4.df$extrav.fix + fm4.df$extrav.ran


p3 <- ggplot(aes(x = extrav, y = popular, group = class), data = pop.df) +
  geom_point(colour = "blue") +
  geom_abline(aes(intercept = int.tot, slope = extrav.tot), data = fm4.df,
              size = .1) +
  geom_abline(aes(intercept = int.fix, slope = extrav.fix), data = fm2.df,
              size = 1.5, colour = "red", alpha = .5) +
  theme_bw() + scale_x_continuous(breaks = c(1:10)) + 
  scale_y_continuous(breaks = c(-1:10)) +
  labs(title = "Random coefficients model of popularity on extaversion")
p3

# To save these graphs, e.g. p3 as a .png file:
ppi <- 300 # set the quality of the figure (# of pixels)
png("./ran_coef_model_hox01.png",
    width=10*ppi, height=8*ppi, res=ppi) # shape of the figure
p3
dev.off()
# the figure should now have been saved in the working directory

#####
######################################



 






##################################################################################################


#########################################

###   Ex 1.3: Illustrating random effects models for linear and non-linear outcomes 



## Comments: With R, Non-linear outcomes random effects models are available as follows: 

# Binary:   glmer
# Poisson:  glmer 
# Ordered:  clmm 
# Unordered multi-category:  mlogit; and/or with additional programming 
#    - see a discussion and examples in Gelman and Hill 2007, c14-5.
####



########################################################
########################################################
##  1) A RANGE OF MODELS FOR NON-LINEAR OUTCOMES ON THE POPULARITY DATA 
########################################################
########################################################



## Examples on the student popularity dataset: 
pop2 <- read.dta(paste(popularity_data), convert.factors=F)
names(pop2)
hist(pop2$popular)

# Generate (artificial) binary, ordered and count-like outcome measures 
pop2$pop2 <- as.numeric(pop2$popular > 5)
table(pop2$pop2)
pop2$pop3 <- pop2$pop2 + 1 + (as.numeric(pop2$popular > 7))
table(pop2$pop3)
pop2$pop7 <- ((as.numeric(pop2$popular > 5)) + (as.numeric(pop2$popular > 6)) 
              + (as.numeric(pop2$popular > 6.5)) + (as.numeric(pop2$popular > 7)) +
           (as.numeric(pop2$popular > 7.5)) + (as.numeric(pop2$popular > 8))   )
table(pop2$pop7)


## Examples of single level models and random effects models: 


## Binary outcome:

table(pop2$pop2)

# Single level binary outcome 
(b1a <- glm(pop2 ~ sex + Cextrav + Ctexp ,
            family = binomial(link="logit"), data = pop2)) 

# Random intercepts binary outcome
(b1b <- glmer(pop2 ~ sex + Cextrav + Ctexp + (1 | class),
            family = binomial(link="logit"), data = pop2)) 

# Random slopes binary outcome 
(b1c <- glmer(pop2 ~ sex + Cextrav + Ctexp + (1 + Cextrav | class),
            family = binomial(link="logit"), data = pop2))

###########




## Ordered logit outcome:

# Single level with 'proportional odds logistic regression':
(o1a <- polr(as.factor(pop3) ~ sex + Cextrav + Ctexp, data=pop2)) 

# Single level with 'ordinal' clm
(o2a <- clm(as.factor(pop3) ~ sex + Cextrav + Ctexp ,  data=pop2, link="logit") ) 

# Random intercepts with 'ordinal' clmm
(o2b <- clmm(as.factor(pop3) ~ sex + Cextrav + Ctexp + (1 | class), 
    data=pop2, link="logit") ) 


# Random slopes with 'ordinal' clmm
(o2c <- clmm(as.factor(pop3) ~ sex + Cextrav + Ctexp + (1 + Cextrav | class), 
    data=pop2, link="logit") ) 




###########



## mlogit model

# These are not illustrated here as they are not provided within glmer

# For info though there are at least three dedicated packages for multinomial logit models: mlogit, mclogit, and mnlogit.

## Martin Elff's 'mclogit' package allows for random effects models to be added to mlogit formulations, 
#    described for instance at https://cran.r-project.org/web/packages/mclogit/mclogit.pdf 
#    and https://github.com/melff/mclogit/ 

## Here is a description of fitting a random effects mlogit via MCMC methods in R:
#  http://hlplab.wordpress.com/2009/05/07/multinomial-random-effects-models-in-r/

## Gelman and Hill 2007 also discuss how the mlogit and ologit can be set up with 
#    random effects, but they don't provide example code


############



## Poisson outcome: 

# Single level
(p1a <- glm(pop7 ~ sex + Cextrav + Ctexp ,
            family = poisson(link="log"), data = pop2)) # no random effects (ergo, glm not glmer)

# Random intercepts
(p1b <- glmer(pop7 ~ sex + Cextrav + Ctexp + (1 | class),
            family = poisson(link="log"), data = pop2)) 

# Random slopes
(p1c <- glmer(pop7 ~ sex + Cextrav + Ctexp + (1 + Cextrav | class),
            family = poisson(link="log"), data = pop2)) 



###########

##################################################################################################









#########################################

###   Ex 1.4: Illustrating random effects models with more than 2 levels  



########################

###### 3 level model examples 
nurses <- read.dta(paste(nurses_data))
summary(nurses)

# Outcome variable: stress
hist(nurses$stress)


## Regular linear regressions:

lin0 <- lm(stress ~ cons, data = nurses) 
lin1 <- lm(stress ~ expcon + age + gender + experien + wardtype + hospsize, data = nurses)

summary(lin0)
summary(lin1)

## Multilevel linear regressions: 


# Ward only clustering
l2a <- lmer(stress ~ expcon + age + gender + experien + 
                       wardtype + hospsize  + (1 | wardid), data = nurses)
summary(l2a) 

# Hospital only clustering
l2b <- lmer(formula = stress ~ expcon + age + gender + experien + 
                       wardtype + hospsize  + (1 | hospital), data = nurses)
summary(l2b) 


## Three level model examples : 

l3a <- lmer(formula = stress ~ expcon + age + gender + experien + 
                       wardtype + hospsize  
      + (1 | wardid) + (1 | hospital), data = nurses)
summary(l3a) 

l3am <- lmer(formula = stress ~ expcon + age + gender + experien + 
               wardtype + hospsize  
             + (1 | wardid) + (1 | hospital), data = nurses, REML=FALSE)
summary(l3am) 
# (for comparison with the mle estimate from the Stata labs)


##  It's as simple as that..!! 
# (In some versions of lme it is also possible to specify explicitly that the 3-level cluster 
# structure must be neatly nested, whereas by default a two-level cross-classification could be 
# incorporated here if relevant).
# The random effects specification would be something like '(1 | hospital/wardid'), but for the lmer
#   package above this does not work). 

##################################################






