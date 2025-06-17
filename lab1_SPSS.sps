* Encoding: UTF-8.
***************************************
** Title and contents

**** SGSSS Summer School 2025

** Short course: Multilevel Models for Applied Social Research

** Materials by Paul Lambert and Kate O'Hara (Univ. Stirling)

***  LAB 1 - SPSS EXERCISES - 'SELECTED POPULAR MODELS AND FUNCTIONS FOR MULTILEVEL MODELLING IN APPLIED SOCIAL RESEARCH'

***   Linked code, data and metadata: at https://github.com/paul-lambert/SGSSS-2025/

**    Version: 16/Jun/2025, written for SPSS v29.0.1.

***************************************.


******************************.
**** Contents listing.

***   Title and contents .
***   Paths and settings.
***   Ex 1.1: Popular random effects models on the London Reading Test datasest .
***   Ex 1.2: Selected random intercepts and slopes models within the pupil popularity data.
***   Ex 1.3: Illustrating random effects models for linear and non-linear outcomes .
***   Ex 1.4: Illustrating random effects models with more than 2 levels  .


********************************.


* . 
* .


**********************************************.
**********************************************.

** PATHS AND SETTINGS


* Downloaded data folder (this should be the folder on your machine where you have downloaded the lab data files to). 
define !path1a () "C:\1\sgsss-2025\data\" !enddefine. 
* (Subsequent commands ought to work smoothly if all of the downloaded files have been copied into this folder).

* Temporary folder (for storing temporary files; should refer to any folder on your machine where you can save temporary files) .
define !path9 () "c:\temp\" !enddefine. 



set onumbers=both ovars=both tnumbers=both tvars=both printback=on . 
* changes some default settings. 



**********************************************************.
**********************************************************.



*********************************************************************************************.
*********************************************************************************************.
***   Ex 1.1: Popular random effects models on the London Reading Test datasest .


dataset close all. 
get stata file=!path1a+"gcse.dta". 
* Stata format versions of the files from Rabe-Hesketh and Skrondal 2008 .
descriptives var=all. 
* This is a dataset of 4059 pupils nested in 65 schools .

sort cases by school pupil .
list /variables=all /cases= from 1 to 50.
* E.g. illustrates the data structure.

temp. 
select if (school=2).
descriptives var=all. 
* E.g. shows summary stats for measures for pupils from school 2 .




** To explore the hierarchical structure a bit: . 
descriptives var=school student.
* We've got 4059 cases in the data.
* ? How many different schools.
sort cases by school student.
compute first=1.
if (school=lag(school)) first=0.
fre var=first. /* Only one (arbitrary) case per cluster is highlighted */
* This shows there's 65 different schools.
* ? How many pupils per class.
sort cases by school student.
aggregate outfile=!path9+"m1.sav" /break=school /nlev1=N(student).
match files file=* /table=!path9+"m1.sav" /by=school.
variable label nlev1 "Number of level 1 units in level 2 unit".
graph /histogram=nlev1.
* This shows the size of the schools in which each pupil is located; if you want just the size per unit then:.
temp.
select if first=1.
fre var=nlev1. 

** In summary:  There's 4659 students, nested in 65 schools, between 2 and 198 students per school . 


** The dependent and explanatory variables of interest are: .
descriptives var=gcse lrt girl schgend . 
variable label gcse "GCSE score (Z score*10)".
variable label lrt "London Reading test score (Z score*10)".
variable label girl "Female".
variable label schgend "Type of school".
add value labels schgend  1 "Mixed gender" 2 "Boys only" 3 "Girls only".
* (Labels from Rabe-Hesketh and Skrondal, 2010: 141).
means tables = gcse lrt by schgend .

descriptives var=gcse lrt /statistics=all. 
correlate var=gcse lrt girl. 

* Strong patterns apparent .

* (Aside: You may frown, but it's fairly robust to use linear correlations for dummy variables...) .
means table=gcse by girl /statistics=anova .
anova var=gcse by girl(0 1) /statistics=reg.
* (see!). 

graph /bar=mean(gcse) mean(lrt) by schgend. 
* Suggests strong school type effect.

fre var=schgend. 
compute sch_2=(schgend=2).
compute sch_3=(schgend=3).
fre var=schgend sch_2 sch_3.


** Regressions not involving random effects .
regression /var= gcse lrt girl sch_2 sch_3 /dep=gcse /method=enter. 
* (LRT is the most important, but gender and school type also matter).


**** Here's an illustration of the overall data and predicted values across lrt :.
regression /var= gcse lrt girl sch_2 sch_3 /dep=gcse /method=enter /save pred(p_gcse). 
graph /scatterplot(overlay)=lrt lrt with gcse p_gcse (pair) .
* (The range of points shows the range of predicted values covered across lrt positions).





**** Regressions - school by school?  .

sort cases by school.
split files by school.
regression /var=gcse lrt   /statistics=coefs /dep=gcse  /method=enter  .
split files off. 

* (Conventional view: this can be done, but it's not very parsimonious, and each school-specific line is under-powered).




****** Global regresions, not involving random effects:. 

regression /var= gcse lrt girl sch_2 sch_3 /dep=gcse /method=enter .
* (LRT is the most important, but gender and school type also matter)


* (Conventional view: Ths may well be an adequate description of the process, however we know that it is currently
*     ignoring an important aspect of the data). 



******** Random intercepts null models :.

mixed gcse 
          /fixed              | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept | subject(school) covtype(vc)   .
mixed gcse 
          /fixed            | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(school) covtype(vc)   .
* i.e. first with reml, them with mle. 



******** Random intercepts models with covariates :.

mixed gcse with  lrt girl sch_2 sch_3
          /fixed    lrt girl sch_2 sch_3          | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept | subject(school) covtype(vc)   .

* (in this example with reml - can you copy and paste and change method to 'ml' for mle estimator). 



************* A random coefficient example: .

mixed gcse with  lrt girl sch_2 sch_3
          /fixed    lrt girl sch_2 sch_3          | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept lrt | subject(school) covtype(un)   .

* (Conventional view: This is likely to be the most helpful of the models we've tried: it fits the 
*    data better than most other and uses only a few parameters to describe a complex but interesting empirical pattern). 



**********************************************************.
**********************************************************.

















*********************************************************************************************.
*********************************************************************************************.
***   Ex 1.2: Selected random intercepts and slopes models within the pupil popularity data .

*   (Here we do a similar exercise but using the student popularity data used in Hox et al).


dataset close all. 
get stata file = !path1a+"popular2.dta".
descriptives var=all.

** This is simulated (fictional) data designed to make a convenient illustration.

** The hierarchy invoves individual pupils in schools.

sort cases by class pupil.
list /variables=all /cases= from 1 to 100.


** To explore the hierarchical structure a bit: . 
descriptives var=class pupil.
* We've got 2000 cases in the data.
* ? How many different schools.
sort cases by class pupil.
compute first=1.
if (class=lag(class)) first=0.
fre var=first. /* Only one (arbitrary) case per class is highlighted */
* This shows there's 100 different classes.
* ? How many pupils per class.
sort cases by class pupil.
aggregate outfile=!path9+"m1.sav" /break=class /npupils=N(pupil).
match files file=* /table=!path9+"m1.sav" /by=class.
variable label npupils "Number of pupils per class".
graph /histogram=npupils.
* This shows the size of the classes in which each pupil is located; if you want just the size of class per class then:.
temp.
select if first=1.
fre var=npupils. 

** In summary:  There's 2000 pupils, nested in 100 classes, between 16 and 26 pupils per class . 


** The dependent and explanatory variables of interest are: .
descriptives var=popular sex extrav texp popteach  /statistics= mean min max stddev semean . 

** Regressions in SPSS not involving random effects .
regression var=popular sex extrav texp    /dependent=popular /method=enter . 
glm popular with sex extrav texp /print=parameter.
* (Two different ways to get to the same result).





** Random effects specifications: . 

* Intercept only model : .

mixed popular  
          /fixed             | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept | subject(class) covtype(vc)   .
* (the reml version).  

mixed popular  
          /fixed         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(class) covtype(vc)   .
* (and the MLE version - i.e. it matches the version in the Hox text). 


** Now repeat these models with explanatory covariates : .

compute ext_exp=extrav*texp.
descriptives var=popular sex extrav texp ext_exp . 

* Linear model:.
regression /var=popular sex extrav texp ext_exp /dep=popular /method=enter. 


* Random intercepts with covariates: .

mixed popular with   sex extrav texp ext_exp
          /fixed    sex extrav texp ext_exp         | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept | subject(class) covtype(vc)   .
* i.e. reml estimation. 


mixed popular with   sex extrav texp ext_exp
          /fixed    sex extrav texp ext_exp         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(class) covtype(vc)   .
* i.e.  with ml . 




** variant of the same model, now with centring of explanatory variables :.
compute Cext_exp=Cextrav*Ctexp.

mixed popular with   sex Cextrav Ctexp Cext_exp
          /fixed    sex Cextrav Ctexp Cext_exp         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(class) covtype(vc)   .


*** Random slope model - repeat the above estimation but also allowing for a random coefficient for pupil extraversion: .

mixed popular with   sex Cextrav Ctexp Cext_exp
          /fixed    sex Cextrav Ctexp Cext_exp         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept Cextrav | subject(class) covtype(un)   .

* (Note the need/preference for the unstructured covariance type constraint).

mixed popular with   sex Cextrav Ctexp Cext_exp
          /fixed    sex Cextrav Ctexp Cext_exp         | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept Cextrav | subject(class) covtype(un)   .



**************************************************************************************.
***********************************************************************************************.










*********************************************************************************************.
*********************************************************************************************.
***   Ex 1.3: Illustrating random effects models for linear and non-linear outcomes .




*********************************************************************************************.
*   a)    Binary outcomes comparison models for the Hox example data on pupil popularity .


* In this analysis, we'll dichotomise the outcome variable from the Hox student popularity 
*   data, then we'll run binary outcome models and compare them with linear models.

dataset close all. 
get stata file=!path1a+"popular2.dta".
descriptives var=all. 
fre var=class.  /* There's 2000 pupils, nested in 100 classes, between 16 and 26 pupils per class */
** The dependent and explanatory variables (centred versions of them) of initial interest are:.
compute  ext_exp=Cextrav*Ctexp.
descriptives var=popular sex Cextrav Ctexp ext_exp .  

** We'll dichotomise the outcome:.
graph /histogram= popular.
aggregate outfile=* mode=addvariables  /meanpop=mean(popular).
fre var=meanpop. 
compute pop2=(popular > meanpop).
fre var=popular meanpop pop2. 
graph /bar=count by pop2 by sex /title="Pupil popularity dichotomised at mean".


***************************************.
** Comparison 1: Single level regressions. 
**.

descriptives var= popular pop2 sex Cextrav Ctexp ext_exp. 

* Linear regression, linear outcome .
regression /var= popular sex Cextrav Ctexp ext_exp /dep=popular /method=enter . 
genlin popular with sex Cextrav Ctexp ext_exp 
    /model sex Cextrav Ctexp ext_exp  link=identity distribution=normal . /* Same as 'regression' above */
*(Just showing the model via two different coding routes for interest - there's no need ordinarily to use both).

* Linear regression, applied to a dichotomous outcome (i.e. the 'linear probability model').
regression /var= pop2 sex Cextrav Ctexp ext_exp /dep=pop2 /method=enter . 
genlin pop2 with sex Cextrav Ctexp ext_exp 
    /model sex Cextrav Ctexp ext_exp  link=identity distribution=normal . /* Same as 'regression' above */


* Logit model (i.e. GLM with logit link and binomial error). 
logistic regression var=pop2 with sex Cextrav Ctexp ext_exp /dep=pop2  /method=enter. 
genlin pop2 with sex Cextrav Ctexp ext_exp 
    /model sex Cextrav Ctexp ext_exp  link=logit distribution=binomial . /* Same as 'logistic regression' above */

* Probit model (i.e. GLM with probit link and binomial error). 
genlin pop2 with sex Cextrav Ctexp ext_exp 
    /model sex Cextrav Ctexp ext_exp  link=probit distribution=binomial . /* Different result - an alternative model */

* Comment: bottom line interpretation of all models is the same, but coefficients have different
*   values due to being on different scales and variances.
**.
**********************************************.


***************************************.
** Comparison 2: Random intercepts regressions. 
**.

*** Background: Linear outcomes 2-level regressions: .

** 2-level linear outcome model using 'mixed'.
mixed popular with  sex Cextrav Ctexp  ext_exp
          /fixed    sex Cextrav Ctexp  ext_exp        | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(class) covtype(vc)   .

** 2-level linear outcome model using 'genlinmixed'.
* (Note a bit of pre-analysis is needed in SPSS: it is necessary that the cluster identifier 
 *    seems to SPSS to have categorical level of measurement, which often doesn't happen automatically and may need adjustment as below).
descriptives var=class.  
delete variables class2 .
autorecode class /into=class2.
codebook class class2. 
genlinmixed /data_structure subjects=class2 /fields target=popular 
    /target_options distribution=normal link=identity 
   /fixed   effects=sex Cextrav Ctexp  ext_exp   use_intercept=true 
   /random use_intercept=true  subjects=class2  covariance_type=variance_components .

*(Note that 'genlinmixed' by default gives you a lot of output displays based on the basic model, including graphics 
*    - in my experience the defaults are of limited value and the normal focus is just on the tables of estimated parameters ).

*********************************************************************************************.
*********************************************************************************************.





*********************************************************************************************.
*   b)   A range of non-linear outcomes 2-level random effects models for the GCSE dataset .


dataset close all. 
get stata file=!path1a+"gcse.dta". 
* Stata format versions of the files from Rabe-Hesketh and Skrondal 2008 .
descriptives var=all. 
* This is a dataset of 4059 pupils nested in 65 schools .
sort cases by school pupil .
list /variables=all /cases= from 1 to 50.
* E.g. illustrates the data structure.

** To explore the hierarchical structure a bit: . 
descriptives var=school student.
* We've got 4059 cases in the data.
* ? How many different schools.
sort cases by school student.
compute first=1.
if (school=lag(school)) first=0.
fre var=first. /* Only one (arbitrary) case per cluster is highlighted */
* This shows there's 65 different schools.


** The dependent and explanatory variables of interest are: .
descriptives var=gcse lrt girl schgend . 
variable label gcse "GCSE score (Z score*10)".
variable label lrt "London Reading test score (Z score*10)".
variable label girl "Female".
variable label schgend "Type of school".
add value labels schgend  1 "Mixed gender" 2 "Boys only" 3 "Girls only".
* (Labels from Rabe-Hesketh and Skrondal, 2010: 141).
means tables = gcse lrt by schgend .


** For random effects models in SPSS, we will also need  a categorical format cluster identifier. 

codebook school.  
delete variables school2 .
autorecode school /into=school2.
codebook school school2. 



** Some manufactured non-linear outcomes measures:.

descriptives var=gcse. 

* Binary:. 
compute hi_gcse = (gcse >= 10). 
 
* Ordered, with 5 categories:. 
compute ord_gcse=gcse. 
recode ord_gcse (-100 thru -10=1) (-10 thru -5=2) (-5 thru 5=3) (5 thru 10=4) (10 thru 100=5). 
fre var=ord_gcse. 

* Multinomial: . 
compute gp_gcse = -9 . 
if ((gcse >= -100 & gcse <=  -20) | (gcse >= 20 & gcse <=  100))  gp_gcse=3.
if (gcse > -20 & gcse <=  0)   gp_gcse=1.
if (gcse > 0 & gcse <  20)   gp_gcse=2.
add value labels gp_gcse 1 "Lower" 2 "Higher" 3 "At the Extremes".
missing values gp_gcse (-9). 
fre var=gp_gcse. 

* Count-like. 
* (There isn't a natural count format measure in the data, but this recoding has a distribution that's typical of a count variable).
compute ct_gcse=gcse. 
recode ct_gcse (-100 thru -5=0) (-10 thru 5=1) (5 thru 10=2) (10 thru 15=3) (15 thru 20=4) (20 thru 25=5) (25 thru 100=6). 
fre var=ct_gcse. 

***************************************************.


fre var=schgend. 
compute sch_2=(schgend=2).
compute sch_3=(schgend=3).
fre var=schgend sch_2 sch_3.


descriptives var=gcse hi_gcse ord_gcse gp_gcse ct_gcse  lrt girl  sch_2 sch_3 /statistics=all. 
correlate var=gcse hi_gcse ord_gcse gp_gcse ct_gcse   lrt girl  sch_2 sch_3 . 
* Numerous empirical patterns apparent, some of them quite strong .

graph /bar= mean(hi_gcse) mean(lrt) by schgend. 
* Suggests strong school type effect (0/1 dummy variables have the convenient property that bar charts of their mean shows the proportion per category).



***************************.


**** Next, we'll run a plausible single level model, random intercepts and random slopes model for each functional form of the output. 

descriptives var=gcse hi_gcse ord_gcse gp_gcse ct_gcse   /statistics=all. 


***************************************.
*** Linear format outcome.

* Single level:. 
descriptives /var=gcse. 
regression /var= gcse lrt girl sch_2 sch_3 /dep=gcse /method=enter. 

* Random intercepts:. 
genlinmixed /data_structure subjects=school2 /fields target=gcse 
    /target_options distribution=normal link=identity 
   /fixed   effects=lrt girl sch_2 sch_3   use_intercept=true 
   /random use_intercept=true  subjects=school2  covariance_type=variance_components .

* Random slopes:. 
genlinmixed /data_structure subjects=school2 /fields target=gcse 
    /target_options distribution=normal link=identity 
   /fixed   effects=lrt girl sch_2 sch_3   use_intercept=true 
   /random effects=lrt  use_intercept=true  subjects=school2  covariance_type=unstructured .

* Equivalent random slopes with 'mixed'. 
mixed gcse with   lrt girl sch_2 sch_3 
          /fixed    lrt girl sch_2 sch_3          | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept lrt | subject(school2) covtype(un)   .




******* Binary outcome. 

* Single level:. 
fre var=hi_gcse. 
logistic regression var= hi_gcse with lrt girl sch_2 sch_3  /dep=hi_gcse /method=enter. 

* Random intercepts:. 
genlinmixed /data_structure subjects=school2 /fields target=hi_gcse 
    /target_options distribution=binomial link=logit 
   /fixed   effects=lrt girl sch_2 sch_3   use_intercept=true 
   /random use_intercept=true  subjects=school2  covariance_type=variance_components .

* Random slopes:. 
genlinmixed /data_structure subjects=school2 /fields target=hi_gcse 
    /target_options distribution=binomial link=logit 
   /fixed   effects=lrt girl sch_2 sch_3   use_intercept=true 
   /random effects=lrt  use_intercept=true  subjects=school2  covariance_type=unstructured .




*************** Ordinal outcome: . 

fre var=ord_gcse.

* Single level:. 
genlin ord_gcse (order=ascending) with  lrt girl sch_2 sch_3 
       /model lrt girl sch_2 sch_3         distribution=multinomial link=cumlogit  .

* Random intercepts:. 
genlinmixed /data_structure subjects=school2 
    /fields target=ord_gcse 
   /target_options reference=1 distribution=multinomial link=logit 
   /fixed   effects=   lrt girl sch_2 sch_3   use_intercept=true 
   /random use_intercept=true  subjects=school2  covariance_type=variance_components 
    /build_options target_category_order=ascending inputs_category_order=ascending  max_iterations=100
      /emmeans_options scale=original padjust=lsd. 


** Random coefficients:.
genlinmixed /data_structure subjects=school2 
    /fields target=ord_gcse 
   /target_options reference=1 distribution=multinomial link=logit 
   /fixed   effects=   lrt girl sch_2 sch_3   use_intercept=true 
   /random effects=lrt  use_intercept=true  subjects=school2  covariance_type=unstructured 
    /build_options target_category_order=ascending inputs_category_order=ascending  max_iterations=20
      /emmeans_options scale=original padjust=lsd. 
* (tip: use 'file -> 'stop processor' if you run out of time). 




******** Multinomial outcome . 

fre var=gp_gcse. 

* Single level:. 
genlinmixed /fields target=gp_gcse 
         /target_options reference=1 distribution=multinomial link=logit 
      /fixed effects=  lrt girl sch_2 sch_3      use_intercept=true .

* Random intercepts:. 
genlinmixed /data_structure subjects=school2 
    /fields target=gp_gcse 
   /target_options reference=1 distribution=multinomial link=logit 
   /fixed   effects=   lrt girl sch_2 sch_3   use_intercept=true 
   /random use_intercept=true  subjects=school2  covariance_type=variance_components .


* Random  slopes:. 
genlinmixed /data_structure subjects=school2 
    /fields target=gp_gcse 
   /target_options reference=1 distribution=multinomial link=logit 
   /fixed   effects=   lrt girl sch_2 sch_3   use_intercept=true 
   /random effects=lrt use_intercept=true  subjects=school2  covariance_type=unstructured 
     /build_options max_iterations=20 . 
* (slow!). 



** Poisson model for count outcomes:. 

fre var=ct_gcse. 
codebook ct_gcse.
* For SPSS, the outcome should be thought to be continuous, so this needs to be revised. 
compute ct_gcse2= ct_gcse.
variable level ct_gcse2 (scale). 
codebook ct_gcse2 ct_gcse .


** Single level poisson . 
genlin ct_gcse  with  lrt girl sch_2 sch_3 
    /model  lrt girl sch_2 sch_3 distribution=poisson link=log  . 

** Random intercepts poisson. 
genlinmixed /data_structure subjects=school2 
    /fields target=ct_gcse2 
   /target_options distribution=poisson link=log 
   /fixed   effects=  lrt girl sch_2 sch_3   use_intercept=true 
   /random use_intercept=true  subjects=school2  covariance_type=variance_components .
* . 

** Random coefficients poisson. 
genlinmixed /data_structure subjects=school2 
    /fields target=ct_gcse2 
   /target_options distribution=poisson link=log 
   /fixed   effects= lrt girl sch_2 sch_3      use_intercept=true 
   /random effects=lrt  use_intercept=true  subjects=school2  covariance_type=unstructured 
        /build_options max_iterations=20 .
* . 



************************************************************************.








**********************************************************.
**********************************************************.
***   Ex 1.4: Illustrating random effects models with more than 2 levels  .




**********************************************************.
* i)   Exploring and analysing the example 3-level nested dataset from Hox (2010).

dataset close all. 
get stata file=!path1a+"nurses.dta".
descriptives var=all.

** This is a dataset on self-reported stress experienced by nurses:.

graph /histogram=stress.

** There is natural hierarchy clustering the nurses into wards, and within wards into hospitals

descriptives var=nurse wardid hospital.
sort cases by wardid.
aggregate outfile=!path9+"m1.sav" /break=wardid /w_size=N(nurse) /w_mean=mean(stress).
match files file=* /table=!path9+"m1.sav" /by=wardid. 
sort cases by hospital.
aggregate outfile=!path9+"m2.sav" /break=hospital /h_size=N(nurse) /h_mean=mean(stress).
match files file=* /table=!path9+"m2.sav" /by=hospital. 
descriptives var=w_size h_size.

** Between 9 and 13 nurses per ward; between 36 and 52 nurses per hospital. 

sort cases by hospital wardid nurse. 
compute h_first=1.
if (lag(hospital) = hospital) h_first=0.
compute w_first=1.
if (lag(wardid)=wardid) & (lag(hospital) = hospital) w_first=0.
fre var=h_first w_first.
* 25 different hospitals and 100 different wards are represented here. 

graph /bar=mean(h_size) mean(h_mean) by hospital /title="Size and mean stress by hospital". 

temp.
select if (w_first=1).
graph /scatterplot(overlay)= w_size h_size with w_mean 
    /title="Ward size and hospital size by mean ward stress".

**********************************************.

*** Here's two two level null models : .

mixed stress      /fixed      | sstype(3)         /method=reml        /print=corb solution  r 
                   /random=intercept | subject(wardid) covtype(vc)   . 
* ICC in two level model at the ward level is 0.658 / (0.301 + 0.658) = 0.686.

mixed stress      /fixed      | sstype(3)         /method=reml        /print=corb solution  r 
                   /random=intercept | subject(hospital) covtype(vc)   . 
* ICC in two level model at the ward level is 0.285 / (0.285 + 0.686) = 0.294.


**** Now a three level null model :.

mixed stress      /fixed      | sstype(3)         /method=reml        /print=corb solution  r 
                   /random=intercept | subject(hospital) covtype(vc) 
             /random=intercept | subject(wardid) covtype(vc)   . 


** ICC at level 3 = 0.174 / (0.174 + 0.489 + 0.301) = 0.174 / 0.964 =0.180  .
** ICC at level 2 = 0.489 / 0.964 = 0.501 .

** This is suggesting clustering at both levels exists, to some degree independently.


**** Three level model with explanatory covariates:

mixed stress  with expcon age gender experien wardtype hospsize 
      /fixed  expcon age gender experien wardtype hospsize     | sstype(3)  
            /method=ml        /print=corb solution  r 
                   /random=intercept | subject(hospital) covtype(vc) 
             /random=intercept | subject(wardid) covtype(vc)   . 

*** Three level model with random coefficients at the hospital level for the effect of the experimental group.

mixed stress  with expcon age gender experien wardtype hospsize 
      /fixed  expcon age gender experien wardtype hospsize     | sstype(3)  
            /method=ml        /print=corb solution  r 
                   /random=intercept  expcon | subject(hospital) covtype(vc) 
             /random=intercept | subject(wardid) covtype(vc)   . 

* In the above we've put the 'variance components' constraints on the random coefficients, though it is more 
*  conventional to keep the covariance type 'unstructured'. 


*** Three level model with random coefficients at ward level, and a cross-level interaction:.

compute experhs=expcon*hospsize.
descriptives var=experhs.

mixed stress  with expcon age gender experien wardtype hospsize experhs 
      /fixed  expcon age gender experien wardtype hospsize  experhs   | sstype(3)  
            /method=ml        /print=corb solution  r 
                   /random=intercept expcon | subject(hospital) covtype(vc) 
             /random=intercept  | subject(wardid) covtype(vc)   . 


**********************************************.




**********************************************************.
* ii)   Exploring and analysing the bhps ghq dataset in 3-levels.
 *********************************************************.
**********************************************************.

*** GHQ dataset with additional binary measures: .

dataset close all. 
get stata file=!path1a+"bhps_ghq_extract1.dta".


** We're looking at scores for GHQ.

graph /histogram=ghq.

** The usual centring/recoding / deletion of missing data:.
descriptives var=fem cohab emp_10hrs  gdn   age spghq2 .
* Generate centred variables:.
compute c_age=age - 46. 
compute c_age2=age**2 - (46*46).
compute c_spghq2=spghq2 - 6.
descriptives var=fem c_age c_age2 cohab emp_10hrs  gdn   age c_spghq2 .
compute miss1=missing(ghq) + missing(fem) + missing(c_age) + missing(cohab) + missing(emp_10hrs) + missing(c_spghq2) 
       + missing(gdn) .
fre var=miss1. 
select if (miss1=0).


** There is a potential 3-level clustering: individuals in households in PSUs .
* (BHPS is a bit more complicated because PSU means original sampling group/area, it doesn't 
*   necessarily correspond to current area. 

descriptives var=pid ohid opsu .
sort cases by ohid.
aggregate outfile=!path9+"m1.sav" /break=ohid /h_size=N(pid) /h_mean=mean(ghq).
match files file=* /table=!path9+"m1.sav" /by=ohid. 
sort cases by opsu.
aggregate outfile=!path9+"m2.sav" /break=opsu /p_size=N(pid) /p_mean=mean(ghq).
match files file=* /table=!path9+"m2.sav" /by=opsu. 
descriptives var=h_size p_size.
** Between 1 and 7 people per household; between 4 and 2231 people per psu. 

* Note however there is one unusually large PSU in the data (it is for Northern Ireland where a different
*   sampling approach was used). For convenience we drop it here:.
select if (opsu~=-8).
descriptives var=h_size p_size.
** Between 1 and 7 people per household; between 4 and 64 people per psu. 

sort cases by opsu ohid pid. 
compute p_first=1.
if (lag(opsu) = opsu) p_first=0.
compute h_first=1.
if (lag(ohid)=ohid) & (lag(opsu) = opsu) h_first=0.
fre var=p_first h_first.
* 399 different psus; 6618 different households are represented here. 

temp.
select if (h_first=1).
graph /scatterplot(overlay)= h_size with h_mean ghq
    /title="Household size and mean household GHQ and indivual GHQ".



**********************************************.

*** Here's two two level null models : .


mixed ghq      /fixed      | sstype(3)         /method=ml        /print=corb solution  r 
                   /random=intercept | subject(ohid) covtype(vc)   . 
* ICC in two level model at the ward level is 4.42 / (4.42 + 25.77) = 0.146.

mixed ghq     /fixed      | sstype(3)         /method=ml        /print=corb solution  r 
                   /random=intercept | subject(opsu) covtype(vc)   . 
* ICC in two level model at the ward level is 0.458 / (0.458 + 29.70) = 0.015.




**** Now a three level null model :.

mixed ghq     /fixed      | sstype(3)         /method=ml        /print=corb solution  r 
                   /random=intercept | subject(opsu) covtype(vc) 
             /random=intercept | subject(ohid) covtype(vc)   . 
* (Very slow estimation) .
* Too little memory for this model at Essex lab pc .

** ICC at level 3 =   .
** ICC at level 2 =  .

** This is suggesting clustering at both levels exists, to some degree independently.


**** Three level model with explanatory covariates:.

 mixed ghq  with  fem c_age c_age2 cohab emp_10hrs c_spghq2  gdn  
      /fixed   fem c_age c_age2 cohab emp_10hrs c_spghq2  gdn     | sstype(3)  
            /method=ml        /print=corb solution  r 
              /random=intercept | subject(opsu) covtype(vc)   
                   /random=intercept | subject(ohid) covtype(vc)    .
 * Press 'file' and 'stop processor' when you're ready!. 
* (Estimation problem: insufficient memory).

*** Three level model with random coefficients at the ward level for the effect of the experimental group.

*********************************************************************************.





*********************************************************************************.
*** EOF. 





