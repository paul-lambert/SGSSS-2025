* Encoding: UTF-8.
***************************************.
** Title and contents

**** SGSSS Summer School 2025

** Short course: Multilevel Models for Applied Social Research

** (Materials by Paul Lambert and Kate O'Hara)

***  LAB 2 - SPSS EXERCISES - 'EXPLORING MULTILVEL DATA AND THE RELATIONSHIP BETWEEN RANDOM EFFECTS AND OTHER MODELLING STRATEGIES' 

***   Linked code, data and metadata: at https://github.com/paul-lambert/SGSSS-2025/

**    Version: 16/Jun/2025, written for SPSS v29.0.1.

***************************************.



******************************.
**** Contents listing.

***   Title and contents .
***   Paths and settings.
*** Ex 2.1: Reflections on more and less optimal scenarios for using random effects models. 
*** Ex 2.2: Comparison of ways of modelling long-format panel data with and without random effects. 
*** Ex 2.3: Reflecting on the relationship between higher level fixed effects and random effects .
*** Ex 2.4: Looking at a cross-classified model example .

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












******************************************************************************************.




**********************************************.
**********************************************.

*** Ex 2.1: Reflections on more and less optimal scenarios for using random effects models. 
**********************************************************.


** i) Telling  a story about higher level variation:.

dataset close all. 
get file=!path1a+"bhps_ttwt1.sav". 
graph /histogram(normal)=qjbttwt.
descriptives var= qpsu qjbttwt fem qage age2 educ3_1 educ3_3 ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_8 ttype_9.

compute miss1=missing(qjbttwt) + missing(fem) + missing(qage) + missing(age2) + missing(educ3_1) + missing(educ3_3) +
       missing(ttype_1) + missing(ttype_2) + missing(ttype_3) + missing(ttype_4) + 
      missing(ttype_5) + missing(ttype_6) + missing(ttype_7) + missing(ttype_9).
fre var=miss1.
select if (miss1=0).
compute c_age=qage - 40.
compute c_age2=c_age**2.
descriptives var= qjbttwt fem c_age c_age2 educ3_1 educ3_3 ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_8 ttype_9.

* The hierarchy is of individuals within psus: .
sort cases by qpsu pid. 
compute first=1.
if  (lag(qpsu)=qpsu) first=0. 
fre var=first. 
* so 399 psus from 5005 cases with all data. 

* Linear model.
regression /var= qjbttwt fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9
      /dep=qjbttwt /method=enter. 

* Random intercepts model (ml): .
mixed qjbttwt with fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9
          /fixed     fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(qpsu) covtype(vc)   .

* An unconstrained random coefficients model: .

mixed qjbttwt with fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9
          /fixed     fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept educ3_1 educ3_3 | subject(qpsu) covtype(un)   .

* In this example we try constraining the variances:  we don't allow  for covariances between the dummy terms, 
*    but we do allow that the slope variance for different dummy terms could be different.

mixed qjbttwt with fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9
          /fixed     fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept educ3_1 educ3_3 | subject(qpsu) covtype(diag)   .

* Whereas here we again don't have covariances between the dummies, but now in addition we force the variance for each dummy 
*      to also be equal. 

mixed qjbttwt with fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9
          /fixed     fem c_age c_age2 educ3_1 educ3_3 
                ttype_1 ttype_2 ttype_3 ttype_4 ttype_5 ttype_6 ttype_7 ttype_9         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept educ3_1 educ3_3 | subject(qpsu) covtype(id)   .

* => Argument: From all of these, the paramaters give useful stories about empirical patterns involving the level 2 structure. 
*    -> a compelling case for exploring these with random effects models. 

**********************************************************.


**********************************************.
** ii) Example of using random effects to have suitable standard errors 
**     for higher level effects. 


** Data preparation .

dataset close all. 
get stata file=!path1a+"bhps_soc_extract1.dta".
descriptives var=ajbsoc lfimnl ajbhrs ahlstat fem age age2 cohab separ widow degdip vocq noqual. 
* Generate some job level measures: .
descriptives var= mcamsis ajbonus .
aggregate outfile=!path9+"m1.sav" /break=ajbsoc /s_fem=mean(fem) /s_mcam=mean(mcamsis) /s_bonus=mean(ajbonus) /s_lfimnl=mean(lfimnl) . 
sort cases by ajbsoc.
match files file=* /table=!path9+"m1.sav" / by=ajbsoc.
variable label s_fem "Job level proportion female".
variable label s_mcam "Job level CAMSIS score".
variable label s_bonus "Job level bonus rate".
descriptives var=ajbsoc s_fem s_mcam s_bonus .

missing values ajbonus s_bonus (lo thru -1).
descriptives var=ajbsoc lfimnl ajbhrs ahlstat fem age age2 cohab separ widow degdip vocq noqual ajbonus mcamsis s_fem s_mcam s_bonus. 
compute miss1=missing(lfimnl) + missing(ajbhrs) +missing(ahlstat) +missing(fem) + missing(age) + missing(cohab) + missing(degdip) 
     + missing(mcamsis) + missing(s_mcam) + missing(ajbonus) + missing(s_bonus).
fre var=miss1.
select if (miss1=0).
descriptives var=ajbsoc lfimnl ajbhrs ahlstat fem age age2 cohab separ widow degdip vocq noqual ajbonus mcamsis s_fem s_mcam s_bonus. 

* Imagine we were primarily interested in the effects of explanatory
*  variables at the higher level.

** Example 1: demographic influences at higher and lower level: .

graph /histogram(normal)=s_fem.

regression /var= lfimnl age age2 fem s_fem
      /dep=lfimnl /method=enter. 

mixed lfimnl with age age2 fem s_fem 
          /fixed    age age2 fem s_fem        | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept | subject(ajbsoc) covtype(vc)   .

** Interpretation: The random effects estimates give a different result, their se.s should be improved but their 
*   actual values may be biassed due to correlation between the random effects and the x variables. 

*** A second example - here looking at the effect of job characteristics:. 

graph /scatterplot=s_bonus with s_mcam .

regression /var= lfimnl s_bonus s_mcam
        /dep=lfimnl /method=enter. 

mixed lfimnl with s_bonus s_mcam 
          /fixed    s_bonus s_mcam        | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept | subject(ajbsoc) covtype(vc)   .

* The models do give different results - there's some uncertainly over best procedure. 

********************************************.

** What about looking at an outcome that's also fixed at the higher level?.

regression /var= s_lfimnl s_bonus s_mcam
        /dep=s_lfimnl /method=enter. 
* These are likely to be misleading due to clustering; robust standard errors would be desirable (?not available in SPSS). 

mixed s_lfimnl with s_bonus s_mcam 
          /fixed    s_bonus s_mcam        | sstype(3)         /method=reml 
         /print=corb solution  r 
          /random=intercept | subject(ajbsoc) covtype(vc)   .
* Can't estimate with no lowest level variance. 
* More realistic standard errors might be at the aggregate level:.

sort cases by ajbsoc pid. 
compute first=1.
if (lag(ajbsoc)=ajbsoc) first=0 .
fre var=first. 

temp.
select if (first=1).
regression /var= s_lfimnl s_bonus s_mcam
        /dep=s_lfimnl /method=enter. 
* These terms might be more realistic esimates, but they will be different to the full model as, effectively, they 
*   are differently weighted. 



*******************************************************************************.

** iii) Using fixed effects and thinking about the difference between 'between' and 'within' .

get stata file=!path1a+"bhps_ghq_extract1.dta".
descriptives var=all.
descriptives var= ghq fem age age2 cohab emp_10hrs .

* Linear model.
regression /var= ghq fem age age2 cohab emp_10hrs  
        /dep=ghq /method=enter. 

* Random effects model: .
mixed ghq with fem age age2 cohab emp_10hrs 
          /fixed    fem age age2 cohab emp_10hrs         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(ohid) covtype(vc)   .

* Fixed effects model: Not programmed into SPSS, but fixed part coefficients can be estimated by 
*   including group means as explanatory variables :.
aggregate outfile=!path9+"m2.sav" /break=ohid /h_fem=mean(fem) 
    /h_age=mean(age) /h_age2=mean(age2) /h_cohab=mean(cohab) /h_emp_10hrs=mean(emp_10hrs).
sort cases by ohid. 
match files file=* /table=!path9+"m2.sav" /by=ohid.
descriptives var=ghq fem age age2 cohab emp_10hrs h_fem h_age h_age2 h_cohab h_emp_10hrs. 

* Random effects model mimicing the fixed effects framework: . 

mixed ghq with fem age age2 cohab emp_10hrs h_fem h_age h_age2 h_cohab h_emp_10hrs
          /fixed    fem age age2 cohab emp_10hrs  h_fem h_age h_age2 h_cohab h_emp_10hrs       | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(ohid) covtype(vc)   .

* The fixed part coefficients for the individual level variables mimic those which we'd get from running a fixed effects model - 
*   they are based on the 'within' story, i.e. what drives differences in ghq from person to person within the household.  

* Note that the fe is fundamentally a different model: it has different
*  fixed part coefs, namely no fixed-in-group vars (e.g. gdn) and  
*  extra parameter (effectively) for each group - in fe model only, 
*  the fixed part parameters are wholly about within-group variations.

**********************************************************.




**********************************************.

** iv)  What about when k is a bit too small for comfort...?.

*** Reminder: Popular perspective that inferences for a k less than about 25 (30 for binary outcomes)
*                 are unreliable (e.g. Bryan and Jenkins 2016).  
*
*** However, Elff et al. argue that even with lower k, inferences should still be accurate 
*             (in the sense of suitably large s.e.'s) so long as we remember a couple of key qualifiers,
*               namely, to use reml, and to use a suitable t-statistic in testing inferences. 
*             (Elf et al. also argue that it has previously been a mistaken conclusion that 
*                Bayesianism offers an intrinsic solution to low-k designs - to Elf et al., 
*                Frequentist approaches are just as reliable, so long as conducted correctly).


** Example on the ESS  .

dataset close all. 
get stata file=!path1a+"ess5_extract_occs_1_model.dta".
fre var=country.

* Y variable.
fre var=tvtot. 

* X variables:.
compute age2=agea**2.
descriptives var= fem agea age2  eduyrs. 

* Already we have quite a small sample of countries - but what if we make it even smaller: .
fre var=country.	 

* Only countries who qualified for the 2021 world cup.
compute select1 = ( country=1 | country=3 | country=6 | country=7 | country=9 | country=11 | country=12  
     | country=14 | country=21 | country=22 | country=24).

temp. 
select if (select1=1).
fre var=country. 
	 
* Only countries that made it to the quarter finals: .

compute select2= (country=1  | country=11 | country=12  | country=14  | country=23 | country=24).

temp. 
select if (select2=1).
fre var=country. 


* Exercise: ...If we think of the results for all countries as 'right', then the parameters based on the 
*     selected samples won't be wrong if their relevant parameters and stanard errors overlap...  

* So, Elf et al. argue that random effects distributinoal estimates, even with quite a small sample sizes should be
*   ok, so long as we do two things...


** i) We should take care to use reml rather than ml: 
*    (Elff et al: mle higher level estimates will be downward biassed with small k, but reml remidies this).

* Random intercepts using reml: .

* For full sample:. 
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=reml 
         /print= descriptives corb solution  r            /random=intercept | subject(country) covtype(vc)   .


* For first sample:. 
temp. 
select if (select1=1).
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=reml 
         /print= descriptives corb solution  r            /random=intercept | subject(country) covtype(vc)   .

* For smallest sample:. 
temp. 
select if (select2=1).
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=reml 
         /print= descriptives corb solution  r            /random=intercept | subject(country) covtype(vc)   .

* Observe: With reml, the model parateters generally, and especially the 
*    random effects parameters (e.g. random intercepts variance), seems to be pretty well 
*    estimated even with small k, in that the standard errors encompass those for the large k values .


*****.
* Random intercepts using mle: .


* For full sample:. 
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=ml 
         /print= descriptives corb solution  r            /random=intercept | subject(country) covtype(vc)   .


* For first sample:. 
temp. 
select if (select1=1).
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=ml 
         /print= descriptives corb solution  r            /random=intercept | subject(country) covtype(vc)   .

* For smallest sample:. 
temp. 
select if (select2=1).
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=ml 
         /print= descriptives corb solution  r            /random=intercept | subject(country) covtype(vc)   .


* Observe: Even with mle, the estimates overlap in their se's, though 
*    there does seem to be a trend towards more sytematic differences between 
*     the parameters with more and with fewer k .

********************************.

* Random slopes using reml .

* For full sample:. 
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=reml 
         /print= descriptives corb solution  r            /random=intercept eduyrs | subject(country) covtype(vc)   .

* For first sample:. 
temp. 
select if (select1=1).
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=reml 
         /print= descriptives corb solution  r            /random=intercept eduyrs | subject(country) covtype(vc)   .

* For smallest sample:. 
temp. 
select if (select2=1).
mixed lrscale with fem agea age2 eduyrs
          /fixed   fem agea age2 eduyrs    | sstype(3)         /method=reml 
         /print= descriptives corb solution  r            /random=intercept eduyrs | subject(country) covtype(vc)   .

** =>  Even in a random slopes context, small k estimates with reml still seem to be working 
*      ok - they correctly suggest more uncertainty, but the estimates seem to be getting it 
*      about right within the bounds of that uncertainty .

****.

** ii) Elff et al stress that we should also take care to use a suitable test statistic 
*       for evaluating significance of random part estimates.

*  [No illustrative example - see Elf et al., part 2]. 

* (My own explorations using other software where different test statistics are available suggest that the 
*    impact of this consideration is usually very small, and might only really emerge when the random part 
*    structure is very complicated [more work desriable]). 

*************************************************.









******************************************************************************************.







*******************************************************.
*** Ex 2.2: Comparison of ways of modelling long-format panel data with and without random effects. 

dataset close all. 
get stata file=!path1a+"bh1to15_long.dta".
descriptives var=all.
fre var=year. 
* (This is a 'long format' panel dataset: multiple records per person).


** Analytical variable prep:. 
compute ghq=zhlghq1.
missing values ghq  (lo thru -1).

compute lninc=-999.
if (zfimn >= 100) lninc = ln(zfimn).
compute fem=(zsex=2).
compute age=zage.
missing values age (lo thru 15) .
fre var=zqfedhi .
compute hied=-999.
if (zqfedhi >= 1) hied=(zqfedhi ge 1 & zqfedhi le 4).
compute noed=-999.
if (zqfedhi >= 1) noed=(zqfedhi >=12).

compute convot=-999.
if (zvote ge 1) convot=(zvote=1).
compute labvot=-999.
if (zvote ge 1) labvot=(zvote=2).
missing values lninc hied noed convot labvot (-999).

descriptives var= ghq lninc fem age hied noed convot labvot .
correlate variables= ghq lninc fem age hied noed convot labvot  .


****** (a) Metric regression illustration: 

** Linear regression on ghq : not appropriate because records aren't independent.

regression /dependent=ghq  
     /method=enter lninc fem age hied noed convot labvot .


****.


** 1) Random effects panel on ghq - just controls for clustering .

mixed ghq with lninc fem age hied noed convot labvot 
       /fixed=lninc fem age hied noed convot labvot   | sstype(3)
         /method=reml 
         /print=corb solution  r
          /random=intercept | subject(pid) covtype(ID)   .

**.


** Some common alternative panel models:. 

** 2) Retrieving the fixed effects model coefficients. 

** The fixed effects model can be obtained in many different ways, but simplest is to use group level means of all variables as predictors, 
*    meaning that the remaining values are the 'within' effects of the predictors: .

sort cases by pid.
aggregate outfile=!path9+"m1.sav" /break=pid /mghq=mean(ghq)
   /mlninc=mean(lninc) /mfem=mean(fem) /mage=mean(age)  /mhied=mean(hied) /mnoed=mean(noed) /mconvot=mean(convot) /mlabvot=mean(labvot) .
match files file=* /table=!path9+"m1.sav" /by=pid.
descriptives /var=mghq mlninc mfem mage mhied mnoed mconvot mlabvot  .
* i.e. we've distributed within-person mean values to each record for these variables. 


* The standard fixed effects model is obtained by linear regression with these means:. 
regression var= ghq lninc fem age hied noed convot labvot    mlninc mfem mage mhied mnoed mconvot mlabvot  
   /dependent=ghq /method=enter . 


*.
* Additionally, the 'hybrid' fixed effects is a model with random effects as well as the fixed effects terms. 
mixed ghq with lninc fem age hied noed convot labvot    mlninc mfem mage mhied mnoed mconvot mlabvot 
       /fixed=lninc fem age hied noed convot labvot  mlninc mfem mage mhied mnoed mconvot mlabvot  | sstype(3)
         /method=reml 
         /print=corb solution  r
          /random=intercept | subject(pid) covtype(ID)   .



** 3) Retrieving the between effects model coefficients. 


* The between effects model is just the model for the group means, where n is the number of groups. 

sort cases by pid wave.
compute first=1.
if (lag(pid)=pid) first=0.
fre var=first.

temp. 
select if (first=1). 
regression var= mghq   mlninc mfem mage mhied mnoed mconvot mlabvot  
   /dependent=mghq /method=enter . 

* (i.e. this is the same as we'd get from "xtreg ghq lninc fem age hied noed convot labvot, i(pid) be"  in Stata).

*********************************************.

*** 4) Growth curve models

*We can do 'growth curve' models by including time or ageing in the model as fixed part 
*     predictors plus potentially as random coefficients: .

compute time=year - 1991.
fre var=time. 

mixed ghq with lninc fem age hied noed convot labvot time 
       /fixed= lninc fem age hied noed convot labvot time  | sstype(3)
         /method=reml 
         /print=corb solution  r
          /random=intercept | subject(pid) covtype(ID)   .
* log-like = 861085 . 
mixed ghq with lninc fem age hied noed convot labvot time 
       /fixed= lninc fem age hied noed convot labvot  time | sstype(3)
         /method=reml 
         /print=corb solution  r
          /random=intercept time | subject(pid) covtype(ID)   .
* log-like=871889 (? - check n). 

compute yob=(year - age).
sort cases by pid year.
compute nrec=0.
if (lag(pid)=pid) nrec=lag(nrec)+1.
fre var=nrec. 
* (sequential order of panel records).

temp.
select if (yob >= 1960 & yob <= 1965).
mixed ghq with lagghq lninc age fem hied noed convot labvot nrec   
       /fixed=lagghq lninc fem age hied noed convot labvot nrec   | sstype(3)
         /method=reml 
         /print=corb solution  r
          /random=intercept nrec | subject(pid) covtype(un)   .

* By focussing upon a particular age cohort, this model is substantively more convincing than several of the others. 
* In empirical terms it doesn't add much though - here  the growth curve variance is insignificant, so that tell's us there's no evidence  
*  variation in the way time effects the outcome from individual to individual . 


* Note: Not shown here, but SPSS also has lower level error term design structures that can also be used to 
*   model autocorrelated ordered dependence in the errors within clusters  (i.e. suited to longitudinal designs when there is a natural 
*     ordering to the cases, i.e. the temporal order). See SPSS documentation for options. 
*.


** 5) Two way error components model:. 


** This model is essentially the cross-classified model for clustering into both time points, and people. 

mixed ghq with lninc fem age hied noed convot labvot   /fixed  lninc fem age hied noed convot labvot 
        | sstype(3)       /method=ml        /print=corb solution  r 
          /random=intercept | subject(time) covtype(vc) 
             /random=intercept | subject(pid) covtype(vc)   . 

** slow to estimate/insufficent memory.
*. 


** 6) Random effects panel model plus lag effects. 

** Panel on ghq with lag effects .
sort cases by pid wave .
compute lagghq=-999.
if (pid=lag(pid)) lagghq = lag(ghq).
missing values lagghq (-999).

descriptives var= ghq lagghq lninc fem age hied noed convot labvot .
correlate variables= ghq lagghq lninc fem age hied noed convot labvot  .

regression /dependent=ghq  
     /method=enter lagghq lninc fem age hied noed convot labvot .

mixed ghq with lagghq lninc fem age hied noed convot labvot 
       /fixed=lagghq lninc fem age hied noed convot labvot   | sstype(3)
         /method=reml 
         /print=corb solution  r
          /random=intercept | subject(pid) covtype(ID)   .

**.
 

** This is about as far as SPSS can go with panel models  -  for instance it doesn't support the many 
*    alternative panel data estimators that are commonly used in economics . 

*************************************************************************************.











**********************************************.
**********************************************.

*   Ex 2.3)  Reflecting on the relationship between higher level fixed effects and random effects .



** The argument here is that fixed and random effects are consuming the same patterns of . 
*    variance - often the attraction of a random effects model is lessened by having .
*    good higher level predictors:.

***Eg for the GHQ dataset:  .

dataset close all. 
get stata file=!path1a+"bhps_ghq_extract1.dta".
descriptives var=ghq fem age age2 cohab degdip vocq noqual emp_10hrs spghq2  ohhsize gdn .

compute miss1=missing(ghq) + missing(fem) + missing(age) + missing(age2) + missing(cohab) 
     + missing(degdip) + missing(vocq) + missing(noqual) + missing(emp_10hrs) + missing(spghq2) + missing(ohhsize) + missing(gdn) . 
fre var=miss1. 
select if (miss1=0). 

*****.

mixed ghq with fem age age2 cohab degdip vocq noqual emp_10hrs
          /fixed    fem age age2 cohab degdip vocq noqual emp_10hrs 
                     | sstype(3)         /method=ml 
         /print=corb solution  r            /random=intercept | subject(ohid) covtype(vc)   .
* Only has individual level predictors, ICC= 0.16 .

mixed ghq with fem age age2 cohab degdip vocq noqual emp_10hrs ohhsize gdn
          /fixed    fem age age2 cohab degdip vocq noqual emp_10hrs  ohhsize gdn
                     | sstype(3)         /method=ml 
         /print=corb solution  r            /random=intercept | subject(ohid) covtype(vc)   .
* Some household level predictors, ICC=0.16.

mixed ghq with fem age age2 cohab degdip vocq noqual emp_10hrs spghq2 ohhsize gdn
          /fixed    fem age age2 cohab degdip vocq noqual emp_10hrs spghq2 ohhsize gdn
                     | sstype(3)         /method=ml 
         /print=corb solution  r            /random=intercept | subject(ohid) covtype(vc)   .
* Other person level predictors, ICC =0.08 . 



*********************************.

** Eg for the occupations dataset (generated in the Stata exercises): . 

dataset close all. 
get stata file=!path1a+"bhps_soc_extract1.dta".

descriptives var= lfimnl fem age age2 degdip vocq noqual cohab mcamsis ajbhrs .
compute miss1=missing(lfimnl) + missing(fem) + missing(age) + missing(age2) + missing(degdip) + missing(vocq) + missing(noqual) 
     + missing(cohab) + missing(mcamsis) + missing(ajbhrs).
fre var=miss1.
select if (miss1=0).

mixed lfimnl 
          /fixed         | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(ajbsoc) covtype(vc)   .
* ICC = 0.44, the max job level clustering of wages we might measure. 


mixed lfimnl with fem age age2 degdip vocq noqual cohab
          /fixed fem age age2 degdip vocq noqual cohab        | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(ajbsoc) covtype(vc)   .

* ICC=0.32 - these are individual level characteristics but they will have a clustering .
*   by job type which means their direct effects account for much of the variance, hence icc decline.

mixed lfimnl with fem age age2 degdip vocq noqual cohab ajbhrs
          /fixed fem age age2 degdip vocq noqual cohab   ajbhrs      | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(ajbsoc) covtype(vc)   .

* Ditto for job hours - a further slight reduction in ICC.

mixed lfimnl with fem age age2 degdip vocq noqual cohab ajbhrs mcamsis 
          /fixed fem age age2 degdip vocq noqual cohab   ajbhrs mcamsis     | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(ajbsoc) covtype(vc)   .

* ICC=0.22 - now after controlling for job-level explanatory variables, the ICC declines further . 
* Comment: the question here is whether it is enough to control for job level fixed variables....

** Note: on these examples, the explanatory variables themselves have a job level ICC, although
*    not in general by design, e.g.  

mixed ajbhrs     /fixed      | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(ajbsoc) covtype(vc)   .
** This job level icc means that the variables fixed and random effects are related to each other. 


**********************************************************.
**********************************************************.








*********************************************************************.
** Ex 2.4: Looking at a  cross-classified model example .


dataset close all. 
get stata file=!path1a+"pupcross.dta".
descriptives var=all.

graph /histogram(normal)=achiev. 

fre var= pschool sschool. 
graph scatterplot=pschool with sschool. 
* The point here is the pupils are nested both by their primary school and secondary 
*    school, however, the nesting is not at all neatly hierarchical, rather it is 'cross-classified'.  


* Two two-level null models:.

mixed achiev 
          /fixed      | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(pschool) covtype(vc)   .

mixed achiev 
          /fixed      | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(sschool) covtype(vc)   .


** Run as a three level model automatically recognising the cross-classified structure:.

mixed achiev 
          /fixed      | sstype(3)         /method=ml 
         /print=corb solution  r 
          /random=intercept | subject(sschool) covtype(vc)  
          /random=intercept | subject(pschool) covtype(vc)  .

* cf. Hox, p176 .

** -> SPSS automatically models cross-classifications correctly (other software needs specific adjustments to code to account for cross-classifications) .


*******************************************. 



*********************************************************************************.





*********************************************************************************.
*** EOF. 


