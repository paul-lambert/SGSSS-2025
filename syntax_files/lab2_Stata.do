***************************************
**# Title and contents

**** SGSSS Summer School 2025

** Short course: Multilevel Models for Applied Social Research

** (Materials by Paul Lambert and Kate O'Hara)

***  LAB 2 - STATA EXERCISES - 'EXPLORING MULTILVEL DATA AND THE RELATIONSHIP BETWEEN RANDOM EFFECTS AND OTHER MODELLING STRATEGIES' 

***   Linked code, data and metadata: at https://github.com/paul-lambert/SGSSS-2025/

**    Version: 17/Jun/2025, written for Stata v17 


***************************************


******************************
*** Contents listing

***   Title and contents 
***   Paths and preliminaries
***   Ex 2.1: Random Effects, Fixed Effects, and the Random Effects Within Between model  
***   Ex 2.2: Comparing random effects and other strategies for dealing with a multilevel data structure 
***   Ex 2.3: Comparing random effects and other models for panel datasets
***   Ex 2.4: A look at effect scores for random effects residuals ('ESRES')  
***   Ex 2.5: Comparing estimators and their performance with different multilevel data scenarios   

********************************







*********************************************************
**#  Paths and preliminaries
********************************


*** Sets alias names for specific files (edit as required to correspond to your own machine - in most instances, the same 'path' for every file): 

global gcse_data "C:\1\sgsss-2025\data\gcse.dta"
** Used in Ex 2.1-3 & 5
** The 'London Reading Test' dataset  
** You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
** Provenance: This dataset is used in many training materails on multilevel modelling in the UK. It was used in Harvey Goldsteins influential 
*     book 'Multilevel Statistical Models'. This version of the dataset was accessed from the website for Rabe-Hesketh & Skrondal's textbook 'Multilevel
*     and Longitudinal Modeling using Stata'


global bhps_ghq_extract1 "C:\1\sgsss-2025\data\bhps_ghq_extract1.dta" 
** Used in lab Ex 2.1
** An extract from the BHPS with selected variables including GHQ (poor mental well-being score) for individuals clustered in households
** This file is supplied in class and is not for onward distribution. 
** The original data from which the extract is derived can be accessed from the UK Data Service, study number 5151

global bhps_soc_extract1 "C:\1\sgsss-2025\data\bhps_soc_extract1.dta"
** Used in lab Ex 2.2
** An extract from the BHPS with selected variables including an income measure for individuals clustered in occupational units
** This file is supplied in class and is not for onward distribution. 
** The original data from which the extract is derived can be accessed from the UK Data Service, study number 5151

global bh1to15_long "C:\1\sgsss-2025\data\bh1to15_long.dta"
** Used in lab Ex 2.3
** An extract from the BHPS with selected variables including an income measure for individuals clustered in occupational units
** This file is supplied in class and is not for onward distribution. 
** The original data from which the extract is derived can be accessed from the UK Data Service, study number 5151


global ukhls_occs_extract_1 "C:\1\sgsss-2025\data\ukhls_occs_extract_1.dta" 
** Used in lab Ex 2.4
** An extract from the UKHLS with selected variables including occupation measures, 
** This file is supplied in class and is not for onward distribution. 
** The original data from which the extract is derived can be accessed from the UK Data Service, study number 6614


global ukhls_educ_extract_1 "C:\1\sgsss-2025\data\ukhls_educ_extract_1.dta" 
** Used in lab Ex 2.4
** An extract from the UKHLS with selected variables including occupation measures, 
** This file is supplied in class and is not for onward distribution. 
** The original data from which the extract is derived can be accessed from the UK Data Service, study number 6614


global fr2006_pcs "C:\1\sgsss-2025\data\fr2006_pcs4_table.dta"
** Used in lab Ex 2.5
** An extract from the French 2006 census microdata available at IPUMS, used
**   as a convenient example of quite a large scale survey datasets 


global ess_extract1 "C:\1\sgsss-2025\data\ess_extract1.dta"
** Used in lab Ex 2.5
** An extract of data from the European Social Survey, pooled across rounds 1-9
** This file is supplied in class and is not for onward distribution. 
** The original data from which the extract is derived can be accessed from the European Social Survey's website


global pupcross_data "C:\1\sgsss-2025\data\pupcross.dta"
** Used in Ex 2.5
** You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
** Provenance: Hox, J. (2010) Multilevel Analysis. London: Sage. 


** Set alias for specific folder locations required for the lab exercise: 


global path6a "C:\soft\stata\ado\" /* location of an extant folder on your machine, where you have write permission, where Stata extension libraries can be installed to */
adopath+ "$path6a"  /* code to ensure that the nominated folder is identified by Stata as a source for code such as extension libraries */


global path9 "c:\temp\" /* location (anywhere with write permission) to allow for temporary storage of derived data files */


**** Extension libraries: 

** The following extension libraries will be required at later exercises 
*   If the 'which' command generates an error, run the code within the comment symbols to installl
*   (most commands don't require them - you can proceed without them if this if it doesn't go smoohtly)


** gllamm, a tool that supports alternative estimation routines for a wide range of models
which gllamm
/* If necessary (if the command 'which gllamm' leads an error message), run the following:
net from http://fmwww.bc.edu/RePEc/bocode/g 
net set ado "$path6a"
net install gllamm
*/ 

** esttab, a tool that supports extra options in processing and presenting the results from multiple model outputs 
which esttab
/* If necessary, run the following:
net from http://fmwww.bc.edu/RePEc/bocode/e 
net set ado "$path6a"
net install estout
*/ 


** tabplot, for a popular descriptive graphic format
which tabplot
/* If necessary, run the following:
net from http://fmwww.bc.edu/RePEc/bocode/t 
net set ado "$path6a"
net install tabplot
*/ 



********************************
********************************






**********************************************************




**********************************************************
**#   Ex 2.1: Random Effects, Fixed Effects, and the Random Effects Within Between model  
**********************************************************



**** Example using the BHPS  GHQ data extract 


de using "$bhps_ghq_extract1"
use "$bhps_ghq_extract1", clear 


** The hierarchy in this dataset is individuals within households 
sort ohid pid
list pid ohid in 1/100

* In our analytical sample we'll have less cases though: 
codebook pid ohid ghq fem age age2 cohab  emp_10hrs  gdn , compact
capture drop miss1
egen miss1=rmiss(ghq fem age age2 cohab  emp_10hrs  gdn)
tab miss1 


* The model we're basically interested in is: 

mixed ghq fem age age2 cohab  gdn  emp_10hrs    if miss1==0 
est store ghq1 


* A conventional multilevel model would add random intercepts to recognise clustering of individuals within households

mixed ghq fem age age2 cohab  gdn  emp_10hrs    if miss1==0 ||ohid:, reml
est store ghq2


* One problem with this formulation is that the current coefficient estimates might be some unknown mix of 'between'
*   (household to household) and 'within' (differences amongst people within households) processes

* (This may or may not be a problem, depending on your substantive focus) 


* In econometrics, it's routine practice to want only the 'within' parameter, for which purpose the fixed 
*    effects model is available in various guises 

xtreg ghq fem age age2 cohab  gdn  emp_10hrs    if miss1==0 , i(ohid) fe
est store ghq_fe

* These coefficients express the average net pattern of how a higher or lower value of x within a household links to higher or lower 
*    ghq than others from the same household
*(note the 'gdn' variable drops out because it's a level 2 variable (there is no variation within households in having a garden))



** Outside of economics, it is more common to use 'group means' at the cluster level to disentangle some or all between from within effects...

** First, just for one variable...: 
**  Make household level 'group mean' variable for emp_10hrs and use it 
capture drop hh_emp
egen hh_emp = mean(emp_10hrs) if miss1==0, by(ohid)
** Also make household level 'deviation frmo group mean' variable for emp_10hrs
capture drop dev_emp
gen dev_emp =  emp_10hrs - hh_emp
codebook pid ohid ghq fem age age2 cohab  gdn emp_10hrs  dev_emp hh_emp  if miss1==0 , compact

* Put them together either using the original lower level or the deviation from mean lower level variables 

mixed ghq fem age age2 cohab  gdn hh_emp emp_10hrs    if miss1==0 ||ohid:, reml
est store orig_var
mixed ghq fem age age2 cohab  gdn hh_emp dev_emp   if miss1==0 ||ohid:, reml
est store dev_var

est table ghq1 ghq2  orig_var dev_var, stats(N ll bic) b(%8.4g) star eq(1)

** Note that in both orig_var and dev_var, the coefficient for the lower level variable captures the 'within effect' 
*    in the context of the remaining model. The two models are identical in statistical terms. 
*   The cofficient for the group mean is different however - in 'dev_var' is estimates the average 'between effect', whereas
*    in 'orig_var' it can be thought of as the 'contextual effect' (= between - within) 

*** So for any specific lower level variable, we can disentangle within from between by calculating group mean and 
*     deviation from mean and adding them both to the model 




*** If we want to, we might also disentangle every possible occurence of within from between, 
*      which amounts to the 'Random effects within-between model' (REWB) 
*      (note - this might not be realistic if you have small numbers of higher level units)

*** For example:

codebook fem age age2 cohab emp_10hrs, compact
foreach var in fem age age2 cohab emp_10hrs {
  capture drop hmean_`var'
  egen hmean_`var'=mean(`var')   if miss1==0 , by(ohid) 
  capture drop hdev_`var'
  gen hdev_`var' = `var' - hmean_`var'
  } 

* Note however, for the quadratic formulation of the age term, the mean and deviation from mean terms should be calculated differently
replace hmean_age2=hmean_age^2 
replace hdev_age2=hmean_age2 - age2 

* (For simplicity, I'll make the comparison here without the household level 'gdn' variable )

mixed ghq hmean_fem hmean_age hmean_age2 hmean_cohab hmean_emp_10hrs   ///
            hdev_fem hdev_age hdev_age2 hdev_cohab hdev_emp_10hrs     if miss1==0    ||ohid:, reml 
est store rewb

** In general, this formulation ought to give us almost exactly the same coefficients as if
*    we used a 'fixed effects' or 'between effects' model estimator 

xtreg ghq fem age age2 cohab emp_10hrs  if miss1==0, i(ohid) fe 
est store fe_2

xtreg ghq fem age age2 cohab emp_10hrs  if miss1==0, i(ohid) be 
est store be_2

est table fe_2 be_2 rewb , stats(N ll bic) b(%8.4g) star eq(1)

* (i.e. 'hdev_...' should match to the fe coefficient, and h_mean to the 'be' coefficient) 


** Aside - the figures above aren't exactly matched and this partly reflects further incomparabilities 
*     involving the age-squared terms. If we repeat the analysis fitting age only as a linear term, 
*     we see exactly the same 'within' coefficients and only minor differences in 'between' coefficients: 

mixed ghq hmean_fem hmean_age  hmean_cohab hmean_emp_10hrs   ///
            hdev_fem hdev_age hdev_cohab hdev_emp_10hrs     if miss1==0    ||ohid:, reml 
est store rewb_3

xtreg ghq fem age cohab emp_10hrs  if miss1==0, i(ohid) fe 
est store fe_3

xtreg ghq fem age  cohab emp_10hrs  if miss1==0, i(ohid) be 
est store be_3

est table fe_3 be_3 rewb_3 , stats(N ll bic) b(%8.4g) star eq(1)


* => So, a common disciplinary difference is that in econ, the fe model is preferred, whereas outside econ, 
*      the rewb model would be used if there was motivation to retrieve the optimal within parameters. 



***********************************










**********************************************************


**********************************************************
**#   Ex 2.2: Comparing random effects and other strategies for dealing with a multilevel data structure 
**********************************************************



*********************
*** i) Some outputs looking at robust standard errors using the GCSE data (also used in the lecture) 

use "$gcse_data", clear
label variable gcse "GCSE score (Z score*10)"
label variable lrt "London Reading test score (Z score*10)"
label variable girl "Female"
label variable schgen "Type of school"
capture label drop schgenl 
label define schgenl 1 "Mixed gender" 2 "Boys only" 3 "Girls only"
label values schgen schgenl
* (Labels from Rabe-Hesketh and Skrondal, 2010: 141)
capture drop sch_*
tab schgen, gen(sch_) /* makes dummy var indicators for school gender */
tab1 schgen sch_*
codebook gcse lrt girl schgen sch_2 sch_3 , compact 

* So this might be the model we want to use
regress gcse lrt girl sch_2 sch_3


* But if we know of clustering by school, we could also try..

regress gcse lrt girl sch_2 sch_3, robust cluster(school) 

svyset , clear
svyset school
svy: regress gcse lrt girl sch_2 sch_3 
* (i.e., svy by default applies robust standard errors, i.e. same results as the regression above)


mixed gcse lrt girl sch_2 sch_3 ||school:, stddev 

mixed gcse lrt girl sch_2 sch_3 ||school:, stddev robust

* These adjustments 'fix the standard errors' by taking account of clustering of values. 



* They don't, however, response to the possible 'between' or 'contextual' influence of school 


* To do that, we might define and use school level variables:

capture drop s_lrt
egen s_lrt = mean(lrt), by(school) 


regress gcse s_lrt lrt girl sch_2 sch_3
* Something like this is common in practice - using a higher level variable, but with no other adjustment 

regress gcse s_lrt lrt girl sch_2 sch_3, robust cluster(school) 
* this would be a bit better, with more appropriate standard errors, particularly impacting the school level variables 

mixed gcse s_lrt lrt girl sch_2 sch_3 ||school:, stddev 
* as would this

*** The above are typical multilevel model examples where we focus on the micro-level influences but with 
*     some extra attention to cluster level correlations and selected cluster-level influences 


*** There are though other commonly used responses that are more extreme than this, which are 
*     particularly familiar in econometrics: 

*** The 'between effects' model, which just focuses on cluster-to-cluster relationships (equivalent to 
*       doing an analysis at the level of cluster averages, i.e. at the macrolevel)

xtreg gcse lrt girl sch_2 sch_3, i(school) be 


*** The 'fixed effects' model, which just focuses on within cluster relationships (equivalent to 
*       doing an analysis with a dummy variable for every cluster so that the remaining coefficients are 
*       driven exclusively by within cluster variations 

xtreg gcse lrt girl sch_2 sch_3, i(school) fe


*** Summary: fe and be and robust standard errors are all plausible repsonses to a multilevel data structure. 
*      The random effects model, potentially with some use of higher level averages (group mean variables) is a 
*       popular substantive choice outside of econ, but is just one plausible choice from others. 






***********************
*** ii) A few more examples looking at robust standard errors versus random intercept coefficients


** Example using a data extract from the BHPS where individuals are clustered into job units   
use "$bhps_soc_extract1", clear
codebook ajbsoc lfimnl ajbhrs ahlstat fem age age2 cohab separ widow degdip vocq noqual, compact
* Some job level measures:
summarize mcamsis ajbonus 
tab ajbonus
capture drop bonus
gen bonus=ajbonus==1
capture drop s_mcam
egen s_mcam=mean(mcamsis), by(ajbsoc)
capture drop s_bonus
egen s_bonus=mean(bonus), by(ajbsoc)
label variable s_mcam "Job level CAMSIS score"
label variable s_bonus "Job level bonus rate"
capture drop miss1
egen miss1=rmiss(lfimnl ajbhrs ahlstat fem age age2 cohab separ widow degdip vocq noqual s_mcam s_bonus)
tab miss1
keep if miss1==0
* Centre continuous variables:
capture drop c_*
summarize ajbhrs, detail
gen c_ajbhrs=ajbhrs - round(r(p50))
summarize s_mcam, detail
gen c_smcam=s_mcam - round(r(p50))
summarize ahlstat, detail
gen c_ahlstat=ahlstat - round(r(p50))
summarize age, detail
gen c_age=age - r(min)
gen c_age2=age^2 - r(min)^2
* Plausible interaction:
capture drop femage
gen femage=fem*c_age
label variable lfimnl "Log monthly income"
label variable c_ajbhrs "Hours of work (centred)"
label variable c_ahlstat "Subjective health (centred)"
label variable c_smcam "Job level camsis (centred)"
label variable bonus "Own receipt of bonus"
label variable cohab "Married/cohabiting"
label variable degdip "Education: Degree/diploma"
label variable vocq "Education: vocational qualification"
label variable noqual "Education: no qualifications" 
label variable femage "Female*linear age interaction" 
codebook lfimnl c_ajbhrs c_ahlstat fem c_age c_age2 cohab degdip vocq noqual femage ///
       c_smcam s_bonus bonus, compact
sav "$path9\soc_temp.dta", replace

use "$path9\soc_temp.dta", clear
summarize
* Imagine we were primarily interested in the effects of explanatory
*  variables at the higher level

** Example 1: demographic influences at higher and lower level: 
capture drop s_fem
egen s_fem=mean(fem), by(ajbsoc)
summarize s_fem /* proportion of women in job*/
codebook lfimnl age age2 fem s_fem , compact
regress lfimnl age age2 fem s_fem
est store reg1
regress lfimnl age age2 fem s_fem, robust cluster(ajbsoc)
est store reg1_rb
mixed lfimnl age age2 fem s_fem ||ajbsoc:, mle 
est store xtm_ri
mixed lfimnl age age2 fem s_fem ||ajbsoc:fem, mle cov(un)
est store xtm_rc


est table reg1 reg1_rb xtm_ri xtm_rc, b(%7.3g) se stats(r2 ll bic N N_clust) eq(1)
** Interpretation: We're seeing fairly big difference between some of the fixed 
**   part estimates in the single level and random effects models, particularly for the 
**   level 2 variable (s_fem) - would benefit from more reflection on the underlying process

*** Routine for displaying estimates and their standard errors for s_fem variable:
capture program drop getse1
program define getse1 
 est restore $mod
 matrix temp=e(b)
 matrix temp2=e(V)
 capture drop coef_$mod
 gen coef_$mod=temp[1,$wx]
 sum coef_$mod
 capture drop se_$mod
 gen se_$mod=(temp2[$wx,$wx])^0.5 
 capture drop ciu_$mod
 gen ciu_$mod=coef_$mod + 1.96*se_$mod
 capture drop cil_$mod
 gen cil_$mod=coef_$mod - 1.96*se_$mod
 sum se_$mod ciu_$mod cil_$mod
end

global wx "4" /* wx tells prog which x variable to focus on */
global mod "reg1"
getse1
global mod "reg1_rb"
getse1
global mod "xtm_ri"
getse1
global mod "xtm_rc"
getse1

* Plot of the coefficient estimates and their CI's: 
capture drop ncol
gen ncol=_n
graph twoway  (rspike ciu_reg1 cil_reg1 ncol if _n==1, lwidth(thick) lcolor(gs12)) ///
    (scatter coef_reg1 ncol if _n==1, msymbol(circle) msize(large) mcol(gs8)) ///
    (rspike ciu_reg1_rb cil_reg1_rb ncol if _n==2, lwidth(thick) lcolor(gs12)) ///
    (scatter coef_reg1_rb ncol if _n==2, msymbol(circle) msize(large) mcol(gs8)) ///
 (rspike ciu_xtm_ri cil_xtm_ri ncol if _n==3, lwidth(thick) lcolor(gs12)) ///
    (scatter coef_xtm_ri ncol if _n==3, msymbol(circle) msize(large) mcol(gs8)) ///
    (rspike ciu_xtm_rc cil_xtm_rc ncol if _n==4, lwidth(thick) lcolor(gs12)) ///
    (scatter coef_xtm_rc ncol if _n==4, msymbol(circle) msize(large) mcol(gs8)), /// 
  xscale(range(0 5)) xlabel(1 "Regress" 2 "Robust Reg" 3 "RI" 4 "RC") ///
   title("Higher level coefficient (s_fem)") legend(off) ///
   subtitle("Regression B and standard error; N=4907; N_g=328")
   

*** A second example - here looking at the effect of job characteristics:

codebook lfimnl s_mcam s_bonus, compact

regress lfimnl s_mcam s_bonus
est store reg1
regress lfimnl s_mcam s_bonus, robust cluster(ajbsoc)
est store reg1_rb
mixed lfimnl s_mcam s_bonus ||ajbsoc:, mle
est store xtm_ri
est table reg1 reg1_rb xtm_ri, stats(ll bic r2 N_clust N) b(%7.3g) se eq(1)

** Again, the re and single-level beta estimates are quite different, and 
*    it could be that the re estimates are biassed due to the correlation 
*    between explanatory variables and random effects (subject to debate)

global wx "2" 
global mod "reg1"
getse1
global mod "reg1_rb"
getse1
global mod "xtm_ri"
getse1

* Plot of the coefficient estimates and their CI's: 
capture drop ncol
gen ncol=_n
graph twoway  (rspike ciu_reg1 cil_reg1 ncol if _n==1, lwidth(thick) lcolor(gs12)) ///
    (scatter coef_reg1 ncol if _n==1, msymbol(circle) msize(large) mcol(gs8)) ///
    (rspike ciu_reg1_rb cil_reg1_rb ncol if _n==2, lwidth(thick) lcolor(gs12)) ///
    (scatter coef_reg1_rb ncol if _n==2, msymbol(circle) msize(large) mcol(gs8)) ///
 (rspike ciu_xtm_ri cil_xtm_ri ncol if _n==3, lwidth(thick) lcolor(gs12)) ///
    (scatter coef_xtm_ri ncol if _n==3, msymbol(circle) msize(large) mcol(gs8)) ///
      , /// 
  xscale(range(0 4)) xlabel(1 "Regress" 2 "Robust Reg" 3 "RI") ///
   title("Higher level coefficient (s_bonus)") legend(off) ///
   subtitle("Regression B and standard error; N=4907; N_g=328")
   
** Comment: The ri model's standard errors ought to be best, but the reason for the 
*            different point estimates would benefit from reflection 

**********************









**********************************************************
**#   Ex 2.3: Comparing random effects and other models for panel datasets
**********************************************************




**** This excercise runs through a series of commonly used panel models and compares their features 

de using "$bh1to15_long"
use "$bh1to15_long", clear

* Data prep stages: 
numlabel _all, add
summarize 
gen ghq=zhlghq1 if zhlghq1 >= 0 
gen lninc=ln(zfimn) if (zfimn >= 100)
summarize ghq lninc
xtsum ghq lninc, i(pid)
gen fem=(zsex==2)
gen age=zage - 40 if zage >= 16 /* age, also centred */
gen age2=zage^2 - 40^2 /* an age-squared term, also centred */
tab zqfedhi 
gen hied=(zqfedhi >= 1 & zqfedhi <= 4) if zqfedhi >= 1
gen noed=(zqfedhi >= 12) if zqfedhi >= 1
gen convot=(zvote4==1) if (zvote >= 1)
gen labvot=(zvote4==2) if (zvote >= 1) 
summarize ghq lninc fem age hied noed convot labvot
correlate ghq lninc fem age hied noed convot labvot 
capture drop fullinf
egen fullinf= rowmiss(ghq lninc fem age hied noed convot labvot)
tab fullinf 
xtdes if fullinf==0, i(pid) t(year)



** Modelling analysis

codebook pid ghq lninc fem age age2 hied noed convot labvot if fullinf==0, compact
* i.e. we'll analyse these records, were we have 103k records from 21k different people 

* Single-level regression ignoring the panel element of the data 
regress ghq lninc fem age age2 hied noed convot labvot
est store lin2

* Regression with robust standard errors for the panel clustering 
regress ghq lninc fem age age2 hied noed convot labvot, robust cluster(pid)
est store clus2

* Version of the fixed effects model
xtreg ghq lninc fem age age2 hied noed convot labvot, i(pid) fe 
est store fe2

* Version of the between effects model 
xtreg ghq lninc fem age age2 hied noed convot labvot, i(pid) be 
est store be2

* Version of the random effects random intercepts model (the 'gls' estimator via 'xtreg') 
xtreg ghq lninc fem age age2 hied noed convot labvot, i(pid) re 
est store re2


* Another version of the random effects random intercepts model (the 'mle' estimator via 'xtreg')
xtreg ghq lninc fem age age2 hied noed convot labvot, i(pid) mle 
est store mle2


est table  lin2 clus2 be2 fe2 re2 mle2, star stats(N r2 rho ll) b(%8.4g) eq(1) 

** This is a common approach to comparing popular panel models

** We normally expect different coefficients in the fixed effects (all about within-person change) and 
*     between effects (all about person-to-person average patterns) coefficients 
** (In panel data, is it sometimes a little surprising how similar 'between' and 'within' coefficients sometime are!)
*  Coefficients from other models would normally be somewhere between the within and between coefficients
*     (for instance, the random effects coefficients are a weighted average of the two, but we don't really 
*      control how much weight one or the other process gets)



* Another version of the random effects random intercepts model (the 'reml' estimator via 'mixed')
mixed ghq lninc fem age age2 hied noed convot labvot ||pid:, reml stddev
est store reml2

* A different version of the random effects model, with random slope as well as intercept (with the 'reml' estimator via 'mixed')
mixed ghq lninc fem age age2 hied noed convot labvot ||pid:age, reml cov(un) stddev
est store reml2b

est table   be2 fe2  mle2 reml2 reml2b, star stats(N r2 rho ll) b(%8.4g) eq(1)

* Usual take-home: the reml random intercepts isn't usually importantly different from the gls or mle models; 
*   but the random slopes model does have something extra that could be of interest (here, evidence of 
*          variations in how age works from person to person) 



**** How different the 'between' and 'within' patterns are, and where the random effects coefficients come out, 
*       can vary a lot from application to application. Just for example: 


* Models for men only
xtreg ghq lninc  age age2 hied noed convot labvot if fem==0, i(pid) fe 
est store fe3m

xtreg ghq lninc age age2 hied noed convot labvot if fem==0, i(pid) be 
est store be3m

xtreg ghq lninc age age2 hied noed convot labvot if fem==0, i(pid) re 
est store re3m

* Repeated for women only
* Models for men only
xtreg ghq lninc  age age2 hied noed convot labvot if fem==1, i(pid) fe 
est store fe3f

xtreg ghq lninc age age2 hied noed convot labvot if fem==1, i(pid) be 
est store be3f

xtreg ghq lninc age age2 hied noed convot labvot if fem==1, i(pid) re 
est store re3f

est table   be3m fe3m re3m be3f fe3f re3f, star stats(N r2 rho ll) b(%8.4g) eq(1)

** For the men, the between effect of income is much larger than the within effect, 
*     and the random effects model coefficent is closer to the within effect; 

** For the women, the within effect of income is more substantial than the between effect, 
*     and the random effects model coefficient is closter to the between effect 






**** Expansion on the 'Hybrid model' (same as the Random Effects Within Between model) in panel data format

* First, remember that the fixed effects equates to the model with deviation terms,
*   and or by including the group means: 

codebook ghq lninc fem age age2 hied noed pid , compact
capture drop rmiss
egen rmiss=rowmiss(ghq lninc fem age age2 hied noed  pid)

xtreg ghq lninc fem age age2 hied noed  if rmiss==0, i(pid) fe
est store fix1 

foreach var in  lninc fem age age2 hied noed  {
  capture drop m_`var'
  capture drop dev_`var'
  egen m_`var' = mean(`var') if rmiss==0, by(pid) 
  gen dev_`var' = `var' - m_`var' 
  }

regress ghq dev_lninc dev_fem dev_age dev_age2 dev_hied dev_noed ///
          m_lninc m_fem m_age m_age2 m_hied m_noed 
est store fix2 

regress ghq lninc fem age age2 hied noed ///
          m_lninc m_fem m_age m_age2 m_hied m_noed 
est store fix3 

est table fix1 fix2 fix3, stats(N ll bic) b(%11.5g) star
* i.e. with either formulation we retrieve the within effect coefficients

** Second, we add random effects for clustering of records to the latter 
*    formulations: Allison (2009) argues that this model is optimal as it 
*     has both optimal coefficients and optimal standard errors


mixed ghq dev_lninc dev_fem dev_age dev_age2 dev_hied dev_noed ///
          m_lninc m_fem m_age m_age2 m_hied m_noed ||pid:, 
est store hyb1 

mixed ghq lninc fem age age2 hied noed ///
          m_lninc m_fem m_age m_age2 m_hied m_noed ||pid:, 
est store hyb2 

est table fix1 fix2 fix3 hyb1 hyb2, stats(N ll bic) b(%11.5g) star equations(1)

* (i.e. the hybrid model is a better fit, whilst still retrieving 
*    the fixed effects estimates)


**********************************************************







**********************************************************
**#   Ex 2.4: A look at effect scores for random effects residuals ('ESRES')  
**********************************************************

** This code replicates the procedures which were used to generate the 'ESRES' outputs based on UKHLS data that are shown in Talk 3




**************************************************************************
*** Example of scaling for occupation data categories 


de using "$ukhls_occs_extract_1" 
use "$ukhls_occs_extract_1" , clear 
codebook, compact


*** SOC10 
tab soc10
mixed ln_pay ||soc10:, reml 
est store m1 
capture drop ebi_p
capture drop ebi_se
predict ebi_p, reffects reses(ebi_se)

table soc10 , stat(mean ebi_p) stat(n ebi_p)  /* ie these numbers would be our 'ESRES' values */

* Viz of the same things: Plot of residuals and standard errors for each category 
capture drop first
egen first=tag(soc10) 
gsort +ebi_p -first
capture drop rank
gen rank=sum(first) 
tab rank
list soc10 rank first ebi_p in 1/100
capture drop labpos
gen labpos = ebi_p + 1.96*ebi_se + 0.4 /* Uses prediction error estimates, ie plus or minus 2 sds */
replace labpos = ebi_p - 1.96*ebi_se - 0.4 if (floor(rank/2))*2==rank 
capture drop soc10b
gen soc10b=soc10 /* for a new measure without text labels */
serrbar ebi_p ebi_se rank if first==1 , ///
     addplot(scatter labpos rank if first==1 , mlabel(soc10b) msymbol(none) mlabpos(0) mlabsize(tiny)  ) ///
     scale(1.96) title("Job level residuals in rank order") ///
    xtitle("Predicting log of monthly earnings") ytitle("Job" "level"  "residual", orientation(horizontal)) legend(off) scheme(s1mono) ///
    yline(0, lcolor(blue*0.4%30) lwidth(thick)) xsize(8) ysize(5) name(occs1, replace) 


*** What about with far fewer categories - using 'SOC3' 
tab soc3
mixed ln_pay ||soc3:, reml 
est store m2 
capture drop ebi_p
capture drop ebi_se
predict ebi_p, reffects reses(ebi_se) 

table soc3, stat(mean ebi_p) stat(n ebi_p)  /* ie these numbers would be our 'ESRES' values */

capture drop first
egen first=tag(soc3) 
gsort +ebi_p -first
capture drop rank
gen rank=sum(first) 
tab rank
list soc3 rank first ebi_p in 1/100
capture drop labpos
gen labpos = ebi_p + 1.96*ebi_se + 0.35 /* Uses prediction error estimates, ie plus or minus 2 sds */
replace labpos = ebi_p - 1.96*ebi_se - 0.35 if (floor(rank/2))*2==rank 

serrbar ebi_p ebi_se rank if first==1 , ///
     addplot(scatter labpos rank if first==1 , mlabel(soc3) msymbol(none) mlabpos(0) mlabsize(medsmall)  ) ///
     scale(1.96) title("Job level residuals in rank order") ///
    xtitle("Predicting log of monthly earnings") ytitle("Job" "level"  "residual", orientation(horizontal)) legend(off) scheme(s1mono) ///
    yline(0, lcolor(blue*0.4%30) lwidth(thick)) xsize(6) ysize(5) xscale(range(0 4))  name(occs1, replace)  



**  Reflection: In either case, we've generated plausible ranking scores for the occupation categories. 
*     There is a reasonable narrative about what they're based on (pay averages in this setting, but it could be whatever is in the model. 
*     They are approriately conservative given shrinkage, which is more relevant for the more detailed of the two measures (soc10). 


******************************************************



*** Example of education qualifications data: categories and scores 

de using "$ukhls_educ_extract_1"
use "$ukhls_educ_extract_1", clear
codebook, compact

numlabel _all, add
tab hiqual_dv qfhigh_dv
* In this case, we'll think about scaling education categories using data from these two related measures 

capture drop qfhigh2
clonevar qfhigh2=qfhigh_dv 
replace qfhigh2=2 if qfhigh_dv==-8 & hiqual_dv==1
replace qfhigh2=3 if qfhigh_dv==-8 & hiqual_dv==2
replace qfhigh2=7 if qfhigh_dv==-8 & hiqual_dv==3
replace qfhigh2=13 if qfhigh_dv==-8 & hiqual_dv==4
replace qfhigh2=16 if qfhigh_dv==-8 & hiqual_dv==5
replace qfhigh2=96 if qfhigh_dv==-8 & hiqual_dv==9
tab qfhigh2 /* i.e. a manually derived combined variable */

* Viz for the data patterns
capture drop cons
gen cons=1
sum dvage
numlabel _all, remove
tabplot qfhigh2 cons if qfhigh2 > 0 & dvage > 25 & dvage <= 80 , bcolor(green*0.5%50) blcolor(gs8) fysize(100) fxsize(60) ///
   xtitle("") ytitle("") xlabel(none) scheme(s1mono) note("") subtitle("Original", span) percent(cons)  height(0.8)
graph save $path9\g1.gph, replace

numlabel _all, add
tab1 hiqual_dv qfhigh2
capture drop qfhigh_4
gen qfhigh_4=qfhigh2
recode qfhigh_4 (1 2=1) (3 4 5 6=2) (7 8 9 10 11 12 13=3) (14 15 16 96=4)
label define qfhigh_4 1 "Degree" 2 "Diploma" 3 "Higher school" 4 "Low schl. or none",  modify
label values qfhigh_4 qfhigh_4
numlabel _all, remove
tabplot qfhigh_4 cons if qfhigh2 > 0 & dvage > 25 & dvage <= 80 , bcolor(green*0.5%50) blcolor(gs8) fysize(100) fxsize(50) ///
   xtitle("") ytitle("") xlabel(none) scheme(s1mono) note("") subtitle("Four category", span) percent(cons)  height(0.4)
graph save $path9\g2.gph, replace


numlabel _all, add
tab1 hiqual_dv qfhigh2
capture drop qfhigh_2
gen qfhigh_2=qfhigh2
recode qfhigh_2 (1 2 3 4 5 6=1) (7 8 9 10 11 12 13 14 15 16 96=2)
label define qfhigh_2 1 "Degree or diploma" 2 "No Deg/Dip", modify
label values qfhigh_2 qfhigh_2
numlabel _all, remove
tabplot qfhigh_2 cons if qfhigh2 > 0 & dvage > 25 & dvage <= 80 , bcolor(green*0.5%50) blcolor(gs8) fysize(100) fxsize(50) ///
   xtitle("") ytitle("") xlabel(none) scheme(s1mono) note("") subtitle("Binary", span) percent(cons) height(0.2)
graph save $path9\g3.gph, replace

graph combine $path9\g1.gph $path9\g2.gph  $path9\g3.gph,  scheme(s1mono) cols(3) xsize(6) ysize(5) ///
   note("UKHLS wave M: Distribution of highest educational qualifications amongst 25-80 year olds.") name(educ, replace) 


** Summary statistics as prospective scores for the categories:

sum fimngrs_dv
capture drop ln_pay
gen ln_pay = ln(fimngrs_dv) if fimngrs > 0 

capture drop ln_mean
egen ln_mean=mean(ln_pay) if qfhigh2 > 0 & dvage > 25 & dvage <= 80, by(qfhigh2)
replace ln_mean = exp(ln_mean)

table qfhigh2 if qfhigh2 > 0 & dvage > 25 & dvage <= 80 , stat(mean ln_pay) stat(mean ln_mean) stat(n ln_pay)

*** ESRES with null model 

mixed ln_pay if qfhigh2 > 0 & dvage > 25 & dvage <= 80 || qfhigh2:, reml
capture drop ln_ref 
predict ln_ref if qfhigh2 > 0 & dvage > 25 & dvage <= 80, reffects
replace ln_ref=ln_ref + _b[_cons]
replace ln_ref = exp(ln_ref)

table qfhigh2 if qfhigh2 > 0 & dvage > 25 & dvage <= 80 , stat(mean ln_pay) stat(mean ln_mean) stat(mean ln_ref) stat(n ln_pay)


*** ESRERs with explanatory variables

capture drop cen_age
gen cen_age = dvage - 30 if dvage >= 25 & dvage <= 80
capture drop cen_age2
gen cen_age2 = dvage^2 - 30^2   
capture drop fem
gen fem=(sex_dv==2)
capture drop age_fem
gen age_fem=cen_age*fem
capture drop age2_fem 
gen age2_fem=cen_age2*fem
codebook ln_pay fem cen_age cen_age2 age_fem age2_fem qfhigh2 if qfhigh2 > 0 & dvage > 25 & dvage <= 80, compact

mixed ln_pay fem cen_age cen_age2 age_fem age2_fem

mixed ln_pay fem cen_age cen_age2 age_fem age2_fem ||qfhigh2:, reml
capture drop ln_ref2 
predict ln_ref2 if qfhigh2 > 0 & dvage > 25 & dvage <= 80, reffects /* qualification specific adjustment */
replace ln_ref2=ln_ref2 + _b[_cons]  /* qualification specific predicted value (for male aged 30) */
replace ln_ref2 = exp(ln_ref2)

label variable ln_ref "Null ESRES"
label variable ln_ref2 "ESRES (controls)" 

table qfhigh2 if qfhigh2 > 0 & dvage > 25 & dvage <= 80 , stat(mean ln_pay) stat(mean ln_mean) stat(mean ln_ref) stat(mean ln_ref2) stat(n ln_pay)

** Any of these scales are plausible scores

capture drop first
egen first=tag(qfhigh2) if ~missing(ln_ref2)

label variable ln_ref2 "ESRES (net of gender & age controls)" 
label variable ln_mean "EPS"

graph matrix  ln_ref2  ln_ref ln_mean if first==1 , scheme(s1mono) half maxes(xlabel(none) ylabel(none))


** Exploring impact of different measures in regression examples: with various functional forms of educ, predicting betting

numlabel _all, add
tab1 hiqual_dv qfhigh2
capture drop qfhigh_4
gen qfhigh_4=qfhigh2
recode qfhigh_4 (1 2=1) (3 4 5 6=2) (7 8 9 10 11 12 13=3) (14 15 16 96=4)
label define qfhigh_4 1 "Degree" 2 "Diploma" 3 "Higher school" 4 "Low schl. or none",  modify
label values qfhigh_4 qfhigh_4
tab1 hiqual_dv qfhigh2
capture drop qfhigh_2
gen qfhigh_2=qfhigh2
recode qfhigh_2 (1 2 3 4 5 6=1) (7 8 9 10 11 12 13 14 15 16 96=2)
label define qfhigh_2 1 "Degree or diploma" 2 "No Deg/Dip", modify
label values qfhigh_2 qfhigh_2
** EPS ('Effect proportional scaling': just calculate scores from conventional summary statistics)  
sum fimngrs_dv
capture drop ln_pay
gen ln_pay = ln(fimngrs_dv) if fimngrs > 0 
capture drop ln_mean
egen ln_mean=mean(ln_pay) if qfhigh2 > 0 & dvage > 25 & dvage <= 80, by(qfhigh2)
replace ln_mean = exp(ln_mean)
*** ESRES null: calcualte scores from random effects residuals 
mixed ln_pay if qfhigh2 > 0 & dvage > 25 & dvage <= 80 || qfhigh2:, reml
capture drop ln_ref 
predict ln_ref if qfhigh2 > 0 & dvage > 25 & dvage <= 80, reffects
replace ln_ref=ln_ref + _b[_cons]
replace ln_ref = exp(ln_ref)

*** ESRES controls: calculate scores from random effects residuals 

codebook ln_pay fem cen_age cen_age2 age_fem age2_fem qfhigh2 if qfhigh2 > 0 & dvage > 25 & dvage <= 80, compact
mixed ln_pay fem cen_age cen_age2 age_fem age2_fem ||qfhigh2:, reml
capture drop ln_ref2 
predict ln_ref2 if qfhigh2 > 0 & dvage > 25 & dvage <= 80, reffects /* qualification specific adjustment */
replace ln_ref2=ln_ref2 + _b[_cons]  /* qualification specific predicted value (for male aged 30) */
replace ln_ref2 = exp(ln_ref2)

foreach var in ln_mean ln_ref ln_ref2 { 
    sum `var' if qfhigh2 > 0 & dvage > 25 & dvage <= 80 
    replace `var' = (`var' - r(mean)) / r(sd)
   }

sum ln_mean ln_ref ln_ref2

** Education measures are:

label variable qfhigh_4 "Education (4-category recode)"
label variable qfhigh_2 "Education (2-category recode)"
label variable ln_mean "EPS"
label variable ln_ref "Null ESRES"
label variable ln_ref2 "ESRES (net of gender & age controls)"
codebook qfhigh2 qfhigh_4 qfhigh_2 ln_mean ln_ref ln_ref2  if qfhigh2 > 0 & dvage > 25 & dvage <= 80 , compact


** Outcome variable 

tab any_bet if qfhigh2 > 0 & dvage > 25 & dvage <= 80 /* yes = 29.9% */

** Variety of models for comparison

logit any_bet fem cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store base

logit any_bet ln_ref fem cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store esres1a

logit any_bet fem c.ln_ref##c.cen_age  cen_age2    if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store esres1b

logit any_bet ln_ref2 fem cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store esres2a

logit any_bet fem c.ln_ref2##c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store esres2b

logit any_bet ln_mean fem cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store eps1a

logit any_bet fem c.ln_mean##c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store eps1b


est table base esres1a esres2a  esres1b esres2b  , stats(N ll bic r2_p) b(%8.4g) star
* (main effects odds ratios are about 80%)

est table base esres1a esres2a eps1a esres1b esres2b  eps1b , stats(N ll bic r2_p) b(%8.4g) star

*** Would we get the same results if we used random effects for the education groups?

melogit any_bet fem cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store mbase

melogit any_bet ln_ref fem cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80  ||qfhigh2:, 
est store mesres1a

melogit any_bet fem c.ln_ref##c.cen_age  cen_age2    if qfhigh2 > 0 & dvage > 25 & dvage <= 80 ||qfhigh2:, 
est store mesres1b


melogit any_bet ln_ref2 fem cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80 ||qfhigh2:, 
est store mesres2a

melogit any_bet fem c.ln_ref2##c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80 ||qfhigh2:, 
est store mesres2b

melogit any_bet ln_mean fem cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80 ||qfhigh2:, 
est store meps1a

melogit any_bet fem c.ln_mean##c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80 ||qfhigh2:, 
est store meps1b


est table base esres1a esres2a  esres1b esres2b  , stats(N ll bic r2_p) b(%8.4g) star
* without random effects (main effects odds ratios are about 80% and significant)
est table mbase mesres1a mesres2a  mesres1b mesres2b  , stats(N ll bic r2_p) b(%8.4g) star
* with random effects (main effects odds ratios are about 80%) => same results (but potentially smaller z-values)


est table base esres1a esres2a eps1a esres1b esres2b  eps1b , stats(N ll bic r2_p) b(%8.4g) star
* ESRES isn't necessarily a better fit to data than EPS, but might have a better substantive story..? 


**************************************************
*** Comparisons with models with fixed effects

numlabel _all, add
tab1 qfhigh_2 qfhigh_4 qfhigh2 if qfhigh2 > 0 & dvage > 25 & dvage <= 80
foreach var in qfhigh_2 qfhigh_4 qfhigh2 {
   recode `var' -9/-1=.m 
  }


logit  any_bet fem ib2.qfhigh_2 c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store bin1a

logit  any_bet fem ib4.qfhigh_4  c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store quart1a

logit  any_bet fem ib96.qfhigh2 c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store full1a


logit  any_bet fem ib2.qfhigh_2##c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store bin1b

logit  any_bet fem ib4.qfhigh_4##c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store quart1b

logit  any_bet fem ib96.qfhigh2##c.cen_age cen_age2     if qfhigh2 > 0 & dvage > 25 & dvage <= 80
est store full1b



est table base esres1a esres2a eps1a bin1a quart1a full1a  , stats(N ll bic r2_p) b(%8.4g) star

est table base esres1b esres2b eps1b bin1b quart1b full1b  , stats(N ll bic r2_p) b(%8.4g) star

** ESRES and EPS better in terms of parsimony than dummy variable models 

* 
esttab base  esres1b esres2b eps1b bin1b quart1b full1b, transform( :  (exp(@))  (exp(@)))  
* (version with odds ratios) 

****************************************************************







**********************************************************



**********************************************************
**#   Ex 2.5: Comparing estimators and their performance with different multilevel data scenarios   
**********************************************************




************************************************************.
*** a) Assessing the impact of sub-optimal sample sizes using the two-level GCSE dataset


** Generally, we say it is desirable to have both large N (total number of cases), 
*    large k (total number of higher level units) and large k-bar (mean number of cases per higher
*     level unit).

** However, it is routine for real-life data not to match these features. 

** Illustration: we'll take a 'rich' dataset, then take random subsamples to illustrate 
*     suboptimal sampling features.


** GCSE attainment dsataset (mlwin sample dataset)

use "$gcse_data", clear

codebook student school gcse lrt girl schgend, compact
svyset school
svydes /* N=4059 pupils in k=65 schools with mean k-bar=62 pupils per school */ 
tab schgend girl 
gen sch_boy=(schgend==2)
gen sch_girl=(schgend==3) /* (i.e. two dummy vars for all-male, all-female school) */
codebook student school gcse lrt girl sch_boy sch_girl, compact


mixed gcse lrt girl sch_boy sch_girl ||school:, reml
est store mix1 
estat icc
* Generally these parameters are likely to be ok. Focus on the following 3:
*  ICC=0.132
*  Beta_lrt=0.560 (0.012)
*  Beta_sch_boy=1.777 (1.135)

* The below exercises make random subsamples of the data and re-estimate values
* Note that for purposes of replicabiliy, is is sensible to set the random 
*   number 'seed' explicitly, which means that the same random numbers will 
*   be generated each time - otherwise we would get slightly different results each 
*   time we re-run, as we'd be working with slightly different datasets 


** i) Impact of lower N, similar k, reduced k-bar 

sort school student 
capture drop irand
set seed 112272016 /* could give any number, but important to give a number */
gen irand = runiform() 
sort irand 
replace irand= _n /* i.e. irand is now a case number from 1 to 4069 in random sort order */


* More preparation...
* ...A bit of bespoke code designed to collect N, k-bar, k, ICC and the two betas of interest from a mixed model output: 
capture program drop getres
program define getres
 matrix define temp1=e(N_g)
 matrix define temp2=e(b)
 matrix define `1' = (  e(N), temp1, e(N)/temp1[1,1] , temp2[1,1] , temp2[1,3], (exp(temp2[1,6]))^2  / ((exp(temp2[1,6]))^2  + (exp(temp2[1,7]))^2 ) )
end

* Run the models, using the prog to pull out the relevant bits of the results

mixed gcse lrt girl sch_boy sch_girl ||school:, reml
est store rall
getres rall_m

mixed gcse lrt girl sch_boy sch_girl ||school: if irand <= 2000, reml
est store r2000
getres r2000_m

mixed gcse lrt girl sch_boy sch_girl ||school: if irand <= 1000, reml
est store r1000
getres r1000_m

mixed gcse lrt girl sch_boy sch_girl ||school: if irand <= 500, reml
est store r500
getres r500_m

mixed gcse lrt girl sch_boy sch_girl ||school: if irand <= 100, reml
est store r100
getres r100_m


* comparing the models: 
est table rall r2000 r1000 r500 r100, stats(N) b(%8.4g) se /* in 'est table' format */

matrix comb_res = ( rall_m \ r2000_m \ r1000_m \ r500_m \ r100_m)  
matrix colnames comb_res = N k k-bar Beta_LRT Beta_Boyscl ICC
matrix list comb_res /* i.e. selected results collated in a matrix of results */

* Generally: reducing N and k-var tends to widen standard errors but results suggest generally 
*    findings are bit more robust for lower level explanatory variables point estimates,
*     but less promising for the higher level variance estimate and the 
*    higher level explanatory variable estimates away from plausible values
*    (though inferences aren't generally wrong, in the sense that the wider standard errors still 
*      encompass the 'true' values)
 

 
** ii) Impact of lower k:

* (Following Elff et al. 2016: important to use reml rather than mle to minimise bias in random part coefs)

sort school student 
capture drop rand1
set seed 112272016
gen rand1=runiform()
capture drop srand1
egen srand1=mean(rand1), by(school) /* this is just a means to getting a school level random variable */
sort srand1
capture drop sindex1
egen sindex1=group(srand1) 
tab sindex /* i.e. this is a school level index with random rank order from 1 to 65 for each school) */ 

mixed gcse lrt girl sch_boy sch_girl ||school: if sindex <= 65, reml
est store keq65 
getres r_keq65
estat icc

mixed gcse lrt girl sch_boy sch_girl ||school: if sindex <= 30, reml
est store keq30 
estat icc
getres r_keq30

mixed gcse lrt girl sch_boy sch_girl ||school: if sindex <= 20, reml
est store keq20 
estat icc
getres r_keq20

mixed gcse lrt girl sch_boy sch_girl ||school: if sindex <= 10, reml
est store keq10 
estat icc
getres r_keq10

mixed gcse lrt girl sch_boy sch_girl ||school: if sindex <= 5, reml
est store keq5 
estat icc
getres r_keq5

est table keq65 keq30 keq20 keq10 keq5, stats(N ) b(%8.4g) se 

matrix comb_res2 = ( r_keq65 \ r_keq30 \ r_keq20 \ r_keq10 \ r_keq5 )  
matrix colnames comb_res2 = N k k-bar Beta_LRT Beta_Boyscl ICC
matrix list comb_res2 /* i.e. selected results in matrix of results */

** Comments: You have to bear in mind that the impact of low-k here is conflated with 
*    the impact of low N - but lower k seems to be associated with larger se and more 
*    inconsistency in the higher level variance and ICC.
*    It is also worth noting that it leads to more inconsistency  in the higher level variable 
*    beta parameter (relatively more than we see for the lower level beta parameter)
*   Broadly, k at 20 or above seems to be getting fairly close(?), though at all 
*    values the widening standard errors still (just about) encompass 'true' values



** iii) Impact of lower k-bar (the average number of cases per cluster) 

sort school student 
capture drop rand1
set seed 112272016
gen rand1=runiform()
sort school rand1
capture drop srand2
bysort school: gen srand2=_n  /* this is just a means to getting a random order pupil-within-school identifier  */
tab srand2
sum srand2
/* i.e. case number order within units, can be used to generate smaller k-bar subsamples*/

mixed gcse lrt girl sch_boy sch_girl ||school: if srand2 <= 198, reml
est store kbar1 
getres r_kbar1
estat icc

mixed gcse lrt girl sch_boy sch_girl ||school: if srand2 >=5 , reml
est store kbar2 
estat icc
getres r_kbar2

mixed gcse lrt girl sch_boy sch_girl ||school: if srand2 <= 30 , reml
est store kbar3 
estat icc
getres r_kbar3

mixed gcse lrt girl sch_boy sch_girl ||school: if srand2 <= 10, reml
est store kbar4 
estat icc
getres r_kbar4

mixed gcse lrt girl sch_boy sch_girl ||school: if  srand2 <=5, reml
est store kbar5 
estat icc
getres r_kbar5

mixed gcse lrt girl sch_boy sch_girl ||school: if srand2 >= 2 & srand2 <=6, reml
est store kbar6 
estat icc
getres r_kbar6

mixed gcse lrt girl sch_boy sch_girl ||school: if  srand2 <=3, reml
est store kbar7
estat icc
getres r_kbar7

est table kbar1 kbar2 kbar3 kbar4 kbar5 kbar6 kbar7, stats(N ) b(%8.4g) se 

matrix comb_res3 = ( r_kbar1 \ r_kbar2 \ r_kbar3 \ r_kbar4 \ r_kbar5  \ r_kbar6  \ r_kbar7 )  
matrix colnames comb_res3 = N k k-bar Beta_LRT Beta_Boyscl ICC
matrix list comb_res3 /* i.e. selected results in matrix of results */

** Comments: You have to bear in mind that the impact of low-k-bar here is conflated with 
*    the impact of low N - but lower k-bar seems to be associated with larger se 
*   Broadly, k-bar below 10 seems to lead to less reliable results; specifically,
*    the last model seems to be wrongly concluding the there is no important higher level variance
*   (but remember we've seen reliable results with much lower k-bars in other 
*     datasets, so long as the N was large enough)

*************************************************************






******************************************************************
** b)  Examples and comments upon convergence problems for models in Stata


*** Common strategies when estimates fail to converge in Stata

*  - Run the model with a finite number of iterations - non-converged results
*     at end of iterations may well reveal an error of specification 

*  - Try the model with fewer parameters...!

*  -  Fiddle around with convergence settings 
*    When very large sample sizes are involved, and the 'backed up' message
*   recurs, in my experience tinkering with the convergence criteria 
*   is likely to lead to a solution . 

** Inconsistencies are compounded by different default convergence critiria 
*   being held for difference mixed model commands, and different versions,
*   in Stata


** Here is an example from my experience, that I solved with trial and error: 

de using "$fr2006_pcs"
use "$fr2006_pcs", clear
summarize
sum h_mcamsis [fw=freq]
* This is an extract from a census dataset with 2.1m records ('frequency weighting' needed to reprsent full size of data)
** The analysis of interest is in how 'h_mcamsis' (husband's occ score) 
*   is clustered by wife's occupational unit group ('w_pcs4'), with or 
*   without other explanatory variables about the husbands job (e.g. 'h_degree' 
*    = percent in the job with a degree)

regress h_mcamsis [fw=freq]
est store m1
areg h_mcamsis [fw=freq], absorb(w_pcs4) /* format of the fixed effects model (dummy variables for wives' occupation unit */
est store m2
* Clearly the wive's occupational categories are linked to the husbands job 
*   score (an r2 using fixed effects of 0.288)

regress h_mcamsis h_degree [fw=freq]
est store m1a
areg h_mcamsis h_degree [fw=freq], absorb(w_pcs4)
est store m2a
* The wive's occ categories influence is much diminished when other control measures
*  are included, but it is still there


*** The following mixed models ought in principle to work, but they don't converge:
**  for some iteration settings, including the default iteraction settings for 
*** Stata v13.0   

mixed h_mcamsis [fw=freq] ||w_pcs4:, mle /* terminate this model with the 'stop' button (white cross in red circle) when you've had enough */
*est store m1c

mixed h_mcamsis h_degree [fw=freq] ||w_pcs4:, mle /* terminate when you've had enough */
*est store m1d

* One imperfect solution is to truncate the iterations: 
mixed h_mcamsis [fw=freq] ||w_pcs4:, mle iterate(6) 
est store m1cn



/*
* Aside: With xtreg, which uses difference convergence thresholds, the same models 
* converge (though only in the large microdata format version of the data
*   as random effects xtreg models don't allow weights)

sav "$path9\temp.dta"", replace
keep h_mcamsis h_degree w_pcs freq 
sum
expand freq
sum  /* i.e. we have converted the data from 'table format' to microdata */

xtreg h_mcamsis  , i(w_pcs4) re
est store m1e

xtreg h_mcamsis h_degree  [fw=freq] , i(w_pcs4) re 
est store m1f

use "$path9\temp.dta"", clear /* returns to the original table format data */
*/


* It is possible to alter various different convergence criteria 
*   in the mixed algorithm: at the em stage (finding starting values); 
*   and in the main gradient based estimation stage; 
*    after some trail and error for this application, I found that the 
*   following seemed to give satisfactory solutions : 

mixed h_mcamsis [fw=freq]  ||w_pcs4:, ///
   mle emtolerance(1) tolerance(0.0001) ltolerance(0.0001)
est store m1g

* (Explanation: by my understanding, loosening the em tolerance meant the
*   important second stage estimations started from a different point and 
*    (perhaps by chance) this led to their smoother convergence in the main model) 


mixed h_mcamsis h_degree [fw=freq] ||w_pcs4:, ///
   mle emtolerance(0.0001) tolerance(0.0001) ltolerance(0.5)
est store m1h

** (Explanation: This is a more conventional route to convergence, just loosening
*    the tolerance for the final log-likelihood stage to a much larger value 
*    than its default)


** Comment: this analysis is for a very large sample size; the larger than 
*    usual convergence thresholds seem a reasonable response to that quality


*****************************************************************







******************************************************
** c) A look at models that struggle to estimate due to large N 


***********
** First define shortcut programmes to be used in examples below


** Programme for running a linear outcomes mixed model in Stata and collecting timing and other summary data via estout
capture program drop mlm_1 
program define mlm_1
 timer clear
 timer on 1
 ${mixed_model} 
 timer off 1 
 timer list 1
 estadd scalar time=r(t1) 
 estadd scalar N_gps=e(N_g)[1,1]
 estadd scalar gp_max_N=e(g_max)[1,1]
 estadd scalar gp_aver=e(g_avg)[1,1]
 estadd scalar gp_min_N=e(g_min)[1,1]
 estat icc
 estadd scalar icc=r(icc2)/(e(k_r)==2) 
 quietly summarize
 estadd scalar N_file=r(N) 
end  

**

** Programme for running a binary outcomes mixed model in Stata and collecting timing and other summary data via estout
capture program drop mlm_3 
program define mlm_3
 timer clear
 timer on 1
 ${mixed_model} 
 timer off 1 
 timer list 1
 estadd scalar time=r(t1) 
 estadd scalar N_gps=e(N_g)[1,1]
 estadd scalar gp_max_N=e(g_max)[1,1]
 estadd scalar gp_aver=e(g_avg)[1,1]
 estadd scalar gp_min_N=e(g_min)[1,1]
 estat icc
 estadd scalar icc=r(icc2)/(e(k_r)==1) 
 quietly summarize
 estadd scalar N_file=r(N) 
end  



*******
*** Example: GCSE attainment models that are big and small and easy and hard

* Preliminary data prep 
use "$gcse_data", clear
label variable gcse "GCSE score (Z score*10)"
label variable lrt "London Reading test score (Z score*10)"
label variable girl "Female"
label variable schgen "Type of school"
capture label drop schgenl 
label define schgenl 1 "Mixed gender" 2 "Boys only" 3 "Girls only"
label values schgen schgenl
* (Labels from Rabe-Hesketh and Skrondal, 2008: 141)
capture drop sch_*
tab schgen, gen(sch_)
tab1 schgen sch_*
egen schlmean=mean(lrt), by(school)
gen lrtsclm=lrt*schlmean 
codebook school student gcse lrt girl schlmean lrtsclm schlmean lrtsclm , compact
sav "$path9\gcse_basic.dta", replace
****************
* Used hereafter:  
de using "$path9\gcse_basic.dta"
dir "$path9\gcse_basic.dta"
use "$path9\gcse_basic.dta", clear
codebook, compact

***** A model that is small and easy: 
* Rabe-Hesketh & Skrondal amongst others show this model with random intercepts and one random slope: 
mixed gcse lrt girl schlmean lrtsclm ||school:lrt, cov(un) mle 
est store gcse1 

* E.g. Plot the model regression line and the school level lines based on school level residuals: 
capture drop ebi 
capture drop esi
est restore gcse1 
est
predict ebs ebi, reffects
capture drop p4
gen p4 = _b[_cons]   ///
    + ebi + (_b[lrt]+ebs)*lrt  /* Empirical bayes adjusted linear predictor */
capture drop p4l
gen p4l = _b[_cons] + _b[lrt]*lrt   /* Linear predictor */
sort school lrt
graph twoway (scatter gcse lrt, mcolor(gs10) msymbol(smcircle) msize(vsmall) ) /// 
    (line p4 lrt, connect(ascending)  lwidth(thin) lcolor(gs4) lpattern(solid) ) ///
    (line p4l lrt, sort  lwidth(thick) lcolor(orange) lpattern(solid) )  ///
     , xtitle("LRT score") ytitle("GCSE") title("GCSE aged 16 by reading test age 11") ///
    legend(cols(2) order(2 3) label(2 "School level residuals") label(3 "Overall")  )

** Four small models, run making use of the summary programmes that are defined above (3 converge easily, 1 is harder)
global mixed_model "mixed gcse lrt girl schlmean lrtsclm ||school:, cov(un) mle iterate(30) "
mlm_1
est store gcse1 /* linear outcomes random intercept */

global mixed_model "mixed gcse lrt girl schlmean lrtsclm ||school:lrt, cov(un) mle iterate(30)"
mlm_1
est store gcse2  /* linear outcomes random slope */

sum gcse, detail
capture drop hi_gcse
gen hi_gcse=(gcse >= 6.8)
version 15: table hi_gcse, c(min gcse mean gcse max gcse n gcse) 

global mixed_model "melogit hi_gcse lrt girl schlmean lrtsclm ||school:, cov(un) intp(7) iterate(30) "
mlm_3
est store hi_gcse1 

global mixed_model "melogit hi_gcse lrt girl schlmean lrtsclm ||school:lrt, cov(un) intp(7) iterate(30) "
mlm_3
est store hi_gcse2 

est table gcse1 gcse2 hi_gcse1 hi_gcse2, stats(N_gps N  gp_aver gp_max_N ll icc converged time) b(%8.4g) star eq(1)


**** What about if the data got a lot bigger...?

use "$path9\gcse_basic.dta", clear
capture drop hi_gcse
gen hi_gcse=(gcse >= 6.8)
sort school student
isid school student
gen id1=_n
sav "$path9\m1.dta", replace  /* 4059 cases */
clear 
set obs 4059000
gen id1 = _n  - ((floor((_n-1)/4059))*4059)
gen rep1 = 1 + floor((_n-1)/4059)
sum id1 
sort id1
merge m:1 id1 using "$path9\m1.dta"
tab _merge
drop _merge
tab school 
sum rep1 
capture drop school2
gen school2=school*100000 + rep1 
codebook school school2 , compact
sav "$path9\temp1.dta", replace


** Linear outcomes, random intercepts comparisons 

use "$path9\gcse_basic.dta", clear
global mixed_model "mixed gcse lrt girl schlmean lrtsclm ||school:,  mle iterate(30) "
mlm_1
est store gcse1 /* Just the original data and model */

use "$path9\temp1.dta", clear
global mixed_model "mixed gcse lrt girl schlmean lrtsclm  if rep1==1 ||school:,  mle iterate(30) "
mlm_1
est store gcse1_1f /* The original data and model, but part of a bigger dataset */
 
use "$path9\temp1.dta", clear
keep if rep1 >=1 & rep <= 10
global mixed_model "mixed gcse lrt girl schlmean lrtsclm   ||school:,  mle iterate(30) "
mlm_1
est store gcse1_10 /* The original k, but n upscaled by 10 (i.e. larger group size) */

use "$path9\temp1.dta", clear
keep if rep1 >=1 & rep <= 10
global mixed_model "mixed gcse lrt girl schlmean lrtsclm   ||school2:,  mle iterate(30) "
mlm_1
est store gcse1_10s /* k and n both upscaled by 10 */

use "$path9\temp1.dta", clear
global mixed_model "mixed gcse lrt girl schlmean lrtsclm  if rep1 >=1 & rep <= 10  ||school2:,  mle iterate(30) "
mlm_1
est store gcse1_10f /* k and n both upscaled by 10, but part of a bigger dataset  */

use "$path9\temp1.dta", clear
keep if rep1 >=1 & rep <= 100
global mixed_model "mixed gcse lrt girl schlmean lrtsclm   ||school:,  mle iterate(30) "
mlm_1
est store gcse1_100 /* The original k, but n upscaled by 100 (i.e. larger group size) */

use "$path9\temp1.dta", clear
keep if rep1 >=1 & rep <= 100
global mixed_model "mixed gcse lrt girl schlmean lrtsclm   ||school2:,  mle iterate(30) "
mlm_1
est store gcse1_100s /* k and n both upscaled by 100 */


est table gcse1 gcse1_1f gcse1_10 gcse1_10s gcse1_10f gcse1_100 gcse1_100s , ///
      stats(gp_aver gp_max_N ll icc converged time N_gps N  N_file) b(%9.4g) star eq(1)


/* Observe considerable differences in estimation time for bigger datasets */
/* Larger versus smaller average group size doesn't seem to influence time (but will influence results) */
/* Observe that the size of dataset effects estimation time even if model applies only to a small subset of it! */


/*
* Only if you can spare 5 mins: 
use "$path9\temp1.dta", clear
keep if rep1 >=1 & rep <= 1000
global mixed_model "mixed gcse lrt girl schlmean lrtsclm   ||school2:,  mle iterate(15) "
mlm_1
est store gcse1_1000s
est table gcse1 gcse1_10s gcse1_100s gcse1_1000s , stats(gp_aver gp_max_N ll icc converged time N_gps N  N_file) b(%9.4g) star eq(1)
*/


** Some examples of binary outcomes models, with random slopes: 

use "$path9\temp1.dta", clear
keep if rep1 ==1 
global mixed_model "melogit hi_gcse lrt girl schlmean lrtsclm ||school:lrt, cov(un) intp(7) iterate(30) "
mlm_3
est store hi_gcse2 /* original n and k */

use "$path9\temp1.dta", clear
global mixed_model "melogit hi_gcse lrt girl schlmean lrtsclm if rep==1 ||school:lrt, cov(un) intp(7) iterate(30) "
mlm_3
est store hi_gcse2_1f /* original n and k, but part of a larger dataset  */

use "$path9\temp1.dta", clear
keep if rep1 >=1 & rep <= 10
global mixed_model "melogit hi_gcse lrt girl schlmean lrtsclm ||school:lrt, cov(un) intp(7) iterate(30) "
mlm_3
est store hi_gcse2_10 /* original k, n upscaled by 10 */

use "$path9\temp1.dta", clear
keep if rep1 >=1 & rep <= 10
global mixed_model "melogit hi_gcse lrt girl schlmean lrtsclm ||school2:lrt, cov(un) intp(7) iterate(30) "
mlm_3
est store hi_gcse2_10s /* n and k both upscaled by 10 */

est table hi_gcse2 hi_gcse2_1f hi_gcse2_10 hi_gcse2_10s , ///
    stats(gp_aver gp_max_N ll icc converged time N_gps N  N_file) b(%9.4g) star eq(1)

** More pronounced delays in estimating some of these non-linear outcoms models 
*   (remember, we know in advance that the model underneath is very sensible, and well-identified, for the data itself)


*


***** Another example: Scenario with very large groups (low k, high k-bar) 

de using "$ess_extract1"
use "$ess_extract1", clear
tab1 year cid happy 
gen misery=(happy >= 0 & happy <= 1) if (happy >= 0 & happy <= 10) 
label variable misery "Very very unhappy"
gen paid_work=(mnactic==1 | mnactic==7) 
capture drop rand_var
set seed 4567
gen rand_var=rnormal(0,1)
capture drop rural
gen rural=(domicil==4 | domicil==5) if (domicil >= 1 & domicil <= 5) 
gen cen_age=age - 40 
gen cen_age2=age^2 - 40^2
summarize misery happy  fem cen_age cen_age2 eduyrs2 paid_work  rural cid rand_var 
tab cntry 
regress misery happy  fem cen_age cen_age2 eduyrs2 paid_work  rural cid rand_var
keep if e(sample)==1
summarize
saveold "$path9\ess_temp.dta", replace version(12)
***

use "$path9\ess_temp.dta", clear

tab cid misery, row

regress misery fem cen_age cen_age2 eduyrs2 paid_work  rural 
est store lin_prob1

logit misery fem cen_age cen_age2 eduyrs2 paid_work  rural 
est store logit1


/*
** This model would make good sense, but by brute force it won't fit in Stata
global mixed_model "melogit misery fem cen_age cen_age2 eduyrs2 paid_work rural ||cid:,  intp(7) iterate(10)"
mlm_3
est store misery1 
*/

******* Example of some alternatives..?

** No random effects...
use "$path9\ess_temp.dta", clear

logit misery fem cen_age cen_age2 eduyrs2 paid_work  rural , robust cluster(cid) 
est store logit1r

** Just use a subsample? 

use "$path9\ess_temp.dta", clear
tab misery 
keep if rand_var <= -2
tab misery
global mixed_model "melogit misery fem cen_age cen_age2 eduyrs2 paid_work rural ||cid:,  intp(7) iterate(20)"
mlm_3
est store misery1_sub1 


** 'Nursed' random effects, using starting values from a smaller sample and laplace estimation (intp=1)....: 

est restore misery1_sub1
matrix define start1=e(b)

use "$path9\ess_temp.dta", clear
global mixed_model "melogit misery fem cen_age cen_age2 eduyrs2 paid_work rural ||cid:,  intmethod(mc) intp(1) iterate(20) from(start1)"
mlm_3
est store mis_st1_lap 


** A version of nursing that ought to work but doesn't....: 
* (Takes ages to not converge - skip it if you're short of time)

est restore misery1_sub1
est 
matrix define start1=e(b)
use "$path9\ess_temp.dta", clear
matrix list start1
global mixed_model "melogit misery fem cen_age cen_age2 eduyrs2 paid_work rural ||cid:,  intp(7) iterate(10) from(start1)"
mlm_3
est store mis_st1_i7 



** ....Alternative nursing - refining the starting values plus weakening the tolerance threshold

use "$path9\ess_temp.dta", clear
keep if rand_var <= -1
tab misery
global mixed_model "melogit misery fem cen_age cen_age2 eduyrs2 paid_work rural ||cid:,  intp(7) iterate(20) "
mlm_3
est store misery1_sub2 
matrix define start2=e(b) 

use "$path9\ess_temp.dta", clear
global mixed_model "melogit misery fem cen_age cen_age2 eduyrs2 paid_work rural ||cid:,  intmethod(mc) intp(5) iterate(20) from(start2) tolerance(0.001) ltolerance(0.0001) showtolerance technique(nr) "
mlm_3
est store mis_st2i5t1 


*****

est table lin_prob1 logit1 logit1r misery1_sub1 mis_st1_lap mis_st1_i7 mis_st2i5t1  , ///
      stats(gp_aver gp_max_N ll icc converged time N_gps N  N_file) b(%9.4g) star eq(1)

**************************************************************







***********************************************
*** d) A look at specifying cross-classified models in Stata


***

* Example using the primary/secondary school dataset that is also described in 
*   Hox 2010: p173ff and is comparable to the data used in Rabe-Hesketh and Skrondal 2012: p443ff


use "$pupcross_data", clear /* Hox version of the dataset */
summarize

histogram achiev /* outcome */
codebook achiev pschool sschool, compact /* 1000 pupils in 50 primary schools and 30 sec schools */

graph twoway (scatter   sschool pschool , mcolor(teal*0.5%30) msymbol(square) msize(medium) ) ///
  , ylabel(1(1)30,grid nolabel) ytitle("Secondary" "school", orientation(horizontal)) scheme(s1mono)
* The point is that pupil are nested into both secondary and primary schools, however, 
*   it is not a neatly hierarchical nesting but rather a 'cross-classified' structure (secondary schools 
*    tend to take pupils form different primary schools on average, but pupils from the same primary often 
*    get spread out across more than one secondary school



* Two two-level models:
mixed achiev ||pschool:, mle
est store m2a

mixed achiev ||sschool:, mle
est store m2b


* If run as a three-level model, the analysis would treat as hierarchical and so wouldn't recognise shared 2nd level variance correctly 
mixed achiev ||pschool: , ||sschool:, mle variance
est store m3a 
mixed achiev ||sschool:, ||pschool: , mle variance
est store m3b 
est table m2a m2b m3a m3b , stats(N ll bic) /* e.g. m3b is actually worse than m2a because it misses some primary school level variance */

* Here is the model with correct, cross-classified variance partitioning: 
mixed achiev ||_all:R.sschool, cov(identity) ||pschool: , mle variance
est store m3c 

* (A specific code extension is required here in Stata, ("_all:R.[id]") to get this to work appropriately - it is a 
*    sort of coding trick, modelling the secondary schools via dummy indicators as random slopes that are constrataided to be equal)



* This dataset was also famously used to illustrate interactions between the two units by allowing for a four level random effects system:

capture drop pbys
egen pbys=group(pschool sschool) /* Unique value for each combination */
codebook pschool sschool pbys, compact
mixed achiev ||_all:R.sschool, cov(identity) ||pschool: , ||pbys:,  mle variance

** Level 4 : other students at the same secondary school as you
** Level 3 : other students at the same primary school as you
** Level 2 : the group of students who went to both the same secondary and primary school as you
** Level 1 : Just you

* cf. ignoring the 'closest students' level, we get a slightly less apropriate model 
mixed achiev ||_all:R.sschool, cov(identity) ||pschool: , mle variance


************************************






**********************************************************




**********************************************************
**#  EOF   
********************



