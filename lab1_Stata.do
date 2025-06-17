
***************************************
**# Title and contents

**** SGSSS Summer School 2025

** Short course: Multilevel Models for Applied Social Research

** Materials by Paul Lambert and Kate O'Hara (Univ. Stirling)

***  LAB 1 - STATA EXERCISES - 'SELECTED POPULAR MODELS AND FUNCTIONS FOR MULTILEVEL MODELLING IN APPLIED SOCIAL RESEARCH'

***   Linked code, data and metadata: at https://github.com/paul-lambert/SGSSS-2025/

**    Version: 16/Jun/2025, written for Stata v17 



***************************************


******************************
**** Contents listing

***   Title and contents 
***   Paths and preliminaries
***   Ex 1.1: Popular random effects models on the London Reading Test datasest 
***   Ex 1.2: Popular procedures when working with a multilevel dataset
***   Ex 1.3: Illustrating higher level residuals using the London Reading Test dataset
***   Ex 1.4: Illustrating random effects models for linear and non-linear outcomes 
***   Ex 1.5: Illustrating random effects models with more than 2 levels  


********************************






*********************************************************
**#  Paths and preliminaries
********************************


*** Sets alias names for specific files (edit as required to correspond to your own machine): 

global gcse_data "C:\1\sgsss-2025\data\gcse.dta"
** Used in Ex 1.1-3
** The 'London Reading Test' dataset  
** You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
** Provenance: This dataset is used in many training materails on multilevel modelling in the UK. It was used in Harvey Goldsteins influential 
*     book 'Multilevel Statistical Models'. This version of the dataset was accessed from the website for Rabe-Hesketh & Skrondal's textbook 'Multilevel
*     and Longitudinal Modeling using Stata'


global census_2011 "C:\data\census_uk\2011\scotland_teaching_8002\censusmicroteaching11s.dta"
** Used in Ex 1.4-5
** You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
** You can access this data file directly from the github location for this course 
** Provenance: This dataset is an anonymised extract of selected cases form the Scottish 2011 census, it is available as UK Data Service 
**   study number 8002, for more information see https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8002 


global nurses_data "C:\1\sgsss-2025\data\nurses.dta" 
** Used in Ex 1.5
** You can access this data file directly from the github address, or download it to your machine and change the location accordingly 
** You can access this data file directly from the github location for this course
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
/* If necessary, run the following:
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











*********************************************************
**#  Ex 1.1: Popular random effects models on the London Reading Test datasest
********************************



*  The London Reading Test data used by Goldstein et al 
*           (the Rabe-hesketh & Skrondal 2008 version of it)


use "$gcse_data", clear

** This is a sample based dataset which has been very widely used to illustrate examples 
*   of multilevel models (e.g. in Goldstein et al 2003; Rasbash et al 2009) 


** Summarising the multilevel structure: A hierarchy of pupils in schools
codebook school student, compact
*  'school' is an indicator variable with a different vlaue for each school: we see there are 65 different schools in the data
sort school student
list school student in 1/90, sepby(school)
* 'student' is an indicator code for each student within a school - the combination of student and school uniquely identifies every case

* In Stata, 'svy' can also be used as a convenient tool for describing complex or multilevel datasets: 
svyset, clear
svyset school /* treat 'svy' commands as for a dataset with Primary Sampling Units defined according to units of 'school' */
svydes /* => Describes the PSU structure, e.g. there's 4059 pupils, nested in 65 schools, between 2 and 198 students per school */


** Summarising the outcome and explanatory variables: 
codebook gcse lrt girl schgen , compact 
*  (external documentation tells me that the following labels are appropriate - e.g. Rabe-Hesketh and Skrondal, 2010: 141)
label variable gcse "GCSE score (Z score*10)"
label variable lrt "London Reading test score (Z score*10)"
label variable girl "Female"
label variable schgen "Type of school"
capture label drop schgenl 
label define schgenl 1 "Mixed gender" 2 "Boys only" 3 "Girls only"
label values schgen schgenl
capture drop sch_*
tab schgen, gen(sch_) /* makes dummy var indicators for school gender */
tab1 schgen sch_*
codebook school student   gcse lrt girl schgen sch_2 sch_3  , compact /* i.e. variables in a format that will be suited to modelling */

graph twoway (scatter gcse lrt, mcolor(teal*0.7%30)), scheme(s1mono) name(des1, replace) 
* Suggests strong gsce-lrt association
graph hbar (mean) gcse lrt, over(schgen) scheme(s1mono) name(des2, replace) legend(order(1 2) label(1 "GSSE score (age 16)") label(2 "LRT score (age 11)")) 
* Suggests there will also be a GCSE pattern linked to the school's gender 


** A conventional single-level model: multiple regression with GCSE as the outcome
regress gcse lrt girl sch_2 sch_3
* Typical interpretations: 
*    About 36% of variation in GCSE at 16 is explained by variation in own gender, school gender, and in age 11 reading score 
*    On average, net of other factors, an increase of 1 unit in age 11 reading score links to an increase of 0.6 GCSE points
*    On average, net of other factors, females have GCSE scores that are 1.3 units higher 
*    On average, net of other factors, people at Boys-only schools have GCSE scores 1.8 units higher than those at mixed schools
*    On average, net of other factors, people at Girls-only schools have GCSE scores 1.7 units higher than those at mixed schools


* e.g. A visualisation of what this model tells us could be...
graph twoway (scatter gcse lrt, mcolor(teal*0.6%30)) ///
    (function y = _b[_cons] + _b[lrt]*x, range(-40 40) lcolor(blue*0.5%50) lwidth(thick)) ///
    (function y = _b[_cons] + _b[lrt]*x + _b[girl] + _b[sch_3], range(-40 40) lcolor(purple*0.8%50) lwidth(thick)), ///
    scheme(s1mono) name(reg1, replace) xtitle("Reading score aged 11") ytitle("GCSE" "at age" "16", orientation(horizontal)) ///
    legend(order(2 3) label(2 "Boys, in a mixed school") label(3 "Girls, in a single-sex school") cols(1) pos(11) ring(0) )  




*** Hitherto, we've ignored the multilevel data structure in the regression above, but here are some multilevel models 
*     with random effects design to take some account of 


*** (a) Random intercepts model with explanatory variables using default Stata estimation settings
mixed gcse lrt girl sch_2 sch_3 ||school:, 

** Typical conclusions would be: 
*     - There is something important at the school level net of the fixed effects story (evident in the improvement in model 
*        fit in this model compared to the single level model (='significant' random intercept variance component of 8.11)
*     - Appropriately modelling the random intercept pattern gives us slightly different regression coefficients and standard
*       errors; as the model is a better model, these are more appropriate esimtates 
*       (e.g. consequentially, we are no longer so sure that school gender is distinctively linked to GCSE profile net of other
*        factors, because both dummy variables are no longer statistically significant) 

** In Stata, postestimation command 'estat' can add helpful info: 
estat icc
*    Net of the fixed part of the model, about 13% of variation in GCSE's is linked to overall variation from school to school 

mixed gcse lrt girl sch_2 sch_3 ||school:, stddev
* In standard deviation units, the school-to-school variation is characterised by a standard deviation of 2.85 around the intercept 
*    of -1.68 , i.e., from school to school our model suggests the intercept ranges around it's average of -1.7, such 
*    that most school have an intercept somewhere between -7.4 to 4.0 



*** (b) Random intercepts null model 

mixed gcse  ||school:, 
estat icc

** The null model with school level random effects ften makes a helpful comparison point: without any other controls, about 
*     17% of the GCSE variation is to do with school to school differences (with control variables is was about 13%)



*** (c) Example of a random slopes model

mixed gcse lrt girl sch_2 sch_3 ||school:lrt, cov(un) 

* In this specification of the model, we all that the beta coefficient of age 11 score might also vary from school to 
*    school, possibly with a systematic relationship to the school-to-school variation pattern in the intercept




* Conventional interpretation: 

*      - There is evidence of random slopes here because this model is a better fit to the data than the random intercepts version 
* (for details): 
mixed gcse lrt girl sch_2 sch_3 ||school:, 
est store ri 
mixed gcse lrt girl sch_2 sch_3 ||school:lrt, cov(un) 
est store rs 
lrtest ri rs /* likelihood ratio test favours the random slopes model */


*     - The variation in the slope can be characterised by a variance of 0.015 (sd = 0.12), implying that whilst the average 
*        lrt slope value is 0.55, from school to school the magnitude of that slope varies, in most instances falling with 
*        the range 0.31-0.79

mixed gcse lrt girl sch_2 sch_3 ||school:lrt, cov(un) stddev


*     - There's a positive correlation between the value of the slope random effect and the value of the intercept random effect, 
*         (r=0.59), i.e. when schools tend to have higher intercepts they also have higher LRT slopes (prior attainment matters more), 
*         a pattern that's sometimes described as 'fanning out'



**********************************************************************************







 


**********************************************************************************
**#   Ex 1.2: Popular procedures when working with a multilevel dataset
**********************



** The first lines repeat the start of Ex 1.1

use "$gcse_data", clear

** This is a sample based dataset which has been very widely used to illustrate examples 
*   of multilevel models (e.g. in Goldstein et al 2003; Rasbash et al 2009) 

** Summarising the multilevel structure: A hierarchy of pupils in schools
codebook school student, compact
*  'school' is an indicator variable with a different vlaue for each school: we see there are 65 different schools in the data
sort school student
list school student in 1/90, sepby(school)
* 'student' is an indicator code for each student within a school - the combination of student and school uniquely identifies every case

* In Stata, 'svy' can also be used as a convenient tool for describing complex or multilevel datasets: 
svyset, clear
svyset school /* treat 'svy' commands as for a dataset with Primary Sampling Units defined according to units of 'school' */
svydes /* => Describes the PSU structure, e.g. there's 4059 pupils, nested in 65 schools, between 2 and 198 students per school */


** Summarising the outcome and explanatory variables: 
codebook gcse lrt girl schgen , compact 
*  (external documentation tells me that the following labels are appropriate - e.g. Rabe-Hesketh and Skrondal, 2010: 141)
label variable gcse "GCSE score (Z score*10)"
label variable lrt "London Reading test score (Z score*10)"
label variable girl "Female"
label variable schgen "Type of school"
capture label drop schgenl 
label define schgenl 1 "Mixed gender" 2 "Boys only" 3 "Girls only"
label values schgen schgenl
capture drop sch_*
tab schgen, gen(sch_) /* makes dummy var indicators for school gender */
tab1 schgen sch_*
codebook school student   gcse lrt girl schgen sch_2 sch_3  , compact /* i.e. variables in a format that will be suited to modelling */


***
*** Here's some examples of other things we often want to do with multilevel datasets


***** i) Count up the cases in different clusters
capture drop n_2
egen n_2=count(student), by(school) /* New var with number from school in it */
label variable n_2 "Number of level 1 units per level 2 unit"
list school student n_2 in 1/90, sepby(school)



**** ii) Make and use a variable that only selects one case per cluster
capture drop first_2
egen first_2=tag(school) /* Makes an indicator valued 1 for only one student per school (the first one in the current sort order) */
tab first_2
list school student n_2 first_2 in 1/90, sepby(school)
* With that indicator, we can readily summarise relevant things at the higher rather than lower level
sum n_2 /* this summary is at the student level but it is school level data */
sum n_2 if first_2==1 /* this data is usually more appropriate: the average sample size of students per school is 62.4 */
histogram n_2 if first_2==1, frequency title("Distribtuion of school sizes in the GCSE data")


**** iii) Derive cluster level summary measures and report or analyse them

* E.g. analysis of cluster level summaries of the outcome
capture drop s_gcse_mean
egen s_gcse_mean=mean(gcse), by(school) 
label variable s_gcse_mean "School GCSE average"

histogram s_gcse_mean if first_2==1 , title("Distribution of school level GCSE averages") 
histogram s_gcse_mean if first_2==1 ,  by(schgend, cols(1))  

capture drop s_gcse_sd
egen s_gcse_sd=sd(gcse), by(school) 
sum s_gcse_mean s_gcse_sd n_2 if first_2==1 
correlate s_gcse_mean s_gcse_sd if first_2==1
graph twoway (scatter s_gcse_mean s_gcse_sd [fw=n_2] if first_2==1, msize(small) mlcolor(navy) mcolor(purple*0.7%50)) ///
   , scheme(s1mono) xtitle("School standard deviation") ytitle("School mean" , orientation(hor))  
* (A pattern of more variation within schools when school average is higher - which to some extent happens by design) 

* E.g. deriving cluster level average of reading test and using it in further analyses
capture drop s_lrt_mean
egen s_lrt_mean=mean(lrt), by(school)
correlate s_gcse_mean s_lrt_mean if first_2==1 /* at the school level, lrt and gcse correlate at 0.69 */
correlate gcse lrt /* at the student level the correlation is 0.59 */
* (the observation of higher correlation at macro than at microlevel is commonplace in social phenomena, indeed the
*     gap in correlations is often much larger, and is generally considered an example of the ecological fallacy in action)


regress gcse lrt 
regress gcse lrt s_lrt_mean 
* A model like this, with a higher level explanatory variable, is often used to assess school impact on the outcome
* It more or less replates the 'fixed effects' model since the lrt coefficient is strictly with 'within' effect of lrt 

* In the above model, though, the s_lrt_mean variable is being treated as if it had 4059 data points but it only really has 65
*  a simple adjustment is to use robust standard errors to recognise this: 
regress gcse lrt s_lrt_mean  , robust cluster(school)
* (observe we have the same model coefficients, but the school mean coefficient has appropriately larger standard errors)

** Conventional interpretation: 
*     - The 'within effect' of lrt (impact of your lrt compared to your peers at the same school) is to boost GCSE by 0.56 on average
*     - The 'contextual effect' of lrt (impact of school average lrt on indivdual gcse net of own lrt influence) is to boost GCSE by 0.35 on average


* This is jumping ahead, but a more complete way of using the school level mean is to then calculate a deviation-from-mean 
*   lower level measure and fit both, with random effects, giving the 'random effects within between model'
capture drop lrt_dev
gen lrt_dev = lrt - s_lrt_mean 

mixed gcse lrt_dev s_lrt_mean ||school:, 

** Conventional interpretation: 
*     - The 'within effect' of lrt (impact of your lrt compared to your peers at the same school) is to boost GCSE by 0.56 
*     - The 'between effect' of lrt (impact of school average lrt on school average gcse) is to boost GCSE by 0.92

regress s_gcse_mean s_lrt_mean if first_2==1 /* (this is comparable to the 'between effects' model of average x on avearge y) */




*** iv) In Stata specifically, 'svy' and 'xt' tools can be convenient tools for summarising multilevel data

* Using SVY to summarise data structure 
svyset, clear
svyset school /* treat 'svy' commands as for a dataset with Primary Sampling Units defined according to units of 'school' */
svydes /* => Describes the PSU structure, e.g. there's 4059 pupils, nested in 65 schools, between 2 and 198 students per school */

* Using SVY to get more appropriate inferential results noting the cluster structure 
ci means gcse lrt 
regress gcse lrt s_lrt_mean 

svy: mean gcse lrt  /* sample based confidence intervals for a single statistic are widened noting the school structure */
svy: regress gcse lrt s_lrt_mean /* confidence intervals for higher level explanatory variable are appropriately widened (=robust standard errors) */

* 'XT' tools refer to panel data where 'i' is the unit and 't' is the time period within the unit
* This corresponds to two-level multilevel data where i is level 2 and t is level 1
* In a few instances, xt commands can be handy tools for working with multilevel data 
*  (but only if the particular placement of 't' doesn't matter - i.e. which unit you are within a cluster can matter in longitudinal 
*    data structures but it is arbitrary in a multilevel data structure)

xtreg gcse lrt , i(school)  /* with default settings, this gives the random intercepts multilevel model estimated with 'gls' */

xtsum lrt, i(school) /* with xtsum, the between is the profile of school level averages, the within is the variation around school level averages */

* i.e. same as: 
sum lrt lrt_dev
sum s_lrt_mean if first_2==1

xttab girl, i(school) /* with xttab, the between=how many schools of all the schools have either category; the within indicates what percent 
                         of students that have that category are in schools that only have that category */


*******************************************









***************************************************************************
**#   Ex 1.3: Illustrating higher level residuals using the London Reading Test dataset
*******************************************




use "$gcse_data", clear

label variable gcse "GCSE score (Z score*10)"
label variable lrt "London Reading test score (Z score*10)"
label variable girl "Female"
label variable schgen "Type of school"
capture label drop schgenl 
label define schgenl 1 "Mixed gender" 2 "Boys only" 3 "Girls only"
label values schgen schgenl
capture drop sch_*
tab schgen, gen(sch_) /* makes dummy var indicators for school gender */
tab1 schgen sch_*
codebook school student   gcse lrt girl schgen sch_2 sch_3  , compact /* i.e. variables in a format that will be suited to modelling */



** Random intercepts model (here using 'reml' estimation algorithm which broadly works to prioritise random part estimation)
mixed gcse lrt girl sch_2 sch_3 ||school:, reml 
est store ri

** Random slopes example, allowing for school-to-school variation in the LRT effect 
mixed gcse lrt girl sch_2 sch_3 ||school:lrt, reml cov(un) 
est store rs


*** From either model we can general the higher level residuals or 'random effects' which are the 'peturbations' around the 
*     average coefficient that empirically apply to the relevant cluster 


* Start with an indicator for one case per cluster as that will help in presenations: 
capture drop onerec
egen onerec=tag(school) /* just an indicator of one record per class */


******** Random intercepts model:

est restore ri
est /* i.e. shows the model currently in memory */


* Find the 'empirical bayes' random intercept residuals (with 'shrinkage') 
capture drop ebi_p 
predict ebi_p, reffects  /* makes a new variable with higher level residual values , drawing on Stata's postestimation tools */

label variable ebi_p "Emprical bayes residuals for random intercepts"
hist ebi_p if onerec==1 /* assumption checking: the residuals ought to have mean 0 and be normally distributed */

graph twoway (scatter ebi_p school if onerec==1, ///
       msymbol(circle) msize(vlarge) mcolor(purple) ) /* just shows the school level residuals, against the school numeric code */

capture drop cluster_rank
egen cluster_rank=group(ebi_p school)
graph twoway (scatter ebi_p cluster_rank if onerec==1, ///
       msymbol(circle) msize(vlarge) mcolor(green) ) /* slightly easier output: graphs the residuals in rank order */


* It's popular to present the ranked residuals as 'caterpillar plots' with standard errors for the residuals 
*    (e.g. some residuals might be based on relatively few cases)

capture drop cluster_ses
predict cluster_ses, reses /* finds estimated standard errors for the residuals, using Stata postestimation tools */

capture drop upper
gen upper = ebi_p + 1.96*cluster_ses //manually making upper bound for confidence interval
capture drop lower
gen lower = ebi_p - 1.96*cluster_ses //lower bound similarly 

graph twoway (scatter ebi_p  cluster_rank if onerec==1, ///
       msymbol(circle) msize(medium) mcolor(green)  )  ///
     (rspike upper lower cluster_rank if onerec==1, lcolor(gs10) lwidth(thin)) ///
    , scheme(s1mono) yline(0, lcolor(purple) lwidth(medium)) subtitle(" 'Caterpillar plot' for class residuals " )


* This version of the graph uses a little extra code to add school labels in a convenient format: 
capture drop evenclus
gen evenclus=6 + 6*(cluster_rank/2==floor(cluster_rank/2) )
tab evenclus /* just indicates if cluster rank number is odd or even , coded 6 or 12, to be used in graph spec */

graph twoway (scatter ebi_p  cluster_rank if onerec==1, ///
       msymbol(circle) msize(medium) mcolor(green) mlabel(school) mlabsize(vsmall) mlabgap(16) mlabvpos(evenclus) )  ///
     (rspike upper lower cluster_rank if onerec==1, lcolor(gs10) lwidth(thin)) ///
    , scheme(s1mono) yline(0, lcolor(purple%50) lwidth(medium)) yscale(range(-12 12)) subtitle(" 'Caterpillar plot' for class residuals " )

* (i.e., we might want to look up some of the extreme schools at either end of the scale, to see if there is anything measured 
*   about them that we might put into the model )



**********  Random slopes models


*** We can do similar things with random slope residuals, but bear in mind we typically have different sets of residuals (for slopes and intercepts)

est restore rs
est /* i.e. shows the model currently in memory */


* Find the 'empirical bayes' slope and  intercept residuals (with 'shrinkage') 
capture drop ebi_rsi
capture drop ebi_rss 
predict ebi_rss ebi_rsi , reffects  /* makes two new variable h higher level residual values , drawing on Stata's postestimation tools */
*  (Stata functions have nominated sequence order for terms, slopes coming before intercepts)
label variable ebi_rss "Emprical bayes residuals for random slope"
label variable ebi_rsi "Emprical bayes residuals for random intercepts"

hist ebi_rsi if onerec==1 /* assumption checking: the residuals ought to have mean 0 and be normally distributed */
hist ebi_rss if onerec==1 

graph twoway (scatter ebi_rss ebi_rsi if onerec==1) , xtitle("Intercepts") ytitle("Slopes") scheme(s1mono) 
* This graph shows the slopes and intercepts tend to be correlated - corresponding to 'cov(lrt,_cons)' in the model results 
est


** For caterpillar plots: 

capture drop cluster_rank1
egen cluster_rank1=group(ebi_rsi school)

capture drop cluster_rank2
egen cluster_rank2=group(ebi_rss school)

capture drop cluster_ses*
predict cluster_ses2 cluster_ses1, reses /* finds estimated standard errors for the residuals, using Stata postestimation tools */

capture drop upper1
gen upper1 = ebi_rsi + 1.96*cluster_ses1 
capture drop lower1
gen lower1 = ebi_rsi - 1.96*cluster_ses1

capture drop upper2
gen upper2 = ebi_rss + 1.96*cluster_ses2 
capture drop lower2
gen lower2 = ebi_rss - 1.96*cluster_ses2
  
capture drop evenclus1
gen evenclus1=6 + 6*(cluster_rank1/2==floor(cluster_rank1/2) )
capture drop evenclus2
gen evenclus2=6 + 6*(cluster_rank2/2==floor(cluster_rank2/2) )


* (For these graphs, I used a bit of trial and error to optimise 'mlabgap' and 'yscale(range()' for the 'graph combine' output )

graph twoway (scatter ebi_rsi  cluster_rank1 if onerec==1, ///
       msymbol(circle) msize(medium) mcolor(green) mlabel(school) mlabsize(vsmall) mlabgap(8) mlabvpos(evenclus1) mlabcol(gs10) )  ///
     (rspike upper1 lower1 cluster_rank1 if onerec==1, lcolor(gs10) lwidth(thin))     , scheme(s1mono) legend(off)  ///
     yline(0, lcolor(purple%50) lwidth(medium)) yscale(range(-12 12)) xtitle("School id", size(small) color(gs10))  xlabel(none)  subtitle(" Intercepts " )
graph save $path9\g1.gph, replace
graph twoway (scatter ebi_rss  cluster_rank2 if onerec==1, ///
       msymbol(circle) msize(medium) mcolor(green) mlabel(school) mlabsize(vsmall) mlabgap(8) mlabvpos(evenclus2) mlabcol(gs10) )  ///
     (rspike upper2 lower2 cluster_rank2 if onerec==1, lcolor(gs10) lwidth(thin))    , scheme(s1mono) legend(off)  ///
     yline(0, lcolor(purple%50) lwidth(medium)) yscale(range(-0.5 0.5)) xtitle("School id", size(small) color(gs10)) xlabel(none)  subtitle(" Slopes " )
graph save $path9\g2.gph, replace
graph combine $path9\g1.gph $path9\g2.gph, scheme(s1mono) title("Slopes and intercepts from random slopes model") cols(1)

* (typically, the most interesting schools are those with above or below average slopes - i.e. LRT matters more or less than usual at them) 








**** Residuals can also be used to visualise the variation in regression lines allowed for under different models
*     (we just add them to the intercept or slope and redraw the regression line accordingly)


** For example, look at the regression line against the range of lrt values in predicting gcse:  

est restore rs
est 
sum lrt
sum ebi_rsi if school==53
scalar s53i=r(mean)  /* stores school 53's residual on the intercept */
sum ebi_rss if school==53
scalar s53s=r(mean) /* stores the slope adjustment that applies to school 53 only */

graph twoway (scatter gcse lrt, mcolor(teal*0.3%50)) ///
     (scatter gcse lrt if school==53, msymbol(x) mcolor(teal*1.5%80)) ///
    (function y=_b[_cons] + _b[lrt]*x , range(-30 30) lcolor(green) lwidth(thick)) ///
    (function y=_b[_cons] + s53i  + (_b[lrt]+s53s)*x , range(-30 30) lcolor(brown*0.8) lwidth(thick)) ///
   (lfit gcse lrt if school==53, lcolor(gs7) lwidth(thin) )  , ///
   scheme(s1mono) legend(order(3 4 5) label(3 "Global average") label(4 "School 53 only (from the model)") label(5 "School 53 (line of direct fit)" ) holes(3) )

** The line for school 53 shows both how it diverges from the global pattern, but also how it is 'shrunk' a little 
*     towards the global average compared to its empirical line of best fit 




*******
*     The code below also visualises intercepts and slopes as adjustments to regression lines -  
*        it reproduces the graphic that was used in the lecture session (for simplicity, for the bivariate model)
*        (code here originates from a comparable example given by Rabe-Hesketh and Skrondal 2008, pp141ff)


** Graphing a single line across whole sample, ignoring clustering:
regress gcse lrt
capture drop p_gcse
predict p_gcse, xb
graph twoway (scatter gcse lrt, mcolor(gs10%50) msymbol(smcircle) ) (line p_gcse lrt, sort lwidth(thick) ) ///
     , xtitle("LRT") ytitle("GCSE" " ", orientation(horizontal)) title("Overall regression") legend(off)
graph save "$path9\bit1.gph", replace

** Graphing lots of lines, for school specific regressions: 
capture drop num
egen num=count(gcse), by(school) /* Tells us how many records per school */
statsby inter=_b[_cons] slope=_b[lrt], by(school) saving("$path9\ols.dta", replace): ///
    regress gcse lrt if num >= 5
* (Runs a regression for each school with 5 or more respondents; stores the regression results in ols.dta)
sort school
merge school using "$path9\ols.dta" /* brings the regression results back into the dataset  */
tab _merge
drop _merge
summarize /* The new variables slope and intercept are school-by-school estimates for the larger schools */
capture drop p2_gcse
gen p2_gcse = inter + slope*lrt
sort school lrt
graph twoway  (scatter gcse lrt, mcolor(gs10%50) msymbol(smcircle) msize(vsmall) )  ///
       (line p2_gcse lrt, connect(ascending))  ,  legend (off) ///
      xtitle("LRT") ytitle("GCSE" " ", orientation(horizontal)) title("School lines (if n >= 5)") 
graph save "$path9\bit2.gph", replace


** Graphing the random intercepts overall model plus the school specific lines that it allows for: 
mixed gcse lrt  ||school:, reml
capture drop lrt_pred
gen lrt_pred= _b[_cons]  + _b[lrt]*lrt /* global lrt_based prediction */
capture drop ebi_p
predict ebi_p, reffects /* The 'Empirical Bayes Estimates' of school level intercept residuals */
capture drop lrt_adj
gen lrt_adj = lrt_pred + ebi_p  /* school specific intercept adjustment to LRT-based prediction  */
codebook school ebi_p lrt_adj, compact
capture drop srank
egen srank=group(ebi_p school) /* important to sort by rank to ensure no connected lines between schools */
gsort -srank
graph twoway (scatter gcse lrt, mcolor(navy*0.4%30) msize(small) msymbol(smcircle) jitter(1)) ///
     (line lrt_adj lrt, connect(ascending)  lwidth(thin) lcolor(gs8) lpattern(solid) ) ///
    (line lrt_pred lrt, sort  lwidth(thick) lcolor(orange) lpattern(solid) )  ///
    ,  legend(order(3 2) label(3 "Overall") label(2 "School-specific lines") col(1) pos(6) ring(1)) ///
   ylabel(#4) xlabel(none) ytitle("GCSE " "score " "(centred) ", orientation(horizontal) ) xtitle("LRT") ///
   title("Random intercepts model", span) 
graph save "$path9\bit3.gph", replace

** Graphing the random slopes overall model plus the school specific lines that it allows for: 
mixed gcse lrt  ||school:lrt, reml
capture drop lrt_pred2
gen lrt_pred2= _b[_cons]  + _b[lrt]*lrt /* global lrt_based prediction */
capture drop ebi_p2
capture drop ebi_s
predict ebi_s ebi_p2, reffects /* The 'Empirical Bayes Estimates' of school level slope and intercept residuals */
capture drop lrt_adj2
gen lrt_adj2 = (_b[_cons] + ebi_p2) + (_b[lrt] + ebi_s)*lrt    /* school specific intercept adjustment to LRT-based prediction  */
codebook school ebi_p2 lrt_adj2, compact
capture drop srank
egen srank=group(ebi_p2 school) /* important to sort by rank to ensure no connected lines between schools */
gsort -srank
graph twoway (scatter gcse lrt, mcolor(navy*0.4%30) msize(small) msymbol(smcircle) jitter(1)) ///
     (line lrt_adj2 lrt, connect(ascending)  lwidth(thin) lcolor(gs8) lpattern(solid) ) ///
    (line lrt_pred2 lrt, sort  lwidth(thick) lcolor(orange) lpattern(solid) )  ///
    ,  legend(order(3 2) label(3 "Overall") label(2 "School-specific lines") col(1) pos(6) ring(1)) ///
   ylabel(#4) xlabel(none) ytitle("GCSE " "score " "(centred) ", orientation(horizontal) ) xtitle("LRT") ///
   title("Random slopes model", span) 
graph save "$path9\bit4.gph", replace

** Bringing the four images together
graph combine "$path9\bit1.gph" "$path9\bit2.gph" "$path9\bit3.gph" "$path9\bit4.gph", cols(2)

******************






*************************************************************







*************************************************************
**#   Ex 1.4: Illustrating random effects models for linear and non-linear outcomes 
********************************************




**** (i) Some examples of binary outcomes models using the GCSE dataset



use "$gcse_data", clear

label variable gcse "GCSE score (Z score*10)"
label variable lrt "London Reading test score (Z score*10)"
label variable girl "Female"
label variable schgen "Type of school"
capture label drop schgenl 
label define schgenl 1 "Mixed gender" 2 "Boys only" 3 "Girls only"
label values schgen schgenl
capture drop sch_*
tab schgen, gen(sch_) /* makes dummy var indicators for school gender */
tab1 schgen sch_*
codebook school student   gcse lrt girl schgen sch_2 sch_3  , compact /* i.e. variables in a format that will be suited to modelling */

** For illustrative purposes, make some derived non-linear outcomes
sum gcse
gen hi_gcse = (gcse >= 10) 
label variable hi_gcse "GCSE is more than 1SD above average" /* binary outcome */



** Summary: A wide range of binary outcome models are available in Stata
*     for random effects multilevel models (since Stata v13).



* a) E.g. the random intercepts binary logit (through various different routines):

melogit  hi_gcse lrt sch_2 sch_3 ||school: ,  /* Default of the 'mixed models' */
* For the icc here: 
di "ICC is: " (0.9780233) / (0.9780233 + (_pi^2/3)) /* working out the icc by hand */
estat icc  /* getting the icc by using the post-estimation routine */




* b) Estimation routines impact results (usually only in small, but nonetheless confusing, ways): 
melogit  hi_gcse lrt sch_2 sch_3 ||school: ,  intp(7) /* Default setting is 7 'integration points' */
est store melog_7

melogit  hi_gcse lrt sch_2 sch_3 ||school: ,  intp(3) /* Minor differences with 3 intergration points */
est store melog_3

meqrlogit  hi_gcse lrt sch_2 sch_3 ||school: , intp(7)  /* 'meqr' uses the default settings that used to apply in earlier Stata versions */
est store meqrlog_7
xtlogit hi_gcse lrt sch_2 sch_3, i(school) /* xtlogit also defaults to random effects but with different estimation tools */
est store xtlog

est table melog_7 melog_3 meqrlog_7 xtlog, eq(1) stats(N ll bic) b(%10.8g) star
 /* substantively everything is still the same, but minor differences in values */



*** c) Various routines are sometimes used for binary outcomes random effects models in Stata


melogit  hi_gcse lrt sch_2 sch_3 ||school:,  intp(7) 

meglm  hi_gcse lrt sch_2 sch_3 ||school:, family(binomial) link(logit)


xtlogit  hi_gcse lrt sch_2 sch_3, i(school),  re 

xtmelogit  hi_gcse lrt sch_2 sch_3 ||school: , intp(7) /* basically = melogit, using legacy code format */

gllamm hi_gcse lrt sch_2 sch_3, i(school) family(binomial) link(logit)  /* using the extension routine 'gllamm' */


xtprobit hi_gcse lrt sch_2 sch_3, i(school)  /* probit rather than logit transformation fucntion - popular in econ */

meprobit  hi_gcse lrt sch_2 sch_3 ||school:,  intp(7) 




*** d) Some prefer odds ratio format coefficients for non-linear outcomes models; others don't 



melogit  hi_gcse lrt sch_2 sch_3 ||school:,  intp(7) 

melogit  hi_gcse lrt sch_2 sch_3 ||school:,  intp(7) or




***  e)  Random slopes models are specified in much the same way (but can be slow to converge in less optimal situations)

melogit   hi_gcse lrt sch_2 sch_3 ||school:lrt,  intp(7) 




**  f) We can look at higher level residuals in similar ways as we did for linear outcomes:

melogit   hi_gcse lrt sch_2 sch_3 ||school:lrt,  intp(7) 
est store rs_melog

* (The predict command can generate the random effects and standard errors, but it is 
*    important to allow for the right number of terms - here two (slopes and intercepts) 
*    - see the help file , via 'predict': 
help melogit postestimation 
*  which includes the statement: 
/*  "The reffects and reses() options often generate multiple new
        variables at once.  When this occurs, the random effects (and
        standard errors) contained in the generated variables correspond to
        the order in which the variance components are listed in the output
        of meglm."  */
capture drop eb_int
capture drop eb_int_se
capture drop eb_slope
capture drop eb_slope_se
est restore rs_melog 

predict eb_slope eb_int , reffects reses(eb_slope_se eb_int_se ) /* the order of the terms is important! */
histogram eb_slope , title("Distribution of LRT resids") scheme(s1mono)
graph save "$path9\g1.gph", replace 
histogram eb_int , title("Distribution of Intercept resids") scheme(s1mono)
graph save "$path9\g2.gph", replace 
graph combine "$path9\g1.gph" "$path9\g2.gph", scheme(s1mono) title("Binary outcome model - School level residuals") /// 
   subtitle("[melogit  hi_gcse lrt sch_2 sch_3 ||school:lrt,  intp(7) ]", color(blue)) 
graph save "$path9\g3.gph", replace
   
capture drop eb_int_u
gen eb_int_u = eb_int + (eb_int_se*1.96)
capture drop eb_int_l
gen eb_int_l = eb_int - (eb_int_se*1.96)
capture drop cluster_rank
egen cluster_rank=group(eb_int school)   
capture drop cluster_tag
egen cluster_tag=tag(school) 
graph twoway (rspike eb_int_u eb_int_l cluster_rank if cluster_tag==1) (scatter eb_int cluster_rank if cluster_tag==1 ) ///
      , yline(0, lcolor(purple)) scheme(s1mono) title("School intercepts - residuals") xtitle("") legend(off)  
graph save "$path9\g4.gph", replace	  
graph combine "$path9\g3.gph" "$path9\g4.gph", scheme(s1mono) cols(1)   	  
****
  

*************************************************







*************************************************************************************
**** (ii) A quick tour of GLMs with and without multilevel data random effects, using the Scottish census open access 2011 dataset


de using ${census_2011}
use ${census_2011} , clear
codebook, compact

* For illustrative purposes, we'll make a level 2 of 'occupation-industry' 
numlabel _all, add
tab1 indus1ts occupat1t, missing

capture drop occ_ind_gp
egen occ_ind_gp=group(indus1ts occupat1t)
tab occ_ind_gp indus1ts /* numbers ascend in industry first, then occupation within industry */
tab occ_ind_gp occupat1t

tabplot indus1ts occupat1t if indus1ts > 0 , name(tabp1, replace) scheme(s1mono) ylabel(none) xlabel(none) showval(offset(0.2))

tab health1 /* we'll use this as the outcome measure */

tab  agegpt health1, row /* an apparent linear association with age */
* ..For simplicty, ignore any other predictors of health from modelling for now, and treat the age measure as if it were a linear score for decade.. 


** Making different non-linear formats of an outcome:

* Ordinal
tab health1 /* default format is ordinal */

* Binary
capture drop good_health 
gen good_health = (health1==1 | health1==2)  /* binary outcome */

* Multinomial 
tab  famcomp health1
capture drop fam_health 
gen fam_health=good_health + 1 
replace fam_health=3 if good_health==0 & (famcomp==1 | famcomp==2) 
replace fam_health=4 if good_health==1 & (famcomp==1 | famcomp==2) 
label define fam_health 1 "BadH, no partner" 2 "GoodH, no partner" 3 "BadH, partner" 4 "GoodH, partner", modify
label values fam_health fam_health
label variable fam_health "Family-by-health categories" 
tab  famcomp fam_health/* i.e. 4 categories, could be treated as multinomial outcome */

* Count (count-like) 
*  (This data doesn't have a naturally count format measure, but we can make one) 
tab1 numhrs econac1t 
capture drop tory_points 
gen tory_points=numhrs 
recode tory_points (-9 1=0) (2=1) (3=2) (4=3)  
replace tory_points=3 if econac1t==2 & numhrs==2
replace tory_points=4 if econac1t==2 & numhrs==3
replace tory_points=5 if econac1t==2 & numhrs==4
replace tory_points=6 if econac1t==2 & numhrs==4 & famcomp==2 
label variable tory_points "Goodness points (Conservative party version)"
tab tory_points /* typical of the format of a count variable (though the substantive measure is not to be taken seriously) */



**
codebook occ_ind_gp persid  health1 good_health fam_health tory_points   agegpt , compact



****** => Stata supports single level and multilevel random effects model formats for most non-linear outcomes, 
*            typically with more than one routine available to implement them - 
*            some of the most popular commands are illustrated below, though 
*             in many cases there are other options (e.g many are also available, expressed as gllmms, via  
*              gllamm, and some can be obtained by a few other specialist commands such as 'bayes' 
*              illustrated in Rabe-Hesketh & Skrondal 2022, section 16.8.3)





**************
** Binary outcome 
 
logit good_health agegpt /* logistic  regression, single level  */

glm good_health agegpt , family(binomial) link(logit) /* logistic regression via glm */

*

probit good_health agegpt /* probit regression */

glm good_health agegpt ,   family(binomial) link(probit)   /* probit regression via glm */



** Binary outcomes two level random effects model 

** Random intercepts examples
 
melogit good_health agegpt  ||occ_ind_gp:, /* random intercepts binary outcome (logit) */

meglm good_health agegpt  ||occ_ind_gp:,  family(binomial) link(logit) /* random intercepts binary outcome (logit) via meglm */


meprobit good_health agegpt  ||occ_ind_gp:,  /* random intercepts binary outcome (probit) */

meglm good_health agegpt  ||occ_ind_gp:, family(binomial) link(probit)   /* random intercepts binary outcome (probit) via meglm */


/*
* Random intercepts logit illustrated in GLLAMM 
gllamm good_health agegpt , i(occ_ind_gp) family(binomial) link(logit) /* binary outcome (logit) via gllamm */
*/




******
** Random slopes models for binary outcomes 

** Specifications are the same as for linear outcomes, but speed, and identifiability,
*    of estimates often become problematic
** Indeed, as lack of convergence is a common problem, model specifications and treatement of results are ideally modified to allow for that

* Example: 
*melogit good_health agegpt  ||occ_ind_gp:agegpt, cov(unstructured) intp(7)  /* random slopes binary outcome (logit) */
* Without refinement, the model above doesn't converge any time soon */

* Responses can include limiting the number of iterations or trying fewer integration points:

melogit good_health agegpt  ||occ_ind_gp:, cov(unstructured) intp(7)
est store ri 

melogit good_health agegpt  ||occ_ind_gp:agegpt, cov(unstructured) intp(7) iterate(20) 
est store rs_ip7

melogit good_health agegpt  ||occ_ind_gp:agegpt, cov(unstructured) intp(2) iterate(20) 
est store rs_ip2

melogit good_health agegpt  ||occ_ind_gp:agegpt, cov(unstructured) intp(20) iterate(20) 
est store rs_ip20

est table ri rs_ip7 rs_ip2 rs_ip20, stats(N ll bic converged) b(%8.4g) star

* (i.e. we limited all models to 20 iteractions, so 'converged' in the summary table shows if the model converged)
* (more integration points = more accurate estimation and more chance of convergence but slower calculations; 
*     fewer integration points = quicker calculations but less accurate) 






********************
** Ordinal outcome  

ologit health1 agegpt /* ordered logistic regression, single level  */

* {ologit doesn't have a natural 'glm' formulation that can be supported in Stata} 


   
** With random effects 

meologit health1 agegpt ||occ_ind_gp:, /* random intercepts  ordered logistic outcome */

meglm  health1 agegpt ||occ_ind_gp:, family(ordinal) link(logit) /* random intercepts ologit via meglm */


/* 
* Random intercepts ordered logit illustrated in gllamm
gllamm health1 agegpt , i(occ_ind_gp)  link(ologit)  /* ordered logistic outcome with random effects */
*/







******************
** Count outcome 

poisson tory_points agegpt /* poisson regression, single level */

glm tory_points agegpt , family(poisson) link(log) /* poisson regression via glm */
 
** Adding random effects: 

mepoisson tory_points agegpt ||occ_ind_gp:, /* random intercepts poisson regression */

meglm tory_points agegpt ||occ_ind_gp:, family(poisson) link(log) /* random intercepts poisson regression via glm */
 




/* 
* Random intercepts poisson model illustrated in gllamm
gllamm tory_points agegpt , i(occ_ind_gp)  family(poisson) link(log)  /* ordered logistic outcome with random effects */
*/






*********************
** Misrepresented outcomes...

* The next two options are really common even though they're sometimes thought to be wrong...


*** Linear assumption model: Treating an ordered (or count) outcome as if it were a linear score

regress health1 agegpt /* single level linear regression on ordered outcome */

glm health1 agegpt , family(gaussian) link(identity) /* single level linear regression via glm */

regress tory_points agegpt /* single level linear regression on count outcome */


mixed health1 agegpt ||occ_ind_gp:, /* random intercepts, linear assumption model example  */

mixed tory_points agegpt ||occ_ind_gp:, /* random intercepts linear regression on count outcome */



*** Linear probability model: Treating a binary outcome as if it were a linear score 


regress poor_health agegpt /*  linear probability model with 'regress'  */

glm poor_health agegpt , family(gaussian) link(identity) /* single level linear probability via glm */

mixed poor_health agegpt ||occ_ind_gp:, /* random intercepts, linear probability model example  */

********







****************
** Multinomial (polytomous) outcome 

mlogit fam_health agegpt, base(1) /* multinomial logit regression */ 

/* mlogit is not an available option via glm */



* Unlike with the other examples, there isn't a simple mlogit extension to random effects available in 
*   Stata
mlogit fam_health agegpt , base(1) /* single level multinomial logit regression */ 
memlogit fam_health agegpt ||occ_ind_gp:,  base(1) /* sorry, no such command! */ 


* The mlogit with random effects can however be run through gllamm 

gllamm fam_health agegpt , i(occ_ind_gp) ///
    family(binomial) link(mlogit) basecategory(1) /* multinomial logistic outcome wih random effects */






/*

* Aside - 
* Rabe-Hesketh and Skrondal 2022 (c12) also illustrate that the related conditional logit model can 
*   be used for multinomial outcomes with and without random effects in standard Stata routines (although
*   the data structure for conditional logits requires a 'long' format with a row for each alternative, and, 
*    in my experience as in this exmample, model estimation / convergence isn't straightforward) 

save $path9\p5_temp.dta, replace

use  $path9\p5_temp.dta, clear
tab fam_health 
keep fam_health agegpt occ_ind_gp persid 
list in 1/5 /* i.e. one row per alternative */
gen alt=1
gen choice=0
replace choice=1 if fam_health==1 
sav $path9\m1.dta, replace /* a subset for whether or not people are in category 1 */
*
use  $path9\p5_temp.dta, clear
tab fam_health 
keep fam_health agegpt occ_ind_gp persid
gen alt=2
gen choice=0
replace choice=1 if fam_health==2 
sav $path9\m2.dta, replace /* a subset for whether or not people are in category 2 */


use  $path9\p5_temp.dta, clear
tab fam_health 
keep fam_health agegpt occ_ind_gp persid
gen alt=3
gen choice=0
replace choice=1 if fam_health==3 
sav $path9\m3.dta, replace /* a subset for whether or not people are in category 3 */


use  $path9\p5_temp.dta, clear
tab fam_health 
keep fam_health agegpt occ_ind_gp persid
gen alt=4
gen choice=0
replace choice=1 if fam_health==4
sav $path9\m4.dta, replace /* a subset for whether or not people are in category 4 */

use $path9\m1.dta, clear
append using $path9\m2.dta
append using $path9\m3.dta
append using $path9\m4.dta
tab alt choice 

sort persid alt choice 
list in 1/16  , sepby(persid)  /* i.e. the data is now arranged in a 'long' format, one row per choice  */  

** Single level discrete choice conditional logit for data in this format:
cmset persid alt /* declares person id and outcome id */
cmclogit choice, casevars(agegpt) basealternative(1) 

** Multilevel random effects discrete choice conditional logit for data in this format 
*   (Specification shown, but actually these models won't converge...): 
cmset occ_ind_gp persid alt /* declares occ-ind group, person id and outcome id */
cmxtmixlogit choice, casevars(agegpt) basealternative(1) /* still single-level, without the occ-ind random effects */
cmxtmixlogit choice, casevars(agegpt)  random(ib1.alt, correlated) intpoints(2) iterate(10) /* allows random effects for occ-ind */
/* with random intercepts - this model struggles to identify - I capped the iterations and used a lower number of 
integration points than is recommended, in order to avoid exessive estimation time */


*/








*************************************************************










*************************************************************
**#   Ex 1.5: Illustrating random effects models with more than 2 levels  
***************************************



*******************************************
** A three level system using a simulated dataset

de using ${nurses_data}
use ${nurses_data}, clear
** This is a (fictional) dataset on self-reported stress experienced by nurses:
summarize
histogram stress

** There is natural hierarchical clustering the nurses into wards, and within wards into hospitals
svyset ward, strata(hospital)
svydes

* E.g. making some ward and hospital level variables: 
capture drop w_size
egen w_size=count(nurse), by(wardid)
capture drop h_size
egen h_size=count(nurse), by(hospital)
summarize h_size w_size
capture drop w_mean
egen w_mean=mean(stress), by(wardid)
capture drop h_mean
egen h_mean=mean(stress), by(hospital)
summarize h_mean w_mean
** Between 9 and 13 nurses per ward; between 36 and 52 nurses per hospital. 

capture drop h_first 
egen h_first=tag(hospital)
capture drop w_first
egen w_first=tag(wardid)
tab1 h_first w_first
* 25 different hospitals and 100 different wards are represented here. 

graph bar (mean) h_size  h_mean , over(hospital) title("Size and mean stress by hospital") 

graph twoway (scatter w_mean w_size ) (scatter w_mean h_size ) if w_first==1, ///
   title("Ward size and hospital size by mean ward stress") xtitle("Ward/Hospital size") ///
    legend(order(1 2) label(1 "Ward size") label(2 "Hospital size") ) 


*** Here's two two level null models : .

mixed stress ||wardid:, mle 
est store l2_a
estat icc
estadd scalar icc=r(icc2)


mixed stress ||hospital:, mle 
est store l2_b
estat icc
estadd scalar icc=r(icc2)

**** Now a three level null model :.

mixed stress ||hospital: ||wardid:, mle 
est store l3_a
estat icc
return list
estadd scalar icc=r(icc2) /* i.e. the level 2 variance proportion */
estadd scalar icc3=r(icc3) /* i.e. the level 3 variance proportion */

* Note that the icc for level 2 as reported by 'estat' is the sum of the level 2 and level 3 variance components 
* (it's the ICC rather than the VPC) 

/*
**** Compare/contrast in GLLAMM:
gllamm stress, i(wardid hospital) adapt nip(12) trace
est store l3_a2
*/

** Results are suggesting clustering at both levels exists, to some degree independently.


**** Three level random intercepts model with explanatory covariates:

mixed stress expcon age gender experien wardtype hospsize ///
      ||hospital: ||wardid:, mle variance
est store l3_b
estat icc
return list
estadd scalar icc=r(icc2) 
estadd scalar icc3=r(icc3) 
* (cf Hox M1, p36)


*** Three level model with random coefficients at the hospital level for the effect of the experimental group.

mixed stress expcon age gender experien wardtype hospsize ///
      ||hospital:expcon, cov(independent), ||wardid: , ml variance
est store l3_c
* (cf Hox M2, p36 - note there are small errors in the Hox estimations - the higher level
*   random slope in the book should be labelled as nu not mu, and the Hox book doesn't 
*   show the estimated covariance, which is included in the model, as in the unstructured variance)



*** Three level model with random coefficients at hospital level, and a cross-level interaction:.

capture drop experhs
gen experhs=expcon*hospsize
summarize experhs

mixed stress expcon age gender experien wardtype hospsize experhs ///
      ||hospital:expcon , cov(un) ||wardid:, mle variance
est store l3_d
* (cf Hox, p36, M3)


** Summary of some of these results....
est table l2_a l2_b l3_a l3_b l3_c l3_d , stats(ll bic N icc3 icc ) b(%8.4g) star


* In Stata, 'est table' gives a relatively messy depiction of combined results, showing the 
*  log of the relevant variance parameters. The code below with 'esttab' gives a somewhat improved output: 

est restore l3_c
est 
est restore l3_d
est
esttab l2_a l2_b l3_a l3_b l3_c l3_d  ,     bic scalars(ll icc3 icc ) not     ///
    transform("#2:_cons  #3:_cons    #4:_cons  #5:_cons :"  (exp(@)^2)  (exp(@)^2)  (exp(@)^2)  (exp(@)^2)  )  /// 
     equations(1:1:1:1:1:1, 2:2:3:3:4:5, 3:3:4:4:5:6, .:.:2:2:3:3, .:.:.:.:2:2, .:.:.:.:.:4 ) ///
     eqlabels("" "L2Var_cons" "L1Var_cons" "L3Var_cons " "L3Var_exp " "atr_cor_l2" ) ///
     mtitles("l2_a" "l2_b" "l3_a" "l3_b" "l3_c" "l3_d" ) 
* (The entries under 'equations' take some thought - i found them iteratively...)


**** 3 level models: A further look at residuals in hierarchical models

mixed stress expcon age gender experien wardtype hospsize ///
      ||hospital: ||wardid:, mle
est store l3_b

capture drop ebi_p* 
predict ebi_p3 ebi_p2, reffects /* 'Empirical Bayes Estimates' of residuals */
list hospital wardid ebi_p3 ebi_p2 in 1/100
** IE ebi_p3 are at the hospital level (level 3) and ebi_p2 are at the ward level (level 2)
histogram ebi_p3 if h_first==1, freq normal title("Empirical Bayes residuals at level 3")
graph save "$path9\bit1.gph", replace
histogram ebi_p2 if w_first==1, freq normal title("Empirical Bayes residuals at level 2")
graph save "$path9\bit2.gph", replace
graph combine "$path9\bit1.gph" "$path9\bit2.gph"

* Caterpillar plot in a 3-level context: 
est
capture drop se_p*
predict se_p3 se_p2 , reses
capture drop upper_p3 
gen upper_p3 = ebi_p3 + 1.96*se_p3
capture drop lower_p3 
gen lower_p3 = ebi_p3 - 1.96*se_p3
capture drop upper_p2 
gen upper_p2 = ebi_p2 + 1.96*se_p2
capture drop lower_p2 
gen lower_p2 = ebi_p2 - 1.96*se_p2

capture drop rank_p*
egen rank_p3=group(ebi_p3 hospital)
egen rank_p2=group(ebi_p2 wardid)

graph twoway (scatter ebi_p3  rank_p3 if h_first==1, ///
       msymbol(circle) msize(medium) mcolor(green) mlabel(hospital) mlabpos(12) mlabcol(gs9) mlabgap(8) mlabsize(vsmall) )  ///
     (rspike upper_p3 lower_p3 rank_p3 if h_first==1, lcolor(gs8) lwidth(thin)) ///
    , scheme(s1mono) yline(0, lcolor(purple) lwidth(medium)) subtitle(" Hospital residuals " ) legend(off) 
graph save $path9\g1.gph, replace

capture drop evenclus
gen evenclus=6 + 6*(rank_p2/2==floor(rank_p2/2) )
graph twoway (scatter ebi_p2  rank_p2 if w_first==1, ///
       msymbol(circle) msize(medium) mcolor(blue) mlabel(hospital) mlabvpos(evenclus) mlabcol(gs9) mlabgap(8) mlabsize(vsmall) )  ///
     (rspike upper_p2 lower_p2 rank_p2 if w_first==1, lcolor(gs8) lwidth(thin)) ///
    , scheme(s1mono) yline(0, lcolor(purple) lwidth(medium)) subtitle(" Ward residuals " "(labelled by hospital)" ) legend(off) 
graph save $path9\g2.gph, replace

graph combine $path9\g1.gph $path9\g2.gph, scheme(s1mono) cols(1)

* (e.g., observe both the best and the worst ward are found in the same hopsital) 

**************
	 




**************************************************
** Another illustation of a three level system, using contrived levels in the Scottish census dataset:


use ${census_2011} , clear
codebook, compact

** Lower level: person (persid)
** Higher level (1): Occupation-by-industry
** Higher level (2): ethnicity, religion and country of birth

numlabel _all, add
tab1 indus1ts occupat1t, missing

capture drop occ_ind_gp
egen occ_ind_gp=group(indus1ts occupat1t)
codebook occ_ind_gp, compact /* 118 different combinations */


tab1 relgs1t cofbt  eths1t 
capture drop rel_eth_gp
gen rel_eth_gp = cofbt*100 + eths1t*10 + relgs1t 
* (as all three measures are 1-digit values, it is more convenient to define permutations with numbers based on those values)
codebook rel_eth_gp, compact  /* 71 different combinations */

**** 
** Other data prep: 
tab1 sex agegpt
capture drop female
gen female=(sex==2) 
capture drop c_age
gen c_age = (agegpt*10) - 40
capture drop age_fem
gen age_fem=c_age*fem

capture drop c_age2 
gen c_age2= c_age^2 - 40^2


codebook health1 female c_age age_fem   occ_ind_gp rel_eth_gp, compact

*** Example of preidcting health outcomes (higher score = worse health), given age and gender plus the extra structure(s) 

*** One level model with age and gender  
mixed health1 female c_age c_age2 age_fem 
est store m1a

** One level model only with age 
mixed health1  c_age c_age2 
est store m1b


*** Two-level model for individuals in occupations:
mixed health1  c_age c_age2 ||occ_ind_gp:, 
estat icc
estadd scalar icc2=r(icc2) 
est store m2a

*** Two-level model for individuals in ethnic groups:
mixed health1 c_age c_age2 ||rel_eth_gp:, 
estat icc
estadd scalar icc2=r(icc2) 
est store m2b



**** Hypothetically, we could think of an hierarchical 3-level structure of indvs in (ethnic group*industry) in industry

capture drop eth_in_occ 
egen eth_in_occ= group(rel_eth_gp occ_ind_gp)
codebook eth_in_occ , compact 

** Note this is heuristic, it is not realistic to the data, but we are saying that ethnic group is wholly contingent upon 
*    industry (we don't recognise the same ethnic groups across industries)

gsort -occ_ind_gp -rel_eth_gp +persid 
list occ_ind_gp rel_eth_gp eth_in_occ persid in 1/150, sepby(occ_ind_gp rel_eth_gp)


* If we modelled this three level structure as hierarchical levels we find:

*** Three-level model for individuals in ethnicity-given-occupation in occupation:
mixed health1 c_age c_age2 ||occ_ind_gp:  ||eth_in_occ:, 
estat icc
estadd scalar icc2=r(icc2) 
est store m3a


di "The occupation level (level 3) icc and vpc = "  .0317953  / ( .0317953  +  .0018472 +  .6395205) 
di "The ethnicity-given-occupation  vpc = "  .0018472  / ( .0317953  +  .0018472 +  .6395205) 
di "The ethnicity-given-occupation  icc = "  (.0317953 + .0018472)  / ( .0317953  +  .0018472 +  .6395205) 



** In this formulation, the 118 occ groups are linked to about 3% of the error variance (small but probably significant), 
**         and the 1451 occ-by-ethnicity groups are linked to about 0.2% of the error variance (very small but still significant) 


* It's an advanced topic, but the above hierarchical formulation of a 3-level model is common and works better on datasets with a natural 
*     3-level hiearchy, but if as here the two higher levels are not really nested, then the 'cross-classified' model is more appropriate


mixed health1 c_age c_age2 ||_all:R.rel_eth_gp,  ||occ_ind_gp:,  
* (Stata notes: the call for a cross-classified model works best if the higher level is the one with fewer units)

di "The ethnicity level icc and vpc = " .0113245 / (.0113245 + .0332933 +  .6392549)
di "The occupation level icc and vpc = " .0332933 / (.0113245 + .0332933 +  .6392549)



*** 
**************************************************









**************************************************************
**# EOF
****************************************************************

