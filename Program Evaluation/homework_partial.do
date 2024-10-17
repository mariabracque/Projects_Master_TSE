/************************************************************************
Project:	            Program Evaluation Homework 2023
Do File: 			    homework.do
Authors: 				Dhruv Jain and Wenxuan Xu
Created version: 		02/1/2023
This version :			02/7/2023
********************************************************************************
Description: This do file replicates some key results from Draca et al. (2011)
********************************************************************************

						///		INSTRUCTIONS	\\\

1. Read the questions carefully and then fill in the missing gaps in the code to
produce the required output.

2. The missing gaps are indicated by << ... >>> or <<< insert code here >>>

3. Go through the do file and questions sequentially.

4. All parts of the code involving a loop or a local variable need to be 
executed together. Please ask your TA to explain in the TP in case you have any 
questions.

5. None of the regression models you will run in this homework include additional
controls used by the authors in the paper. 

6. Please book slots during the Helpdesks if you have any doubts or need help.

*********************************************************************************/


clear all
clear matrix
set mem 500m
set matsize 4000 
cap log close
set more off

global path "C:\Users\maria\Documents\00 TSE\M1\Program Evaluation\Homework"
  
cd "$path" 


*** Load the dataset ***

use "homework.dta", clear


********************************************************************************
*********************** Part 0: Data Exploration  ******************************
********************************************************************************

describe // See description of the variables 

tab ocu 
tab week 
 
scatter crime h



********************************************************************************
*********************** Part I: Raw Difference-in-Difference *******************
********************************************************************************


							/// Q1: Preliminaries \\\

							
*... Define indicator for treated boroughs
//borough IDs 1,2,3,6,14 are in the treatment group

gen treat=0 
replace treat = 1 if ocu == 1
replace treat = 1 if ocu == 2 
replace treat = 1 if ocu == 3 
replace treat = 1 if ocu == 6 
replace treat = 1 if ocu == 14 		

label var treat "1=treated boroughs; 0=control boroughs"


*... Define indicator for policy period & previous year comparison period 

gen policy=.

replace policy=1 if week>=80 & week<=85		//policy was implemented between weeks 80-85 (inclusive)
replace policy=0 if week>=28 & week<=33

label var policy "1=6 weeks of Operation Theseus; 0=same 6 weeks in the previous year"
/* Note: The variable << policy >> defined here can be understood as the 
<< POST >> variable in a standard 2 period DID model */



keep if policy!=. 
/* We keep only 12 weeks of data for this part of the analysis 
(6 weeks of policy and 6 weeks in the previous year which is the comparison period) */

sort policy ocu week


/* Note: At this stage our dataset is at the borough-week level. In order to 
compute the raw difference-in-difference we need two observations per borough 
which contain the average of total crime and total police hours in the 
pre-period and the post-period. To do this we use the "collapse" command in Stata. 
You can see details of this command by typing << help collapse >> in the Stata 
command window */

collapse crime h pop treat, by(policy ocu) 


*... Let us create the interaction variable between treatment status and post-period. Let us also normalize crime by population.

gen treatXpolicy= treat*policy		//create interaction between treatment and policy

gen crimepop=(crime/pop)*1000

gen hpop=(h/pop)*1000



				/// Q2: Computing double difference manually \\\

			
/*... Now we compute the average police and crime in the treatment and control
groups in the pre and post period */


* Police
sum hpop if treat==1 & policy==0 [aw=pop] //the authors use analytic weights by borough population, to replicate their results, we will do the same when generating all our results

scalar P10= r(mean)  //store the average hpop in this scalar, try using (help summarize) to see what the mean is stored as 

sum hpop if treat==1 & policy==1 [aw=pop]
scalar P11= r(mean)
 
sum hpop if treat==0 & policy==0 [aw=pop]
scalar P00= r(mean)

sum hpop if treat==0 & policy==1 [aw=pop]
scalar P01= r(mean)


* Crime
sum crimepop if treat==1 & policy==0 [aw=pop]
scalar C10= r(mean)

sum crimepop if treat==1 & policy==1 [aw=pop]
scalar C11= r(mean)

sum crimepop if treat==0 & policy==0 [aw=pop]
scalar C00= r(mean)

sum crimepop if treat==0 & policy==1 [aw=pop]
scalar C01= r(mean)


*... The raw difference-in-difference is given by:

display "DID Police:" (P11 - P01) - (P10 - P00)
	// write down the double diff using the averages obtained above

display "DID Crime:" (C11 - C01) - (C10 - C00)		// write down the double diff using the averages obtained above



		/// Q3: Calculating DD and standard errors using Regression \\\


*... The regression gives you the DD and its standard error directly!


* Police

reg hpop treat policy treatXpolicy [aw=pop], cluster(ocu)  // think about what regression you will run in a 2x2 DID


* Crime

reg crimepop treat policy treatXpolicy [aw=pop], cluster(ocu) // also weight your regression with borough population and cluster SE at the borough level



********************************************************************************
******************** Part II: Main DID Results of the Paper ********************
********************************************************************************


use "homework.dta", clear // load the original dataset again


						/// Q1: Preliminaries \\\
						
						
*... Define an indicator for treated boroughs

// borough IDs 1,2,3,6,14 are in the treatment group
gen treat=0 
replace treat=1 if ocu==1 | ocu==2 | ocu==3 | ocu==6 | ocu==14

label var treat "1=treated boroughs; 0=control boroughs"


*... Time variables and Interaction with Treatment

gen policy=(week>=80 & week<=85) 		/* dummy takes value 1 during the 6 weeks of 
operation Theseus and 0 otherwise*/

gen treatXpolicy=treat*policy 


gen post=(week>=86) 		/*dummy takes value 1 after operation Theseus is completed
i.e. week 86 onwards and 0 otherwise */

gen treatXpost=treat*post


gen full=(week>=80) 		/* dummy takes value 1 once operation Theseus begins 
i.e. from week 80 onwards and 0 otherwise */

gen treatXfull=treat*full


*... Police and Crime variables

gen hpop=h/pop
lab var hpop "police hours per head of population (per 1000)"

gen crimepop=crime/pop
lab var crimepop "total crimes per head of population (per 1000)"

gen suspop=(theft+violence+robbery)/pop
lab var suspop "total susceptible crimes per head of population (per 1000)"



*... Taking logs of all variables and then seasonally difference them

foreach var in crimepop hpop theft violence robbery crim_damage suspop {
	
	gen l`var'=log(`var') 			// create log variables

	sort ocu week			// think carefully about the sorting here, this allows us to use the difference variable in the next step 
	
	by ocu: gen dl`var'=l`var'-l`var'[week-52]  // create seasonally diff log variables
}


save "homework2.dta", replace  //saving dataset for future use



						/// Q2: Reduced Form \\\
						

*... DID Estimates on police deployment (hours per 1000 population)

** Not splitting post-attack period

* We use the Stata prefix 'xi' in order to specify that the following regression has a categorical variable. This is not necessary in newer versions of Stata, but for given the age of this paper, is required to reproduce the results. Learn more with (help xi).

xi: reg dlhpop treatXfull full i.week [aw=pop], cluster(ocu)	
	
	/* Notes: the above code should help you understand (1) how to weight your regressions,
	(2) how to cluster standard errors and (3) to include << xi >> when creating 
	dummies from a categorical variable (think about what i.week does!)  */

	
** Splitting post-attack period into 2 parts

xi: reg dlhpop treatXpolicy policy treatXpost post  i.week [aw=pop], cluster(ocu)	// include week FE, cluster SE at borough level, weight regression with borough population




*... DID Estimates on total crime (crime per 1000 population)

** Not splitting post-attack period

xi: reg dlcrimepop treatXfull full i.week [aw=pop], cluster(ocu) 	// include week FE, cluster SE at borough level, weight regression with borough population


** Splitting post-attack period into 2 parts

xi: reg dlcrimepop treatXpolicy policy treatXpost post i.week [aw=pop], cluster(ocu)	// include week FE, cluster SE at borough level, weight regression with borough population



					/// Q3: Structural Results \\\
					
					
*... OLS Estimates and split post-attack period into two parts 

xi: reg dlcrimepop dlhpop policy post i.week [aw=pop],cluster(ocu) 



*... IV Estimates and split post-attack period into two parts 

** First Stage and test of weak instruments

xi: reg dlhpop treatXpolicy treatXpost i.week [aw=pop],cluster(ocu)  // run first stage with treatXpolicy and treatXpost as excluded instruments
						// include week FE, cluster SE at borough level, weight regression with borough pop.
						

test treatXpolicy treatXpost			// check if excluded instruments are weak or not

** 2SLS 

xi:ivregress 2sls dlcrimepop policy post (dlhpop = treatXpolicy treatXpost) i.week [aw=pop],cluster(ocu)	// include week FE, cluster SE at borough level, weight regression with borough pop.



********************************************************************************
***************** Part III: Treatment Effect by Crime Type  ********************
********************************************************************************


use "homework2.dta", clear  			// load "homework2.dta"


replace dlrobbery=0 if dlrobbery==. & dltheft!=. /* authors replaced dlrobbery=0 
if it was missing but dltheft was not missing. We simply follow them*/


foreach var in dltheft dlviolence dlrobbery dlcrim_damage{

eststo `var' : xi: reg `var' treatXpolicy policy treatXpost post i.week [aw=pop],cluster(ocu)  // complete the regression equation

}

// Let's display the results in a table on Stata output window

esttab dltheft dlviolence dlrobbery dlcrim_damage , stats(N) keep(treatXpolicy treatXpost)  // write the 4 names in which the results are stored



********************************************************************************
************** Part IV: Week-on-Week Policy and Placebo Effects  ***************
********************************************************************************


						/// Q1: Police Deployment \\\

						
use "homework2.dta", clear 			// load "homework2.dta"

** Construct a loop to get weeks before & after

local i=53
while `i'<=104 {

gen attack`i'=(week==`i')
gen x`i'_treat=attack`i'*treat

qui xi: reg dlhpop attack`i' x`i'_treat  i.week [aw=pop], cluster(ocu) 

gen btreat`i'= _b[x`i'_treat] 				// store the beta coefficient of the interaction term
gen bse`i'= _se[x`i'_treat]					// store the standard error of the interaction term

local i=`i'+1

}

keep btreat* bse*
order bse*

keep if _n==1

gen k=1
	
reshape long btreat bse, i(k) j(week) 
drop k

lab var btreat "co-efficient for treatment effect"
lab var bse "std error for treatment effect"

gen lo=btreat+(2*bse)
gen hi=btreat-(2*bse) 

gen policy=(week>=80 & week<=85)

gen policy_lo=lo if policy==1
lab var policy_lo "Operation Theseus"

gen policy_hi=hi if policy==1
lab var policy_hi " "

replace lo=. if policy==1
replace hi=. if policy==1

lab var lo "Confidence Intervals"
lab var hi " "

gen tstat= btreat/bse	// compute the t-statistic here
gen five=.
replace five=btreat if abs(tstat)>=1.95
replace five=. if policy==1
lab var five "Significant at 5% (non-policy weeks)"
  
  
// Creating the Figure 
twoway rcap lo hi week, xline(86, lp(dash) ) xline(79, lp(dash)) text(0.60 82.5 "Policy On") xlab( 55 "Jan" 59 "Feb" 63 "Mar" 67 "Apr" 71 "May" 75 "Jun" 79 "Jul" 83 "Aug" 87 "Sep" 91 "Oct" 95 "Nov" 99 "Dec" 103 "Jan") xtitle("Weeks") ytitle("Change in log(police/population)") title("Week-by-Week Treatment Effects", size(med)) ysc(r(-0.5 0.25)) || rcap  policy_lo policy_hi week, lw(thick) legend(on lab(1 "Confidence Intervals") lab(2 "Operation Theseus") size(small)) 

graph export "police.png", replace



							/// Q2: Total Crimes \\\
							
							
use "homework2.dta", clear 			// load "homework2.dta"


*		<< insert code here >> and use total crimes per population (per 1000)

local i=53
while `i'<=104 {

gen attack`i'=(week==`i')
gen x`i'_treat=attack`i'*treat

qui xi: reg dlcrimepop attack`i' x`i'_treat  i.week [aw=pop], cluster(ocu) 

gen btreat`i'= _b[x`i'_treat] 				// store the beta coefficient of the interaction term
gen bse`i'= _se[x`i'_treat]					// store the standard error of the interaction term

local i=`i'+1

}

keep btreat* bse*
order bse*

keep if _n==1

gen k=1
	
reshape long btreat bse, i(k) j(week) 
drop k

lab var btreat "co-efficient for treatment effect"
lab var bse "std error for treatment effect"

gen lo=btreat+(2*bse)
gen hi=btreat-(2*bse) 

gen policy=(week>=80 & week<=85)

gen policy_lo=lo if policy==1
lab var policy_lo "Operation Theseus"

gen policy_hi=hi if policy==1
lab var policy_hi " "

replace lo=. if policy==1
replace hi=. if policy==1

lab var lo "Confidence Intervals"
lab var hi " "

gen tstat= btreat/bse	// compute the t-statistic here
gen five=.
replace five=btreat if abs(tstat)>=1.95
replace five=. if policy==1
lab var five "Significant at 5% (non-policy weeks)"
  
  
// Creating the Figure 
twoway rcap lo hi week, xline(86, lp(dash) ) xline(79, lp(dash)) text(0.60 82.5 "Policy On") xlab( 55 "Jan" 59 "Feb" 63 "Mar" 67 "Apr" 71 "May" 75 "Jun" 79 "Jul" 83 "Aug" 87 "Sep" 91 "Oct" 95 "Nov" 99 "Dec" 103 "Jan") xtitle("Weeks") ytitle("Change in log(total crime/population)") title("Week-by-Week Treatment Effects", size(med)) ysc(r(-0.5 0.25)) || rcap  policy_lo policy_hi week, lw(thick) legend(on lab(1 "Confidence Intervals") lab(2 "Operation Theseus") size(small)) 

graph export "total_crime.png", replace  			



							/// Q3: Susceptible Crimes \\\
							

use "homework2.dta", clear  			// load "homework2.dta"


*		<< insert code here >> and use total susceptible crimes per population (per 1000)

local i=53
while `i'<=104 {

gen attack`i'=(week==`i')
gen x`i'_treat=attack`i'*treat

qui xi: reg dlsuspop attack`i' x`i'_treat  i.week [aw=pop], cluster(ocu) 

gen btreat`i'= _b[x`i'_treat] 				// store the beta coefficient of the interaction term
gen bse`i'= _se[x`i'_treat]					// store the standard error of the interaction term

local i=`i'+1

}

keep btreat* bse*
order bse*

keep if _n==1

gen k=1
	
reshape long btreat bse, i(k) j(week) 
drop k

lab var btreat "co-efficient for treatment effect"
lab var bse "std error for treatment effect"

gen lo=btreat+(2*bse)
gen hi=btreat-(2*bse) 

gen policy=(week>=80 & week<=85)

gen policy_lo=lo if policy==1
lab var policy_lo "Operation Theseus"

gen policy_hi=hi if policy==1
lab var policy_hi " "

replace lo=. if policy==1
replace hi=. if policy==1

lab var lo "Confidence Intervals"
lab var hi " "

gen tstat= btreat/bse	// compute the t-statistic here
gen five=.
replace five=btreat if abs(tstat)>=1.95
replace five=. if policy==1
lab var five "Significant at 5% (non-policy weeks)"
  
  
// Creating the Figure 
twoway rcap lo hi week, xline(86, lp(dash) ) xline(79, lp(dash)) text(0.60 82.5 "Policy On") xlab( 55 "Jan" 59 "Feb" 63 "Mar" 67 "Apr" 71 "May" 75 "Jun" 79 "Jul" 83 "Aug" 87 "Sep" 91 "Oct" 95 "Nov" 99 "Dec" 103 "Jan") xtitle("Weeks") ytitle("Change in log(Susceptible crime/population)") title("Week-by-Week Treatment Effects", size(med)) ysc(r(-0.5 0.25)) || rcap  policy_lo policy_hi week, lw(thick) legend(on lab(1 "Confidence Intervals") lab(2 "Operation Theseus") size(small)) 

graph export "susceptible_crime.png", replace 