*************************************************************************
******************* HOMESCHOOLING PROJECT *******************************
*************************************************************************

***FILENAME: PFI_12.do
***AUTHOR: Mitchell Pudil
***DATA COLLECTED FROM: United States Department of Education
***						National Center for Education Statistics
						
**************************************************************************


*************Variables***********
*Codebook: https://nces.ed.gov/nhes/data/2016/cbook_ecpp_pu.pdf

gen year = 2016



* Keep only if respondant answered question of whether child was homeschooled 

drop if homeschool==-1 

* Keep only if exactly one mother in household

keep if hhmom==1
drop if P1REL==1 & P1SEX==2 & P1REL==1 & P2SEX==2


replace homeschool=0 if homeschool==2 //For homeschool: 1 is yes, 2 is no

* Child's Sex
* 1 is male, 2 is female
rename csex cmale
replace cmale=0 if cmale==2

* Household Income (Midpoint)

replace ttlhhinc=2500 if ttlhhinc==1   // We take a weighted average here to account for the fact that there were more households with
// a household income of 0 or were in debt
replace ttlhhinc=15000 if ttlhhinc==2
replace ttlhhinc=25000 if ttlhhinc==3
replace ttlhhinc=35000 if ttlhhinc==4
replace ttlhhinc=45000 if ttlhhinc==5
replace ttlhhinc=55000 if ttlhhinc==6
replace ttlhhinc=67500 if ttlhhinc==7
replace ttlhhinc=87500 if ttlhhinc==8
replace ttlhhinc=125000 if ttlhhinc==9
replace ttlhhinc=200000 if ttlhhinc==10

gen lnhhinc=ln(ttlhhinc)

sum ttlhhinc if homeschool==0
sum ttlhhinc if homeschool==1

* Race of Child  
replace cwhite=0 if cwhite==2
replace cblack=0 if cblack==2
replace chispan=0 if chispan==2
replace casian=0 if casian==2
replace camind=0 if camind==2
replace cpaci=0 if cpaci==2

gen cwhiteonly=0
replace cwhiteonly=1 if cwhite==1 & cblack==0 & chispan==0 & casian==0 & cpaci==0

gen cblackonly=0
replace cblackonly=1 if cwhite==0 & cblack==1 & chispan==0 & casian==0 & cpaci==0

gen chispanonly=0
replace chispanonly=1 if cwhite==0 & cblack==0 & chispan==1 & casian==0 & cpaci==0

gen cother=0
replace cother=1 if cwhiteonly==0 & cblackonly==0 & chispanonly==0

sum cwhiteonly if homeschool==0
sum cwhiteonly if homeschool==1

sum cblackonly if homeschool==0
sum cblackonly if homeschool==1


* Age of Child
rename AGE2015 cage
sum cage if homeschool==0
sum cage if homeschool==1

* Number of siblings
rename numsibsx numsib
sum numsib if homeschool==0
sum numsib if homeschool==1

* P1REL, P1SEX, P1MRSTA, P1EDUC, P1AGE


*Mother Education
gen mothered=.
replace mothered=P1EDUC if P1REL==1 & P1SEX==2
replace mothered=P2EDUC if P2REL==1 & P2SEX==2

gen meducation=0
replace meducation=6 if mothered==1 // 8th grade or less
replace meducation=11 if mothered==2 // High school, no diploma
replace meducation=13 if mothered==3 // High School diploma or GED
replace meducation=15 if mothered==4 //Vocational
replace meducation=15 if mothered==5 // Some college, no degree
replace meducation=15 if mothered==6 // Associate's Degree
replace meducation=17 if mothered==7 // Bachelor's 
replace meducation=19 if mothered==8 // Master's
replace meducation=21 if mothered > 8 // Doctorate/professional degree



sum meducation if homeschool==0
sum meducation if homeschool==1


* Religious
rename fogroupx religious
replace religious=0 if religious==2

sum religious if homeschool==0
sum religious if homeschool==1



* Hours and days homeschooled not included in this survey


* Reasons for Homeschooling

tab hsmostx  // (A: peer pressure, B: Dissatisfied with instruction, C: Religious Instruction,
* D: Moral instruction, E: Health Problems, F: Temporary Illness, G: Special Needs,
* H: Nontraditional Education, I: Other

tab hsdissatx

tab hsrelgon if homeschool==1
tab hsmoral if homeschool==1
tab hsrelgon if hsmoral==1 & homeschool==1
// Total number of parents who homeschooled their kids for religious or moral 
// instruction is 20 (religious) + 29 (moral) - 19 (both) = 30


tab hsdisablx if homeschool==1
tab hsillx if homeschool==1
tab hsdisablx if hsillx==1 & homeschool==1
// Total number of parents who homeschooled their kids for health or temp. illness
// is 15 (health) + 5 (temporary illness) - 5 (both) = 15


tab hsspclndx if homeschool==1

tab hsaltx

// Zipcode
destring ziplocl, replace
replace ziplocl=1 if ziplocl < 14 | ziplocl > 40
replace ziplocl=2 if ziplocl > 20 & ziplocl < 24
replace ziplocl=3 if ziplocl > 30 & ziplocl < 34


// Mother's age
gen momage=.
replace momage=P1AGE if P1REL==1 & P1SEX==2
replace momage=P2AGE if P2REL==1 & P2SEX==2

// Mother is married (mother is currently married)
gen maritalmom=.
replace maritalmom=P1MRSTA  if P1REL==1 & P1SEX==2
replace maritalmom=P2MRSTA if P2REL==1 & P2SEX==2

gen marriedmom=.
replace marriedmom=1 if maritalmom==1
replace marriedmom=0 if marriedmom > 1



// For summary statistics/regression
keep year ttlhhinc cwhiteonly chispanonly cblackonly cother cenreg ziplocl homeschool lnhhinc mothered meducation cmale momage cage ziplocl religious marriedmom numsib hsdisablx hsillx hsdisablx hsspclndx hsaltx maritalmom 


// For regression only

keep year ttlhhinc cwhiteonly cblackonly mothered cenreg ziplocl homeschool lnhhinc meducation cmale momage cage ziplocl religious marriedmom numsib maritalmom
rename mothered momed
rename ttlhhinc hhinc, replace
rename cwhiteonly cwhite
rename cblackonly cblack

order year homeschool lnhhinc momed maritalmom momage cage numsib cmale cwhite cblack cenreg ziplocl religious hhinc meducation marriedmom
drop if momage==.



logistic homeschool lnhhinc meducation marriedmom i.cenreg i.ziplocl momage cage cmale numsib cwhite cblack religious, robust 
outreg2 using g6.doc, stnum(replace coef=exp(coef), replace se=coef*se) cti(odds ratio) 








