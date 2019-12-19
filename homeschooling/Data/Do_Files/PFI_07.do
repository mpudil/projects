*************************************************************************
******************* HOMESCHOOLING PROJECT *******************************
*************************************************************************

***FILENAME: PFI_07.do
***AUTHOR: Mitchell Pudil
***DATA COLLECTED FROM: United States Department of Education
***						National Center for Education Statistics NHES-PFI 2012
						
**************************************************************************


*************Variables***********

***Dependent Variable***		// homeschool (from homeschl) and hshour (from hshours)

gen homeschool=substr(homeschl, 1,1)
destring(homeschool), force replace
replace homeschool=0 if homeschool==2 //0 if not homeschooled, 1 if they are

***Important Independent Variables for Regression***
*Household income
*Mother Education (none-HS, High grad, Some college, bachelor, grad+)
*Child is male
*Parent's age (average and separate- only want mother's age for reg's)
*Child has disability
*Single Mother
*Child from U.S.: cbornus
*Child Race (Hispanic, White, Black, control for all X): chispan, cwhite, cblack, camind, casian, cpaci, craceoth
*Community Type (urban, suburban): ziplocl

***Important Variables for Summary Stats)***
*sreason (parent's reason for homeschooling)
*sefuture
***********************************



*Income: HINCOME

gen hhinc=substr(hincome,1,2)
destring(hhinc), replace
replace hhinc=2500 if hhinc==1
replace hhinc=7500 if hhinc==2
replace hhinc=12500 if hhinc==3
replace hhinc=17500 if hhinc==4
replace hhinc=22500 if hhinc==5
replace hhinc=27500 if hhinc==6
replace hhinc=32500 if hhinc==7
replace hhinc=37500 if hhinc==8
replace hhinc=42500 if hhinc==9
replace hhinc=47500 if hhinc==10
replace hhinc=55000 if hhinc==11
replace hhinc=67500 if hhinc==12
replace hhinc=87500 if hhinc==13
replace hhinc=150000 if hhinc==14

gen lnhhinc=ln(hhinc)

*Mother's Education (use i.momed)

* 1: less than HS diploma    2: HS Grad    3: Some college/voc   4: College grad   5: Grad school


gen birthmother=substr(momtype1,1,2)
destring(birthmother), replace
replace birthmother=. if birthmother==-1
replace birthmother=. if birthmother==2
replace birthmother=. if birthmother==3
replace birthmother=. if birthmother==4
replace birthmother=. if birthmother==5

gen momed=substr(momeduc1,1,1)
destring momed, force replace
replace momed=momed*birthmother

gen momed2=substr(momeduc2,1,2)
destring(momed2), replace
keep if momed2==-1


*Mother Marital Status

gen maritalmom=substr(momstat1,1,1)
destring maritalmom, force replace

gen marriedmom=.
replace marriedmom=1 if maritalmom==1
replace marriedmom=0 if maritalmom > 1

*Mother Education

 
gen meducation=.
replace meducation=8 if momed==1
replace meducation=12 if momed==2
replace meducation=15 if momed==3
replace meducation=17 if momed==4
replace meducation=20 if momed==5
 


*Child's Sex

gen cmale=substr(sex,1,1)
destring cmale, replace
replace cmale=0 if cmale==2



*Parent's Average (remember to only use mother age in final reg's)

*momage1   dadage1

replace momage1=. if momage1==-1
replace dadage1=. if dadage1==-1
gen parentage=(momage1+dadage1)/2
replace momage1=momage1*birthmother


*Child has disability

gen cdisabled=substr(hsdisabl,1,1)
destring(cdisabled), force replace

replace cdisabled=0 if cdisabled==2


*Child Born in U.S.

gen cunited=substr(cbornus,1,1)
destring(cunited), replace
replace cunited=0 if cunited==2
replace cunited =0 if cunited==3




*Child Race (use i.crace)

gen cWhite=substr(cwhite,1,1)
destring cWhite, replace
replace cWhite=0 if cWhite==2

gen cHispan=substr(chispan,1,1)
destring cHispan, replace
replace cHispan=0 if cHispan==2 


gen cBlack=substr(cblack,1,1)
destring cBlack, replace
replace cBlack=0 if cBlack==2

gen cOther=substr(craceoth,1,1)
destring cOther, replace
replace cOther=0 if cOther==2
replace cOther=1 if cWhite+cHispan+cBlack==0

//Mixed race --> Other race 

replace cWhite=2 if cWhite==1 & cWhite+cHispan+cBlack + cOther > 1
replace cOther=1 if cWhite==2
replace cWhite=0 if cWhite==2

replace cHispan=2 if cHispan==1 & cWhite+cHispan+cBlack + cOther > 1
replace cOther=1 if cHispan==2
replace cHispan=0 if cHispan==2

replace cBlack=2 if cBlack==1 & cWhite+cHispan+cBlack + cOther > 1
replace cOther=1 if cBlack==2
replace cBlack=0 if cBlack==2


gen crace=0
replace crace=1 if cWhite==1
replace crace=2 if cHispan==1
replace crace=3 if cBlack==1
replace crace=4 if cOther==1


*Child Age

rename age2006 cage

*Community Type (1 is urban, 2 is suburban, 3 is rural)-- use i.zip

gen zip=substr(ziplocl,1,2)
destring(zip), replace
replace zip=1 if zip<4
replace zip=1 if zip==7
replace zip=1 if zip==8
replace zip=1 if zip==9
replace zip=2 if zip==4
replace zip=2 if zip==5
replace zip=2 if zip==6
replace zip=3 if zip==10
replace zip=3 if zip==11
replace zip=3 if zip==12


*Census Region

gen region=substr(cenreg,1,1)
destring region, replace
*Year

gen year=2007

*Religious

gen religious=substr(forelcls,1,1)
destring religious, replace
replace religious=0 if religious==2


keep numsibs cage homeschool cmale cwhite cblack hhinc lnhhinc momed religious region momage1 cunited year maritalmom zip

rename region cenreg
rename zip ziplocl

logistic homeschool lnhhinc meducation marriedmom i.cenreg i.ziplocl momage numsib religious, robust 
outreg2 using hspaper3.doc, stnum(replace coef=exp(coef), replace se=coef*se) cti(odds ratio) 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
