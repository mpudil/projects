*************************************************************************
******************* HOMESCHOOLING PROJECT *******************************
*************************************************************************

***FILENAME: PFI_03.do
***AUTHOR: Mitchell Pudil
***DATA COLLECTED FROM: United States Department of Education
***						National Center for Education Statistics NHES-PFI 2003
						
**************************************************************************


*************Variables***********

***Dependent Variable***		// homeschool (from homeschl) and hshour (from hshours)

gen homeschool=substr(homeschl, 1,1)
destring(homeschool), force replace
replace homeschool=0 if homeschool==2 //0 if not homeschooled, 1 if they are

***Possible Explanatory Variables for Regression***
*Household income: 
*Mother Education (none-HS, High grad, Some college, bachelor, grad+)
*Child is male
*Parent's age (average and separate- only want mother's age for reg's)
*Child has disability
*Single Mother
*Child from U.S.: cbornus
*Child Race (Hispanic, White, Black, control for all X): chispan, cwhite, cblack, camind, casian, cpaci, craceoth
*Community Type (urban, suburban): ziplocl
*Child Age: age2002 -> cage

***Important Variables for Summary Stats)***
*sreason (parent's reason for homeschooling)
*sefuture
***********************************

*Homeschooling Reason

gen reason=substr(hsmost,1,2)
destring reason, replace
replace reason=. if reason==-1

tab reason

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


by homeschool, sort : summarize hhinc


*Mother's Education (use i.momed)

* 1: less than HS diploma (8)   2: HS Grad (13)    3: Some college/voc (15)   4: College grad (17)   5: Grad school (20)

gen momeduc2=.
replace momeduc2=8 if substr(momeduc,1,1)=="1"
replace momeduc2=13 if substr(momeduc,1,1)=="2"
replace momeduc2=15 if substr(momeduc,1,1)=="3"
replace momeduc2=17 if substr(momeduc,1,1)=="4"
replace momeduc2=20 if substr(momeduc,1,1)=="5"


// Birth Mother

gen birthmother=substr(momtype1,1,2)
destring(birthmother), replace
replace birthmother=. if birthmother==-1
replace birthmother=. if birthmother==2
replace birthmother=. if birthmother==3
replace birthmother=. if birthmother==4
replace birthmother=. if birthmother==5


replace momeduc2=momeduc2*birthmother


*Child's Sex

gen cmale=substr(sex,1,1)
destring cmale, replace
replace cmale=0 if cmale==2


*Parent's Average (remember to only use mother age in final reg's)

*momage1   dadage1

replace momage=. if momage==-1
replace dadage=. if dadage==-1
replace momage=momage*birthmother


*Child has disability: Discarded because only parents of homeschoolers answered the question

*Child Born in U.S.

gen cunited=substr(cbornus,1,1)
destring(cunited), replace
replace cunited=0 if cunited==2
replace cunited =0 if cunited==3




*Child Race (use i.crace)

gen craces=substr(crace,1,2)
destring craces, replace

tab crace if homeschool==1
tab crace if homeschool==0


*Child Age

rename age2002 cage
tab cage if homeschool==1
tab cage if homeschool==0

*Census Region

gen region=substr(cenreg,1,1)
destring region, replace


*Community Type (1 is urban, 2 is suburban, 3 is rural)-- use i.zip

gen zip=substr(zipurban,1,1)
destring zip, replace


*Mom marital status (i.maritalmom)

gen maritalmom=substr(momstat,1,2)
destring maritalmom, replace
replace maritalmom=. if maritalmom==-1
replace maritalmom=maritalmom*birthmother

*1: Married or Remarried
*2: Separated
*3: Divorced
*4: Widowed
*5: Never Married


gen motherfather=0
replace motherfather=1 if maritalmom==1



*Religious

gen religious=substr(forelig,1,1)
destring religious, replace
replace religious=0 if religious==2


*Number of Siblings

tab numsibs if homeschool==1
tab numsibs if homeschool==0

*Year

gen year=2003






**For Combined Data and Summary Statistics Only:
 
 keep numsibs cage homeschool cmale cwhite cblack hhinc lnhhinc momed religious region
 momage cunited year maritalmom zip
 
 rename momed meducation
 
 gen marriedmom=.
 replace marriedmom=1 if maritalmom==1
 replace marriedmom=0 if maritalmom > 1
 
 rename region cenreg
 rename zip ziplocl
 
 
 ** Summary Statistics
sum * if homeschool==1
sum * if homeschool==0


 
 
 
 
 
logistic homeschool lnhhinc meducation marriedmom momage cage cmale numsib cwhite cblack religious i.cenreg i.ziplocl, robust 
outreg2 using hpaper2.doc, stnum(replace coef=exp(coef), replace se=coef*se) cti(odds ratio) 



 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
