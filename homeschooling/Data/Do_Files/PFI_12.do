*************************************************************************
******************* HOMESCHOOLING PROJECT *******************************
*************************************************************************

***FILENAME: PFI_12.do
***AUTHOR: Mitchell Pudil
***DATA COLLECTED FROM: United States Department of Education
***						National Center for Education Statistics
						
**************************************************************************


*************Variables***********

***Dependent Variable***
*homeschlx (if child is homeschooled- 1 if yes)
gen homeschool=substr(homeschlx,1,2)
destring homeschool, replace
drop if homeschool==-1
replace homeschool=0 if homeschool==2

***Independent Variables***


*Child is Male

gen cmale=substr(csex,1,1)
destring cmale, replace
replace cmale=0 if cmale==2


*Child Race

gen crace=substr(raceethn,1,1)
destring crace, replace

gen cWhite=0
replace cWhite=1 if crace==1

gen cBlack=0
replace cBlack=1 if crace==2

gen cHispanic=0
replace cHispanic=1 if crace==3

gen cOther=0
replace cOther=1 if crace==4


*Household Income

gen hhinc2=substr(ttlhhinc,1,2)
destring hhinc2, replace

replace hhinc2=2500 if hhinc2==1
replace hhinc2=15000 if hhinc2==2
replace hhinc2=25000 if hhinc2==3
replace hhinc2=35000 if hhinc2==4
replace hhinc2=45000 if hhinc2==5
replace hhinc2=55000 if hhinc2==6
replace hhinc2=67500 if hhinc2==7
replace hhinc2=87500 if hhinc2==8
replace hhinc2=125000 if hhinc2==9
replace hhinc2=200000 if hhinc2==10

gen lnhhinc2=ln(hhinc2)

*Child Age
rename age2011 cage

*Number of Siblings: 
rename numsibsx numsib





//Drop if no mothers or two biological mothers


gen bio=substr(p1rel,1,1)
destring bio, replace
replace bio=0 if bio>1



gen bio2=substr(p2rel,1,2)
destring bio2, replace
replace bio2=0 if bio2==-1
replace bio2=0 if bio2>1



gen p1mom=0
replace p1mom=1 if p1sex=="2 Female"
gen biomom1=bio*p1mom



gen p2mom=0
replace p2mom=1 if p2sex=="2 Female"
gen biomom2=bio2*p2mom


drop if biomom1+biomom2==0
drop if biomom1+biomom2==2



*Mother Education (Parent 1)

gen educ1=substr(p1educ,1,10)
gen edyears1=.
replace edyears1=6 if educ1=="1 8th grad"
replace edyears1=11 if educ1=="2 High sch"
replace edyears1=13 if educ1=="3 High sch"
replace edyears1=15 if educ1=="4 Vocation"
replace edyears1=15 if educ1=="5 Some col"
replace edyears1=15 if educ1=="6 Associat"
replace edyears1=17 if educ1=="7 Bachelor"
replace edyears1=18 if educ1=="8 Some gra"
replace edyears1=19 if educ1=="9 Master's"
replace edyears1=21 if educ1=="10 Doctora"
replace edyears1=21 if educ1=="11 Profess"


gen momeduc1=edyears1*biomom1



*Mother Education (Parent 2)

gen educ2=substr(p2educ,1,10)
gen edyears2=.
replace edyears2=6 if educ1=="1 8th grad"
replace edyears2=11 if educ1=="2 High sch"
replace edyears2=13 if educ1=="3 High sch"
replace edyears2=15 if educ1=="4 Vocation"
replace edyears2=15 if educ1=="5 Some col"
replace edyears2=15 if educ1=="6 Associat"
replace edyears2=17 if educ1=="7 Bachelor"
replace edyears2=18 if educ1=="8 Some gra"
replace edyears2=19 if educ1=="9 Master's"
replace edyears2=21 if educ1=="10 Doctora"
replace edyears2=21 if educ1=="11 Profess"


gen momeduc2=edyears2*biomom2

*Total Mother's Education

gen meducation=momeduc1+momeduc2


**Religious Attendance

gen religious=substr(fogroupx,1,1)
destring religious, replace
replace religious=0 if religious==2





*** Community Type

gen ziptype=substr(ziplocl,1,2)
destring ziptype, replace

gen urban=0
replace urban=1 if ziptype==11|12|13|31|32|33

gen suburban=0 
replace suburban=1 if ziptype==21
replace suburban=1 if ziptype==22
replace suburban=1 if ziptype==23

gen rural=0
replace rural=1 if ziptype==41
replace rural=1 if ziptype==42
replace rural=1 if ziptype==43


gen zip=0
replace zip=1 if urban==1
replace zip=2 if suburban==1
replace zip=3 if rural==1


***Census Region

gen region=substr(cenreg,1,1)
destring region, replace



**Mom Age

gen momage1=p1age*biomom1
gen p2Age=substr(p2age,1,2)
destring p2Age, replace
replace p2Age=0 if p2Age==-1
replace p2Age=0 if p2Age==.


gen momage2=p2Age*biomom2
replace momage2=0 if momage2==.
gen momage=momage1+momage2

*Marital Status 1

gen marital1 = substr(p1mrsta,1,1)
destring marital1, replace

replace marital1=9 if marital1==2
replace marital1=10 if marital1==3
replace marital1=2 if marital1==4
replace marital1=3 if marital1==5
replace marital1=4 if marital1==6
replace marital1=5 if marital1==7
replace marital1=6 if marital1==9
replace marital1=7 if marital1==10

gen marital1mom=biomom1*marital1  // Mom (Parent 1) Marital Status




*Marital Status 2

gen marital2 = substr(p2mrsta,1,2)
destring marital2, replace
replace marital2=0 if marital2==-1
replace marital2=9 if marital2==2
replace marital2=10 if marital2==3
replace marital2=2 if marital2==4
replace marital2=3 if marital2==5
replace marital2=4 if marital2==6
replace marital2=5 if marital2==7
replace marital2=6 if marital2==9
replace marital2=7 if marital2==10

gen marital2mom=biomom2*marital2  // Mom (Parent 2) Marital Status

*Mom marital status 

gen maritalmom=marital1mom+marital2mom

gen marriedmom=.
replace marriedmom=1 if maritalmom==1
replace marriedmom=0 if maritalmom>1

*Child born in U.S.

gen cunited=substr(cplcbrth,1,1)
destring cunited, replace
replace cunited=0 if cunited>1




*Year

gen year=2012



**For Combined Regression ONLY

keep year cWhite cBlack region homeschool lnhhinc2 meducation cmale momage cage cunited zip religious marriedmom numsib
rename cWhite cwhite
rename cBlack cblack 
rename lnhhinc2 lnhhinc 
rename zip ziplocl 
rename region cenreg


logistic homeschool lnhhinc meducation marriedmom i.cenreg i.ziplocl momage cage cmale numsib cwhite cblack religious, robust 
outreg2 using hspaper9.doc, stnum(replace coef=exp(coef), replace se=coef*se) cti(odds ratio) 



