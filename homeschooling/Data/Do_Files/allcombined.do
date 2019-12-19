*************************************************************************
******************* HOMESCHOOLING PROJECT *******************************
*************************************************************************

***FILENAME: allcombined.do
***AUTHOR: Mitchell Pudil
***DATA COLLECTED FROM: United States Department of Education
***						National Center for Education Statistics 
						
**************************************************************************



*****COMBINED DATA NOTES******   

**cenreg**
*1: Northeast
*2: South
*3: Midwest
*4: West

**zip** (double check this)
*1: Urban
*2: Suburban
*3: Rural

*Momed
*1: Less than high school
*2: HS diploma
*3: Some college
*4: Bachelor's
*5: Post-grad

**crace*

*1: White only
*2: Black only
*3: Hispanic only
*4: Other 

*Marital Status*

*1: Married or Remarried
*2: Separated
*3: Divorced
*4: Widowed
*5: Never Married
*6: Domestic Partnership (2012 only)
*7: Living with a partner (2012 only)


// Logistic Regressions
ssc install outreg2
ssc install estout


clogit homeschool lnhhinc meducation marriedmom momage cage cmale numsib cwhite cblack religious i.year
xtlogit homeschool lnhhinc i.year meducation marriedmom momage cage cmale numsib cwhite cblack religious , robust 



gen largefam=0
replace largefam=1 if numsib > 2

gen large_hhinc = lnhhinc*largefam



//All Years
logistic homeschool lnhhinc meducation marriedmom i.cenreg i.ziplocl i.year momage numsib religious, robust 
outreg2 using hsp.doc, stnum(replace coef=exp(coef), replace se=coef*se) cti(odds ratio) 



// 2003 thru 2012 Model

/* CAUTION WITH NEXT LINE OF CODE: WILL DELETE DATA: */
drop if year < 2016 

logistic homeschool lnhhinc meducation marriedmom i.cenreg i.ziplocl i.year momage numsib religious, robust 
outreg2 using hsp.doc, stnum(replace coef=exp(coef), replace se=coef*se) cti(odds ratio) 












