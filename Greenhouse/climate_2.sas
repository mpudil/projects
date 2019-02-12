*SAS Climate

* Greenhouse Gases Data;

*library for permanet SAS dataset;
libname S381 '\\Client\H$\Desktop\Winter_2019\STAT 381\Data';

* Global temperature data;
data temp1;
	infile '\\Client\H$\Desktop\Winter_2019\STAT 381\Data\nasa.gov.GLB.Ts+dSST.txt'
		firstobs = 9;
	input x1-x20;
	* Note the lines between decades and the rows at the end need to be deleted;
	if x1^=.;
run;

*Change data from monthly different rows to row for each month;
data temp2;
	set temp1;

	year=x1;
	temp=x2; month=1; output;
	temp=x3; month=2; output;
	temp=x4; month=3; output;
	temp=x5; month=4; output;
	temp=x6; month=5; output;
	temp=x7; month=6; output;
	temp=x8; month=7; output;
	temp=x9; month=8; output;
	temp=x10; month=9; output;
	temp=x11; month=10; output;
	temp=x12; month=11; output;
	temp=x13; month=12; output;

	keep year month temp;
run;


* remove missing values for global temperature;
data temp2;
	set temp2;
	if temp^=.;
run;


data co2;
	infile '\\Client\H$\Desktop\Winter_2019\STAT 381\Data\nasa.gov.co2_mlo_surface-flask_1_ccgg_month.txt'
		firstobs = 71;
	input site year month co2;
	drop site;
run;

proc print data=co2;
run;

data ch4;
	infile '\\Client\H$\Desktop\Winter_2019\STAT 381\Data\nasa.gov.ch4_mlo_surface-flask_1_ccgg_month.txt'
		firstobs=71;
	input site year month ch4;
	drop site;
run;


* merge into a single permanent SAS dataset;


data n20;
	infile '\\Client\H$\Desktop\Winter_2019\STAT 381\Data\N2O.txt'
		firstobs=51;
	input year month n2o sd sample;
	drop sd sample;
run;


data hcfc;
	infile '\\Client\H$\Desktop\Winter_2019\STAT 381\Data\HCFC.txt'
		firstobs=51;
	input year month HCFC sd sample;
	drop sd sample;
run;

data sulfur;
	infile '\\Client\H$\Desktop\Winter_2019\STAT 381\Data\sulfur.txt'
		firstobs = 51;
	input site year month sulfur;
	drop site;
run;


data S381.Climate0;
	merge Temp2 Co2 Ch4;
	by year month;
run;

proc reg data=S381.Climate0;
	model temp = co2 ch4;
run; 

data S381.Climate1;
	merge Temp2 Co2 Ch4 N20 Hcfc Sulfur;
	by year month;
run;

proc print data=S381.Climate1;
run;


proc reg data=S381.Climate1;
	model temp = co2 ch4 n2o HCFC sulfur / vif;
run;












