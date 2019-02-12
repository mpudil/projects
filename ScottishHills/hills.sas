*Hills;

filename webhills url "http://www.statsci.org/data/general/hills.txt";

* Atkinsons dataset;
data hillraces;
	infile webhills firstobs=2 delimiter='09'x;
	length race $15;
	input race $ distance climb time;
run;



* Scatter plots;
proc SGPLOT data=hillsdata;
	scatter x=distance y=time / datalabel=race;
run;


proc SGPLOT data=hillsdata;
	scatter x=climb y=time / datalabel=race;
run;

proc reg data=hillsdata;
	model time = climb distance / vif r influence;
	output out=resids student=sy;
run;

*Histogram of studentized residuals;

PROC chart DATA = resids;
vbar sy;
RUN;

* S-W normality test;

proc univariate data=resids NORMAL;
	var sy;
run;

proc print data=resids;
run;

