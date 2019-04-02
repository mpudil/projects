*Avalanche Events;

libname S381 "/home/mitchellpudil30/STAT_381";

*Whenever you call this Libname with the same reference, it will be pointed to all of the datasets that have ever been called in that libname
could be put on a server, or a cloud and everyone can access it
Also helpful for large datasets so you don't have to run it again or if you might come back at a future time so you can divide the two pieces of work;


data avalancheEvents;
	informat date mmddyy12.;
	length Region $20.;
	length Location $100. ;
	infile "/home/mitchellpudil30/STAT_381/avalanche.csv"
		dlm = "," dsd;
	input Nid Date Region Location;
	mymonth = month(Date);
	myyear = year(Date);
	if region = "Salt Lake";
run;

data weather; 
	length Name $44. Date $9. DT32c $4. Snowc $6. Tminc $7.;
	infile "/home/mitchellpudil30/STAT_381/weather.csv" dlm = "," dsd missover firstobs = 2;
	input  name date dt32c snowc tminc;
	dt32 = input(DT32c, 3.0);
	SNOW = input(snowc, 5.0);
	TMIN = input(tminc, 5.0);
	SAS_Date = input(compress(Date, "-"), yymmn6.);
	myyear = year(SAS_Date);
	mymonth = month(SAS_Date);
run;

*Tally the avalanches for each month;

proc freq data = avalancheEvents;
	tables myyear*mymonth / out = avalancheCount(drop = percent rename=(count = Avalanches)) 
		noprint nopercent sparse /*there may be observations that have 0, keep the zero*/;
	where mymonth in (1,2,3,12) and myyear >=2000;
run;




*Merge weather and avalanche;
proc sql;
	create table Final as 
		select avalancheCount.Avalanches, weather.*
		from avalancheCount join weather
		on avalancheCount.myyear = weather.myyear and avalanchecount.mymonth = weather.mymonth;
run;

data S381.avalanche;
	set final;
run;


*EDA;

proc gplot data=avalancheCount;
      plot Avalanches*myyear;
      symbol1 v=star c=blue;
      title "Utah Avalanches by Year";
   run;
   quit;
   title;

proc gplot data=S381.avalanche;
      plot Avalanches*dt32;
      symbol1 v=star c=red;
      title "Utah Avalanches by Temperature";
   run;
   quit;
   title;
   
   
proc gplot data=S381.avalanche;
      plot Avalanches*SNOW;
      symbol1 v=star c=purple;
      title "Utah Avalanches by Snowfall";
   run;
   quit;
   title;
   
   
   *Estimate a Poisson Regression Model for Avalanche;

proc genmod data = S381.avalanche;
	model avalanches = Snow Tmin DT32 / dist = poisson link = log; /*Link is the connection between your linear model and your exponential family*/
	contrast 'test1' Snow 1 Tmin 0 DT32 0;
run;

*All of the coefficients are in the transformed version. For every extra inch of snow, we decrease the risk of avalanches by -.0279;


proc print data=january noobs;
var avalanches;
run;

data january;
	set S381.avalanche;
	if mymonth=1;
run;

data december;
	set S381.avalanche;
	if mymonth=12;
run;

proc print data=december noobs;
var avalanches;
run;

data february;
	set S381.avalanche;
	if mymonth=2;
run;

proc print data=february noobs;
var avalanches;
run;

data march;
	set S381.avalanche;
	if mymonth=3;
run;













