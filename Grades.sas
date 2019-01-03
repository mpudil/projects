/* Final */

ods html body= "/home/mitchellpudil30/sasuser.v94/FinalReports.html";

title "Report 1";
proc format;
	invalue GPAgrade
	"A" = 4.0
	"A-" = 3.7
	"B+" = 3.4
	"B" = 3.0
	"B-" = 2.7
	"C+" = 2.4
	"C" = 2.0
	"C-" = 1.7
	"D+" = 1.4
	"D" = 1.0
	"D-" = 0.7
	other=0.0
;
run;

proc format;
	value classes
	0<-30 = "Freshman"
	30<-60 = "Sophmore"
	60<-90 = "Junior"
	90<-high = "Senior"
	other = "?"
;
run;

proc format;
	value $semester
	1 = "Winter"
	2 = "Winter2"
	3 = "Spring"
	4 = "Summer"
	5 = "Fall"
	6 = "Fall2"
;
run;


data final;
	infile "/home/mitchellpudil30/sasuser.v94/HW 5/*.txt" dlm="@";
	length ID $ 5 Course $ 10;
	input ID $ Date Course $ Credit Grade $;
	Year=substrn(Date,2,2);
	Term=substrn(Date,1,1);
	GPAgrade = input(Grade,GPAgrade.);
run;


proc sql;
	create table grades as
	select ID, Year, Term, sum(Credit*GPAgrade)/sum(Credit) as GPA,
		sum(Credit*GPAgrade) as num, sum(Credit) as den
	from final
	where Grade not in ("P", "I", "T", "W")
	group by ID, Year, Term;
quit;

data cumulative;
	set grades;
	by ID;
	retain cumnum 0;
	retain cumden 0;
	if first.ID then cumnum=0;
	if first.ID then cumden=0;
	cumnum = cumnum + num;
	cumden = cumden + den;
	cumgpa = cumnum/cumden;
run;

%macro gradem(grade);
if Grade in ("&grade.+", "&grade.", "&grade.-") then &grade.=1;
%mend;

data report;
	set final;
	A=0; B=0; C=0; D=0; E=0; W=0;
	%gradem(A); %gradem(B); %gradem(C); %gradem(D);
 	if Grade in ("E", "IE", "UW", "WE") then E=1;
 	if Grade = "W" then W=1;
 run;


proc sql;
	create table gradesum as 
	select ID, sum(A) as A, sum(B) as B, sum(C) as C, sum(D) as D, sum(E) as E, sum(W) as W
	from report
	group by ID;
quit;

proc sql;
	create table report1 as
	select gradesum.ID, A, B, C, D, E, W, Year, Term, GPA, cumgpa 
	from gradesum, cumulative
	where gradesum.ID = cumulative.ID;
quit;



data bystudent;
set final;
GradePoint=input(Grade,GPAgrade.);
	if Grade="P" then do;
		EarnedHours = Credit;
		GradedHours = 0;
		end;
	if Grade in ("E", "IE", "UW", "WE") then do;
		EarnedHours = 0;
		GradedHours = Credit;
		end;
	if Grade in ("W", "I", "T", "NS") then do;
		EarnedHours= 0;
		GradedHours= 0;
		end;
	if Grade in ("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-") then do;
		EarnedHours=Credit;
		GradedHours=Credit;
		end;
run;


proc sql;
	create table final2 as 
	select ID, Year, Term, sum(GradedHours*GradePoint)/sum(GradedHours) as GPA,
	sum(GradedHours) as Ghours, sum(EarnedHours) as EHours,
	sum(GradePoint*Gradedhours) as GPAPoints
	from bystudent
	group by ID, Year, Term;
quit;
	

data report1pt1; 
	set final2;
	retain CGhours 0;
	retain Cpoints 0;
	retain CEhours 0;
	if first.ID then do;
		CGhours = 0;
		Cpoints = 0;
		CEhours=0;
		end;
	CGhours = CGhours + Ghours;
	Cpoints = Cpoints + GPApoints;
	CEhours=CEhours + Ehours;
	CGPA = Cpoints/CGhours;
	Class=put(CEhours,classes.);
	Sem = put(Term, $semester.);
	by ID;
run;
	
proc sql;
	create table justletters as
	select ID, A, B, C, D, E, W
	from gradesum;
quit;

proc sql;
	create table repeats as
	select ID,
	count(Course) - count(DISTINCT Course) as repeats
	from final
	group by ID;
quit;

proc sql;
	create table finalreport1 as
	select report1pt1.ID, Year, Sem, GPA, CGPA, CEhours, CGhours, repeats, Class, A, B, C, D, E, W
	from justletters, report1pt1, repeats
	where justletters.ID=report1pt1.ID=repeats.ID;
quit;

proc report data=finalreport1;
	format CGPA 6.2;
run;


title "Report 2";

proc sql;
	create table Coursetable as
	select ID, Course 
	from final;
quit;
proc sql;
	create table overall as
	select finalreport1.ID, Year, Sem, Course, GPA, CGPA, CEhours, CGhours, repeats, Class, A, B, C, D, E, W
	from finalreport1, Coursetable
	where finalreport1.ID = Coursetable.ID;
quit;
	

data mathstat;
	set overall;
	if substr(Course,1,4) in ("MATH", "STAT") then output;
run;

data reportmathstat;
	set mathstat;
	by ID;
	if last.ID then output;
run;

data mathreport;
	set reportmathstat;
	Topic="Math/Stat";
run;

data reportfinal;
	set finalreport1;
	by ID;
	if last.ID then output;
run;


data finalreport2;
	set reportfinal;
	Topic="Overall";
run;

proc sql;
	create table t1 as select ID, Year, Sem, CGPA, CEHours, CGHours, Repeats, Class, A, B, C, D, E, W, Topic
	from mathreport;
quit;

proc sql;
	create table t2 as select ID, Year, Sem, CGPA, CEHours, CGHours, Repeats, Class, A, B, C, D, E, W, Topic
	from finalreport2;
quit;

data new;
	set t1 t2;
run;
	
proc report data=new;
	format CGPA 6.2;
run;


title "Report 3";

proc sort data= finalreport1;
	where 60<CGhours<130;
	by DESCENDING CGPA;
run;

proc sql noprint;
	select round(count(distinct ID)*.1) into :ob
	from finalreport1;
quit;

proc report data=finalreport1 (obs= &ob);
	column ID CGPA;
	format CGPA 6.2;
run;

title "Report 4";

proc sort data= reportmathstat;
	where 20<CGhours;
	by DESCENDING CGPA;
run;

proc sql noprint;
	select round(count(distinct ID)*.1) into :op
	from reportmathstat;
quit;

proc report data=reportmathstat (obs=&op);
	column ID CGPA;
	format CGPA 6.2;
run;


ods html close;
