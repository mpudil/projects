BYU TV

filename tvdetail '/home/mitchellpudil30/STAT_381/byutv1.csv';

proc format;
	invalue viewers
	"<<" = 0;
run;


data ratings;
	infile tvdetail delimiter=',' missover dsd lrecl=32767 firstobs=7;
	informat time time10.;
	informat VAR3 viewers.; informat VAR6 viewers.; informat VAR9 viewers.;
	input time $ VAR2 $ VAR3 $ VAR4 $ VAR5 $ VAR6 $ VAR7 $ VAR8 $ VAR9 $ VAR10 $;
	lagtime = LAG(time);
	if time = . then time1 = lagtime + 900;
	else time1 = time;
run;

/* Create separate tables for each state*/
proc sql;
	create table albuquerque as
	select time1 as Time format time5., VAR2, VAR3 from ratings;
	
	create table saltlake as
	select time1 as Time format time5., VAR2, VAR9 from ratings;
	
	create table all as 
	select albuquerque.Time, albuquerque.VAR2 as program, VAR3 as NM, saltlake.Time, VAR9 as Utah
	from albuquerque, saltlake 
	where albuquerque.Time=saltlake.Time;
	
	
	/* Create a table that has time, program, SLC, total for Mtn West */
	create table audience1 as
	select time1, VAR2 as program, VAR9 as SaltLake, 
		sum(VAR3, VAR6, VAR9) as MWTotal
	from ratings;
quit;