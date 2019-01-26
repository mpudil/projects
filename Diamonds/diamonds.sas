/* Diamond Prep Code*/

data diamonds;
	input Carat Color $ Clarity $ Cert $ Price;
	datalines;
                    0.3  D VS2  GIA  1302
                    0.3  E VS1  GIA  1510
                    0.3  G VVS1 GIA  1510
                    0.3  G VS1  GIA  1260
                    0.31 D VS1  GIA  1641
                    0.31 E VS1  GIA  1555
                    0.31 F VS1  GIA  1427
                    0.31 G VVS2 GIA  1427
                    0.31 H VS2  GIA  1126
                    0.31 I VS1  GIA  1126
                    0.32 F VS1  GIA  1468
                    0.32 G VS2  GIA  1202
                    0.33 E VS2  GIA  1327
                    0.33 I VS2  GIA  1098
                    0.34 E VS1  GIA  1693
                    0.34 F VS1  GIA  1551
                    0.34 G VS1  GIA  1410
                    0.34 G VS2  GIA  1269
                    0.34 H VS1  GIA  1316
                    0.34 H VS2  GIA  1222
                    0.35 E VS1  GIA  1738
                    0.35 F VS1  GIA  1593
                    0.35 G VS1  GIA  1447
                    0.35 H VS2  GIA  1255
                    0.36 F VS1  GIA  1635
                    0.36 H VVS2 GIA  1485
                    0.37 F VS2  GIA  1420
                    0.37 H VS1  GIA  1420
                    0.4  F VS1  GIA  1911
                    0.4  H VS1  GIA  1525
                    0.41 F VS1  GIA  1956
                    0.43 H VVS2 GIA  1747
                    0.45 I VS1  GIA  1572
                    0.46 E VVS2 GIA  2942
                    0.48 G VVS2 GIA  2532
                    0.5  E VS1  GIA  3501
                    0.5  E VS1  GIA  3501
                    0.5  F VVS2 GIA  3501
                    0.5  F VS1  GIA  3293
                    0.5  G VS1  GIA  3016
                    0.51 F VVS2 GIA  3567
                    0.51 G VS1  GIA  3205
                    0.52 D VS2  GIA  3490
                    0.52 E VS1  GIA  3635
                    0.52 F VVS2 GIA  3635
                    0.52 F VS1  GIA  3418
                    0.53 D VS1  GIA  3921
                    0.53 F VVS2 GIA  3701
                    0.53 F VS1  GIA  3480
                    0.53 G VVS2 GIA  3407
                    0.54 E VS1  GIA  3767
                    0.54 F VVS1 GIA  4066
                    0.55 E VVS2 GIA  4138
                    0.55 F VS1  GIA  3605
                    0.55 G VVS2 GIA  3529
                    0.56 F VS1  GIA  3667
                    0.56 I VVS2 GIA  2892
                    0.57 G VVS2 GIA  3651
                    0.59 G VVS2 GIA  3773
                    0.6  F VS1  GIA  4291
                    0.62 E VVS1 GIA  5845
                    0.63 G VVS2 GIA  4401
                    0.64 G VVS1 GIA  4759
                    0.66 H VVS1 GIA  4300
                    0.7  F VS1  GIA  5510
                    0.7  G VS1  GIA  5122
                    0.7  H VVS2 GIA  5122
                    0.7  I VS2  GIA  3861
                    0.71 F VVS2 GIA  5881
                    0.71 F VS1  GIA  5586
                    0.71 F VS2  GIA  5193
                    0.71 H VVS2 GIA  5193
                    0.72 F VS2  GIA  5263
                    0.8  I VVS2 GIA  5441
                    0.82 I VS2  GIA  4948
                    0.84 H VS2  GIA  5705
                    0.85 F VS2  GIA  6805
                    0.86 H VVS2 GIA  6882
                    0.89 H VS1  GIA  6709
                    0.9  I VVS2 GIA  6682
                    0.5  E VS1  GIA  3501
                    0.5  G VVS1 GIA  3432
                    0.51 F VVS1 GIA  3851
                    0.55 H IF   GIA  3605
                    0.56 E VS1  GIA  3900
                    0.57 H VVS1 GIA  3415
                    0.6  H IF   GIA  4291
                    0.63 E IF   GIA  6512
                    0.7  E VS1  GIA  5800
                    0.7  F VVS1 GIA  6285
                    0.7  F VS2  GIA  5122
                    0.7  F VS2  GIA  5122
                    0.7  G VS1  GIA  5122
                    0.7  H VVS2 GIA  5122
                    0.71 D VS1  GIA  6372
                    0.71 E VS1  GIA  5881
                    0.71 H VVS2 GIA  5193
                    0.72 E VS1  GIA  5961
                    0.72 H VVS1 GIA  5662
                    0.73 E VS2  GIA  5738
                    0.73 H VS1  GIA  5030
                    0.73 H VS1  GIA  5030
                    0.73 I VVS1 GIA  4727
                    0.73 I VS1  GIA  4221
                    0.74 G VVS2 GIA  5815
                    0.74 H VS2  GIA  4585
                    0.75 D VVS2 GIA  7368
                    0.75 I VVS2 GIA  4667
                    0.75 I VS1  GIA  4355
                    0.76 D IF   GIA  9885
                    0.77 F VVS1 GIA  6919
                    0.78 H VS1  GIA  5386
                    0.8  I VS2  GIA  4832
                    0.83 E VS2  GIA  7156
                    0.9  F VS1  GIA  7680
                    1    D VVS1 GIA 15582
                    1    D VS1  GIA 11419
                    1    E VS1  GIA 10588
                    1    E VS2  GIA  9757
                    1    F IF   GIA 13913
                    1    F VVS2 GIA 10588
                    1    F VS1  GIA 10713
                    1    F VS2  GIA  9480
                    1    G VVS2 GIA  9896
                    1    G VS1  GIA  9619
                    1    G VS2  GIA  9169
                    1    G VS2  GIA  9203
                    1    H VS2  GIA  8788
                    1    I VS1  GIA  8095
                    1    I VS2  GIA  7818
                    1.01 D VVS1 GIA 16008
                    1.01 E VS1  GIA 10692
                    1.01 E VS2  GIA  9853
                    1.01 F VS1  GIA 10272
                    1.01 F VS2  GIA  9573
                    1.01 H VS1  GIA  9153
                    1.01 H VS2  GIA  8873
                    1.01 I VVS1 GIA  8873
                    1.01 I VVS2 GIA  8455
                    1.01 I VS2  GIA  7895
                    1.02 F VS1  GIA 10372
                    1.02 F VS2  GIA  9666
                    1.02 G VVS2 GIA 10090
                    1.03 E VS1  GIA 10900
                    1.04 F VS1  GIA 10571
                    1.04 I IF   GIA  9563
                    1.05 I VVS2 GIA  8781
                    1.06 G VS2  GIA  9743
                    1.06 H VS2  GIA  9302
                    1.07 I VVS2 GIA  8945
                    1.1  H VS2  GIA  9646
                    0.18 F VVS1 IGI   823
                    0.18 F VVS2 IGI   765
                    0.18 G IF   IGI   803
                    0.18 G IF   IGI   803
                    0.18 G VVS2 IGI   705
                    0.18 H IF   IGI   725
                    0.19 D VVS2 IGI   967
                    0.19 E IF   IGI  1050
                    0.19 F IF   IGI   967
                    0.19 F VVS1 IGI   863
                    0.19 F VVS2 IGI   800
                    0.19 G IF   IGI   842
                    0.19 G VVS1 IGI   800
                    0.19 H IF   IGI   758
                    0.2  D VS1  IGI   880
                    0.2  G IF   IGI   880
                    0.2  G VS1  IGI   705
                    0.2  G VS2  IGI   638
                    0.21 D VS1  IGI   919
                    0.21 E IF   IGI  1149
                    0.21 F IF   IGI  1057
                    0.21 G IF   IGI   919
                    0.22 E IF   IGI  1198
                    0.23 E IF   IGI  1248
                    0.23 F IF   IGI  1147
                    0.23 G IF   IGI   995
                    0.24 H IF   IGI  1108
                    0.25 F IF   IGI  1485
                    0.25 G IF   IGI  1283
                    0.25 H IF   IGI  1149
                    0.25 I IF   IGI  1082
                    0.26 F IF   IGI  1539
                    0.26 F VVS1 IGI  1365
                    0.26 F VVS2 IGI  1260
                    0.26 I IF   IGI  1121
                    0.27 F IF   IGI  1595
                    0.27 H IF   IGI  1233
                    0.28 I IF   IGI  1199
                    0.29 G IF   IGI  1471
                    0.29 I IF   IGI  1238
                    0.3  E VVS2 IGI  1580
                    0.3  F VVS2 IGI  1459
                    0.3  G VVS1 IGI  1459
                    0.3  H VVS2 IGI  1218
                    0.3  I IF   IGI  1299
                    0.31 E VVS2 IGI  1628
                    0.31 F VVS1 IGI  1628
                    0.31 I IF   IGI  1337
                    0.32 H IF   IGI  1462
                    0.33 H IF   IGI  1503
                    0.34 F VVS1 IGI  1773
                    0.34 F VVS2 IGI  1636
                    0.35 F VVS1 IGI  1821
                    0.35 G VVS2 IGI  1540
                    0.4  G IF   IGI  2276
                    0.41 I VVS1 IGI  1616
                    0.41 I VVS2 IGI  1506
                    0.47 F VVS2 IGI  2651
                    0.48 F VS1  IGI  2383
                    0.5  G IF   IGI  3652
                    0.51 E VVS2 IGI  3722
                    0.51 F VVS1 IGI  3722
                    0.52 I IF   IGI  3095
                    0.55 F VVS2 IGI  3706
                    0.56 E VVS2 IGI  4070
                    0.56 G VVS2 IGI  3470
                    0.58 E VVS1 IGI  4831
                    0.58 F VVS1 IGI  4209
                    0.58 G VVS1 IGI  3821
                    0.7  G VVS1 IGI  5607
                    0.7  G VVS2 IGI  5326
                    0.71 D VS1  IGI  6160
                    0.76 F VVS2 IGI  6095
                    0.78 G VVS2 IGI  5937
                    1    H VVS2 IGI  9342
                    1.01 G VS1  IGI  9713
                    1.01 H VS2  IGI  8873
                    1.01 I VS1  IGI  8175
                    0.5  F VVS1 HRD  3778
                    0.5  G VVS1 HRD  3432
                    0.51 F VVS1 HRD  3851
                    0.52 E VS2  HRD  3346
                    0.52 H VVS1 HRD  3130
                    0.53 F VVS1 HRD  3995
                    0.53 F VVS2 HRD  3701
                    0.55 G VVS2 HRD  3529
                    0.56 F VS1  HRD  3667
                    0.56 F VS2  HRD  3202
                    0.57 F VS2  HRD  3256
                    0.57 H VVS1 HRD  3415
                    0.58 H IF   HRD  3792
                    0.6  G VS1  HRD  3925
                    0.6  G VS2  HRD  3421
                    0.6  H VVS1 HRD  3925
                    0.61 H VVS2 HRD  3616
                    0.62 I VVS2 HRD  3615
                    0.64 H VVS2 HRD  3785
                    0.65 I VVS2 HRD  3643
                    0.66 H VVS1 HRD  4300
                    0.7  E VVS1 HRD  6867
                    0.7  E VVS2 HRD  6285
                    0.7  G VVS1 HRD  5800
                    0.7  G VVS2 HRD  5510
                    0.7  H VS2  HRD  4346
                    0.71 G IF   HRD  6372
                    0.71 H VVS2 HRD  5193
                    0.72 H VVS1 HRD  5662
                    0.73 F VS2  HRD  5333
                    0.73 G VVS1 HRD  6041
                    0.74 H VVS1 HRD  5815
                    0.8  F IF   HRD  8611
                    0.8  F VS1  HRD  6905
                    0.8  G VVS2 HRD  6905
                    0.8  H VVS2 HRD  6416
                    0.8  H VS1  HRD  6051
                    0.81 E VVS1 HRD  8715
                    0.81 E VS2  HRD  6988
                    0.81 F VS1  HRD  6988
                    0.81 G VS1  HRD  6495
                    0.81 H IF   HRD  7358
                    0.82 F VS2  HRD  6572
                    0.82 G VVS2 HRD  7072
                    0.85 F VVS1 HRD  8359
                    0.85 F VS2  HRD  6805
                    0.85 G VVS1 HRD  7711
                    0.86 H VS2  HRD  5835
                    1    D VVS2 HRD 13775
                    1    E VVS1 HRD 14051
                    1    E VVS2 HRD 11419
                    1    E VS1  HRD 10588
                    1    F VVS1 HRD 11696
                    1    F VVS2 HRD 10588
                    1    G VVS1 HRD 10450
                    1    G VVS2 HRD  9896
                    1    G VS2  HRD  9203
                    1    H VVS1 HRD  9480
                    1    H VS1  HRD  9065
                    1    H VS2  HRD  8788
                    1    I VVS1 HRD  8788
                    1    I VVS2 HRD  8372
                    1    I VS1  HRD  8095
                    1    I VS2  HRD  7818
                    1.01 D VVS2 HRD 13909
                    1.01 E VVS2 HRD 11531
                    1.01 E VS1  HRD 10692
                    1.01 F VVS1 HRD 11811
                    1.01 F VS1  HRD 10272
                    1.01 G VVS2 HRD  9993
                    1.01 G VS2  HRD  9293
                    1.01 H VVS2 HRD  9433
                    1.01 H VS1  HRD  9153
                    1.01 I VVS1 HRD  8873
                    1.01 I VS1  HRD  8175
                    1.02 F VVS2 HRD 10796
                    1.06 H VVS2 HRD  9890
                    1.02 H VS2  HRD  8959
                    1.09 I VVS2 HRD  9107
					;
run;


* EDA; 

/*Graph of Carat and Price (not logged)*/
proc gplot data = diamonds;
	plot Price *Carat;
run;

* Model for just Price;
proc reg data = diamonds;
	model Price = Carat/ cli clm;
run;



/*utilize proc sql to transform the data with a log*/


proc sql;
create table diamonds_log as
	select * ,log(price) as log_price, log(carat) as log_carat
	from diamonds;
quit;
run;
/*Replicate the tail function */



* Plot log price and log Carat;
proc gplot data = diamonds_log;
	plot log_price *log_carat;
run;



/* We should use the log transformation because the graph shows that it represents a more linear effect compared to the non transformed model.
Since we will be using an addative model, the log transformed values are preferable*/



proc sql outobs = 5;
	select * 
	from diamonds_log;
run;


* Analysis;



proc reg data = diamonds_log;
	model log_price = log_carat/ clb r;
run;

/*The explanatory variable is log_carat and the response variable is log_price

log price = beta0 + beta1 * log_carat + e ~ N(0, sigma ^2)

The model suggests a statistically significant relationship between the carat size and the price of the ring with a log transformation (t-test)

B1 indicates that with a 1% increase in Carat we expect a increase of 1.5% in price all other things held constant

The 95% CI for carat is (1.500, 1.574) so there is a significant effect of carat on price.

The estimate model for price: price = 9.12775 + 1.537 * log_carrat size

To see publication quality graphic, see results viewer and select graph of predicted values 



*/

/*Prediction interval for 1 carrat diamond*/

data one_carat;
	input Carat Color $ Clarity $ Cert $ Price;
	datalines; 
	1 . . . .
	;
run;

proc sql;
	create table one_carat as 
		select *, log(carat) as log_carat, log(price) as log_price
		from one_carat;
	quit;
run;

data prediction_diamond;
	set diamonds_log one_carat;
run;

proc reg data = prediction_diamond;
	model log_price = log_carat/ cli clm;
run;



*Prediction interval for log price is (8.7952 9.4603) which in prices is (6602.48,12839.74);

proc reg data = diamonds_log;
	model log_price = log_carat/ cli clm;
run;


/* Compute R2 and use the value in reporting how well the model predicts Price.

Begin by including predicted values into diamonds_log dataset */

proc syslin data=diamonds_log out=pred;
	model log_price = log_carat;
	output predicted=log_pred_price;
run;

* Calculate exponentiated prediction;


data pred_price; set pred;
   pred_price = exp(log_pred_price);
run;

* Include mean of price as well in dataset;
proc sql;
	create table prediction as 
		select *, mean(Price) as mean_price
		from pred_price;
quit;


* Calculate SSE and SST;
proc sql;
	create table ss as 
		select *, sum((Price - pred_price)**2) as sse, sum((Price - mean_price)**2) as sst
		from prediction;
quit;


* Calculate R-squared;
proc sql;
	create table rsq as 
		select *, 1 - sse/sst as rsquared
		from ss;
quit;


/* So the R squared for Price is 0.905 which means that carat size explains 90.5% of the variation in the price;


Report summary statistics of the ‘absolute prediction error’ distribution (in Singapore $). 
When does the model predict really well? When does the model predict poorly, and what would you 
suggest to improve model prediction in these cases?

*/

proc sql;
	create table abs_pred_err as
		select *, abs(Price - pred_price) as Absolute_Prediction_Error
		from rsq;
quit;
run;

proc means data=abs_pred_err;
  var Absolute_Prediction_Error;
  title 'Summary Statistics for Absolute Prediction Error';
run;




/*

The Model predicts well when the carat size is smaller. As the carat size gets larger we have a higher variance. A way to fix this prediction 
is to get more data on significantly large diamonds so that we can more accurately predict these higher carat values

*/




*/ For publication quality graph of uncertainty:

 Get prediction interval table;


proc reg data=diamonds_log;
	model log_price = log_carat/ cli clm;
	var Price Carat;
	plot Price*Carat;
	ods output OutputStatistics = log_pred_lu;
run;

data predlu; set log_pred_lu;
   predl = exp(LowerCL);
   predu = exp(UpperCL);
   prediction = exp(PredictedValue);
   id = _N_;  
RUN;


data diamonds1; set diamonds;
      id = _N_;  
RUN;



proc sql;
	create table actual as
	select Carat, Price, id from diamonds1;
quit;

proc sql;
	create table p as
	select prediction, predl, predu, id from predlu;
quit;



data q; * For some reason, the join command wasn't quite working, so this is an alternative way of doing that;
   merge actual p;
   by id;
run;


proc plot data=q;
	plot Price*Carat='*'
		 prediction*Carat='-' / overlay box;
		 title 'Plot of Prediction and Actual Price';
run;
		 
		 
proc sql;
	create table r as select * from q order by Carat;
quit;


* Graph data with prediction and prediction intervals;

proc sgplot data=r;
	scatter x=Carat y=Price;
  	series x=Carat y=predl / legendlabel="Lower Bound" lineattrs=(color=lightblue pattern=dash) ;
  	series x=Carat y=predu / legendlabel="Upper Bound" lineattrs=(color=blue pattern=dash) ;
  	series x=Carat y=prediction / legendlabel="Predicted Value" lineattrs=(color=red pattern=solid) ;
  	yaxis label="Price";
  	title '95% Prediction Interval for Price|Carat';
run;






