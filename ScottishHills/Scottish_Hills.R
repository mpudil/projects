# Mitchell Pudil
# STAT 330 
# Scottish Hill Races

hills <- read.table(header=TRUE, text = "
                       Race	Distance	Climb	Time
                       Greenmantle	2.5	650	16.083
                       Carnethy	6	2500	48.35
                       CraigDunain	6	900	33.65
                       BenRha	7.5	800	45.6
                       BenLomond	8	3070	62.267
                       Goatfell	8	2866	73.217
                       BensofJura	16	7500	204.617
                       Cairnpapple	6	800	36.367
                       Scolty	5	800	29.75
                       Traprain	6	650	39.75
                       LairigGhru	28	2100	192.667
                       Dollar	5	2000	43.05
                       Lomonds	9.5	2200	65
                       CairnTable	6	500	44.133
                       EildonTwo	4.5	1500	26.933
                       Cairngorm	10	3000	72.25
                       SevenHills	14	2200	98.417
                       KnockHill	3	350	78.65
                       BlackHill	4.5	1000	17.417
                       CreagBeag	5.5	600	32.567
                       KildconHill	3	300	15.95
                       MeallAnt-Suidhe	3.5	1500	27.9
                       HalfBenNevis	6	2200	47.633
                       CowHill	2	900	17.933
                       NBerwickLaw	3	600	18.683
                       CreagDubh	4	2000	26.217
                       Burnswark	6	800	34.433
                       LargoLaw	5	950	28.567
                       Criffel	6.5	1750	50.5
                       Acmony	5	500	20.95
                       BenNevis	10	4400	85.583
                       Knockfarrel	6	600	32.383
                       TwoBreweries	18	5200	170.25
                       Cockleroi	4.5	850	28.1
                       MoffatChase	20	5000	159.833
                       ")

tail(hills)

plot(hills$Distance, hills$Time)
identify(hills$Distance, hills$Time) 

plot(hills$Distance, hills$Time)
points(hills$Distance, hills$Time)
points(hills$Distance[31], hills$Time[31], col="red", pch=19)
par(mfrow=c(1,1))


plot(hills$Climb, hills$Time)
points(hills$Climb, hills$Time)
points(hills$Climb[31], hills$Time[31], col="red", pch=19)
par(mfrow=c(1,1))

#Can we compute a measure to help in these cases where it is "close"?


#Compute Regression Diagnostics

#fit model using all obs

out.hills <- lm(Time~Distance+Climb, data=hills)

#Compute leverage for Ben Nevis race

leverage.hills <- lm.influence(out.hills)$hat
subset(leverage.hills,hills$Race=="BenNevis")
#So column 31 is a little bit influential, but it could be worse. 

#Compute Cook's Distance for Moffat Chase race
cd.hills <- cooks.distance(out.hills)
subset(cd.hills,hills$Race=="MoffatChase")


par(mfrow=c(1,1))
plot(hills$Distance, hills$Time)
points(hills$Distance, hills$Time)
plot(hills$Distance, hills$Time)
points(hills$Distance[35], hills$Time[35], col="red", pch=19)
par(mfrow=c(1,1))

#Compute R-studentized residuals (residuals put onto a normalized distribution)
#for Cairn Table
R.hills <- rstudent(out.hills)
subset(R.hills,hills$Race=="CairnTable")


par(mfrow=c(1,2))
plot(hills$Distance, hills$Time)
points(hills$Distance[14], hills$Time[14], col="red", pch=19)
plot(hills$Distance, hills$Time)
points(hills$Distance[14], hills$Time[14], col="red", pch=19)
par(mfrow=c(1,2))

#Validata the normality assumption 
hist(R.hills) #Concern: outliers. Use KS test (see notes)
ks.test(R.hills, "pnorm")  # P value us 0.0366, so conlcude non-normality

#Is Kildcon Hill an outlier?
subset(R.hills,hills$Race=="KildconHill") #standardized value for race is 0.2
#so not an outlier.
#p value is 0.036


#Is MoffatChase influential?
subset(leverage.hills, hills$Race=="MoffatChase")  #Value 0.2, So Moffat Chase 
#is influential
subset(cd.hills,hills$Race=="MoffatChase") #Here, it looks like it is not influential
#We conclude it is influential because at least one of these procedures says it is

#Good influential or bad influential?
#Does it fit the pattern? Yes. So it is good influential.

# Is LairigGhru influential?
subset(leverage.hills, hills$Race=="LairigGhru")
subset(cd.hills,hills$Race=="LairigGhru")

#Good or bad?
par(mfrow=c(1,2))
plot(hills$Distance, hills$Time)
points(hills$Distance[11], hills$Time[11], col="red", pch=19)
plot(hills$Climb, hills$Time)
points(hills$Climb[11], hills$Time[11], col="red", pch=19)
par(mfrow=c(1,2))
#Bad because it is an outlier with climb. If we throw it out, we lose data
# but our model is better able to predict short, modest climbs.


#Is CowHill an outlier?
subset(R.hills,hills$Race=="CowHill")
# p-value for H0: CowHill not an outlier
2*(1-pnorm(0.314)) #Fail to reject. Nothing wrong with CowHill



#Is KnockHill an outlier?
subset(R.hills,hills$Race=="KnockHill")
# p-value for H0: KnockHill not an outlier
2*(1-pnorm(7.6108)) #Reject H0. KnockHill is an outlier for sure (p < 0.001)


#Summarization of data issues
#Outliers/non-normality: KnockHill, BensofJura
#Bad Influential: LairigGhru, BensofJura

























