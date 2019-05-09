############
# Final Prject for Data Science for Economsits, Spring 2019: Evalutating QB Contract Value
############

############ Source Code/README


### First, import the 3 data required to run the code. The .CSV files are located at www.WHATever the URL is for the repository

ALL.QB <- read.csv("ALLqb2018withsalary.csv")
ROOKIE.QB <- read.csv("ROOKIEqb2018withsalary.csv")
VET.QB <- read.csv("VETqb2018withsalary.csv")

### Library stargazer ttach the datasets you wish to work with at the time. Do not forget to detach when moving between datasets. Let's begin with the ALL.QB

library(stargazer)
attach(ALL.QB)

### Use the model below to determine the p-values for each variable in the . After each interation removed the least significant variable. Repeat this
### until all remaining variables are at least p < 0.10 significance. One "*" or more will appear by each variable when this has been achieved.

# Below is the model with all variables included
ALL.MODEL<- lm(log(SALARY) ~ Age + G + GS + Cmp. + Yds + TD + TD. + Int + Int. + Lng + Y.A + AY.A + Y.C + Y.G + Rate + QBR + Sk 
                              + Yds.1 + NY.A + ANY.A + Sk. + X4QC + GWD, data=ALL.QB)
summary(ALL.MODEL)
# This is the model after all remaining vaiables are significant
ALL.MODEL <- lm(log(SALARY) ~ Age + G + Yds + Y.A + Y.G
               + X4QC + GWD, data=ALL.QB)

### I'm not really sure why I have to do log other than for the coefficients. My project is intented to judge the difference in expected salary
### and actually salary so that is how I am going to proceed. 
summary(ALL.MODEL)

### Now let's see how the model predicts a quarterback's worth both in terms of log and in absolute terms. Big Ben Roethlisberger had the most
### passing attempts for the 2018 season so he provides us with the largest sample size

fit.ALL<-ALL.MODEL
# Log
predict(fitBEN, newdata = data.frame(Age=36,G=16,Yds=5129,Y.A=7.6,Y.G=320.6,X4QC=2,GWD=3))

###
# Absolute
ALL.MODEL <- lm((SALARY) ~ Age + G + Yds + Y.A + Y.G
                + X4QC + GWD, data=ALL.QB)
fit.ALL<-ALL.MODEL
predict(fit.ALL, newdata = data.frame(Age=36,G=16,Yds=5129,Y.A=7.6,Y.G=320.6,X4QC=2,GWD=3))

### Now substract Big Ben's expected salary from his observed salary.

# Absolute
head(ALL.QB)
24442226-21850000
### Looks like Big Ben was underpaid $2,592,226 last year according to this model! 

# Percentage difference
(24442226/21850000)-1

### The model was only off 11.86%, but given the nature of predictions, this is not too far off.

### This process can be used for any quaterback in their respective model, just make sure you are using the correct model as well as the correct dataset
### i.e. use the Rookie model with the rookie dataset.

####################
# END OF README, BEGINNING TO ENTER ALL DATE BY HAND
####################

stargazer(ALL.QB, align = TRUE,single.row = TRUE,font.size = ,column.sep.width = "1pt")
attach(ALL.QB)
print(ALL.QB)

luck.all <- predict(fit.ALL, newdata = data.frame(Age=29,G=16,Yds=4593,Y.A=7.2,Y.G=287.1,X4QC=3,GWD=3))
luck.all-24594000
(luck.all/24594000)-1
  
ryan.all<-predict(fit.ALL, newdata = data.frame(Age=33,G=16,Yds=4924,Y.A=8.1,Y.G=307.8,X4QC=1,GWD=1))
ryan.all-30000000
(ryan.all/30000000)-1

cousins.all<-predict(fit.ALL, newdata = data.frame(Age=30,G=16,Yds=4298,Y.A=7.1,Y.G=268.6,X4QC=1,GWD=0))
cousins.all-28000000
(cousins.all/28000000)-1

head(ALL.QB)
rodgers.all<- predict(fit.ALL, newdata = data.frame(Age=35,G=16,Yds=4442,Y.A=8.1,Y.G=277.6,X4QC=3,GWD=3))
rodgers.all-33500000
(rodgers.all/33500000)-1

keenum.all<- predict(fit.ALL, newdata = data.frame(Age=30,G=16,Yds=3890,Y.A=6.6,Y.G=243.1,X4QC=3,GWD=4))
keenum.all-18000000
(keenum.all/18000000)-1

mahomes.all<- predict(fit.ALL, newdata = data.frame(Age=23,G=16,Yds=5097,Y.A=8.8,Y.G=318.4,X4QC=2,GWD=2))
mahomes.all-4106447
(mahomes.all/4106447)-1

manning.all<- predict(fit.ALL, newdata = data.frame(Age=37,G=16,Yds=4299,Y.A=7.5,Y.G=268.7,X4QC=1,GWD=2))
manning.all-21000000
(manning.all/21000000)-1

brady.all<- predict(fit.ALL, newdata = data.frame(Age=31,G=16,Yds=4355,Y.A=7.6,Y.G=272.2,X4QC=1,GWD=2))
brady.all-15000000
(brady.all/15000000)-1

goff.all<-predict(fit.ALL, newdata = data.frame(Age=24,G=16,Yds=4688,Y.A=8.4,Y.G=293,X4QC=4,GWD=4))
goff.all-6984418
(goff.all/6984419)-1

stafford.all<-predict(fit.ALL, newdata = data.frame(Age=30,G=16,Yds=3777,Y.A=6.8,Y.G=236.1,X4QC=0,GWD=1))
stafford.all-27000000
(stafford.all/27000000)-1

carr.all<-predict(fit.ALL, newdata = data.frame(Age=27,G=16,Yds=4049,Y.A=7.3,Y.G=253.1,X4QC=3,GWD=3))
carr.all-25000000
(carr.all/25000000)-1

prescott.all <-predict(fit.ALL, newdata = data.frame(Age=25,G=16,Yds=3885,Y.A=7.4,Y.G=242.8,X4QC=3,GWD=5))
prescott.all-680848
(prescott.all/680848)-1

rivers.all<-predict(fit.ALL, newdata = data.frame(Age=37,G=16,Yds=4308,Y.A=8.5,Y.G=269.3,X4QC=3,GWD=3))
rivers.all-20812500
(rivers.all/20815500)-1

watson.all<-predict(fit.ALL, newdata = data.frame(Age=23,G=16,Yds=4165,Y.A=8.2,Y.G=260.3,X4QC=5,GWD=5))
watson.all-3463570
(watson.all/3463570)-1

brees.all<-predict(fit.ALL, newdata = data.frame(Age=39,G=15,Yds=3992,Y.A=8.2,Y.G=266.1,X4QC=6,GWD=7))
brees.all-25000000
(brees.all/25000000)-1

mayfield.all<-predict(fit.ALL, newdata = data.frame(Age=23,G=14,Yds=3725,Y.A=7.7,Y.G=266.1,X4QC=3,GWD=4))
mayfield.all-8170745
(mayfield.all/8170745)-1

newton.all<-predict(fit.ALL, newdata = data.frame(Age=29,G=14,Yds=3395,Y.A=7.2,Y.G=242.5,X4QC=2,GWD=2))
newton.all-20760000
(newton.all/20760000)-1

trubisky.all<-predict(fit.ALL, newdata = data.frame(Age=24,G=14,Yds=3223,Y.A=7.4,Y.G=230.2,X4QC=1,GWD=2))
trubisky.all-7258106
(trubisky.all/7258106)-1

wilson.all<-predict(fit.ALL, newdata = data.frame(Age=30,G=16,Yds=3448,Y.A=8.1,Y.G=215.5,X4QC=2,GWD=4))
wilson.all-21900000
(wilson.all/21900000)-1

darnold.all<-predict(fit.ALL, newdata = data.frame(Age=21,G=13,Yds=2865,Y.A=6.9,Y.G=220.4,X4QC=1,GWD=1))
darnold.all-7561929
(darnold.all/7561929)-1

bortles.all<-predict(fit.ALL, newdata = data.frame(Age=26,G=13,Yds=2718,Y.A=6.7,Y.G=209.,X4QC=0,GWD=1))
bortles.all-18000000
(bortles.all/18000000)-1

wentz.all<-predict(fit.ALL, newdata = data.frame(Age=26,G=11,Yds=3074,Y.A=7.7,Y.G=279.5,X4QC=2,GWD=2))
wentz.all-6669085
(wentz.all/6669085)-1

rosen.all<-predict(fit.ALL, newdata = data.frame(Age=21,G=14,Yds=2278,Y.A=5.8,Y.G=162.7,X4QC=1,GWD=2))
rosen.all-4399439
(rosen.all/4399439)-1

flacco.all<-predict(fit.ALL, newdata = data.frame(Age=33,G=9,Yds=2465,Y.A=6.5,Y.G=273.9,X4QC=0,GWD=0))
flacco.all-22133333
(flacco.all/22133333)-1

winston.all<-predict(fit.ALL, newdata = data.frame(Age=24,G=11,Yds=2992,Y.A=7.9,Y.G=272,X4QC=0,GWD=1))
winston.all-6337819
(winston.all/6337819)-1

dalton.all<-predict(fit.ALL, newdata = data.frame(Age=31,G=11,Yds=2566,Y.A=7,Y.G=233.3,X4QC=3,GWD=4))
dalton.all-16000000
(dalton.all/16000000)-1

mariota.all<-predict(fit.ALL, newdata = data.frame(Age=25,G=14,Yds=2528,Y.A=7.6,Y.G=180.6,X4QC=2,GWD=3))
mariota.all-6053494
(mariota.all/6053494)-1

smith.all<-predict(fit.ALL, newdata = data.frame(Age=34,G=10,Yds=2180,Y.A=6.6,Y.G=218,X4QC=0,GWD=0))
smith.all-23500000
(smith.all/23500000)-1

allen.all<-predict(fit.ALL, newdata = data.frame(Age=22,G=12,Yds=2074,Y.A=6.5,Y.G=172.8,X4QC=2,GWD=3))
allen.all-5295760
(allen.all/5295760)-1

mullens.all<-predict(fit.ALL, newdata = data.frame(Age=23,G=8,Yds=2277,Y.A=8.3,Y.G=284.6,X4QC=0,GWD=1))
mullens.all
mullens.all-525000
(mullens.all/525000)-1

tannehill.all<-predict(fit.ALL, newdata = data.frame(Age=30,G=11,Yds=1979,Y.A=7.2,Y.G=179.9,X4QC=3,GWD=3))
tannehill.all-19250000
(tannehill.all/19250000)-1

fitzpatrick.all<-predict(fit.ALL, newdata = data.frame(Age=36,G=8,Yds=2366,Y.A=9.6,Y.G=295.8,X4QC=0,GWD=0))
fitzpatrick.all-3300000
(fitzpatrick.all/3300000)-1

foles.all<-predict(fit.ALL, newdata = data.frame(Age=29,G=5,Yds=1413,Y.A=7.2,Y.G=282.6,X4QC=2,GWD=2))
foles.all-5500000
(foles.all/5500000)-1

osweiler.all<-predict(fit.ALL, newdata = data.frame(Age=28,G=7,Yds=1247,Y.A=7.0,Y.G=178,X4QC=1,GWD=1))
osweiler.all
osweiler.all-880000
(osweiler.all/880000)-1

driskel.all<-predict(fit.ALL, newdata = data.frame(Age=25,G=9,Yds=1003,Y.A=5.7,Y.G=111.4,X4QC=0,GWD=0))
driskel.all-613135
(driskel.all/613135)-1

jackson.all<-predict(fit.ALL, newdata = data.frame(Age=21,G=16,Yds=1201,Y.A=7.1,Y.G=75.1,X4QC=0,GWD=1))
jackson.all-2367912
(jackson.all/2367912)-1

beathard.all<-predict(fit.ALL, newdata = data.frame(Age=25,G=6,Yds=1252,Y.A=7.4,Y.G=208.7,X4QC=0,GWD=0))
beathard.all-882162
(beathard.all/882162)-1

kessler.all<-predict(fit.ALL, newdata = data.frame(Age=25,G=5,Yds=709,Y.A=5.4,Y.G=114.8,X4QC=0,GWD=0))
kessler.all-847271
kessler.all/847271-1

mckown.all <-predict(fit.ALL, newdata = data.frame(Age=39,G=4,Yds=539,Y.A=4.9,Y.G=134.8,X4QC=0,GWD=0))
mckown.all/10000000-1

### Finding the average error in percentage
11.8 +
  -9.7+
  -26.17+
  -2.6+
  -30.25+
  21.34+
  97.26+
  19.1+
  22.15+
  51.3+
  -22.47+
  -24.5+
  1172.3+
  11.1+
  183.2+
  -14.17+
  -19.48+
  -11.47+
  11.16+
  -56.15+
  -150.28+
  2.5+
  -47.11+
  -20.5+
  0+
  -17.8+
  -16.4+
  -586.3+
  -43.3+
  163.8+
  73.6+
  117.4+
  657+
  176+
  -332+
  -664+
  34

730.36/40

########
# AVERAGE ERROR WITH ALL: 18.259%
########

### Average Error of just veterens

11.8 +
  -9.7+
  -26.17+
  -2.6+
  -30.25+
  21.34+
  19.1+
  22.15+
  -22.47+
  -24.5+
  11.1+
  -14.17+
  -11.47+
  -56.15+
  -36.44+
  2.5+
  -20.5+
  -17.8+
  -43.3+
  163.8+
  73.6+
  117.4+
  34
161.27/23  

#########
#Percent error for veterens: 7.01%
########

##### Percent error for rookie contracts

97.26+
  51.3+
  1172.3+
  183.2+
  -19.48+
  11.16+
  22.88+
  56.76+
  -150.28+
  -47.11+
  0+
  -16.4+
  -566.3+
  657+
  176+
  -332+
  -664
632.29/17  

##########
# Percent error with rookies: 37.194
##########


