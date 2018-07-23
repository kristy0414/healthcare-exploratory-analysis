lung<-read.csv(file="DataFile_Sp18.csv",header=TRUE)
attach(lung)
library(nlme)
library(corrplot)
library(car)
install.packages("lmtest")
library(lmtest)
library(MASS)

summary(fit1)
step(fit1,trace=TRUE)
range(medBMI)
range(Age)
range(PulmonaryExacerbations)
fit1<-lm(ppFEV1~.,data=lung)
par(mfrow=c(3,4))
hist(lung$ppFEV1,main="ppFEV1")
plot(as.factor(lung$PulmonaryExacerbations),
     main="times of worsening infections")
hist(lung$medBMI,main="BMI")
hist(lung$Age,main="age")
plot(lung$Gender,main="gender")
plot(lung$PA,main="pseudomonas bacteria")
plot(lung$MRSA,main="MRSA bacteria")
plot(lung$TOBI,main="inhaled tobramycin antibiotic")
plot(lung$DailyAirway,main="daily airway clearance techniques")
plot(lung$RegExercise,main="daily exercise")
plot(lung$Depression,main="depression")
plot(lung$Diabetes,main="diabetes")
par(mfrow=c(2,2))


lp<-log(lung$ppFEV1)
hist(lp,main="log of ppFEV1")
hist(lung$ppFEV1,main="ppFEV1")
fit2<-lm(lp~PulmonaryExacerbations+medBMI+Age+Gender
         +PA+MRSA+TOBI+DailyAirway+RegExercise+Depression+Diabetes,data=lung)
summary(fit2)
length(which(lung$PulmonaryExacerbations==1))
fit3<-lm(ppFEV1~PulmonaryExacerbations+medBMI+Age+Gende
         +PA+MRSA+TOBI+DailyAirway+RegExercise+Depression+Diabetes,data=lung)
summary(fit3)
anova(fit3)
AIC(fit2,fit3)
# no use log transformation, due to violation of normality hypothesis of residuals
# in linear models. So even the AIC of log fit is way more smaller 
# than the original form, I won't choose it. 

par(mfrow=c(2,4))
qqnorm(fit2$residuals,main="log of ppFEV1")
qqline(fit2$residuals,col="red")
qqnorm(fit3$residuals,main="ppFEV1")
qqline(fit3$residuals,col="red")
par(mfrow=c(2,2))
plot(fit2)
plot(fit3)
anova(fit3)

#fit4 drop depression and diabetes.
fit4<-lm(ppFEV1~PulmonaryExacerbations+medBMI+Age+Gender+PA+MRSA+TOBI
         +DailyAirway+RegExercise,data=lung)
anova(fit3,fit4)
AIC(fit3,fit4)

# compare two models, since p value are all greater than 0.05,
# the results show that they are not signigicant different,
# if you want to have a simple model,then use fit4. 
# if you want to follow the rule to choose a smaller AIC, then use fit3.

plot(fit4)
plot(fit3$fitted.values,ppFEV1)
lines(x=c(0,100),y=c(0,100),col="red")


# test any interaction terms exists 
# interactions from gender and any other variables
# put every interactions in the model, then delete the item
# that the associated p value is greater than 0.05.
interaction.plot(lung$medBMI,lung$Gender,lung$ppFEV1)
interaction.plot(lung$Diabetes,lung$Gender,lung$ppFEV1)
interaction.plot(lung$Depression,lung$Gender,lung$ppFEV1)
interaction.plot(lung$PulmonaryExacerbations,lung$Gender,lung$ppFEV1)
interaction.plot(lung$Age,lung$Gender,lung$ppFEV1)
interaction.plot(lung$PA,lung$Gender,lung$ppFEV1)
interaction.plot(lung$TOBI,lung$Gender,lung$ppFEV1)
interaction.plot(lung$MRSA,lung$Gender,lung$ppFEV1)
interaction.plot(lung$DailyAirway,lung$Gender,lung$ppFEV1)
interaction.plot(lung$RegExercise,lung$Gender,lung$ppFEV1)

par(mfrow=c(1,1))

fit5<-lm(ppFEV1~PulmonaryExacerbations+PulmonaryExacerbations*Gender
         +medBMI+medBMI*Gender+Age+Age*Gender+Gender+PA+PA*Gender
         +MRSA+MRSA*Gender+TOBI+TOBI*Gender+DailyAirway+DailyAirway*Gender
         +RegExercise+RegExercise*Gender+Depression+Depression*Gender+Diabetes
         +Gender*Diabetes,data=lung)
anova(fit5)
# only remain:gender*age,gender*diabetes.


#separate data by two parts, as male and female groups
male<- lung[which(lung$Gender=="Male"),] 
female<-lung[which(lung$Gender=="Female"),]
fit6<-lm(ppFEV1~PulmonaryExacerbations+medBMI+Age+
         PA+MRSA+TOBI+DailyAirway+RegExercise+Depression+Diabetes,data=male)
fit7<-lm(ppFEV1~PulmonaryExacerbations+medBMI+Age
   +PA+MRSA+TOBI+DailyAirway+RegExercise+Depression+Diabetes,data=female)
AIC(fit3,fit6,fit7)
summary(fit6)
summary(fit7)
summary(fit3)

# center the age,it just change the interpretation of this variable.
# test any inteaction terms exists 
# interactions from age and any other variables
# put every interactions in the model, then delete the item
# that the associated p value is greater than 0.05.
interaction.plot(lung$Age,lung$medBMI,lung$ppFEV1)
interaction.plot(lung$Age,lung$Diabetes,lung$ppFEV1)
interaction.plot(lung$Age,lung$Depression,lung$ppFEV1)
interaction.plot(lung$Age,lung$PulmonaryExacerbations,lung$ppFEV1)
interaction.plot(lung$Age,lung$Gender,lung$ppFEV1)
interaction.plot(lung$Age,lung$PA,lung$ppFEV1)
interaction.plot(lung$Age,lung$TOBI,lung$ppFEV1)
interaction.plot(lung$Age,lung$MRSA,lung$ppFEV1)
interaction.plot(lung$Age,lung$DailyAirway,lung$ppFEV1)
interaction.plot(lung$Age,lung$RegExercise,lung$ppFEV1)

mean(Age)
fit8<-lm(ppFEV1~PulmonaryExacerbations+PulmonaryExacerbations*I(Age-34)
         +medBMI+medBMI*I(Age-34)+I(Age-34)+Gender+I(Age-34)*Gender
         +PA+PA*I(Age-34)+MRSA+MRSA*I(Age-34)+TOBI+TOBI*I(Age-34)
         +DailyAirway+DailyAirway*I(Age-34)+RegExercise+RegExercise*I(Age-34)
         +Depression+Depression*I(Age-34)+Diabetes+Diabetes*I(Age-34),data=lung)
anova(fit8)
#only remain:age*gender,age*diabetes.


#So fit9 will include age*gender,age*diabetes,gender*diabetes
fit9<-lm(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+Gender
         +PA+MRSA+TOBI+DailyAirway+RegExercise+Depression+Diabetes
         +I(Age-34)*Gender+I(Age-34)*Diabetes+Gender*Diabetes,data=lung)
anova(fit3,fit9)
AIC(fit3,fit9)
# fit9 is better


#fit10 drop depression if you want a simple model
fit10<-lm(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+Gender
         +PA+MRSA+TOBI+DailyAirway+RegExercise+Diabetes
         +I(Age-34)*Gender+I(Age-34)*Diabetes+Gender*Diabetes,data=lung)
anova(fit9,fit10)
AIC(fit9,fit10)
#keep fit10


# fit11 add quadratic form of I(Age-34) into fit10.
fit11<-lm(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender
          +PA+MRSA+TOBI+DailyAirway+RegExercise+Diabetes
          +I(Age-34)*Gender+I(Age-34)*Diabetes+Gender*Diabetes,data=lung)
anova(fit11,fit10)
summary(fit11)
summary(fit12)
anova(fit12)
AIC(fit11,fit10)

# keep fit 11


# try to test any random effect exist
# look at the data, the most possible idea for random effect
# is the 4 types of race.
# So, create a new column, and assigned by 1 to 4.
# that represent 4 kinds of races.
lung$Race[which(lung$RaceWhite=="Yes")]<-1
lung$Race[which(lung$RaceHispanic=="Yes")]<-2
lung$Race[which(lung$RaceAA=="Yes")]<-3
lung$Race[which(lung$RaceAsian=="Yes")]<-4
lung$Race<-as.factor(lung$Race)
stripchart(ppFEV1~Race,data=lung,main="4 types of race")

# compare two models:one has random effect, the other one do not has.
# gls means general least squares that allows unequal variance and not independent errors.
fit12<-gls(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender
            +PA+MRSA+TOBI+DailyAirway+RegExercise+Diabetes
            +I(Age-34)*Gender+I(Age-34)*Diabetes+Gender*Diabetes,data=lung)
fit13<-lme(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender
            +PA+MRSA+TOBI+DailyAirway+RegExercise+Diabetes
            +I(Age-34)*Gender+I(Age-34)*Diabetes+Gender*Diabetes,random=~1|Race,data=lung)
anova(fit12,fit13)
# So keep fit 11 without random effect of race.
# Because the data of race is very skewed. most of the data is categorized as white.
# the data is not balanced from 4 types of race and may not be able 
# to detect the random effect of race.
# but in the future, if the data could collect more observations besides white race
# we could test this again.

#test different vaiance of gender groups.
fit14<-gls(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender
           +PA+MRSA+TOBI+DailyAirway+RegExercise+Diabetes
           +I(Age-34)*Gender+I(Age-34)*Diabetes+Gender*Diabetes,
           weights=varIdent(form=~1|Gender),data=lung)
anova(fit12,fit14)
# keep fit 11 without different variance in gender groups.


# fit15 include different variance of diabetes, since diabetes has significant
# interaction terms with both gender and I(age-34).
fit15<-gls(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender
           +PA+MRSA+TOBI+DailyAirway+RegExercise+Diabetes
           +I(Age-34)*Gender+I(Age-34)*Diabetes+Gender*Diabetes,
           weights=varIdent(form=~1|Diabetes),data=lung)
anova(fit12,fit15)
#keep fit 11 without difference variance in diabetes groups.


# fit16 include dependent error into Age
# varPower means the errors will change due to the levels of that variable.
# representing a power variance function structure.
fit16<-gls(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender
           +PA+MRSA+TOBI+DailyAirway+RegExercise+Diabetes
           +I(Age-34)*Gender+I(Age-34)*Diabetes+Gender*Diabetes,
           weights=varPower(form=~Age),data=lung)
anova(fit12,fit16)
# keep fit11.

anova(fit11)
summary(fit11)

#from avona of fit11
# fit18 drop diabetes from fit11 . if you want to get a simple model.
fit18<-lm(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender+
            I(Age-34)*Gender+PA+MRSA+TOBI+DailyAirway+RegExercise,data=lung)
anova(fit18,fit11)
#keep fit11

#from summary of fit11
#fit19 delete MRSA
fit19<-lm(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender+
            +I(Age-34)*Gender+PA+TOBI+DailyAirway+RegExercise
          +Diabetes+I(Age-34)*Diabetes+Gender*Diabetes,data=lung)
anova(fit19,fit11)
#keep fit19

#fit20 delete MARA and diabetes*gender
fit20<-lm(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender+
            +I(Age-34)*Gender+PA+TOBI+DailyAirway+RegExercise
          +Diabetes+I(Age-34)*Diabetes,data=lung)
anova(fit20,fit11)
summary(fit20)
#keep fit20

#fit21 delete MARA, Age*dtabetes and diabetes*gender
fit21<-lm(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender+
            +I(Age-34)*Gender+PA+TOBI+DailyAirway+RegExercise
          +Diabetes,data=lung)
anova(fit21,fit11)
# keep fit11

# some types of test to test the hypothesis of "ordinary least square" 
# in linear regerssion models
# So base on that, the best linear unbiased estimators will get from 
# ordinary least squares method

# detect multicollinearity.
# if Multicollinearity exist, it will hurt the effect of estimators
# from "Ordinary linear squares" method
# see correlation for gls
par(mfrow=c(2,2))
plot(fit20)
fit22<-gls(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender+
            +I(Age-34)*Gender+PA+TOBI+DailyAirway+RegExercise
          +Diabetes+I(Age-34)*Diabetes,data=lung)
summary(fit22)

#variables need to be numerical values in the correlation test. 
#then test numerical variables' correlation
#results show that they are reasonably independent. 
par(mfrow=c(1,1))
corrplot(cor(data.frame(PulmonaryExacerbations,medBMI,Age)))
cor(data.frame(PulmonaryExacerbations,medBMI,Age))

#Outliers in data can distort predictions and affect the accuracy
outlierTest(fit20)
# Most extreme observation is row 26.

cooksd<-cooks.distance(fit20)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 
# plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red") 
# add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  
# add labels

# you could do imputation with mean / median / mode if you could find which variable
# has extreme value and then imputate with mean or median.
# But for this data I prefer just delete them and treat them as missing values.
# because for the extreme case you can not find 
# the extreme variable affected model appropriately.
# here, from the outlier plot,
# I will choose row:385,330,408,445. delete them and fit a new model.

newlung<-lung[-c(385,330,408,445),]
fit17<-lm(ppFEV1~PulmonaryExacerbations+medBMI+I(Age-34)+I((Age-34)^2)+Gender
          +PA+TOBI+DailyAirway+RegExercise+Diabetes
          +I(Age-34)*Gender+I(Age-34)*Diabetes,data=newlung)
par(mfrow=c(2,4))
plot(fit17)
plot(fit20)
summary(fit17)
summary(fit20)

# diagnostic results do not show big difference.
# keep fit20 that inculde all observations.


# first hypothesis: errors are normal distribution.  
# external means remove oberservation i item then calculate the residuals
# plot shows it basiclly meet the requirement of normality.
hist(studres(fit20), main="frequency of external Studentized Residuals",
     xlab="Studentized Residuals")
qqnorm(fit20$residuals)
qqline(fit20$residuals,col="red")

# second hypothesis: errors are independent.
dwtest(fit20)

#because p value is 0.4155, 
#we accept null hypothesis, we conclude that the errors are independent.

# third hypothesis: errors have equal variance.
ncvTest(fit20)
#because p value is 0.7527874, 
#we accept null hypothesis, we conclude that the errors' variance is equal.

summary(fit20)
##### So, final model is fit11.#####
