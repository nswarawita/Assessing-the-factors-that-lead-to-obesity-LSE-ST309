library(data.table); library(tidyverse); library(randomForest); library(sjPlot); library(MASS); library(tree); library(ROCR); library(car)

# Loading in the data
dat <- read.csv("309 Data.csv", header = T)

# ------------------------- 2.2 Data Preparation	 --------------------------------------

# Renaming the predictors
setnames(dat, old = c('FAVC', 'FCVC','NCP','CAEC', 'CH2O', 'SCC', 'FAF', 'TUE', 'CALC', 'MTRANS', 'NObeyesdad'), 
         new = c('Fast food intake', 'Vegetable consumption freq', 'No main meals', 'Food between meals','Daily liquid intake','Calc daily calories', 'Physical activity freq', 'Time on tech', 'Alc consumption', 'Transportation used', 'Weight class'))
setnames(dat, old = c('family_history_with_overweight','SMOKE'), 
         new = c('Family history with overweight','Smoke'))

summary(dat) # No missing values in the data

# Add column for BMI
dat$BMI <- dat$Weight / (dat$Height)^2
summary(dat)

# Make a new data set with changes
write.csv(dat, 'dat_bmi.csv')
dat_bmi <- read.csv("dat_bmi.csv", header = T)
head(dat_bmi)
summary(dat_bmi)

# Remove weight class as is based off bmi, remove weight and height as bmi is based off them 
dat_bmi <- dat_bmi[,-c(18,1,4,5)]
head(dat_bmi)

# Pairs not very informative due to amount of catagorical variables.
# Using a subset of 100 datapoints was still not very informative as lots of the predictors are factors.


# 2.2.1 Converting variables coded as continuous variables to categorical variables
# Most of the data data takes integer values, then a large set takes decimal values between integers. 
# This is due to how the data was collected.
attach(dat_bmi)
summary(No.main.meals)
No.main.meals[which(No.main.meals < 1.4999999)]<-"One"
No.main.meals[which(No.main.meals > 1.5 & No.main.meals < 2.4999999)]<-"Two"
No.main.meals[which(No.main.meals > 2.5 & No.main.meals < 3.4999999)]<-"Three"
No.main.meals[which(No.main.meals > 3.5 & No.main.meals < 4.4999999)]<-"Four"
dat_bmi$No.main.meals <- No.main.meals
dat_bmi$No.main.meals <- as.factor(No.main.meals)
summary(dat_bmi$No.main.meals)

summary(Vegetable.consumption.freq)
Vegetable.consumption.freq[which(Vegetable.consumption.freq > 0.5 & Vegetable.consumption.freq < 1.4999999)]<-"Never"
Vegetable.consumption.freq[which(Vegetable.consumption.freq > 1.5 & Vegetable.consumption.freq < 2.4999999)]<-"Sometimes"
Vegetable.consumption.freq[which(Vegetable.consumption.freq > 2.5 & Vegetable.consumption.freq < 3.4999999)]<-"Always"
dat_bmi$Vegetable.consumption.freq <- Vegetable.consumption.freq
dat_bmi$Vegetable.consumption.freq <- as.factor(Vegetable.consumption.freq)
summary(dat_bmi$Vegetable.consumption.freq)

summary(Physical.activity.freq)
Physical.activity.freq[which(Physical.activity.freq < 0.4999999)]<-"Never"
Physical.activity.freq[which(Physical.activity.freq > 0.5 & Physical.activity.freq < 1.4999999)]<-"1-2 days"
Physical.activity.freq[which(Physical.activity.freq > 1.5 & Physical.activity.freq < 2.4999999)]<-"2-4 days"
Physical.activity.freq[which(Physical.activity.freq > 2.5 & Physical.activity.freq < 3.4999999)]<-"4-5 days"
dat_bmi$Physical.activity.freq <- Physical.activity.freq
dat_bmi$Physical.activity.freq <- as.factor(Physical.activity.freq)
summary(dat_bmi$Physical.activity.freq)

summary(Time.on.tech)
Time.on.tech[which(Time.on.tech < 0.4999999)]<-"0-2 hours"
Time.on.tech[which(Time.on.tech > 0.5 & Time.on.tech < 1.4999999)]<-"3-5 hours"
Time.on.tech[which(Time.on.tech > 1.5 & Time.on.tech < 2.4999999)]<-"More than 5 hours"
dat_bmi$Time.on.tech <- Time.on.tech
Time.on.tech <- factor(Time.on.tech)
dat_bmi$Time.on.tech <- as.factor(dat_bmi$Time.on.tech)
summary(dat_bmi$Time.on.tech)

summary(Daily.liquid.intake)
Daily.liquid.intake[which(Daily.liquid.intake > 0.5 & Daily.liquid.intake < 1.4999999)]<-"Less than a litre"
Daily.liquid.intake[which(Daily.liquid.intake > 1.5 & Daily.liquid.intake < 2.4999999)]<-"1-2 litres"
Daily.liquid.intake[which(Daily.liquid.intake > 2.5 & Daily.liquid.intake < 3.4999999)]<-"More than 2 litres"
dat_bmi$Daily.liquid.intake <- Daily.liquid.intake
Daily.liquid.intake <- as.factor(Daily.liquid.intake)
dat_bmi$Daily.liquid.intake <- as.factor(dat_bmi$Daily.liquid.intake)
summary(dat_bmi$Daily.liquid.intake)

summary(dat_bmi)


#correcting variable classes
dat_bmi$Gender <- as.factor(dat_bmi$Gender)
dat_bmi$Family.history.with.overweight <- as.factor(dat_bmi$Family.history.with.overweight)
dat_bmi$Fast.food.intake <- as.factor(dat_bmi$Fast.food.intake)
dat_bmi$Vegetable.consumption.freq <- as.factor(dat_bmi$Vegetable.consumption.freq)
dat_bmi$No.main.meals<- as.factor(dat_bmi$No.main.meals)
dat_bmi$Food.between.meals <- as.factor(dat_bmi$Food.between.meals)
dat_bmi$Smoke <- as.factor(dat_bmi$Smoke)
dat_bmi$Daily.liquid.intake<- as.factor(dat_bmi$Daily.liquid.intake)
dat_bmi$Calc.daily.calories <- as.factor(dat_bmi$Calc.daily.calories)
dat_bmi$Physical.activity.freq <- as.factor(dat_bmi$Physical.activity.freq)
dat_bmi$Time.on.tech <- as.factor(dat_bmi$Time.on.tech)
dat_bmi$Alc.consumption <- as.factor(dat_bmi$Alc.consumption)
dat_bmi$Transportation.used<- as.factor(dat_bmi$Transportation.used)


summary(dat_bmi)

# ------------------------- 2.3 Variables and relationships among variables	 --------------------------------------
# Plots for individual variables

Histogram_BMI <- ggplot(data=dat_bmi)+
  geom_histogram(aes(x=BMI,y=..density..), binwidth = 2.5, colour = 'black', fill = 'lightpink')
Histogram_BMI #Approximately normally distributed
dat_bmi[order(dat_bmi$BMI),]
Histogram_BMI_append <- ggplot(data=dat_bmi)+
  geom_histogram(aes(x=BMI,y=..density..), binwidth = 1, colour = 'black', fill = 'lightpink')

Histogram_1 <- ggplot(data=dat_bmi)+
  geom_histogram(aes(x=Age,y=..density..))
Histogram_1 #Somewhat [positively skewed]
options(max.print=1000000)
dat_bmi[order(dat_bmi$Age),]
dat_bmi[134,] # Oldest was 61
dat_bmi[253,] # 2nd oldest was 56

Bar_chart_1 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Gender))+
  labs(title = "Barchart of Gender", x = "Gender", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_1

Bar_chart_2 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Family.history.with.overweight))+
  labs(title = "Barchart of Family.history.with.overweight", x = "Family.history.with.overweight", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_2

Bar_chart_3 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Fast.food.intake))+
  labs(title = "Barchart of Fast.food.intake", x = "Fast.food.intake", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_3

Bar_chart_4 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Vegetable.consumption.freq))+
  labs(title = "Barchart of Vegetable.consumption.freq", x = "Vegetable.consumption.freq", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_4

Bar_chart_5 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=No.main.meals))+
  labs(title = "No.main.meals", x = "No.main.meals", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_5

Bar_chart_6 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Food.between.meals))+
  labs(title = "Barchart of Food.between.meals", x = "Food.between.meals", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_6

Bar_chart_7 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Smoke))+
  labs(title = "Barchart of Smoke", x = "Smoke", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_7

Bar_chart_8 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Daily.liquid.intake))+
  labs(title = "Barchart of Daily.liquid.intake", x = "Daily.liquid.intake", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_8

Bar_chart_9 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Calc.daily.calories))+
  labs(title = "Barchart of Calc.daily.calories", x = "Calc.daily.caloriesr", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_9

Bar_chart_10 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Physical.activity.freq))+
  labs(title = "Barchart of Physical.activity.freq", x = "Physical.activity.freq", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_10

Bar_chart_11 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Time.on.tech))+
  labs(title = "Barchart of Time.on.tech", x = "Time.on.tech", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_11

Bar_chart_12 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Alc.consumption))+
  labs(title = "Barchart of Alc.consumption", x = "Alc.consumption", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_12

Bar_chart_13 <- ggplot(data=dat_bmi)+
  geom_bar(aes(x=Transportation.used))+
  labs(title = "Barchart of Transportation.used", x = "Transportation.used", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
Bar_chart_13

str(dat_bmi)
# Plots for individuals predictors vs BMI

Boxplot_Gender <- ggplot(data=dat_bmi,aes(x=Gender, y=BMI)) +
  geom_boxplot()
Boxplot_Gender # Both male and female
tapply(dat_bmi$BMI,dat_bmi$Gender,median, na.rm=T) # Approximately the same

Scatterplot_Age <- ggplot(dat_bmi,aes(x=Age, y=BMI))+
  geom_point()+
  theme_bw()+
  geom_smooth(method=lm,se=F) 
Scatterplot_Age

Boxplot_Family.history.with.overweight <- ggplot(data=dat_bmi,aes(x=Family.history.with.overweight, y=BMI)) +
  geom_boxplot()
Boxplot_Family.history.with.overweight # Yes has higher quartiles

Boxplot_Fast.food.intake <- ggplot(data=dat_bmi,aes(x=Fast.food.intake, y=BMI)) +
  geom_boxplot()
Boxplot_Fast.food.intake # Yes has higher quartiles

Boxplot_Vegetable.consumption.freq <- ggplot(data=dat_bmi,aes(x=Vegetable.consumption.freq, y=BMI)) +
  geom_boxplot()
Boxplot_Vegetable.consumption.freq

Boxplot_No.main.meals <- ggplot(data=dat_bmi,aes(x=No.main.meals, y=BMI)) +
  geom_boxplot()
Boxplot_No.main.meals # Relevel to 3

Boxplot_Food.between.meals <- ggplot(data=dat_bmi,aes(x=Food.between.meals, y=BMI)) +
  geom_boxplot()
Boxplot_Food.between.meals # No had possible outliers

Boxplot_Smoke <- ggplot(data=dat_bmi,aes(x=Smoke, y=BMI)) +
  geom_boxplot()
Boxplot_Smoke # Yes and no had similar quartiles

Boxplot_Daily.liquid.intake <- ggplot(dat_bmi,aes(x=Daily.liquid.intake, y=BMI))+
  geom_boxplot()
Boxplot_Daily.liquid.intake

Boxplot_Calc.daily.calories <- ggplot(data=dat_bmi,aes(x=Calc.daily.calories, y=BMI)) +
  geom_boxplot()
Boxplot_Calc.daily.calories #No had higher quartiles

Boxplot_Physical.activity.freq <- ggplot(data=dat_bmi,aes(x=Physical.activity.freq, y=BMI)) +
  geom_boxplot()
Boxplot_Physical.activity.freq # Often and sometimes had a similar median, relevel

Boxplot_Time.on.tech <- ggplot(data=dat_bmi,aes(x=Time.on.tech, y=BMI)) +
  geom_boxplot()
Boxplot_Time.on.tech # Never and some have a similar median

Boxplot_Alc.consumption <- ggplot(data=dat_bmi,aes(x=Alc.consumption, y=BMI)) +
  geom_boxplot()
Boxplot_Alc.consumption # Sometimes had the highest BMI

Boxplot_Transportation.used <- ggplot(data=dat_bmi,aes(x=Transportation.used, y=BMI)) +
  geom_boxplot()
Boxplot_Transportation.used # Bike and motorbike



# 2.2.2 Merging levels of Alc.consumption and Transportation.used and relevelling them 
levels(dat_bmi$Alc.consumption) <- c("Frequently", "Frequently", "Never", "Sometimes")
dat_bmi$Alc.consumption <- factor(dat_bmi$Alc.consumption, ordered = FALSE )
levels(dat_bmi$Alc.consumption) <- unique(c("Frequently", "Frequently", "Never", "Sometimes"))

levels(dat_bmi$Transportation.used) <- c("Personal Vehicle", "Walk/Bike", "Personal Vehicle", "Public Transport", "Walk/Bike")
dat_bmi$Transportation.used <- factor(dat_bmi$Transportation.used, ordered = FALSE )
levels(dat_bmi$Transportation.used) <- unique(c("Personal Vehicle", "Walk/Bike", "Personal Vehicle", "Public Transport", "Walk/Bike"))

summary(dat_bmi)

# 2.2.3 Changing the base level of predictors

# Renaming and relevelling No main meeals
plot(BMI~No.main.meals, dat=dat_bmi)
No.main.meals <- factor(dat_bmi$No.main.meals, levels = c("One", "Two", "Three", "Four"))
dat_bmi$No.main.meals <- relevel(dat_bmi$No.main.meals, ref="Three", dat=dat_bmi)

summary(dat_bmi)
# Relevel Vegetable.consumption.freq to Sometimes
Vegetable.consumption.freq <- factor(Vegetable.consumption.freq, levels = c("Never", "Sometimes", "Always"))
dat_bmi$Vegetable.consumption.freq <- relevel(Vegetable.consumption.freq, ref="Sometimes", dat=dat_bmi)

dat_bmi$Gender <- relevel(dat_bmi$Gender, ref="Male", dat=dat_bmi)
dat_bmi$Family.history.with.overweight <- relevel(dat_bmi$Family.history.with.overweight, ref="yes", dat=dat_bmi)
dat_bmi$Fast.food.intake <- relevel(dat_bmi$Fast.food.intake, ref="yes", dat=dat_bmi)
dat_bmi$Food.between.meals <- relevel(dat_bmi$Food.between.meals, ref="Sometimes", dat=dat_bmi)
dat_bmi$Alc.consumption <- relevel(dat_bmi$Alc.consumption, ref="Sometimes", dat=dat_bmi) #
dat_bmi$Transportation.used <- relevel(dat_bmi$Transportation.used, ref="Public Transport", dat=dat_bmi)

summary(dat_bmi)


# ------------------------- 3 Data analysis using statistical learning procedures	 --------------------------------------

#creating a new variable which divides the bmi variable as high or not coded as yes or no using a threshold of 25
High=ifelse(BMI<=25, "No", "Yes") # Define the label High for BMI greater than the standard of 25
High <- as.factor(High)
dat_bmi2=data.frame(dat_bmi, High) # combine the label into the data set 
str(dat_bmi2)

set.seed(1000)
training = sample(1:nrow(dat_bmi2), 1056) # randomly select 1056 numbers between 1 and 2111
testing = dat_bmi[-training]

# ------------------------- 3.1 Multiple Linear Regression (MLR) to predict BMI --------------------------------------

# Make data set without underweight people
summary(dat_bmi)
bmi.norm.ov <- dat_bmi[which(dat_bmi$BMI>18.4999999999),]
dat_mlr.0 <- bmi.norm.ov
dat_mlr.0$log_Age <- (log(dat_mlr.0$Age))
dat_mlr <- na.omit(dat_mlr.0[training,-2])
summary(dat_mlr)
head(dat_mlr)
# Build the mlr

bestsub.1<-leaps::regsubsets(BMI~.,nvmax=16,data=dat_mlr)
plot(bestsub.1, scale="bic")
bmi.lm1 <- lm(BMI ~ .-Smoke -Gender - Physical.activity.freq, data = dat_mlr)
summary(bmi.lm1)
par(mfrow = c(2,2))
plot(bmi.lm1, which = c(1:4))

which(cooks.distance(bmi.lm1) > 0.035)
# Performing outlier analysis 
# point 218 has a cooks distance much higher than the rest
par(mfrow = c(1,1))
dat_mlr_1 <- na.omit(dat_mlr[-324,])
bestsub.2<-leaps::regsubsets(BMI~.,nvmax=16,data=dat_mlr_1)
plot(bestsub.2, scale="bic")
bmi.lm2 <- lm(BMI ~ .-Smoke -Gender - Physical.activity.freq, data = dat_mlr_1)
summary(bmi.lm2)
par(mfrow = c(2,2))
plot(bmi.lm2)
plot(bmi.lm2, which = 4)  # These are plotted separately so one can go in the report and the cooks distance is for our knowledge.

# Dosen't Changes the model - the point was not too influential.
tab_model(bmi.lm1)
vif(bmi.lm1)


# The remainder of the points seem fine 

fit.data <- na.omit(dat_mlr.0[-training,-2])
test.fitt <- predict.lm(bmi.lm1, fit.data)
gg.mlr <- ggplot(fit.data, aes(x=(BMI), y = test.fitt)) +
  geom_point(colour = 'Red1')+
  geom_abline(slope =1, intercept =0, colour = 'Black') +
  labs(x = 'Actual BMI', y = 'Predicted BMI')
gg.mlr


# Making subsets 
dat_o40 <- dat_mlr[which(dat_mlr$BMI >40),]
# No BMI equalled 40
dat_u40 <- dat_mlr[which(dat_mlr$BMI <40),]
dim(dat_o40) # Only 132 data points 
dim(dat_u40)

# 3.1.1 MLR to predict BMI less than 40

bestsub.3<-leaps::regsubsets(BMI~.,nvmax=16,data=dat_u40)
par(mfrow = c(2,1))
plot(bestsub.3, scale="bic")
bmi.lm3 <- lm(BMI ~ .-No.main.meals- Smoke - Daily.liquid.intake - Calc.daily.calories - Time.on.tech - Alc.consumption , data = dat_u40)
summary(bmi.lm3)
par(mfrow = c(2,2))
plot(bmi.lm3)
plot(bmi.lm3, which = 4)

vif(bmi.lm3)
tab_model(bmi.lm3)


# 3.1.2 MLR to predict BMI over 40 

names(dat_o40) # liquid, physical, time tech, age, bmi
summary(dat_o40)
dat_o40.1 <- dat_o40[,c(8,10,11,14,15)] # Lack of variation / observations
names(dat_o40.1)
summary(dat_o40.1)

# Merge veg: never sometime, time on tech , 3-5 and 5, alc cons freq and sometimes, remove rown


levels(dat_o40.1$Physical.activity.freq) <- c('1-2 days', '2-4 days',NA,'Never') # 0 people responded 4-5 days
levels(dat_o40.1$Time.on.tech) <- c('0-2 hours', '3-5 hours',NA)
dat_o40.2 <- na.omit(dat_o40.1)

summary(dat_o40.2)
# Change base levels
dat_o40.2$Daily.liquid.intake <- relevel(dat_o40.2$Daily.liquid.intake, ref="More than 2 litres", dat=dat_o40.2)
dat_o40.2$Physical.activity.freq <- relevel(dat_o40.2$Physical.activity.freq, ref="Never", dat=dat_o40.2)
dat_o40.2$Time.on.tech <- relevel(dat_o40.2$Time.on.tech, ref="3-5 hours", dat=dat_o40.2)
summary(dat_o40.2)

bestsub.4<-leaps::regsubsets(BMI~.,nvmax=5,data=dat_o40.2)
plot(bestsub.4, scale="bic")
bmi.lm4 <- lm(BMI ~ Daily.liquid.intake + log_Age, data = dat_o40.2)
summary(bmi.lm4)
par(mfrow = c(2,2))
plot(bmi.lm4)
plot(bmi.lm4, which = 4)
which(cooks.distance(bmi.lm4)>0.08)
summary(bmi.lm4)
# 1815 has a large
summary(dat_o40.2)

dat_o40.3 <- na.omit(dat_o40.2[-55,])
# From here on, a different method is used to remove outliers, this makes it far easier, using subsets you have to guess random numbers

bestsub.5<-leaps::regsubsets(BMI~.,nvmax=5,data=dat_o40.3)
plot(bestsub.5, scale="bic")
bmi.lm5 <- lm(BMI ~ Daily.liquid.intake  + log_Age, data = dat_o40.3)
summary(bmi.lm5)
par(mfrow = c(2,2))
plot(bmi.lm5)
plot(bmi.lm5, which = 4)
which(cooks.distance(bmi.lm5) > 0.2)

# Virtually the same as before, keep model bmi.lm4

tab_model(bmi.lm4)

vif(bmi.lm4)

fit.dat.2 <- (dat_mlr.0[which(dat_mlr.0$BMI < 40),])
fit.dat.2 <- na.omit(fit.dat.2[-training,])
test.fitt2 <- (predict.lm(bmi.lm3, fit.dat.2))
gg.mlr2 <- ggplot(fit.dat.2, aes(x=(BMI), y = test.fitt2)) +
  geom_point(colour = 'darkgreen') +
  geom_abline(slope =1, intercept =0, colour = 'black') +
  labs( x = 'Actual BMI', y = 'Predicted BMI')
gg.mlr2

fit.dat.3 <- (dat_mlr.0[which(dat_mlr.0$BMI > 40),])
fit.dat.3 <- na.omit(fit.dat.3[-training,])
test.fitt3 <- (predict.lm(bmi.lm4, fit.dat.3))
gg.mlr3 <- ggplot(fit.dat.3, aes(x=(BMI), y = test.fitt3)) +
  geom_point(colour = 'limegreen') +
  geom_abline(slope =1, intercept =0, colour = 'black')+
  labs( x = 'Actual BMI', y = 'Predicted BMI')
gg.mlr3


gg.mlr4 <- ggplot(fit.dat.2) +
  geom_point(mapping = aes(x=BMI, y = test.fitt2), colour = 'darkgreen') +
  geom_point(fit.dat.3, mapping = aes(x=BMI, y = test.fitt3), colour = 'limegreen') +
  geom_abline(slope =1, intercept =0, colour = 'black')+
  labs(x = 'Actual BMI', y = 'Predicted BMI')
gg.mlr4


# ----------------------- 3.2 Trees ---------------------------------

# 3.2.1 Decision trees

#create tree with all the data - high variable as outcome
tree.high=tree(High~.-BMI, dat_bmi2) # . indicates using all the predictors, # - BMI: exclude BMI as high is now a variable with same info
summary(tree.high)
par(mfrow = c(1,1))
plot(tree.high) 
text(tree.high, pretty=0, cex=0.6)

# Pruning the tree
cv.bmi = cv.tree(tree.high,FUN=prune.misclass)
cv.bmi
cv.tree(tree.high, FUN=prune.misclass, K=10)

#test tree performance on training data made previously in mil
# set.seed(1000)
#training = sample(1:nrow(dat_bmi2), 1056) # randomly select 1056 numbers between 1 and 2111
testing = dat_bmi2[-training,]

#testData=dat_bmi2[-train,] # test data for checking performance
High.testing = High[-training]
tree2=tree(High~.-BMI, dat_bmi2, subset=training)
plot(tree2)
text(tree2, pretty=0, cex=0.6)
tree.pred=predict(tree2, testing, type="class") # type="vector" returns
# predictive probabilities, check ?predict.tree
table(tree.pred, High.testing)

#calculate misclassification for testing data
(55+60)/(222+55+60+718)

# 3.2.2 Regression trees
#create tree with all the data - bmi variable as outcome
tree.BMI=tree(BMI~.-BMI, dat_bmi) # . indicates using all the predictors, # - BMI: exclude BMI as high is now a variable with same info
plot(tree.BMI) 
text(tree.BMI, pretty=0, cex=0.6)

# Fitting a tree with the training data
set.seed(309)
train = sample(1:nrow(dat_bmi), nrow(dat_bmi)/2) #create training set

tree.reg=tree(BMI~., data=dat_bmi, subset=train)
plot(tree.reg)
text(tree.reg, pretty=0, cex=0.6)

#test data to see power of tree 
bmi.test=dat_bmi[-train, "BMI"]
bmi.predict=predict(tree.reg, newdata=dat_bmi[-train,])
mean((bmi.predict-bmi.test)^2) # 20.8732


# 3.2.3 bagging 
bag.bmi=randomForest(BMI~., data=dat_bmi, subset=train,
                     mtry=14, importance=T) 
bag.bmi
#The option mtry=13 demands to search over all p = 14 variables in each split, and therefore, it is a Bagging fitting. The mean RSS is 10.69873, and % of variance explained is 83.35.

#now try the bagging output with the test sample 
bmi.predict=predict(bag.bmi, newdata=dat_bmi[-train,])
bmi.predict
mean((bmi.predict-bmi.test)^2)
#RSS lower

#we can measure the importance of each variable based on 2 things 
importance(bag.bmi)
#we can see which variable are most important for each measure by a plot
varImpPlot(bag.bmi, col=c("limegreen","red")) # Importance measure plot


# 3.2.4 random forest 
rf.bmi=randomForest(BMI~., data=dat_bmi, subset=train,
                    mtry=7, importance=T) # ‘mtry=7’ sets m=6<p from 14 we used before in bagging 
rf.bmi

# --------------- 3.3 Making a Glm, testing for High BMI -------------------
summary(dat_bmi)
dat_bmi3 <- dat_bmi2[,-15]
dim(dat_bmi3)
summary(dat_bmi3)
testing <- dat_bmi3[-training]
names(dat_bmi3)
bmi.glm <- glm(High ~ ., data = dat_bmi3, subset = training, family = binomial)
summary(bmi.glm)
bmi.glm2 <- glm(High ~ .-Calc.daily.calories, data = dat_bmi3, subset = training, family = binomial)
summary(bmi.glm2)
bmi.glm3 <- glm(High ~ .-Vegetable.consumption.freq-Calc.daily.calories, data = dat_bmi3, subset = training, family = binomial)
summary(bmi.glm3)
bmi.glm4 <- glm(High ~ .-Vegetable.consumption.freq-Calc.daily.calories-Time.on.tech, data = dat_bmi3, subset = training, family = binomial)
summary(bmi.glm4)
bmi.glm5 <- glm(High ~ .-Vegetable.consumption.freq-Calc.daily.calories-Time.on.tech - Fast.food.intake, data = dat_bmi3, subset = training, family = binomial)
summary(bmi.glm5)
bmi.glm6 <- glm(High ~ .-Vegetable.consumption.freq-Calc.daily.calories-Time.on.tech - Fast.food.intake - Gender, data = dat_bmi3, subset = training, family = binomial)
summary(bmi.glm6)
par(mfrow = c(2,2))
plot(bmi.glm6, which = c(1:5))

# Confusion matrix
pred.glm=predict(bmi.glm6, dat_bmi3[-training,], type="response")
pred=ifelse((pred.glm>=0.5), "Yes", "No")
conf_mtx=table(pred, dat_bmi3[-training,]$High, deparse.level = 2)
conf_mtx
set.seed(100)
disp <- sample(1:nrow(dat_bmi3), 50)

predicted.data <- data.frame(
  probability.of.High=predict.glm(bmi.glm6, newdata = dat_bmi3[disp,], type = 'response'),
  High=dat_bmi3[disp,]$High)

ggplot(data=predicted.data, aes(x=High, y=probability.of.High)) +
  geom_point(aes(color=High)) +
  xlab("High") +
  ylab("Predicted probability of High") 

tab_model(bmi.glm6)


















