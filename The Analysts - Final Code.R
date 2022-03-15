############### LIBRARIES #########
library(tidyverse)
library(MASS)
library(VIM)
library(gmodels)
library(caret)
library(lubridate)
library(ROSE)
library(dplyr)
library(ggplot2)
library(eeptools) # for age calc from DOB
library(ModelMetrics)
library(rpart) # for decision trees
library(rpart.plot)
library(randomForest)
library(rattle)
library(e1071) # for svm
library(MASS)
library(broom)
library(caret)
library(fastDummies)
library(funModeling) 
library(Hmisc)
library(e1071)
library(pls)
library(glmnet)
library(devtools)
library(ggbiplot)
library(mlbench)
library(stats)
library(factoextra)
library(Rtsne)
library(umap)
library(FactoMineR)
library(Amelia)
library(mice)
library(plyr)
library(magrittr)
library(naniar)
library(ggcorrplot)
library(tidyr)
library(formattable)
library(hrbrthemes)
library(viridis)
library(reshape2)
library(plotrix)


#__________________________________________________________________________________________________________
#EXPLORATORY DATA ANALYSIS
#reading the train data in R
fraud_data = read.csv("fraudTrain.csv")
#View(fraud_data)
#check for missingness in the data
missmap(fraud_data)
#joining both the first and last name
f_train = unite(fraud_data,"name", first:last, sep = " ", remove = FALSE, na.rm = FALSE)
# removing the columns first and last

#f = f_train%>%group_by(name)%>%tally

#f_train%>View(f1)%filter(name) %>% select(~.)
#grouping the data by name
f1 = f_train %>% arrange(name)
#separate the date of birth into month, day and year
f1 = f1 %>% separate(dob, sep="/", into = c("month", "day", "year"))

f1 = as.integer(f1$year)
f1$age = 2020 - f1["year"]

##checking the data based on categories
fcat = f_train %>% arrange(category)
categories = unique(f1$category)# check the number of the unique categories in the dataset 
#View(categories)
#filter by fraud = 1
f1_fraud = f1%>% filter(is_fraud ==1)
f1_fraud$category %>% summarise(frequency())


fraud_cat = table(f1_fraud$category)
fraud_cat = data.frame(fraud_cat)
colnames(Var1, category)
ggplot(data=f1_fraud) + geom_bar(mapping = aes(x = category))

#filter by non-fraud

f1_nfraud = f1%>% filter(is_fraud ==0)
f1_nfraud$category %>% summarise(frequency())


nfraud_cat = table(f1_nfraud$category)
nfraud_cat = data.frame(nfraud_cat)
colnames(Var1, category)
# plot to show categories that are non-fraudulent
ggplot(data=f1_nfraud) + geom_bar(mapping = aes(x = category))

##FROM THE ANALYSIS, WE SEE THAT FRAUDULENT  TRANSACTIONS ARE MORE PROMINENT IN
##GROCERY_POS AND SHOPPING_NET TRANSACTIONS

#merchant 
f1_merchant = f1_fraud %>% filter(category =='grocery_pos')
merchant = unique(f1_merchant$merchant)
View(merchant)
f1_merchant_aggr = aggregate(f1_merchant[,6], list(f1_merchant$merchant), FUN= "mean")
?aggregate
f1_merchant_aggr$f1 ='1'
View(f1_merchant_aggr)

# non fraudulent transactions
f0_merchant = f1_nfraud %>% filter(category =='grocery_pos')
nmerchant = unique(f0_merchant$merchant)
#N.B. There are 50 merchants associated with grocery pos purchases irregardless of 
#whether or not they are fraudulent cases
f0_merchant_aggr = aggregate(f0_merchant[,6], list(f0_merchant$merchant), FUN= "mean")
f0_merchant_aggr$f1 ='0'

# Doing row bind of fraudulent and non-fraudulent transactions
total_join = rbind(f1_merchant_aggr,f0_merchant_aggr)

# renaming variables
total_join =total_join %>% rename(Merchant = Group.1, meanAmt = x, status = f1)

# grouped bar chart
ggplot(total_join, aes(fill=status, y=meanAmt, x = Merchant)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

#---------------------------------------------------------------------------

f0_merchant_join = f0_merch_aggr$x
f0_merchant_join = data.frame(f0_merch_join)

merchant_totaljoin = cbind(f1_merch_aggr,f0_merch_join)
merchant_totaljoin =merch_totaljoin %>% rename(Merchant = Group.1, f1 = x, f0 = f0_merch_join)
#separate(merch_totaljoin, c("Merchant"), sep = "_")

#---------------------------------------------------------------------------

# Analysis of shopping_net transactions that are fraudulent
f1_merch = f1_fraud %>% filter(category =='shopping_net')
merch = unique(f1_merch$merchant)
merch = data.frame(merch)
View(merch)
merch %>% arrange()

# Analysis of shopping_net transactions that are non-fraudulent
f0_merch = f1_nfraud %>% filter(category =='shopping_net')
nmerch = unique(f0_merch$merchant)

#there are also 50 merchants associated with shopping_net category
shoppingf1_merch_aggr = aggregate(f1_merch[,6], list(f1_merch$merchant), FUN= "mean")
shoppingf1_merch_aggr$f1 = '1'
shoppingf0_merch_aggr = aggregate(f0_merch[,6], list(f0_merch$merchant), FUN= "mean")
shoppingf0_merch_aggr$f1 = '0'

#shopp_merch_join = shoppingf0_merch_aggr
#shopp_merch_join$f1 = '0'
#shopp_merch_join = data.frame(shopp_merch_join)
#shopp_merch_totaljoin = rbind(shoppingf1_merch_aggr,shopp_merch_join)
shopp_merch_totaljoin = rbind(shoppingf1_merch_aggr,shoppingf0_merch_aggr)
shopp_merch_totaljoin =shopp_merch_totaljoin %>% rename(Merchant = Group.1, meanAmt = x, status = f1)

#plot comparing the mean amount of fraud and non-fraud transactions
#based on merchants associated with shopping_net category
ggplot(shopp_merch_totaljoin, aes(fill=status, y=meanAmt, x = Merchant)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()


# Here, it is so evident that the fraudsters spending pattern on internet shopping
# is higher compared to the mean amounts spend by an average individual.
# We tried to check if this spending is applicable to certain merchants, but this isn't the case, 
# as it shows the fraudsters are good at covering their tracks

#checking to see the unique entries in city.
city = unique(f1_merchant$city)
# there are 505 unique cities. that's a lot to work on.

#analysis by state
unique(f1_merchant$state)
state1_aggr = aggregate(f1_merch[,6], list(f1_merch$state), FUN= "mean")
state1_aggr$f1 = '1'
state0_aggr = aggregate(f0_merch[,6], list(f0_merch$state), FUN= "mean")
state0_aggr$f1 = '0'

state_mean_join = rbind(state1_aggr,state0_aggr)
state_mean_join =state_mean_join %>% rename(State = Group.1, meanAmt = x, status = f1)

ggplot(state_mean_join, aes(fill=status, y=meanAmt, x = State)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

# lets compare maximum amounts
max_state0_aggr = aggregate(f0_merch[,6], list(f0_merch$state), FUN= "max")
max_state0_aggr$f1 = '0'
max_state1_aggr = aggregate(f1_merch[,6], list(f1_merch$state), FUN= "max")
max_state1_aggr$f1 = '1'
state_max_join = rbind(max_state1_aggr, max_state0_aggr)
state_max_join =state_max_join %>% rename(State = Group.1, maxAmt = x, status = f1)

ggplot(state_max_join, aes(fill=status, y=maxAmt, x = State)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()


# comparing the max amounts spent in the non-fraudulent cases and non fraudulent cases
# it is seen that the fraudsters do a good job in masking their activities such that their spending
# is never greater than the max amount of spending that comes from a state. The next point of call might 
# be a person by person analysis.

min_state0_aggr = aggregate(f0_merch[,6], list(f0_merch$state), FUN= "min")
min_state0_aggr$f1 = '0'
min_state1_aggr = aggregate(f1_merch[,6], list(f1_merch$state), FUN= "min")
min_state1_aggr$f1 = '1'

state_min_join = rbind(min_state1_aggr, min_state0_aggr)
state_min_join =state_min_join %>% rename(State = Group.1, minAmt = x, status = f1)

ggplot(state_min_join, aes(fill=status, y=minAmt, x = State)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()
# As expected, the mininum amount spent is way greater when considering the fraudulent and non-fraudulent cases.


##### Splitting the time of transaction
trans = f1_fraud %>% separate(trans_date_trans_time, sep=" ", into = c("date", "time"))
trans1= trans%>% filter(is_fraud ==1)
#there are 7506 transactions that are fraudulent
trans_am = trans1%>%filter(time<12)
# of these, 2907 are in the morning, i.e. 38.7% occur in the morning
tail(trans_am)
trans_pm = trans1%>%filter(time>12) 
# of these, 4599 are in the afternoon, i.e. 61.3% occur in the afternoon

# NON-FRAUDULENT CASES
nfraud_trans = f1_nfraud %>% separate(trans_date_trans_time, sep=" ", into = c("date", "time"))
trans0= nfraud_trans%>% filter(is_fraud ==0)
#there are 1289169  transactions that are non-fraudulent
trans0_am = trans0%>%filter(time<12)
# of these, 505469 are in the morning, i.e. 39.2% occur in the morning
tail(trans_am)
trans0_pm = trans0%>%filter(time>=12) 
# of these, 783700 are in the afternoon, i.e. 60.8% occur in the afternoon

#Generally most transactions occur in the afternoon

# lets look at the percentage of transactions that after 8pm, i.e 20:00 hrs in both cases

fraud_trans_pm = trans1%>%filter(time>=20) 
#3971 of the 4599 transactions after 12pm ~86% occur from 8 pm

nonfraudtrans0_pm = trans0%>%filter(time>=20) 
# 260746 of the  783700 ~33% non fraudulent transactions occur from 8pm

fraud_trans_pm1 = trans1%>%filter(time>=22) 
#3835 of the 3971 fraudulent transactions after 8pm ~97% occur from 10 pm

nonfraud_trans0_pm1 = trans0%>%filter(time>=22) 
#130251 of the 260746 non-fraudulent transactions after 8pm ~50% occur from 10 pm

#making piecharts of the time analysis

# Analysis of the fraudulent activity time data
slices = c(2907,4599)
lbls = c( 'Morning', 'Afternoon')
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, pct)
lbls = paste(lbls, "%", sep = "")
pie3D(slices, labels = lbls, explode = 0.1, main = " Distribution of Fraud Activities Timeline")

# Analysis of the non-fraudulent activity time data
slices = c(505469, 783700)
lbls = c( 'Morning', 'Afternoon')
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, pct)
lbls = paste(lbls, "%", sep = "")
pie3D(slices, labels = lbls, explode = 0.1, main = " Distribution of Non-Fraud Activities Timeline")

# Analysis of the fraudulent activity in the afternoon after 8pm
slices = c(3971, 628)
lbls = c( '12:00 - 20:00 pm', '20:01 - 23:59 pm')
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, pct)
lbls = paste(lbls, "%", sep = "")
pie3D(slices, labels = lbls, explode = 0.1, main = " Distribution of Fraud Activities Post Noon")

# Analysis of the non-fraudulent activity in the afternoon after 8pm
slices = c(260746, 522954)
lbls = c( '12:00 - 20:00 pm', '20:01 - 23:59 pm')
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, pct)
lbls = paste(lbls, "%", sep = "")
pie3D(slices, labels = lbls, explode = 0.1, main = " Distribution of Non-Fraud Activities Post Noon")

# Analysis of the fraudulent activity in the evening after 10pm
slices = c(3835, 136)
lbls = c( '22:00 - 23:59 pm', '20:01 - 21:59 pm')
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, pct)
lbls = paste(lbls, "%", sep = "")
pie3D(slices, labels = lbls, explode = 0.1, main = " Fraction of Fraud Activities After 10pm")

# Analysis of the non-fraudulent activity in the evening after 10pm
slices = c(130251, 130495)
lbls = c( '22:00 - 23:59 pm', '20:01 - 21:59 pm')
pct = round(slices/sum(slices)*100)
lbls = paste(lbls, pct)
lbls = paste(lbls, "%", sep = "")
pie3D(slices, labels = lbls, explode = 0.1, main = " Fraction of Non-Fraud Activities After 10pm")

# Histogram distribution of variables  to decide if transformation is needed
hist(fraud_data$amt, main = "Distribution of Amount in dataset")
hist(f1_fraud$amt, main = "Distribution of Amount in Fraudulent Transactions")
hist(f1_nfraud$amt, main = "Distribution of Amount in Non-Fraudulent Transactions")

#applying  box cox transformation on the fraudulent data

as.numeric(fraud_data$amt)
Box = boxcox(f1_fraud$amt~1, lambda = seq(-3,3,0.1))
cox = data.frame(Box$x, Box$y)
cox2 = cox[with(cox, order(-cox$Box.y)),]
cox2[1,]
lambda = cox2[1,"Box.x"]
lambda
# T_Amt refers to transformed amount data
T_Amt = (f1_fraud$amt ^(lambda - 1)/(lambda))
hist(T_Amt) #histogram of the transformed data

# applying box cox transform on non-fraudulent data
Box = boxcox(f1_nfraud$amt~1, lambda = seq(-3,3,0.1))
cox = data.frame(Box$x, Box$y)
cox2 = cox[with(cox, order(-cox$Box.y)),]
cox2[1,]
lambda = cox2[1,"Box.x"]
lambda
# T_Amt refers to transformed amount data
Nonfraud_transform = (f1_nfraud$amt ^(lambda - 1)/(lambda))
hist(Nonfraud_transform) #histogram of the transformed data


#Exploratory data analysis Cont.
#read in into files
fraud = read.csv("fraudTrain.csv")
fraud_test = read.csv("fraudtest.csv")
prop.table(table(fraud$is_fraud))


#split the date and time into 
fraud = fraud %>% separate (trans_date_trans_time, into = c("date", "time"), sep = " ")
fraud = fraud %>% separate(dob, into = c("year", "month", "day"), sep = "-")
fraud$age = 2020 - as.numeric(fraud$year)

fraud$name = paste(fraud$first, " ", fraud$last)

fraud_test = fraud_test %>% separate (trans_date_trans_time, into = c("date", "time"), sep = " ")
fraud_test = fraud_test %>% separate(dob, into = c("year", "month", "day"), sep = "-")

fraud_test$age = 2020 - as.numeric(fraud_test$year)

fraud_test$name = paste(fraud_test$first, " ", fraud_test$last)

fraud_test$age
#mean of all non-fraud transactions 
fraud %>% group_by(name) %>%
  dplyr::summarise(mean_amt = mean(amt))

fraud[fraud$name=="Aaron   Murray",]


#The results show that the accuracy is very high even 
#with no model at all and leaving the prediction of fraud to random 
#chance. This is because we have an imbalanced dataset. 


#Time is important because certain events are expected to occur
#at similar moments. The customers are a lot of times making transactions at the same time
#this can be useful in the evaluation of customer behavior and also detection of 
#fraud from the time of the day that the transaction occurred. 
#information can be captured for the time stamps on the transactions. 
#the periodic behavior of time has to be modeled in the model. 


#cost of fraud: The money the customer lost from fraud 
total_cost = sum(fraud$amt[fraud$is_fraud=="1"])
total_cost
frauds = fraud[fraud$is_fraud =="1",]

x = frauds %>% group_by(name) %>%
  dplyr::summarise(sum_amt = sum(amt))%>%
  arrange(desc(sum_amt))

y = head(x)
formattable(y)

##datatype of the features 
str(fraud)
str(fraud_test)

##make fraud to become a factor 
fraud$is_fraud= as.factor(fraud$is_fraud)
fraud_test$is_fraud= as.factor(fraud_test$is_fraud)

##visulaization and exploration 
fraud %>%
  count("is_fraud")


ggplot(data = fraud, mapping = aes(x= is_fraud, y = amt))+
  geom_boxplot()

ggplot(data = fraud_new, mapping = aes(x= is_fraud, y = amt))+
  geom_boxplot()

ggplot(data = fraud, mapping = aes(x= is_fraud, y = age))+
  geom_boxplot()

ggplot(data = fraud_new, mapping = aes(x= is_fraud, y = age))+
  geom_boxplot()


##sampling data to balance the dataset 
oversampling_result = ovun.sample(formula = is_fraud ~., data = fraud,
                                  method = "over", N = (1289169/0.5), 
                                  seed = 10)

oversampled_credit = oversampling_result$data
prop.table(table(oversampled_credit$is_fraud))

undersampling_result = ovun.sample(formula = is_fraud ~., data = fraud,
                                   method = "under", N = (7506/0.5), 
                                   seed = 10)

undersampled_credit = undersampling_result$data
prop.table(table(undersampled_credit$is_fraud))

nrow(undersampled_credit)
#under and over sampling
sampling_result = ovun.sample(formula = is_fraud ~., data = fraud,
                              method = "both", N = 100000,p=0.5, 
                              seed = 10)

sampled_credit = sampling_result$data
prop.table(table(sampled_credit$is_fraud))

###use a new dataframe and remove the varaibles which are not of use. 
fraud_new = undersampled_credit[,-c(1,2,4,5,8,9,10,11,12,14,15,16,18,20,21,22,23,24,25)]
fraud_new$time = as.numeric(hms(fraud_new$time))/3600

#remove data not useful from test data 
fraud_new_test = fraud_test[,-c(1,2,4,5,8,9,10,11,12,14,15,16,18,20,21,22,23,24,25)]
fraud_new_test$time = as.numeric(hms(fraud_new_test$time))/3600

##remove more features 
fraud_new = fraud_new[,-c(4,6,9)]
fraud_new_test = fraud_new_test[,-c(4,6,9)]


#MODELING_______________________________________________________________________
##SUPPORT VECTOR MACHINE

tuned = tune.svm(is_fraud~., data = fraud_new, gamma = c(0.1, 0.5,1,2,3,4),
                 cost = 10^2, tunecontrol = tune.control(cross=10), 
                 type = 'C-classification')

#view model results 
print(tuned)
tuned
summary(tuned)
tuned$best.model
tuned$performances
best.rbf = tuned$best.model

#predict the model on test and train data 
rbf.test = predict(best.rbf, newdata = fraud_new_test)
rbf.train = predict(best.rbf, newdata = fraud_new)

##tables 
table(rbf.test, fraud_new_test$is_fraud)
table(rbf.train, fraud_new$is_fraud)

#confusion matrix 
train_y = caret::confusionMatrix(rbf.train, fraud_new$is_fraud, positive = "1")
test_y = caret::confusionMatrix(rbf.test, fraud_new_test$is_fraud, positive = "1")


#model evaluation
test_y$byClass
train_y$byClass
test_y
train_y

#___________________________________
##RANDOM FOREST
control = trainControl(method = "repeatedcv", number = 10, repeats =3 , search = "grid")
tunegrid = expand.grid(.mtry = c(1:3))


metric = "Accuracy"

rf_gridsearch = train(is_fraud~time+amt+age+category+city_pop, data = fraud_new,
                      method ="rf", metric = metric, tuneGrid = tunegrid, trControl = control)

print(rf_gridsearch)
plot(rf_gridsearch)

model_2 = randomForest(is_fraud~time+amt+age+category+city_pop, data = fraud_new, 
                       ntree = 1500, mtry = 3,trControl = control)


##model prediction on test and train 
RF.train = predict(model_2, newdata = fraud_new)
RF.test = predict(model_2, newdata = fraud_new_test)


##tables 
table(RF.train, fraud_new$is_fraud)
table(RF.test, fraud_new_test$is_fraud)

##Confusion Matrix 
test_y_RF = caret::confusionMatrix(RF.test, fraud_new_test$is_fraud, positive = "1")
train_y_RF = caret::confusionMatrix(model_2$predicted, fraud_new$is_fraud, positive = "1")


#model evaluation
test_y_RF$byClass
train_y_RF$byClass
test_y_RF
train_y_RF

#Feature Importance
model_2$importance

#divide data into different part]
fraud_new

amt_below <- filter(fraud_new, amt < 100)
ggplot(data = amt_below, mapping = aes(x= is_fraud, y = amt))+
  geom_boxplot()
amt_btw <- filter(fraud_new, (amt>100)&(amt<1000))
ggplot(data = amt_btw, mapping = aes(x= is_fraud, y = amt))+
  geom_boxplot()
amt_above <- filter(fraud_new, amt >1000)
ggplot(data = amt_above, mapping = aes(x= is_fraud, y = amt))+
  geom_boxplot()
fraud_new

#model them per group 
tune_svm_below = tune.svm(is_fraud~time+amt+age+category+city_pop, data = amt_below,gamma = c(0.1, 0.5,1,2,3,4),
                          tunecontrol = tune.control(cross=10), type = 'C-classification', kernel = 
                            "radial", cost=10^2)

tune_svm_btw = tune.svm(is_fraud~time+amt+age, data = amt_btw, gamma = c(0.1, 0.5,1,2,3,4),
                        cost = 10^2, tunecontrol = tune.control(cross=10), type = 'C-classification',kernel = 
                          "radial")

tune_svm_above = tune.svm(is_fraud~time+amt+age, data = amt_above, gamma = c(0.1, 0.5,1,2,3,4),
                          cost = 10^2, tunecontrol = tune.control(cross=10), type = 'C-classification', kernel = 
                            "radial")


model_2 = randomForest(is_fraud~time+amt+age+category+city_pop, data = amt_below, 
                       ntree = 1500, mtry = 3,trControl = control)
model_3 = randomForest(is_fraud~time+amt+age+category+city_pop, data = amt_btw, 
                       ntree = 1500, mtry = 3,trControl = control)
model_4 = randomForest(is_fraud~time+amt+age+category+city_pop, data = amt_above, 
                       ntree = 1500, mtry = 3,trControl = control)

amt_below_test <- filter(fraud_new_test, amt < 100)
amt_btw_test <- filter(fraud_new_test, (amt>100)&(amt<1000))
amt_above_test <- filter(fraud_new_test, amt > 1000)


RF.test_below = predict(model_2, newdata = amt_below_test)
table(RF.test_below, amt_below_test$is_fraud)

RF.test_btw = predict(model_3, newdata = amt_btw_test)
table(RF.test_btw, amt_btw_test$is_fraud)

RF.test_above = predict(model_4, newdata = amt_above_test)
table(RF.test_above, amt_above_test$is_fraud)

##Confusion Matrix 
test_y_RF_below = caret::confusionMatrix(RF.test_below, amt_below_test$is_fraud, positive = "1")
test_y_RF_below$byClass

test_y_RF_btw = caret::confusionMatrix(RF.test_btw, amt_btw_test$is_fraud, positive = "1")
test_y_RF_btw$byClass

test_y_RF_above = caret::confusionMatrix(RF.test_above, amt_above_test$is_fraud, positive = "1")
test_y_RF_above$byClass
#___________________________________
#RANDOM FOREST II, LOGISTIC REGRESSION, DECISION TREE
Test = read.csv("fraudTest.csv")
Train = read.csv("fraudTrain.csv")
# converting date and time to date format
Train$trans_date_trans_time = as_datetime(Train$trans_date_trans_time)
Test$trans_date_trans_time = as_datetime(Test$trans_date_trans_time)

# extraction of age from date of birth for both train and test data
Train$Time = Train$trans_date_trans_time - Train$trans_date_trans_time[1]
Train$age = floor(age_calc(as.Date(Train$dob), units = "years"))

Test$Time = Test$trans_date_trans_time - Test$trans_date_trans_time[1]
Test$age = floor(age_calc(as.Date(Test$dob), units = "years"))


Train$name = paste(Train$first, " ", Train$last)
nlevels(factor(Train$name))  # 973 distinct names 
# I won't use this variable in my prediction

Test$name = paste(Test$first, " ", Test$last)
nlevels(factor(Test$name))  # 917 distinct names 

# Extraction of important variables as determined from exploratory data analysis
Train_new = Train[,c(5,6,9,12,16,23:26)]
Test_new = Test[,c(5,6,9,12,16,23:26)]

# sampling the data to eliminate skewness/imbalance
undersampling = ovun.sample(formula = is_fraud ~., data = Train_new,
                            method = "under", N = (7506/0.5), 
                            seed = 10)
undersamplingTest = ovun.sample(formula = is_fraud ~., data = Test_new,
                                method = "under", N = (7506/0.5), 
                                seed = 10)
# here, fraud cases are over-sampled while normal transactions are undersampled
both_result = ovun.sample(formula = is_fraud ~., data = Train_new,
                          method = "both", N = 200000,p=0.5, 
                          seed = 10)
under_sample = undersampling$data
under_sampleTest = undersamplingTest$data
under_over = both_result$data


#___________________________________
##RANDOM FOREST II
# model built on undersampled data
RF <- randomForest(data = under_sample, is_fraud ~ amt+city_pop+
                     Time+age+category, importance = T, ntrees=500, mtry=3)
### We were able to run the model below on a 16gb RAM computer. If your computer
# cannot allocate enough memory to it, we suggest you clear up the global 
# environment and start running this notebook from line 567 so as to have enough
# memory to allocate to the random forest model RF2 below. Thanks

# model built on oversampled (actually a sampling in both directions) data
RF2 <- randomForest(data = under_over, is_fraud ~ amt+city_pop+
                      Time+age+category, importance = T, ntrees=500, mtry=3)

##### Evaluation of model on the sampled data used in model development
predRF = predict(RF, newdata = under_sample)
predRF2 = predict(RF2, newdata = under_over)

logLoss(under_sample$is_fraud,predRF) #0.02803996
logLoss(under_over$is_fraud,predRF2) #0.002611031

conRF1 = caret::confusionMatrix(factor(as.numeric(predRF>0.5)),factor(under_sample$is_fraud), positive="1") #
conRF2 = caret::confusionMatrix(factor(as.numeric(predRF2>0.5)),factor(under_over$is_fraud), positive="1") #

##### Evaluation of models on full, imbalanced Train Data ###########
pred_RF = predict(RF, newdata = Train_new)    # model built on undersampled data
pred_RF2 = predict(RF2, newdata = Train_new)  # model built on oversampled data

logLoss(Train_new$is_fraud,pred_RF) #0.0956855
logLoss(Train_new$is_fraud,pred_RF2) #0.02229709

conRF3 = caret::confusionMatrix(factor(as.numeric(pred_RF>0.5)),factor(Train_new$is_fraud), positive="1") #
conRF4 = caret::confusionMatrix(factor(as.numeric(pred_RF2>0.5)),factor(Train_new$is_fraud), positive="1") #

##### Evaluation of model on full, imbalanced Test Data ############
predTRF = predict(RF, newdata = Test_new)
predTRF2 = predict(RF2, newdata = Test_new)

logLoss(Test_new$is_fraud,predTRF) #0.1024986
logLoss(Test_new$is_fraud,predTRF2) #0.02663759

conRF5 = caret::confusionMatrix(factor(as.numeric(predTRF>0.5)),factor(Test_new$is_fraud), positive="1") #
conRF6 = caret::confusionMatrix(factor(as.numeric(predTRF2>0.5)),factor(Test_new$is_fraud), positive="1") #


#___________________________________
##LOGISTIC REGRESSION

fit <- glm(data = under_sample, is_fraud ~ amt+gender+state+city_pop+
             Time+age+category, family = "binomial")
fit2 = glm(data = under_sample, is_fraud ~ amt+city_pop+
             Time+age+category, family = "binomial")
fit3 = glm(data = under_over, is_fraud ~ amt+city_pop+
             Time+age+category, family = "binomial")
summary(fit)
summary(fit2)
summary(fit3)
#### result on sampled train data
logLoss(under_sample$is_fraud,fit$fitted.values) #0.3638177
logLoss(under_sample$is_fraud,fit2$fitted.values) #0.3670037
logLoss(under_over$is_fraud,fit3$fitted.values) #0.3638437

# confusion matrix evaluation
conf_lr2 = caret::confusionMatrix(factor(under_sample$is_fraud),factor(as.numeric(fit2$fitted.values>0.5)), positive="1") #0.8178 
conf_lr3 = caret::confusionMatrix(factor(under_over$is_fraud),factor(as.numeric(fit3$fitted.values>0.5)), positive="1") #0.8256
# oversampling doesn't seem to have much effect on the result of the logistic regression

#### result on imbalanced Train Data
fitpred = predict(fit, newdata = Train_new)
fitpred2 = predict(fit2, newdata = Train_new)
fitpred3 = predict(fit3, newdata = Train_new)

# logLoss for prediction on imbalanced data
logLoss(Train_new$is_fraud,fitpred2) #0.6135498
logLoss(Train_new$is_fraud,fitpred3) #0.6164157
# oversampling logLoss is even higher, i.e worse

con2 = caret::confusionMatrix(factor(as.numeric(fitpred2>0.5)),factor(Train_new$is_fraud),
                              positive="1")
con3 = caret::confusionMatrix(factor(as.numeric(fitpred3>0.5)),factor(Train_new$is_fraud),
                              positive="1")
# precision is slightly better for the model built on the oversampled data

#### result on imbalanced Test Data
fitpredT2 = predict(fit2, newdata = Test_new)
fitpredT3 = predict(fit3, newdata = Test_new)
confTest2 = caret::confusionMatrix(factor(as.numeric(fitpredT2>0.5)),factor(Test_new$is_fraud),
                                   positive="1")
confTest3 = caret::confusionMatrix(factor(as.numeric(fitpredT3>0.5)),factor(Test_new$is_fraud),
                                   positive="1")



#___________________________________
##DECISION TREE
DecT2 <-rpart(data = under_sample, is_fraud ~ amt+city_pop+
                Time+age+category,                   
              parms=list(split="gini"),   #can change to information gain
              control=rpart.control(xval=10)  ) # built on undersampled data

DecT3 <-rpart(data = under_over, is_fraud ~ amt+city_pop+
                Time+age+category,                   
              parms=list(split="gini"),   #can change to information gain
              control=rpart.control(xval=10)  )  # built on oversampled data

summary(DecT2)
printcp(DecT2)

plotcp(DecT2)
fancyRpartPlot(DecT2)

# finding min error
cp_min2 = DecT2$cptable[which.min(DecT2$cptable[,"xerror"]),"CP"]
cp_min3 = DecT3$cptable[which.min(DecT3$cptable[,"xerror"]),"CP"]

DecTP = prune(DecT2,cp=cp_min2) # pruning to cp of min error
DecTP3 = prune(DecT3,cp=cp_min3) # pruning to cp of min error


prp(DecTP,type=2)
prp(DecTP, type=2, extra=100, nn=TRUE, fallen.leaves=TRUE,
    faclen=0,varlen=0, shadow.col="grey", branch.lty=3)

fancyRpartPlot(DecTP)
fancyRpartPlot(DecTP3)

#### models tested on the sampled train data used in building them
predDT = predict(DecTP, newdata = under_sample)
predDT3 = predict(DecTP3, newdata = under_over)


logLoss(under_sample$is_fraud,predDT)  #0.2030567
logLoss(under_over$is_fraud,predDT3)  #0.1870239

con_DT1 = caret::confusionMatrix(factor(under_sample$is_fraud),factor(as.numeric(predDT>0.5)), positive="1") #
con_DT2 = caret::confusionMatrix(factor(under_over$is_fraud),factor(as.numeric(predDT3>0.5)), positive="1") #


# tested on imbalanced train data
predDT1 = predict(DecTP, newdata = Train_new)
predDT3 = predict(DecTP3, newdata = Train_new)


logLoss(Train_new$is_fraud,predDT1) #0.1943065
logLoss(Train_new$is_fraud,predDT3) #0.1876037

confDT = caret::confusionMatrix(factor(as.numeric(predDT1>0.5)),factor(Train_new$is_fraud), positive="1") #
confDT3 = caret::confusionMatrix(factor(as.numeric(predDT3>0.5)),factor(Train_new$is_fraud), positive="1") #

# tested on imbalanced test data

predDT_1 = predict(DecTP, newdata = Test_new)
predDT_3 = predict(DecTP3, newdata = Test_new)


logLoss(Test_new$is_fraud,predDT_1) #0.1940856
logLoss(Test_new$is_fraud,predDT_3) #0.1874587

confDT_1 = caret::confusionMatrix(factor(as.numeric(predDT_1>0.5)),factor(Test_new$is_fraud), positive="1") #
confDT_3 = caret::confusionMatrix(factor(as.numeric(predDT_3>0.5)),factor(Test_new$is_fraud), positive="1") #
