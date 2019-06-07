#importing library
library("readxl")
library("ggplot2")

#Importing the dataset
df <- read_excel('Absenteeism_at_work_Project.xls')

#getting the names of the column in the dfset
colnames(df)

colnames(df) = gsub(" ", "_", colnames(df))

#getting the dimension of dfset
dim(df)

#getting the summary of the dfset
summary(df)

missing_values_count = data.frame(apply(df,2,function(x){sum(is.na(x))}))
names(missing_values_count)[1] =  "Missing_values_count"
print(missing_values_count)

# Droping observation in which "Absenteeism time in hours" has missing value
df = df[!is.na(df$Absenteeism_time_in_hours), ]

#checking null values in target variable
sum(is.na(df$Absenteeism_time_in_hours))

#Checking Dimension again with removed NA's from Target Variable
dim(df)

## Imputing Missing Values using mean to fill the dataframe
fillNAwithMean <- function(x){
  na_index <- which(is.na(x))        
  mean_x <- mean(x, na.rm=T)
  x[na_index] <- mean_x
  return(x)
}

(df <- apply(df,2,fillNAwithMean))

#checking if NA exits
sum(is.na(df))

apply(df, 2, function(x) any(is.na(x)))

#Density plots
library("kdensity")
plot(density(df$ID))
plot(density(df$Reason_for_absence))
plot(density(df$Month_of_absence))
plot(density(df$Day_of_the_week))
plot(density(df$Seasons))
plot(density(df$Transportation_expense))
plot(density(df$Distance_from_Residence_to_Work))
plot(density(df$Service_time))
plot(density(df$Age))
plot(density(df$Work_load_Average.day))
plot(density(df$Hit_target))
plot(density(df$Absenteeism_time_in_hours))
plot(density(df$Body_mass_index))
plot(density(df$Height))
plot(density(df$Weight))
plot(density(df$Pet))
plot(density(df$Social_smoker))
plot(density(df$Social_drinker))
plot(density(df$Son))
plot(density(df$Education))
plot(density(df$Disciplinary_failure))

#boxplots
boxplot(ID  ~ Absenteeism_time_in_hours, data = df)
boxplot(Reason_for_absence  ~ Absenteeism_time_in_hours, data = df)
boxplot(Month_of_absence  ~ Absenteeism_time_in_hours, data = df)
boxplot(Day_of_the_week  ~ Absenteeism_time_in_hours, data = df)
boxplot(Seasons  ~ Absenteeism_time_in_hours, data = df)
boxplot(Work_load_Average.day  ~ Absenteeism_time_in_hours, data = df)
boxplot(Age  ~ Absenteeism_time_in_hours, data = df)
boxplot(Hit_target  ~ Absenteeism_time_in_hours, data = df)
boxplot(Disciplinary_failure  ~ Absenteeism_time_in_hours, data = df)
boxplot(Education  ~ Absenteeism_time_in_hours, data = df)
boxplot(Son  ~ Absenteeism_time_in_hours, data = df)
boxplot(Social_smoker  ~ Absenteeism_time_in_hours, data = df)
boxplot(Social_drinker  ~ Absenteeism_time_in_hours, data = df)
boxplot(Pet  ~ Absenteeism_time_in_hours, data = df)
boxplot(Weight  ~ Absenteeism_time_in_hours, data = df)
boxplot(Height  ~ Absenteeism_time_in_hours, data = df)
boxplot(Body_mass_index  ~ Absenteeism_time_in_hours, data = df)

#histograms for each column
for (col in 2:ncol(df)) {
  hist(df[,col])
}

#density details
apply(df,  2,  density, kernel="gaussian", bw=15)

#splitting the data into train/test
set.seed(1)
row.number <- sample(1:nrow(df), 0.8*nrow(df))
train = df[row.number,]
#train = data.frame(train)
test = df[-row.number,]
#test = data.frame(test)
dim(train)
dim(test)

#response of the target variable
ggplot(train, aes(Absenteeism_time_in_hours)) + geom_density(fill="blue")
ggplot(train, aes(log(Absenteeism_time_in_hours))) + geom_density(fill="green")
ggplot(train, aes(sqrt(Absenteeism_time_in_hours))) + geom_density(fill="brown")

#linear regression model with first parameter
library(mlbench)
library(caret)
model1 = lm(Reason_for_absence ~ Absenteeism_time_in_hours, data=train, na.action=na.omit)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

#removing the redundant features 
model2 = update(model1, ~.-Pet-Height-Son-Weight-Social_smoker-Social_drinker) 
summary(model2)
par(mfrow=c(2,2))
plot(model2)


#Lets  make default model and add square term in the model.
model3 = lm(Absenteeism_time_in_hours~Month_of_absence+Day_of_the_week+Seasons+Transportation_expense+Distance_from_Residence_to_Work+
              Service_time+Work_load_Average.day+Body_mass_index+I(Month_of_absence^2)+ I(Day_of_the_week^2)+I(Seasons^2)+ I(Transportation_expense^2)+ I(Distance_from_Residence_to_Work^2)+ 
              I(Service_time^2)+ I(Work_load_Average.day^2)+ I(Body_mass_index^2), data=train)
summary(model3)
plot(model3)

##Removing the insignificant variables of model3
model4=update(model3, ~.-Body_mass_index-I(Body_mass_index^2))
summary(model4)
par(mfrow=c(2,2))
plot(model4)

#predicting the test data performance with different models developed
pred1 <- predict(model4, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$Absenteeism_time_in_hours)^2)/length(test$Absenteeism_time_in_hours))
c(RMSE = rmse, R2=summary(model4)$r.squared)
par(mfrow=c(1,1))
plot(pred1)

pred2 <- predict(model3, newdata = test)
rmse <- sqrt(sum((exp(pred2) - test$Absenteeism_time_in_hours)^2)/length(test$Absenteeism_time_in_hours))
c(RMSE = rmse, R2=summary(model3)$r.squared)
par(mfrow=c(1,1))
plot(pred2)



