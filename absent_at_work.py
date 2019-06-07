#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Fri Oct  5 09:56:02 2018

@author: rahul
"""

#importing libraries
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

#Importing the dataset
df = pd.read_excel('Absenteeism_at_work_Project.xls')

#describing the dataset to getthe statistics of the dataset 
df.describe()

#More information on dataset
df.info()

#getting the dimension of dataset
df.shape

#replacing space in names of columns with underscor
df.columns = df.columns.str.replace(' ', '_')

#check whether there are missing values in the dataframe
count_missing_values = pd.DataFrame(df.isnull().sum())
print(count_missing_values)

# Droping observation in which "Absenteeism time in hours" has missing value
df = df.drop(df[df['Absenteeism_time_in_hours'].isnull()].index, axis=0)
print(df.shape)

#checking null values in target variable
print(df['Absenteeism_time_in_hours'].isnull().sum())

## Imputing Missing Values using mean to fill 
df = df.fillna(df.mean())

#verifing again the presence of missing values in the dataframe
verify_missing_values = pd.DataFrame(df.isnull().sum())
print(verify_missing_values)

#plotting the histograms 
df.hist(figsize=(15,12))
plt.subplots_adjust(left=0.125, bottom=0.1, right=0.9, top= 0.9, wspace=0.5, hspace=0.9)
plt.show()

#checking distribution of the 'variables' with the 'density' plot
df.plot.density(figsize=(15,12), subplots = True , layout=(8,3), sharex=False)
plt.show()

#checking the outliers with box plot of each variables
df.plot.box(figsize=(15,12), subplots = True , layout=(8,3), sharex=False, sharey= False)
plt.subplots_adjust(left=0.125, bottom=0.1, right=0.9, top= 0.9, wspace=0.5, hspace=0.9)
plt.show()

#Removing the outliers
columns_with_outliers =['Service_time', 'Pet', 'Age', 'Work_load_Average/day_', 'Transportation_expense',
       'Hit_target', 'Education' , 'Height', 'Absenteeism_time_in_hours']       

for col in columns_with_outliers:
    
    quartile_75, quartile_25 = np.percentile(df[col], [75,25])       # Fetching quartile at 25th and 75th Percentile
    interquartile_range = quartile_75 - quartile_25                  # Finding difference to get interquartile range
    
    lower_limit = quartile_25 - (interquartile_range*1.5)            # Defining lower limit
    upper_limit = quartile_75 + (interquartile_range*1.5)            # Defining upper limit
    
    df.loc[df[col]< lower_limit,col] = np.nan                        # Replacing the outliers values with NA.
    df.loc[df[col]> upper_limit,col] = np.nan


df = df.fillna(df.mean())                                            # Imputing missing values with mean
df.isnull().sum().sum()                                              # Checking if there is any missing value

#checking the box plot of each variables without outliers
df.plot.box(figsize=(15,12), subplots = True , layout=(8,3), sharex=False, sharey= False)
plt.subplots_adjust(left=0.125, bottom=0.1, right=0.9, top= 0.9, wspace=0.5, hspace=0.9)
plt.show()

#Forming the input and target data
X = df[['ID', 'Reason_for_absence', 'Month_of_absence', 'Day_of_the_week','Seasons','Transportation_expense','Distance_from_Residence_to_Work', 'Service_time', 'Age','Work_load_Average/day_', 'Hit_target', 'Disciplinary_failure','Education', 'Son', 'Social_drinker', 'Social_smoker', 'Pet', 'Weight', 'Height', 'Body_mass_index']]
y = df[['Absenteeism_time_in_hours']]

#splitting the datasets
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

##applying model
from sklearn.linear_model import LinearRegression
model = LinearRegression()
fit_model = model.fit(X_train, y_train)
predict_model = fit_model.predict(X_train)
test_predict = fit_model.predict(X_test)

print('\t')

print('Coefficients: \n', model.coef_)

print('\t')

#cross validating through mean_square_error
from sklearn.metrics import mean_squared_error
print("Mean squared error: %.2f"
      % mean_squared_error(y_test, test_predict))





