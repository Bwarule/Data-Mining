# -*- coding: utf-8 -*-
"""
Created on Mon Apr 16 10:26:50 2018

@author: bharat.warule
"""

# D:\Data_Analysis_Exercise\Data Analysis Exercise
import gc
gc.collect()

# import itertools
import pandas as pd
import numpy as np
# from numpy import nanpercentile
# Load libraries
# from sklearn.cross_validation import cross_val_score
# from sklearn.linear_model import LogisticRegression
# from sklearn.ensemble import RandomForestClassifier
# from sklearn.feature_extraction.text import TfidfVectorizer
# from sklearn.metrics import classification_report
# from sklearn.feature_extraction.text import TfidfTransformer
# from nltk.stem.porter import PorterStemmer
# from sklearn.model_selection import cross_val_score
# from sklearn.datasets import make_classification
# from sklearn import preprocessing

DataIn=pd.read_csv('D:/Data_Analysis_Exercise/Data Analysis Exercise/trainingData.csv')
train = DataIn
# Function definition missing value
def missing_value( DataIn, variable_list ):
   # Add both the parameters and return them."
   from sklearn.preprocessing import Imputer
   
   # Impute Missing Values With Means 
   # Create an imputer object that looks for 'Nan' values, then replaces them with 
   # the mean value of the feature by columns (axis=0)
   mean_imputer = Imputer(missing_values='NaN', strategy='mean', axis=0)
   # Train the imputor on the df dataset
   mean_imputer = mean_imputer.fit(DataIn[variable_list])
   # Apply the imputer to the df dataset
   imputed_df = mean_imputer.transform(DataIn[variable_list].values)
   return imputed_df;
   
# Function definition percentile_value
def percentile_value( DataIn ):
    # Add both the parameters and return them."
    import numpy as np
    variable_list =  DataIn.select_dtypes(['float64','int64']).columns
    # Create a list to store the data
    outlier_data = pd.DataFrame([])
    for variable in variable_list:
	# value_q = train[variable].quantile([.05, .95]) # ----- 
	#-----pandas function which doesn't work missing 
      value_q = np.percentile(DataIn[variable].dropna(), [5,95])
      value_q = pd.DataFrame(value_q,index=['pct_5','pct_95']).T
      value_q['variable_name'] = pd.Series(variable, index=value_q.index)
	# outlier_data.append(value_q)
      outlier_data = outlier_data.append(value_q);
    return outlier_data;
    
# Function definition percentile_value
def percentile_capData( DataIn ):
    # Add both the parameters and return them."
    import numpy as np
    variable_list =  DataIn.select_dtypes(['float64','int64']).columns
    # Create a list to store the data
    outlier_data = pd.DataFrame([])
    for variable in variable_list:
	# value_q = train[variable].quantile([.05, .95]) # ----- 
	#-----pandas function which doesn't work missing 
      value_q = np.percentile(DataIn[variable].dropna(), [5,95])
      value_q = pd.DataFrame(value_q,index=['pct_5','pct_95']).T
      value_q['variable_name'] = pd.Series(variable, index=value_q.index)
	# outlier_data.append(value_q)
      outlier_data = outlier_data.append(value_q);
    # nrow = outlier_data.shape[0]
    # cap the outliers using percentile
    for variable in variable_list:
        print(DataIn[variable].describe())
        lowerpct = outlier_data.loc[outlier_data['variable_name']==variable,'pct_5'].values
        upperpct = outlier_data.loc[outlier_data['variable_name']==variable,'pct_95'].values
        lowerpct = float(lowerpct) # lowerpct.astype(float)
        upperpct = float(upperpct) # lowerpct.astype(float)
        # outlier_data.iloc[outlier_data['variable_name']==variable]['pct_5']
        # DataIn.ix[DataIn[variable] > pd.to_numeric(lowerpct), variable] = lowerpct
        DataIn[variable].where(DataIn[variable] < lowerpct, lowerpct, inplace=True)
        DataIn[variable].where(DataIn[variable] > upperpct, upperpct, inplace=True)
        # DataIn[variable] = DataIn[variable].mask(DataIn[variable] < lowerpct, lowerpct, axis=1)
        # DataIn[variable] = DataIn[variable].mask(DataIn[variable] > upperpct, upperpct, axis=1)
        print(DataIn[variable].describe());
              
    return outlier_data,DataIn;

primes = train.select_dtypes(['float64','int64']).columns
# np.percentile(DataIn['monthly_expenses'].dropna(), [5,95])
# Create a list to store the data
outlier_data = pd.DataFrame([])

for prime in primes:
    # value_q = train[primes[prime]].quantile([.05, .95])
    # value_q = train[prime].quantile([.05, .95]) # ----- pandas function which doesn't work missing 
    value_q = np.percentile(train[prime], [5,95])
    value_q = pd.DataFrame(value_q,index=['pct_5','pct_95']).T
    value_q['variable_name'] = pd.Series(prime, index=value_q.index)
    # outlier_data.append(value_q)
    outlier_data = outlier_data.append(value_q);
   
# Function definition cap the outliers using 
df = pd.DataFrame(np.random.randint(0,100,size=(5, 2)), columns=list('AB'))
down_quantiles = df.quantile(0.05)
df.mask(df < down_quantiles, down_quantiles, axis=1)   
up_quantiles = df.quantile(0.95)
df.mask(df > up_quantiles, up_quantiles, axis=1)   

## short code --- reemove missing or impute before capping 
df = train.dropna(axis=0, how='any')
df = df[primes]
df = df.mask(df < df.quantile(0.05), df.quantile(0.05), axis=1)
df = df.mask(df > df.quantile(0.95), df.quantile(0.95), axis=1)
train.describe()   
df.describe()   
# Create a list of the features we will eventually want for our model
features = train.select_dtypes(['float64','int64']).columns
# selected only continous variable 

train.ix[train.loan_amount > 15000, 'loan_amount'] = 15000
# train['loan_amount'] > 15000
train.count() - train['sanitary_availability'].count()
train.columns
value_q = train['monthly_expenses'].quantile([.05, .95])
value_q.to_frame().T
train.count()
train['monthly_expenses'].type()
train['monthly_expenses'].describe()

# Impute Missing Values With Means 
from sklearn.preprocessing import Imputer
# Create an imputer object that looks for 'Nan' values, then replaces them with 
# the mean value of the feature by columns (axis=0)
mean_imputer = Imputer(missing_values='NaN', strategy='mean', axis=0)

# Train the imputor on the df dataset
mean_imputer = mean_imputer.fit(train[features])
# Apply the imputer to the df dataset
imputed_df = mean_imputer.transform(train[features].values)
## error :  could not convert string to float: 'Meat Businesses'
# View Data
# View the data

train[features].describe()
train[features] = mean_imputer.transform(train[features].values)
trainimputed = missing_value( train,  features)
