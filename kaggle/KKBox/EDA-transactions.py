### Exploratory Data Analysis



#%% Imports
import pandas as pd
import os



#%% reading the csv files
os.chdir("C:\\Users\\USMEJOS\\workspace\\personal\\kaggle\\KKBox")

eda_train_data = pd.read_csv("data/train.csv")
eda_transactions_data = pd.read_csv("data/transactions.csv")

eda_train_data.head()
eda_transactions_data.head()

eda_train_data.dtypes
eda_transactions_data.dtypes



#%%