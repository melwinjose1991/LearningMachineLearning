### Exploratory Data Analysis - train & transactions



#%% Imports
import pandas as pd
import numpy as np
import os
from ggplot import *



#%% Reading the csv files
os.chdir("C:\\Users\\Melwin Jose\\workspace\\LearningMachineLearning\\kaggle\\KKBox")

eda_train_data = pd.read_csv("data/train.csv")
eda_transactions_data = pd.read_csv("data/transactions.csv")

eda_train_data.head()
eda_transactions_data.head()

eda_train_data.dtypes
eda_transactions_data.dtypes



#%% payment_method_id
tmp = eda_transactions_data.ix[:,0:2].copy()
print("# of nulls in payment_method_id : ", pd.isnull(tmp.payment_method_id).sum())
print("Unique values for payment_method_id : ", tmp.payment_method_id.unique().size)

merged_df = pd.merge(eda_train_data, tmp, on='msno', how='left')
pt = pd.pivot_table(merged_df, index=["payment_method_id"], values=["is_churn"], aggfunc=[len, np.sum, np.mean])
pt = pd.DataFrame(pt.to_records())

