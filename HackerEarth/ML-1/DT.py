import pandas as pd
import numpy as np
from sklearn import tree
import graphviz as gv
import pydotplus

data = pd.read_csv("data/train_indessa.csv")

## Getting values
# data.iloc[i] for getting the ith row
# data.iloc[i]["<column>"] for getting the value of a column 
#        of ith entry

## Column operations
# data.dtypes to get the types of column
# data.<column>.unique() to get unique values in the column
# data['term'].isnull().values.any() check if any entry is NaN

data['grade'] = data['grade'].astype('category')
data['term'] = data['term'].astype('category')
data['emp_length'] = data['emp_length'].astype('category')

data['loan_status'] = data['loan_status'].astype('category') 

data_star = data[['grade','term','emp_length','loan_status']].copy()
cat_columns = data.select_dtypes(['category']).columns
data_star[cat_columns] = data_star[cat_columns].apply(lambda x: x.cat.codes)

dt = tree.DecisionTreeClassifier()
dt_fit = dt.fit(data_star[['grade','term','emp_length']], data_star[['loan_status']])

with open("tree.dot", 'w') as f:
    f = tree.export_graphviz(dt_fit, out_file=f)
graph = pydotplus.graph_from_dot_data(f) 
graph.write_pdf("tree.pdf") 

#dt_fit.predict(data_star.iloc[0:100][['grade','term','emp_length']])

