import pandas as pd
import numpy as np
import graphviz as gv
import random

from sklearn import tree
from sklearn.model_selection import train_test_split

print("Reading data")
data = pd.read_csv("data/train_indessa.csv")

## Categorical Variables
data['term'] = data['term'].astype('category')
data['batch_enrolled'] = data['batch_enrolled'].astype('category')
data['grade'] = data['grade'].astype('category')
data['sub_grade'] = data['sub_grade'].astype('category')
data['emp_length'] = data['emp_length'].astype('category')
data['home_ownership'] = data['home_ownership'].astype('category')

data['verification_status'] = data['verification_status'].astype('category')
data['pymnt_plan'] = data['pymnt_plan'].astype('category')
data['purpose'] = data['purpose'].astype('category')
# XXX data['title'] = data['title'].astype('category')
data['zip_code'] = data['zip_code'].astype('category')
data['addr_state'] = data['addr_state'].astype('category')
data['application_type'] = data['application_type'].astype('category')
data['initial_list_status'] = data['initial_list_status'].astype('category')
data['verification_status_joint'] = data['verification_status_joint'].astype('category')
data['last_week_pay'] = data['last_week_pay'].astype('category')

data['loan_status'] = data['loan_status'].astype('category') 
cat_columns = data.select_dtypes(['category']).columns

data_star = data[cat_columns].copy()
data_star[cat_columns] = data_star[cat_columns].apply(lambda x: x.cat.codes)


## annual_inc
data_star['annual_inc']=data['annual_inc']
inc_nan_rows =  data_star['annual_inc'].index[data_star['annual_inc'].apply(np.isnan)]
inc_mean = np.mean(data_star['annual_inc'])
data_star.loc[inc_nan_rows,'annual_inc'] = inc_mean

## loan_amnt
data_star['loan_amnt']=data['loan_amnt']

## funded_amnt
data_star['funded_amnt']=data['funded_amnt']

## pub_rec
pub_rec_nan_rows =  data['pub_rec'].index[data['pub_rec'].apply(np.isnan)]
data_star['pub_rec'] = data.set_value(pub_rec_nan_rows, col='pub_rec', value=0)['pub_rec']

## funded_amnt_inv
data_star['funded_amnt_inv']=data['funded_amnt_inv']

## Predictors 
predictors = ['term', 'batch_enrolled', 'grade', 'sub_grade', 'emp_length', 'home_ownership', 'verification_status', \
              'pymnt_plan', 'purpose', 'zip_code', 'addr_state', 'application_type', 'initial_list_status', 'verification_status_joint', \
              'verification_status_joint', 'last_week_pay', \
              'annual_inc', 'loan_amnt', 'funded_amnt', 'funded_amnt_inv', 'pub_rec']

## Random Forest
train, test = train_test_split(data_star, test_size = 0.25)

print("Training on train")
#rf = RandomForestClassifier(n_estimators=100, n_jobs=4)
rf = tree.DecisionTreeClassifier(min_samples_split=0.02)
rf_fit = rf.fit(train[predictors], train[['loan_status']].values.ravel())

with open("tree.dot", 'w') as f:
    f = tree.export_graphviz(rf_fit, out_file=f)

print("Predicting test")
Y_pred = rf_fit.predict(test[predictors])
Y_actual = [test['loan_status']]
total = len((Y_actual == Y_pred)[0])
correct = np.sum(Y_actual == Y_pred)

print("accuracy:",float(correct)/total)


## Test Data
if True:
    ## test
    data = pd.read_csv("data/test_indessa.csv")
    
    data['term'] = data['term'].astype('category')
    data['batch_enrolled'] = data['batch_enrolled'].astype('category')
    data['grade'] = data['grade'].astype('category')
    data['sub_grade'] = data['sub_grade'].astype('category')
    data['emp_length'] = data['emp_length'].astype('category')
    data['home_ownership'] = data['home_ownership'].astype('category')
    data['verification_status'] = data['verification_status'].astype('category')
    data['pymnt_plan'] = data['pymnt_plan'].astype('category')
    data['purpose'] = data['purpose'].astype('category')
    # XXX data['title'] = data['title'].astype('category')
    data['zip_code'] = data['zip_code'].astype('category')
    data['addr_state'] = data['addr_state'].astype('category')
    data['application_type'] = data['application_type'].astype('category')
    data['initial_list_status'] = data['initial_list_status'].astype('category')
    data['verification_status_joint'] = data['verification_status_joint'].astype('category')
    data['last_week_pay'] = data['last_week_pay'].astype('category')
    cat_columns = data.select_dtypes(['category']).columns
    
    data_star = data[cat_columns].copy()
    data_star[cat_columns] = data_star[cat_columns].apply(lambda x: x.cat.codes)
    
    data_star['annual_inc']=data['annual_inc']
    inc_nan_rows =  data_star['annual_inc'].index[data_star['annual_inc'].apply(np.isnan)]
    inc_mean = np.mean(data_star['annual_inc'])
    data_star.loc[inc_nan_rows,'annual_inc'] = inc_mean
    
    data_star['loan_amnt']=data['loan_amnt']
    data_star['funded_amnt']=data['funded_amnt']
    data_star['funded_amnt_inv']=data['funded_amnt_inv']
    
    pub_rec_nan_rows =  data['pub_rec'].index[data['pub_rec'].apply(np.isnan)]
    data_star['pub_rec'] = data.set_value(pub_rec_nan_rows, col='pub_rec', value=0)['pub_rec']

 
    Y_pred = rf_fit.predict_proba(data_star[predictors])
    results = pd.DataFrame({'member_id': data['member_id'], 'loan_status':Y_pred[:,[0]].flatten()})
    results = results[['member_id','loan_status']]
    results.to_csv("dt_results.csv", index=False)


