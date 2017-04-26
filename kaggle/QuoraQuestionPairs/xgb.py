import numpy as np 
import pandas as pd
import os
import gc
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.metrics import log_loss

# returns a data-frame
df_train = pd.read_csv('data/train.csv')
df_train.head()

df_test = pd.read_csv('data/test.csv')
df_test.head()



## Data frame analysis of train
print('Total number of question pairs for training: {}'.format(len(df_train)))
print('Duplicate pairs: {}%'.format(round(df_train['is_duplicate'].mean()*100, 2)))

# 1D ndarray
qids = pd.Series(df_train['qid1'].tolist() + df_train['qid2'].tolist())

print('Total number of questions in the training data: {}'.format(len(np.unique(qids))))
print('Number of questions that appear multiple times: {}'.format(np.sum(qids.value_counts() > 1)))


plt.figure(figsize=(12, 5))
plt.hist(qids.value_counts(), bins=50)
plt.yscale('log', nonposy='clip')
plt.title('Log-Histogram of question appearance counts')
plt.xlabel('Number of occurences of question')
plt.ylabel('Number of questions')
plt.show()


## Data frame analysis of test
print('Total number of question pairs for testing: {}'.format(len(df_test)))


## Avg Prediction
p = df_train['is_duplicate'].mean()
print('Predicted score:', log_loss(df_train['is_duplicate'], np.zeros_like(df_train['is_duplicate']) + p))

sub = pd.DataFrame({'test_id': df_test['test_id'], 'is_duplicate': p})
sub.to_csv('naive_submission.csv', index=False)
sub.head()
