Competition Link : https://www.hackerearth.com/challenge/competitive/machine-learning-challenge-2/
Final Leaderboard Standing #6 : https://www.hackerearth.com/challenge/competitive/machine-learning-challenge-2/leaderboard/

Techniques :
1> word2vec : n=500  
2> Engineered features  
	- One hot encoding of top words in 'desc' of train and test - stopwords removed  
	- length of each of the string features  
	- difference between timestamp features in seconds  
	- month/year/day of the timestamp features  
	- converted goal to USD and used the log form of it  
2> Classifier : hyper-parameter tuned xgboost
(Tried KNN and LASSO regression, didn't a promissing results)  
3> Majority Voting from 3 xboosts  