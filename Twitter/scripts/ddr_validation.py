# Imports
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LogisticRegressionCV
import pandas as pd
import numpy as np
from imblearn.over_sampling import RandomOverSampler
from imblearn.over_sampling import SMOTE

# Load tweets and human ratings
data = pd.read_csv('tweet_loadings.csv')
rater1 = pd.read_csv('validation_rater1.csv')
rater2 = pd.read_csv('validation_rater2.csv')
rater3 = pd.read_csv('validation_rater3.csv')
rater4 = pd.read_csv('validation_rater4.csv')

# Create merged dataframes (each rater w/ DDR loadings)
merged_1 = data.merge(ratings_d, how = 'inner', on = 'our_id')
merged_2 = data.merge(ratings_a, how = 'inner', on = 'our_id')
merged_3 = data.merge(ratings_p, how = 'inner', on = 'our_id')
merged_4 = data.merge(ratings_k, how = 'inner', on = 'our_id')

# Replace NaNs w/ 0
merged_1 = merged_d.fillna(0)
merged_2 = merged_a.fillna(0)
merged_3 = merged_p.fillna(0)
merged_4 = merged_k.fillna(0)

# Identify feature and outcome columns for logistic regression ("_base" columns are the DDR loadings)

feature_cols = ['careVirtue_base', 'careVice_base', 'fairnessVirtue_base', 'fairnessVice_base',
               'authorityVirtue_base', 'authorityVice_base', 'loyaltyVirtue_base', 'loyaltyVice_base',
               'sancityVirtue_base', 'sancityVice_base']
outcome_cols = ['HarmVirtue', 'HarmVice', 'FairnessVirtue', 'FairnessVice', 'LoyaltyVirtue', 'LoyaltyVice',
                'AuthorityVirtue', 'AuthorityVice', 'SanctityVirtue', 'SanctityVice']

# Create a list of the rater dataframes to loop through
dataframes = [merged_1, merged_2, merged_3, merged_4]

# Instantiate logistic regression with 10-fold cross validation
logitCV = LogisticRegressionCV(cv=10, scoring = 'f1')
# Instantiate SMOTE re-sampler
smt = SMOTE()
# Initialize a list to populate with mean F1 scores
mean_f1_scores = []

# Note SMOTE re-sampling doesn't work when there are <6 items in category
for merged_data in dataframes:
    X = merged_data[feature_cols]
    for column in outcome_cols:
        try:
            y = merged_data[column]
            X_resampled, y_resampled = smt.fit_sample(X, y)
            logitCV.fit(X_resampled, y_resampled)
            mean_f1_scores.append(logitCV.score(X_resampled, y_resampled))
        except ValueError:
            print('error')
            
print np.mean(mean_f1_scores)