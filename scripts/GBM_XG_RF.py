# -*- coding: utf-8 -*-
"""
Created on Tue Nov  7 09:57:38 2017

@author: m00760171
"""

import numpy as np
import matplotlib.pyplot as plt

from sklearn import ensemble
from sklearn import datasets
from sklearn.utils import shuffle
from sklearn.metrics import mean_squared_error
from sklearn.ensemble import GradientBoostingClassifier
import pandas as pd

# Importing the dataset
dataset_train = pd.read_csv('C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_OFF_OFF_OFF.csv')
X_train = dataset_train.iloc[:, 0:39].values
y_train = dataset_train.iloc[:, 39].values
dataset_test = pd.read_csv('C:/Users/m00760171/Desktop/Templates/trunk/test/normal/data_OFF_OFF_OFF.csv')
X_test = dataset_test.iloc[:, 0:39].values
y_test = dataset_test.iloc[:, 39].values

# Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)


# #############################################################################
# Fit GBM
#clf = GradientBoostingClassifier(n_estimators=50, learning_rate=0.1,
# max_depth=5, random_state=123).fit(X_train, y_train)
clf = GradientBoostingClassifier(n_estimators=50, random_state=123).fit(X_train, y_train)
y_pred_proba = clf.predict_proba(X_test)
y_pred = clf.predict(X_test)

from sklearn.metrics import roc_auc_score, accuracy_score
roc_auc_score(y_test, y_pred_proba[:,1])
accuracy_score(y_test, y_pred)


# #############################################################################
# Plot training deviance
from sklearn.ensemble import RandomForestClassifier
classifier = RandomForestClassifier(n_estimators = 10, criterion = 'entropy', random_state = 0)
classifier.fit(X_train, y_train)
#y_conf_1 = classifier.decision_function(X_test)
y_conf_2 = classifier.predict_proba(X_test)
# Predicting the Test set results
y_pred = classifier.predict(X_test)
# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix, accuracy_score
cm = confusion_matrix(y_test, y_pred)
print('Random Forest accuracy = ' + str(100.0*accuracy_score(y_test, y_pred)))
roc_auc_score(y_test, y_pred_proba[:,1])
