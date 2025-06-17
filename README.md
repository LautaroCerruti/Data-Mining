# Data Mining Assignments

This repository contains a set of practical assignments for a data mining course. The code is written in R and provided mainly as Jupyter notebooks (`*.ipynb`) along with helper R scripts and datasets. Each folder corresponds to a different assignment, and a PDF inside each folder explains the exercise in detail.

## Contents

| Folder | Description |
| ------ | ----------- |
| **TP1** | Introduction to classification on synthetic datasets (diagonals and spirals). Implements decision trees with `rpart`, k-nearest neighbors (`knn`), and support vector machines (`svm`). Includes examples of k-fold cross‑validation for model evaluation. |
| **TP2** | Feature selection and classification on the `ckd_dataset.csv` chronic kidney disease data. Provides R functions for greedy forward/backward selection and recursive feature elimination. Models include SVMs, random forests, and naive Bayes. |
| **TP3** | Clustering analysis using the `gym_members_exercise_tracking.csv` dataset. Demonstrates preprocessing with log transforms, scaling, and principal component analysis (PCA) followed by K‑means and hierarchical clustering (average and complete linkage). Determines the number of clusters using the gap statistic and stability measures. |
| **TP4** | Comparison of several classifiers on a multiclass problem. Implements random forests, SVMs with RBF and polynomial kernels, and gradient boosting with `xgboost`. Parameter tuning is performed with cross‑validation. |
| **TP-Final** | Final project using League of Legends match data (`games_10min_wclass.csv`). Explores variable selection (wrapper methods, Kruskal–Wallis filter, recursive feature elimination), clustering (K‑means, hierarchical), and classification (Random Forest, LDA, SVM, XGBoost). |

Each assignment folder also stores serialized R objects (`*.RData`) with intermediate or final results.

## Usage

The notebooks were developed with the R kernel for Jupyter. To reproduce the exercises, open the corresponding notebook inside each folder and ensure the required R packages are installed. The PDFs provide the theoretical background and additional instructions for each assignment.
