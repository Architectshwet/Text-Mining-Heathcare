# Text-Mining-Heathcare

## Aim
To build a classification model based on the Text in the Summary and Description of the
call to classify the ticket to appropriate Category (out of 5 Categories) and Subcategories
(Out of 20 Sub Categories).

## Procedures

1) Used data.table, dplyr, plyr libraries to prepare the data for analysis and qdap and tm libraries to clean the text column by forming corpus followed by applying dtm and further sparsity.

2) Used wordcloud and ggplot2 libraries to create visualizations between text column and categories & subcategories column.

3) Used the concept of removing correlated terms with common terms as part of feature engineering.

4) Designed a H2o based random forest, gradient boost, deeplearning and xgboost and finally stacked them all to form the final model which gave an accuracy of 99% on classifying categories and 65% accuracy on classifying sub categories.

