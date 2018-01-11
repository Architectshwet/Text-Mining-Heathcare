### We already reduced our SUMMARY dtm and DATA dtm by removing sparse terms.
rm(list = ls())
##initializing setup
libs = c("data.table","plyr","dplyr","tm","SnowballC","ggplot2","wordcloud")
lapply(libs, require, character.only = TRUE)
rm(libs)

load('First_step.dat')
load('Second_step.dat')
load('Third_step.dat')

#generated dtms
dtm_DATA;dtm_SUMMARY

#dtms with removed sparse terms
sparsed_dtm_DATA;sparsed_dtm_SUMMARY

# We reduced DATA terms from 73656 to 510 by removing sparse terms
# We reduced SUMMARY terms from 9750 to 403 by removing sparse terms
dim(combined_data)
#Now,lets remove correlated terms from the dataset formed by reduced dtms
df <- combined_data
dim(df)
#finding correlations using pearson method.
corr <- data.table(cor(df, use = "complete.obs", method= "pearson")) 
dim(corr)
names(corr)
corr.terms
corr.terms <- NULL
nrow(corr)
ncol(corr)
for(i in 1:(nrow(corr)-1)){
  for(j in (i+1):ncol(corr)){
    if((abs(corr[[i,j]])>0.85) ==T){
      corr.terms = c(corr.terms, names(corr)[i])
      print(paste(colnames(corr)[i],',',colnames(corr)[j])) # print rows and column numbers which are correlated
    }
  }
}
rm(corr,i,j)
corr.terms
corr.terms <- unique(corr.terms) #correlated terms > 85% in combined_data
corr.terms
length(corr.terms) #no of correlated terms with > 85 % correlation
common_unique1 #top 10 most frequent common words in categories.

#We already found the top 100 most frequent common terms in Third_step
unneccesary_words <- c(corr.terms,common_unique1) #terms to be removed, combined together
length(unneccesary_words)
unneccesary_words = unique(unneccesary_words) #finding the unique word
length(unneccesary_words)
del_col = df[,!(names(df) %in% unneccesary_words)] #features to be filtered out
df = as.data.frame(df) #converting to data.frame as data.table takes time
df = df[,del_col] #filtered out the correlated and most common terms
dim(df)

df = data.table(df) #converting back to data.table for faster operations
class(df)


#target variables
categories = data_req$categories
sub_categories = data_req$sub_categories

#previous appoinment is independent variable
previous_appointment = data_req$previous_appointment
names(df)

master_data_cat <- as.data.frame(cbind(df, previous_appointment, sub_categories, categories))
master_data_sub = as.data.frame(cbind(df,previous_appointment,categories,sub_categories))
dim(master_data_cat);dim(master_data_sub)


save(master_data_cat,master_data_sub, file = 'Fourth_step.dat')

