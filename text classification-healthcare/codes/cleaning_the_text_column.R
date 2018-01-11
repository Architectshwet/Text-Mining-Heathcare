### we are going to sample out a portion of dataset and clean the text to form
### a corpus and then tokenize using document term matrix.
rm(list = ls())

getwd()
setwd("C:/Users/Architect_shwet/Documents")
getwd()
##initializing setup
libs <- c("data.table","plyr","dplyr","tm","SnowballC")
lapply(libs, require, character.only = TRUE)
rm(libs)

#loading the prepped dataset from first step.
load('First_step.dat')

#function for cleaning the corpus
clean_corpus <- function(data){
  data_corpus <- Corpus(VectorSource(data)) #forming a corpus
  data_corpus <- tm_map(data_corpus,removePunctuation) #removing punchuations
  data_corpus <- tm_map(data_corpus,removeNumbers) #removing numbers
  data_corpus <- tm_map(data_corpus,tolower) #converting to lowercase
  data_corpus <- tm_map(data_corpus,removeWords,stopwords("English")) #removing english stopwords
  data_corpus <- tm_map(data_corpus,removeWords,stopwords("SMART"))
  data_corpus <- tm_map(data_corpus,stemDocument) #performing stemming
  data_corpus <- tm_map(data_corpus,stripWhitespace) #removing the whitespaces
  
}

#Cleaning the SUMMARY and DATA columns
data_corpus_SUMMARY <- clean_corpus(data_req$SUMMARY)
data_corpus_DATA <- clean_corpus(data_req$DATA)
class(data_corpus_SUMMARY)
#generating Document Term Matrices for SUMMARY and DATA columns
dtm_SUMMARY <- DocumentTermMatrix(data_corpus_SUMMARY,control = list(weighting = weightTf))
dtm_DATA <- DocumentTermMatrix(data_corpus_DATA,control = list(weighting = weightTf))

dtm_SUMMARY;dtm_DATA



#not able to convert Document matrix of SUMMARY and DATA to a matrix 
SUMMARY_data_dtm <- as.matrix(dtm_SUMMARY)
DATA_data_dtm <- as.matrix(dtm_DATA)

#removing sparse terms from the Document Term Matrices of SUMMARY and DATA columns
sparsed_dtm_SUMMARY = removeSparseTerms(dtm_SUMMARY,0.999)
sparsed_dtm_DATA = removeSparseTerms(dtm_DATA,0.99)
colnames(dtm_SUMMARY)
sparsed_dtm_SUMMARY;sparsed_dtm_DATA
#rm(dtm_SUMMARY,dtm_DATA) #not needed further, saves memory
#generating the datasets from the dtms generated
SUMMARY_data = data.table(as.matrix(sparsed_dtm_SUMMARY))
DATA_data = data.table(as.matrix(sparsed_dtm_DATA))
dim(SUMMARY_data);dim(DATA_data)

#rm(sparsed_dtm_SUMMARY,sparsed_dtm_DATA) #not needed further, saves memory

#combing the SUMMARY AND DATA datasets to form single dataset
combined_data = bind_cols(SUMMARY_data,DATA_data)
dim(combined_data)
#rm(data_corpus_SUMMARY,data_corpus_DATA,dtm_SUMMARY,dtm_DATA,SUMMARY_data,DATA_data)


#saving the data needed for third step.
save(combined_data,clean_corpus,dtm_DATA,dtm_SUMMARY
     ,sparsed_dtm_DATA,sparsed_dtm_SUMMARY, data_corpus_SUMMARY,data_corpus_DATA,
     SUMMARY_data, DATA_data, file = 'Second_step.dat')




