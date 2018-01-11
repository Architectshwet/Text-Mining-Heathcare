### we are going to study the distribution of words using the dataset created 
rm(list = ls())
##initializing setup

setwd("C:/Users/Architect_shwet/Documents")
getwd()


libs = c("data.table","plyr","dplyr","tm","SnowballC","ggplot2","wordcloud")
lapply(libs, require, character.only = TRUE)
rm(libs)
##Best way to explore a dataset is to analyse the wordclouds.

load('Second_step.dat')
load('First_step.dat')

#word cloud for corpus SUMMARY data
png("wordcloudforcorpussummary.png", ,width = 480,height = 480)
wordcloud(data_corpus_SUMMARY, scale = c(4,0.75), random.order = F, max.words = 150, colors=brewer.pal(8, "Dark2"))
dev.off()

#word cloud for corpus DATA column
png("wordcloudforcorpusdata.png", ,width = 480,height = 480)
wordcloud(data_corpus_DATA, scale = c(4,0.75), random.order = F, max.words = 150, colors=brewer.pal(8, "Dark2"))
dev.off()



#combined data
head(combined_data)
#forming a dataset with frquencies in descending order
word_freq <- sort(colSums(combined_data), decreasing = T)
colSums(combined_data)
word_freq <- data.table(Terms = names(word_freq), frequency = word_freq)
word_freq$Terms <- removeNumbers(word_freq$Terms)
head(word_freq,10)

# creating word cloud for top 200 words in frequency
png("WholeData_Wordcloud.png",width = 480,height = 480)
wordcloud(word_freq$Terms, word_freq$frequency,max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()
#most frequent words in the datasetfc
most_freq <- word_freq[frequency>50000]
class(most_freq)

# Bar graph for words that are more frequent appearing more than 50000 times
png("Top_5_most_frequent_terms.png")
ggplot(most_freq, aes(Terms, frequency)) + geom_bar(stat = "identity", fill = "pink") + scale_x_discrete("Terms")+ scale_y_continuous("frequency", breaks = seq(0,140000, by = 10000))+ theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ labs(title = "Bar Chart") 
dev.off()
#subsetting the combined data category wise and cleaning the corresponding corpuses
categories <- data_req$categories
combined_data_cat <- cbind(combined_data,categories)

#Subsetting the datsets by category
head(combined_data_cat,2)
appoinments <- combined_data_cat[categories == "APPOINTMENTS"]
ask_a_doctor <- combined_data_cat[categories == "ASK_A_DOCTOR"]
miscellaneous <- combined_data_cat[categories == "MISCELLANEOUS"]
lab <- combined_data_cat[categories == "LAB"]
prescription <- combined_data_cat[categories == "PRESCRIPTION"]

#Wordclouds of terms in combined_data, category wise 

#png("Appointments.png")
wordcloud(names(appoinments),numcolwise(sum)(appoinments),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Ask_a_doctor.png")
wordcloud(names(ask_a_doctor),numcolwise(sum)(ask_a_doctor),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Miscellaneous.png")
wordcloud(names(miscellaneous),numcolwise(sum)(miscellaneous),max.words = 200, scale =c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Lap.png")
wordcloud(names(lab),numcolwise(sum)(lab),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Prescription.png")
wordcloud(names(prescription),numcolwise(sum)(prescription),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()
rm(appoinments,ask_a_doctor,miscellaneous,lab,prescription)


#SUMMARY data

word_freq_summary <- sort(colSums(SUMMARY_data), decreasing = T)
word_freq_summary <- data.table(Terms = names(word_freq_summary), frequency = word_freq_summary)
head(word_freq_summary)

# creating word cloud for top 200 words in frequency
png("WholeData_Wordcloud_summary.png",width = 480,height = 480)
wordcloud(word_freq_summary$Terms, word_freq_summary$frequency,max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()
most_freq_summary  <- word_freq_summary[frequency > 3850]
most_freq_summary 
png("Top_5_most_frequent_terms_summary.png")
ggplot(most_freq_summary , aes(Terms, frequency))+
  geom_bar(stat = 'identity', colour = "black", fill = 'pink')+
  labs(title= 'High frequent Terms')
dev.off()


#subsetting the combined data category wise and cleaning the corresponding corpuses
categories <- data_req$categories
SUMMARY_data_cat <- cbind(SUMMARY_data,categories)

appointments_summary <- SUMMARY_data_cat[categories == "APPOINTMENTS"]
ask_a_doctor_summary <- SUMMARY_data_cat[categories == "ASK_A_DOCTOR"]
miscellaneous_summary <- SUMMARY_data_cat[categories == "MISCELLANEOUS"]
lab_summary <- SUMMARY_data_cat[categories == "LAB"]
prescription_summary <- SUMMARY_data_cat[categories == "PRESCRIPTION"]

#Wordclouds of terms in summary_data, category wise 

#png("Appointments_summary.png")
wordcloud(names(appointments_summary),numcolwise(sum)(appointments_summary),max.words = 200, scale = c(4,0.75),
                   random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Ask_a_doctor_summary.png")
wordcloud(names(ask_a_doctor_summary),numcolwise(sum)(ask_a_doctor_summary),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Miscellaneous_summary.png")
wordcloud(names(miscellaneous_summary),numcolwise(sum)(miscellaneous_summary),max.words = 200, scale =c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Lap_summary.png")
wordcloud(names(lab_summary),numcolwise(sum)(lab_summary),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Prescription_summary.png")
wordcloud(names(prescription_summary),numcolwise(sum)(prescription_summary),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

rm(appointments_summary,ask_a_doctor_summary,miscellaneous_summary,lab_summary,prescription_summary)


#DATA data

word_freq_data <- sort(colSums(DATA_data), decreasing = T)
word_freq_data <- data.table(Terms = names(word_freq_data), frequency = word_freq_data)
head(word_freq_data)

# creating word cloud for top 200 words in frequency
png("WholeData_Wordcloud_data.png",width = 480,height = 480)
wordcloud(word_freq_data$Terms, word_freq_data$frequency,max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()
most_freq_data  <- word_freq_data[frequency > 50000]
most_freq_data
png("Top_5_most_frequent_terms_data.png")
ggplot(most_freq_data , aes(Terms, frequency))+
  geom_bar(stat = 'identity', colour = "black", fill = 'pink')+
  labs(title= 'High frequent Terms')
dev.off()


png("Top_5_most_frequent_terms_data.png")
ggplot(most_freq_data, aes(Terms, frequency)) + geom_bar(stat = "identity", fill = "pink") + scale_x_discrete("Terms")+ scale_y_continuous("frequency", breaks = seq(0,140000, by = 10000))+ theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ labs(title = "Bar Chart") 
dev.off()






#subsetting the combined data category wise and cleaning the corresponding corpuses
categories <- data_req$categories
DATA_data_cat <- cbind(DATA_data,categories)

appointments_data <- DATA_data_cat[categories == "APPOINTMENTS"]
ask_a_doctor_data <- DATA_data_cat[categories == "ASK_A_DOCTOR"]
miscellaneous_data <- DATA_data_cat[categories == "MISCELLANEOUS"]
lab_data <- DATA_data_cat[categories == "LAB"]
prescription_data <- DATA_data_cat[categories == "PRESCRIPTION"]

#Wordclouds of terms in combined_data, category wise 

#png("Appointments_data.png")
wordcloud(names(appointments_data),numcolwise(sum)(appointments_data),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Ask_a_doctor_data.png")
wordcloud(names(ask_a_doctor_data),numcolwise(sum)(ask_a_doctor_data),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Miscellaneous_data.png")
wordcloud(names(miscellaneous_data),numcolwise(sum)(miscellaneous_data),max.words = 200, scale =c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Lap_data.png")
wordcloud(names(lab_data),numcolwise(sum)(lab_data),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

#png("Prescription_data.png")
wordcloud(names(prescription_data),numcolwise(sum)(prescription_summary),max.words = 200, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
#dev.off()

rm(appointments_data,ask_a_doctor_data,miscellaneous_data,lab_data,prescription_data)



## Exploring the dataset category wise.

#subsetting the dataset category wise and cleaning the corresponding corpuses

#Subsetting the datsets by category

app_df <- data_req[categories == "APPOINTMENTS"]
ask_df <- data_req[categories == "ASK_A_DOCTOR"]
mis_df <- data_req[categories == "MISCELLANEOUS"]
lab_df <- data_req[categories == "LAB"]
pre_df <- data_req[categories == "PRESCRIPTION"]

#Cleaning the SUMMARY and DATA columns for each category dataset

appoinments_corpus_SUMMARY <- clean_corpus(app_df$SUMMARY)
appoinments_corpus_DATA <- clean_corpus(app_df$DATA)
ask_a_doctor_corpus_SUMMARY <- clean_corpus(ask_df$SUMMARY)
ask_a_doctor_corpus_DATA <- clean_corpus(ask_df$DATA)
miscellaneous_corpus_SUMMARY <- clean_corpus(mis_df$SUMMARY)
miscellaneous_corpus_DATA <- clean_corpus(mis_df$DATA)
lab_corpus_SUMMARY <- clean_corpus(lab_df$SUMMARY)
lab_corpus_DATA <- clean_corpus(lab_df$DATA)
prescription_corpus_SUMMARY <- clean_corpus(pre_df$SUMMARY)
prescription_corpus_DATA <- clean_corpus(pre_df$DATA)

rm(app_df,ask_df,mis_df,lab_df,pre_df)

#generating respective Document Term Matrices for each category

appoinments_dtm_SUMMARY <- DocumentTermMatrix(appoinments_corpus_SUMMARY)
appoinments_dtm_SUMMARY <- removeSparseTerms(appoinments_dtm_SUMMARY,0.999)
appoinments_dtm_SUMMARY

appoinments_dtm_DATA <- DocumentTermMatrix(appoinments_corpus_DATA)
appoinments_dtm_DATA <- removeSparseTerms(appoinments_dtm_DATA,0.985)
appoinments_dtm_DATA

ask_a_doctor_dtm_SUMMARY <- DocumentTermMatrix(ask_a_doctor_corpus_SUMMARY)
ask_a_doctor_dtm_SUMMARY <- removeSparseTerms(ask_a_doctor_dtm_SUMMARY,0.999)
ask_a_doctor_dtm_SUMMARY

ask_a_doctor_dtm_DATA <- DocumentTermMatrix(ask_a_doctor_corpus_DATA)
ask_a_doctor_dtm_DATA <- removeSparseTerms(ask_a_doctor_dtm_DATA,0.985)
ask_a_doctor_dtm_DATA

miscellaneous_dtm_SUMMARY <- DocumentTermMatrix(miscellaneous_corpus_SUMMARY)
miscellaneous_dtm_SUMMARY <- removeSparseTerms(miscellaneous_dtm_SUMMARY,0.999)
miscellaneous_dtm_SUMMARY

miscellaneous_dtm_DATA <- DocumentTermMatrix(miscellaneous_corpus_DATA)
miscellaneous_dtm_DATA <- removeSparseTerms(miscellaneous_dtm_DATA,0.985)
miscellaneous_dtm_DATA

lab_dtm_SUMMARY <- DocumentTermMatrix(lab_corpus_SUMMARY)
lab_dtm_SUMMARY <- removeSparseTerms(lab_dtm_SUMMARY,0.999)
lab_dtm_SUMMARY

lab_dtm_DATA <- DocumentTermMatrix(lab_corpus_DATA)
lab_dtm_DATA <- removeSparseTerms(lab_dtm_DATA,0.985)
lab_dtm_DATA

prescription_dtm_SUMMARY <- DocumentTermMatrix(prescription_corpus_SUMMARY)
prescription_dtm_SUMMARY <- removeSparseTerms(prescription_dtm_SUMMARY,0.999)
prescription_dtm_SUMMARY

prescription_dtm_DATA <- DocumentTermMatrix(prescription_corpus_DATA)
prescription_dtm_DATA <- removeSparseTerms(prescription_dtm_DATA,0.985)
prescription_dtm_DATA

#Forming datatables with terms and frequencies category wise

app_freq_SUMMARY <- sort(colSums(data.table(as.matrix(appoinments_dtm_SUMMARY))), decreasing = T)
app_freq_SUMMARY <- data.table(Terms = names(app_freq_SUMMARY), frequency = app_freq_SUMMARY)
rm(appoinments_corpus_SUMMARY,appoinments_dtm_SUMMARY)
head(app_freq_SUMMARY)

ask_freq_SUMMARY <- sort(colSums(data.table(as.matrix(ask_a_doctor_dtm_SUMMARY))), decreasing = T)
ask_freq_SUMMARY <- data.table(Terms = names(ask_freq_SUMMARY), frequency = ask_freq_SUMMARY)
rm(ask_a_doctor_corpus_SUMMARY,ask_a_doctor_dtm_SUMMARY)
head(ask_freq_SUMMARY)

mis_freq_SUMMARY <- sort(colSums(data.table(as.matrix(miscellaneous_dtm_SUMMARY))), decreasing = T)
mis_freq_SUMMARY <- data.table(Terms = names(mis_freq_SUMMARY), frequency = mis_freq_SUMMARY)
rm(miscellaneous_corpus_SUMMARY,miscellaneous_dtm_SUMMARY)
head(mis_freq_SUMMARY)

lab_freq_SUMMARY <- sort(colSums(data.table(as.matrix(lab_dtm_SUMMARY))), decreasing = T)
lab_freq_SUMMARY <- data.table(Terms = names(lab_freq_SUMMARY), frequency = lab_freq_SUMMARY)
rm(lab_corpus_SUMMARY,lab_dtm_SUMMARY)
head(lab_freq_SUMMARY)

pre_freq_SUMMARY <- sort(colSums(data.table(as.matrix(prescription_dtm_SUMMARY))), decreasing = T)
pre_freq_SUMMARY <- data.table(Terms = names(pre_freq_SUMMARY), frequency = pre_freq_SUMMARY)
rm(prescription_corpus_SUMMARY,prescription_dtm_SUMMARY)
head(pre_freq_SUMMARY)

app_freq_DATA <- sort(colSums(data.table(as.matrix(appoinments_dtm_DATA))), decreasing = T)
app_freq_DATA <- data.table(Terms = names(app_freq_DATA), frequency = app_freq_DATA)
rm(appoinments_corpus_DATA,appoinments_dtm_DATA)
head(app_freq_SUMMARY)

ask_freq_DATA <- sort(colSums(data.table(as.matrix(ask_a_doctor_dtm_DATA))), decreasing = T)
ask_freq_DATA <- data.table(Terms = names(ask_freq_DATA), frequency = ask_freq_DATA)
rm(ask_a_doctor_corpus_DATA,ask_a_doctor_dtm_DATA)
head(ask_freq_DATA)

mis_freq_DATA <- sort(colSums(data.table(as.matrix(miscellaneous_dtm_DATA))), decreasing = T)
mis_freq_DATA <- data.table(Terms = names(mis_freq_DATA), frequency = mis_freq_DATA)
rm(miscellaneous_corpus_DATA,miscellaneous_dtm_DATA)
head(mis_freq_DATA)

lab_freq_DATA <- sort(colSums(data.table(as.matrix(lab_dtm_DATA))), decreasing = T)
lab_freq_DATA <- data.table(Terms = names(lab_freq_DATA), frequency = lab_freq_DATA)
rm(lab_corpus_DATA,lab_dtm_DATA)
head(lab_freq_DATA)

pre_freq_DATA <- sort(colSums(data.table(as.matrix(prescription_dtm_DATA))), decreasing = T)
pre_freq_DATA <- data.table(Terms = names(pre_freq_DATA), frequency = pre_freq_DATA)
rm(prescription_corpus_DATA,prescription_dtm_DATA)
head(pre_freq_DATA)

#Combined data for category wise
combined_app_data <- rbind(app_freq_DATA,app_freq_SUMMARY)
head(combined_app_data)
combined_ask_data <- rbind(ask_freq_DATA,ask_freq_SUMMARY)
head(combined_ask_data)
combined_mis_data <- rbind(mis_freq_DATA,mis_freq_SUMMARY)
head(combined_mis_data)
combined_lab_data <- rbind(lab_freq_DATA,lab_freq_SUMMARY)
head(combined_lab_data)
combined_pre_data <- rbind(pre_freq_DATA,pre_freq_SUMMARY)
head(combined_pre_data)


#Comb_data = rbind(combined_app_data,combined_ask_data,combined_lab_data,combined_mis_data,combined_pre_data)


#wordclouds for individual categories of the combined data

# creating word cloud for top 300 words in frequency

png("individual_appointment.png")
wordcloud(combined_app_data$Terms, combined_app_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_ask_a_doctor.png")
wordcloud(combined_ask_data$Terms, combined_ask_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_miscellanious.png")
wordcloud(combined_mis_data$Terms, combined_mis_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_lab.png")
wordcloud(combined_lab_data$Terms, combined_lab_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2")) 
dev.off()

png("individual_prescription.png")
wordcloud(combined_pre_data$Terms, combined_pre_data$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()


#wordclouds for individual categories of the summary column
png("individual_appointment_summary.png")
wordcloud(app_freq_SUMMARY$Terms, app_freq_SUMMARY$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_ask_a_doctor_summary.png")
wordcloud(ask_freq_SUMMARY$Terms, ask_freq_SUMMARY$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_miscellanious_summary.png")
wordcloud(mis_freq_SUMMARY$Terms, mis_freq_SUMMARY$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_lab_summary.png")
wordcloud(lab_freq_SUMMARY$Terms, lab_freq_SUMMARY$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2")) 
dev.off()

png("individual_prescription_summary.png")
wordcloud(pre_freq_SUMMARY$Terms, pre_freq_SUMMARY$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()



##wordclouds for individual categories of the data column
png("individual_appointment_data.png")
wordcloud(app_freq_DATA$Terms, app_freq_DATA$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_ask_a_doctor_data.png")
wordcloud(ask_freq_DATA$Terms, ask_freq_DATA$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_miscellanious_data.png")
wordcloud(mis_freq_DATA$Terms, mis_freq_DATA$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()

png("individual_lab_data.png")
wordcloud(lab_freq_DATA$Terms, lab_freq_DATA$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2")) 
dev.off()

png("individual_prescription_data.png")
wordcloud(pre_freq_DATA$Terms, pre_freq_DATA$frequency,max.words = 300, scale = c(4,0.75),
          random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()


#Finding the common terms present in any two categories from SUMMARY column
common_SUMMARY = app_freq_SUMMARY[app_freq_SUMMARY$Terms %in% ask_freq_SUMMARY$Terms,] %>%
  rbind(app_freq_SUMMARY[app_freq_SUMMARY$Terms %in%  mis_freq_SUMMARY$Terms,]) %>%
  rbind(app_freq_SUMMARY[app_freq_SUMMARY$Terms %in% lab_freq_SUMMARY$Terms,]) %>%
  rbind(app_freq_SUMMARY[app_freq_SUMMARY$Terms %in% pre_freq_SUMMARY$Terms,]) %>%
  rbind(ask_freq_SUMMARY[ask_freq_SUMMARY$Terms %in% mis_freq_SUMMARY$Terms,]) %>%
  rbind(ask_freq_SUMMARY[ask_freq_SUMMARY$Terms %in% lab_freq_SUMMARY$Terms,]) %>%
  rbind(ask_freq_SUMMARY[ask_freq_SUMMARY$Terms %in% pre_freq_SUMMARY$Terms,]) %>%
  rbind(mis_freq_SUMMARY[mis_freq_SUMMARY$Terms %in% lab_freq_SUMMARY$Terms,]) %>%
  rbind(mis_freq_SUMMARY[mis_freq_SUMMARY$Terms %in% pre_freq_SUMMARY$Terms,]) %>%
  rbind(lab_freq_SUMMARY[lab_freq_SUMMARY$Terms %in% pre_freq_SUMMARY$Terms,])

head(common_SUMMARY)


#Finding the common terms present in any two categories from DATA column
common_DATA = app_freq_DATA[app_freq_DATA$Terms %in% ask_freq_DATA$Terms,] %>%
  rbind(app_freq_DATA[app_freq_DATA$Terms %in% mis_freq_DATA$Terms,]) %>%
  rbind(app_freq_DATA[app_freq_DATA$Terms %in% lab_freq_DATA$Terms,]) %>%
  rbind(app_freq_DATA[app_freq_DATA$Terms %in% pre_freq_DATA$Terms,]) %>%
  rbind(ask_freq_DATA[ask_freq_DATA$Terms %in% mis_freq_DATA$Terms,]) %>%
  rbind(ask_freq_DATA[ask_freq_DATA$Terms %in% lab_freq_DATA$Terms,]) %>%
  rbind(ask_freq_DATA[ask_freq_DATA$Terms %in% pre_freq_DATA$Terms,]) %>%
  rbind(mis_freq_DATA[mis_freq_DATA$Terms %in% lab_freq_DATA$Terms,]) %>%
  rbind(mis_freq_DATA[mis_freq_DATA$Terms %in% pre_freq_DATA$Terms,]) %>%
  rbind(lab_freq_DATA[lab_freq_DATA$Terms %in% pre_freq_DATA$Terms,])

head(common_DATA)

common = rbind(common_DATA,common_SUMMARY) #combining both the common terms
common = common[order(common$frequency,decreasing = TRUE)]
common
png("wordcloud_common_terms_between_categories.png")
wordcloud(unique(common$Terms), common$frequency,max.words = 100, scale = c(4,0.75),
random.order = F, colors=brewer.pal(8, "Dark2"))
dev.off()
common = common[1:15] #extractin only top 15 most frequent common terms
common
library(ggplot2)
png("Common_terms_between_categories.png")
ggplot(common, aes(Terms, frequency))+
  geom_bar(stat = 'identity', colour = "black", fill = 'pink')+
  labs(title= 'High frequent Common Terms between categories')
dev.off()

common_unique = unique(common$Terms)
length(common_unique)
common_unique
common_unique1 <- common_unique[1:15]
library(ggplot2)

png("stacked_bar_chart.png")
ggplot(data_req, aes(categories, fill = sub_categories)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "categories", y = "Count of Outlets")
dev.off()


png("stacked_bar_chart2.png")
ggplot(data_req, aes(categories, fill = previous_appointment)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "categories", y = "Count of Outlets")
dev.off()
png("stacked_bar_chart3.png")
ggplot(data_req, aes(sub_categories, fill = previous_appointment)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "categories", y = "Count of Outlets") +  coord_flip()
dev.off()
png("heat_map.png")
ggplot(data_req, aes(categories, sub_categories))+
  geom_raster(aes(fill = previous_appointment))+
  labs(title ="Heat Map", x = "categories", y = "sub_categories") + theme(legend.position="bottom",axis.text.x = element_text(angle=90))
dev.off()

png("Barplot_category.png")
ggplot(data_req, aes(x= categories)) + geom_bar(colour= 'black', fill= 'pink') + labs(title = "Number of Categories in each type", x = "Type of categories", y = "Number of categories") + geom_text(stat='count', aes(label=..count..), vjust=-0.25)
dev.off()

png("Barplot_sub_category.png")
ggplot(data_req, aes(x= sub_categories)) + geom_bar(colour= 'black', fill= 'pink') + labs(title = "Number of sub Categories in each type", x = "Type of sub categories", y = "Number of sub categories") + geom_text(stat='count', aes(label=..count..), vjust=0.05) +  coord_flip()
dev.off()


#These 100 most frequent words between categories are filtered out in feature engineering

save(common_unique1,file = "Third_step.dat")


























