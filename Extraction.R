rm(list = ls())

install.packages("readtext")
install.packages("stringr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")
install.packages("splitstackshape")
install.packages("readxl")
install.packages("psych")
install.packages("textclean")
install.packages("reshape")
install.packages("reshape2")
install.packages("knit2pdf")
install.packages("rmarkdown")

library(readtext)
library(stringr)
library(tidyverse)
library(dplyr)
library(plyr)
library(splitstackshape)
library(readxl)
library(psych)
library(textclean)
library(reshape)
library(reshape2)
library(knit2pdf)
library(rmarkdown)

#Import LoIs as dataframes
loi_raw_1 <- readtext::readtext("Batch1")
loi_raw_2 <- readtext::readtext("Batch2")
loi_raw_3 <- readtext::readtext("Batch3")
loi_raw_4 <- readtext::readtext("Batch4")
loi_raw_5 <- readtext::readtext("Batch5")
loi_raw_6 <- readtext::readtext("Batch6")


#Add variable to LoIs
loi_raw_1$batch_number <- 1
loi_raw_2$batch_number <- 2
loi_raw_3$batch_number <- 3
loi_raw_4$batch_number <- 4
loi_raw_5$batch_number <- 5
loi_raw_6$batch_number <- 6

#Add serial number to LoIs
loi_raw_1$serial_number<- paste("loi_", 1:nrow(loi_raw_1), sep = "")
loi_raw_2$serial_number<- paste("loi_", 1:nrow(loi_raw_2), sep = "")
loi_raw_3$serial_number<- paste("loi_", 1:nrow(loi_raw_3), sep = "")
loi_raw_4$serial_number<- paste("loi_", 1:nrow(loi_raw_4), sep = "")
loi_raw_5$serial_number<- paste("loi_", 1:nrow(loi_raw_5), sep = "")
loi_raw_6$serial_number<- paste("loi_", 1:nrow(loi_raw_6), sep = "")

#Combine all batches
loi_raw  <- rbind(loi_raw_1,loi_raw_2,loi_raw_3,loi_raw_4,loi_raw_5,loi_raw_6)

#Remove LoIs
rm(loi_raw_1,loi_raw_2,loi_raw_3,loi_raw_4,loi_raw_5,loi_raw_6)

##################################################################################################
##################################################################################################

#Clean text LoIs
loi_raw$text <- replace_non_ascii(loi_raw$text)
loi_raw$text <- gsub("References.+","",loi_raw$text)            #Remove everything after references
loi_raw$text <- gsub("REFERENCES.+","",loi_raw$text)                #Remove everything after references
loi_raw$text <- gsub('[[:punct:]]+',' ',loi_raw$text)              #Remove punctuation
loi_raw$text <- gsub("http[[:alnum:][:punct:]]*", "", loi_raw$text) #Remove URL / Hyperlink
loi_raw$text <- gsub("http[^[:space:]]*", "", loi_raw$text)         #Remove URL / Hyperlink
loi_raw$text <- gsub('(f|ht)tp\\S+\\s*',"", loi_raw$text)           #Remove URL / Hyperlink
loi_raw$text <- tolower(loi_raw$text)                               #Lower the letter

##########################################################################################################
#######################################EXTRACT DISEASES###################################################
##########################################################################################################

#Create list of diseases WHO
diseases_list <- "\\bhiv\\b|\\btb\\b|tuberc[uo]los[ei]s*|malaria|influenzae|influenz[ae]|dengue|rab[gi]es|trachoma|\\byaws\\b|
                  lepr[eo]sy|chagas|leishmani[oa]ses|taeniasis|neurocysticercos[ei]s|dracuncul[oi]as[ei]s|
                  echinococcos[ei]s|onchocerc[oi]as[ei]s|schistosomias[ei]s|mycetoma|buruli\\s+ulcer|
                  human\\s+immunodeficiency\\s+viruses|trypanosomiasis|sleeping\\s+sickness|guinea\\s+worm|
                  foodborne\\s+trematodiases|lymphatic\\s+filariasis|river\\s+blindness|
                  soil\\s+transmitted\\s+helminthiases" 

#Extract list of diseases WHO
loi_raw$diseases <- str_extract_all(loi_raw$text, diseases_list)

#Fill empty cells with 'other'
loi_raw$diseases <- str_replace_all(loi_raw$diseases , "character\\(0\\)", "other")

#Create long dataset with one disease per row
loi_diseases <- cSplit(loi_raw, "diseases", ",", "long")
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\btb\\b", "tuberculosis")
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\binfluenzae\\b", "influenza")
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\s+", " ")
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "c\\(", "")
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\)", "")
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\"", "")

#Collapse long dataset into LoIs
loi_count_1 <- loi_diseases
loi_count_1$tally <- 1
loi_count_1 <- aggregate(tally~doc_id+serial_number+batch_number+diseases,loi_count_1,sum)
sum(loi_count_1$tally) #to check if sum of diseases are equal to number of rows in long dataset

#Add variables for each diseases with counts
loi_column_1 <- cast(loi_count_1,doc_id+serial_number+batch_number~diseases)

#Delete mentions of other diseases and create dummy variables
loi_column_1$average_disease <- rowMeans(loi_column_1[,4:24],na.rm=T)
bool_disease <- ifelse(loi_column_1[,4:24] >= loi_column_1$average_disease,1,NA)
loi_column_1[,4:24] <- bool_disease

##################################################################################################
#######################################EXTRACT COUNTRIES##########################################
##################################################################################################

#Import file with Country, WHO Region and WB Income Group
country_wb_who <- read.csv("Country List.csv")

#Create country list
country_list <- country_wb_who[,2]
country_list <- paste(country_list,collapse = "|")
country_list <- str_replace_all(country_list , "\\\\s+", " ")
country_list <- str_replace_all(country_list , "gambie", "gambia") 

#Extract country from LoIs
loi_country <- loi_raw
loi_country$country <- str_extract_all(loi_raw$text, country_list)
loi_country$diseases <- NULL

#Create long dataset with one country per row
loi_country <- cSplit(loi_country, "country", ",", "long") 
loi_country$country <- str_replace_all(loi_country$country , "c\\(", "")          #Remove c(
loi_country$country <- str_replace_all(loi_country$country , "\\)", "")           #Remove )
loi_country$country <- str_replace_all(loi_country$country , "\"", "")            #Remove "

#Collapse long dataset into LoIs
loi_count_2 <- loi_country
loi_count_2$tally <- 1
loi_count_2 <- aggregate(tally~doc_id+serial_number+batch_number+country,loi_count_2,sum)
sum(loi_count_2$tally) #to check if sum of countries are equal to number of rows in long dataset

#Add variables for each country with counts
loi_column_2 <- cast(loi_count_2,doc_id+serial_number+batch_number~country)
names(loi_column_2)[names(loi_column_2) == "character(0"] <- "unidentified"

#Delete mentions of other countries and create dummy variables
loi_column_2$average_country <- rowMeans(loi_column_2[,4:50],na.rm=T)
bool_country <- ifelse(loi_column_2[,4:50] >= loi_column_2$average_country,1,NA)
bool_country <- loi_column_2[,4:50]*bool_country[,1:47]
loi_column_2[,4:50] <- bool_country

##################################################################################################
#######################################WHO REGIONS AND WB INCOME GROUPS###########################
##################################################################################################

#Merge country with WHO regions and WB income group in long dataset
loi_count_2$country <- str_replace_all(loi_count_2$country,"character\\(0","unknown")
loi_count_3  <- merge(loi_count_2,country_wb_who,all.x = TRUE,by="country")
loi_count_3$tally <- 1

##Collapse long dataset into LoIs - WHO_Region
loi_count_3_1 <- aggregate(tally~doc_id+serial_number+batch_number+who_region,loi_count_3,sum,na.action = na.pass)

##Collapse long dataset into LoIs - WB Income Group
loi_count_3_2 <- aggregate(tally~doc_id+serial_number+batch_number+wb_income_group,loi_count_3,sum,na.action = na.pass)

##Add variables for each WHO_Region with counts
loi_column_3 <- cast(loi_count_3_1,doc_id+serial_number+batch_number~who_region,na.action = na.pass)

##Add variables for each WB Income Group with counts
loi_column_4 <- cast(loi_count_3_2,doc_id+serial_number+batch_number~wb_income_group,na.action = na.pass)

#########################################################################################################

#Merge all sub-datasets to form one full dataset for diseases, countries, regions and income groups
loi_all <- merge(merge(loi_column_1,loi_column_2),merge(loi_column_3,loi_column_4))

rm(loi_column_1,loi_column_2, loi_column_3,loi_column_4)
#rm(loi_count_1,loi_count_2,loi_count_3,loi_count_3_1,loi_count_3_2)

##########################################################################################################
#############################################VISUALISATIONS COUNTRIES AND DISEASES########################
##########################################################################################################
#Countries and diseases - Create a dataset for visualization
viz_disease <- prop.table(table(loi_count_1$diseases))*100
viz <- as.data.frame(viz_disease)
viz$type <- "disease"

viz_country <- as.data.frame(prop.table(table(loi_count_2$country))*100)
viz <- rbind.fill(viz,viz_country)
viz$type[is.na(viz$type)] <- 0
viz$type <- str_replace_all(viz$type,"0","country")

viz_region <- as.data.frame(prop.table(table(loi_count_3$who_region))*100)
viz <- rbind.fill(viz,viz_region)
viz$type[is.na(viz$type)] <- 0
viz$type <- str_replace_all(viz$type,"0","WHO region")

viz_income <- as.data.frame(prop.table(table(loi_count_3$wb_income_group))*100)
viz <- rbind.fill(viz,viz_income)
viz$type[is.na(viz$type)] <- 0
viz$type <- str_replace_all(viz$type,"0","WB income group")

viz$Var1 <- str_replace_all(viz$Var1,"character\\(0","unidentified")

names(viz)[names(viz) == "Freq"] <- "Percentage"
names(viz)[names(viz) == "Var1"] <- "Group"
viz <- subset(viz, viz$Group!="unidentified")

g1 <- ggplot(subset(viz, type=="disease"), aes(x=reorder(Group, Percentage), y=Percentage)) +  xlab("Disease") + ylab("Percentage") + geom_bar(stat="identity", fill="indianred2") + theme_minimal() + coord_flip()
g2 <- ggplot(subset(viz, type=="country"), aes(x=reorder(Group, Percentage), y=Percentage)) +  xlab("Country") + ylab("Percentage") + geom_bar(stat="identity", fill="turquoise4") + theme_minimal() + expand_limits(y=c(0.0, 20.0), x=c(0, 0)) + coord_flip()
g3 <- ggplot(subset(viz, type=="WHO region"), aes(x=reorder(Group, Percentage), y=Percentage)) +  xlab("WHO Region") + ylab("Percentage")+ geom_bar(stat="identity", fill="tan2") + theme_minimal() + coord_flip()
g4 <- ggplot(subset(viz, type=="WB income group"), aes(x=reorder(Group, Percentage), y=Percentage)) +  xlab("WB Income Group") + ylab("Percentage")+ geom_bar(stat="identity", fill="darkseagreen") + theme_minimal() + coord_flip()

#########################################################################################################
##########################################EXTRACT RESEARCH METHODS#######################################
#########################################################################################################

#Upload lexicon for Research Methods 
method <- read.csv("Research Methods.csv")
#names(method)[names(method) == "ï..General_Terms"] <- "General_Terms"

#########################################################################################################
#General Terms

#Creating a list for - General Terms
general_terms <- tolower(method[,1])
general_terms <- as.data.frame(general_terms)
general_terms <- general_terms[rowSums(general_terms=="")!=ncol(general_terms), ]
general_terms <- paste(general_terms)
general_terms <- str_replace_all(general_terms , " ", "\\\\s+") 

#Extracting General Terms from LoIs
loi_method <- loi_raw
loi_method$general_terms <- str_extract_all(loi_raw$text, general_terms)
loi_method$diseases <- NULL

#Create long dataset with one 'General Term' per row
loi_method <- cSplit(loi_method, "general_terms", ",", "long")
loi_method$general_terms <- str_replace_all(loi_method$general_terms , "c\\(", "")          #Remove c(
loi_method$general_terms <- str_replace_all(loi_method$general_terms , "\\)", "")           #Remove )
loi_method$general_terms <- str_replace_all(loi_method$general_terms , "\"", "")            #Remove "

#Collapse long dataset into LoIs
loi_count_method_1 <- loi_method
loi_count_method_1$tally <- 1
loi_count_method_1 <- aggregate(tally~doc_id+serial_number+batch_number+general_terms,loi_count_method_1,sum)
sum(loi_count_method_1$tally) #to check if sum of general terms are equal to number of rows in long dataset

#Add variables for each 'General Terms' with counts
loi_column_method_1 <- cast(loi_count_method_1,doc_id+serial_number+batch_number~general_terms)
names(loi_column_method_1)[names(loi_column_method_1) == "character(0"] <- "unidentified"

#########################################################################################################
#Research Designs

#Creating a list for - Research Designs
research_designs <- tolower(method[,2])
research_designs <- as.data.frame(research_designs)
research_designs <- research_designs[rowSums(research_designs=="")!=ncol(research_designs), ]
research_designs <- paste(research_designs)
research_designs <- str_replace_all(research_designs , " ", "\\\\s+") 

#Extracting Research Designs from LoIs
loi_method_2 <- loi_raw
loi_method_2$research_designs <- str_extract_all(loi_raw$text, research_designs)
loi_method_2$diseases <- NULL

#Create long dataset with one 'Research Design' per row
loi_method_2 <- cSplit(loi_method_2, "research_designs", ",", "long") 
loi_method_2$research_designs <- str_replace_all(loi_method_2$research_designs , "c\\(", "")          #Remove c(
loi_method_2$research_designs <- str_replace_all(loi_method_2$research_designs , "\\)", "")           #Remove )
loi_method_2$research_designs <- str_replace_all(loi_method_2$research_designs , "\"", "")            #Remove "

#Collapse long dataset into LoIs
loi_count_method_2 <- loi_method_2
loi_count_method_2$tally <- 1
loi_count_method_2 <- aggregate(tally~doc_id+serial_number+batch_number+research_designs,loi_count_method_2,sum)
sum(loi_count_method_2) #to check if sum of research designs are equal to number of rows in long dataset

#Add variables for each 'Research Design' with counts
loi_column_method_2 <- cast(loi_count_method_2,doc_id+serial_number+batch_number~research_designs)
names(loi_column_method_2)[names(loi_column_method_2) == "character(0"] <- "unidentified"

#########################################################################################################
#Data Sources

#Creating a list for - Data Sources
data_sources <- tolower(method[,3])
data_sources <- as.data.frame(data_sources)
data_sources <- data_sources[rowSums(data_sources=="")!=ncol(data_sources), ]
data_sources <- paste(data_sources,collapse = "|")
data_sources <- str_replace_all(data_sources , " ", "\\\\s+") 

#Extracting Data Sources from LoIs
loi_method_3 <- loi_raw
loi_method_3$data_sources <- str_extract_all(loi_raw$text, data_sources)
loi_method_3$diseases <- NULL

#Create long dataset with one 'Data Source' per row
loi_method_3 <- cSplit(loi_method_3, "data_sources", ",", "long") 
loi_method_3$data_sources <- str_replace_all(loi_method_3$data_sources , "c\\(", "")          #Remove c(
loi_method_3$data_sources <- str_replace_all(loi_method_3$data_sources , "\\)", "")           #Remove )
loi_method_3$data_sources <- str_replace_all(loi_method_3$data_sources , "\"", "")            #Remove "

#Collapse long dataset into LoIs
loi_count_method_3 <- loi_method_3
loi_count_method_3$tally <- 1
loi_count_method_3 <- aggregate(tally~doc_id+serial_number+batch_number+data_sources,loi_count_method_3,sum)
sum(loi_count_method_3$tally) #to check if sum of data sources are equal to number of rows in long dataset

#Add variables for each 'Data Source' with counts
loi_column_method_3 <- cast(loi_count_method_3,doc_id+serial_number+batch_number~data_sources)
names(loi_column_method_3)[names(loi_column_method_3) == "character(0"] <- "unidentified"

#########################################################################################################
#Qualitative Methods

#Creating a list for - Qualitative Methods
qual_methods <- tolower(method[,4])
qual_methods <- as.data.frame(qual_methods)
qual_methods <- qual_methods[rowSums(qual_methods=="")!=ncol(qual_methods), ]
qual_methods <- paste(qual_methods,collapse = "|")
qual_methods <- str_replace_all(qual_methods , " ", "\\\\s+") 

#Extracting Qualitative Methods from LoIs
loi_method_4 <- loi_raw
loi_method_4$qual_methods <- str_extract_all(loi_raw$text, qual_methods)
loi_method_4$diseases <- NULL

#Create long dataset with one 'Qualitative Methods' per row
loi_method_4 <- cSplit(loi_method_4, "qual_methods", ",", "long") 
loi_method_4$qual_methods <- str_replace_all(loi_method_4$qual_methods , "c\\(", "")          #Remove c(
loi_method_4$qual_methods <- str_replace_all(loi_method_4$qual_methods , "\\)", "")           #Remove )
loi_method_4$qual_methods <- str_replace_all(loi_method_4$qual_methods , "\"", "")            #Remove "

#Collapse long dataset into LoIs
loi_count_method_4 <- loi_method_4
loi_count_method_4$tally <- 1
loi_count_method_4 <- aggregate(tally~doc_id+serial_number+batch_number+qual_methods,loi_count_method_4,sum)
sum(loi_count_method_4$tally) #to check if sum of qualitative methods are equal to number of rows in long dataset

#Add variables for each 'Data Source' with counts
loi_column_method_4 <- cast(loi_count_method_4,doc_id+serial_number+batch_number~qual_methods)
names(loi_column_method_4)[names(loi_column_method_4) == "character(0"] <- "unidentified"

#Replace FGD
loi_column_method_4[,9] <- rowSums(loi_column_method_4[,c("focus group discussion", "fgd")], na.rm=TRUE)
loi_column_method_4[,8] <- NULL

#########################################################################################################
#Quantitative Methods

#Creating a list for - Quantitative Methods
quan_methods <- tolower(method[,5])
quan_methods <- as.data.frame(quan_methods)
quan_methods <- quan_methods[rowSums(quan_methods=="")!=ncol(quan_methods), ]
quan_methods <- paste(quan_methods,collapse = "|")
quan_methods <- str_replace_all(quan_methods , " ", "\\\\s+") 

#Extracting Quantitative Methods from LoIs
loi_method_5 <- loi_raw
loi_method_5$quan_methods <- str_extract_all(loi_raw$text, quan_methods)
loi_method_5$diseases <- NULL

#Create long dataset with one 'Quantitative Methods' per row
loi_method_5 <- cSplit(loi_method_5, "quan_methods", ",", "long") 
loi_method_5$quan_methods <- str_replace_all(loi_method_5$quan_methods , "c\\(", "")          #Remove c(
loi_method_5$quan_methods <- str_replace_all(loi_method_5$quan_methods , "\\)", "")           #Remove )
loi_method_5$quan_methods <- str_replace_all(loi_method_5$quan_methods , "\"", "")            #Remove "

#Collapse long dataset into LoIs
loi_count_method_5 <- loi_method_5
loi_count_method_5$tally <- 1
loi_count_method_5 <- aggregate(tally~doc_id+serial_number+batch_number+quan_methods,loi_count_method_5,sum)
sum(loi_count_method_5$tally) #to check if sum of quantitative methods are equal to number of rows in long dataset

#Add variables for each 'Quantitative Method' with counts
loi_column_method_5 <- cast(loi_count_method_5,doc_id+serial_number+batch_number~quan_methods)
names(loi_column_method_5)[names(loi_column_method_5) == "character(0"] <- "unidentified"

#########################################################################################################

#Add Research Methods extraction variables into the full dataset
loi_all_methods <- merge(merge(merge(loi_column_method_1,loi_column_method_2),
                               merge(loi_column_method_3,loi_column_method_4))
                         ,loi_column_method_5) 

loi_all <- merge(loi_all,loi_all_methods)

#rm(loi_count_method_1, loi_count_method_2, loi_count_method_3, loi_count_method_4)
rm(loi_column_method_1, loi_column_method_2, loi_column_method_3, loi_column_method_4, loi_column_method_5)

##########################################################################################################
#############################################VISUALISATIONS RESEARCH METHODS##############################
##########################################################################################################

#Research Methods - Create a dataset for visualization
viz_general_terms <- prop.table(table(loi_count_method_1$general_terms))*100
viz <- as.data.frame(viz_general_terms)
viz$type <- "general_terms"

viz_research_designs <- as.data.frame(prop.table(table(loi_count_method_2$research_designs))*100)
viz <- rbind.fill(viz,viz_research_designs)
viz$type[is.na(viz$type)] <- 0
viz$type <- str_replace_all(viz$type,"0","research_design")

viz_data_sources <- as.data.frame(prop.table(table(loi_count_method_3$data_sources))*100)
viz <- rbind.fill(viz,viz_data_sources)
viz$type[is.na(viz$type)] <- 0
viz$type <- str_replace_all(viz$type,"0","data_sources")

viz_qual_methods <- as.data.frame(prop.table(table(loi_count_method_4$qual_methods))*100)
viz <- rbind.fill(viz,viz_qual_methods)
viz$type[is.na(viz$type)] <- 0
viz$type <- str_replace_all(viz$type,"0","qual_methods")

viz_quan_methods <- as.data.frame(prop.table(table(loi_count_method_5$quan_methods))*100)
viz <- rbind.fill(viz,viz_quan_methods)
viz$type[is.na(viz$type)] <- 0
viz$type <- str_replace_all(viz$type,"0","quan_methods")

viz$Var1 <- str_replace_all(viz$Var1,"character\\(0","unidentified")

names(viz)[names(viz) == "Freq"] <- "Percentage"
names(viz)[names(viz) == "Var1"] <- "Research_Method"
viz <- subset(viz, viz$Research_Method!="unidentified")

g6 <- ggplot(subset(viz, type=="general_terms"), aes(x=reorder(Research_Method, Percentage), y=Percentage)) +  xlab("General Terms") + ylab("Percentage") + geom_bar(stat="identity", fill="indianred2") + theme_minimal() + coord_flip()
g7 <- ggplot(subset(viz, type=="research_design"), aes(x=reorder(Research_Method, Percentage), y=Percentage)) +  xlab("Research Design") + ylab("Percentage") + geom_bar(stat="identity", fill="turquoise4") + theme_minimal() + coord_flip()
g8 <- ggplot(subset(viz, type=="data_sources"), aes(x=reorder(Research_Method, Percentage), y=Percentage)) +  xlab("Data Sources") + ylab("Percentage")+ geom_bar(stat="identity", fill="tan2") + theme_minimal() + coord_flip()
g9 <- ggplot(subset(viz, type=="qual_methods"), aes(x=reorder(Research_Method, Percentage), y=Percentage)) +  xlab("Qualitative Methods") + ylab("Percentage")+ geom_bar(stat="identity", fill="darkseagreen") + theme_minimal() + coord_flip()
g10 <- ggplot(subset(viz, type=="quan_methods"), aes(x=reorder(Research_Method, Percentage), y=Percentage)) + xlab("Quantitative Methods") + ylab("Percentage") + geom_bar(stat="identity", fill="steelblue") + theme_minimal() + coord_flip()

##########################################################################################################
##############################################EXTRACT IR STRATEGIES#######################################
##########################################################################################################

#Upload lexicon for IR Strategies 
ir_strategies <- read.csv("IR Strategies_Global.csv")

#Creating a list for IR Strategies
ir_strategies_mooc <- tolower(ir_strategies[,2])
ir_strategies_mooc <- as.data.frame(ir_strategies_mooc)
ir_strategies_mooc <- ir_strategies_mooc[rowSums(ir_strategies_mooc=="")!=ncol(ir_strategies_mooc), ]
ir_strategies_mooc <- paste(ir_strategies_mooc,collapse = "|")
ir_strategies_mooc <- str_replace_all(ir_strategies_mooc , " ", "\\\\s+")

#Extracting IR strategies from LoIs
loi_strategies <- loi_raw
loi_strategies$mooc_strategy <- str_extract_all(loi_raw$text, ir_strategies_mooc)
loi_strategies$diseases <- NULL

#Create long dataset with one 'IR Strategy' per row
loi_strategies <- cSplit(loi_strategies, "mooc_strategy", ",", "long") 
loi_strategies$mooc_strategy <- str_replace_all(loi_strategies$mooc_strategy ,"c\\(","")          #Remove c(
loi_strategies$mooc_strategy <- str_replace_all(loi_strategies$mooc_strategy ,"\\)","")           #Remove )
loi_strategies$mooc_strategy <- str_replace_all(loi_strategies$mooc_strategy ,"\"","")            #Remove "

#Collapse long dataset into LoIs
loi_count_strategies <- loi_strategies
loi_count_strategies$tally <- 1
loi_count_strategies <- aggregate(tally~doc_id+serial_number+batch_number+mooc_strategy,loi_count_strategies,sum)

#Add variables for each 'IR Strategy' with counts
loi_column_strategies <- cast(loi_count_strategies,doc_id+serial_number+batch_number~mooc_strategy)
names(loi_column_strategies)[names(loi_column_strategies) == "character(0"] <- "unidentified"

#To find any similarities in colnames before merging
#colname_diff <- colnames(loi_all) %in% colnames(loi_column_strategies)

#Merge All
loi_all <- merge(loi_all,loi_column_strategies)

##########################################################################################################
#############################################VISUALISATIONS IR STRATEGIES#################################
##########################################################################################################

#Create dataset for visualisations
viz_strategies <- as.data.frame(prop.table(table(loi_count_strategies$mooc_strategy))*100)
names(viz_strategies)[names(viz_strategies) == "Var1"] <- "MOOC_Strategy"

ir_strategies$MOOC_Strategy <- tolower(ir_strategies[,2])

ir_strategies$MOOC_Strategy <- str_replace_all(ir_strategies$MOOC_Strategy,"character\\(0\\)","character\\(0" )
viz_strategies <- merge(viz_strategies,ir_strategies)
names(viz_strategies)[names(viz_strategies) == "Freq"] <- "Percentage"

viz_strategies$MOOC_Strategy <- str_replace_all(viz_strategies$MOOC_Strategy,"character\\(0","unidentified")

g11 <- ggplot(viz_strategies, aes(x=reorder(Literature_Strategy, Percentage), y=Percentage)) +  xlab("IR Strategy") + ylab("Percentage")+ geom_bar(stat="identity", fill="darkseagreen") + theme_minimal() + coord_flip()

##########################################################################################################
##############################################EXTRACT IR OUTCOMES#########################################
##########################################################################################################

#Upload lexicon for IR Outcomes 
outcomes <- read.csv("Outcomes.csv")

names(outcomes)[names(outcomes) == "ï..Outcomes"] <- "outcomes"
outcomes$other_terms <- tolower(outcomes$other_terms)

#Creating a list for IR Outcomes
other_terms <- tolower(outcomes[,2])
other_terms <- as.data.frame(other_terms)
other_terms <- other_terms[rowSums(other_terms=="")!=ncol(other_terms), ]
other_terms <- paste(other_terms,collapse = "|")
other_terms <- str_replace_all(other_terms, " ", "\\\\s+") 
loi_outcome <- loi_raw
loi_outcome$other_terms <- str_extract_all(loi_raw$text, other_terms)
loi_outcome$diseases <- NULL

#Create long dataset with one 'IR Strategy' per row
loi_outcome <- cSplit(loi_outcome, "other_terms", ",", "long") 
loi_outcome$other_terms <- str_replace_all(loi_outcome$other_terms ,"c\\(","")          #Remove c(
loi_outcome$other_terms <- str_replace_all(loi_outcome$other_terms ,"\\)","")           #Remove )
loi_outcome$other_terms <- str_replace_all(loi_outcome$other_terms ,"\"","")            #Remove "
loi_outcome$other_terms <- str_replace_all(loi_outcome$other_terms ,"character\\(0","character\\(0\\)")           

#Collapse long dataset into LoIs
loi_outcome <- merge(loi_outcome,outcomes, by="other_terms")
loi_count_outcome <- loi_outcome
loi_count_outcome$tally <- 1
loi_count_outcome <- aggregate(tally~doc_id+serial_number+batch_number+outcomes+loi_outcome,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Add variables for each 'IR Outcome' with counts
loi_column_outcome <- cast(loi_count_outcome,doc_id+serial_number+batch_number~outcomes)

loi_all <- merge(loi_all,loi_column_outcome)

#setwd(" ")
#write.csv(loi_all,"loi_all.csv")

summary <- summary(loi_all)
#write.csv(summary,"Summary.csv")

##########################################################################################################
#############################################VISUALISATIONS IR OUTCOMES#################################
##########################################################################################################

#Create dataset for visualisations
viz_outcomes <- as.data.frame(prop.table(table(loi_count_outcome$Outcomes))*100)
names(viz_outcomes)[names(viz_outcomes) == "Var1"] <- "IR_Outcomes"
names(viz_outcomes)[names(viz_outcomes) == "Freq"] <- "Percentage"

viz_outcomes$IR_Outcomes <- str_replace_all(viz_outcomes$IR_Outcomes,"character\\(0","unidentified")
#viz <- subset(viz, viz$Research_Method!="unidentified research method")


g12 <- ggplot(viz_outcomes, aes(x=reorder(IR_Outcomes, Percentage), y=Percentage)) +  xlab("IR Outcomes") + ylab("Percentage")+ geom_bar(stat="identity", fill="steelblue") + theme_minimal() + coord_flip()
