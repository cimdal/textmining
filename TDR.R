rm(list = ls()) 

library(readtext)
library(stringr)
library(tidyverse)
library(dplyr)
library(splitstackshape)
library(readxl)
library(psych)
library(textclean)
library(reshape)

#Read PDF
loi_raw_1 <- readtext::readtext("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/LOI/Batch 1")
loi_raw_2 <- readtext::readtext("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/LOI/Batch 2")
loi_raw_3 <- readtext::readtext("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/LOI/Batch 3")
loi_raw_4 <- readtext::readtext("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/LOI/Batch 4")
loi_raw_5 <- readtext::readtext("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/LOI/Batch 5")
loi_raw_6 <- readtext::readtext("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/LOI/Batch 6")

#Read Excel - Country, WHO Region, WB Income Group
wb <- read.csv("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/TDR - Excel.csv")
names(wb)[names(wb) == "ï..country"] <- "country"

#Research Methods 
method <- read.csv("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/Research Methods.csv")
names(method)[names(method) == "ï..General_Terms"] <- "General_Terms"

#Converting to Data Frame
loi_raw_1 <- as.data.frame(loi_raw_1)
loi_raw_2 <- as.data.frame(loi_raw_2)
loi_raw_3 <- as.data.frame(loi_raw_3)
loi_raw_4 <- as.data.frame(loi_raw_4)
loi_raw_5 <- as.data.frame(loi_raw_5)
loi_raw_6 <- as.data.frame(loi_raw_6)

#Batch Number - PDF
loi_raw_1$evaluation_score <- 1
loi_raw_2$evaluation_score <- 2
loi_raw_3$evaluation_score <- 3
loi_raw_4$evaluation_score <- 4
loi_raw_5$evaluation_score <- 5
loi_raw_6$evaluation_score <- 6

#Serial Number - LoI
loi_raw_1$serial_number<- paste("loi_", 1:nrow(loi_raw_1), sep = "")
loi_raw_2$serial_number<- paste("loi_", 1:nrow(loi_raw_2), sep = "")
loi_raw_3$serial_number<- paste("loi_", 1:nrow(loi_raw_3), sep = "")
loi_raw_4$serial_number<- paste("loi_", 1:nrow(loi_raw_4), sep = "")
loi_raw_5$serial_number<- paste("loi_", 1:nrow(loi_raw_5), sep = "")
loi_raw_6$serial_number<- paste("loi_", 1:nrow(loi_raw_6), sep = "")

#Combine All Batches
loi_raw  <- rbind(loi_raw_1,loi_raw_2,loi_raw_3,loi_raw_4,loi_raw_5,loi_raw_6)

#Text Clean 
loi_raw$text <- replace_non_ascii(loi_raw$text)
loi_raw$text <- gsub("Reference*.*",'',loi_raw$text)                #Remove everything after references
loi_raw$text <- gsub("REFERENCE*.*",'',loi_raw$text)                #Remove everything after references
loi_raw$text <- gsub('[[:punct:] ]+',' ',loi_raw$text)              #Remove punctuation
loi_raw$text <- gsub("http[[:alnum:][:punct:]]*", "", loi_raw$text) #Remove URL / Hyperlink
loi_raw$text <- gsub("http[^[:space:]]*", "", loi_raw$text)         #Remove URL / Hyperlink
loi_raw$text <- gsub('(f|ht)tp\\S+\\s*',"", loi_raw$text)           #Remove URL / Hyperlink
loi_raw$text <- tolower(loi_raw$text)                               #Lower the letter

##########################################################################################################

#Stage1: Disease
diseases_list <- "\\bhiv\\b|\\btb\\b|tuberc[uo]los[ei]s*|malaria|influenzae|influenz[ae]|dengue|rab[gi]es|trachoma|\\byaws\\b|
                  lepr[eo]sy|chagas|leishmani[oa]ses|taeniasis|neurocysticercos[ei]s|dracuncul[oi]as[ei]s|
                  echinococcos[ei]s|onchocerc[oi]as[ei]s|schistosomias[ei]s|mycetoma|buruli\\s+ulcer|
                  human\\s+immunodeficiency\\s+viruses|trypanosomiasis|sleeping\\s+sickness|guinea\\s+worm|
                  foodborne\\s+trematodiases|lymphatic\\s+filariasis|river\\s+blindness|
                  soil\\s+transmitted\\s+helminthiases" 

#Text Mining - Disease
loi_raw$diseases <- str_extract_all(loi_raw$text, diseases_list)

#Data Cleaning - Replace 
loi_raw$diseases <- str_replace_all(loi_raw$diseases , "character\\(0\\)", "others")        #Filling Empty Cells

loi_diseases <- cSplit(loi_raw, "diseases", ",", "long") 
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\btb\\b", "tuberculosis")#Replace abbreviation
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "c\\(", "")                #Remove c(
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\\)", "")                 #Remove )
loi_diseases$diseases <- str_replace_all(loi_diseases$diseases , "\"", "")                  #Remove " "

#Frequency Table
loi_count <- loi_diseases
loi_count$tally <- 1
loi_count <- aggregate(tally~doc_id+serial_number+evaluation_score+diseases,loi_count,sum)
#sum(loi_count$tally) #to check if sum of diseases are equal 

#Column
loi_column <- cast(loi_count,doc_id+serial_number+evaluation_score~diseases)

##########################################################################################################

#Country - Excel

country_list <- wb[,2]
country_list <- paste(country_list,collapse = "|")
country_list <- str_replace_all(country_list , " ", "\\\\s+") 

#Text Mining - Country
loi_country <- loi_raw
loi_country$country <- str_extract_all(loi_raw$text, country_list)
loi_country$diseases <- NULL

loi_country <- cSplit(loi_country, "country", ",", "long") 

loi_country$country <- str_replace_all(loi_country$country , "c\\(", "")          #Remove c(
loi_country$country <- str_replace_all(loi_country$country , "\\)", "")           #Remove )
loi_country$country <- str_replace_all(loi_country$country , "\"", "")            #Remove "

#Frequency Table
loi_count_2 <- loi_country
loi_count_2$tally <- 1
loi_count_2 <- aggregate(tally~doc_id+serial_number+evaluation_score+country,loi_count_2,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_2 <- cast(loi_count_2,doc_id+serial_number+evaluation_score~country)
names(loi_column_2)[names(loi_column_2) == "character(0"] <- "unknown"
#########################################################################################################

#WHO_Region & WB Income Group - Excel

#Merge
loi_count_2$country <- str_replace_all(loi_count_2$country,"character\\(0","unknown")
loi_count_3  <- merge(loi_count_2,wb,all.x = TRUE,by="country")
loi_count_3$tally <- 1

#Frequency Table - WHO_Region
loi_count_3_1 <- aggregate(tally~doc_id+serial_number+evaluation_score+who_region,loi_count_3,sum,na.action = na.pass)

#Frequency Table - WB Income Group
loi_count_3_2 <- aggregate(tally~doc_id+serial_number+evaluation_score+wb_income_group,loi_count_3,sum,na.action = na.pass)

#Column - WHO_Region
loi_column_3 <- cast(loi_count_3_1,doc_id+serial_number+evaluation_score~who_region,na.action = na.pass)

#Column - WB Income Group
loi_column_4 <- cast(loi_count_3_2,doc_id+serial_number+evaluation_score~wb_income_group,na.action = na.pass)

#########################################################################################################

#Merge all to form a wide table
loi_all <- merge(merge(loi_column,loi_column_2),merge(loi_column_3,loi_column_4))

#########################################################################################################

#Research Method - General Terms
general_terms <- tolower(method[,1])
general_terms <- as.data.frame(general_terms)
general_terms <- general_terms[rowSums(general_terms=="")!=ncol(general_terms), ]
general_terms <- paste(general_terms)
general_terms <- str_replace_all(general_terms , " ", "\\\\s+") 

#Text Mining - Country
loi_method <- loi_raw
loi_method$general_terms <- str_extract_all(loi_raw$text, general_terms)
loi_method$diseases <- NULL

loi_method <- cSplit(loi_method, "general_terms", ",", "long") 

loi_method$general_terms <- str_replace_all(loi_method$general_terms , "c\\(", "")          #Remove c(
loi_method$general_terms <- str_replace_all(loi_method$general_terms , "\\)", "")           #Remove )
loi_method$general_terms <- str_replace_all(loi_method$general_terms , "\"", "")            #Remove "

#Frequency Table
loi_count_method_1 <- loi_method
loi_count_method_1$tally <- 1
loi_count_method_1 <- aggregate(tally~doc_id+serial_number+evaluation_score+general_terms,loi_count_method_1,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_1 <- cast(loi_count_method_1,doc_id+serial_number+evaluation_score~general_terms)
names(loi_column_method_1)[names(loi_column_method_1) == "character(0"] <- "unknown"



#Research Method - Research Design
research_designs <- tolower(method[,2])
research_designs <- as.data.frame(research_designs)
research_designs <- research_designs[rowSums(research_designs=="")!=ncol(research_designs), ]
research_designs <- paste(research_designs)
research_designs <- str_replace_all(research_designs , " ", "\\\\s+") 

#Text Mining - Country
loi_method_2 <- loi_raw
loi_method_2$research_designs <- str_extract_all(loi_raw$text, research_designs)
loi_method_2$diseases <- NULL

loi_method_2 <- cSplit(loi_method_2, "research_designs", ",", "long") 

loi_method_2$research_designs <- str_replace_all(loi_method_2$research_designs , "c\\(", "")          #Remove c(
loi_method_2$research_designs <- str_replace_all(loi_method_2$research_designs , "\\)", "")           #Remove )
loi_method_2$research_designs <- str_replace_all(loi_method_2$research_designs , "\"", "")            #Remove "

#Frequency Table
loi_count_method_2 <- loi_method_2
loi_count_method_2$tally <- 1
loi_count_method_2 <- aggregate(tally~doc_id+serial_number+evaluation_score+research_designs,loi_count_method_2,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_2 <- cast(loi_count_method_2,doc_id+serial_number+evaluation_score~research_designs)
names(loi_column_method_2)[names(loi_column_method_2) == "character(0"] <- "unknown"



#Research Method - Data Sources 
data_sources <- tolower(method[,3])
data_sources <- as.data.frame(data_sources)
data_sources <- data_sources[rowSums(data_sources=="")!=ncol(data_sources), ]
data_sources <- paste(data_sources,collapse = "|")
data_sources <- str_replace_all(data_sources , " ", "\\\\s+") 

#Text Mining - Country
loi_method_3 <- loi_raw
loi_method_3$data_sources <- str_extract_all(loi_raw$text, data_sources)
loi_method_3$diseases <- NULL

loi_method_3 <- cSplit(loi_method_3, "data_sources", ",", "long") 

loi_method_3$data_sources <- str_replace_all(loi_method_3$data_sources , "c\\(", "")          #Remove c(
loi_method_3$data_sources <- str_replace_all(loi_method_3$data_sources , "\\)", "")           #Remove )
loi_method_3$data_sources <- str_replace_all(loi_method_3$data_sources , "\"", "")            #Remove "

#Frequency Table
loi_count_method_3 <- loi_method_3
loi_count_method_3$tally <- 1
loi_count_method_3 <- aggregate(tally~doc_id+serial_number+evaluation_score+data_sources,loi_count_method_3,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_3 <- cast(loi_count_method_3,doc_id+serial_number+evaluation_score~data_sources)
names(loi_column_method_3)[names(loi_column_method_3) == "character(0"] <- "unknown"



#Research Method - Qualitative Methods 
qual_methods <- tolower(method[,4])
qual_methods <- as.data.frame(qual_methods)
qual_methods <- qual_methods[rowSums(qual_methods=="")!=ncol(qual_methods), ]
qual_methods <- paste(qual_methods,collapse = "|")
qual_methods <- str_replace_all(qual_methods , " ", "\\\\s+") 

#Text Mining - Country
loi_method_4 <- loi_raw
loi_method_4$qual_methods <- str_extract_all(loi_raw$text, qual_methods)
loi_method_4$diseases <- NULL

loi_method_4 <- cSplit(loi_method_4, "qual_methods", ",", "long") 

loi_method_4$qual_methods <- str_replace_all(loi_method_4$qual_methods , "c\\(", "")          #Remove c(
loi_method_4$qual_methods <- str_replace_all(loi_method_4$qual_methods , "\\)", "")           #Remove )
loi_method_4$qual_methods <- str_replace_all(loi_method_4$qual_methods , "\"", "")            #Remove "

#Frequency Table
loi_count_method_4 <- loi_method_4
loi_count_method_4$tally <- 1
loi_count_method_4 <- aggregate(tally~doc_id+serial_number+evaluation_score+qual_methods,loi_count_method_4,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_4 <- cast(loi_count_method_4,doc_id+serial_number+evaluation_score~qual_methods)
names(loi_column_method_4)[names(loi_column_method_4) == "character(0"] <- "unknown"



#Research Method - Quantitative Methods 
quan_methods <- tolower(method[,4])
quan_methods <- as.data.frame(quan_methods)
quan_methods <- quan_methods[rowSums(quan_methods=="")!=ncol(quan_methods), ]
quan_methods <- paste(quan_methods,collapse = "|")
quan_methods <- str_replace_all(quan_methods , " ", "\\\\s+") 

#Text Mining - Country
loi_method_5 <- loi_raw
loi_method_5$quan_methods <- str_extract_all(loi_raw$text, quan_methods)
loi_method_5$diseases <- NULL

loi_method_5 <- cSplit(loi_method_5, "quan_methods", ",", "long") 

loi_method_5$quan_methods <- str_replace_all(loi_method_5$quan_methods , "c\\(", "")          #Remove c(
loi_method_5$quan_methods <- str_replace_all(loi_method_5$quan_methods , "\\)", "")           #Remove )
loi_method_5$quan_methods <- str_replace_all(loi_method_5$quan_methods , "\"", "")            #Remove "

#Frequency Table
loi_count_method_5 <- loi_method_5
loi_count_method_5$tally <- 1
loi_count_method_5 <- aggregate(tally~doc_id+serial_number+evaluation_score+quan_methods,loi_count_method_5,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_5 <- cast(loi_count_method_5,doc_id+serial_number+evaluation_score~quan_methods)
names(loi_column_method_5)[names(loi_column_method_5) == "character(0"] <- "unknown"

#########################################################################################################

#Merge all to form a wide table
loi_all_methods <- merge(merge(merge(loi_column_method_1,loi_column_method_2),merge(loi_column_method_3,loi_column_method_4)),loi_column_method_5)

#########################################################################################################

#Excel - Participants List 
participants_list <- read.csv("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/Participants List.csv")
names(participants_list)[names(participants_list) == "ï..serial_number"] <- "serial_number"

loi_participants <- merge(participants_list,loi_all)

#Sample Visualisation 
ggplot(data.frame(loi_participants$Gender), aes(x=loi_participants$Gender)) +
  geom_bar()

ggplot(data.frame(loi_participants$Year.of..birth), aes(x=loi_participants$Year.of..birth)) +
  geom_bar()

ggplot(data.frame(loi_participants$From.which.country.are.you.a.citizen.passport.holder.), aes(x=loi_participants$From.which.country.are.you.a.citizen.passport.holder.)) +
  geom_bar() + coord_flip()
barplot(table(loi_participants$From.which.country.are.you.a.citizen.passport.holder.))


