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
country_wb_who <- read.csv("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/Country List.csv")
names(country_wb_who)[names(country_wb_who) == "ï..country"] <- "country"

#Converting to Data Frame
loi_raw_1 <- as.data.frame(loi_raw_1)
loi_raw_2 <- as.data.frame(loi_raw_2)
loi_raw_3 <- as.data.frame(loi_raw_3)
loi_raw_4 <- as.data.frame(loi_raw_4)
loi_raw_5 <- as.data.frame(loi_raw_5)
loi_raw_6 <- as.data.frame(loi_raw_6)

#Batch Number - PDF
loi_raw_1$batch_number <- 1
loi_raw_2$batch_number <- 2
loi_raw_3$batch_number <- 3
loi_raw_4$batch_number <- 4
loi_raw_5$batch_number <- 5
loi_raw_6$batch_number <- 6

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
loi_count <- aggregate(tally~doc_id+serial_number+batch_number+diseases,loi_count,sum)
#sum(loi_count$tally) #to check if sum of diseases are equal 

#Column
loi_column <- cast(loi_count,doc_id+serial_number+batch_number~diseases)

#Average - Disease 
loi_column$average_disease <- rowMeans(loi_column[,4:23],na.rm=T)
bool_disease <- ifelse(loi_column[,4:23] >= loi_column$average_disease,1,NA)
bool_disease <- loi_column[,4:23]*bool_disease[,1:20]
loi_column[,4:23] <- bool_disease

##########################################################################################################

#Country - Excel
country_list <- country_wb_who[,2]
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
loi_count_2 <- aggregate(tally~doc_id+serial_number+batch_number+country,loi_count_2,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_2 <- cast(loi_count_2,doc_id+serial_number+batch_number~country)
names(loi_column_2)[names(loi_column_2) == "character(0"] <- "unidentified country"

#Average - Country
loi_column_2$average_country <- rowMeans(loi_column_2[,4:50],na.rm=T)
bool_country <- ifelse(loi_column_2[,4:50] >= loi_column_2$average_country,1,NA)
bool_country <- loi_column_2[,4:50]*bool_country[,1:47]
loi_column_2[,4:50] <- bool_country

#########################################################################################################

#WHO_Region & country_wb_who Income Group - Excel

#Merge
loi_count_2$country <- str_replace_all(loi_count_2$country,"character\\(0","unknown")
loi_count_3  <- merge(loi_count_2,country_wb_who,all.x = TRUE,by="country")
loi_count_3$tally <- 1

#Frequency Table - WHO_Region
loi_count_3_1 <- aggregate(tally~doc_id+serial_number+batch_number+who_region,loi_count_3,sum,na.action = na.pass)

#Frequency Table - WB Income Group
loi_count_3_2 <- aggregate(tally~doc_id+serial_number+batch_number+wb_income_group,loi_count_3,sum,na.action = na.pass)

#Column - WHO_Region
loi_column_3 <- cast(loi_count_3_1,doc_id+serial_number+batch_number~who_region,na.action = na.pass)

#Column - WB Income Group
loi_column_4 <- cast(loi_count_3_2,doc_id+serial_number+batch_number~wb_income_group,na.action = na.pass)

#########################################################################################################

#Merge all to form a wide table
loi_all <- merge(merge(loi_column,loi_column_2),merge(loi_column_3,loi_column_4))

#########################################################################################################

#Research Methods 
method <- read.csv("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/Research Methods.csv")
names(method)[names(method) == "ï..General_Terms"] <- "General_Terms"

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
loi_count_method_1 <- aggregate(tally~doc_id+serial_number+batch_number+general_terms,loi_count_method_1,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_1 <- cast(loi_count_method_1,doc_id+serial_number+batch_number~general_terms)
names(loi_column_method_1)[names(loi_column_method_1) == "character(0"] <- "unidentified general terms"



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
loi_count_method_2 <- aggregate(tally~doc_id+serial_number+batch_number+research_designs,loi_count_method_2,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_2 <- cast(loi_count_method_2,doc_id+serial_number+batch_number~research_designs)
names(loi_column_method_2)[names(loi_column_method_2) == "character(0"] <- "unidentified research design"

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
loi_count_method_3 <- aggregate(tally~doc_id+serial_number+batch_number+data_sources,loi_count_method_3,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_3 <- cast(loi_count_method_3,doc_id+serial_number+batch_number~data_sources)
names(loi_column_method_3)[names(loi_column_method_3) == "character(0"] <- "unidentified data sources"



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
loi_count_method_4 <- aggregate(tally~doc_id+serial_number+batch_number+qual_methods,loi_count_method_4,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_4 <- cast(loi_count_method_4,doc_id+serial_number+batch_number~qual_methods)
names(loi_column_method_4)[names(loi_column_method_4) == "character(0"] <- "unidentified qualitative methods"

#fgd to full term
loi_column_method_4[,9] <- rowSums(loi_column_method_4[,c("focus group discussion", "fgd")], na.rm=TRUE)
loi_column_method_4[,8] <- NULL

#Research Method - Quantitative Methods 
quan_methods <- tolower(method[,5])
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
loi_count_method_5 <- aggregate(tally~doc_id+serial_number+batch_number+quan_methods,loi_count_method_5,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_method_5 <- cast(loi_count_method_5,doc_id+serial_number+batch_number~quan_methods)
names(loi_column_method_5)[names(loi_column_method_5) == "character(0"] <- "unidentified quantitative methods"

#########################################################################################################

#Merge all to form a wide table
loi_all_methods <- merge(merge(merge(loi_column_method_1,loi_column_method_2),
                    merge(loi_column_method_3,loi_column_method_4))
                        ,loi_column_method_5) 

loi_all <- merge(loi_all,loi_all_methods)

#Data Visualisation - Percentage Missing for Research Methods
error_general_terms <- prop.table(table(loi_count_method_1$general_terms))*100

error <- as.data.frame(error_general_terms)
error$type <- "general_terms"

error_research_designs <- as.data.frame(prop.table(table(loi_count_method_2$research_designs))*100)
error <- rbind.fill(error,error_research_designs)
error$type[is.na(error$type)] <- 0
error$type <- str_replace_all(error$type,"0","research_design")

error_data_sources <- as.data.frame(prop.table(table(loi_count_method_3$data_sources))*100)
error <- rbind.fill(error,error_data_sources)
error$type[is.na(error$type)] <- 0
error$type <- str_replace_all(error$type,"0","data_sources")

error_qual_methods <- as.data.frame(prop.table(table(loi_count_method_4$qual_methods))*100)
error <- rbind.fill(error,error_qual_methods)
error$type[is.na(error$type)] <- 0
error$type <- str_replace_all(error$type,"0","qual_methods")

error_quan_methods <- as.data.frame(prop.table(table(loi_count_method_5$quan_methods))*100)
error <- rbind.fill(error,error_quan_methods)
error$type[is.na(error$type)] <- 0
error$type <- str_replace_all(error$type,"0","quan_methods")

error$Var1 <- str_replace_all(error$Var1,"character\\(0","unidentified research method")

names(error)[names(error) == "Freq"] <- "Percentage"
names(error)[names(error) == "Var1"] <- "Research_Method"

ggplot(error, aes(fill=Research_Method, y=Percentage, x=type)) + 
  geom_bar(position="stack", stat="identity") 

#########################################################################################################

#Excel - IR Strategies

ir_strategies <- read.csv("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/Strategies.csv")

#IR Strategies - MOOC 
ir_strategies_mooc <- tolower(ir_strategies[,2])
ir_strategies_mooc <- as.data.frame(ir_strategies_mooc)
ir_strategies_mooc <- ir_strategies_mooc[rowSums(ir_strategies_mooc=="")!=ncol(ir_strategies_mooc), ]
ir_strategies_mooc <- paste(ir_strategies_mooc,collapse = "|")
ir_strategies_mooc <- str_replace_all(ir_strategies_mooc , " ", "\\\\s+") 

#Text Mining - IR Strategies - MOOC 
loi_strategies <- loi_raw
loi_strategies$mooc_strategy <- str_extract_all(loi_raw$text, ir_strategies_mooc)
loi_strategies$diseases <- NULL

loi_strategies <- cSplit(loi_strategies, "mooc_strategy", ",", "long") 

loi_strategies$mooc_strategy <- str_replace_all(loi_strategies$mooc_strategy ,"c\\(","")          #Remove c(
loi_strategies$mooc_strategy <- str_replace_all(loi_strategies$mooc_strategy ,"\\)","")           #Remove )
loi_strategies$mooc_strategy <- str_replace_all(loi_strategies$mooc_strategy ,"\"","")            #Remove "

#Frequency Table
loi_count_strategies <- loi_strategies
loi_count_strategies$tally <- 1
loi_count_strategies <- aggregate(tally~doc_id+serial_number+batch_number+mooc_strategy,loi_count_strategies,sum)

#Column
loi_column_strategies <- cast(loi_count_strategies,doc_id+serial_number+batch_number~mooc_strategy)
names(loi_column_strategies)[names(loi_column_strategies) == "character(0"] <- "unidentified IR strategy"

#Merge All
loi_all <- merge(loi_all,loi_column_strategies)

#Data Visualisation - Percentage Missing for MOOC Strategy 
error_strategies <- as.data.frame(prop.table(table(loi_count_strategies$mooc_strategy))*100)
names(error_strategies)[names(error_strategies) == "Var1"] <- "MOOC_Strategy"

ir_strategies$MOOC_Strategy <- tolower(ir_strategies[,2])
ir_strategies$MOOC_Strategy <- str_replace_all(ir_strategies$MOOC_Strategy,"character\\(0\\)","character\\(0" )

error_strategies <- merge(error_strategies,ir_strategies)
names(error_strategies)[names(error_strategies) == "Freq"] <- "Percentage"

error_strategies$MOOC_Strategy <- str_replace_all(error_strategies$MOOC_Strategy,"character\\(0","unidentified strategy")

ggplot(error_strategies, aes(fill=Literature_Strategy, y=Percentage, x=MOOC_Strategy)) + 
  geom_bar(position="stack", stat="identity") + coord_flip()

#########################################################################################################

#Excel - IR Strategies

outcomes <- read.csv("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/Outcomes.csv")
names(outcomes)[names(outcomes) == "ï..Outcomes"] <- "outcomes"
outcomes$other_terms <- tolower(outcomes$other_terms)

other_terms <- tolower(outcomes[,2])
other_terms <- as.data.frame(other_terms)
other_terms <- other_terms[rowSums(other_terms=="")!=ncol(other_terms), ]
other_terms <- paste(other_terms,collapse = "|")
other_terms <- str_replace_all(other_terms, " ", "\\\\s+") 

#Text Mining - IR Strategies - MOOC 
loi_outcome <- loi_raw
loi_outcome$other_terms <- str_extract_all(loi_raw$text, other_terms)
loi_outcome$diseases <- NULL

loi_outcome <- cSplit(loi_outcome, "other_terms", ",", "long") 

loi_outcome$other_terms <- str_replace_all(loi_outcome$other_terms ,"c\\(","")          #Remove c(
loi_outcome$other_terms <- str_replace_all(loi_outcome$other_terms ,"\\)","")           #Remove )
loi_outcome$other_terms <- str_replace_all(loi_outcome$other_terms ,"\"","")            #Remove "
loi_outcome$other_terms <- str_replace_all(loi_outcome$other_terms ,"character\\(0","character\\(0\\)")           
    
loi_outcome <- merge(loi_outcome,outcomes, by="other_terms")

#Frequency Table
loi_count_outcome <- loi_outcome
loi_count_outcome$tally <- 1
loi_count_outcome <- aggregate(tally~doc_id+serial_number+batch_number+outcomes,loi_outcome,sum)
#sum(loi_count_2$tally) #to check if sum of diseases are equal 

#Column
loi_column_outcome <- cast(loi_count_outcome,doc_id+serial_number+batch_number~outcomes)

loi_all <- merge(loi_all,loi_column_outcome)

write.csv(loi_all,"loi_all.csv")

#########################################################################################################

#Excel - Participants List 
participants_list <- read.csv("C:/Users/asus/OneDrive - Sunway Education Group/UNU - IIGH/TDR/Stage 1/Participants List.csv")
names(participants_list)[names(participants_list) == "ï..serial_number"] <- "serial_number"

#Data Clean
participants_list$country <- tolower(participants_list$country)
participants_list$country <- str_replace_all(participants_list$country,"united states of america","united states")
participants_list$country <- str_replace_all(participants_list$country,"usa","united states")
participants_list$country <- str_replace_all(participants_list$country,"republic of the philippines","philippines")
participants_list$country <- str_replace_all(participants_list$country,"british","united kingdom")
participants_list$country <- str_replace_all(participants_list$country,"uk","united kingdom")
participants_list$country <- str_replace_all(participants_list$country,"england","united kingdom")
participants_list$country <- str_replace_all(participants_list$country,"p.r.","")
participants_list$country <- str_replace_all(participants_list$country,"zimbabwean","zimbabwe")
participants_list$country <- str_replace_all(participants_list$country,"cameroonian","cameroon")
participants_list$country <- str_replace_all(participants_list$country,"(?<=ia).*","")
participants_list$country <- str_replace_all(participants_list$country,"the gambia","gambia")
participants_list$country <- str_replace_all(participants_list$country,"somali$","somalia")
participants_list$country <- str_replace_all(participants_list$country,"of cameroon","cameroon")
participants_list$country <- str_replace_all(participants_list$country,"of congo","of the congo")
participants_list$country <- str_replace_all(participants_list$country,"or the congo","of the congo")
participants_list$country <- str_replace_all(participants_list$country,"srilankan","sri lanka")

participants_list$country <- str_replace_all(participants_list$country,"\\/yes","")
participants_list$country <- str_replace_all(participants_list$country,"\\/\\s+yes","")
participants_list$country <- str_replace_all(participants_list$country,"citizen","")
participants_list$country <- str_replace_all(participants_list$country,"with.*","")
participants_list$country <- str_replace_all(participants_list$country,"nationality.*","")
participants_list$country <- str_replace_all(participants_list$country,"\\(.*","")

participants_list$country <- str_replace_all(participants_list$country,"argentina\\/italy","c\\(\\'\\'argentina\\'\\'\\,\\'\\'italy\\'\\'\\)")
participants_list$country <- str_replace_all(participants_list$country,"guinea\\/canada","c\\(\\'\\'guinea\\'\\'\\,\\'\\'canada\\'\\'\\)")
participants_list$country <- str_replace_all(participants_list$country,"united kingdom\\s+\\&\\s+ghana","c\\(\\'\\'united kingdom\\'\\'\\,\\'\\'ghana\\'\\'\\)")
participants_list$country <- str_replace_all(participants_list$country,"united states\\s+\\/\\s+spain","c\\(\\'\\'united states\\'\\'\\,\\'\\'spain\\'\\'\\)")


loi_participants <- merge(participants_list,loi_all)
loi_all$no <- 1:nrow(loi_all)
loi_all_part <- merge(loi_participants,loi_all, all=T)

n_occur <- as.data.frame(table(loi_all_part$no))
names(n_occur)[names(n_occur) == "Var1"] <- "no"
repeat_occur <- n_occur[n_occur$Freq>1,]

loi_all_part <- loi_all_part[-which(duplicated(loi_all_part$no)),]

loi_all_part$birth_year[is.na(loi_all_part$birth_year)] <- 0
loi_all_part$birth_year <- str_replace_all(loi_all_part$birth_year,"0$","Unknown")

loi_all_part$gender[is.na(loi_all_part$gender)] <- 0
loi_all_part$gender <- str_replace_all(loi_all_part$gender,"0","Unknown")

loi_all_part$country[is.na(loi_all_part$country)] <- 0
loi_all_part$country <- str_replace_all(loi_all_part$country,"0","Unknown")

loi_all_part$academic_qualification[is.na(loi_all_part$academic_qualification)] <- 0
loi_all_part$academic_qualification <- str_replace_all(loi_all_part$academic_qualification,"0","Unknown")

loi_all_part$prof_category[is.na(loi_all_part$prof_category)] <- 0
loi_all_part$prof_category <- str_replace_all(loi_all_part$prof_category,"0","Unknown")

#Data Visualisation 

#Gender
ggplot(data.frame(loi_all_part$gender), aes(x=loi_all_part$gender)) +
  geom_bar() + coord_flip()

#Birth Year
ggplot(data.frame(loi_all_part$birth_year), aes(x=loi_all_part$birth_year)) +
  geom_bar() + coord_flip()

#Country
participants_country <-  as.data.frame(loi_all_part$country)
names(participants_country)[names(participants_country) == "loi_all_part$country"] <- "country"

participants_country <- cSplit(participants_country, "country", ",", "long") 

participants_country$country <- str_replace_all(participants_country$country, "c\\(", "")       #Remove c(
participants_country$country <- str_replace_all(participants_country$country, "\\)", "")        #Remove )
participants_country$country <- str_replace_all(participants_country$country, "\\'", "")        #Remove "

ggplot(data.frame(participants_country), aes(x=country)) +
  geom_bar() + coord_flip()


#Country Match
test <- subset(loi_all_part,select=c("country","serial_number","doc_id","batch_number"))
names(test)[names(test) == "country"] <- "birth_country"

test <- merge(loi_count_2,test,all=T)

test$check <-ifelse(test$birth_country == test$country,print("match"),print("non-match"))

ggplot(data.frame(test$check), aes(x=test$check)) +
  geom_bar() + coord_flip()


#Academic Qualification
acad_qual <-  as.data.frame(loi_all_part$academic_qualification)
names(acad_qual)[names(acad_qual) == "loi_all_part$academic_qualification"] <- "academic_qualification"

acad_qual <- cSplit(acad_qual, "academic_qualification", ",", "long") 

acad_qual$academic_qualification <- str_replace_all(acad_qual$academic_qualification, "c\\(", "")       #Remove c(
acad_qual$academic_qualification <- str_replace_all(acad_qual$academic_qualification, "\\)", "")        #Remove )
acad_qual$academic_qualification <- str_replace_all(acad_qual$academic_qualification, "\\'", "")        #Remove "

ggplot(data.frame(acad_qual), aes(x=academic_qualification)) +
  geom_bar() + coord_flip()

#Professional Category
prof_cat <-  as.data.frame(loi_all_part$prof_category)
names(prof_cat)[names(prof_cat) == "loi_all_part$prof_category"] <- "prof_category"

prof_cat <- cSplit(prof_cat, "prof_category", ",", "long") 

prof_cat$prof_category <- str_replace_all(prof_cat$prof_category, "c\\(", "")       #Remove c(
prof_cat$prof_category <- str_replace_all(prof_cat$prof_category, "\\)", "")        #Remove )
prof_cat$prof_category <- str_replace_all(prof_cat$prof_category, "\\'", "")        #Remove "

ggplot(data.frame(prof_cat), aes(x=prof_category)) +
  geom_bar() + coord_flip()



