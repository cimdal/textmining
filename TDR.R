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
loi_raw_1 <- readtext::readtext("C:/Users.../Stage 1/LOI/Batch 1")
loi_raw_2 <- readtext::readtext("C:/Users.../Stage 1/LOI/Batch 2")

wb <- read.csv("C:/Users/.../CLASS4.csv")

#Converting to Data Frame
loi_raw_1 <- as.data.frame(loi_raw_1)
loi_raw_2 <- as.data.frame(loi_raw_2)

#Batch Number - PDF
loi_raw_1$evaluation_score <- 1
loi_raw_2$evaluation_score <- 2

#Serial Number - LoI
loi_raw_1$serial_number<- paste("loi_", 1:nrow(loi_raw_1), sep = "")
loi_raw_2$serial_number<- paste("loi_", 1:nrow(loi_raw_2), sep = "")

#Combine All Batches
loi_raw  <- rbind(loi_raw_1,loi_raw_2)

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
sum(loi_count$tally) #to check if sum of diseases are equal

#Column
loi_column <- cast(loi_count,doc_id+serial_number+evaluation_score~diseases)

#Filtering using subset
#loi_subset <- subset(loi_count,tally<=5,select=c(evaluation_score,serial_number,diseases,tally))
#loi <- subset(loi_count,loi_count$tally>5)

#loi_count$evaluation_score <- ifelse(loi_count$tally<5, 2, loi_count$diseases)

##########################################################################################################

#Alternative Filtering
#Filtering using dplyr
loi_filter <- loi_diseases %>%
  select(evaluation_score, serial_number,diseases) %>%
  group_by(evaluation_score,serial_number,diseases) %>%
  count(evaluation_score,serial_number,diseases)%>%
  view()

sum(loi_filter$n) #to check if sum of diseases are equal

#Filtering if disease count is repeated at least 3 times
loi_filter <-loi_diseases %>%
  select(evaluation_score, serial_number, diseases) %>%
  group_by(evaluation_score,serial_number,diseases) %>%
  count(evaluation_score,serial_number,diseases)%>%
  filter(n,n>5)%>%
  view()


##########################################################################################################

#Stage1: Country

#Manual Method
country_list <- "Algeria|Angola|Benin|Burkina\\s+Faso|Camero[uo]n|Comoros|
                Cape\\s+Verde|Chad|Equatorial\\s+Guine[ea]|Gabon|Gambi[ea]|Ghana|
                Guine[ea]$|Guine[ea]\\s+Bissau|Liberia|Madagascar|Mali|Mauritani[ea]|
                Mauritius|Niger$|Nigeria|Macedonia|Serbia|South\\s+Sudan|Tanzania|
                Venezuela|Moldova|Montenegro|Sao\\s+Tome\\s+and Principe|Senegal|Seychelles|
                Sierra\\s+Leone|Togo|Botswana|Burundi|Central\\s+African\\s+Republic|Congo|
                Côte\\s+d\\s+Ivoire|Congo|Eritrea|Ethiopia|Kenya|Lesotho|Malawi|Mozambique|
                Namibia|Rwanda|South\\s+Africa|Swaziland|Uganda|Tanzania|Zambia|Zimbabwe|Canada|
                Cuba|United\\s+States\\s+of\\s+America|Antigua\\s+and\\s+Barbuda|Argentina|Bahamas|
                Barbados|Belize|Brazil|Chile|Colombia|Costa\\s+Rica|Dominica$|Dominican\\s+Republic|
                El\\s+Salvador|Grenada|Guyana|Honduras|Jamaica|Mexico|Panama|Paraguay|
                Saint\\s+Kitts\\s+and\\s+Nevis|Saint\\s+Lucia|Saint\\s+Vincent\\s+and\\s+the\\s+Grenadines|
                Suriname|Trinidad\\s+and\\s+Tobago|Uruguay|Bolivia|Ecuador|
                Guatemala|Haiti|Nicaragua|Peru|Bahrain|Cyprus|Iran|Jordan|Kuwait|Lebanon|
                Libyan\\s+Arab\\s+Jamahiriya|Libya|Oman|Qatar|Saudi\\s+Arabia|S[iy]ria|Tunisia|
                United\\s+Arab\\s+Emirates|Afghanistan|Djibouti|Egypt|Ira[qk]|Morocco|Pakistan|
                Somalia|Sudan|Yemen|Andorra|Austria|Belgium|Croatia|Czech\\s+Republic|Denmark|
                Finland|France|Germany|Greece|Iceland|Ireland|Israel|Italy|Luxembourg|Malta|
                Monaco|Netherlands|Norway|Portugal|San\\s+Marino|Slovenia|Spain|Sweden|Switzerland|
                United\\s+Kingdom|Albania|Armenia|Azerbaijan|Bosnia\\s+and\\s+Herzegovina|Bulgaria|
                Georgia|Kyrgyzstan|Poland|Romania|Slovakia|Tajikistan|Turkey|Turkmenistan|
                Uzbekistan|Yugoslavia|Belarus|Estonia|Hungary|Kazakhstan|Latvia|Lithuania|
                Moldova|Russian|Ukraine|Indonesia|Sri\\s+Lanka|Thailand|Timor\\s+Leste|Bangladesh|
                Bhutan|South\\s+Korea|India|Maldives|Myanmar|Nepal|Australia|Brunei|Japan|New\\s+Zealand|
                Singapore|Cambodia|China|Cook\\s+Islands|Fiji|Kiribati|Lao|Malaysia|Marshall\\s+Islands|
                Micronesia|Mongolia|Nauru|Niue|Palau|Papua\\s+New\\s+Guinea|Philippines|North\\s+Korea|
                Samoa|Solomon\\s+Islands|Tonga|Tuvalu|Vanuatu|Vietnam|Viet\\s+Nam"

country_list <- tolower(country_list)

#Text Mining - Country (Excel, then use _1)
loi_diseases$country<- str_extract_all(loi_diseases$text, country_list)

#Data Cleaning - Replace
loi_diseases$country <- str_replace_all(loi_diseases$country , "character\\(0\\)", "others")        #Filling Empty Cells

loi_diseases<- cSplit(loi_diseases, "country", ",", "long")
loi_diseases$country <- str_replace_all(loi_diseases$country , "character\\(0\\)", "")        #Remove "character(0)"
loi_diseases$country <- str_replace_all(loi_diseases$country , "c\\(", "")                    #Remove c(
loi_diseases$country <- str_replace_all(loi_diseases$country , "\\)", "")                     #Remove )
loi_diseases$country <- str_replace_all(loi_diseases$country , "\"", "")

#To Remove Duplicates
loi_diseases <- loi_diseases[!duplicated(loi_diseases), by = c("serial_number, diseases, country")]

#########################################################################################################

#Excel-Country

#Using Excel Method
country_list_1 <- tolower(wb[,1])
country_list_1 <- paste(country_list_1,collapse = "|")
country_list_1 <- str_replace_all(country_list_1 , " ", "\\\\s+")

#Text Mining - Country
loi_country <- loi_raw
loi_country$country <- str_extract_all(loi_raw$text, country_list_1)
loi_country$diseases <- NULL

#Data Cleaning - Replace
loi_country$country <- str_replace_all(loi_country$country , "character\\(0\\)", "unknown")

loi_country <- cSplit(loi_country, "country", ",", "long")

loi_country$country <- str_replace_all(loi_country$country , "c\\(", "")            #Remove c(
loi_country$country <- str_replace_all(loi_country$country , "\\)", "")             #Remove )
loi_country$country <- str_replace_all(loi_country$country , "\"", "")              #Remove "

#Frequency Table
loi_count_2 <- loi_country
loi_count_2$tally <- 1
loi_count_2 <- aggregate(tally~doc_id+serial_number+evaluation_score+country,loi_count_2,sum)
sum(loi_count_2$tally) #to check if sum of diseases are equal

#Column
loi_column_2 <- cast(loi_count_2,doc_id+serial_number+evaluation_score~country)


#########################################################################################################

#Stage1: WHO Region
WHO_AFRO = list("Algeria",
                "Angola",
                "Benin",
                "Burkina Faso",
                "Cameroon",
                "Cape Verde",
                "Chad",
                "Equatorial Guinea",
                "Gabon",
                "Gambia",
                "Ghana",
                "Guinea",
                "Guinea Bissau",
                "Liberia",
                "Madagascar",
                "Mali",
                "Mauritania",
                "Mauritius",
                "Niger",
                "Nigeria",
                "Sao Tome and Principe",
                "Senegal",
                "Seychelles",
                "Sierra Leone",
                "Togo",
                "Botswana",
                "Burundi",
                "Central African Republic",
                "Congo",
                "C??te d Ivoire",
                "Eritrea",
                "Ethiopia",
                "Kenya",
                "Lesotho",
                "Malawi",
                "Mozambique",
                "Namibia",
                "Rwanda",
                "South Africa",
                "Swaziland",
                "Uganda",
                "Zambia",
                "Zimbabwe",
                "Comoros",
                "Tanzania")
WHO_AMRO = list("Canada",
                "Cuba",
                "United States of America",
                "Antigua and Barbuda",
                "Argentina",
                "Bahamas",
                "Barbados",
                "Belize",
                "Brazil",
                "Chile",
                "Colombia",
                "Costa Rica",
                "Dominica",
                "Dominican",
                "El Salvador",
                "Grenada",
                "Guyana",
                "Honduras",
                "Jamaica",
                "Mexico",
                "Panama",
                "Paraguay",
                "Saint Kitts and Nevis",
                "Saint Lucia",
                "Saint Vincent and the Grenadines",
                "Suriname",
                "Trinidad and Tobago",
                "Uruguay",
                "Venezuela",
                "Bolivia",
                "Ecuador",
                "Guatemala",
                "Haiti",
                "Nicaragua",
                "Peru")
WHO_EMRO = list("Bahrain",
                "Cyprus",
                "Iran",
                "Jordan",
                "Kuwait",
                "Lebanon",
                "Oman",
                "Qatar",
                "Saudi Arabia",
                "Syria",
                "Tunisia",
                "United Arab Emirates",
                "Afghanistan",
                "Djibouti",
                "Egypt",
                "Iraq",
                "Morocco",
                "Pakistan",
                "Somalia",
                "Sudan",
                "Yemen",
                "Libya")
WHO_EURO = list("Andorra",
                "Austria",
                "Belgium",
                "Croatia",
                "Czech Republic",
                "Denmark",
                "Finland",
                "France",
                "Germany",
                "Greece",
                "Iceland",
                "Ireland",
                "Israel",
                "Italy",
                "Luxembourg",
                "Malta",
                "Monaco",
                "Netherlands",
                "Norway",
                "Portugal",
                "San Marino",
                "Slovenia",
                "Spain",
                "Sweden",
                "Switzerland",
                "United Kingdom",
                "Albania",
                "Armenia",
                "Azerbaijan",
                "Bosnia and Herzegovina",
                "Bulgaria",
                "Georgia",
                "Kyrgyzstan",
                "Poland",
                "Romania",
                "Slovakia",
                "Tajikistan",
                "Macedonia",
                "Turkey",
                "Turkmenistan",
                "Uzbekistan",
                "Yugoslavia",
                "Belarus",
                "Estonia",
                "Hungary",
                "Kazakhstan",
                "Latvia",
                "Lithuania",
                "Russian Federation",
                "Ukraine",
                "Moldova",
                "Montenegro",
                "Serbia")
WHO_SEARO = list("Indonesia",
                 "Sri Lanka",
                 "Thailand",
                 "Timor Leste",
                 "Bangladesh",
                 "Bhutan",
                 "North Korea",
                 "India",
                 "Maldives",
                 "Myanmar",
                 "Nepal")
WHO_WPRO = list("Australia",
                "Brunei",
                "Japan",
                "New Zealand",
                "Singapore",
                "Cambodia",
                "China",
                "Cook Islands",
                "Fiji",
                "Kiribati",
                "Lao",
                "Malaysia",
                "Marshall Islands",
                "Micronesia",
                "Mongolia",
                "Nauru",
                "Niue",
                "Palau",
                "Papua New Guinea",
                "Philippines",
                "South Korea",
                "Samoa",
                "Solomon Islands",
                "Tonga",
                "Tuvalu",
                "Vanuatu",
                "Vietnam",
                "Viet nam")

WHO_AFRO <- tolower(WHO_AFRO)
WHO_AMRO <- tolower(WHO_AMRO)
WHO_EMRO <- tolower(WHO_EMRO)
WHO_EURO <- tolower(WHO_EURO)
WHO_SEARO <- tolower(WHO_SEARO)
WHO_WPRO <- tolower(WHO_WPRO)

loi_diseases <- mutate(loi_diseases, WHO_region = ifelse(country %in% WHO_AFRO, "WHO_AFRO",
                                                         ifelse(country %in% WHO_AMRO, "WHO_AMRO",
                                                                ifelse(country %in% WHO_EMRO, "WHO_EMRO",
                                                                       ifelse(country %in% WHO_EURO, "WHO_EURO",
                                                                              ifelse(country %in% WHO_SEARO,"WHO_SEARO",
                                                                                     ifelse(country %in% WHO_WPRO,"WHO_WPRO", "NA")))))))

###############################################################################################################################

#Stage 1- WHO Region using Excel
WHO_Region <- wb[1:2]


###############################################################################################################################

wb <- read_excel("C:/Users/.../Country/CLASS.xls")
income_wb <- wb %>% select(4,8)
income_wb <- income_wb[-c(1:5),]
names(income_wb)[1] <- "country"
names(income_wb)[2] <- "World_Bank_income_group"
income_wb$country <- gsub('[[:punct:]]',' ',income_wb$country)
income_wb$country <- tolower(income_wb$country)
loi_diseases <- left_join(loi_diseases,income_wb, by = "country")
