# Package setup -----------------------------------------------------------

# Packages needed for this script
CRAN_packages <-
  c("tidyverse")

# Install / Update packages available on CRAN 

# Packages already installed
installed_packages <- installed.packages()[,1]

# Check if you have the packages
check_packages <- CRAN_packages %in% installed_packages

# Install packages you don't have
not_installed <- CRAN_packages[!check_packages]
if (length(not_installed > 0)) {
  sapply(not_installed,install.packages)
}

# Load packages

library(tidyverse)

# Load data (alter file path as needed)
load("Senegal_Business_Experiment/Author_Files/data_source.rdata")

# Renaming data
data <- data_source


# Prepare data for main analysis ------------------------------------------

# Indicator variable coded 1 if respondent's district matches seller's political connection
data <- data %>% mutate(district_match = ifelse(trans.enum==1 & data$neighborhood == "Medina" |
                                                  trans.enum==7  & data$neighborhood == "Medina" | 
                                                  trans.enum==9 & data$neighborhood == "Medina" |
                                                  trans.enum==2 & data$neighborhood == "Golf Sud" |
                                                  trans.enum==6  & data$neighborhood == "Golf Sud" |
                                                  trans.enum==8  & data$neighborhood == "Golf Sud" |
                                                  trans.enum==3  & data$neighborhood == "Pikine" |
                                                  trans.enum==4  & data$neighborhood == "Pikine"|
                                                  trans.enum==5  & data$neighborhood == "Pikine", 
                                                1, 
                                                0))

# Indicator variable coded 1 if respondent's ethnicity matches seller's ethnicity
data <- data %>% mutate(coethnic = ifelse(trans.enum==1 & ethnicity==4 |
                                            trans.enum==2 & ethnicity==3 |
                                            trans.enum==3 & ethnicity==5 |
                                            trans.enum==4 & ethnicity==1 |
                                            trans.enum==5 & ethnicity==4 |
                                            trans.enum==6 & ethnicity==1 |
                                            trans.enum==7 & ethnicity==4 |
                                            trans.enum==8 & ethnicity==2 |
                                            trans.enum==9 & ethnicity==4, 
                                          1, 0))

# Indicator variable coded 1 if respondent's religion matches seller's (does not include Muslims not belonging to a brotherhood)
data <- data %>% mutate(coreligion = ifelse(trans.enum==2 & religion==4 |
                                              trans.enum==3 & religion==1 |
                                              trans.enum==4 & religion==3 |
                                              trans.enum==8 & religion==2, 
                                            1, 0))

# Indicator variable coded 1 if respondent is coethnic and/or coreligious to seller
data <- data %>% mutate(coethnicreligion = ifelse(coethnic==1 | coreligion==1, 1, 0))


# Indicator variable coded 1 if respondent reported a political connection
data <- data %>% mutate(any_connection = connection_mayor + connection_central +
                          connection_police + connection_justice) %>%
  mutate(any_connection = ifelse(any_connection > 0, 1, 0))


# Indicator variable coded 1 if respondent purchased phone credit with delayed delivery
data <- data %>% mutate(subscription_trust = ifelse(subscription>1, 1, 0))

# Indicator variable coded 1 if respondent purchased any level of phone credit 
data <- data %>% mutate(purchased = ifelse(subscription==1 | subscription==2 | subscription==3,
                                           1, 0))

# Indicator variable coded 1 if respondent attrited (i.e. did not complete endline survey)
data <- data%>%mutate(attrit = ifelse(data$missing_endline==1,1, 0))

# Imputing missing education values -- center of scale
data$educ_level[data$educ_level==98 | data$educ_level == 99
                       | data$missing_endline==1] <- 3

# Imputing missing or incorrect age values -- mean
data$descriptives.age[data$descriptives.age==0 | data$descriptives.age ==3 | 
                        data$missing_endline==1] <- mean(data$descriptives.age, na.rm=T)

# Imputing missing student indicator -- mean
data$student[data$missing_endline==1] <- mean(data$student, na.rm=T)

# Imputing missing employed indicator -- mean
data$employed[data$missing_endline==1] <- mean(data$employed, na.rm=T)

# Imputing missing coethnic or coreligious indicator -- mean
data$coethnicreligion[data$missing_endline==1] <- mean(data$coethnicreligion, na.rm=T)

# Imputing missing political connection indicator -- mean
data$any_connection[data$missing_endline==1] <- mean(data$any_connection, na.rm=T)

# Imputing missing belief in sellers' connections -- mean
data$thinks_seller_is_connected[data$thinks_seller_is_connected > 3 | is.na(data$thinks_seller_is_connected)==T] <- mean(data$thinks_seller_is_connected[data$thinks_seller_is_connected < 2], na.rm=T)


# Indicator variables for pooled treatment group
data <- data %>% mutate(pool_treatment_group = ifelse(treatment_group==1 | treatment_group==3, 1,
                                                      ifelse(treatment_group==4 | treatment_group==6, 3,
                                                             ifelse(treatment_group==2, 2, 
                                                                    ifelse(treatment_group==5, 4, NA)))))
data <- data %>% mutate(pool_T1 = 
                          ifelse(pool_treatment_group == 3 | pool_treatment_group == 4, 1,0)) %>%
  mutate(pool_T2 = 
           ifelse(pool_treatment_group == 2 | pool_treatment_group == 4, 1,0))


# Indicator variable coded 1 if respondent stated they would have pursued any form of recourse
data <- data %>% mutate(would_have_done_something = ifelse(broken2_full==1 | broken3_full==1  |
                                                             broken4_full==1, 1, 0))

# Imputing missing values for escaping punishment variables -- setting to center of scale
data$escape_police[data$escape_police==98 | data$escape_police==99 | is.na(data$escape_police)==T] <- 3
data$escape_justice[data$escape_justice==98 | data$escape_justice==99| is.na(data$escape_justice)==T] <- 3
data$escape_muni[data$escape_muni==98 | data$escape_muni==99| is.na(data$escape_muni)==T] <- 3

# Imputing missing values for trust and quality questions -- means
data$seller_quality[data$seller_quality==98 | is.na(data$seller_quality)==T] <- mean(data$seller_quality, na.rm=T)
data$seller_trust[data$seller_trust==98 | is.na(data$seller_trust)==T] <- mean(data$seller_trust, na.rm=T)
data$`enum_questions:follow_up_questions`[is.na(data$`enum_questions:follow_up_questions`==T)] <- mean(data$`enum_questions:follow_up_questions`, na.rm=T)
data$`enum_questions:follow_up_polite`[is.na(data$`enum_questions:follow_up_polite`==T)] <- mean(data$`enum_questions:follow_up_polite`, na.rm=T)
data$`enum_questions:follow_up_suspicious`[is.na(data$`enum_questions:follow_up_suspicious`==T)] <- mean(data$`enum_questions:follow_up_suspicious`, na.rm=T)

# Indicator variable coded 1 if respondent stated they would seek formal recourse (police, courts) 
data <- data %>% mutate(would_hypothetically_use_formal_enforcement = ifelse(rupture3_full==1 | rupture5_full==1, 1, 0))

# Imputing missing values for whether respondent used courts -- mean
data$court_use[data$court_use==98 | data$court_use == 99 | is.na(data$court_use)==T] <- mean(data$court_use[data$court_use<90], na.rm=T)

# Imputing missing values for whether respondent used police -- mean
data$police_use[data$police_use==98 | data$police_use== 99 | is.na(data$police_use)==T] <- mean(data$police_use[data$police_us<90], na.rm=T)

# Imputing missing values for how likely a lawyer is to resolve their case -- setting to center of scale
data$lawyer_fix[data$lawyer_fix==98 | data$lawyer_fix == 99 | is.na(data$lawyer_fix)==T] <- 2 

# Imputing missing values for how likely police are to resolve their case -- setting to center of scale
data$police_fix[data$police_fix==98 | data$police_fix == 99 | is.na(data$police_fix)==T] <- 2

# Imputing missing values for how likely courts are to resolve their case -- setting to center of scale
data$court_fix[data$court_fix==98 | data$court_fix == 99 | is.na(data$court_fix)==T] <- 2


# Saving dataset for analysis (alter file path as needed)
save(data, file="../Replication files/Analysis/data_analysis.RData")
