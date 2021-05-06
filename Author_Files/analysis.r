# Package setup -----------------------------------------------------------

# Packages needed for this script
CRAN_packages <-
  c("tidyverse", "stargazer", "multcomp", "car", "haven", "sjlabelled", "nnet")

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
library(stargazer)
library(multcomp)
library(car)
library(haven)
library(sjlabelled)
library(nnet)

# Load data (alter file path as needed)
load("../Replication files/Analysis/data_analysis.RData")

# Ensure that all output will print in console
options(max.print = 2000)

### RESULTS FROM MAIN BODY ###

# Figure 1: Perceived impunity of connected people ------------------------

# Creating dataset for figure
police_escape <- as.data.frame(data$escape_police)
courts_escape <- as.data.frame(data$escape_justice)
muni_escape <- as.data.frame(data$escape_muni)
names(police_escape) <- names(courts_escape) <- names(muni_escape)  <-  "Escape"
police_escape <- mutate(police_escape, type="Police")
courts_escape <- mutate(courts_escape, type="Courts")
muni_escape <- mutate(muni_escape, type="Council")
figure1_data <- rbind(police_escape, courts_escape, muni_escape)
figure1_data_pct <- figure1_data %>% group_by(type, Escape) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))

# Generating figure
ggplot(figure1_data_pct, aes(as.factor(Escape), freq*100, fill=type)) +
  geom_bar(stat="identity", position="dodge")+
  theme_bw() +
  xlab("Probability of escaping punishment") + ylab("Percentage") + ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey80", "grey50", "grey20")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=14, face="bold"),
        legend.text=element_text(size=14)) +
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5), 
                   labels=c("Very easy to\nescape", 
                            "A little easy to\nescape",
                            "No effect",
                            "A little hard to\nescape",
                            "Very hard to\nescape")) + 
  theme(legend.title=element_blank()) +
  ylim(0,60)



# Figure 2: Respondents' connections --------------------------------------
table(data$connection_central) #verifying how many people are connected to central government
central <- (152/(152+1270))*100 #percentage reporting this connection
table(data$connection_mayor) #verifying how many people are connected to mayor
mayor <- (381/(381+1041))*100 #percentage reporting this connection
table(data$connection_police) #verifying how many people are connected to police
police <- (343/(343+1079))*100 #percentage reporting this connection
table(data$connection_justice) #verifying how many people are connected to courts
justice <- (170/(170+1252))*100 #percentage reporting this connection

#constructing dataset for plot
names <- c("Central", "Council", "Police", "Law")
pers <- c(central, mayor, police, justice)
tab <- as.data.frame(cbind(names, pers))
tab$pers <- as.numeric(as.character(tab$pers))

#generating plot
tab %>% ggplot(aes(names, pers)) +
  geom_bar(stat="identity") +
  scale_y_continuous(name = "Percentage", limits = c(0, 100)) + 
  scale_x_discrete(name = "Institution buyer is connected to",
                   labels = c("Central government", "Council", "Courts", "Police")) +
  theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=14, face="bold"),
        legend.text=element_text(size=14))


# Figure 3: Distribution of buyers' purchases -----------------------------
ggplot(data, aes(as.factor(subscription))) + geom_histogram(stat="count") + 
  theme_bw() +
  xlab("Purchase level") + ylab("Count") + ggtitle("Distribution of purchase levels") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=14, face="bold")) +
  scale_x_discrete(labels=c("Declined \n deal", "No delay", "Delay($)", "Delay($$$)"))



# Table 4: Manipulation check ---------------------------------------------
table4 <- lm(thinks_seller_is_connected ~ pool_T1 +
                as.factor(enum) + 
                as.factor(blockid) + 
                descriptives.age + 
                gender + 
                as.factor(educ_level) + 
                employed + 
                student +
                coethnicreligion*pool_T1
              , data = data)
summary(table4)

#Control group outcome mean and standard deviation
data %>% group_by(pool_T1) %>% summarise(avg = mean(thinks_seller_is_connected),
                                         std = sd(thinks_seller_is_connected)) %>% .[1,]


# Table 5: Average treatment effects --------------------------------------

#Model 1: Unpooled, purchased at all
m1 <- lm(purchased ~ T1*T2 + T1*T3 + 
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) +
           employed +
           student +
           coethnicreligion*T1 +
           coethnicreligion*T2 +
           coethnicreligion*T3,
         data=data)

#control mean and SD
data %>% group_by(T1, T2, T3) %>%
  summarise(mean(purchased),
            sd(purchased)) %>% .[1,]


#Model 2: Pooled, purchased at all
m2 <- lm(purchased ~ pool_T1*pool_T2 + 
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1 +
           coethnicreligion*pool_T2,
         data=data)

#control mean and SD
data %>% group_by(pool_T1, pool_T2) %>%
  summarise(mean(purchased),
            sd(purchased)) %>% .[1,]



#Model 3: Unpooled, purchased with delay
m3 <- lm(subscription_trust ~ T1*T2 + T1*T3 + 
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) +
           employed +
           student +
           coethnicreligion*T1 +
           coethnicreligion*T2 +
           coethnicreligion*T3,
         data=data)
#control mean and SD
data %>% group_by(T1, T2, T3) %>%
  summarise(mean(subscription_trust),
            sd(subscription_trust)) %>% .[1,]

#Model 4: Pooled, purchased with delay

m4 <- lm(subscription_trust ~ pool_T1*pool_T2 + 
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1 +
           coethnicreligion*pool_T2,
         data=data)
#control mean and SD
data %>% group_by(pool_T1, pool_T2) %>%
  summarise(mean(subscription_trust),
            sd(subscription_trust)) %>% .[1,]

#creating table
stargazer(m1, m2, m3, m4,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_level", 
                   "employed","student", "coethnicreligion"),
          #star.cutoffs = c(0.2, 0.1, 0.02), #uncomment for one-sided test of T1/pool_T1 effect
          float=F,
          df=F)

# Figure 4a: effect of sellers connections by buyer connections - purchased at all --------

#organizing purchase-at-all outcome data by connection treatment group and buyers' political connections
avg_by_dyad <- data[data$missing_endline==0,] %>% group_by(any_connection, pool_T1) %>% 
  summarise(mean(purchased),
            SD = sd(purchased),
            n = n(),
            SE = sd(purchased)/sqrt(n))
avg_by_dyad$pool_T1 <- as.character(avg_by_dyad$pool_T1)
names(avg_by_dyad)[3] <- "purchased"

#group means
avg_by_dyad 

#figure
avg_by_dyad %>% ggplot(aes(any_connection, purchased)) +
  theme_bw() + 
  coord_cartesian(ylim=c(0, .5)) + 
  scale_fill_manual(values = c("grey80", "grey20"),
                    name = "Treatment",
                    labels = c("No signal", "Political connection \nsignaled")) +
  geom_bar(aes(fill = as.factor(pool_T1)), stat="identity", position = "dodge") +
  scale_x_continuous(breaks=c(0,1), labels=c("No", "Yes")) +
  xlab("Buyer connected") + ylab("Purchased at all") + 
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=16, face="bold")) +
  theme(legend.text=element_text(size=12))  +
  annotate("text", label = "A", x = -.22, y = .3, size=5)  +
  annotate("text", label = "B", x = .22, y = .29, size=5)  +
  annotate("text", label = "C", x = .78, y = .38, size=5)  +
  annotate("text", label = "D", x = 1.22, y = .34, size=5)

#model for difference tests
r1 <- lm(purchased ~ pool_T1*any_connection +  
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1,
         data=data)
summary(r1) #see pool_T1 and pool_T1:any_connection for 2nd and 3rd reported tests (B-A and diff-in-diff, respectively)
summary(glht(r1, linfct = c("pool_T1 + pool_T1:any_connection >= 0"))) #for 1st reported test (D-C)


# Figure 4b: effect of sellers connections by buyer connections - purchased with delay --------

#organizing purchase-with-delay outcome data by connection treatment group and buyers' political connections
avg_by_dyad <- data[data$missing_endline==0,] %>% group_by(any_connection, pool_T1) %>% 
  summarise(mean(subscription_trust),
            SD = sd(subscription_trust),
            n = n(),
            SE = sd(subscription_trust)/sqrt(n))
dyad <- filter(avg_by_dyad, is.na(any_connection)==F)
dyad$pool_T1 <- as.character(dyad$pool_T1)
names(dyad)[3] <- "Subscription_Trust"

#group means
avg_by_dyad 

#figure
dyad %>% ggplot(aes(any_connection, Subscription_Trust)) +
  theme_bw() + 
  coord_cartesian(ylim=c(0, .5)) + 
  scale_fill_manual(values = c("grey80", "grey20"),
                    name = "Treatment",
                    labels = c("No signal", "Political connection \nsignaled")) +
  geom_bar(aes(fill = as.factor(pool_T1)), stat="identity", position = "dodge") +
  scale_x_continuous(breaks=c(0,1), labels=c("No", "Yes")) +
  xlab("Buyer connected") + ylab("Purchased with delay") + 
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=16, face="bold")) +
  theme(legend.text=element_text(size=12))  +
  annotate("text", label = "A", x = -.22, y = .18, size=5)  +
  annotate("text", label = "B", x = .22, y = .18, size=5)  +
  annotate("text", label = "C", x = .78, y = .21, size=5)  +
  annotate("text", label = "D", x = 1.22, y = .20, size=5)

#model for difference tests
r2 <- lm(subscription_trust ~ pool_T1*any_connection +  
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1,
         data=data)
summary(r2) #see pool_T1 and pool_T1:any_connection for 2nd and 3rd reported tests (B-A and diff-in-diff, respectively)
summary(glht(r2, linfct = c("pool_T1 + pool_T1:any_connection >= 0"))) #for 1st reported test (D-C)

# Figure 5a: effect of contracts by buyer connections - purchased at all --------

#organizing purchase-at-all outcome data by contract treatment group and buyers' political connections
avg_by_dyad5 <- data %>% group_by(any_connection, pool_T2) %>% 
  summarise(mean(purchased),
            SD = sd(purchased),
            n = n(),
            SE = sd(purchased)/sqrt(n))
dyad5 <- filter(avg_by_dyad5, any_connection==0 | any_connection==1)
dyad5$pool_T2 <- as.character(dyad5$pool_T2)
names(dyad5)[3] <- "purchased"

#group means
dyad5 

#figure

dyad5 %>% ggplot(aes(any_connection, purchased)) +
  theme_bw() + 
  coord_cartesian(ylim=c(0, .5)) + 
  scale_fill_manual(values = c("grey80", "grey20"),
                    name = "Treatment",
                    labels = c("No contract", "Formal contract")) +
  geom_bar(aes(fill = as.factor(pool_T2)), stat="identity", position = "dodge") +
  scale_x_continuous(breaks=c(0,1), labels=c("No", "Yes")) +
  xlab("Buyer connected") + ylab("Purchased at all") + 
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=16, face="bold")) +
  theme(legend.text=element_text(size=12)) +
  annotate("text", label = "E", x = -.22, y = .3, size=5)  +
  annotate("text", label = "F", x = .22, y = .28, size=5)  +
  annotate("text", label = "G", x = .78, y = .32, size=5)  +
  annotate("text", label = "H", x = 1.22, y = .45, size=5)  

#model for difference tests
r3 <- lm(purchased ~ pool_T2*any_connection  +  
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T2,
         data=data)
summary(r3) #see pool_T2 and pool_T2:any_connection for 2nd and 3rd reported tests (F-E and diff-in-diff, respectively)
summary(glht(r3, linfct = c("pool_T2 + pool_T2:any_connection = 0"))) #for 1st reported test (H-G)

# Figure 5b: effect of contracts by buyer connections - purchased with delay --------

#organizing purchase-with-delay outcome data by contract treatment group and buyers' political connections
avg_by_dyad5 <- data %>% group_by(any_connection, pool_T2) %>% 
  summarise(mean(subscription_trust),
            SD = sd(subscription_trust),
            n = n(),
            SE = sd(subscription_trust)/sqrt(n))
dyad5 <- filter(avg_by_dyad5, any_connection==0 | any_connection==1)
dyad5$pool_T2 <- as.character(dyad5$pool_T2)
names(dyad5)[3] <- "subscription_trust"

#group means
dyad5

#figure
dyad5 %>% ggplot(aes(any_connection, subscription_trust)) +
  theme_bw() + 
  #theme(panel.grid = element_blank()) +
  coord_cartesian(ylim=c(0, .5)) + 
  scale_fill_manual(values = c("grey80", "grey20"),
                    name = "Treatment",
                    labels = c("No contract", "Formal contract")) +
  geom_bar(aes(fill = as.factor(pool_T2)), stat="identity", position = "dodge") +
  scale_x_continuous(breaks=c(0,1), labels=c("No", "Yes")) +
  xlab("Buyer connected") + ylab("Purchased with delay") + 
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=16, face="bold")) +
  theme(legend.text=element_text(size=12)) +
  annotate("text", label = "E", x = -.22, y = .17, size=5)  +
  annotate("text", label = "F", x = .22, y = .19, size=5)  +
  annotate("text", label = "G", x = .78, y = .17, size=5)  +
  annotate("text", label = "H", x = 1.22, y = .27, size=5)  

#model for difference tests
r4 <- lm(subscription_trust ~ pool_T2*any_connection  +  
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T2,
         data=data)
summary(r4) #see pool_T2 and pool_T2:any_connection for 2nd and 3rd reported tests (F-E and diff-in-diff, respectively)
summary(glht(r4, linfct = c("pool_T2 + pool_T2:any_connection = 0"))) #for 1st reported test (H-G)

# Table 6: Treatment effects on quality measures --------------------------

#Models and group means

quality_m1 <- lm(seller_quality ~ pool_T1 + pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
quality_m2 <- lm(seller_quality ~ pool_T1*pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
group_by(data, pool_T1, pool_T2) %>% 
  summarise(avg = mean(seller_quality),
            stddev = sd(seller_quality))  %>% .[1,]

quality_m3 <- lm(seller_trust ~ pool_T1 + pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
quality_m4 <- lm(seller_trust ~ pool_T1*pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
group_by(data, pool_T1, pool_T2) %>% 
  summarise(avg = mean(seller_trust),
            stddev = sd(seller_trust)) %>% .[1,]

quality_m5 <- lm(`enum_questions:follow_up_questions` ~ pool_T1 + pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
quality_m6 <- lm(`enum_questions:follow_up_questions` ~ pool_T1*pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
group_by(data, pool_T1, pool_T2) %>% 
  summarise(avg = mean(`enum_questions:follow_up_questions`),
            stddev = sd(`enum_questions:follow_up_questions`)) %>% .[1,]


quality_m7 <- lm(`enum_questions:follow_up_polite` ~ pool_T1 + pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
quality_m8 <- lm(`enum_questions:follow_up_polite` ~ pool_T1*pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
group_by(data, pool_T1, pool_T2) %>% 
  summarise(avg = mean(`enum_questions:follow_up_polite`),
            stddev = sd(`enum_questions:follow_up_polite`)) %>% .[1,]


quality_m9 <- lm(`enum_questions:follow_up_suspicious` ~ pool_T1 + pool_T2 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student +
                   coethnicreligion*pool_T1 +
                   coethnicreligion*pool_T2, 
                 data=data)
quality_m10 <- lm(`enum_questions:follow_up_suspicious` ~ pool_T1*pool_T2 +  
                    as.factor(enum) +
                    as.factor(blockid) + 
                    descriptives.age + 
                    gender + 
                    as.factor(educ_level) + 
                    employed + 
                    student +
                    coethnicreligion*pool_T1 +
                    coethnicreligion*pool_T2, 
                  data=data)
group_by(data, pool_T1, pool_T2) %>% 
  summarise(avg = mean(`enum_questions:follow_up_suspicious`),
            stddev = sd(`enum_questions:follow_up_suspicious`)) %>% .[1,]


stargazer(quality_m1, quality_m2, #Buyers's perceived quality of seller
          quality_m3, quality_m4, #Buyer's trust of seller
          quality_m5, quality_m6, #Buyer asked lots of followup questions,
          quality_m7, quality_m8, #Buyer's politeness
          quality_m9, quality_m10, #Buyer's suspicion
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_level", 
                   "employed","student", "coethnicreligion"),
          float=F,
          df = F)




### APPENDIX RESULTS ###

# Table D1: Average Treatment Effects (outcome coded 0 to 3) ---------------
zerotothree1 <- lm(subscription ~ T1*T2 + T1*T3  + 
                     as.factor(enum) +
                     as.factor(blockid) + 
                     descriptives.age + 
                     gender + 
                     as.factor(educ_level) +
                     employed +
                     student +
                     coethnicreligion*T1 +
                     coethnicreligion*T2 +
                     coethnicreligion*T3,
                   data=data)

zerotothree2 <- lm(subscription ~ T1 + T2 + T3   + 
                     as.factor(enum) +
                     as.factor(blockid) + 
                     descriptives.age + 
                     gender + 
                     as.factor(educ_level) +
                     employed +
                     student +
                     coethnicreligion*T1 +
                     coethnicreligion*T2 +
                     coethnicreligion*T3,
                   data=data)

zerotothree3 <- lm(subscription ~ pool_T1*pool_T2 + 
                     as.factor(enum) +
                     as.factor(blockid) + 
                     descriptives.age + 
                     gender + 
                     as.factor(educ_level) + 
                     employed + 
                     student +
                     coethnicreligion*pool_T1 +
                     coethnicreligion*pool_T2,
                   data=data)
zerotothree4 <- lm(subscription ~ pool_T1 + pool_T2 + 
                     as.factor(enum) +
                     as.factor(blockid) + 
                     descriptives.age + 
                     gender + 
                     as.factor(educ_level) + 
                     employed + 
                     student +
                     coethnicreligion*pool_T1 +
                     coethnicreligion*pool_T2,
                   data=data)

stargazer(zerotothree1, zerotothree2, zerotothree3, zerotothree4,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_rescaled", 
                   "employed","student", "coethnicreligion"),
          #star.cutoffs = c(0.2, 0.1, 0.02), #Uncomment to obtain one-sided tests for T1/pool_T1 
          float=F,
          df=F)


# Figure D2 (a) -----------------------------------------------------------
avg_by_dyad <- data[data$missing_endline==0,] %>% group_by(any_connection, pool_T1) %>% 
  summarise(mean(subscription),
            SD = sd(subscription),
            n = n(),
            SE = sd(subscription)/sqrt(n))
dyad <- filter(avg_by_dyad, is.na(any_connection)==F)
dyad$pool_T1 <- as.character(dyad$pool_T1)
names(dyad)[3] <- "Subscription"


dyad %>% ggplot(aes(any_connection, Subscription)) +
  theme_bw() + 
  #theme(panel.grid = element_blank()) +
  coord_cartesian(ylim=c(0, .8)) + 
  scale_fill_manual(values = c("grey80", "grey20"),
                    name = "Treatment",
                    labels = c("No signal", "Political connection \nsignaled")) +
  geom_bar(aes(fill = as.factor(pool_T1)), stat="identity", position = "dodge") +
  scale_x_continuous(breaks=c(0,1), labels=c("No", "Yes")) +
  xlab("Buyer connected") + ylab("Subscription purchased") + 
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=16, face="bold")) +
  theme(legend.text=element_text(size=12))  +
  annotate("text", label = "A", x = -.22, y = .54, size=5)  +
  annotate("text", label = "B", x = .22, y = .52, size=5)  +
  annotate("text", label = "C", x = .78, y = .68, size=5)  +
  annotate("text", label = "D", x = 1.22, y = .58, size=5) 

# Difference tests
ar1 <- lm(subscription ~ pool_T1*any_connection + 
            as.factor(enum) +
            as.factor(blockid) + 
            descriptives.age + 
            gender + 
            as.factor(educ_level) + 
            employed + 
            student +
            coethnicreligion*pool_T1,
          data=data)
summary(ar1)
summary(glht(ar1, linfct = c("pool_T1 + pool_T1:any_connection >= 0"))) 


# Figure D2 (b) -----------------------------------------------------------

avg_by_dyad5 <- data %>% group_by(any_connection, pool_T2) %>% 
  summarise(mean(subscription),
            SD = sd(subscription),
            n = n(),
            SE = sd(subscription)/sqrt(n))
dyad5 <- filter(avg_by_dyad5, any_connection==0 | any_connection==1)
dyad5$pool_T2 <- as.character(dyad5$pool_T2)
names(dyad5)[3] <- "subscription"

dyad5 %>% ggplot(aes(any_connection, subscription)) +
  theme_bw() + 
  #theme(panel.grid = element_blank()) +
  coord_cartesian(ylim=c(0, .8)) + 
  scale_fill_manual(values = c("grey80", "grey20"),
                    name = "Treatment",
                    labels = c("No contract", "Formal contract")) +
  geom_bar(aes(fill = as.factor(pool_T2)), stat="identity", position = "dodge") +
  scale_x_continuous(breaks=c(0,1), labels=c("No", "Yes")) +
  xlab("Buyer connected") + ylab("Subscription purchased") + 
  theme(legend.title = element_text(colour="black", size=14)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=16, face="bold")) +
  theme(legend.text=element_text(size=12)) +
  annotate("text", label = "E", x = -.22, y = .55, size=5)  +
  annotate("text", label = "F", x = .22, y = .52, size=5)  +
  annotate("text", label = "G", x = .78, y = .55, size=5)  +
  annotate("text", label = "H", x = 1.22, y = .8, size=5)  

# Difference tests
ar2 <-  lm(subscription ~ pool_T2*any_connection + 
             as.factor(enum) +
             as.factor(blockid) + 
             descriptives.age + 
             gender + 
             as.factor(educ_level) + 
             employed + 
             student +
             coethnicreligion*pool_T2,
           data=data)
summary(ar2)
summary(glht(ar2, linfct = c("pool_T2 + pool_T2:any_connection = 0")))


# Table E2: Balance ----------------------------------------------------------

#Gender
bal_gender <- lm(gender~ as.factor(treatment_group), data = data)
bal_gender_transpose <- t(coef(summary(bal_gender))[, 1:2])
stargazer(bal_gender_transpose)
linearHypothesis(bal_gender, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")

#Education
bal_educ_level <- lm(educ_level~ as.factor(treatment_group), data = data)
bal_educ_level_transpose <- t(coef(summary(bal_educ_level))[, 1:2])
stargazer(bal_educ_level_transpose)
linearHypothesis(bal_educ_level, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")

#Age
bal_descriptives.age <- lm(descriptives.age~ as.factor(treatment_group), data = data)
bal_descriptives.age_transpose <- t(coef(summary(bal_descriptives.age))[, 1:2])
stargazer(bal_descriptives.age_transpose)
linearHypothesis(bal_descriptives.age, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")

#Employed
bal_employed <- lm(employed~ as.factor(treatment_group), data = data)
bal_employed_transpose <- t(coef(summary(bal_employed))[, 1:2])
stargazer(bal_employed_transpose)
linearHypothesis(bal_employed, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")

#Student
bal_student <- lm(student~ as.factor(treatment_group), data = data)
bal_student_transpose <- t(coef(summary(bal_student))[, 1:2])
stargazer(bal_student_transpose)
linearHypothesis(bal_student, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")

#Trust councils
data$trust_mairie[data$trust_mairie==98|data$trust_mairie==99|is.na(data$trust_mairie)==T] <- mean(data$trust_mairie[data$trust_mairie<20], na.rm=T)
bal_trust_mairie <- lm(trust_mairie~ as.factor(treatment_group), data = data)
bal_trust_mairie_transpose <- t(coef(summary(bal_trust_mairie))[, 1:2])
stargazer(bal_trust_mairie_transpose)
linearHypothesis(bal_trust_mairie, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")

#Trust courts
data$trust_courts[data$trust_courts==98|data$trust_courts==99|is.na(data$trust_courts)==T] <- mean(data$trust_courts[data$trust_courts<20], na.rm=T)
bal_trust_courts <- lm(trust_courts~ as.factor(treatment_group), data = data)
bal_trust_courts_transpose <- t(coef(summary(bal_trust_courts))[, 1:2])
stargazer(bal_trust_courts_transpose)
linearHypothesis(bal_trust_courts, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")

#Have used courts
data$court_use[data$court_use==98|data$court_use==99|is.na(data$court_use)==T] <- mean(data$court_use[data$court_use<20], na.rm=T)
bal_court_use <- lm(court_use~ as.factor(treatment_group), data = data)
bal_court_use_transpose <- t(coef(summary(bal_court_use))[, 1:2])
stargazer(bal_court_use_transpose)
linearHypothesis(bal_court_use, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")


#Coethnicity
bal_coeth <- lm(coethnicreligion~ as.factor(treatment_group), data = data)
bal_coeth_transpose <- t(coef(summary(bal_coeth))[, 1:2])
stargazer(bal_coeth_transpose)
linearHypothesis(bal_coeth, "as.factor(treatment_group)2-as.factor(treatment_group)3-as.factor(treatment_group)4-as.factor(treatment_group)5-as.factor(treatment_group)6=0")



# Table F3: Summary stats for control variables ---------------------------

varlist <-c("descriptives.age", "gender", "employed", 
            "student", "educ_level", "coethnicreligion")
stargazer(
  data[,varlist], 
  summary.stat = c("n","mean", "sd", "min", "median", "max")
)


# Table G4: Attrition as predicted by treatment arm -----------------------

attrition <- lm(attrit~T1+T2+T3 +  
                  as.factor(enum) +
                  as.factor(blockid) + 
                  descriptives.age + 
                  gender + 
                  as.factor(educ_level) + 
                  employed + 
                  student +
                  coethnicreligion*T1 +
                  coethnicreligion*T2 +
                  coethnicreligion*T3, 
                data =data)
stargazer(attrition, 
          omit = c("blockid", "trans.enum", "age", "gender", "educ_rescaled", 
                   "employed","student", "coethnicreligion", "enum")
)

#group means
data %>% group_by(T1,T2,T3) %>% 
  summarise(mean(attrit),
            SD = sd(attrit),
            n = n(),
            SE = sd(attrit)/sqrt(n))


# Table H5: Contract choice as predicted by treatment ---------------------

choice <- lm(chose_contract ~ pool_T1*any_connection +  
               descriptives.age + 
               gender + 
               as.factor(educ_level) + 
               employed + 
               student +
               coethnicreligion*pool_T1, 
             data=data)
stargazer(choice,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_rescaled", 
                   "employed","student", "coethnicreligion"),
          float=F,
          df=F)
data %>% group_by(pool_T1, any_connection) %>%
  summarise(mean(chose_contract, na.rm=T), sd(chose_contract, na.rm=T))


# Table I6: Corresponding model output for Figures 5 and 6 ----------------

r1 <- lm(purchased ~ pool_T1*any_connection +  
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1,
         data=data)

r2 <- lm(subscription_trust ~ pool_T1*any_connection +  
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1,
         data=data)

r3 <- lm(purchased ~ pool_T2*any_connection  +  
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T2,
         data=data)

r4 <- lm(subscription_trust ~ pool_T2*any_connection  +  
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T2,
         data=data)

stargazer(r1, r2, r3, r4,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_rescaled", 
                   "employed","student", "coethnicreligion"),
          float=F,
          df=F)

# Treatment effect among connected buyers
summary(glht(r1, linfct = c("pool_T1 + pool_T1:any_connection >= 0"))) 
summary(glht(r2, linfct = c("pool_T1 + pool_T1:any_connection >= 0"))) 
summary(glht(r3, linfct = c("pool_T2 + pool_T2:any_connection = 0"))) 
summary(glht(r4, linfct = c("pool_T2 + pool_T2:any_connection = 0"))) 


# Figure J3: Trust in courts across Africa --------------------------------

#Load Afrobarometer data directly from  website (internet connection required)
ab <- read_spss("https://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav")

#if Afrobarometer data is instead saved locally, don't run the above line;
  #rather, uncomment and executive the following line:
#ab <- read_spss("merged_r6_data_2016_36countries2.sav")

ab <- ab %>% dplyr::select("COUNTRY", "Q52J")
ab$COUNTRY <- as_label(ab$COUNTRY)
ab$name <- haven::as_factor(ab$COUNTRY)

#removing non-answers/don't knows/etc.
ab$Q52J[ab$Q52J==-1 | ab$Q52J==9 | ab$Q52J==99] <- NA

means_by_country <- ab%>% group_by(name) %>% summarise(mean = mean(Q52J, na.rm=T))

#ordering by outcome and making graph
means_by_country <- means_by_country[order(means_by_country$mean),]
means_by_country$name <- factor(means_by_country$name, levels=means_by_country$name)
means_by_country <- means_by_country %>% mutate(ToHighlight = ifelse(name == "Senegal", "yes", "no"))
means_by_country %>% ggplot(aes(x=factor(name), y=mean, fill=ToHighlight)) +
  geom_bar(stat="identity") + coord_flip() + theme_bw() +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=10)) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=14, face="bold"),
        legend.text=element_text(size=14)) +
  ylab("Trust in courts of law") +
  xlab("") +
  scale_y_continuous(limits = c(0,3), expand = c(0, 0)) +
  scale_fill_manual(values = c( "yes"="tomato", "no"="gray" ), guide = FALSE)



# Table K7: Correlation ---------------------------------------------------

corr_vars <- data %>% dplyr::select(any_connection, gender, educ_level, employed, descriptives.age, student)

cors <- cor(corr_vars)
stargazer(cors, summary=F, float=F)



# Table L8: Effects by in-group -------------------------------------------
m2 <- lm(purchased ~ pool_T1*pool_T2 + 
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1 +
           coethnicreligion*pool_T2,
         data=data)

m4 <- lm(subscription_trust ~ pool_T1*pool_T2 + 
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1 +
           coethnicreligion*pool_T2,
         data=data)

stargazer(m2, m4,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_rescaled", 
                   "employed","student"), 
          #star.cutoffs = c(0.2, 0.1, 0.02), #uncomment for one-sided test of T1/pool_T1 effect
          float=F,
          df=F)
#control means
data %>% group_by(pool_T1, pool_T2, coethnicreligion) %>%
  summarise(mean(purchased),
            sd(purchased)) %>% .[1,]
data %>% group_by(pool_T1, pool_T2, coethnicreligion) %>%
  summarise(mean(subscription_trust),
            sd(subscription_trust)) %>% .[1,]



# Table M9: Testing findability via district match ------------------------
d2 <- lm(purchased ~ pool_T1*pool_T2*district_match + 
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1 +
           coethnicreligion*pool_T2,
         data=data)
d4 <- lm(subscription_trust ~ pool_T1*pool_T2*district_match + 
           as.factor(enum) +
           as.factor(blockid) + 
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student +
           coethnicreligion*pool_T1 +
           coethnicreligion*pool_T2,
         data=data)
stargazer(d2, d4,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_level", 
                   "employed","student", "coethnicreligion"),
          #star.cutoffs = c(0.2, 0.1, 0.02), #uncomment for one-sided test of T1/pool_T1 effect
          float=F,
          df=F)
data %>% group_by(pool_T1, pool_T2, district_match) %>%
  summarise(mean(purchased),
            sd(purchased)) %>% .[1,] 
data %>% group_by(pool_T1, pool_T2, district_match) %>%
  summarise(mean(subscription_trust),
            sd(subscription_trust)) %>% .[1,]


# Table N10: Buyer suspicion as predicted by treatment --------------------

data$skepticism_buyer[data$skepticism_buyer==98|is.na(data$skepticism_buyer)==T] <- mean(data$skepticism_buyer[data$skepticism_buyer<10], na.rm=T)

skepticism_t1 <- lm(skepticism_buyer ~ pool_T1 +  
                      as.factor(enum) +
                      as.factor(blockid) + 
                      descriptives.age + 
                      gender + 
                      as.factor(educ_level) + 
                      employed + 
                      student, 
                    data=data)
skepticism_t3 <- lm(skepticism_buyer ~ T3 +  
                      as.factor(enum) +
                      as.factor(blockid) + 
                      descriptives.age + 
                      gender + 
                      as.factor(educ_level) + 
                      employed + 
                      student, 
                    data=data)

data %>% group_by(pool_T1) %>% 
  summarise(mean(skepticism_buyer),
            SD = sd(skepticism_buyer)) %>% .[1,]
data %>% group_by(T3) %>% 
  summarise(mean(skepticism_buyer),
            SD = sd(skepticism_buyer)) %>% .[1,]

stargazer(skepticism_t1,skepticism_t3,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_level", 
                   "employed","student", "coethnicreligion"),
          add.lines = list(c("Control outcome mean", "0.75", "0.75"),
                           c("Control outcome std. dev.", "1.21", "1.23"),
                           c("Fixed effects", "Yes"),
                           c("Controls", "Yes")),
          float=F,
          df = F)


# Table O11: Buyers' connections as predicted by treatment ----------------
overreport <- lm(any_connection ~ pool_T1 +  
                   as.factor(enum) +
                   as.factor(blockid) + 
                   descriptives.age + 
                   gender + 
                   as.factor(educ_level) + 
                   employed + 
                   student, 
                 data=data)

data %>% group_by(pool_T1) %>% 
  summarise(mean(any_connection),
            SD = sd(any_connection),
            n = n(),
            SE = sd(any_connection)/sqrt(n))
stargazer(overreport,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_level", 
                   "employed","student", "coethnicreligion"),
          add.lines = list(c("Control outcome mean", "0.510"),
                           c("Control outcome std. dev.", "0.493"),
                           c("Fixed effects", "Yes"),
                           c("Controls", "Yes")),
          float=F,
          df = F)



# Table S12: Multinomial results ------------------------------------------

#format outcome as categorical variable
data <- data %>% mutate(subscription_categorical = as.factor(subscription)) 

multinom1 <- multinom(subscription_categorical ~ pool_T1*pool_T2  +as.factor(enum) +
                        as.factor(blockid) + descriptives.age + gender + as.factor(educ_level) + employed + student +
                        coethnicreligion*pool_T1 +
                        coethnicreligion*pool_T2, data = data, MaxNWts = 1100)
multinom1.rrr = exp(coef(multinom1))

multinom2 <- multinom(subscription_categorical ~ pool_T1*any_connection +  as.factor(enum) +
                        + as.factor(blockid) + 
                        descriptives.age + gender + employed + student + 
                        as.factor(educ_level) + 
                        coethnicreligion*pool_T1,
                      data=data, MaxNWts = 1100)
multinom2.rrr = exp(coef(multinom2))

multinom3 <- multinom(subscription_categorical ~ pool_T2*any_connection +  as.factor(enum) +
                        + as.factor(blockid) + 
                        descriptives.age + gender + employed + student + 
                        as.factor(educ_level) + 
                        coethnicreligion*pool_T2,
                      data=data, MaxNWts = 1100)
multinom3.rrr = exp(coef(multinom3))

stargazer(multinom1, multinom2, multinom3,
          coef=list(multinom1.rrr,
                    multinom2.rrr,
                    multinom3.rrr), 
          p.auto = F, omit = c("blockid", "trans.enum", "as.factor", "age", "gender", 
                               "employed","student", "coethnicreligion", "educ_level"),
          #star.cutoffs = c(0.2, 0.1, 0.02), #uncomment for pool_T1
          float=F,
          df=F)

##Treatment effects of just politically connected buyers
multinom6 <- multinom(subscription_categorical ~ pool_T1 +  as.factor(enum) +
                        as.factor(blockid) + 
                        descriptives.age + gender + employed + student + 
                        as.factor(educ_level) + 
                        coethnicreligion*pool_T1,
                      data=data[data$any_connection==1,], MaxNWts = 1100)
multinom6.rrr = exp(coef(multinom6))

multinom7 <- multinom(subscription_categorical ~ pool_T2 +  as.factor(enum) +
                        as.factor(blockid) + 
                        descriptives.age + gender + employed + student + 
                        as.factor(educ_level) + 
                        coethnicreligion*pool_T2,
                      data=data[data$any_connection==1,], MaxNWts = 1100)
multinom7.rrr = exp(coef(multinom7))

stargazer(multinom6, multinom7,
          coef=list(multinom6.rrr,
                    multinom7.rrr), 
          p.auto = F, omit = c("blockid", "trans.enum", "as.factor", "age", "gender", 
                               "employed","student", "coethnicreligion", "educ_level"),
          #star.cutoffs = c(0.2, 0.1, 0.02), #uncomment for pool_T1
          float=F,
          df=F)


# Table T13: Belief in sellers' connections by buyers' connections --------

unconnected_buyers_more_likely <- lm(thinks_seller_is_connected ~ any_connection*pool_T1 + 
                                       as.factor(enum) +
                                       as.factor(blockid) +
                                       descriptives.age + 
                                       gender + 
                                       as.factor(educ_level) + 
                                       employed + 
                                       student + 
                                       coethnicreligion*pool_T1, 
                                     data=data)

unconnected_buyers_more_likely_sum <- data %>% group_by(any_connection, pool_T1) %>% 
  summarise(mean(thinks_seller_is_connected),
            sd(thinks_seller_is_connected))


stargazer(unconnected_buyers_more_likely,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_level", 
                   "employed","student", "coethnicreligion"),
          add.lines = list(c("Control outcome mean", 
                             paste(round(unconnected_buyers_more_likely_sum[1,3], 3))),
                           c("Control outcome std. dev.", 
                             paste(round(unconnected_buyers_more_likely_sum[1,4], 3))),
                           c("Fixed effects", "Yes"),
                           c("Controls", "Yes")),
          float=F,
          #star.cutoffs = c(0.2, 0.1, 0.02), 
          df = F)


# Table U14: Connected buyers more likely to seek recourse ----------------

reg_would_do_something <- lm(would_have_done_something ~ any_connection +
                               descriptives.age + 
                               gender + 
                               as.factor(educ_level) + 
                               employed + 
                               student, 
                             data=data)
reg_would_do_something_sum <- data %>% group_by(any_connection) %>% 
  summarise(mean(would_have_done_something), 
            sd(would_have_done_something),
            n()) 

reg_would_hypothetically_use_formal_enforcement <- lm(would_hypothetically_use_formal_enforcement ~ any_connection +
                                                        descriptives.age + 
                                                        gender + 
                                                        as.factor(educ_level) + 
                                                        employed + 
                                                        student, 
                                                      data=data)

reg_would_hypothetically_use_formal_enforcement_sum <- data %>% group_by(any_connection) %>% 
  summarise(mean(would_hypothetically_use_formal_enforcement),
            sd(would_hypothetically_use_formal_enforcement),
            n()) 

r3 <- lm(court_use ~ any_connection +
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student, data = data)
r4 <- lm(police_use ~ any_connection +
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student, data = data)
r5 <- lm(lawyer_fix ~ any_connection +
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student, data = data)
r6 <- lm(police_fix ~ any_connection +
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student, data = data)
r7 <- lm(court_fix ~ any_connection +
           descriptives.age + 
           gender + 
           as.factor(educ_level) + 
           employed + 
           student, data = data)

r3_sum <- data %>% group_by(any_connection) %>% summarise(mean(court_use, na.rm=T), 
                                                          sd(court_use, na.rm=T))
r4_sum <- data %>% group_by(any_connection) %>% summarise(mean(police_use, na.rm=T), 
                                                          sd(police_use, na.rm=T))
r5_sum <- data %>% group_by(any_connection) %>% summarise(mean(lawyer_fix, na.rm=T), 
                                                          sd(lawyer_fix, na.rm=T))
r6_sum <- data %>% group_by(any_connection) %>% summarise(mean(police_fix, na.rm=T), 
                                                          sd(police_fix, na.rm=T))
r7_sum <- data %>% group_by(any_connection) %>% summarise(mean(court_fix, na.rm=T), 
                                                          sd(court_fix, na.rm=T))
stargazer(reg_would_do_something, reg_would_hypothetically_use_formal_enforcement, r3, r4, r5, r6, r7,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "educ_level", 
                   "employed","student", "coethnicreligion"),
          add.lines = list(c("Control outcome mean", 
                             paste(round(reg_would_do_something_sum[1,2], 3)),
                             paste(round(reg_would_hypothetically_use_formal_enforcement_sum[1,2], 3)),
                             paste(round(r3_sum[1,2], 3)),
                             paste(round(r4_sum[1,2], 3)),
                             paste(round(r5_sum[1,2], 3)),
                             paste(round(r6_sum[1,2], 3)),
                             paste(round(r7_sum[1,2], 3))),
                           c("Control outcome std. dev.", 
                             paste(round(reg_would_do_something_sum[1,3], 3)),
                             paste(round(reg_would_hypothetically_use_formal_enforcement_sum[1,3], 3)),
                             paste(round(r3_sum[1,3], 3)),
                             paste(round(r4_sum[1,3], 3)),
                             paste(round(r5_sum[1,3], 3)),
                             paste(round(r6_sum[1,3], 3)),
                             paste(round(r7_sum[1,3], 3))),
                           c("Fixed effects", "No", "No", "No", "No", "No", "No", "No"),
                           c("Controls", "No", "No", "No", "No", "No", "No", "No")),
          float=F,
          df = F)


# Table V15: Interactive controls -----------------------------------------

# Education 
#demeaning education variable
data <- data %>% mutate(educ_level_demeaned = educ_level - mean(data$educ_level))


#1a.
orig1 <- lm(purchased ~ pool_T1*any_connection + educ_level + 
              as.factor(enum) + as.factor(blockid) + 
              descriptives.age + gender +  employed + student +
              coethnicreligion*pool_T1, data = data)
summary(glht(orig1, linfct = c("pool_T1 + pool_T1:any_connection >= 0")))
#1b.
orig2 <- lm(purchased ~ pool_T2*any_connection + educ_level + 
              as.factor(enum) + as.factor(blockid) + 
              descriptives.age + gender +  employed + student +
              coethnicreligion*pool_T2, data = data)
summary(glht(orig2, linfct = c("pool_T2 + pool_T2:any_connection = 0")))
#to
#2a
demean1 <- lm(purchased ~ pool_T1*any_connection + pool_T1*educ_level_demeaned + 
                as.factor(enum) + as.factor(blockid) + 
                descriptives.age + gender +  employed + student +
                coethnicreligion*pool_T1, data = data)
summary(glht(demean1, linfct = c("pool_T1 + pool_T1:any_connection >= 0")))

#2c
demean2 <- lm(purchased ~ pool_T2*any_connection + pool_T2*educ_level_demeaned + 
                as.factor(enum) + as.factor(blockid) + 
                descriptives.age + gender +  employed + student +
                coethnicreligion*pool_T2, data = data)
summary(glht(demean2, linfct = c("pool_T2 + pool_T2:any_connection = 0")))

# Employment 
#demeaning employed variable
data <- data %>% mutate(employed_demeaned = employed - mean(data$employed))

#3a.
orig3 <- lm(purchased ~ pool_T1*any_connection + employed + 
              as.factor(enum) + as.factor(blockid) + as.factor(educ_level) +
              descriptives.age + gender  + student +
              coethnicreligion*pool_T1, data = data)
summary(glht(orig3, linfct = c("pool_T1 + pool_T1:any_connection >= 0")))
#3b.
orig4 <- lm(purchased ~ pool_T2*any_connection + employed + 
              as.factor(enum) + as.factor(blockid) + as.factor(educ_level) +
              descriptives.age + gender  + student +
              coethnicreligion*pool_T2, data = data)
summary(glht(orig4, linfct = c("pool_T2 + pool_T2:any_connection = 0")))
#to
#4a
demean3 <- lm(purchased ~ pool_T1*any_connection + pool_T1*employed_demeaned + 
                as.factor(enum) + as.factor(blockid) + as.factor(educ_level) +
                descriptives.age + gender  + student +
                coethnicreligion*pool_T1, data = data)
summary(glht(demean3, linfct = c("pool_T1 + pool_T1:any_connection >= 0")))

#4c
demean4 <- lm(purchased ~ pool_T2*any_connection + pool_T2*employed_demeaned + 
                as.factor(enum) + as.factor(blockid) + as.factor(educ_level) +
                descriptives.age + gender  + student +
                coethnicreligion*pool_T2, data = data)
summary(glht(demean4, linfct = c("pool_T2 + pool_T2:any_connection = 0")))

#means and sds for table
data %>% group_by(pool_T1, any_connection) %>% summarise(mean(purchased),
                                                         sd(purchased))
data %>% group_by(pool_T2, any_connection) %>% summarise(mean(purchased),
                                                         sd(purchased))

stargazer(orig1, orig2, demean1, demean2,
          orig3, orig4, demean3, demean4,
          omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "education", 
                   "educ_level", "educ_level_demeaned", "employed_demeaned",
                   "employed","student", "coethnicreligion"),
          add.lines = list(c("Control outcome mean", 
                             0.289, 0.289, 0.290, 0.290,0.289, 0.289, 0.290, 0.290),
                           c("Control outcome std. dev.", 
                             0.454, 0.454, 0.454, 0.454,0.454, 0.454, 0.454, 0.454),
                           c("Fixed effects", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes"),
                           c("Controls", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes")),
          float=F,
          df = F)


# Table W16: Probit -------------------------------------------------------

#ATEs
probit1 <- glm(purchased ~ pool_T1*pool_T2  +  as.factor(enum)  +  
                 as.factor(enum) + as.factor(blockid) + descriptives.age + 
                 gender + as.factor(educ_level) + employed + student +
                 coethnicreligion*pool_T1 +
                 coethnicreligion*pool_T2,
               family = binomial(link = "probit"), data = data)

probit2 <- glm(subscription_trust ~ pool_T1*pool_T2  +  
                 as.factor(enum) + as.factor(blockid) + descriptives.age + 
                 gender + as.factor(educ_level) + employed + student +
                 coethnicreligion*pool_T1 +
                 coethnicreligion*pool_T2,
               family = binomial(link = "probit"), data = data)

#Political connection treatment by buyers' connections
probit3 <- glm(purchased ~ pool_T1*any_connection  +  
                 as.factor(enum) + as.factor(blockid) + descriptives.age + 
                 gender + as.factor(educ_level) + employed + student +
                 coethnicreligion*pool_T1,
               family = binomial(link = "probit"), data = data)

probit4 <- glm(subscription_trust ~ pool_T1*any_connection  +  
                 as.factor(enum) + as.factor(blockid) + descriptives.age + 
                 gender + as.factor(educ_level) + employed + student +
                 coethnicreligion*pool_T1,
               family = binomial(link = "probit"), data = data)

#Formal contract treatment by buyers' connections
probit5 <- glm(purchased ~ pool_T2*any_connection  +  
                 as.factor(enum) + as.factor(blockid) + descriptives.age + 
                 gender + as.factor(educ_level) + employed + student +
                 coethnicreligion*pool_T2,
               family = binomial(link = "probit"), data = data)

probit6 <- glm(subscription_trust ~ pool_T2*any_connection  +  
                 as.factor(enum) + as.factor(blockid) + descriptives.age + 
                 gender + as.factor(educ_level) + employed + student +
                 coethnicreligion*pool_T2,
               family = binomial(link = "probit"), data = data)

stargazer(stargazer(probit1, probit2, probit3, probit4, probit5, probit6,
                    omit = c("blockid", "trans.enum", "as.factor", "age", "gender", "education", 
                             "employed","student", "coethnicreligion", "educ_level"),
                    #star.cutoffs = c(0.2, 0.1, 0.02), #uncomment for pool_T1
                    float=F, 
                    df=F))

#Difference tests for Panel B
summary(glht(probit3, linfct = c("pool_T1 + pool_T1:any_connection >= 0")))
summary(glht(probit4, linfct = c("pool_T1 + pool_T1:any_connection >= 0")))
summary(glht(probit5, linfct = c("pool_T2 + pool_T2:any_connection = 0")))
summary(glht(probit6, linfct = c("pool_T2 + pool_T2:any_connection = 0")))







