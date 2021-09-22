library(tidyverse) #loading packages
library(haven)
library(dplyr)
setwd("/Users/aveniaghazarian/Documents/Task4")

cohort2030<-load("Cohort2030.RData")
Ak91_clean<-load("Ak91_clean.RData")


########### Regression Formulas ###########
year<-paste0("YR", 20:28,collapse = "+")
qtr1<-paste0("QTR", 120:129, collapse = "+")
qtr2<-paste0("QTR", 220:229, collapse = "+")
qtr3<-paste0("QTR", 320:329, collapse = "+")
qtr<-paste(qtr1,qtr2,qtr3, sep="+",collapse = "+")

(fml1 <- as.formula(paste("LWKLYWGE~ EDUC+", paste(year, collapse= "+"))))
(fml3 <- as.formula(paste("LWKLYWGE~ EDUC+AGEQ+AGEQSQ+", paste(year, collapse= "+"))))
(fml5 <- as.formula(paste("LWKLYWGE~ EDUC+RACE+MARRIED+SMSA+NEWENG+MIDATL+ENOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT+", paste(year, collapse= "+"))))
(fml7 <- as.formula(paste("LWKLYWGE~ EDUC+RACE+MARRIED+SMSA+NEWENG+MIDATL+ENOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT+AGEQ+AGEQSQ+", paste(year, collapse= "+"))))

(fml2 <- as.formula(paste("LWKLYWGE~EDUC+", paste0(year, collapse= "+") , "|" , qtr ,"+", paste(year, collapse= "+"))))
(fml4 <- as.formula(paste("LWKLYWGE~EDUC+AGEQ+AGEQSQ+", paste0(year, collapse= "+") , "|" , qtr ,"+", paste(year, collapse= "+"),"+AGEQ+AGEQSQ")))
(fml6 <- as.formula(paste("LWKLYWGE~EDUC+RACE+MARRIED+SMSA+NEWENG+MIDATL+ENOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT+", paste0(year, collapse= "+") , "|" , qtr ,"+", paste(year, collapse= "+"), "+RACE+MARRIED+SMSA+NEWENG+MIDATL+ENOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT")))
(fml8 <- as.formula(paste("LWKLYWGE~EDUC+RACE+MARRIED+SMSA+NEWENG+MIDATL+ENOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT+AGEQ+AGEQSQ+", paste0(year, collapse= "+") , "|" , qtr , "+RACE+MARRIED+SMSA+NEWENG+MIDATL+ENOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT+AGEQ+AGEQSQ+", paste(year, collapse= "+"))))

col1<-lm(fml1, data=Cohort2030)
col3<-lm(fml3, data=Cohort2030)
col5<-lm(fml5, data=Cohort2030)
col7<-lm(fml7, data=Cohort2030)

col2<-ivreg(fml2, data=Cohort2030)
col4<-ivreg(fml4, data=Cohort2030)
col6<-ivreg(fml6, data=Cohort2030)
col8<-ivreg(fml8, data=Cohort2030)

#There is apparently something wrong with col4 and col8 but I couldn't figure out what!
stargazer(col1, col2, col3, col5, col6, col7, omit=c("NEWENG", "MIDATL", "ENOCENT", "WNOCENT", "SOATL", "ESOCENT", "WSOCENT", "MT", "YR20",
                                                     "YR21", "YR22", "YR23", "YR24", "YR25", "YR26", "YR27", "YR28")  ,title="Table IV",
          add.lines = list(c("9 years of birth dummies", "Yes", "YES", "YES", "YES", "YES", "YES"),
                           c("8 Region of residence dummies", "No","No", "No", "YES", "YES", "YES" )),align= TRUE, no.space = TRUE)


################## Figure V #################

time = seq(30.25, 50, 0.25)
time = as.data.frame(time)
n=nrow(time)
data<-Ak91_clean %>%
  filter(CENSUS == 80 )%>% 
  select(YOB, QOB, LWKLYWGE) %>%
  mutate(YQ=YOB + 0.25 * QOB)%>%
  group_by(YQ) %>% 
  summarise_all(mean)
data<-cbind(data, time)

ggplot(data, aes(x = time, y = LWKLYWGE)) + 
  geom_line() +
  geom_point(aes(shape = as.factor(QOB),fill = factor(QOB)), size = 0.3)+
  xlab("Year of Birth") + 
  ylab("Log Weekly Earnings")


















