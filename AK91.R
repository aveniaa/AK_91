library(tidyverse) #loading packages
library(haven)
library(dplyr)
setwd("/Users/aveniaghazarian/Documents/Task4")


Ak91 <- read_dta(file = "NEW7080.dta")
nrow(Ak91)
Ak91_clean <- Ak91%>% rename(
  AGE	=	v1,
  AGEQ	=	v2,
  EDUC	=	v4,
  ENOCENT	=	v5,
  ESOCENT	=	v6,
  LWKLYWGE	=	v9,
  MARRIED	=	v10,
  MIDATL	=	v11,
  MT	=	v12,
  NEWENG	=	v13,
  CENSUS	=	v16,
  QOB	=	v18,
  RACE	=	v19,
  SMSA	=	v20,
  SOATL	=	v21,
  WNOCENT	=	v24,
  WSOCENT	=	v25,
  YOB	=	v27	
)%>% select(-v8)%>% mutate(COHORT = ifelse(YOB %in% 20:29, 20.29, 
                                           ifelse(YOB %in% 30:39, 30.39, 
                                                  ifelse(YOB %in% 40:49, 40.49,20.29))))

Ak91_clean$AGEQ<-ifelse(Ak91_clean$CENSUS==80, Ak91_clean$AGEQ-1900, Ak91_clean$AGEQ)
Ak91_clean$AGEQSQ<-sqrt(Ak91_clean$AGEQ)
################## Generating YOB Dummies ###################
timee <- seq(2,10,1)

Ak91_clean <- Ak91_clean %>% mutate(YR20 = ifelse(YOB==1920|YOB==30|YOB==40, 1, 0),
                      YR21 = ifelse(YOB==1921|YOB==31|YOB==41, 1, 0),
                      YR22 = ifelse(YOB==1922|YOB==32|YOB==42, 1, 0),
                      YR23 = ifelse(YOB==1923|YOB==33|YOB==43, 1, 0),
                      YR24 = ifelse(YOB==1924|YOB==34|YOB==44, 1, 0),
                      YR25 = ifelse(YOB==1925|YOB==35|YOB==45, 1, 0),
                      YR26 = ifelse(YOB==1926|YOB==36|YOB==46, 1, 0),
                      YR27 = ifelse(YOB==1927|YOB==37|YOB==47, 1, 0),
                      YR28 = ifelse(YOB==1928|YOB==38|YOB==48, 1, 0),
                      YR29 = ifelse(YOB==1929|YOB==39|YOB==49, 1, 0),
                      QTR1 = ifelse(QOB==1, 1, 0),
                      QTR2 = ifelse(QOB==2, 1, 0),
                      QTR3 = ifelse(QOB==3, 1, 0),
                      QTR4 = ifelse(QOB==4, 1, 0),
                      QTR120 = QTR1*YR20,
                      QTR121 = QTR1*YR21,
                      QTR122 = QTR1*YR22,
                      QTR123 = QTR1*YR23,
                      QTR124 = QTR1*YR24,
                      QTR125 = QTR1*YR25,
                      QTR126 = QTR1*YR26,
                      QTR127 = QTR1*YR27,
                      QTR128 = QTR1*YR28,
                      QTR129 = QTR1*YR29,
                      QTR220 = QTR2*YR20,
                      QTR221 = QTR2*YR21,
                      QTR222 = QTR2*YR22,
                      QTR223 = QTR2*YR23,
                      QTR224 = QTR2*YR24,
                      QTR225 = QTR2*YR25,
                      QTR226 = QTR2*YR26,
                      QTR227 = QTR2*YR27,
                      QTR228 = QTR2*YR28,
                      QTR229 = QTR2*YR29,
                      QTR320 = QTR3*YR20,
                      QTR321 = QTR3*YR21,
                      QTR322 = QTR3*YR22,
                      QTR323 = QTR3*YR23,
                      QTR324 = QTR3*YR24,
                      QTR325 = QTR3*YR25,
                      QTR326 = QTR3*YR26,
                      QTR327 = QTR3*YR27,
                      QTR328 = QTR3*YR28,
                      QTR329 = QTR3*YR29)

Cohort2030<-Ak91_clean%>%filter(COHORT<20.30)
nrow(Ak91)
nrow(Cohort2030)
save(Cohort2030, file="Cohort2030.RData")
save(Ak91_clean, file="Ak91_clean.RData")









