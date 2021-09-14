title: "Homework 2"
author: "Adelia Fida"
date: "9/14/2021"

#Study group names (Dawa Lama,Nickolas Alonso & Adelia Fida)

summary(acs2017_ny$DEGFIELD)
summary(acs2017_ny$DEGFIELD)
mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts") ])
mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Engineering") ])
summary(acs2017_ny$DEGFIELD)
mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts") ])
mean(acs2017_ny$RENT[ (acs2017_ny$DEGFIELD == "Fine Arts") ])
sd(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts") ])
var(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts") ])
median(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts") ])
mean(acs2017_ny$HISPAND[ (acs2017_ny$DEGFIELD == "Fine Arts") ])


mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical Sciences") ])
sd(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical Sciences") ])
(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical Sciences" & acs2017_ny$RENT >1000)
])
sub1 <-(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical Sciences" & acs2017_ny$RENT
                         >1000)
])
mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical Sciences" & acs2017_ny$RENT >1000)
])
mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical Sciences") ])
mean(acs2017_ny$FOODSTMP[ (acs2017_ny$DEGFIELD == "Physical Sciences") ])
mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts") ])
age_physci_rentover1000 <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical Sciences" &
                                               acs2017_ny$RENT >1000)
])



finart_hispanic <- (acs2017_ny$DEGFIELD == "Fine Arts"& acs2017_ny$HISPAN)age_physci_rentover1000_HISPAN <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical
Sciences" & acs2017_ny$RENT >1000 & acs2017_ny$Hisp_DomR)
])

cosmetology_culinaryarts <- acs2017_ny$DEGFIELD[(acs2017_ny$DEGFIELD == "Cosmetology
Services and Culinary Arts")]
libart <- acs2017_ny$DEGFIELD[(acs2017_ny$DEGFIELD == "Liberal Arts and Humanities")]
PFPRL <- acs2017_ny$DEGFIELD[(acs2017_ny$DEGFIELD == "Physical Fitness, Parks, Recreation,
and Leisure")]
summary(cosmetology_culinaryarts)
summary(socialscience)
summary(PFPRL)
summary(libart)
acs2017_ny$Covid_risk <- ((acs2017_ny$PUMA > 4600) & acs2017_ny$PUMA < 6000) |
  ((acs2017_ny$IND_number > 8500) & (acs2017_ny$IND_number < 8700))
AGE20_60FAM_RACE <- norm_variable(RACE). (acs2017_ny$DEGFIELD == "Fine Arts")
AGE20_60FAM_EMP <- (EMPSTAT)
#IMPORTANT!
finart_employed_under66 <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts" &
                                               acs2017_ny$AGE < 66 & acs2017_ny$EMPSTAT == 2)])


finart_employed_under66 <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts" &
                                               acs2017_ny$AGE < 66 & acs2017_ny$EMPSTAT == 2)])
finart_employed_under66 <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Fine Arts" &
                                               acs2017_ny$AGE < 66 & acs2017_ny$EMPSTAT == 2)])
acs2017_ny$DEGFIELD[(DEGFIELD == "Fine Arts")&(female)]
neighboorhoods_bronx_finearts_number <- length(which(neighboorhoods_bronx_finearts == 1))
acs2017_ny$Covid_risk <- ((acs2017_ny$IND_number > 4600) & acs2017_ny$IND_number < 6000)
age_physci_rentover1000_HISPAN <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical
Sciences" & acs2017_ny$RENT >1000 & acs2017_ny$Hisp_DomR)
])
#put greate than and less than to check the neigh boor hoods of each region.
female_gender_finearts <- length(which(gender_finearts == 1))race_degfin_ <- (acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Fine Arts") &
                                                                                                  (acs2017_ny$RACED == "White") ])
gender_finearts <- (acs2017_ny$female[ (acs2017_ny$DEGFIELD == "Fine Arts" )])
# first change the factor back into a number
acs2017_ny$DEGFIELD <- as.numeric(levels(acs2017_ny$IND))[acs2017_ny$IND]
acs2017_ny$Covid_risk <- ((acs2017_ny$IND_number > 4600) & acs2017_ny$IND_number < 6000) |
  ((acs2017_ny$IND_number > 8500) & (acs2017_ny$IND_number < 8700))
# then pick certain ranges of numbers
#This the variable below is the same as:
#
gender_finearts <- (acs2017_ny$female[ (acs2017_ny$DEGFIELD == "Fine Arts" )])
gender_finearts22 <- (gender_finearts-min(gender_finearts,na.rm =
                                            TRUE))/(max(gender_finearts,na.rm = TRUE)) - min(gender_finearts,na.rm = TRUE)
acs2017_ny$DEGFIELD[(DEGFIELD == "Fine Arts")&(female)]
#The category of both people with degrees in Fine Arts and Social Sciences is too large to

#Now we will check how the two groups differ, while narrowing the factors of people with

mean(acs2017_ny$AGE[(acs2017_ny$DEGFIELD == "Fine Arts")])
mean(acs2017_ny$AGE[(acs2017_ny$DEGFIELD == "Social Sciences")])
finart_artsector <- acs2017_ny$DEGFIELD[(acs2017_ny$DEGFIELD == "Fine Arts")&
                                          (acs2017_ny$HISPAN)]
#For example, if we picked the majors of fine arts or physical science.
#fine arts men are 41 percent male, 59 percent female.
#to add additional restrictions, exclue deomographic, such as asians. Excluse unemployed

acs2017_ny$DEGFIELD <- as.numeric(levels(acs2017_ny$DEGFIELD))[acs2017_ny$DEGFIELD]
fineart_HISPANIC <- acs2017_ny$DEGFIELD[(DEGFIELD == "Fine Arts")&(HISPAN)]
PUMA_lev <-read.csv("PUMA_levels.csv")
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1) & (acs2017_ny$AGE > 20) &
                    (acs2017_ny$AGE < 55))dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1) & (acs2017_ny$DEGFIELD) &
                                                              (acs2017_ny$AGE < 66))


age_physci_rentover1000_HISPAN <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Physical
Sciences" & acs2017_ny$RENT >1000 & acs2017_ny$Hisp_DomR)
])

acs2017_ny$Covid_risk <- ((acs2017_ny$IND_number > 4600) & acs2017_ny$IND_number < 6000) |
  ((acs2017_ny$IND_number > 8500) & (acs2017_ny$IND_number < 8700))
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) &
                    (acs2017_ny$AGE < 66) & (acs2017_ny$educ_college))
degfield_fin_art <- norm_variable( acs2017_ny$educ_college[(acs2017_ny$DEGFIELD == "Fine
Arts")] )
neighboorhoods_finearts <- (acs2017_ny$PUMA[ (acs2017_ny$DEGFIELD == "Fine Arts" )])
EMST_finarts_age20_60 <- subset(acs2017_ny$DEGFIELD[ (acs2017_ny$DEGFIELD == "Fine Arts" &
                                                        acs2017_ny$AGE > 20 & acs2017_ny < 66 & acs2017_ny$EMPSTAT == 2)])

                                
#important
libart_employed_under66 <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Liberal Arts and
Humanities" & acs2017_ny$AGE < 66 & acs2017_ny$EMPSTAT == 1)])
rararacece_libart_employed_under66 <- (acs2017_ny$RACE == 1)[ (acs2017_ny$DEGFIELD ==
                                                                 "Liberal Arts and Humanities" & acs2017_ny$AGE < 66 & acs2017_ny$EMPSTAT == 1)]
gender_neighboorhoods_bronx_liberal_arts <- (acs2017_ny$female[ (acs2017_ny$DEGFIELD ==
                                                                   "Liberal Arts and Humanities" & acs2017_ny$PUMA > 3700 & acs2017_ny$PUMA < 3711) ] )
race_neighboorhoods_bronx_liberal_arts <- (acs2017_ny$RACE[ (acs2017_ny$DEGFIELD ==
                                                               "Liberal Arts and Humanities" & acs2017_ny$PUMA > 3700 & acs2017_ny$PUMA < 3711) ] )
CrossTable(race_neighboorhoods_bronx_liberal_arts,gender_neighboorhoods_bronx_liberal_arts)
gender_finearts <- (acs2017_ny$female[ (acs2017_ny$DEGFIELD == "Fine Arts" )])
finearts <- acs2017_ny$AGE[(acs2017_ny$DEGFIELD == "Fine Arts")]
ducation_finearts <- acs2017_ny$EDUCD[(acs2017_ny$DEGFIELD == "Fine Arts" &
                                         acs2017_ny$EDUCD ==acs2017_ny$AGE < 66 & acs2017_ny$EMPSTAT == 1)]







