## ---------------------------#
##
## Script name: 01-SMS in Huehuetla with CPV 2010 and Intercensal 2015 datasets
##
## Purpose of script: Runs the SMS method provided by Robin Lovelace
##                    The adding value is the provision of an algorithm 
##                    that overcomes applying SMS when missing 
##                    values exist because of data privacy issues. 
##                    The algorithm is implemented in Mexico with 
##                    census data provided by INEGI.
##
## Author: PhD candidate, Néstor De la Paz Ruíz, ITC, University of Twente
##
## Date Created: 2020-09-01
##
## Copyright (c) Néstor de la Paz Ruíz, 2020
## Email: delapazruiz@gmail.com
##
## ---------------------------#
##
## Notes:
## Link references of SMS: https://spatial-microsim-book.robinlovelace.net/intro.html
##
## List of required R scripts:
##            01-SMS_Huehuetla_CPV2010_intercensal2015.R
##            02-Huehuetla_CPV2010_Data privacy algorithm.R
##            03-Huehuetla_Intercensal2015_data preparation.R
##            
## List of Rmarkdown documentation:
##            1-Private data estimation-Hue_CPV2010.Rmd
##            2-SMS_Hue_CPV2010_intercen2015.Rmd
##
## Input data: 
##            1) Block level: huehuetlaCPV2010block.csv (censo pob y vivi 2010)
##            2) Individual level: huehuetla_individuals2015.csv (Intercensal 2015)
##            3) Totals by localityl: huehuetlaCPV2010totals_bylocality.csv
## ---------------------------#


# Loading Huehuetla data ---------------------------------

#setwd("C:/SMS_synthetic_population/spatial-microsim-book-master")
setwd("C:/1.population.modelling/population.modelling")


library(tidyverse)
library(tidyr)
library(dplyr)
library(mipfp)
library(sf)# to work .shp with tidy verse
library(spData)
library(foreign)#to export dbf

#options(error = traceback)
#individuals_example <- read.csv("NetLogo/DWW_agent_weight_mipfp.csv")


#Import block census (Constrains) ---------------------------------
#Data data cleaning

#Block survey data: Cleaning data and categories in "hpcons"
#Import original constrain variables data, cleaning string values with NA &
hpconsfull <- read.csv("data/huehuetlaCPV2010block.csv", # before named hdcons.csv
                       na.strings = c("*", "N.D.")
                       ) # stringsAsFactors=FALSE, as.numeric(), use to convert to number
#clave manzana old = 7119

#Explore column names of original data
colnames(hpconsfull)

#Selection of columns that own constrains of interest: AGE-SEX,EDU,ECON
hpcons <- hpconsfull [, c(9:55, 91:105, 133:138)] #Constrains: AGE-SEX,EDU,ECON

#Define new head of constrains with nomenclature
colnames(hpcons) <- c("POBTOT","POBMAS","POBFEM","P_0A2","P_0A2_M","P_0A2_F","P_3YMAS","P_3YMAS_M","P_3YMAS_F","P_5YMAS","P_5YMAS_M","P_5YMAS_F","P_12YMAS","P_12YMAS_M","P_12YMAS_F","P_15YMAS","P_15YMAS_M","P_15YMAS_F","P_18YMAS","P_18YMAS_M","P_18YMAS_F","P_3A5","P_3A5_M","P_3A5_F","P_6A11","P_6A11_M","P_6A11_F","P_8A14","P_8A14_M","P_8A14_F","P_12A14","P_12A14_M","P_12A14_F","P_15A17","P_15A17_M","P_15A17_F","P_18A24","P_18A24_M","P_18A24_F","P_15A49_F","P_60YMAS","P_60YMAS_M","P_60YMAS_F","REL_H_M","POB0_14","POB15_64","POB65_MAS","P3A5_NOA","P3A5_NOA_M","P3A5_NOA_F","P6A11_NOA","P6A11_NOAM","P6A11_NOAF","P12A14NOA","P12A14NOAM","P12A14NOAF","P15A17A","P15A17A_M","P15A17A_F","P18A24A","P18A24A_M","P18A24A_F","PEA","PEA_M","PEA_F","PE_INAC","PE_INAC_M","PE_INAC_F")

#Explore new colums name
colnames(hpcons)

#Import row of original real totals according to Census
hptotals <- read.csv("data/huehuetlaCPV2010totals_bylocality.csv", na.strings = c("*", "N.D.")) #before named hp_totals.csv
hptotcons <- hptotals [, c(9:55, 91:105, 133:138)]

#Define new head of totals of constrains
colnames(hptotcons) <- c("POBTOT","POBMAS","POBFEM","P_0A2","P_0A2_M","P_0A2_F","P_3YMAS","P_3YMAS_M","P_3YMAS_F","P_5YMAS","P_5YMAS_M","P_5YMAS_F","P_12YMAS","P_12YMAS_M","P_12YMAS_F","P_15YMAS","P_15YMAS_M","P_15YMAS_F","P_18YMAS","P_18YMAS_M","P_18YMAS_F","P_3A5","P_3A5_M","P_3A5_F","P_6A11","P_6A11_M","P_6A11_F","P_8A14","P_8A14_M","P_8A14_F","P_12A14","P_12A14_M","P_12A14_F","P_15A17","P_15A17_M","P_15A17_F","P_18A24","P_18A24_M","P_18A24_F","P_15A49_F","P_60YMAS","P_60YMAS_M","P_60YMAS_F","REL_H_M","POB0_14","POB15_64","POB65_MAS","P3A5_NOA","P3A5_NOA_M","P3A5_NOA_F","P6A11_NOA","P6A11_NOAM","P6A11_NOAF","P12A14NOA","P12A14NOAM","P12A14NOAF","P15A17A","P15A17A_M","P15A17A_F","P18A24A","P18A24A_M","P18A24A_F","PEA","PEA_M","PEA_F","PE_INAC","PE_INAC_M","PE_INAC_F")
colnames(hptotcons)

#Check NA values for totals and fill them based on sum of MF 
hptotcons[which(is.na(hptotcons))]

#Only if required - Completing missing values in totals
which(is.na(hptotcons))
hptotcons[,c(51,52,53,54)]
hptotcons[,c(52,53)] <- c(2,1) #Optional values 2,1, for M-f missing
hptotcons[,c(51,52,53,54)]

#Execute program to fill missing individuals (NA values).
#Required table inputs: 1.hpcons & 2.hptotcons {standar headers]
#Load constraints separately
source("code/02-Huehuetla_CPV2010_Data privacy algorithm.R") # before named h_hpcons_2010.R
sum(hpcons1)
sum(hpcons2)
sum(hpcons3)

#"Original" constrain variables that required for cleaning and transformation
hpcons.orig <- hpcons %>%  select( 
   POBTOT, POBMAS, POBFEM,
   P_12YMAS, P_12YMAS_M, P_12YMAS_F,
   P_0A2, P_0A2_M, P_0A2_F, 
   P_3A5, P_3A5_M, P_3A5_F,
   P_6A11, P_6A11_M, P_6A11_F,
   P_12A14, P_12A14_M, P_12A14_F,
   P_15A17, P_15A17_M, P_15A17_F,
   P_18A24, P_18A24_M, P_18A24_F,
   P3A5_NOA, P3A5_NOA_M, P3A5_NOA_F,
   P6A11_NOA, P6A11_NOAM, P6A11_NOAF,
   P12A14NOA, P12A14NOAM, P12A14NOAF,
   P18A24A, P18A24A_M, P18A24A_F,
   P15A17A, P15A17A_M, P15A17A_F,
   PEA, PEA_M, PEA_F,
   PE_INAC, PE_INAC_M, PE_INAC_F
   ) %>% as.data.frame()


# Loading Individual survey ---------------------------------
# Encuesta Intercensal 2015: Variable "hind"

#Import original constrain data, cleaning string values with NA
hindfull <- read.csv("data/huehuetla_individuals2015.csv")
colnames(hindfull)

#Individual- survey linking variable selections
hind <- 
  hindfull [, c(
    #Main variables for Spatial microsimulation
    14,# $SEXO -    SEX?: 1= MAN, 3=WOMAN
    15,# $EDAD -    AGE?: 0..109, 110, 999 = NA
    33,# $ASISTEN - ASSIST TO SCHOOL [ASISTEN]?: 5= YES, 7=NO, 9,NA= Null
    50,# $CONACT -  GOING TO WORK?  10,11,12,13,14,15,16 = YES, 20,31,32,33,34,35,99,NA= No
    #Complementary information of individuals
    51,52, #activity (work) name, work position
    20,#health service?    
    34,35,38,39,# location-mobility (education)
    41,42,#$ESCOLARI, $NIVACAD education
    48,49,# conjugal situation
    54,55,#vacations and health
    60, # $INGTRMEN - monthly economic income
    61,62,63,65,66, #variables of mobility (go to work) 
    69,70,71,72,73,74,75,76, #day to day activities (hours)
    79, #sons alive (number)
    1,2 # $ID_VIV, $ID_PERSONA IDs viv persona
  )]
head(hind)

#Explore main linking variables
summary(hind$EDAD)
summary(hind$SEXO)
summary(hind$ASISTEN)
summary(hind$CONACT)

#Execute program to process individual data.
#Required inputs: 1.hind
source("code/03-Huehuetla_Intercensal2015_data preparation.R")

#Explore and select individual data variables: Age, gender, school, work
summary(hind)
hind4 <- hind[,c(1,2,3,4,12,13)] #c(18,33,34)]
hind4[is.na(hind4)] = 0
summary(hind4)
head(hind4)


# Create weight matrix ---------------------------------
# - All zones together

# Initial weight matrix
weight_init_onezone <- table(hind4)
# Check order of the variables
dimnames(weight_init_onezone) 

# Repeat the initial matrix n_zone times
init_cells <- rep(weight_init_onezone, each = nrow(hpcons0))

# Define the names
names <- c(list(rownames(hpcons0)),as.list(dimnames(weight_init_onezone)))

# Structure the data
weight_init <- array(init_cells, dim = 
                       c(nrow(hpcons0),dim(weight_init_onezone)),
                     dimnames = names)

# To correctly perform the Ipfp process, 
# we have to use coherent constraints, 
# with same marginals per zone.
table(rowSums(hpcons2)==rowSums(hpcons1))
table(rowSums(hpcons3)==rowSums(hpcons1))
table(rowSums(hpcons2)==rowSums(hpcons3))

# Observe the global total. Check data if sums differ
sum(hpcons1)
sum(hpcons2)
sum(hpcons3)


#Convert the constraint 1 to be readable for mipfp ---------------------------------


# Transform hpcons1 into an 3D-array : con1_convert
#Values c(2,3) represent the position of variable sex and age
names <- c(list(rownames(hpcons0)),dimnames(weight_init)[c(2,3)])
#Values 2,7, represent a table of 2 dimensions with 7 variables of age
hpcons1_conv <- array(NA, dim=c(nrow(hpcons0),2,7), dimnames = names)

for(zone in rownames(hpcons0)){
  for (sex in dimnames(hpcons1_conv)$SEXO){
    for (age in dimnames(hpcons1_conv)$EDAD){
      hpcons1_conv[zone,sex,age] <- hpcons1[zone,paste(sex,age,sep="")]
    }
  }
}


#Perform the Ipfp function ---------------------------------


# check margins per zone: 
table(rowSums(hpcons1)==apply(hpcons1_conv, 1, sum))

#Define the target-constrain used by IPFP hpcons1,2,3
target <- list(hpcons1_conv, as.matrix(hpcons2), as.matrix(hpcons3))

#1:block, 2:sex, 3:age, 4:school, 5:work
descript <- list(c(1,2,3), c(1,4),c(1,5))

#Generate SMS matrix 
weight_mipfp <- Ipfp( weight_init, descript, target, 
                      print = FALSE, iter = 15000, tol=1e-7)

remove(names, target, descript, age, sex, zone, 
       weight_init_onezone, weight_init, init_cells)

# Integerisation and expansion ---------------------------------
# for MIPFP 


# Save the matrix in a new variable
res <- weight_mipfp
int_mipfp <- res$x.hat
# Integerise zone per zone
n_zone <- nrow(hpcons0)

# 'Truncate, replicate, sample' (TRS) method of integerisation
int_trs <- function(x){
  xv <- as.vector(x)
  xint <- floor(xv)
  r <- xv - xint
  def <- round(sum(r)) # the deficit population
  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}

# NUMBER of ',' in [i,,,,,] = , in res$x.hat
for (i in 1:n_zone){
  int_mipfp[i,,,,,,] <- int_trs(int_mipfp[i,,,,,,])
}

# Expansion of mipfp integerised weight matrix
int_expand_array <- function(x){
  # Transform the array into a dataframe
  count_data <- as.data.frame.table(x)
  # Store the indices of categories for the final population
  indices <- rep(1:nrow(count_data), count_data$Freq)
  # Create the final individuals
  ind_data <- count_data[indices,]
  ind_data
}

#Apply function for expansion
indiv_mipfp <- int_expand_array(int_mipfp)
Names <- c("Block", "Sex", "Age", "Go_School", "Go_Work",
           "Escolar_grade", "Escolar_level","Freq")
colnames(indiv_mipfp) <- Names

#First exploration of results
indiv_mipfp %>% count(Block)
summary(indiv_mipfp)
sum(summary(indiv_mipfp$Sex))
sum(hpcons1)
sum(hpcons1)-sum(summary(indiv_mipfp$Sex))

remove(res, i, Names, n_zone)


# Evaluation ---------------------------------
# Simulation vs observation

#Define variables to compare global simulation v.s. census observations
# "shp" stands for simulated huehuetal population. 
shpcons1 <- apply(weight_mipfp$x.hat,c(1,2,3),sum)
shpcons2 <- apply(weight_mipfp$x.hat,c(1,4),sum)
shpcons3 <- apply(weight_mipfp$x.hat,c(1,5),sum)

#Dataframe is required for plotting and individual variable cheking
shpcons1.df <- as.data.frame(shpcons1) 
shpcons2.df <- as.data.frame(shpcons2)
shpcons3.df <- as.data.frame(shpcons3)

#Verify name-order of data frames for plotting and correlations
colnames(shpcons3.df) <- c("PEA", "PE_INAC")
colnames(shpcons2.df) <- c("PA", "NOA")
shpcons1.df <- shpcons1.df[, 
c("m.P_0A2", "m.P_3A5", "m.P_6A11",
  "m.P_12A14","m.P_15A17","m.P_18A24","m.P_25A130",
  "f.P_0A2","f.P_3A5","f.P_6A11",
  "f.P_12A14","f.P_15A17","f.P_18A24","f.P_25A130")]
head(shpcons1.df)
head(shpcons2.df)
head(shpcons3.df)

#Aggregate all variables once the order-name pf variables are correct
shpcons0.df <- cbind(
  shpcons1.df, shpcons2.df, shpcons3.df )

head(shpcons0.df)


# Plotting results ---------------------------------
# Simulated vs observations(census-data)

#Plot of global correlations of the constrains
plot(as.numeric(as.matrix(hpcons3)), as.numeric(shpcons3),
     main="Workers variable: Count of individuals in a block",
     ylab="Simulated individuals",
     xlab="Observed census data",
     col = c("red", "blue"),
     pch = c(1, 3),
     sub = (paste("Correlation = ", 
                  cor(as.numeric(as.matrix(hpcons3)),
                      as.numeric(as.matrix(shpcons3)))))
)
    legend("topleft",
           c("Census","Individuals simulation"),
           fill=c("red","blue")
    )

plot(as.numeric(as.matrix(hpcons2)), as.numeric(shpcons2),
     main="School assistance variable: Count of individuals in a block",
     ylab="Simulated individuals",
     xlab="Observed census data",
     col = c("red", "blue"),
     pch = c(1, 3),
     sub = (paste("Correlation = ", 
                  cor(as.numeric(as.matrix(hpcons2)),
                      as.numeric(as.matrix(shpcons2)))))
)
    legend("topleft",
           c("Census","individuals Simulation"),
           fill=c("red","blue")
    )

plot(as.numeric(as.matrix(hpcons1_conv)), as.numeric(shpcons1),
     main="Age & Sex variables: Count of individuals in a block",
     ylab="Simulated individuals",
     xlab="Observed census data",
     col = c("red", "blue"),
     pch = c(1, 3),
     sub = (paste("Correlation = ", 
                   cor(as.numeric(as.matrix(hpcons1_conv)),
                     as.numeric(as.matrix(shpcons1)))))
     )
     legend("topleft",
            c("Census","Individuals simulation"),
            fill=c("red","blue")
          )

#Verification with AGE-SEX
plot(hpcons1$mP_0A2, shpcons1.df$m.P_0A2)
plot(hpcons1$fP_0A2, shpcons1.df$f.P_0A2)
plot(hpcons1$fP_25A130, shpcons1.df$f.P_25A130)
plot(hpcons1$mP_25A130, shpcons1.df$m.P_25A130)

#NOT GOING to SCHOOL
plot(hpcons2$NOA, shpcons2.df$NOA)
plot(hpcons2$PA, shpcons2.df$PA)

#GO TO WORK / NOT GOING TO WORK
plot(hpcons3$PE_INAC, shpcons3.df$PE_INAC)
plot(hpcons3$PEA, shpcons3.df$PEA)


# Global correlations of variables ---------------------------------
# GLOBAL & VARIABLES: simulated vs observations

#GLOBAL Correlation of simulation v.s. census observations
cor(as.numeric(as.matrix(hpcons3)), as.numeric(as.matrix(shpcons3)))
cor(as.numeric(as.matrix(hpcons2)), as.numeric(shpcons2))
cor(as.numeric(as.matrix(hpcons1_conv)), as.numeric(shpcons1))

#VARIABLES (columns) correlations (x18)

#Correlations by AGE-SEX[1:14] SCHOOL[15:16] WORK[17:18]
CorCol <- rep(0, dim(hpcons0)[2]) 

for (i in 1:dim(hpcons0)[2]){ # Correlation for each variable
  num_hpcons0 <- as.numeric(hpcons0[,i]) 
  num_shpcons0.df <- as.numeric(shpcons0.df[,i]) 
  CorCol[i] <- cor (num_hpcons0, num_shpcons0.df)
}

#AGE-SEX [1:14] SCHOOL [15:16] WORK [17:18]
CorCol <- as.data.frame(CorCol)
rownames(CorCol) <- c(
  "m.P_0A2", "m.P_3A5", "m.P_6A11",
  "m.P_12A14","m.P_15A17","m.P_18A24","m.P_25A130",
  "f.P_0A2","f.P_3A5","f.P_6A11",
  "f.P_12A14","f.P_15A17","f.P_18A24","f.P_25A130",
  "PEA", "PE_INAC",
  "PA", "NOA")
#Print correlations by colums (viriables)
CorCol


# Correlations by block ---------------------------------
# simulated vs observations(census-data)

#BLOCK (row) correlations (x56)

#initialize the vector of correlations 
CorRow <- rep(0, dim(hpcons0)[1])

# calculate the correlation for each zone 
for (i in 1:dim(hpcons0)[1]){ 
  num_hpcons0 <- as.numeric(hpcons0[i,]) 
  num_shpcons0.df <- as.numeric(shpcons0.df[i,]) 
  CorRow[i] <- cor (num_hpcons0, num_shpcons0.df)
}

# Summary of the correlations per block 
summary (CorRow)
# Identify the block with the worst fit 
which.min(CorRow)
# Top 5 worst block values
returnValue(head(order(CorRow), n = 5))
#List from less to more accurate 
CorRow[order(CorRow)]

remove(i, num_hpcons0, num_shpcons0.df)

# Maximum SMS errors ---------------------------------
# Simulated vs observations(census-data)

# Maximum error of hpcons1,2,3
max(abs(hpcons1_conv - shpcons1))
max(abs(hpcons2 - shpcons2))
max(abs(hpcons3 - shpcons3))

# Index of the maximum error
which(abs(shpcons3-hpcons3) == max(abs(shpcons3-hpcons3)), arr.ind = TRUE)
which(abs(shpcons2-hpcons2) == max(abs(shpcons2-hpcons2)), arr.ind = TRUE)
which(abs(shpcons1-hpcons1_conv) == max(abs(shpcons1-hpcons1_conv)), arr.ind = TRUE)

