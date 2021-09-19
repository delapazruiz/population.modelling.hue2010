#Four table constrains are generated hpcons.c, hpcons1, hpcons2, hpcons3
#The sumrow_mf function has to be added for clean.pvar.NOA and so on
library(tidyverse)

# Clean "n" P_12YMAS Population 12 years and more------------------------

#debugonce(clean.pvar)

clean.pvar <- function(vec.mf = hpcons$P_12YMAS,
                       vec.m = hpcons$P_12YMAS_M,
                       vec.f = hpcons$P_12YMAS_F,
                       t.tot.mf = hptotcons$P_12YMAS,
                       t.tot.m = hptotcons$P_12YMAS_M,
                       t.tot.f = hptotcons$P_12YMAS_F,
                       nam.mf.n = "P_12YMAS.f",
                       nam.m.n ="P_12YMAS_M.f",
                       nam.f.n ="P_12YMAS_F.f"){
  hpcons$vec.mf.n <- 0 #Create a dummy columns to store new values
  hpcons$vec.m.n <-  0
  hpcons$vec.f.n <-  0
  
  vec.mf.n <- hpcons$vec.mf.n# variables names used from now on
  vec.m.n <- hpcons$vec.m.n
  vec.f.n <- hpcons$vec.f.n
  
  #Function used in the while conditional: compare new and original data
  smcol_mf <- function(i){sum.out<- (sum(vec.mf.n)+sum(vec.mf, na.rm = TRUE))
  return(sum.out)}#sum of missing & knowing values
  smcol_m <- function(i){sum.out<- (sum(vec.m.n)+sum(vec.m, na.rm = TRUE))
  return(sum.out)}
  smcol_f <- function(i){sum.out<- (sum(vec.f.n)+sum(vec.f, na.rm = TRUE))
  return(sum.out)}
  
  #Conditionals for totals MF: Filling missing values
  while (t.tot.mf > smcol_mf(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.mf[i])#Targeting the missing fields to fill
          &(t.tot.mf > smcol_mf(i))#Keep sum values under real total
          &(hpcons$POBTOT[i] > vec.mf.n[i])#keep by block values under POB totals
      ) {vec.mf.n[i] <- vec.mf.n[i]+1}#Add individual
    }
  }
  #Prepare variable MF for for exporting. Note: do not move to the end
  vec.mf [is.na(vec.mf)]= 0 # clean NA values for sum
  vec.df.mf.n <- as.data.frame(vec.mf + vec.mf.n)#Sum current and cleaned columns
  colnames(vec.df.mf.n) <- c(nam.mf.n)#Generic variable name of cleaned data
  
  #Conditionals for totals M: Filling missing values
  
  #First make sure to cover all female conditions
  
  while (t.tot.m > smcol_m(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.m[i])#Targeting the missing fields to fill
          & !is.na(vec.f[i])#For female precondition for male cells
          #First fill all values where total and females are defined
          & (((vec.mf + vec.mf.n)[i] - vec.f[i]) != vec.m.n[i])
      ){while (((vec.mf + vec.mf.n)[i] - vec.f[i]) != vec.m.n[i]){
        vec.m.n[i] <- vec.m.n[i]+1
      }
      }
    }
    for (i in 1:nrow(hpcons)){
      if(is.na(vec.m[i])#Targeting the missing fields to fill
         & is.na(vec.f[i])#2nd possibility with female values
         & (t.tot.m > smcol_m(1))#Keep sum values under real total
      ){#keep by block values under POB totals
        vec.m.n[i] <- vec.m.n[i]+1#Add individual
      }
    }
  }
  #Prepare variable M for exporting
  vec.m [is.na(vec.m)]= 0# clean NA values for sum
  vec.df.m.n <- as.data.frame(vec.m + vec.m.n)#Sum current and cleaned columns
  colnames(vec.df.m.n) <- c(nam.m.n)#Generic variable name of cleaned data
  
  #Conditionals for totals F: Filling missing values
  while (t.tot.f > smcol_f(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.f[i])#Targeting the missing fields to fill
          & (t.tot.f > smcol_f(i))#Keep sum values under real total
          #Special condition to consider: Calculate difference of new totals
          & ( (vec.mf+vec.mf.n)[i] != ((vec.m+vec.m.n)[i] + vec.f.n[i]))
          & (( (vec.mf+vec.mf.n)[i] - (vec.m+vec.m.n)[i]) != 0))
      {vec.f.n[i] <- vec.f.n[i]+1#Add individual
      }
    }
  }
  #Prepare variable F for for exporting
  vec.f [is.na(vec.f)]= 0# clean NA values for sum
  vec.df.f.n <- as.data.frame(vec.f + vec.f.n)#Sum current and cleaned columns
  colnames(vec.df.f.n) <- c(nam.f.n)#Generic variable name of cleaned data
  
  #Prepare FINAL data.frame of  MF, M, and F for exporting
  clean.vec <- cbind(vec.df.mf.n, vec.df.m.n, vec.df.f.n)
  return(clean.vec)#Retrieve the outcomes of the function
}



#Clean P_nAm Age-gender (x7): 0-2,25-130 ------------------------

#debugonce(clean.pvar.P_nAm)

clean.pvar.P_nAm <- function(vec.mf = hpcons$P_0A2,
                             vec.m = hpcons$P_0A2_M,
                             vec.f = hpcons$P_0A2_F,
                             t.tot.mf = hptotcons$P_0A2,
                             t.tot.m = hptotcons$P_0A2_M,
                             t.tot.f = hptotcons$P_0A2_F,
                             nam.mf.n = "P_0A2.f",
                             nam.m.n ="P_0A2_M.f",
                             nam.f.n ="P_0A2_F.f"){
  hpcons$vec.mf.n <- 0 #Create a dummy columns to store new values
  hpcons$vec.m.n <-  0
  hpcons$vec.f.n <-  0
  
  vec.mf.n <- hpcons$vec.mf.n# variables names used from now on
  vec.m.n <- hpcons$vec.m.n
  vec.f.n <- hpcons$vec.f.n
  
  #Function used in the while conditionals:
  #Compare new and original data from row totals
  #If a variable (e.g. P_0A2.c) was already calculated, then use those values
  if (exists("P_0A2.c")) {(hpcons$P_0A2 <- P_0A2.c$P_0A2.f) &
      (hpcons$P_0A2_M <- P_0A2.c$P_0A2_M.f) &
      (hpcons$P_0A2_F <- P_0A2.c$P_0A2_F.f)}
  
  if (exists("P_3A5.c")) {(hpcons$P_3A5 <- P_3A5.c$P_3A5.f)&
      (hpcons$P_3A5_M <- P_3A5.c$P_3A5_M.f)&
      (hpcons$P_3A5_F <- P_3A5.c$P_3A5_F.f)}
  
  if (exists("P_6A11.c")) {(hpcons$P_6A11 <- P_6A11.c$P_6A11.f)&
      (hpcons$P_6A11_M <- P_6A11.c$P_6A11_M.f)&
      (hpcons$P_6A11_F <- P_6A11.c$P_6A11_F.f)}
  
  if (exists("P_12A14.c")) {(hpcons$P_12A14 <- P_12A14.c$P_12A14.f)&
      (hpcons$P_12A14_M <- P_12A14.c$P_12A14_M.f)&
      (hpcons$P_12A14_F <- P_12A14.c$P_12A14_F.f)}
  
  if (exists("P_15A17.c")) {(hpcons$P_15A17 <- P_15A17.c$P_15A17.f)&
      (hpcons$P_15A17_M <- P_15A17.c$P_15A17_M.f)&
      (hpcons$P_15A17_F <- P_15A17.c$P_15A17_F.f)}
  
  if (exists("P_18A24.c")) {(hpcons$P_18A24 <- P_18A24.c$P_18A24.f)&
      (hpcons$P_18A24_M <- P_18A24.c$P_18A24_M.f)&
      (hpcons$P_18A24_F <- P_18A24.c$P_18A24_F.f)}
  
  #Sum of new i row value in the iteration with known row values
  smrow_mf <- function(i){sum.out<- (sum(hpcons[i, c("vec.mf.n")]) +
                                       sum(hpcons[i,c(
                                         "P_0A2","P_3A5","P_6A11","P_12A14","P_15A17","P_18A24")], na.rm = TRUE))
  return(sum.out)}
  
  smrow_m <- function(i){sum.out<- (sum(hpcons[i, c("vec.m.n")]) +
                                      sum(hpcons[i,c(
                                        "P_0A2_M","P_3A5_M","P_6A11_M","P_12A14_M","P_15A17_M","P_18A24_M")], na.rm = TRUE))
  return(sum.out)}
  
  smrow_f <- function(i){sum.out<- (sum(hpcons[i, c("vec.f.n")]) +
                                      sum(hpcons[i,c(
                                        "P_0A2_F","P_3A5_F","P_6A11_F","P_12A14_F","P_15A17_F","P_18A24_F")], na.rm = TRUE))
  return(sum.out)}
  
  #Function used in the while conditional: 
  #Compare new and original data from known columns totals
  #Sum of new i column value in the iteration with known column values
  smcol_mf <- function(i){sum.out<- (sum(vec.mf.n)+sum(vec.mf, na.rm = TRUE))
  return(sum.out)}#sum of missing & known values
  
  smcol_m <- function(i){sum.out<- (sum(vec.m.n)+sum(vec.m, na.rm = TRUE))
  return(sum.out)}
  
  smcol_f <- function(i){sum.out<- (sum(vec.f.n)+sum(vec.f, na.rm = TRUE))
  return(sum.out)}
  
  #Conditionals for totals MF: Filling missing values
  while (t.tot.mf > smcol_mf(i)){#while vales are not == real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.mf[i])#Targeting the missing fields to fill
          &(t.tot.mf > smcol_mf(i))#Keep sum col values under real total
          &(POBTOT.c$POBTOT.f[i] > smrow_mf(i))#keep row sum variable under POB totals
          &(3 > vec.mf.n[i])#Only values less than 3 are confidential
      ) {vec.mf.n[i] <- vec.mf.n[i]+1}#Add individual
    }
  }
  #Prepare variable MF for for exporting. Note: do not move to the end
  vec.mf [is.na(vec.mf)]= 0 # clean NA values for sum
  vec.df.mf.n <- as.data.frame(vec.mf + vec.mf.n)#Sum current and cleaned columns
  colnames(vec.df.mf.n) <- c(nam.mf.n)#Generic variable name of cleaned data
  
  #Conditionals for totals M: Filling missing values
  
  #First make sure to cover all female conditions
  
  while (t.tot.m > smcol_m(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.m[i])#Targeting the missing fields to fill
          & !is.na(vec.f[i])#For female precondition for male cells
          & (POBTOT.c$POBMAS.f[i] > smrow_m(i))#Keep sum col values under real total
          #First fill all values where total and females are defined
          & (((vec.mf + vec.mf.n)[i] - vec.f[i]) != vec.m.n[i])
      ){while (((vec.mf + vec.mf.n)[i] - vec.f[i]) != vec.m.n[i]){
        vec.m.n[i] <- vec.m.n[i]+1
      }
      }
    }
    for (i in 1:nrow(hpcons)){
      if(is.na(vec.m[i])#Targeting the missing fields to fill
         & is.na(vec.f[i])#2nd possibility with female values
         & (t.tot.m > smcol_m(i))#Keep sum col values under real total
         & (POBTOT.c$POBMAS.f[i] > smrow_m(i))#Keep sum row values under real total
         & (vec.m.n[i] != vec.df.mf.n[i,c(1)])#must be different that the MF total of the cell
         & (3 > vec.m.n[i])#Only values less than 3 are confidential
      ){#keep by block values under POB totals
        vec.m.n[i] <- vec.m.n[i]+1#Add individual
      }
    }
  }
  #Prepare variable M for exporting
  vec.m [is.na(vec.m)]= 0# clean NA values for sum
  vec.df.m.n <- as.data.frame(vec.m + vec.m.n)#Sum current and cleaned columns
  colnames(vec.df.m.n) <- c(nam.m.n)#Generic variable name of cleaned data
  
  #Conditionals for totals F: Filling missing values
  while (t.tot.f > smcol_f(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.f[i])#Targeting the missing fields to fill
          & (t.tot.f > smcol_f(i))#Keep sum col values under real total
          & (POBTOT.c$POBFEM.f[i] > smrow_f(i))#Keep sum row values under real total
          #Special condition: verify that male[i] keeps under total MF[i]
          & ( (vec.mf+vec.mf.n)[i] != ((vec.m+vec.m.n)[i] + vec.f.n[i]))
          #Special condition: Calculate difference of new totals
          & (( (vec.mf+vec.mf.n)[i] - (vec.m+vec.m.n)[i]) != 0)
          & (3 > vec.m.n[i]))#Only values less than 3 are confidential
      {vec.f.n[i] <- vec.f.n[i]+1#Add individual
      }
    }
  }
  #Prepare variable F for for exporting
  vec.f [is.na(vec.f)]= 0# clean NA values for sum
  vec.df.f.n <- as.data.frame(vec.f + vec.f.n)#Sum current and cleaned columns
  colnames(vec.df.f.n) <- c(nam.f.n)#Generic variable name of cleaned data
  
  #Prepare FINAL data.frame of  MF, M, and F for exporting
  clean.vec <- cbind(vec.df.mf.n, vec.df.m.n, vec.df.f.n)
  return(clean.vec)#Retrieve the outcomes of the function
}


#Clean P_nAm.NOA, A, School assistance or not (x3):3-5, 6-11, 12-14 (x2):15-17, 18-24  ------------------------


#debugonce(clean.pvar.NOA)
clean.pvar.NOA <- function(vec.mf = hpcons$P3A5_NOA,
                           vec.m = hpcons$P3A5_NOA_M,
                           vec.f = hpcons$P3A5_NOA_F,
                           t.tot.mf = hptotcons$P3A5_NOA,
                           t.tot.m = hptotcons$P3A5_NOA_M,
                           t.tot.f = hptotcons$P3A5_NOA_F,
                           nam.mf.n = "P3A5_NOA.f",
                           nam.m.n ="P3A5_NOA_M.f",
                           nam.f.n ="P3A5_NOA_F.f"){
  hpcons$vec.mf.n <- 0 #Create a dummy columns to store new values
  hpcons$vec.m.n <-  0
  hpcons$vec.f.n <-  0
  
  vec.mf.n <- hpcons$vec.mf.n# variables names used from now on
  vec.m.n <- hpcons$vec.m.n
  vec.f.n <- hpcons$vec.f.n
  
  #Function used in the while conditionals:
  #Compare new and original data from row totals
  #If a variable (e.g. P_0A2.c) was already calculated, then use those values
  
  if (nam.mf.n == "P3A5_NOA.f"){
    (mx.row.mf <- P_3A5.c$P_3A5.f)&
      (mx.row.m <- P_3A5.c$P_3A5_M.f)&
      (mx.row.f <- P_3A5.c$P_3A5_F.f)
  }
  
  if (nam.mf.n == "P6A11_NOA.f"){
    (mx.row.mf <- P_6A11.c$P_6A11.f)&
      (mx.row.m <- P_6A11.c$P_6A11_M.f)&
      (mx.row.f <- P_6A11.c$P_6A11_F.f)
  }
  
  if (nam.mf.n == "P12A14NOA.f"){
    (mx.row.mf <- P_12A14.c$P_12A14.f)&
      (mx.row.m <- P_12A14.c$P_12A14_M.f)&
      (mx.row.f <- P_12A14.c$P_12A14_F.f)
  }
  
  if (nam.mf.n == "P15A17A.f"){
    (mx.row.mf <- P_15A17.c$P_15A17.f)&
      (mx.row.m <- P_15A17.c$P_15A17_M.f)&
      (mx.row.f <- P_15A17.c$P_15A17_F.f)
  }
  
  if (nam.mf.n == "P18A24A.f"){
    (mx.row.mf <- P_18A24.c$P_18A24.f)&
      (mx.row.m <- P_18A24.c$P_18A24_M.f)&
      (mx.row.f <- P_18A24.c$P_18A24_F.f)
  }
  
  #Function used in the while conditional: 
  #Compare new and original data from known columns totals
  #Sum of new i column value in the iteration with known column values
  smcol_mf <- function(i){sum.out<- (sum(vec.mf.n)+sum(vec.mf, na.rm = TRUE))
  return(sum.out)}#sum of missing & known values
  
  smcol_m <- function(i){sum.out<- (sum(vec.m.n)+sum(vec.m, na.rm = TRUE))
  return(sum.out)}
  
  smcol_f <- function(i){sum.out<- (sum(vec.f.n)+sum(vec.f, na.rm = TRUE))
  return(sum.out)}
  
  #Conditionals for totals MF: Filling missing values
  while (t.tot.mf > smcol_mf(i)){#while vales are not == real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.mf[i])#Targeting the missing fields to fill
          &(t.tot.mf > smcol_mf(i))#Keep sum col values under real total
          &(3 > vec.mf.n[i])#Only values less than 3 are confidential
          &(mx.row.mf[i] > vec.mf.n[i])
          &(mx.row.mf[i] != 0)
      ) {vec.mf.n[i] <- vec.mf.n[i]+1}#Add individual
    }
  }
  #Prepare variable MF for for exporting. Note: do not move to the end
  vec.mf [is.na(vec.mf)]= 0 # clean NA values for sum
  vec.df.mf.n <- as.data.frame(vec.mf + vec.mf.n)#Sum current and cleaned columns
  colnames(vec.df.mf.n) <- c(nam.mf.n)#Generic variable name of cleaned data
  
  #Conditionals for totals M: Filling missing values
  
  #First make sure to cover all female conditions
  
  while (t.tot.m > smcol_m(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.m[i])#Targeting the missing fields to fill
          & !is.na(vec.f[i])#For female precondition for male cells
          #First fill all values where total and females are defined
          & (((vec.mf + vec.mf.n)[i] - vec.f[i]) != vec.m.n[i])
      ){while (((vec.mf + vec.mf.n)[i] - vec.f[i]) != vec.m.n[i]){
        vec.m.n[i] <- vec.m.n[i]+1#Add individual
      }
      }
    }
    for (i in 1:nrow(hpcons)){
      if(is.na(vec.m[i])#Targeting the missing fields to fill
         & is.na(vec.f[i])#2nd possibility with female values
         & (t.tot.m > smcol_m(i))#Keep sum col values under real total
         & (vec.m.n[i] != vec.df.mf.n[i,c(1)])#must be different that the MF total of the cell
         & (mx.row.m[i] > vec.m.n[i])
         & (3 >= vec.m.n[i])#Only values less than 3 are confidential
      ){
        vec.m.n[i] <- vec.m.n[i]+1#Add individual
      }
    }
  }
  #Prepare variable M for exporting
  vec.m [is.na(vec.m)]= 0# clean NA values for sum
  vec.df.m.n <- as.data.frame(vec.m + vec.m.n)#Sum current and cleaned columns
  colnames(vec.df.m.n) <- c(nam.m.n)#Generic variable name of cleaned data
  
  #Conditionals for totals F: Filling missing values
  while (t.tot.f > smcol_f(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.f[i])#Targeting the missing fields to fill
          & (t.tot.f > smcol_f(i))#Keep sum col values under real total
          #Special condition: verify that male[i] keeps under total MF[i]
          & ( (vec.mf+vec.mf.n)[i] != ((vec.m+vec.m.n)[i] + vec.f.n[i]))
          #Special condition: Calculate difference of new totals
          & (( (vec.mf+vec.mf.n)[i] - (vec.m+vec.m.n)[i]) != 0)
          & (3 >= vec.m.n[i])#Only values less than 3 are confidential
      )#& (mx.row.f[i] > vec.f.n[i]))
      {vec.f.n[i] <- vec.f.n[i]+1#Add individual
      }
    }
  }
  #Prepare variable F for for exporting
  vec.f [is.na(vec.f)]= 0# clean NA values for sum
  vec.df.f.n <- as.data.frame(vec.f + vec.f.n)#Sum current and cleaned columns
  colnames(vec.df.f.n) <- c(nam.f.n)#Generic variable name of cleaned data
  
  #Prepare FINAL data.frame of  MF, M, and F for exporting
  clean.vec <- cbind(vec.df.mf.n, vec.df.m.n, vec.df.f.n)
  return(clean.vec)#Retrieve the outcomes of the function
}



#Clean PEA Work assistance or not (x2): PEA,PE_INAC ------------------------

#debugonce(clean.pvar.PEA)

clean.pvar.PEA <- function(vec.mf = hpcons$PEA,
                           vec.m = hpcons$PEA_M,
                           vec.f = hpcons$PEA_F,
                           t.tot.mf = hptotcons$PEA,
                           t.tot.m = hptotcons$PEA_M,
                           t.tot.f = hptotcons$PEA_F,
                           nam.mf.n = "PEA.f",
                           nam.m.n ="PEA_M.f",
                           nam.f.n ="PEA_F.f"){
  hpcons$vec.mf.n <- 0 #Create a dummy columns to store new values
  hpcons$vec.m.n <-  0
  hpcons$vec.f.n <-  0
  
  vec.mf.n <- hpcons$vec.mf.n# variables names used from now on
  vec.m.n <- hpcons$vec.m.n
  vec.f.n <- hpcons$vec.f.n
  
  #Function used in the while conditional: 
  #Compare new and original data from known columns totals
  #Sum of new i column value in the iteration with known column values
  smcol_mf <- function(i){sum.out<- (sum(vec.mf.n)+sum(vec.mf, na.rm = TRUE))
  return(sum.out)}#sum of missing & known values
  
  smcol_m <- function(i){sum.out<- (sum(vec.m.n)+sum(vec.m, na.rm = TRUE))
  return(sum.out)}
  
  smcol_f <- function(i){sum.out<- (sum(vec.f.n)+sum(vec.f, na.rm = TRUE))
  return(sum.out)}
  
  #Conditionals for totals MF: Filling missing values
  while (t.tot.mf > smcol_mf(i)){#while vales are not == real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.mf[i])#Targeting the missing fields to fill
          &(t.tot.mf > smcol_mf(i))#Keep sum col values under real total
          &(P_12YMAS.c$P_12YMAS.f [i] > vec.mf.n[i])#keep by block values under 12YMAS MF
      ) {vec.mf.n[i] <- vec.mf.n[i]+1}#Add individual
    }
  }
  #Prepare variable MF for for exporting. Note: do not move to the end
  vec.mf [is.na(vec.mf)]= 0 # clean NA values for sum
  vec.df.mf.n <- as.data.frame(vec.mf + vec.mf.n)#Sum current and cleaned columns
  colnames(vec.df.mf.n) <- c(nam.mf.n)#Generic variable name of cleaned data
  
  #Conditionals for totals M: Filling missing values
  
  #First make sure to cover all female conditions
  
  while (t.tot.m > smcol_m(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.m[i])#Targeting the missing fields to fill
          & !is.na(vec.f[i])#For female precondition for male cells
          #First fill all values where total and females are defined
          & (((vec.mf + vec.mf.n)[i] - vec.f[i]) != vec.m.n[i])
      ){while (((vec.mf + vec.mf.n)[i] - vec.f[i]) != vec.m.n[i]){
        vec.m.n[i] <- vec.m.n[i]+1#Add individual
      }
      }
    }
    for (i in 1:nrow(hpcons)){
      if(is.na(vec.m[i])#Targeting the missing fields to fill
         & is.na(vec.f[i])#2nd possibility with female values
         & (t.tot.m > smcol_m(i))#Keep sum col values under real total
         & (vec.m.n[i] != vec.df.mf.n[i,c(1)])#must be different that the MF total of the cell
         &(P_12YMAS.c$P_12YMAS_M.f [i] > vec.m.n[i])#keep by block values under 12YMAS M
      ){
        vec.m.n[i] <- vec.m.n[i]+1#Add individual
      }
    }
  }
  #Prepare variable M for exporting
  vec.m [is.na(vec.m)]= 0# clean NA values for sum
  vec.df.m.n <- as.data.frame(vec.m + vec.m.n)#Sum current and cleaned columns
  colnames(vec.df.m.n) <- c(nam.m.n)#Generic variable name of cleaned data
  
  #Conditionals for totals F: Filling missing values
  while (t.tot.f > smcol_f(i)){#while vales are not = real total population
    for (i in 1:nrow(hpcons)){#For each cell in the target variable column
      if (is.na(vec.f[i])#Targeting the missing fields to fill
          & (t.tot.f > smcol_f(i))#Keep sum col values under real total
          #Special condition: verify that male[i] keeps under total MF[i]
          & ( (vec.mf+vec.mf.n)[i] != ((vec.m+vec.m.n)[i] + vec.f.n[i]))
          #Special condition: Calculate difference of new totals
          & (( (vec.mf+vec.mf.n)[i] - (vec.m+vec.m.n)[i]) != 0)
          &(P_12YMAS.c$P_12YMAS_F.f [i] > vec.f.n[i])#keep by block values under 12YMAS M
      ){vec.f.n[i] <- vec.f.n[i]+1#Add individual
      }
    }
  }
  #Prepare variable F for for exporting
  vec.f [is.na(vec.f)]= 0# clean NA values for sum
  vec.df.f.n <- as.data.frame(vec.f + vec.f.n)#Sum current and cleaned columns
  colnames(vec.df.f.n) <- c(nam.f.n)#Generic variable name of cleaned data
  
  #Prepare FINAL data.frame of  MF, M, and F for exporting
  clean.vec <- cbind(vec.df.mf.n, vec.df.m.n, vec.df.f.n)
  return(clean.vec)#Retrieve the outcomes of the function
}




#Filling: POBTOT & P_12YMAS ------------------------

#TEST for total population - male and female
POBTOT.c <- clean.pvar(vec.mf = hpcons$POBTOT,
                       vec.m = hpcons$POBMAS,
                       vec.f = hpcons$POBFEM,
                       t.tot.mf = hptotcons$POBTOT,
                       t.tot.m = hptotcons$POBMAS,
                       t.tot.f = hptotcons$POBFEM,
                       nam.mf.n = "POBTOT.f",
                       nam.m.n ="POBMAS.f",
                       nam.f.n ="POBFEM.f")

# Run function to fill a set of 3 variables: Totals(M+F), Male, and Female
P_12YMAS.c <- clean.pvar(vec.mf = hpcons$P_12YMAS,
                         vec.m = hpcons$P_12YMAS_M,
                         vec.f = hpcons$P_12YMAS_F,
                         t.tot.mf = hptotcons$P_12YMAS,
                         t.tot.m = hptotcons$P_12YMAS_M,
                         t.tot.f = hptotcons$P_12YMAS_F,
                         nam.mf.n = "P_12YMAS.f",
                         nam.m.n ="P_12YMAS_M.f",
                         nam.f.n ="P_12YMAS_F.f")

#If all true, cleaned var matches the real total
sum(P_12YMAS.c$P_12YMAS.f) == hptotcons$P_12YMAS
sum(P_12YMAS.c$P_12YMAS_M.f) == hptotcons$P_12YMAS_M
sum(P_12YMAS.c$P_12YMAS_F.f) == hptotcons$P_12YMAS_F
sum(P_12YMAS.c$P_12YMAS.f) == (sum(P_12YMAS.c$P_12YMAS_M.f) + sum(P_12YMAS.c$P_12YMAS_F.f))
ifelse (P_12YMAS.c$P_12YMAS.f == 
          (P_12YMAS.c$P_12YMAS_M.f + P_12YMAS.c$P_12YMAS_F.f), TRUE, FALSE)


#Filling: P_nAm - Population from n to m years *ERROS SOLVED ------------------------
#(x7): 0-2,3-5,6-11,12-14*,15-17*,18-24(key),25-130 filter: 8-14, 15-49F


# Testing other other variables
P_0A2.c <- clean.pvar.P_nAm(vec.mf = hpcons$P_0A2,
                            vec.m = hpcons$P_0A2_M,
                            vec.f = hpcons$P_0A2_F,
                            t.tot.mf = hptotcons$P_0A2,
                            t.tot.m = hptotcons$P_0A2_M,
                            t.tot.f = hptotcons$P_0A2_F,
                            nam.mf.n = "P_0A2.f",
                            nam.m.n ="P_0A2_M.f",
                            nam.f.n ="P_0A2_F.f")

# Testing other other variables
P_3A5.c <- clean.pvar.P_nAm(vec.mf = hpcons$P_3A5,
                            vec.m = hpcons$P_3A5_M,
                            vec.f = hpcons$P_3A5_F,
                            t.tot.mf = hptotcons$P_3A5,
                            t.tot.m = hptotcons$P_3A5_M,
                            t.tot.f = hptotcons$P_3A5_F,
                            nam.mf.n = "P_3A5.f",
                            nam.m.n ="P_3A5_M.f",
                            nam.f.n ="P_3A5_F.f")

# Testing other other variables
P_6A11.c <- clean.pvar.P_nAm(vec.mf = hpcons$P_6A11,
                             vec.m = hpcons$P_6A11_M,
                             vec.f = hpcons$P_6A11_F,
                             t.tot.mf = hptotcons$P_6A11,
                             t.tot.m = hptotcons$P_6A11_M,
                             t.tot.f = hptotcons$P_6A11_F,
                             nam.mf.n = "P_6A11.f",
                             nam.m.n ="P_6A11_M.f",
                             nam.f.n ="P_6A11_F.f")

# Testing other other variables
# SOLVED VERIFICATION
P_12A14.c <- clean.pvar.P_nAm(vec.mf = hpcons$P_12A14,
                              vec.m = hpcons$P_12A14_M,
                              vec.f = hpcons$P_12A14_F,
                              t.tot.mf = hptotcons$P_12A14,
                              t.tot.m = hptotcons$P_12A14_M,
                              t.tot.f = hptotcons$P_12A14_F,
                              nam.mf.n = "P_12A14.f",
                              nam.m.n ="P_12A14_M.f",
                              nam.f.n ="P_12A14_F.f")

# Testing other other variables
# SOLVED VERIFICATION
P_15A17.c <- clean.pvar.P_nAm(vec.mf = hpcons$P_15A17,
                              vec.m = hpcons$P_15A17_M,
                              vec.f = hpcons$P_15A17_F,
                              t.tot.mf = hptotcons$P_15A17,
                              t.tot.m = hptotcons$P_15A17_M,
                              t.tot.f = hptotcons$P_15A17_F,
                              nam.mf.n = "P_15A17.f",
                              nam.m.n ="P_15A17_M.f",
                              nam.f.n ="P_15A17_F.f")

# Testing other other variables
P_18A24.c <- clean.pvar.P_nAm(vec.mf = hpcons$P_18A24,
                              vec.m = hpcons$P_18A24_M,
                              vec.f = hpcons$P_18A24_F,
                              t.tot.mf = hptotcons$P_18A24,
                              t.tot.m = hptotcons$P_18A24_M,
                              t.tot.f = hptotcons$P_18A24_F,
                              nam.mf.n = "P_18A24.f",
                              nam.m.n ="P_18A24_M.f",
                              nam.f.n ="P_18A24_F.f")

P_25A130.c <- as.data.frame(cbind(
  (POBTOT.c$POBTOT.f - 
     (P_0A2.c$P_0A2.f + P_3A5.c$P_3A5.f + P_6A11.c$P_6A11.f + 
        P_12A14.c$P_12A14.f + P_15A17.c$P_15A17.f + P_18A24.c$P_18A24.f)),
  
  (POBTOT.c$POBMAS.f - 
     (P_0A2.c$P_0A2_M.f + P_3A5.c$P_3A5_M.f + P_6A11.c$P_6A11_M.f + 
        P_12A14.c$P_12A14_M.f + P_15A17.c$P_15A17_M.f + P_18A24.c$P_18A24_M.f)),
  
  (POBTOT.c$POBFEM.f - 
     (P_0A2.c$P_0A2_F.f + P_3A5.c$P_3A5_F.f + P_6A11.c$P_6A11_F.f + 
        P_12A14.c$P_12A14_F.f + P_15A17.c$P_15A17_F.f + P_18A24.c$P_18A24_F.f))
))
colnames(P_25A130.c) <- c("P_25A130.f","P_25A130_M.f","P_25A130_F.f")

#Dummy variables for P_nAm (x2): P_12A130, P_0A11
P_12A130.c <- as.data.frame(cbind(
  (POBTOT.c$POBTOT.f - (P_0A2.c$P_0A2.f + P_3A5.c$P_3A5.f + P_6A11.c$P_6A11.f)),
  (POBTOT.c$POBMAS.f - (P_0A2.c$P_0A2_M.f + P_3A5.c$P_3A5_M.f + P_6A11.c$P_6A11_M.f)),
  (POBTOT.c$POBFEM.f - (P_0A2.c$P_0A2_F.f + P_3A5.c$P_3A5_F.f + P_6A11.c$P_6A11_F.f))))
colnames(P_12A130.c) <- c("P_12A130.f","P_12A130_M.f","P_12A130_F.f")


#Filling: School assistance or not (x3) *ERROS SOLVED ------------------------
#       (x3):3-5, 6-11*na.t, 12-14*


#Cleaning variable with function
P3A5_NOA.c <- clean.pvar.NOA(vec.mf = hpcons$P3A5_NOA,
                             vec.m = hpcons$P3A5_NOA_M,
                             vec.f = hpcons$P3A5_NOA_F,
                             t.tot.mf = hptotcons$P3A5_NOA,
                             t.tot.m = hptotcons$P3A5_NOA_M,
                             t.tot.f = hptotcons$P3A5_NOA_F,
                             nam.mf.n = "P3A5_NOA.f",
                             nam.m.n ="P3A5_NOA_M.f",
                             nam.f.n ="P3A5_NOA_F.f")

#Cleaning variable with function
#VERIFY: Missing totals
#Note for this variable the file
P6A11_NOA.c <- clean.pvar.NOA(vec.mf = hpcons$P6A11_NOA,
                              vec.m = hpcons$P6A11_NOAM,
                              vec.f = hpcons$P6A11_NOAF,
                              t.tot.mf = hptotcons$P6A11_NOA,
                              t.tot.m = hptotcons$P6A11_NOAM,
                              t.tot.f = hptotcons$P6A11_NOAF,
                              nam.mf.n = "P6A11_NOA.f",
                              nam.m.n ="P6A11_NOAM.f",
                              nam.f.n ="P6A11_NOAF.f")


#WARNING: PARTIALLY KNOWN VARIABLE Cleaning variable with function
#VERIFY: Few totals values

P12A14NOA.c <- clean.pvar.NOA(vec.mf = hpcons$P12A14NOA,
                              vec.m = hpcons$P12A14NOAM,
                              vec.f = hpcons$P12A14NOAF,
                              t.tot.mf = hptotcons$P12A14NOA,
                              t.tot.m = hptotcons$P12A14NOAM,
                              t.tot.f = hptotcons$P12A14NOAF,
                              nam.mf.n = "P12A14NOA.f",
                              nam.m.n ="P12A14NOAM.f",
                              nam.f.n ="P12A14NOAF.f")


#Filling: School assistance or not (x2)------------------------
#        (x2):15-17, 18-24


#Cleaning variable with function
P15A17A.c <- clean.pvar.NOA(vec.mf = hpcons$P15A17A,
                            vec.m = hpcons$P15A17A_M,
                            vec.f = hpcons$P15A17A_F,
                            t.tot.mf = hptotcons$P15A17A,
                            t.tot.m = hptotcons$P15A17A_M,
                            t.tot.f = hptotcons$P15A17A_F,
                            nam.mf.n = "P15A17A.f",
                            nam.m.n ="P15A17A_M.f",
                            nam.f.n ="P15A17A_F.f")

#Cleaning variable with function
P18A24A.c <- clean.pvar.NOA(vec.mf = hpcons$P18A24A,
                            vec.m = hpcons$P18A24A_M,
                            vec.f = hpcons$P18A24A_F,
                            t.tot.mf = hptotcons$P18A24A,
                            t.tot.m = hptotcons$P18A24A_M,
                            t.tot.f = hptotcons$P18A24A_F,
                            nam.mf.n = "P18A24A.f",
                            nam.m.n ="P18A24A_M.f",
                            nam.f.n ="P18A24A_F.f")

#Filling: PEA - Work assistance or not "WORK" (x2)------------------------
#         (x2): PEA Active 12ymas, PE_INAC Inactive 12ymas


#debugonce(clean.pvar.PEA)
#Cleaning variable with function
PEA.c <- clean.pvar.PEA(vec.mf = hpcons$PEA,
                        vec.m = hpcons$PEA_M,
                        vec.f = hpcons$PEA_F,
                        t.tot.mf = hptotcons$PEA,
                        t.tot.m = hptotcons$PEA_M,
                        t.tot.f = hptotcons$PEA_F,
                        nam.mf.n = "PEA.f",
                        nam.m.n ="PEA_M.f",
                        nam.f.n ="PEA_F.f")


#Cleaning variable with function
PE_INAC.c <- clean.pvar.PEA(vec.mf = hpcons$PE_INAC,
                            vec.m = hpcons$PE_INAC_M,
                            vec.f = hpcons$PE_INAC_F,
                            t.tot.mf = hptotcons$PE_INAC,
                            t.tot.m = hptotcons$PE_INAC_M,
                            t.tot.f = hptotcons$PE_INAC_F,
                            nam.mf.n = "PE_INAC.f",
                            nam.m.n ="PE_INAC_M.f",
                            nam.f.n ="PE_INAC_F.f")


#Final constrains variables ------------------------
#(x6): 0-11, 12-130, [NOA.f, PA.f, PEA.f, PE_INAC.f]P0A130


#NOT assist to school - (x2): NOAT.c, PAT.c
NOAT.c <- as.data.frame(cbind(
  (P_0A2.c$P_0A2.f + P3A5_NOA.c$P3A5_NOA.f + P6A11_NOA.c$P6A11_NOA.f + P12A14NOA.c$P12A14NOA.f) +
    (P_15A17.c$P_15A17.f - P15A17A.c$P15A17A.f) + (P_18A24.c$P_18A24.f - P18A24A.c$P18A24A.f)+
    (P_25A130.c$P_25A130.f)))
colnames(NOAT.c) <- c("NOA")

#Assist to school - (x2): NOAT.c, PAT.c
PAT.c <- as.data.frame(cbind(
  (P_3A5.c$P_3A5.f - P3A5_NOA.c$P3A5_NOA.f) + (P_6A11.c$P_6A11.f - P6A11_NOA.c$P6A11_NOA.f)+
    (P_12A14.c$P_12A14.f - P12A14NOA.c$P12A14NOA.f) + (P15A17A.c$P15A17A.f) + (P18A24A.c$P18A24A.f)))
colnames(PAT.c) <- c("PA")

#Verification NOAT.c, PAT.c
sum(NOAT.c + PAT.c) == sum(POBTOT.c$POBTOT.f)
ifelse (POBTOT.c$POBTOT.f == 
          (PAT.c$PAT.f + NOAT.c$NOAT.f), TRUE, FALSE)

#Keeping variables integrity 0A11 & 12YMAS == POBTOT
P_0A11.c <- as.data.frame(cbind(
  POBTOT.c$POBTOT.f - P_12YMAS.c$P_12YMAS.f))
colnames(P_0A11.c) <- c("P_0A11.f")

#NOT going to work  - (x2): PE_INACT, PEAT
PE_INACT.c <- as.data.frame(cbind(
  PE_INAC.c$PE_INAC.f + (P_0A11.c$P_0A11.f)  
  #Due to census discrepancies in original data, compensation of values is required
  + (P_12YMAS.c$P_12YMAS.f - (PEA.c$PEA.f + PE_INAC.c$PE_INAC.f))
))
colnames(PE_INACT.c) <- c("PE_INAC")

#Go to work  - (x2): PE_INACT, PEAT
PEAT.c <- as.data.frame(cbind(
  PEA.c$PEA.f ))
colnames(PEAT.c) <- c("PEA")


#EXPORT RESULTS - TABLE with all variables ------------------------
# (x4): hpcons.c, hpcons1, hpcons2, hpcons3


hpcons.c <- cbind(
  P_0A2.c, P_3A5.c, P_6A11.c, P_12A14.c, P_15A17.c, P_18A24.c, P_25A130.c,
  NOAT.c, PAT.c, 
  PE_INACT.c, PEAT.c)

hpcons1 <- as.data.frame(cbind(
  P_0A2.c$P_0A2_M.f, 
  P_3A5.c$P_3A5_M.f, 
  P_6A11.c$P_6A11_M.f, 
  P_12A14.c$P_12A14_M.f, 
  P_15A17.c$P_15A17_M.f,
  P_18A24.c$P_18A24_M.f, 
  P_25A130.c$P_25A130_M.f, 
  
  P_0A2.c$P_0A2_F.f, 
  P_3A5.c$P_3A5_F.f, 
  P_6A11.c$P_6A11_F.f,
  P_12A14.c$P_12A14_F.f,
  P_15A17.c$P_15A17_F.f,
  P_18A24.c$P_18A24_F.f,
  P_25A130.c$P_25A130_F.f
))

colnames(hpcons1) <- c(
  "mP_0A2",
  "mP_3A5",
  "mP_6A11",
  "mP_12A14",
  "mP_15A17",
  "mP_18A24",
  "mP_25A130",
  
  "fP_0A2",
  "fP_3A5",
  "fP_6A11",
  "fP_12A14",
  "fP_15A17",
  "fP_18A24",
  "fP_25A130"
)


hpcons2 <- cbind(PAT.c, NOAT.c)

hpcons3 <- cbind(PEAT.c, PE_INACT.c)

hpcons0 <- cbind(
  hpcons1, hpcons2, hpcons3)

#no need to write hpcons0.csv
#write.csv(hpcons0, "code/hpcons0.csv", row.names = F)

#RMD: Inputs for Evaluation graphs  ------------------------
# (x4): hpcons.c, hpcons1, hpcons2, hpcons3

hpcons.estima <- cbind(
  POBTOT.c, 
  P_12YMAS.c,
  P_0A2.c, 
  P_3A5.c, 
  P_6A11.c, 
  P_12A14.c, 
  P_15A17.c, 
  P_18A24.c, 
  
  P3A5_NOA.c, 
  P6A11_NOA.c, 
  P12A14NOA.c,
  P15A17A.c, 
  P18A24A.c,

  PEA.c, 
  PE_INAC.c
)


hptotcons.observ <- hptotcons %>% 
  select( 
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
    P15A17A, P15A17A_M, P15A17A_F,
    P18A24A, P18A24A_M, P18A24A_F,
    
    PEA, PEA_M, PEA_F,
    PE_INAC, PE_INAC_M, PE_INAC_F
  ) %>% 
  as.data.frame()
  

remove(
  clean.pvar, clean.pvar.NOA, clean.pvar.P_nAm, clean.pvar.PEA,
  # smcol_mf, smcol_f, smcol_m, smrow_mf,smrow_f, smrow_m,
  
  POBTOT.c, P_12YMAS.c,
  P_0A2.c, P_3A5.c, P_6A11.c, P_12A14.c, P_15A17.c, P_18A24.c, P_25A130.c,
  P_12A130.c, P_0A11.c,
  
  P3A5_NOA.c, P6A11_NOA.c, P12A14NOA.c, P15A17A.c, P18A24A.c,
  NOAT.c, PAT.c,
  
  PEA.c, PE_INAC.c,
  PE_INACT.c, PEAT.c, hpcons.c
)



#VERIFICATION ------------------------
#Un-comment (Ctrl + Shift + C ) if you want to verify if conditions are fulfilled


# #If all true, cleaned var matches the real total
# sum(POBTOT.c$POBTOT.f) == hptotcons$POBTOT
# sum(POBTOT.c$POBMAS.f) == hptotcons$POBMAS
# sum(POBTOT.c$POBFEM.f) == hptotcons$POBFEM
# sum(POBTOT.c$POBTOT.f) == (sum(POBTOT.c$POBMAS.f) + sum(POBTOT.c$POBFEM.f))
# ifelse (POBTOT.c$POBTOT.f ==
#           (POBTOT.c$POBMAS.f + POBTOT.c$POBFEM.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P_12YMAS.c$P_12YMAS.f) == hptotcons$P_12YMAS
# sum(P_12YMAS.c$P_12YMAS_M.f) == hptotcons$P_12YMAS_M
# sum(P_12YMAS.c$P_12YMAS_F.f) == hptotcons$P_12YMAS_F
# sum(P_12YMAS.c$P_12YMAS.f) == (sum(P_12YMAS.c$P_12YMAS_M.f) + sum(P_12YMAS.c$P_12YMAS_F.f))
# ifelse (P_12YMAS.c$P_12YMAS.f ==
#           (P_12YMAS.c$P_12YMAS_M.f + P_12YMAS.c$P_12YMAS_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P_0A2.c$P_0A2.f) == hptotcons$P_0A2
# sum(P_0A2.c$P_0A2_M.f) == hptotcons$P_0A2_M
# sum(P_0A2.c$P_0A2_F.f) == hptotcons$P_0A2_F
# sum(P_0A2.c$P_0A2.f) == (sum(P_0A2.c$P_0A2_M.f) + sum(P_0A2.c$P_0A2_F.f))
# ifelse (P_0A2.c$P_0A2.f ==
#           (P_0A2.c$P_0A2_M.f + P_0A2.c$P_0A2_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P_3A5.c$P_3A5.f) == hptotcons$P_3A5
# sum(P_3A5.c$P_3A5_M.f) == hptotcons$P_3A5_M
# sum(P_3A5.c$P_3A5_F.f) == hptotcons$P_3A5_F
# sum(P_3A5.c$P_3A5.f) == (sum(P_3A5.c$P_3A5_M.f) + sum(P_3A5.c$P_3A5_F.f))
# ifelse (P_3A5.c$P_3A5.f ==
#           (P_3A5.c$P_3A5_M.f + P_3A5.c$P_3A5_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P_6A11.c$P_6A11.f) == hptotcons$P_6A11
# sum(P_6A11.c$P_6A11_M.f) == hptotcons$P_6A11_M
# sum(P_6A11.c$P_6A11_F.f) == hptotcons$P_6A11_F
# sum(P_6A11.c$P_6A11.f) == (sum(P_6A11.c$P_6A11_M.f) + sum(P_6A11.c$P_6A11_F.f))
# ifelse (P_6A11.c$P_6A11.f ==
#           (P_6A11.c$P_6A11_M.f + P_6A11.c$P_6A11_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P_12A14.c$P_12A14.f) == hptotcons$P_12A14
# sum(P_12A14.c$P_12A14_M.f) == hptotcons$P_12A14_M
# sum(P_12A14.c$P_12A14_F.f) == hptotcons$P_12A14_F
# sum(P_12A14.c$P_12A14.f) == (sum(P_12A14.c$P_12A14_M.f) + sum(P_12A14.c$P_12A14_F.f))
# ifelse (P_12A14.c$P_12A14.f ==
#           (P_12A14.c$P_12A14_M.f + P_12A14.c$P_12A14_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P_15A17.c$P_15A17.f) == hptotcons$P_15A17
# sum(P_15A17.c$P_15A17_M.f) == hptotcons$P_15A17_M
# sum(P_15A17.c$P_15A17_F.f) == hptotcons$P_15A17_F
# sum(P_15A17.c$P_15A17.f) == (sum(P_15A17.c$P_15A17_M.f) + sum(P_15A17.c$P_15A17_F.f))
# ifelse (P_15A17.c$P_15A17.f ==
#           (P_15A17.c$P_15A17_M.f + P_15A17.c$P_15A17_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P_18A24.c$P_18A24.f) == hptotcons$P_18A24
# sum(P_18A24.c$P_18A24_M.f) == hptotcons$P_18A24_M
# sum(P_18A24.c$P_18A24_F.f) == hptotcons$P_18A24_F
# sum(P_18A24.c$P_18A24.f) == (sum(P_18A24.c$P_18A24_M.f) + sum(P_18A24.c$P_18A24_F.f))
# ifelse (P_18A24.c$P_18A24.f ==
#           (P_18A24.c$P_18A24_M.f + P_18A24.c$P_18A24_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# ifelse (P_25A130.c$P_25A130.f ==
#           (P_25A130.c$P_25A130_M.f + P_25A130.c$P_25A130_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P3A5_NOA.c$P3A5_NOA.f) == hptotcons$P3A5_NOA
# sum(P3A5_NOA.c$P3A5_NOA_M.f) == hptotcons$P3A5_NOA_M
# sum(P3A5_NOA.c$P3A5_NOA_F.f) == hptotcons$P3A5_NOA_F
# sum(P3A5_NOA.c$P3A5_NOA.f) == (sum(P3A5_NOA.c$P3A5_NOA_M.f) + sum(P3A5_NOA.c$P3A5_NOA_F.f))
# ifelse (P3A5_NOA.c$P3A5_NOA.f ==
#           (P3A5_NOA.c$P3A5_NOA_M.f + P3A5_NOA.c$P3A5_NOA_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P6A11_NOA.c$P6A11_NOA.f) == hptotcons$P6A11_NOA
# sum(P6A11_NOA.c$P6A11_NOAM.f) == hptotcons$P6A11_NOAM
# sum(P6A11_NOA.c$P6A11_NOAF.f) == hptotcons$P6A11_NOAF
# sum(P6A11_NOA.c$P6A11_NOA.f) == (sum(P6A11_NOA.c$P6A11_NOAM.f) + sum(P6A11_NOA.c$P6A11_NOAF.f))
# ifelse (P6A11_NOA.c$P6A11_NOA.f ==
#           (P6A11_NOA.c$P6A11_NOAM.f + P6A11_NOA.c$P6A11_NOAF.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P12A14NOA.c$P12A14NOA.f) == hptotcons$P12A14NOA
# sum(P12A14NOA.c$P12A14NOAM.f) == hptotcons$P12A14NOAM
# sum(P12A14NOA.c$P12A14NOAF.f) == hptotcons$P12A14NOAF
# sum(P12A14NOA.c$P12A14NOA.f) == (sum(P12A14NOA.c$P12A14NOAM.f) + sum(P12A14NOA.c$P12A14NOAF.f))
# ifelse (P12A14NOA.c$P12A14NOA.f ==
#           (P12A14NOA.c$P12A14NOAM.f + P12A14NOA.c$P12A14NOAF.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P15A17A.c$P15A17A.f) == hptotcons$P15A17A
# sum(P15A17A.c$P15A17A_M.f) == hptotcons$P15A17A_M
# sum(P15A17A.c$P15A17A_F.f) == hptotcons$P15A17A_F
# sum(P15A17A.c$P15A17A.f) == (sum(P15A17A.c$P15A17A_M.f) + sum(P15A17A.c$P15A17A_F.f))
# ifelse (P15A17A.c$P15A17A.f ==
#           (P15A17A.c$P15A17A_M.f + P15A17A.c$P15A17A_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(P18A24A.c$P18A24A.f) == hptotcons$P18A24A
# sum(P18A24A.c$P18A24A_M.f) == hptotcons$P18A24A_M
# sum(P18A24A.c$P18A24A_F.f) == hptotcons$P18A24A_F
# sum(P18A24A.c$P18A24A.f) == (sum(P18A24A.c$P18A24A_M.f) + sum(P18A24A.c$P18A24A_F.f))
# ifelse (P18A24A.c$P18A24A.f ==
#           (P18A24A.c$P18A24A_M.f + P18A24A.c$P18A24A_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(PEA.c$PEA.f) == hptotcons$PEA
# sum(PEA.c$PEA_M.f) == hptotcons$PEA_M
# sum(PEA.c$PEA_F.f) == hptotcons$PEA_F
# sum(PEA.c$PEA.f) == (sum(PEA.c$PEA_M.f) + sum(PEA.c$PEA_F.f))
# ifelse (PEA.c$PEA.f ==
#           (PEA.c$PEA_M.f + PEA.c$PEA_F.f), TRUE, FALSE)
# 
# #If all true, cleaned var matches the real total
# sum(PE_INAC.c$PE_INAC.f) == hptotcons$PE_INAC
# sum(PE_INAC.c$PE_INAC_M.f) == hptotcons$PE_INAC_M
# sum(PE_INAC.c$PE_INAC_F.f) == hptotcons$PE_INAC_F
# sum(PE_INAC.c$PE_INAC.f) == (sum(PE_INAC.c$PE_INAC_M.f) + sum(PE_INAC.c$PE_INAC_F.f))
# ifelse (PE_INAC.c$PE_INAC.f ==
#           (PE_INAC.c$PE_INAC_M.f + PE_INAC.c$PE_INAC_F.f), TRUE, FALSE)
# 
# #FINALLY verify sum of total population and rows
# #Sum of MF variables cleaned vs POBTOT
# sum(POBTOT.c$POBTOT.f)
# sum(P_25A130.c$P_25A130.f,
#     P_18A24.c$P_18A24.f,
#     P_15A17.c$P_15A17.f,
#     P_12A14.c$P_12A14.f,
#     P_6A11.c$P_6A11.f,
#     P_3A5.c$P_3A5.f,
#     P_0A2.c$P_0A2.f)
# #Sum of separated Male - Female
# sum(P_25A130.c$P_25A130_F.f,
#     P_18A24.c$P_18A24_F.f,
#     P_15A17.c$P_15A17_F.f,
#     P_12A14.c$P_12A14_F.f,
#     P_6A11.c$P_6A11_F.f,
#     P_3A5.c$P_3A5_F.f,
#     P_0A2.c$P_0A2_F.f,
#     P_25A130.c$P_25A130_M.f,
#     P_18A24.c$P_18A24_M.f,
#     P_15A17.c$P_15A17_M.f,
#     P_12A14.c$P_12A14_M.f,
#     P_6A11.c$P_6A11_M.f,
#     P_3A5.c$P_3A5_M.f,
#     P_0A2.c$P_0A2_M.f)
# #Sum of Female cleaned vs POBFEM
# sum(POBTOT.c$POBFEM.f)
# sum(P_25A130.c$P_25A130_F.f,
#     P_18A24.c$P_18A24_F.f,
#     P_15A17.c$P_15A17_F.f,
#     P_12A14.c$P_12A14_F.f,
#     P_6A11.c$P_6A11_F.f,
#     P_3A5.c$P_3A5_F.f,
#     P_0A2.c$P_0A2_F.f)
# #Sum of Male cleaned vs POBMAS
# sum(POBTOT.c$POBMAS.f)
# sum(P_25A130.c$P_25A130_M.f,
#     P_18A24.c$P_18A24_M.f,
#     P_15A17.c$P_15A17_M.f,
#     P_12A14.c$P_12A14_M.f,
#     P_6A11.c$P_6A11_M.f,
#     P_3A5.c$P_3A5_M.f,
#     P_0A2.c$P_0A2_M.f)
# #Row comparison of estimated vs real for M+F
# ifelse (POBTOT.c$POBTOT.f ==
#         (P_25A130.c$P_25A130.f+
#            P_18A24.c$P_18A24.f+
#            P_15A17.c$P_15A17.f+
#            P_12A14.c$P_12A14.f+
#            P_6A11.c$P_6A11.f+
#            P_3A5.c$P_3A5.f+
#            P_0A2.c$P_0A2.f), TRUE, FALSE)
# #Row comparison of estimated vs real for M
# ifelse (POBTOT.c$POBMAS.f ==
#         (P_25A130.c$P_25A130_M.f+
#            P_18A24.c$P_18A24_M.f+
#            P_15A17.c$P_15A17_M.f+
#            P_12A14.c$P_12A14_M.f+
#            P_6A11.c$P_6A11_M.f+
#            P_3A5.c$P_3A5_M.f+
#            P_0A2.c$P_0A2_M.f), TRUE, FALSE)
# #Row comparison of estimated vs real for F
# ifelse (POBTOT.c$POBFEM.f ==
#         (P_25A130.c$P_25A130_F.f+
#            P_18A24.c$P_18A24_F.f+
#            P_15A17.c$P_15A17_F.f+
#            P_12A14.c$P_12A14_F.f+
#            P_6A11.c$P_6A11_F.f+
#            P_3A5.c$P_3A5_F.f+
#            P_0A2.c$P_0A2_F.f), TRUE, FALSE)
# #Differences by F+M totals
# POBTOT.c$POBTOT.f -
#     (P_25A130.c$P_25A130.f+
#        P_18A24.c$P_18A24.f+
#        P_15A17.c$P_15A17.f+
#        P_12A14.c$P_12A14.f+
#        P_6A11.c$P_6A11.f+
#        P_3A5.c$P_3A5.f+
#        P_0A2.c$P_0A2.f)
# #Differences by M totals
# POBTOT.c$POBMAS.f -
#           (P_25A130.c$P_25A130_M.f+
#              P_18A24.c$P_18A24_M.f+
#              P_15A17.c$P_15A17_M.f+
#              P_12A14.c$P_12A14_M.f+
#              P_6A11.c$P_6A11_M.f+
#              P_3A5.c$P_3A5_M.f+
#              P_0A2.c$P_0A2_M.f)
# #Differences by F totals
# POBTOT.c$POBFEM.f -
#           (P_25A130.c$P_25A130_F.f+
#              P_18A24.c$P_18A24_F.f+
#              P_15A17.c$P_15A17_F.f+
#              P_12A14.c$P_12A14_F.f+
#              P_6A11.c$P_6A11_F.f+
#              P_3A5.c$P_3A5_F.f+
#              P_0A2.c$P_0A2_F.f)
# #Sum of Differences by F+M and real totals
# sum(POBTOT.c$POBTOT.f -
#   (P_25A130.c$P_25A130.f+
#      P_18A24.c$P_18A24.f+
#      P_15A17.c$P_15A17.f+
#      P_12A14.c$P_12A14.f+
#      P_6A11.c$P_6A11.f+
#      P_3A5.c$P_3A5.f+
#      P_0A2.c$P_0A2.f))
# 
# #Verification PE_INACT, PEAT
#   sum(P_0A11.c$P_0A11.f, P_12YMAS.c$P_12YMAS.f) == sum(POBTOT.c$POBTOT.f)
#   sum(POBTOT.c$POBTOT.f)
#   sum(PEAT.c + PE_INACT.c)
#   sum(PEAT.c + PE_INACT.c) == sum(POBTOT.c$POBTOT.f)
#   PEAT.c$PEAT.f
#   PE_INACT.c$PE_INACT.f
# 
#   ifelse (POBTOT.c$POBTOT.f ==
#              (PEAT.c$PEAT.f + PE_INACT.c$PE_INACT.f), TRUE, FALSE)


