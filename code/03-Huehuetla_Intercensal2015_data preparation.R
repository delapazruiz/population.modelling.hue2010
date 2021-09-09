# Rename the categories in "hind" to correspond to the one of "hpcons1,2,3"
# Test binning the age variable 
brks <- c(-1,2,5,11,14,17,24,130) # set break points from 0 to 120 years 
cut(hind$EDAD, breaks = brks) # bin the age variable

# Convert age into a categorical variable 
labs <- c("P_0A2", "P_3A5", "P_6A11", "P_12A14",
          "P_15A17", "P_18A24", "P_25A130") # create the labels 
cut(hind$EDAD, breaks = brks, labels = labs)

# Overwrite the age variable with categorical age bands 
hind$EDAD <- cut(hind$EDAD, breaks = brks, labels = labs)
any(is.na(hind$EDAD))
#remove(hindfull)

#Change 3 of female to 2
hind$SEXO[hind$SEXO == 3] <- 2
#Replace numbers for letters
hind$SEXO <- sapply(hind$SEXO, FUN = switch, "m","f")

#Change 1 for assisting to school(PA), 2 for no assisting(NOA)
hind$ASISTEN[is.na(hind$ASISTEN)] <- 2
hind$ASISTEN [hind$ASISTEN == 7] <- 2
hind$ASISTEN [hind$ASISTEN == 9] <- 2
hind$ASISTEN [hind$ASISTEN == 5] <- 1

#Define character instead of 1,2.
hind$ASISTEN <- sapply(hind$ASISTEN, FUN = switch, "PA", "NOA")

#Change 1 for assisting to school(PA), 2 for no assisting(NOA)
hind$CONACT[is.na(hind$CONACT)] <- 2
hind$CONACT [hind$CONACT == 20] <- 2
hind$CONACT [hind$CONACT == 31] <- 2
hind$CONACT [hind$CONACT == 32] <- 2
hind$CONACT [hind$CONACT == 33] <- 2
hind$CONACT [hind$CONACT == 34] <- 2
hind$CONACT [hind$CONACT == 35] <- 2
hind$CONACT [hind$CONACT == 99] <- 2

hind$CONACT [hind$CONACT == 10] <- 1
hind$CONACT [hind$CONACT == 11] <- 1
hind$CONACT [hind$CONACT == 12] <- 1
hind$CONACT [hind$CONACT == 13] <- 1
hind$CONACT [hind$CONACT == 14] <- 1
hind$CONACT [hind$CONACT == 15] <- 1
hind$CONACT [hind$CONACT == 16] <- 1

hind$CONACT <- sapply(hind$CONACT, FUN = switch, "PEA", "PE_INAC")

remove(labs, brks)


