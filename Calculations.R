
# ICD code files

df6 = df5[df5$diag_icd10 =='C760',]
setDT(df6)
df7 = df6[,1:19 ]

# Metabolites Categorization(BMI,AGE)
#Removes NAs

# BMI

df22[df22$BMI > 18.5 & df22$BMI <= 25, "BMI_group"] <- "18.5-25"
df22[df22$BMI > 25 & df22$BMI <= 30, "BMI_group"] <- "25-30"
df22[df22$BMI > 30, "BMI_group"] <- "> 30"

# Age 

df22[df22$Age <= 12, "age_group"] <- "0-12"
df22[df22$Age > 13 & df22$Age <= 19, "age_group"] <- "13-19"
df22[df22$Age > 20 & df22$Age <= 35, "age_group"] <- "20-35"
df22[df22$Age > 35 & df22$Age <= 50, "age_group"] <- "35-50"
df22[df22$Age > 50 & df22$Age <= 65, "age_group"] <- "50-65"
df22[df22$Age > 65, "age_group"] <- "> 65"

# Healthy Patient criteria

df7 = filter(df6,Liver_fat < 5 & HDL_Cholesterol > 0.700 & LDL_Cholesterol > 1.00, )
unique(df7$Eid)

df12 = filter(Total_Triglycerides < 0.150 & Total_Free_Cholesterol < 0.200, )

#Check NAs in column
df7 <-df6[,19]
df8 = filter(df6, rowSums(is.na(df6)) == ncol(df6)-1)
print(df8$Eid)
df9 <- df8[!duplicated(df8), ]

#Graphical Observation
vtree(df6, c("diag_icd10","Sex","BMI_group","age_group"))
pie(occurance2, by.x=c(Freq), by.y=c(Var1))
