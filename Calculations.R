
# ICD code files
df5 = df4%>%
  select('Eid','Patient_1','Patient_2','Patient_3','Patient_4','Patient_5','Patient_6','Patient_7','Patient_8','Patient_9','Patient_10','Patient_11','Patient_12','Patient_13','Patient_14','Patient_15','Patient_16','Patient_17','Patient_18','Patient_19')
df6 = df5[df5$diag_icd10 =='C760',]
setDT(df6)

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
Hepa = df4%>%
  select('Eid','Total_Free_Cholesterol','Liver_fat','Concentration_of_HDL','Total_Triglycerides','Concentration_of_HDL','LDL','IDL')
max(Hepa$Concentration_of_HDL, na.rm = TRUE)
Hepa1 = filter(Hepa,Liver_fat < 5 & Concentration_of_HDL < 0.050 & LDL < 0.100 & Total_Triglycerides < 1.5 & Total_Free_Cholesterol < 2, )


df7 = df6[!duplicated(df6), ]
# filter(df4, Total_Triglycerides < 1.7 , )
 
#Check NAs in column

No_medication = filter(df5, rowSums(is.na(df5)) == ncol(df5)-1)
print(df5$Eid)
No_medication_1 <- No_medication[!duplicated(No_medication), ]
write.csv(No_medication, "C:/Users/User/Desktop/PhD Documentation/My drafts/No_Medication.csv", row.names=FALSE)
write.csv(Healthy_Patients, "C:/Users/User/Desktop/PhD Documentation/My drafts/Healthy_Patients.csv", row.names=FALSE)

#Graphical Observation
vtree(df6, c("diag_icd10","Sex","BMI_group","age_group"))
vtree(df6, c("diag_icd10","Sex","BMI_group","age_group"))
pie(occurance2, by.x=c(Freq), by.y=c(Var1))