library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(lubridate)
library(plyr)
library(readr)
library(janitor)
library(DT)
library(vtree)

# Read Text File in the R Script 

df1 <- read.csv(file = 'C:/Users/User/Desktop/Data/medication.csv')
df11 <- read.csv(file = 'C:/Users/User/Desktop/Data/covariates_Markus.csv')
data <- read.table(file = 'C:/Users/User/Desktop/Data/hesin_diag.txt', header = TRUE ,sep = "\t")
data2 <- read.table(file = 'C:/Users/User/Desktop/Data/hesin.txt', header = TRUE , sep = "\t")

setnames(df3, old=colnames(df3), new = c('Eid','x','Patient_1','Patient_2','Patient_3','Patient_4','Patient_5','Patient_6','Patient_7','Patient_8','Patient_9','Patient_10','Patient_11','Patient_12','Patient_13','Patient_14','Patient_15','Patient_16','Patient_17','Patient_18','Patient_19','Patient_20' ,'Patient_21','Patient_22','Patient_23','Patient_24','Patient_25','Patient_26','Patient_27','Patient_28','Patient_29','Patient_30','Patient_31','Patient_32','Patient_33','Patient_34','Patient_35','Patient_36','Patient_37','Patient_38','Patient_39','Patient_40','Patient_41','Patient_42','Patient_43','Patient_44','Patient_45','Patient_46','Patient_47','Patient_48'))
setnames(data,"requires.eid",'eid_1')
setnames(df11, old=colnames(df11), new = c('No.','Eid_1','Birth_Year','Birth_Month','BMI','Age_at_Obs.','Gender','Fruit_Intake','Oily_Fish'))

# 1.1 Find Common Ids in the Data frame:

common <- intersect(data$eid, data2$eid_1)
data_common = data[common, ]
data2_common = data2[common, ]
common_comb = merge(data, data2, by.x=c('eid_1','ins_index'), by.y=c('eid','ins_index'))%>% 
  select('eid_1','diag_icd10', 'epistart', 'epiend', 'admidate', 'disdate')
df2 = common_comb

## 1.2 Medications

common2 <- intersect(df2$eid_1, df1$eid)
df1_common = df1[common2, ]
df2_common = df2[common2, ]
common2_comb = merge(df1, df2, by.x=c('eid'), by.y=c('eid_1'))
df3 = common2_comb
setnames(df3, old=colnames(df3), new = c('Eid','x','Patient_1','Patient_2','Patient_3','Patient_4','Patient_5','Patient_6','Patient_7','Patient_8','Patient_9','Patient_10','Patient_11','Patient_12','Patient_13','Patient_14','Patient_15','Patient_16','Patient_17','Patient_18','Patient_19','Patient_20' ,'Patient_21','Patient_22','Patient_23','Patient_24','Patient_25','Patient_26','Patient_27','Patient_28','Patient_29','Patient_30','Patient_31','Patient_32','Patient_33','Patient_34','Patient_35','Patient_36','Patient_37','Patient_38','Patient_39','Patient_40','Patient_41','Patient_42','Patient_43','Patient_44','Patient_45','Patient_46','Patient_47','Patient_48','Diag-code','epistart','epiend','admidate','disdate'))

## Medication with Diagnosis info.

common3 <- intersect(df3$Eid , df11$eid)
df3_common = df3[common3, ]
df11_common = df11[common3,]
common3_comb = merge(df3, df11, by.x=c('Eid'), by.y=c('Eid_1'))%>% 
  select('Eid','Patient_1', 'Patient_2','Patient_3','Patient_4','Patient_5','Patient_6','Patient_7','Patient_8','Patient_9','Patient_10','Patient_11','Patient_12','Patient_13','Patient_14','Patient_15','Patient_16','Patient_17','Patient_18','Patient_19','Patient_20' ,'Patient_21','Patient_22','Patient_23','Patient_24','Patient_25','Patient_26','Patient_27','Patient_28','Patient_29','Patient_30','Patient_31','Patient_32','Patient_33','Patient_34','Patient_35','Patient_36','Patient_37','Patient_38','Patient_39','Patient_40','Patient_41','Patient_42','Patient_43','Patient_44','Patient_45','Patient_46','Patient_47','Patient_48','Diag-code','epistart','epiend', 'admidate', 'disdate', 'BMI','Gender','Birth_Year')
df4 = common3_comb

## 1.4 Metabolites

common4 <- intersect(df4$Eid, df22$eid)
df4_common = df4[common3, ]
df22_common = df22[common3, ]
common4_comb = merge(df4, df22, by.x=c('Eid'), by.y=c('eid'))
df5 = common4_comb

common5 <- intersect(df5$eid, data3$eid)
df5_common = df5[common4, ]
data3_common = data3[common4, ]
common5_comb = merge(df5, data3, by.x=c('Eid'), by.y=c('eid'))
df6 = common5_comb


# Medications frequent occurrence in patients  

occurance <- data.frame(table(df3$Patient_1))
occurance2 = occurance[occurance$Freq > 200000,]

# Mutation of data for Medication

sapply(df3$epistart, typeof)
class(df3$epistart)
df3$epistart <- dmy(df3$epistart)
df3$epiend <- dmy(df3$epiend)
class(as.Date(df3$admidate, "%d%m%y"))
class(as.Date(df3$disdate, "%d%m%y"))

# Days difference

df3$date_diff = difftime(df3$epiend,df3$epistart,units = "days")

# Changing data values

for (i in seq_along(df3)) { df3[[i]][df3[[i]] %in% 1140875408] <- "allopurinol" } %>% { df3[[i]][df3[[i]] %in% 1140916682] <- "evening primrose oil" } %>% { df3[[i]][df3[[i]] %in% 1140884412] <- "sumatriptan" } %>% { df3[[i]][df3[[i]] %in% 1140910814] <- "sodium thyroxine" } %>% { df3[[i]][df3[[i]] %in% 1140863152] <- "diazepam" } %>% { df3[[i]][df3[[i]] %in% 1140851028] <- "chalk" } %>% { df3[[i]][df3[[i]] %in% 1140883504] <- "cetirizine" } %>% { df3[[i]][df3[[i]] %in% 1140865580] <- "asacol-400mg-e/c-tablet" }
for (i in seq_along(df3)) { df3[[i]][df3[[i]] %in% 1140868226] <- "Aspirin" } %>% { df3[[i]][df3[[i]] %in% 1140879616] <- "amitriptyline" } %>% { df3[[i]][df3[[i]] %in% 1140922174] <- "alendronate_sodium"} %>% { df3[[i]][df3[[i]] %in% 1140866738] <- "atenolol" } %>% { df3[[i]][df3[[i]] %in% 1141146234] <- "atorvastatin" } %>% { df3[[i]][df3[[i]] %in% 1141194794] <- "bendroflumethiazide" } %>% { df3[[i]][df3[[i]] %in% 1140870390] <- "ferrous sulphate" } %>% { df3[[i]][df3[[i]] %in% 1141188442] <- "glucosamine product" } %>% { df3[[i]][df3[[i]] %in% 1140871310] <- "ibuprofen" } %>% { df3[[i]][df3[[i]] %in% 1140888366] <- "thiamine_preparation" } %>% { df3[[i]][df3[[i]] %in% 1140860696] <- "lisinopril" } %>% { df3[[i]][df3[[i]] %in% 1140864752] <- "lansoprazole" } %>% { df3[[i]][df3[[i]] %in% 1140865634] <- "omeprazole" } 
for (i in seq_along(df3)) { df3[[i]][df3[[i]] %in% 1140861958] <- "simvastatin" } %>% { df3[[i]][df3[[i]] %in% 1140871024] <- "vitamin_b_compound_tablet" } %>% { df3[[i]][df3[[i]] %in% 1140879760] <- "bisoprolol" } %>% { df3[[i]][df3[[i]] %in% 1140879802] <- "amlodipine" } %>% { df3[[i]][df3[[i]] %in% 1140909674] <- "cod_liver_oil_capsule" } %>% { df3[[i]][df3[[i]] %in% 1140923346] <- "co-codamol" } %>% { df3[[i]][df3[[i]] %in% 1141182628] <- "tiotropium" } %>% { df3[[i]][df3[[i]] %in% 1141176832] <- "seretide_50_evohaler" } %>% { df3[[i]][df3[[i]] %in% 1141145660] <- "valsartan" } %>% { df3[[i]][df3[[i]] %in% 1140863144] <- "zopiclone" } %>% { df3[[i]][df3[[i]] %in% 1140926606] <- "salbutamol_100micrograms_spacehaler" } %>% { df3[[i]][df3[[i]] %in% 1140888510] <- "verapamil" }
for (i in seq_along(df3)) { df3[[i]][df3[[i]] %in% 1140883066] <- "insulin product" } %>% { df3[[i]][df3[[i]] %in% 1140860806] <- "ramipril" } %>% { df3[[i]][df3[[i]] %in% 1140909708] <- "furosemide" }%>% { df3[[i]][df3[[i]] %in% 99999] <- "uncoded" } %>% { df3[[i]][df3[[i]] %in% 1140864992] <- "tramadol" } %>% { df3[[i]][df3[[i]] %in% 1140884600] <- "metformin" } %>% { df3[[i]][df3[[i]] %in% 1140865716] <- "senna" } %>% { df3[[i]][df3[[i]] %in% 2038460150] <- "paracetamol" } %>% { df3[[i]][df3[[i]] %in% 1140874930] <- "prednisolone" } %>% { df3[[i]][df3[[i]] %in% 1140888266] <- "warfarin" } %>% { df3[[i]][df3[[i]] %in% 1140910766] <- "nicorandil" } %>% { df3[[i]][df3[[i]] %in% 1140874420] <- "quinine" }  %>% { df3[[i]][df3[[i]] %in% 1140879406] <- "ranitidine" } %>% { df3[[i]][df3[[i]] %in% 1140881856] <- "salbutamol" } %>% { df3[[i]][df3[[i]] %in% 1141191044] <- "levothyroxine sodium" } 

# ICD code files

df6 = df5[df5$diag_icd10 =='C760',]
setDT(df6)
df7 = df6[,1:19 ]

# Metabolites Categorization(BMI,AGE)
#Removes NAs
df22 <- na.omit(df22)
df6 <- na.omit(df6)
data3 <- na.omit(data3)

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
