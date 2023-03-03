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

medication <- read.csv(file = 'C:/Users/User/Desktop/Data/medication.csv')[ ,1:21]
covariates <- read.csv(file = 'C:/Users/User/Desktop/Data/covariates_Markus.csv')[ ,2:7]
hesin_diag <- read.table(file = 'C:/Users/User/Desktop/Data/hesin_diag.txt' ,header = TRUE , sep = "\t" )%>% 
  select('requires.eid','ins_index','diag_icd10')
hesin <- read.table(file = 'C:/Users/User/Desktop/Data/hesin.txt', header = TRUE , sep = "\t") %>% 
  select('eid','ins_index','epistart','epiend','disdate','admidate')
metabolites <- read.csv(file = 'C:/Users/User/Desktop/Data/metabolites.csv')
mri <- read.csv(file = 'C:/Users/User/Desktop/Data/MRI.csv')%>%
  select('eid','X40061.2.0')
#After setnames of metabolites#
metabolite <- metabolites%>%
  select('Eid','Total_Free_Cholesterol','Total_Cholines','Concentration_of_HDL','Total_Phospholipids_in_Lipoprotein','Total_Triglycerides','Concentration_of_HDL','LDL','IDL',' L_VLDL ','M_VLDL','S_VLDL','VS_VLDL','L_LDL',' M_LDL ',' VL_HDL',' M_HDL','Linoleic_Acid','Omega-3 Fatty_Acids','Omega-6_Fatty_Acids','DHA','Degree of Unsaturation','Apolipoprotein_B','Apolipoprotein_A1','Glucose','Lactate','Pyruvate','Citrate','Phosphatidylcholines','Creatinine','Albumin','Acetate','Acetoacetate','Acetone')

# Mutation of data for Medication
setnames(hesin_diag,"requires.eid",'eid_1')
setnames(covariates, old=colnames(covariates), new = c('Eid_1','Birth_Year','Birth_Month','BMI','Age_at_Obs.','Gender'))
setnames(medication, old=colnames(medication), new = c('x','eid','Patient_1','Patient_2','Patient_3','Patient_4','Patient_5','Patient_6','Patient_7','Patient_8','Patient_9','Patient_10','Patient_11','Patient_12','Patient_13','Patient_14','Patient_15','Patient_16','Patient_17','Patient_18','Patient_19'))
setnames(metabolites, old=colnames(metabolites), new = c('No.','Eid','Total_Cholesterol','Total_Cholesterol-HDL_C','Remnant_Cholesterol','VLDL_Cholesterol','Clinical_LDL_Cholesterol','LDL_Cholesterol','HDL_Cholesterol','Total_Triglycerides','Triglycerides_in_VLDL', 'Triglycerides_in_LDL','Triglycerides_in_HDL','Total_Phospholipids_in_Lipoprotein','Phospholipids_in_VLDL', 'Phospholipids_in_LDL','Phospholipids_in_HDL','Total_Esterified_Cholesterol', 'Cholesteryl_Esters_in_VLDL','Cholesteryl_Esters_in_LDL','Cholesteryl_Esters_in_HDL','Total_Free_Cholesterol','Free_Cholesterol_in_VLDL','Free_Cholesterol_in_LDL','Free Cholesterol in HDL', 'Total Lipids in Lipoprotein ', 'Total Lipids in VLDL','Total Lipids in LDL','Total Lipids in HDL','Total  Lipoprotein ','VLDL','LDL','Concentration_of_HDL','Average Diameter for VLDL ','Average Diameter for LDL ,Average Diameter for HDL_','Phosphoglycerides','Triglycerides_to_Phosphoglycerides ratio','Total_Cholines','Phosphatidylcholines','Sphingomyelins','Apolipoprotein_B','Apolipoprotein_A1','Apolipo protein B-Apolipoprotein A1 ratio',
                                                         'Total_Fatty_Acids','Degree of Unsaturation','Omega-3 Fatty_Acids','Omega-6_Fatty_Acids','Polyunsaturated_Fatty_Acids','Monounsaturated_Fatty_Acids','Saturated_Fatty_Acids','Linoleic_Acid','DHA','Omega-3_Fatty_Acids_to_Total_Fatty_Acids_percentage','Omega-6 Fatty Acids to Total Fatty Acids percentage','Polyunsaturated Fatty Acids to Total Fatty Acids percentage','Monounsaturated Fatty Acids to Total Fatty Acids percentage','Saturated Fatty Acids to Total Fatty Acids percentage','Linoleic Acid to Total Fatty Acids percentage','Docosahexaenoic Acid to Total Fatty Acids percentage','Polyunsaturated Fatty Acids to Monounsaturated Fatty Acids ratio','Omega-6 Fatty Acids to Omega-3 Fatty Acids ratio','Alanine','Glutamine','Glycine','Histidine','Total  Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)','Isoleucine','Leucine','Valine','Phenylalanine','Tyrosine','Glucose','Lactate','Pyruvate','Citrate','3-Hydroxybutyrate','Acetate','Acetoacetate','Acetone','Creatinine','Albumin','Glycoprotein Acetyls','	 Chylomicrons and EL_VLDL ','Total Lipids in Chylomicrons and EL_VLDL',
                                                         'Phospholipids in Chylomicrons and EL_VLDL','Cholesterol in Chylomicrons and EL_VLDL','Cholesteryl Esters in Chylomicrons and EL_VLDL','Free Cholesterol in Chylomicrons and EL_VLDL','Triglycerides in Chylomicrons and EL_VLDL',' VL_VLDL Particle','Total Lipids in VL_VLDL','Phospholipids in VL_VLD','Cholesterol in VL_VLDL','Cholesteryl Esters in VL_VLDL','	Free Cholesterol in VL_VLDL','Triglycerides in VL_VLDL',' L_VLDL ','Total Lipids in L_VLDL','Phospholipids in L_VLDL','Cholesterol in L_VLDL','Cholesteryl Esters in L_VLDL','Free Cholesterol in L_VLDL','Triglycerides in L_VLDL','M_VLDL','Total Lipids in M_VLDL','Phospholipids in M_VLDL','Cholesterol in M_VLDL','Cholesteryl Esters in M_VLDL','Free Cholesterol in M_VLDL','Triglycerides in M_VLDL','S_VLDL','Total Lipids in S_VLDL','Phospholipids in S_VLDL','Cholesterol in S_VLDL','Cholesteryl Esters in S_VLDL','Free Cholesterol in S_VLDL','Triglycerides in S_VLDL',
                                                         'VS_VLDL','Total Lipids in VS_VLDL','Phospholipids in_VS_VLDL','Cholesterol in_VS_VLDL','Cholesteryl Esters in VS_VLDL','Free Cholesterol in VS_VLDL','Triglycerides in VS_VLDL','IDL','Total Lipids in IDL','Phospholipids in IDL','Cholesterol in IDL','	Cholesteryl Esters in IDL','Free_Cholesterol_in_IDLs','Triglycerides in IDL','L_LDL','	Total Lipids in L_LDL','Phospholipids in L_LDL','Cholesterol in L_LDL','Cholesteryl Esters in L_LDL','Free Cholesterol in L_LDL','Triglycerides in L_LDL',' M_LDL ','Total Lipids in M_LDL','Phospholipids in M_LDL','Cholesterol in M_LDL','Cholesteryl Esters in M_LDL','Free Cholesterol in M_LDL','Triglycerides in M_LDL',' S_LDL ','Total Lipids in S_LDL','Phospholipids in S_LDL','Cholesterol in S_LDL','Cholesteryl Esters in S_LDL','Free Cholesterol in S_LDL','Triglycerides in S_LDL',' VL_HDL','Total Lipids in VL_HDL','Phospholipids in VL_HDL','Cholesterol in VL_HDL',
                                                         'Cholesteryl Esters in VL_HDL','Free Cholesterol in VL_HDL','Triglycerides in_VL_HDL',' L_HDL_','Total Lipids in L_HDL','Phospholipids in L_HDL','Cholesterol in L_HDL','Cholesteryl Esters in L_HDL','Free Cholesterol in L_HDL','Triglycerides in L_HDL',' M_HDL','Total Lipids in M_HDL','Phospholipids in M_HDL','Cholesterol in M_HDL','Ethnicity','BMI','Age','Sex'))
setnames(mri,old=colnames(mri), new = c('eid',"Liver_fat"))

#sapply(data2$epistart, typeof), class(data2$epistart)
hesin$epistart <- dmy(hesin$epistart)
hesin$epiend <- dmy(hesin$epiend)
hesin$admidate <- dmy(hesin$admidate)
hesin$disdate <- dmy(hesin$disdate)

class(as.Date(data2$admidate, "%d%m%y"))
class(as.Date(data2$disdate, "%d%m%y"))

# Days difference

hesin$epi_diff = difftime(hesin$epiend,hesin$epistart,units = "days")
hesin$admi_diff = difftime(hesin$disdate,hesin$admidate,units = "days")
metabolite <- na.omit(metabolite)
mri <- na.omit(mri)

# Changing data values

for (i in seq_along(medication)) { medication[[i]][medication[[i]] %in% 1140875408] <- "allopurinol" } %>% { medication[[i]][medication[[i]] %in% 1140916682] <- "evening primrose oil" } %>% { medication[[i]][medication[[i]] %in% 1140884412] <- "sumatriptan" } %>% { medication[[i]][medication[[i]] %in% 1140910814] <- "sodium thyroxine" } %>% { medication[[i]][medication[[i]] %in% 1140863152] <- "diazepam" } %>% { medication[[i]][medication[[i]] %in% 1140851028] <- "chalk" } %>% { medication[[i]][medication[[i]] %in% 1140883504] <- "cetirizine" } %>% { medication[[i]][medication[[i]] %in% 1140865580] <- "asacol-400mg-e/c-tablet" }
for (i in seq_along(medication)) { medication[[i]][medication[[i]] %in% 1140868226] <- "Aspirin" } %>% { medication[[i]][medication[[i]] %in% 1140879616] <- "amitriptyline" } %>% { medication[[i]][medication[[i]] %in% 1140922174] <- "alendronate_sodium"} %>% { medication[[i]][medication[[i]] %in% 1140866738] <- "atenolol" } %>% { medication[[i]][medication[[i]] %in% 1141146234] <- "atorvastatin" } %>% { medication[[i]][medication[[i]] %in% 1141194794] <- "bendroflumethiazide" } %>% { medication[[i]][medication[[i]] %in% 1140870390] <- "ferrous sulphate" } %>% { medication[[i]][medication[[i]] %in% 1141188442] <- "glucosamine product" } %>% { medication[[i]][medication[[i]] %in% 1140871310] <- "ibuprofen" } %>% { medication[[i]][medication[[i]] %in% 1140888366] <- "thiamine_preparation" } %>% { medication[[i]][medication[[i]] %in% 1140860696] <- "lisinopril" } %>% { medication[[i]][medication[[i]] %in% 1140864752] <- "lansoprazole" } %>% { medication[[i]][medication[[i]] %in% 1140865634] <- "omeprazole" } 
for (i in seq_along(medication)) { medication[[i]][medication[[i]] %in% 1140861958] <- "simvastatin" } %>% { medication[[i]][medication[[i]] %in% 1140871024] <- "vitamin_b_compound_tablet" } %>% { medication[[i]][medication[[i]] %in% 1140879760] <- "bisoprolol" } %>% { medication[[i]][medication[[i]] %in% 1140879802] <- "amlodipine" } %>% { medication[[i]][medication[[i]] %in% 1140909674] <- "cod_liver_oil_capsule" } %>% { medication[[i]][medication[[i]] %in% 1140923346] <- "co-codamol" } %>% { medication[[i]][medication[[i]] %in% 1141182628] <- "tiotropium" } %>% { medication[[i]][medication[[i]] %in% 1141176832] <- "seretide_50_evohaler" } %>% { medication[[i]][medication[[i]] %in% 1141145660] <- "valsartan" } %>% { medication[[i]][medication[[i]] %in% 1140863144] <- "zopiclone" } %>% { medication[[i]][medication[[i]] %in% 1140926606] <- "salbutamol_100micrograms_spacehaler" } %>% { medication[[i]][medication[[i]] %in% 1140888510] <- "verapamil" }
for (i in seq_along(medication)) { medication[[i]][medication[[i]] %in% 1140883066] <- "insulin product" } %>% { medication[[i]][medication[[i]] %in% 1140860806] <- "ramipril" } %>% { medication[[i]][medication[[i]] %in% 1140909708] <- "furosemide" }%>% { medication[[i]][medication[[i]] %in% 99999] <- "uncoded" } %>% { medication[[i]][medication[[i]] %in% 1140864992] <- "tramadol" } %>% { medication[[i]][medication[[i]] %in% 1140884600] <- "metformin" } %>% { medication[[i]][medication[[i]] %in% 1140865716] <- "senna" } %>% { medication[[i]][medication[[i]] %in% 2038460150] <- "paracetamol" } %>% { medication[[i]][medication[[i]] %in% 1140874930] <- "prednisolone" } %>% { medication[[i]][medication[[i]] %in% 1140888266] <- "warfarin" } %>% { medication[[i]][medication[[i]] %in% 1140910766] <- "nicorandil" } %>% { medication[[i]][medication[[i]] %in% 1140874420] <- "quinine" }  %>% { medication[[i]][medication[[i]] %in% 1140879406] <- "ranitidine" } %>% { medication[[i]][medication[[i]] %in% 1140881856] <- "salbutamol" } %>% { medication[[i]][medication[[i]] %in% 1141191044] <- "levothyroxine sodium" } 


# 1.1 Find Common Ids in the Data frame:

common <- intersect(hesin$eid, hesin_diag$eid_1)
hesin_common = hesin[common, ]
hesin_diag_common = hesin_diag[common, ]
df1 = merge(hesin_diag,hesin, by.x=c('eid_1','ins_index'), by.y=c('eid','ins_index'))%>% 
  select('eid_1','diag_icd10', 'admi_diff', 'epi_diff')

## 1.2 Metabolites

common2 <- intersect(df1$eid_1, metabolite$Eid)
df1_common = df1[common2, ]
metabolites_common = metabolite[common2, ]
df2 = merge(metabolite, df1, by.x=c('Eid'), by.y=c('eid_1'))

## 1.3 MRI data 

common3 <- intersect(df2$Eid , mri$eid)
df2_common = df2[common3, ]
mri_common = mri[common3,]
df3 = merge(df2, mri, by.x=c('Eid'), by.y=c('eid'))

## 1.4 Medications

common4 <- intersect(df3$Eid, medication$eid)
df4_common = df3[common4, ]
medication_common = medication[common4, ]
df4 = merge(df3, medication, by.x=c('Eid'), by.y=c('eid'))

## 1.5 Healthy Patient list

common5 <- intersect(df6$Eid , No_medication_1$Eid)
df5_common = df6[common5, ]
No_medication_1 = No_medication_1[common5,]
positive_control = merge(No_medication_1, df8, by.x=c('Eid'), by.y=c('Eid'))

# Medications frequent occurrence in patients  

occurance <- data.frame(table(df3$Patient_1))
occurance2 = occurance[occurance$Freq > 200000,]