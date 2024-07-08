# Use of DietR Package to compare our vitamin D data to Abby Johnson's
# in her study that we used as our model

# Sept 2023

# This follows ASA24_01_load_and_clean.R

# https://computational-nutrition-lab.github.io/DietR/index.html

# I downloaded the DietR repo from Github
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package/GitHub_tutorial/DietR")

# Name your main directory where input files are pulled.
main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
source("lib/specify_data_dir.R")
source("lib/load_clean_ASA24.R")
source("lib/average.by.R")
source("lib/QCOutliers.R")
source("lib/Food_tree_scripts/format.foods_2.r")

# to get out of the subsection folders:
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# ==============================================================================
# Load ASA24 data
# ==============================================================================

# Load your unprocessed (raw) food items-level data
items_raw_AJ <- read.csv("AJdata/MCTs_23887_Items.csv", sep = ",", header=T)
items_raw_VD <- read.csv("VDMTdata/ASA24items_cleaned_copy.csv", sep = ",", header=T)

# Rename Food_Description as Main.food.description
names(items_raw_AJ)[names(items_raw_AJ) == "Food_Description"] <- "Main.food.description"
names(items_raw_VD)[names(items_raw_VD) == "Food_Description"] <- "Main.food.description"

# Double Check for Main.food.description
names(items_raw_AJ)[names(items_raw_AJ) == "Main.food.description"]
names(items_raw_VD)[names(items_raw_VD) == "Main.food.description"]

# Save the items file as a .txt file
write.table(items_raw_AJ, "AJdata/MCTs_23887_Items.txt", sep="\t", row.names=F)
write.table(items_raw_VD, "VDMTdata/asa24_Items.txt", sep="\t", row.names=F)

# Replace special characters with underscores --> save as formatted
FormatFoods(input_fn =  "AJdata/MCTs_23887_Items.txt",
            output_fn = "AJdata/MCTs_23887_Items_f.txt")
FormatFoods(input_fn =  "VDMTdata/asa24_Items.txt",
            output_fn = "VDMTdata/asa24_Items_f.txt")

# Load the Items_f.txt file, ignoring quotation marks, and loading all the columns
# as characters so that FoodID will keep the trailing ".0"
items_f_AJ <- read.delim("AJdata/MCTs_23887_Items_f.txt", quote="", colClasses="character")
items_f_VD <- read.delim("VDMTdata/asa24_Items_f.txt", quote="", colClasses="character")

# All special characters in the items data should have been replaced with an underscore
# in the Main.foood.description column, the 3rd from the last column of the items_f.

# To confirm: --> yes
head(items_f_AJ)
head(items_f_VD)

# Add a human-readable sample identifier (SampleID) with a desired prefix,
# saving it as a txt file.
# SampleIDs are IDs unique to each combination of users and day
# and represent days of dietary intake in this dataset.
AddSampleIDtoItems(input.fn = "AJdata/MCTs_23887_Items_f.txt",
                   user.name = "UserName",
                   recall.no = "ReportingDate", # this only needs to be unique per ASA entry
                   prefix="AJ",
                   out.fn="AJdata/MCTs_23887_Items_f_id.txt")
# NOTE: RecallNo has been removed from AJ's data- maybe bc it was an older ASA edition?

AddSampleIDtoItems(input.fn = "VDMTdata/asa24_Items_f.txt",
                   user.name = "UserName",
                   recall.no = "StudyDay", # i am using this column that Ella created
                   prefix="VD",
                   out.fn="VDMTdata/asa24_Items_f_id.txt")

# Load the formatted Items file with SampleID added
items_AJ_f_id <- read.delim("AJdata/MCTs_23887_Items_f_id.txt", quote="", colClasses="character")
items_VD_f_id <- read.delim("VDMTdata/asa24_Items_f_id.txt", quote="", colClasses="character")

# A combination of the specified prefix and sequential number should be added
# in the SampleID column, the first column of the items_f_id dataframe
head(items_AJ_f_id)
head(items_VD_f_id)

# Ensure your items file has the expected dimensions --> yes
# Note that items_f_id should have 3 more columns than items_raw because
# FoodID, Old.Main.food.description, and SampleID have been added
dim(items_AJ_f_id) # 10.5k items
dim(items_VD_f_id) # compared to 8.5k items
dim(items_raw_AJ)
dim(items_raw_VD)

# ==============================================================================
# making my own set of metadata so that i can split between men and women for QC [VDMT]
# (bmi is taken from the codebook, and added in a later script to use)
# ==============================================================================

library(readxl)

setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy")
codebook <- read_excel('Codebook-VDMT.xlsx', sheet = 'DEMOGRAPH')
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

head(codebook)
colnames(codebook)

codebook_sex <- subset(codebook, select = c(ID, SEX, Intervention))
rm(codebook)

# adjust ID to match the asa data (VDMT001 --> VDMT01)
unique(items_VD_f_id$UserName)
unique(codebook_sex$ID)
codebook_sex$ID <- gsub('T0', 'T', codebook_sex$ID)

head(codebook_sex) # change was successful

colnames(codebook_sex) <- c('UserName', 'SEX', 'Intervention')

# ===============================================================================================================
#  Merge individuals' metadata to items.
# ===============================================================================================================

## VD

# add metadata to ITEMS
items_VD_f_id <- read.delim("VDMTdata/asa24_Items_f_id.txt", quote="", colClasses="character")
items_VD_f_id_meta <- merge(x=items_VD_f_id, y=codebook_sex, by="UserName", all.x=T)

write.table(items_VD_f_id_meta, "VDMTdata/asa24_Items_f_id_meta.txt", sep="\t", row.names=F, quote=F)

## AJ

# I found this file on github: https://github.com/knights-lab/dietstudy_analyses/blob/master/data/maps/UserName_map.txt
ind_metadata_AJ <- read.table('AJdata/UserName_map.txt', sep = "\t", header = T)

# check that usernames are matched --> they are the same format, but there are 37 in metadata, and 51 in og data
unique(ind_metadata_AJ$UserName)
unique(items_AJ_f_id$UserName)

colnames(ind_metadata_AJ)

# specify important vars
ind_metadata_AJ_s <- subset(ind_metadata_AJ, select = c(UserName, Gender, Supplement,
                                                        Study.Status, oilGrams.assigned))

# Add metadata to ITEMS
items_AJ_f_id_meta <- merge(x=items_AJ_f_id, y=ind_metadata_AJ_s, by="UserName", all.x=T)

write.table(items_AJ_f_id_meta, "AJdata/MCTs_23887_Items_f_id_meta.txt", sep="\t", row.names=F, quote=F)

# ==============================================================================
# Generate new totals file from the items file. (this is to make one line per ASA entry)
# ==============================================================================

# understanding RecallNo, and how many Reporting dates there were
unique(items_AJ_f_id$ReportingDate) # 21 days were recorded (but according to the paper, should be 17)
unique(items_VD_f_id$StudyDay) # 15
unique(items_AJ_f_id$UserName) # 51 participants (she says 34)
unique(items_VD_f_id$UserName) # 43 participants

# AJ data *should* have 578 user/day arrangements
 #51 * 21 # these numbers are according to the data
34 * 17 # these are according to the paper- I'll use this
 # this decrease in day is understandable, maybe some ppl filled it out a few days later
 # apparently 37 began the the study, but 3 dropped out

# VD data *should* have 645 user/day arrangements
43 * 15
# however, these calculations are the perfect world scenario, where each person
# filled out each day, which isn't the case

GenerateTotals(inputfn = "AJdata/MCTs_23887_Items_f_id.txt",
               User.Name = "UserName",
               Recall.No = "ReportingDate",
               outfn = "AJdata/MCTs_23887_Tot.txt")

GenerateTotals(inputfn = "VDMTdata/asa24_Items_f_id.txt",
               User.Name = "UserName",
               Recall.No = "StudyDay",
               outfn = "VDMTdata/asa24_Tot.txt")

new_totals_AJ <- read.table("AJdata/MCTs_23887_Tot.txt", header=T, sep="\t")
new_totals_VD <- read.table("VDMTdata/asa24_Tot.txt", header=T, sep="\t")
nrow(new_totals_AJ) # 366
nrow(new_totals_VD) # 576

# *** this is one huge difference in our data:
# it seems that our VD data has much better responses rates
# however, according to the paper, she uses 34 ppl and 17 days, but her data
# has 51 ppl and 21 days

# ===============================================================================================================
# Add the participants' metadata to the totals
# ===============================================================================================================

# Add this metadata of each participant to totals.
# 'NA' will be inserted to UserNames which are not in ind_metadata.
new_totals_AJ_m <- merge(x=new_totals_AJ, y=ind_metadata_AJ_s, by="UserName", all.x=T)
new_totals_VD_m <- merge(x=new_totals_VD, y=codebook_sex, by="UserName", all.x=T)

# Check that the items data and metadata are merged.
head(new_totals_AJ_m)
head(new_totals_VD_m)

# Save the merged dataframe as a .txt file.
write.table(new_totals_AJ_m, "AJdata/MCTs_23887_Tot_m.txt", sep="\t", row.names=F, quote=F)
write.table(new_totals_VD_m, "VDMTdata/asa24_Tot_m.txt", sep="\t", row.names=F, quote=F)

# ==============================================================================
# Calculate the mean of totals/participant (this is to make one row per person)
# ==============================================================================

# Calculate the mean of the totals data across all the days for each participant
AverageBy(data= new_totals_AJ,
          by= "UserName",
          start.col= "FoodAmt",
          end.col= "A_DRINKS",
          outfn="AJdata/MCTs_23887_Tot_mean.txt")
AverageBy(data= new_totals_VD,
          by= "UserName",
          start.col= "FoodAmt",
          end.col= "A_DRINKS",
          outfn="VDMTdata/asa24_Tot_mean.txt")

new_totals_mean_AJ <- read.table("AJdata/MCTs_23887_Tot_mean.txt", header=T, sep="\t")
new_totals_mean_VD <- read.table("VDMTdata/asa24_Tot_mean.txt", header=T, sep="\t")

nrow(new_totals_mean_AJ) # 51 - seems to show that her data has 51 ppl
head(new_totals_mean_AJ)

nrow(new_totals_mean_VD) # 43 people
head(new_totals_mean_VD)

# ===============================================================================================================
# Add the participants' metadata to the mean totals [VDMT]
# ===============================================================================================================

# Add this metadata of each participant in the mean totals.
# 'NA' will be inserted to UserNames which are not in codebook_sex
new_totals_mean_AJ_m <- merge(x=new_totals_mean_AJ, y=ind_metadata_AJ_s, by="UserName", all.x=T)
new_totals_mean_VD_m <- merge(x=new_totals_mean_VD, y=codebook_sex, by="UserName", all.x=T)

# Check that the mean totals and the users' metadata are merged --> yes
head(new_totals_mean_AJ_m, 2)
head(new_totals_mean_VD_m, 2)

# Save the merged dataframe as a .txt file.
write.table(new_totals_mean_AJ_m, "AJdata/MCTs_23887_Tot_mean_m.txt", sep="\t", row.names=F, quote=F)
write.table(new_totals_mean_VD_m, "VDMTdata/asa24_Tot_mean_m.txt", sep="\t", row.names=F, quote=F)

# ===============================================================================================================
# Quality Control (QC) for the mean totals data
# ===============================================================================================================

# Load your totals to be used as input for QC
# use new_totals_mean_AJ_m & new_totals_mean_VD_m

# Split your dataset to males and females because different thresholds apply for males and females.
new_totals_mean_AJ_m_M <- subset(new_totals_mean_AJ_m, Gender=='M')
new_totals_mean_AJ_m_F <- subset(new_totals_mean_AJ_m, Gender=='F')
new_totals_mean_VD_m_M <- subset(new_totals_mean_VD_m, SEX==1) # male is 1
new_totals_mean_VD_m_F <- subset(new_totals_mean_VD_m, SEX==2) # female is 2

# ---------------------------------------------------------------------------------------------------------------
# QC for males [VDMT]
# Define your males totals dataset to be used as input.
QCtotals <- new_totals_mean_VD_m_M

# Flag if KCAL is <650 or >5700 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 650, max = 5700)
# none

# Flag if PROT is <25 or >240 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 25, max = 240)
# none

# Flag if TFAT is <25 or >230 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 25, max = 230)
# none

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 400)
# none

# Name the males totals after QC.
QCed_M <- QCtotals

# ---------------------------------------------------------------------------------------------------------------
# QC for females [VDMT]
# Define your females totals dataset to be used as input.
QCtotals <- new_totals_mean_VD_m_F

# Flag if KCAL is <600 or >4400 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 600, max = 4400)
# none

# Flag if PROT is <10 or >180 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 10, max = 180)
# none

# Flag if TFAT is <15 or >185 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 15, max = 185)
# none

# Flag if VC (Vitamin C) is <5 or >350 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 350)
# none

# Name the females totals after QC.
QCed_F <- QCtotals

# ---------------------------------------------------------------------------------------------------------------
# Combine the rows of M and F.
QCtotals_MF_VD <- rbind(QCed_M, QCed_F)

# soooo, no participants needed to be cut from VDMT based off of these measures
# Save as a .txt file.
write.table(new_totals_mean_VD_m, "VDMTdata/asa24_Tot_mean_m_QCed.txt", sep="\t", quote=F, row.names=F)

# ---------------------------------------------------------------------------------------------------------------
# QC for males [AJ]
# Define your males totals dataset to be used as input.
QCtotals <- new_totals_mean_AJ_m_M

# Flag if KCAL is <650 or >5700 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 650, max = 5700)
# 4 found!
KCAL_outliers <- subset(QCtotals, KCAL < 650 | KCAL > 5700)
# Sort the rows by KCAL and show only the specified variables.
KCAL_outliers[order(KCAL_outliers$KCAL, decreasing = T),
              c('UserName', 'KCAL', 'FoodAmt', 'PROT', 'TFAT', 'CARB')]
# participants 26, 6, 8, and 25

# Flag if PROT is <25 or >240 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 25, max = 240)
# 5 found!
PROT_outliers <- subset(QCtotals, PROT < 25 | PROT > 240)
PROT_outliers[order(PROT_outliers$PROT, decreasing = T),
              c('UserName', 'KCAL', 'FoodAmt', 'PROT', 'TFAT', 'CARB')]
# participants 26, 6, 3, 25, 8

# Flag if TFAT is <25 or >230 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 25, max = 230)
# 6 found
TFAT_outliers <- subset(QCtotals, TFAT < 25 | TFAT > 230)
TFAT_outliers[order(PROT_outliers$TFAT, decreasing = T),
              c('UserName', 'KCAL', 'FoodAmt', 'PROT', 'TFAT', 'CARB')]
# only 5 are printing... participants 8, 25, 6, 3, 26

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 400)
# 2 found
VC_outliers <- subset(QCtotals, VC < 5 | VC > 400)
VC_outliers[order(PROT_outliers$VC, decreasing = T),
              c('UserName', 'KCAL', 'FoodAmt', 'PROT', 'TFAT', 'CARB', 'VC')]
# participants 11 and 8

# 3's are barely over
# 6's are a bit over
# most of 8's are just a little over, or a bit over
# 11's VC is a bit over
# 25's are barely over
# 26's are pretty considerably over .... let's investigate

# Name the males totals after QC.
QCed_M <- QCtotals

# ---------------------------------------------------------------------------------------------------------------
# QC for females [AJ]
# Define your females totals dataset to be used as input.
QCtotals <- new_totals_mean_m_AJ_F

# Flag if KCAL is <600 or >4400 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 600, max = 4400)
# 1
KCAL_outliers <- subset(QCtotals, KCAL < 600 | KCAL > 4400)
KCAL_outliers$UserName # participant 4

# Flag if PROT is <10 or >180 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 10, max = 180)
# 1
PROT_outliers <- subset(QCtotals, PROT < 10 | PROT > 180)
PROT_outliers$UserName # participant 4

# Flag if TFAT is <15 or >185 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 15, max = 185)
# 1
TFAT_outliers <- subset(QCtotals, TFAT < 15 | TFAT > 185)
TFAT_outliers$UserName # participant 4

# Flag if VC (Vitamin C) is <5 or >350 --> ask remove or not --> if yes, remove those rows
QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 350)
# 1 outlier
VC_outliers <- subset(QCtotals, VC < 5 | VC > 350)
VC_outliers$UserName # participant 4
VC_outliers[order(PROT_outliers$VC, decreasing = T),
            c('UserName', 'KCAL', 'FoodAmt', 'PROT', 'TFAT', 'CARB')]

# participant 4 was an outlier for all categories... by a humungous amount
# but participant 4 is found in her paper

# Name the females totals after QC.
QCed_F <- QCtotals

# I am hesitant to get rid of any people as a whole, esp bc AJ didn't

# ---------------------------------------------------------------------------------------------------------------
# Combine the rows of M and F.
QCtotals_MF_AJ <- rbind(QCed_M, QCed_F)

setwd(main_wd)

# 9/15: go ahead and get rid of ppl if they're not in her paper;
# let's not worry about the big difference in ASA entry counts tho
# so bc I'm going off of participants from the paper, I won't worry about the DietR QC

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# creating final files

# these are already loaded
new_totals_AJ_m <- read.table("AJdata/MCTs_23887_Tot_m.txt", header=T, sep="\t")
new_totals_VD_m <- read.table("VDMTdata/asa24_Tot_m.txt", header=T, sep="\t")

# keep 34 participants, per article (11 & 12 were the meal-replacement ones)
to_keep <- c('MCTs01', 'MCTs03', 'MCTs04', 'MCTs05', 'MCTs06', 'MCTs07', 'MCTs08',
             'MCTs09', 'MCTs10', 'MCTs11', 'MCTs12', 'MCTs13', 'MCTs14', 'MCTs15',
             'MCTs16', 'MCTs18', 'MCTs19', 'MCTs20', 'MCTs21', 'MCTs22', 'MCTs23',
             'MCTs24', 'MCTs25', 'MCTs26', 'MCTs27', 'MCTs28', 'MCTs29', 'MCTs31',
             'MCTs32', 'MCTs33', 'MCTs34', 'MCTs35', 'MCTs36', 'MCTs37')

new_totals_AJ_m_QC <- new_totals_AJ_m[new_totals_AJ_m$UserName %in% to_keep, ]
write.table(new_totals_AJ_m_QC, "AJdata/MCTs_23887_Tot_m_QC.txt", sep="\t", row.names=F, quote=F)
write.table(new_totals_VD_m, "VDMTdata/asa24_Tot_m_QC.txt", sep="\t", row.names=F, quote=F) # identical to asa24_Tot_m.txt

# QC the mean totals # new_totals_mean_AJ_m & new_totals_mean_VD_m
new_totals_mean_AJ_m_QC <- new_totals_mean_AJ_m[new_totals_mean_AJ_m$UserName %in% to_keep, ]
write.table(new_totals_mean_AJ_m_QC, "AJdata/MCTs_23887_Tot_mean_m_QC.txt", sep="\t", row.names=F, quote=F)
write.table(new_totals_mean_VD_m, "VDMTdata/asa24_Tot_mean_m_QC.txt", sep="\t", row.names=F, quote=F) # identical to asa24_Tot_m.txt


# QC the ITEMS dataframes # use items_VD_f_id_meta & items_AJ_f_id_meta
items_AJ_f_id_meta_QC <- items_AJ_f_id_meta[items_AJ_f_id_meta$UserName %in% to_keep, ]

write.table(items_AJ_f_id_meta_QC, "AJdata/MCTs_23887_Items_QC.txt", sep="\t", row.names=F, quote=F)
write.table(items_VD_f_id_meta, "VDMTdata/asa24_Items_QC.txt", sep="\t", row.names=F, quote=F) # identical to asa24_Items_f_id_meta.txt
