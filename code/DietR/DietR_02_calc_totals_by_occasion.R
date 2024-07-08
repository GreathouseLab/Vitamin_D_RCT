# ========================================================================================
# Calculate totals by occasion, user, and day.
# Useful for analyzing dietary intake per occasion (breakfast, lunch, etc..).
# Version 1
# Created on 03/16/2022 by Rie Sadohara
# ========================================================================================

# Here we will calculate totals by occasion, user, and day.
# This will be useful if a researcher wants to look at the sum of each eating occasion
# per participant. In contrast, the totals file we generated summed all the occasions of
# a certain day into one datapoint.

# ========================================================================================
# Load functions and packages
# ========================================================================================
# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package/GitHub_tutorial/DietR")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
source("lib/specify_data_dir.R")
source("lib/calc_ASA24totals_by_occasion.R")

setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# ========================================================================================
# Generate sum of foods consumed by occasion
# ========================================================================================

# Load your items data.
items_AJ_QCed <- read.delim("AJdata/MCTs_23887_Items_QC.txt", sep="\t", header=T)
items_VD_QCed <- read.delim("VDMTdata/asa24_Items_QC.txt", sep="\t", header=T)


# Generate sum of foods consumed by each user, by day, and by occasion.
# Output will be saved as: Items_by_User_Occ.
SumByOccasion(items.data=items_AJ_QCed, User.Name='UserName',
                Recall.No='ReportingDate', Occ.No='Occ_No')
Items_by_User_Occ_AJ <- Items_by_User_Occ

# Take a look at the first 10 rows of the first 6 columns.
# Items are summed by occasion number for each participant's RecallNo.
Items_by_User_Occ_AJ[1:10, 1:6]

# The Items_by_User_Occ function adds occasion names (breakfast, snacks etc.) to items.data.
# Output will be saved as: Sum_by_User_Day_Occ.

AddOccNames(items.data=items_AJ_QCed, User.Name='UserName',
            Recall.No='ReportingDate', Occ.No='Occ_No', Occ.Name='Occ_Name')
Sum_by_User_Day_Occ_AJ <- Sum_by_User_Day_Occ
rm(Sum_by_User_Day_Occ) # this variable will be created again with the VD data

# The output Sum_by_User_Day_Occ has the sum of foods consumed by each user, day,
# and occasion, with occasion names in words.

## The same with VD data

SumByOccasion(items.data=items_VD_QCed, User.Name='UserName',
              Recall.No='StudyDay', Occ.No='Occ_No')
Items_by_User_Occ_VD <- Items_by_User_Occ

Items_by_User_Occ_VD[1:10, 1:6]

AddOccNames(items.data=items_VD_QCed, User.Name='UserName',
            Recall.No='StudyDay', Occ.No='Occ_No', Occ.Name='Occ_Name')
Sum_by_User_Day_Occ_VD <- Sum_by_User_Day_Occ

# The output Sum_by_User_Day_Occ has the sum of foods consumed by each user, day,
# and occasion, with occasion names in words.

# Take a look at the first 6 rows. Spelled-out occasion names corresponding to occasion name
# have been added at the end.
head(Sum_by_User_Day_Occ_AJ)
head(Sum_by_User_Day_Occ_VD)

# ---------------------------------------------------------------------------------------------------------------
# Save Sum_by_User_Day_Occ as a txt file.
write.table(Sum_by_User_Day_Occ_AJ, 'AJdata/MCTs_23887_Sum_by_User_Day_Occ.txt', sep="\t", row.names=F, quote=F)
write.table(Sum_by_User_Day_Occ_VD, 'VDMTdata/asa24_Sum_by_User_Day_Occ.txt', sep="\t", row.names=F, quote=F)

# This will be useful if a researcher wants to look at the sum of each eating occasion
# per participant. (Because Totals file sums all the occasions as one day)
