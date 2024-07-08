# ===============================================================================================================
# Prepare data for PCA and other cluster analysis.
# Version 1
# Created on 01/13/2022 by Rie Sadohara
# ===============================================================================================================

# Here, we will prepare ASA24 totals data for PCA and clustering analyses.
# We will need to calculate average dietary data per person across all days (if desired),
# remove variables that have zero variance, and collapse variables by correlation
# (i.e. remove redundancy of variables that are highly correlated).

# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
source("GitHub_tutorial/DietR/lib/specify_data_dir.R")
source("GitHub_tutorial/DietR/lib/prep_data_for_clustering.R")

# You can come back to the main directory by:
setwd(main_wd)

# ===============================================================================================================
# Import data and prepare them for analyses.
# ===============================================================================================================

# There may be some variables that you would like to omit before performing PCA.
# Define which columns to drop.
drops <- c("FoodAmt", "KCAL", "MOIS") # MOIS is water

# Load the totals data (for each day, not averaged, but with the individuals in the QC-ed mean totals)
# "_m" stands for "metadata added", not "means".
totals_AJ <- read.table("AJdata/MCTs_23887_Tot_m_QC.txt", sep="\t", header=T)
totals_VD <- read.table("VDMTdata/asa24_Tot_m_QC.txt", sep="\t", header=T)

# Take only the columns whose names are NOT in the drops vector.
totals_2_AJ <- totals_AJ[ , !(names(totals_AJ) %in% drops)]
totals_2_VD <- totals_VD[ , !(names(totals_VD) %in% drops)]

# Load the averaged and QC-ed totals that has one data per participant with metadata.
totals_mean_AJ <- read.table("AJdata/MCTs_23887_Tot_mean_m_QC.txt", sep="\t", header=T)
totals_mean_VD <- read.table("VDMTdata/asa24_Tot_mean_m_QC.txt", sep="\t", header=T)

# Take only the columns whose names are NOT in the drops vector.
totals_mean_2_AJ <- totals_mean_AJ[ , !(names(totals_mean_AJ) %in% drops)]
totals_mean_2_VD <- totals_mean_VD[ , !(names(totals_mean_VD) %in% drops)]

# ===============================================================================================================
# add BMI to all the data
# ===============================================================================================================

## I did not grab BMI from the metadata, so I'm going back to grab this variable

 # AJ
ind_metadata_AJ <- read.table('AJdata/UserName_map.txt', sep = "\t", header = T)
ind_metadata_AJ_bmi <- subset(ind_metadata_AJ, select = c(UserName, BMI))

totals_AJ_bmi <- merge(x=totals_AJ, y=ind_metadata_AJ_bmi, by="UserName", all.x=T)
totals_2_AJ_bmi <- merge(x=totals_2_AJ, y=ind_metadata_AJ_bmi, by="UserName", all.x=T)
totals_mean_AJ_bmi <- merge(x=totals_mean_AJ, y=ind_metadata_AJ_bmi, by="UserName", all.x=T)
totals_mean_2_AJ_bmi <- merge(x=totals_mean_2_AJ, y=ind_metadata_AJ_bmi, by="UserName", all.x=T)

 # VD
library(readxl)
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy")
codebook <- read_excel('Codebook-VDMT.xlsx', sheet = 'BODY COMP')
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

codebook_bmi <- subset(codebook, select = c(ID, BMI))
rm(codebook)
# adjust ID to match the asa data (VDMT001 --> VDMT01)
codebook_bmi$ID <- gsub('T0', 'T', codebook_bmi$ID)
colnames(codebook_bmi) <- c('UserName', 'BMI')

totals_VD_bmi <- merge(x=totals_VD, y=codebook_bmi, by="UserName", all.x=T)
totals_2_VD_bmi <- merge(x=totals_2_VD, y=codebook_bmi, by="UserName", all.x=T)
totals_mean_VD_bmi <- merge(x=totals_mean_VD, y=codebook_bmi, by="UserName", all.x=T)
totals_mean_2_VD_bmi <- merge(x=totals_mean_2_VD, y=codebook_bmi, by="UserName", all.x=T)

# ===============================================================================================================
# NUTRIENTS: Use data as is.
# ===============================================================================================================

# Obtain the column numbers for UserName, BMI, start.col="PROT" through end.col="B12_ADD".
userID_col_AJ <- match("UserName", names(totals_2_AJ_bmi))
BMI_col_AJ   <-  match("BMI"     , names(totals_2_AJ_bmi))
start_col_AJ <-  match("PROT"    , names(totals_2_AJ_bmi))
end_col_AJ   <-  match("B12_ADD" , names(totals_2_AJ_bmi))

userID_col_VD <- match("UserName", names(totals_2_VD_bmi))
BMI_col_VD   <-  match("BMI"     , names(totals_2_VD_bmi))
start_col_VD <-  match("PROT"    , names(totals_2_VD_bmi))
end_col_VD   <-  match("B12_ADD" , names(totals_2_VD_bmi))

# Select the BMI, body weight, and the nutrient variables.
user_BMI_nut_AJ <- totals_2_AJ_bmi[ , c(userID_col_AJ, BMI_col_AJ, start_col_AJ:end_col_AJ)]
user_BMI_nut_VD <- totals_2_VD_bmi[ , c(userID_col_VD, BMI_col_VD, start_col_VD:end_col_VD)]

# Ensure user_BMI_nut has only the selected columns (variables).
colnames(user_BMI_nut_AJ)
colnames(user_BMI_nut_VD)

# Process this input, user_BMI_nut, for clustering analysis as follows.
  # 1: Take complete cases in your variables of interest,
  # 2: Save the original totals of the complete cases individuals as a .txt,
  # 3: Keep non-zero columns,
  # 4: Remove the userID,
  # 5: Identify correlated variables and remove them,
  # 6: Save with uncorrelated variables as a .txt,
  # 7: Save correlation matrix as a .txt.

PrepForClustering(input_df = user_BMI_nut_AJ,
                  userID = "UserName",
                  original_totals_df = totals_AJ_bmi,
                  complete_cases_fn =   "AJdata/MCTs_23887_Tot_m_QC_Nut_asis_c.txt",
                  clustering_input_fn = "AJdata/MCTs_23887_Tot_m_QC_Nut_asis_c_rv.txt",
                  corr_matrix_fn =      "AJdata/MCTs_23887_Tot_m_QC_Nut_asis_c_corr_matrix.txt")
# Clustering 64 features...getting means...choosing reps...collapsed from 64 to 28
PrepForClustering(input_df = user_BMI_nut_VD,
                  userID = "UserName",
                  original_totals_df = totals_VD_bmi,
                  complete_cases_fn =   "VDMTdata/asa24_Tot_m_QC_Nut_asis_c.txt",
                  clustering_input_fn = "VDMTdata/asa24_Tot_m_QC_Nut_asis_c_rv.txt",
                  corr_matrix_fn =      "VDMTdata/asa24_Tot_m_QC_Nut_asis_c_corr_matrix.txt")
# Clustering 64 features...getting means...choosing reps...collapsed from 64 to 39

# ===============================================================================================================
# NUTRIENTS: Take average of each user across all days
# ===============================================================================================================

# Obtain the column numbers for UserName, BMI, start.col="PROT" through end.col="B12_ADD"  in totals_mean_2.
UserName_col_AJ <- match("UserName" , names(totals_mean_2_AJ_bmi))
BMI_col_AJ   <-    match("BMI"      , names(totals_mean_2_AJ_bmi))
start_col_AJ <-    match("PROT"     , names(totals_mean_2_AJ_bmi))
end_col_AJ   <-    match("B12_ADD"  , names(totals_mean_2_AJ_bmi))

UserName_col_VD <- match("UserName" , names(totals_mean_2_VD_bmi))
BMI_col_VD   <-    match("BMI"      , names(totals_mean_2_VD_bmi))
start_col_VD <-    match("PROT"     , names(totals_mean_2_VD_bmi))
end_col_VD   <-    match("B12_ADD"  , names(totals_mean_2_VD_bmi))

# Select the BMI, body weight, and the nutrient variables.
m_user_BMI_nut_AJ <- totals_mean_2_AJ_bmi[ , c(UserName_col_AJ, BMI_col_AJ, start_col_AJ:end_col_AJ)]
m_user_BMI_nut_VD <- totals_mean_2_VD_bmi[ , c(UserName_col_VD, BMI_col_VD, start_col_VD:end_col_VD)]

# Process this input for clustering analyses.
PrepForClustering(input_df = m_user_BMI_nut_AJ,
                  userID = "UserName",
                  original_totals_df = totals_mean_AJ_bmi,
                  complete_cases_fn =   "AJdata/MCTs_23887_Tot_mean_m_QC_Nut_ave_c.txt",
                  clustering_input_fn = "AJdata/MCTs_23887_Tot_mean_m_QC_Nut_ave_c_rv.txt",
                  corr_matrix_fn =      "AJdata/MCTs_23887_Tot_mean_m_QC_Nut_ave_c_corr_matrix.txt")
# Clustering 64 features...getting means...choosing reps...collapsed from 64 to 21.
PrepForClustering(input_df = m_user_BMI_nut_VD,
                  userID = "UserName",
                  original_totals_df = totals_mean_VD_bmi,
                  complete_cases_fn =   "VDMTdata/asa24_Tot_mean_m_QC_Nut_ave_c.txt",
                  clustering_input_fn = "VDMTdata/asa24_Tot_mean_m_QC_Nut_ave_c_rv.txt",
                  corr_matrix_fn =      "VDMTdata/asa24_Tot_mean_m_QC_Nut_ave_c_corr_matrix.txt")
# Clustering 64 features...getting means...choosing reps...collapsed from 64 to 36

# ===============================================================================================================
# FOOD CATEGORIES: Use data as is.
# ===============================================================================================================

# Obtain the column numbers for BMI, UserName, start.col="F_TOTAL" through end.col="A_DRINKS".
userID_col_AJ <- match("UserName" , names(totals_2_AJ_bmi))
BMI_col_AJ   <-  match("BMI"      , names(totals_2_AJ_bmi))
start_col_AJ <-  match("F_TOTAL"  , names(totals_2_AJ_bmi))
end_col_AJ   <-  match("A_DRINKS" , names(totals_2_AJ_bmi))

userID_col_VD <- match("UserName" , names(totals_2_VD_bmi))
BMI_col_VD   <-  match("BMI"      , names(totals_2_VD_bmi))
start_col_VD <-  match("F_TOTAL"  , names(totals_2_VD_bmi))
end_col_VD   <-  match("A_DRINKS" , names(totals_2_VD_bmi))

# Select the BMI, body weight, and the nutrient variables.
user_BMI_cat_AJ <- totals_2_AJ_bmi[ , c(userID_col_AJ, BMI_col_AJ, start_col_AJ:end_col_AJ)]
user_BMI_cat_VD <- totals_2_VD_bmi[ , c(userID_col_VD, BMI_col_VD, start_col_VD:end_col_VD)]

# Process this input for clustering analyses.
PrepForClustering(input_df = user_BMI_cat_AJ,
                  userID = "UserName",
                  original_totals_df = totals_AJ_bmi,
                  complete_cases_fn =   "AJdata/MCTs_23887_Tot_m_QC_Cat_asis_c.txt",
                  clustering_input_fn = "AJdata/MCTs_23887_Tot_m_QC_Cat_asis_c_rv.txt",
                  corr_matrix_fn =      "AJdata/MCTs_23887_Tot_m_QC_Cat_asis_c_corr_matrix.txt")
# Clustering 38 features...getting means...choosing reps...collapsed from 38 to 30.
PrepForClustering(input_df = user_BMI_cat_VD,
                  userID = "UserName",
                  original_totals_df = totals_VD_bmi,
                  complete_cases_fn =   "VDMTdata/asa24_Tot_m_QC_Cat_asis_c.txt",
                  clustering_input_fn = "VDMTdata/asa24_Tot_m_QC_Cat_asis_c_rv.txt",
                  corr_matrix_fn =      "VDMTdata/asa24_Tot_m_QC_Cat_asis_c_corr_matrix.txt")
# Clustering 38 features...getting means...choosing reps...collapsed from 38 to 32.

# ===============================================================================================================
# FOOD CATEGORIES: Take average of each user across all days
# ===============================================================================================================

# Obtain the column numbers for UserName, BMI, start.col="F_TOTAL" through end.col="A_DRINKS" in totals_mean_2.
UserName_col_AJ <- match("UserName", names(totals_mean_2_AJ_bmi))
BMI_col_AJ      <- match("BMI", names(totals_mean_2_AJ_bmi))
start_col_AJ    <- match("F_TOTAL", names(totals_mean_2_AJ_bmi))
end_col_AJ      <- match("A_DRINKS", names(totals_mean_2_AJ_bmi))

UserName_col_VD <- match("UserName", names(totals_mean_2_VD_bmi))
BMI_col_VD      <- match("BMI", names(totals_mean_2_VD_bmi))
start_col_VD    <- match("F_TOTAL", names(totals_mean_2_VD_bmi))
end_col_VD      <- match("A_DRINKS", names(totals_mean_2_VD_bmi))

# Pick up the BMI, body weight, and the nutrient variables.
m_user_BMI_cat_AJ <- totals_mean_2_AJ_bmi[ , c(UserName_col_AJ, BMI_col_AJ, start_col_AJ:end_col_AJ)]
m_user_BMI_cat_VD <- totals_mean_2_VD_bmi[ , c(UserName_col_VD, BMI_col_VD, start_col_VD:end_col_VD)]

# Process this input for clustering analyses.
PrepForClustering(input_df = m_user_BMI_cat_AJ,
                  userID = "UserName",
                  original_totals_df = totals_mean_AJ,
                  complete_cases_fn =   "AJdata/MCTs_23887_Tot_mean_m_QC_Cat_ave_c.txt",
                  clustering_input_fn = "AJdata/MCTs_23887_Tot_mean_m_QC_Cat_ave_c_rv.txt",
                  corr_matrix_fn =      "AJdata/MCTs_23887_Tot_mean_m_QC_Cat_ave_c_corr_matrix.txt")
# Clustering 38 features...getting means...choosing reps...collapsed from 38 to 26.
PrepForClustering(input_df = m_user_BMI_cat_VD,
                  userID = "UserName",
                  original_totals_df = totals_mean_VD,
                  complete_cases_fn =   "VDMTdata/asa24_Tot_mean_m_QC_Cat_ave_c.txt",
                  clustering_input_fn = "VDMTdata/asa24_Tot_mean_m_QC_Cat_ave_c_rv.txt",
                  corr_matrix_fn =      "VDMTdata/asa24_Tot_mean_m_QC_Cat_ave_c_corr_matrix.txt")
# Clustering 38 features...getting means...choosing reps...collapsed from 38 to 31.

# ===============================================================================================================
# Come back to the main directory
setwd(main_wd)
