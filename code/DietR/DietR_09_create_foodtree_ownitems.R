# ===============================================================================================================
# Generate a foodtree from ASA24 data without using FilterDBByDiet, a clean version.
# Version 2
# Created on 02/16/2023 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC".
# ===============================================================================================================

# This brief script is to serve as an example of generating foodtrees and IFC tables with your own dataset.
# IFC tables contain participants and food items, showing the consumption amount of each food item by each
# participant. IFC tables also contain the taxonomy (food group information) for each food items and will be
# used in ordination (grouping) analyses.

# This script demonstrates how to:
# 1. Build a foodtree with food items reported by VVKAJ study participants.
# 2. Generate IFC tables from the taxonomy information of reported food items.

# As explained in the previous script, the functions in Food_tree_scripts folder expects that the input files are
# tab-delimited txt file with no special characters that impede correct loading such as:
#   "
#   '
#   #
#   &

# The use of the FormatFoods function in 02_load_clean_ASA24.R script has already dealt with special characters in
# the VVKAJ items data, but it is helpful to know the assumptions which the functions you are going to use were built on.

# Before proceeding, create a new folder called "Foodtree" in the "VVKAJ" folder, in which you will save the output.

# ---------------------------------------------------------------------------------------------------------------

# Set your working directory as the main directory (dietary_patterns)
# Session --> Set working directory --> Choose directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Load the packages/scripts necessary for tree building.
if (!require("reshape2", quietly = TRUE))install.packages("reshape2")
# Load the data.tree package necessary for newick.tree.r, and if it is not installed, install it.
if (!require("data.tree", quietly = TRUE))install.packages("data.tree")

# Load source scripts
source("GitHub_tutorial/DietR/lib/specify_data_dir.R")
source("GitHub_tutorial/DietR/lib/Food_tree_scripts/newick.tree.r")
source("GitHub_tutorial/DietR/lib/Food_tree_scripts/make.food.tree.r") # This needs 'newick.tree.r' already loaded.
source("GitHub_tutorial/DietR/lib/Food_tree_scripts/make.food.ifc.r")
source("GitHub_tutorial/DietR/lib/Food_tree_scripts/make.fiber.ifc.r")
source("GitHub_tutorial/DietR/lib/Food_tree_scripts/make.dhydrt.ifc.r")

# ===============================================================================================================
# Generate a foodtree from food items reported in your study.
# ===============================================================================================================

# Create foodtree with the reduced dataset (only reported foods) classified at
# a desired level of classification.

## NOTE: this uses 4 of 5 levels, but this can be changed with num_levels

MakeFoodTree(nodes_fn = "GitHub_tutorial/DietR/eg_data/Food_tree_eg/NodeLabelsMCT.txt",
             food_database_fn = "AJdata/MCTs_23887_Items_QC.txt",
             addl_foods_fn = NULL,
             num_levels = 4,
             output_tree_fn =     "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.tree.nwk",
             output_taxonomy_fn = "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.tax.txt")

MakeFoodTree(nodes_fn = "GitHub_tutorial/DietR/eg_data/Food_tree_eg/NodeLabelsMCT.txt",
             food_database_fn = "VDMTdata/asa24_Items_QC.txt",
             addl_foods_fn = NULL,
             num_levels = 4,
             output_tree_fn = "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.tree.nwk",
             output_taxonomy_fn = "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.tax.txt")

  # nodes_fn:           the food level (node) information for each food item.
  # food_database_fn:   whole ASA24 database to use.
  # addl_foods_fn:      additional foods that are not in ASA24 database but you would like to add; soylent_codes
  #                     could be added in this case. If none, enter "NULL" instead.
  # num_levels:         number of food levels (1 - 5) to save.
  # output_tree_fn:     output tree file name. Should end with ".nwk"
  # output_taxonomy_fn: output taxonomy file (to be used later) name.

# ===============================================================================================================
# Generate standard, grams of fiber, and dehydrated grams per kcal IFC tables to be used later.
# ===============================================================================================================
# Make the standard ifc table with data in gram weights of food.
# For the food_records_fn argument, you need to supply the items data which contains 'FoodAmt' column.

# It is OK to see see a warning message:
# In write.table(dhydrt.ifc, output_fn, sep = "\t", quote = F, append = TRUE) :
#   appending column names to file
MakeFoodIfc(food_records_fn =  "AJdata/MCTs_23887_Items_QC.txt",
            food_record_id =   "SampleID",
            food_taxonomy_fn = "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.tax.txt",
            output_fn =        "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.food.ifc.txt")

# food_records_fn:  Same as food_records_fn in MakeFoodTree.
# food_record_id:   Your SampleID (User x Day)
# food_taxonomy_fn: Taxonomy file produced by MakeFoodTree.
# output_fn:        Name output ifc file.

######### real MakeFoodIfc function: found from: DietR/lib/Food_tree_scripts/make.food.ifc.R
food_records_fn =  "VDMTdata/asa24_Items_QC.txt"
food_record_id =   "SampleID"
food_taxonomy_fn = "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.tax.txt"
output_fn =        "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.food.ifc.txt"

diet <- read.table(food_records_fn,
                   header = TRUE, sep="\t",
                   colClasses="character",
                   quote="", strip.white=T)
diet$FoodAmt <- as.numeric(diet$FoodAmt)
cdiet <- aggregate(diet$FoodAmt, by=list(diet[, food_record_id], diet$FoodID), FUN=sum)
colnames(cdiet) <- c(food_record_id, "FoodID", "total.grams")
cdiet.w <- reshape(cdiet, timevar = "FoodID", idvar = food_record_id, direction = "wide")
cdiet.w[is.na(cdiet.w)] <- 0
rownames(cdiet.w) <- cdiet.w[, 1] # make record_ids the rownames
cdiet.w <- cdiet.w[, -1]
colnames(cdiet.w) <- gsub("total.grams.", "", colnames(cdiet.w)) #rename column names to FoodIDs only
t.cdiet.w <- t(cdiet.w)

# Ella had to change some things about food.taxonomy to get rid of some duplicate
# Main.food.description- bc some had two different food IDs bc of diff ModCodes
library(dplyr)
food.taxonomy <- read.table(food_taxonomy_fn, sep="\t", colClasses="character", quote="", header=T, row=1)
food.taxonomy_u <- food.taxonomy |> distinct(food.taxonomy$Main.food.description)
food.ifc <- merge(t.cdiet.w, food.taxonomy_u, by=0)
colnames(food.ifc)[578] <- 'Main.food.description' # Ella added this
rownames(food.ifc) <- food.ifc[, 'Main.food.description']
remove.col.ix <- which(colnames(food.ifc) %in% c("Main.food.description", "Row.names")) # Column numbers of the specified columns.
food.ifc <- food.ifc[, -remove.col.ix]
cat("#FOODID\t", file = output_fn)
write.table(food.ifc, output_fn, sep="\t", quote=F, append=TRUE)

#####################################

# Make an ifc table with data in grams of fiber per food
MakeFiberIfc(food_records_fn=  "AJdata/MCTs_23887_Items_QC.txt",
             food_record_id=   "SampleID",
             food_taxonomy_fn= "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.tax.txt",
             output_fn=        "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.fiber.ifc.txt")

################################## full function for MakeFiberIfc for VDMT data
food_records_fn=  "VDMTdata/asa24_Items_QC.txt"
food_record_id=   "SampleID"
food_taxonomy_fn= "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.tax.txt"
output_fn=        "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.fiber.ifc.txt"

diet <- read.table(food_records_fn, header = TRUE, sep="\t", colClasses="character", quote="", strip.white=T)
diet$FIBE <- as.numeric(diet$FIBE)
cdiet <- aggregate(diet$FIBE, by=list(diet[,food_record_id], diet$FoodID), FUN=sum)
colnames(cdiet) <- c(food_record_id, "FoodID", "fiber.grams")
cdiet.w <- reshape(cdiet, timevar = "FoodID", idvar = food_record_id, direction = "wide")
cdiet.w[is.na(cdiet.w)] <- 0
rownames(cdiet.w) <- cdiet.w[,1] # make record_ids the rownames
cdiet.w <- cdiet.w[,-1]
colnames(cdiet.w) <- gsub("fiber.grams.", "", colnames(cdiet.w)) # rename column names to FoodIDs only
t.cdiet.w <- t(cdiet.w)

food.taxonomy <- read.table(food_taxonomy_fn, sep="\t", colClasses="character", quote="", header=T, row=1)
food.taxonomy_u <- food.taxonomy |> distinct(food.taxonomy$Main.food.description)

fiber.ifc <- merge(t.cdiet.w, food.taxonomy_u, by=0)
colnames(fiber.ifc)[578] <- 'Main.food.description'
rownames(fiber.ifc) <- fiber.ifc[,"Main.food.description"]
remove.col.ix <- which(colnames(fiber.ifc) %in% c("Main.food.description", "Row.names"))
fiber.ifc <- fiber.ifc[,-remove.col.ix]
cat("#FOODID\t", file=output_fn)
write.table(fiber.ifc, output_fn, sep = "\t", quote = F, append=TRUE)

##################################

# Make an ifc table as dehydrated grams per kcal.
MakeDhydrtIfc(food_records_fn=  "AJdata/MCTs_23887_Items_QC.txt",
              food_record_id =  "SampleID",
              food_taxonomy_fn= "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.tax.txt",
              output_fn =       "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.dhydrt.ifc.txt")

################################## full function for MakeDhydrtIfc for VDMT data
food_records_fn=  "VDMTdata/asa24_Items_QC.txt"
food_record_id=   "SampleID"
food_taxonomy_fn= "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.tax.txt"
output_fn=        "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.dhydrt.ifc.txt"

diet <- read.table(food_records_fn, header = TRUE, sep="\t", colClasses="character", quote="", strip.white=T)
diet$FoodAmt <- as.numeric(diet$FoodAmt)
diet$KCAL <- as.numeric(diet$KCAL)
diet$MOIS <- as.numeric(diet$MOIS)
diet$dhydrt <- (diet$FoodAmt - diet$MOIS)
cdiet <- aggregate(diet$dhydrt, by=list(diet[,food_record_id], diet$FoodID), FUN=sum)
colnames(cdiet) <- c(food_record_id, "FoodID", "dhydrt")
cdiet.w <- reshape(cdiet, timevar = "FoodID", idvar = food_record_id, direction = "wide")
cdiet.w[is.na(cdiet.w)] <- 0
rownames(cdiet.w) <- cdiet.w[,1] # make record_ids the rownames
cdiet.w <- cdiet.w[,-1]
colnames(cdiet.w) <- gsub("dhydrt.", "", colnames(cdiet.w)) #rename column names to FoodIDs only
t.cdiet.w <- t(cdiet.w)

dhydrt.ifc <- merge(t.cdiet.w, food.taxonomy_u, by=0)
colnames(dhydrt.ifc)[578] <- 'Main.food.description'
rownames(dhydrt.ifc) <- dhydrt.ifc[,"Main.food.description"]
remove.col.ix <- which(colnames(dhydrt.ifc) %in% c("Main.food.description", "Row.names"))
dhydrt.ifc <- dhydrt.ifc[,-remove.col.ix]
inf.vals <- which(rowSums(dhydrt.ifc[,-ncol(dhydrt.ifc)]) == Inf)
dhydrt.ifc <- dhydrt.ifc[!(rownames(dhydrt.ifc) %in% names(inf.vals)),]
cat("#FOODID\t", file=output_fn)

write.table(dhydrt.ifc, output_fn, sep = "\t", quote = F, append=TRUE)

###### also for the future, instead of asa24_Items_f_id_s_m_QCed_4Lv.tax.txt, use
write.table(food.taxonomy_u, "VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv_updated.tax.txt")
