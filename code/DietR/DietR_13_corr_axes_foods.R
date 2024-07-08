# ===============================================================================================================
# Generate a heatmap of correlation between food categories and ordination Axes.
# Version 2
# Created on 02/16/2023 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC".
# The create_corr_frame function credit: Mo Hutti.
# ===============================================================================================================

# In this script, we will analyze correlation between ordination axes values and foods.

# Set your working directory to the main directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# load the necessary packages and the source code.
library(ggplot2)
source("GitHub_tutorial/DietR/lib/specify_data_dir.R")
source("GitHub_tutorial/DietR/lib/corr_axes_foods.R") # I had to change directory paths within this file for it to work
source("GitHub_tutorial/DietR/lib/ggplot2themes.R")
source("GitHub_tutorial/DietR/lib/SubsetByFirstChaInCol.R")
source("GitHub_tutorial/DietR/lib/create_corr_frame.R")

# Load the distinct 100 colors for use.
distinct100colors <- readRDS("GitHub_tutorial/DietR/lib/distinct100colors.rda")

# You can come back to the main directory by:
setwd(main_wd)

# Specify the directory where the data is.
SpecifyDataDirectory("AJdata/Ordination/")

# ===============================================================================================================
# Weighted unifrac distance ordination results
# ===============================================================================================================

# From sorted IFC table, generate a table of total amount of food consumed by all the individuals,
# and a table with correlation coefficients, p-values, and q-values with desired threshold between
# food items and Axes that were saved in the ordination section.
# Be careful about not to confuse WEIGHTED and UNweighted unifrac distances.

## Investigating these two docs bc CorrAxesFood isn't making sense:
## I added a column name manually to this copy so that it reads in correctly
## to_fix <- read_tsv('../Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted_copy.txt')
## meta <- read_tsv('MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt')
## colnames(to_fix) # 342 rows: same ones are missing
## rows <- sort(meta$Row.names) # missing 13-16, 148-151, 278-279 # THIS WAS OUT OF ORDER
## length(rows) # 342 rows

# WEIGHTED unifrac distance results. # THIS ISN"T WORKING
#CorrAxesFood(food_ifc_soted = "../Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted_copy.txt",
#             AmountSums_out_fn = "MCTs_23887_Items_f_id_s_m_QCed_4Lv_AmountSums.txt",
#             qval_threshold = 0.05,
#             meta_users =            "MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt",
#             corr_axes_foods_outfn = "MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")

  # food_ifc_soted:     xxx.food.ifc.sorted.txt file, saved in the ordination section.
  # AmountSums_out_fn:  output filename to be saved which has the total consumption amount of each food.
  # qval_threshold:     q-value threshold to call a correlation significant.
  # meta_users:         xxx.meta_users.txt file, waved in the ordination section.
  # corr_axes_foods_outfn: output filename to be saved which has the correlation between foods and Axes.

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-# CorrAxesFood function #-#-#-#-#-#-#-#-#-#-#-#-#-#

qval_threshold = 0.05
food1 <- read.delim('../Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted_copy.txt', row.names = 1)
food2 <- food1[, !colnames(food1) == "taxonomy"]

## this confirms the format: the foods should be rownames, not their own column
##testing <- read.delim('/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package/GitHub_tutorial/DietR/eg_data/VVKAJ/Foodtree/VVKAJ_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt')
## therefore, I added row.names = 1 to the above read.delim

food3 <- as.data.frame(t(food2)) # transpose so rows will be the individuals (for which a distance matrix is calculated)
food3_s <- food3[order(rownames(food3)), ] # sort individuals (rows) in order.
x <- food3_s

# Save the total amount consumed by all the individuals.
write.table(x=as.data.frame(colSums(x)),
            file = 'MCTs_23887_Items_f_id_s_m_QCed_4Lv_AmountSums.txt',
            sep="\t", row.names=T, quote=F )

loaded_leg <- read.table('MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt',
                         sep="\t", header=T) # Load the meta_users, which has userID and Axis values.
rownames(loaded_leg) <- loaded_leg$Row.names # Add rownames: X83732 etc. This will stay even after selecting only Axis. columns.
loaded_leg <- loaded_leg[order(loaded_leg$Row.names),] # NEEDED TO SORT

# pick up only columns whose names start with "Axis.".
loaded_leg_Axisonly <- SubsetByFirstChaInCol(input.df = loaded_leg, starting.str = "Axis.")
y <- as.data.frame(loaded_leg_Axisonly)  # food group values

# make sure the samples are the same.
if( identical(rownames(x), rownames(y)) == F){
  return("The columnnames of X and Y are different. Ensure your food_ifc_soted and\n
                       meta_users have the same set of individuals.")
}else{
  # Now test correlation of each of the columns in x with columns in y. This will take several minutes.
  # The variables (food items) that have been tested will be printed out in the console.
  dat <- create_corr_frame(x, y)

  # Change column names of x and y to more meaningful ones.
  colnames(dat)[1:2] <- c("Food","Axis")

  # Mark rows that have qvalues < 0.25 with an asterisk in a column called "Significance".
  dat$Significance <- cut(dat$qval, breaks=c(-Inf, qval_threshold, Inf), label=c("*", ""))

  # Sort dat by qval (small to large).
  dat_s <- dat[order(dat$qval), ]

  write.table(x=dat_s,
              file = 'MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt',
              sep="\t", row.names=F, quote=F)
}

#-#-#-#-#-#-#-#-#-#-#-#-# End of CorrAxesFood function #-#-#-#-#-#-#-#-#-#-#-#-#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# ---------------------------------------------------------------------------------------------------------------
# Load and analyze the output.

dat_AJ <- read.delim("MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")

# Check the number of food items with significant q-values.
nrow(subset(dat_AJ, Significance=="*"))

# There are 8 food items significantly correlated with one of the axes.
# Show only food items that are significantly correlated with one of the axes.
subset(dat_AJ, Significance=="*")

# ELLA: there are 218 in ours...

# Axis.1 is positively correlated with miso soup, green tea, white rice, and soy sauce.
# Those are commonly used in Japanese cuisine. So, it makes sense that Japanese-diet consumers are
# clustered on the right side of the biplot (Axis.1 x Axis.2 plot). Similarly, Axis.2 is negatively
# correlated with Avocado and Nutritional powder mix, whey based, and they are commonly found in
# keto (high protein, high fat, low carb) diet. And keto-diet consuming individuals tend to have lower
# Axis.2 values in the biplot.
# Correlation between axes and foods can be useful in identifying characteristics of the diet based on
# clustered individuals on the ordination biplot.

# It is also possible to view each axis separately.
# Select Axis 1 rows
dat_AJ_1 <- subset(dat_AJ, Axis=="Axis.1")
head(dat_AJ_1[order(dat_AJ_1$qval), ], 10)

# Select Axis 2 rows and sort by qval.
dat_AJ_2 <- subset(dat_AJ, Axis=="Axis.2")
head(dat_AJ_2[order(dat_AJ_2$qval), ], 10)

# Select Axis 3 rows and sort by qval.
dat_AJ_3 <- subset(dat_AJ, Axis=="Axis.3")
head(dat_AJ_3[order(dat_AJ_3$qval), ], 10)

# Select Axis 4 rows and sort by qval.
dat_AJ_4 <- subset(dat_AJ, Axis=="Axis.4")
head(dat_AJ_4[order(dat_AJ_4$qval), ], 10)


# ===============================================================================================================
# UNweighted unifrac distance ordination results.
# ===============================================================================================================

# xxx_AmountSums.txt will be generated again, but its content will be the same regardless of which distance method
# (weighted or unweighted unifrac or else) was used, as long as the food.ifc_sorted is the same.

#CorrAxesFood(food_ifc_soted = "../Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt",
#             AmountSums_out_fn = "MCTs_23887_Items_f_id_s_m_QCed_4Lv_AmountSums.txt",
#             qval_threshold = 0.05,
#             meta_users =            "MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_meta_users.txt",
#             corr_axes_foods_outfn = "MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-# CorrAxesFood function #-#-#-#-#-#-#-#-#-#-#-#-#-#

qval_threshold = 0.05
food1 <- read.delim('../Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted_copy.txt', row.names = 1)
food2 <- food1[, !colnames(food1) == "taxonomy"]
food3 <- as.data.frame(t(food2)) # transpose so rows will be the individuals (for which a distance matrix is calculated)
food3_s <- food3[order(rownames(food3)), ] # sort individuals (rows) in order.
x <- food3_s

loaded_leg <- read.table('MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_meta_users.txt',
                         sep="\t", header=T) # Load the meta_users, which has userID and Axis values.
rownames(loaded_leg) <- loaded_leg$Row.names # Add rownames: X83732 etc. This will stay even after selecting only Axis. columns.
loaded_leg <- loaded_leg[order(loaded_leg$Row.names),] # NEEDED TO SORT

# pick up only columns whose names start with "Axis.".
loaded_leg_Axisonly <- SubsetByFirstChaInCol(input.df = loaded_leg, starting.str = "Axis.")
y <- as.data.frame(loaded_leg_Axisonly)  # food group values

# make sure the samples are the same.
if( identical(rownames(x), rownames(y)) == F){
  return("The columnnames of X and Y are different. Ensure your food_ifc_soted and\n
                       meta_users have the same set of individuals.")
}else{
  # Now test correlation of each of the columns in x with columns in y. This will take several minutes.
  # The variables (food items) that have been tested will be printed out in the console.
  dat <- create_corr_frame(x, y)

  # Change column names of x and y to more meaningful ones.
  colnames(dat)[1:2] <- c("Food","Axis")

  # Mark rows that have qvalues < 0.25 with an asterisk in a column called "Significance".
  dat$Significance <- cut(dat$qval, breaks=c(-Inf, qval_threshold, Inf), label=c("*", ""))

  # Sort dat by qval (small to large).
  dat_s <- dat[order(dat$qval), ]

  write.table(x=dat_s,
              file = 'MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt',
              sep="\t", row.names=F, quote=F)
}

#-#-#-#-#-#-#-#-#-#-#-#-# End of CorrAxesFood function #-#-#-#-#-#-#-#-#-#-#-#-#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# ---------------------------------------------------------------------------------------------------------------
# Load and analyze the output.

# UNweighted can be viewed in the same way.
dat_AJ <- read.delim("MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt")

# Check the number of food items with significant q-values.
nrow(subset(dat_AJ, Significance=="*"))

# Show only food items that are significantly correlated with one of the axes.
subset(dat_AJ, Significance=="*")

# ELLA: THERE ARE 381 SIGNIFICANT FOODS IN THE AJ DATA

dat_AJ_1 <- subset(dat_AJ, Axis=="Axis.1")
head(dat_AJ_1[order(dat_AJ_1$qval), ], 10)
dat_AJ_2 <- subset(dat_AJ, Axis=="Axis.2")
head(dat_AJ_2[order(dat_AJ_2$qval), ], 10)
dat_AJ_3 <- subset(dat_AJ, Axis=="Axis.3")
head(dat_AJ_3[order(dat_AJ_3$qval), ], 10)
dat_AJ_4 <- subset(dat_AJ, Axis=="Axis.4")
head(dat_AJ_4[order(dat_AJ_4$qval), ], 10)

# ===============================================================================================================
# SAME THING AS ABOVE, BUT NOW WITH OUT VD DATA INSTEAD OF AJ'S
# ===============================================================================================================
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package/VDMTdata/Ordination")

CorrAxesFood(food_ifc_soted = "../Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt",
             AmountSums_out_fn = "asa24_Items_f_id_s_m_QCed_4Lv_AmountSums.txt",
             qval_threshold = 0.05,
             meta_users =            "asa24_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt",
             corr_axes_foods_outfn = "asa24_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")

# ---------------------------------------------------------------------------------------------------------------
# Load and analyze the output.

dat_VD <- read.delim("asa24_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_corr_axes_foods_thr0.05.txt")

# Check the number of food items with significant q-values.
nrow(subset(dat_VD, Significance=="*"))

# There are *226* food items significantly correlated with one of the axes.
# Show only food items that are significantly correlated with one of the axes.
subset(dat_VD, Significance=="*")

# Axis.1 is positively correlated with miso soup, green tea, white rice, and soy sauce.
# Those are commonly used in Japanese cuisine. So, it makes sense that Japanese-diet consumers are
# clustered on the right side of the biplot (Axis.1 x Axis.2 plot). Similarly, Axis.2 is negatively
# correlated with Avocado and Nutritional powder mix, whey based, and they are commonly found in
# keto (high protein, high fat, low carb) diet. And keto-diet consuming individuals tend to have lower
# Axis.2 values in the biplot.
# Correlation between axes and foods can be useful in identifying characteristics of the diet based on
# clustered individuals on the ordination biplot.

# It is also possible to view each axis separately.
# Select Axis 1 rows
dat_VD_1 <- subset(dat_VD, Axis=="Axis.1")
head(dat_VD_1[order(dat_VD_1$qval), ], 10)

# Select Axis 2 rows and sort by qval.
dat_VD_2 <- subset(dat_VD, Axis=="Axis.2")
head(dat_VD_2[order(dat_VD_2$qval), ], 10)

# Select Axis 3 rows and sort by qval.
dat_VD_3 <- subset(dat_VD, Axis=="Axis.3")
head(dat_VD_3[order(dat_VD_3$qval), ], 10)

# Select Axis 4 rows and sort by qval.
dat_VD_4 <- subset(dat_VD, Axis=="Axis.4")
head(dat_VD_4[order(dat_VD_4$qval), ], 10)

# ===============================================================================================================
# UNweighted unifrac distance ordination results.
# ===============================================================================================================

# xxx_AmountSums.txt will be generated again, but its content will be the same regardless of which distance method
# (weighted or unweighted unifrac or else) was used, as long as the food.ifc_sorted is the same.

CorrAxesFood(food_ifc_soted = "../Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt",
             AmountSums_out_fn = "asa24_Items_f_id_s_m_QCed_4Lv_AmountSums.txt",
             qval_threshold = 0.05,
             meta_users =            "asa24_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_meta_users.txt",
             corr_axes_foods_outfn = "asa24_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt")

# ---------------------------------------------------------------------------------------------------------------
# Load and analyze the output.

# UNweighted can be viewed in the same way.
dat_VD <- read.delim("asa24_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_corr_axes_foods_thr0.05.txt")

# Check the number of food items with significant q-values.
nrow(subset(dat_VD, Significance=="*")) # 469 significant foods

dat_VD_1 <- subset(dat_VD, Axis=="Axis.1")
head(dat_VD_1[order(dat_VD_1$qval), ], 10)
dat_VD_2 <- subset(dat_VD, Axis=="Axis.2")
head(dat_VD_2[order(dat_VD_2$qval), ], 10)
dat_VD_3 <- subset(dat_VD, Axis=="Axis.3")
head(dat_VD_3[order(dat_VD_3$qval), ], 10)
dat_VD_4 <- subset(dat_VD, Axis=="Axis.4")
head(dat_VD_4[order(dat_VD_4$qval), ], 10)

