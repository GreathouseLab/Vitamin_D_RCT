# ===============================================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat.
# Use the average of totals of two days.
# Version 2
# Created on 11/10/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
source("GitHub_tutorial/DietR/lib/specify_data_dir.R")
source("GitHub_tutorial/DietR/lib/percent_kcal.R")
source("GitHub_tutorial/DietR/lib/ggplot2themes.R")

# Call color palette.
diet_colors <- c('#e89c54', '#d06c5c', '#f0c4b4', '#a0bca4', '#a8bccc', '#d8dce4')
distinct100colors <- readRDS("GitHub_tutorial/DietR/lib/distinct100colors.rda")

# You can come back to the main directory by:
setwd(main_wd)

# ===============================================================================================================
# Import data from your data directory
# ===============================================================================================================

# Load the totals data.
totals_AJ <- read.table("AJdata/MCTs_23887_Tot_m_QC.txt", sep="\t", header=T)
totals_VD <- read.table("VDMTdata/asa24_Tot_m_QC.txt", sep="\t", header=T)

# ===============================================================================================================
# Calculate the percentage of calories from Carbohydrate, Protein, and Total Fat.
# ===============================================================================================================

# We will calculate the percentage of calories from each of the three macronutrients
# in the sum of calories from the three macronutrients. Thus, the percentage of
# calories from CARB, PROT, and TFAT will add up to 100.

# Calculate the %KCAL of CARB, PROT, and TFAT for each user and take means by Group (Diet).
CPTpctKcalPerUser(inputfn=totals_AJ, group='Supplement', across='UserName',
                  outfn="AJdata/MCTs_23887_Tot_mean_m_QCed_CPT_kcal.txt")
CPTpctKcalPerUser(inputfn=totals_VD, group='Intervention', across='UserName',
                  outfn="VDMTdata/asa24_Tot_mean_m_QCed_CPT_kcal.txt")

# Load the output.
CPT_kcal_AJ <- read.table("AJdata/MCTs_23887_Tot_mean_m_QCed_CPT_kcal.txt", sep="\t", header=T)
CPT_kcal_VD <- read.table("VDMTdata/asa24_Tot_mean_m_QCed_CPT_kcal.txt", sep="\t", header=T)

# CPT_kcal has Group, macronutrient, n, mean, and sd of each group.
# Do not alter the column names of CPT_kcal in order to use the plotting functions in this script.
CPT_kcal_AJ
CPT_kcal_VD

### GENERALLY, IT SEEMS THAT OUR AVERAGES ARE COMPARABLE, BUT SHE HAD LARGER SDs
### HOWEVER, THIS COULD BE DUE TO THE LESS RIGOROUS CLEANING THAT WAS DONE ON HER DATA
### PER MY WORK WITHIN THE DIETR PACKAGE

# ===============================================================================================================
# Generate a stacked barchart without SD.
# ===============================================================================================================

# Order Diets by a certain macronutrient by the "order.by" argument.
# You can also specify the stacking order of all the macronutrients by the "macronu.order" argument.
# Note that the last item will be on the bottom of the barchart.
PlotStackedwoSD(data=CPT_kcal_AJ,
                order.by = "Protein",
                macronut.order=c("Carbohydrate", "Total Fat", "Protein")) 

# The chart is saved as "stacked_wo_SD".
stacked_wo_SD

ggsave("output/MCTs_23887_Tot_mean_m_QCed_CPT_kcal_wo_SD.jpeg", stacked_wo_SD,
         device="jpeg", width=6.2, height=4.2, units="in")

PlotStackedwoSD(data=CPT_kcal_VD,
                order.by = "Protein",
                macronut.order=c("Carbohydrate", "Total Fat", "Protein"))

# The chart is saved as "stacked_wo_SD".
stacked_wo_SD

ggsave("output/asa24_Tot_mean_m_QCed_CPT_kcal_wo_SD.jpeg", stacked_wo_SD,
       device="jpeg", width=6.2, height=4.2, units="in")

# ===============================================================================================================
# Generate the "dodge"-type of barchart (3 bars per user, NOT STACKED).
# ===============================================================================================================

# Order Diets by a certain macronutrient by the "order.by" argument. You can also specify the plotting order of all the
# macronutrients by the "macronu.order" argument. Note that the first item will be the leftmost bar.
PlotDodged(data= CPT_kcal_AJ,
           order.by = "Protein",
           macronut.order=c("Carbohydrate", "Total Fat", "Protein"))
dodged_w_SD

ggsave("output/MCTs_23887_Tot_mean_m_QCed_CPT_kcal_dodged_wo_SD.jpeg", dodged_w_SD,
         device="jpeg", width=6, height=4.5, units="in")

PlotDodged(data= CPT_kcal_VD,
           order.by = "Protein",
           macronut.order=c("Carbohydrate", "Total Fat", "Protein"))
dodged_w_SD

ggsave("output/asa24_Tot_mean_m_QCed_CPT_kcal_dodged_wo_SD.jpeg", dodged_w_SD,
       device="jpeg", width=6, height=4.5, units="in")

# ===============================================================================================================
# Generate a stacked barchart with SD as error bars.
# ===============================================================================================================

# Create a vector that contains all the group levels (diets, in this case).
# This "groups" vector will be used in the CalcStackedSD function within the PlotStackedWithSD function.
groups <- unique(CPT_kcal_AJ$Group)

# Order Diet by a certain macronutrient by the "order.by" argument. You can also specify the stacking order of all the
# macronutrients by the "macronu.order" argument. Note that the last item will be on the bottom of the barchart.
PlotStackedWithSD(data= CPT_kcal_AJ,
                  order.by = "Protein",
                  macronut.order=c("Carbohydrate", "Total Fat", "Protein")) + 
  scale_fill_manual(values = diet_colors)

stacked_with_SD

ggsave("output/MCTs_23887_Tot_mean_m_QCed_CPT_kcal_with_SD.jpeg", stacked_with_SD +
         scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)),
       device="jpeg", width=6, height=4.5, units="in")

groups <- unique(CPT_kcal_VD$Group)
PlotStackedWithSD(data= CPT_kcal_VD,
                  order.by = "Protein",
                  macronut.order=c("Carbohydrate", "Total Fat", "Protein")) + 
  scale_fill_manual(values = diet_colors)
stacked_with_SD

ggsave("output/asa24_Tot_mean_m_QCed_CPT_kcal_with_SD.jpeg", stacked_with_SD,
       device="jpeg", width=6, height=4.5, units="in")

# Change the Y axis scale if necessary. Note that if the error bars of Carbohydrates disappear
# after changing the limits of Y axis, it may be because the error bars are higher than the max Y.
# Ensure you have enough max Y value to accommodate the error bars.

# You can also change the breakpoints of the Y axis.
stacked_with_SD + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100))

# --------------------------------------------------------------------------------------------------------------
# Come back to the main directory
setwd(main_wd)
