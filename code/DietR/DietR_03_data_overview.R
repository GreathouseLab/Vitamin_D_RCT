# ===============================================================================================================
# Take an overview of ASA24 items and totals data.
# Version 2 without the line plot.
# Created on 11/10/2022 by Rie Sadohara
# Updated on 03/08/2023 by Rie Sadohara
# ===============================================================================================================

# We can view summary statistics of either the individual food data or the totals data. First, let us take
# a look at theitems data. This section is not intended to be a complete guide to analysis, but rather to give
# you some ideas for how to explore this data.

# ===============================================================================================================
# Set working directory
# ===============================================================================================================

# Set your working directory to the main directory.
#Session --> Set working directory --> Choose directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# Import source code to run the analyses to follow and generate plots.
source("GitHub_tutorial/DietR/lib/specify_data_dir.R")
source("GitHub_tutorial/DietR/lib/data_overview.R")
source("GitHub_tutorial/DietR/lib/ggplot2themes.R")


# Make a vector of colors for each factor of Diet so that plots will have consistent colors.
diet_colors <- c('#e89c54', '#d06c5c', '#f0c4b4', '#a0bca4', '#a8bccc', '#d8dce4')
#names(diet_colors) <- c()

setwd(main_wd)

# ===============================================================================================================
# Load and analyze (QC-ed) ASA24 food items data
# ===============================================================================================================

# Load your items data to be analyzed.
# "_f_id_s_m" stands for: "food names formatted", "SampleID added", "selected individuals",
# "metadata merged", and "the individuals that passed the QC of averaged totals".
items_AJ_QCed <- read.delim("AJdata/MCTs_23887_Items_QC.txt", sep="\t", header=T)
items_VD_QCed <- read.delim("VDMTdata/asa24_Items_QC.txt", sep="\t", header=T)

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics

# Summary statistics of one variable can be obtained by using R's summary() function.

# View min, quantiles, mean, etc. for a variable in your dataset.
summary(items_AJ_QCed$KCAL)
summary(items_VD_QCed$KCAL)

# To calculate these summary statistics for multiple variables, use the SummaryStats() function.
# Calculate the minimum, 1st quantile, median, mean, 3rd quantile, max, and standard deviation
# for each variable in the input dataframe and save as a .txt file.
SummaryStats(inputdf = items_AJ_QCed,
             outfn = "AJdata/MCTs_23887_items_AJ_QCed_summ.txt")
SummaryStats(inputdf = items_VD_QCed,
             outfn = "VDMTdata/asa24_items_VD_QCed_summ.txt")

# [NOTE] These are individual items, not by user or day.

items_AJ_QCed[which.max(items_AJ_QCed$KCAL), ] # 8 pizza (slices?)
items_VD_QCed[which.max(items_VD_QCed$KCAL), ] # a whole brownie pan

items_VD_QCed_test <- items_VD_QCed[-which.max(items_VD_QCed$KCAL), ]
items_VD_QCed_test[which.max(items_VD_QCed_test$KCAL), c(1:5, 12, 26, 29:30, 133)] # 2nd highest is pizza (HowMany = 6)
items_VD_QCed_test <- items_VD_QCed_test[- which.max(items_VD_QCed_test$KCAL), ]
items_VD_QCed_test[which.max(items_VD_QCed_test$KCAL), c(1:5, 12, 26, 29:30, 133)] # 3rd is 14 chicken breasts...
# participant 15, from 2/27/2022 (StudyDay 13), and RecallRecId 5330fad7-3700-4aba-9967-a8fd0bbe859d
items_VD_QCed_test <- items_VD_QCed_test[- which.max(items_VD_QCed_test$KCAL), ]
items_VD_QCed_test[which.max(items_VD_QCed_test$KCAL), c(1:5, 12, 26, 29:30, 133)] # 4th is pizza (HowMany = 8)
items_VD_QCed_test <- items_VD_QCed_test[- which.max(items_VD_QCed_test$KCAL), ]
items_VD_QCed_test[which.max(items_VD_QCed_test$KCAL), c(1:5, 12, 26, 29:30, 133)] # 5th is sandwich meat 

items_AJ_QCed_test <- items_AJ_QCed[-which.max(items_AJ_QCed$KCAL), ]
items_AJ_QCed_test[which.max(items_AJ_QCed_test$KCAL), ] # 2nd highest is 6 slices of pizza
items_AJ_QCed_test <- items_AJ_QCed_test[- which.max(items_AJ_QCed_test$KCAL), ]
items_AJ_QCed_test[which.max(items_AJ_QCed_test$KCAL), ] # 3rd is 4 tacos/tostadas
items_AJ_QCed_test <- items_AJ_QCed_test[- which.max(items_AJ_QCed_test$KCAL), ]
items_AJ_QCed_test[which.max(items_AJ_QCed_test$KCAL), ] # 4th is 5 pizzas

# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.

# Boxplot of KCAL by users.
users_kcal_AJ <- ggplot(items_AJ_QCed, aes(x=UserName, y=KCAL)) +
  geom_boxplot() + no_grid + space_axes + rotate_X_labels
users_kcal_AJ

ggsave("output/MCTs_23887_ITEMS_QC_users_kcal.jpeg", users_kcal_AJ,
       device="jpeg", width=8, height=4.6, units="in")

users_kcal_VD <- ggplot(items_VD_QCed, aes(x=UserName, y=KCAL)) +
  geom_boxplot() + no_grid + space_axes + rotate_X_labels
users_kcal_VD

ggsave("output/asa24_ITEMS_QC_users_kcal.jpeg", users_kcal_VD,
       device="jpeg", width=10, height=4.6, units="in")

# Similarly, generate a boxplot of KCAL by gender.
gender_kcal_AJ <- ggplot(items_AJ_QCed, aes(x=Gender, y=KCAL)) +
  geom_boxplot() + no_grid + space_axes
gender_kcal_AJ

# Save it as file.
ggsave("output/MCTs_23887_ITEMS_QC_sex_kcal.jpeg", gender_kcal_AJ,
         device="jpeg", width=3.5, height=4.6, units="in")

# same for VDMT # male is 1, female is 2
gender_kcal_VD <- ggplot(items_VD_QCed, aes(x=as.character(SEX), y=KCAL)) +
  geom_boxplot() + no_grid + space_axes + scale_x_discrete("SEX", labels = c('M', 'F'))
gender_kcal_VD

ggsave("output/asa24_ITEMS_QC_sex_kcal.jpeg", gender_kcal_VD,
       device="jpeg", width=3.5, height=4.6, units="in")

# ---------------------------------------------------------------------------------------------------------------
# Scatterplot

# Scatterplots can be generated to look at the relationship between two numeric
# variables. Here we look at total fat and kilocalories. We would expect these
# values to be related because fat contributes a high number of calories in foods.

# Scatterplot of two numeric variables: TFAT and KCAL.
TFAT_KCAL_AJ <- ggplot(items_AJ_QCed, aes(x=TFAT, y=KCAL)) +
  geom_point() + no_grid + space_axes + theme(aspect.ratio = 1)
TFAT_KCAL_AJ

# Save it as file.
ggsave("output/MCTs_23887_ITEMS_QC_KCAL_TFAT.jpeg", TFAT_KCAL_AJ,
         device="jpeg", width=4.6, height=4.1, units="in")

TFAT_KCAL_VD <- ggplot(items_VD_QCed, aes(x=TFAT, y=KCAL)) +
  geom_point() + no_grid + space_axes + theme(aspect.ratio = 1)
TFAT_KCAL_VD

# Save it as file.
ggsave("output/asa24_ITEMS_QC_KCAL_TFAT.jpeg", TFAT_KCAL_VD,
       device="jpeg", width=4.6, height=4.1, units="in")

# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient
cor.test(x=items_AJ_QCed$TFAT, y=items_AJ_QCed$KCAL, method="pearson")
cor.test(x=items_VD_QCed$TFAT, y=items_VD_QCed$KCAL, method="pearson")

# in both datasets, kcal and tfat IS correlated significantly

# ===============================================================================================================
# Load and analyze (QC-ed) ASA24 mean totals data
# ===============================================================================================================

# Load your QC-ed mean totals data to be analyzed
tot_mean_m_QCed_AJ <- read.delim("AJdata/MCTs_23887_Tot_mean_m_QC.txt")
tot_mean_m_QCed_VD <- read.delim("VDMTdata/asa24_Tot_mean_m_QC.txt")

# Note that each row is the mean of total dietary intake of each user.
tot_mean_m_QCed_AJ[1:4, 1:4]
tot_mean_m_QCed_VD[1:4, 1:4]

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics

# Summary statistics of one variable
summary(tot_mean_m_QCed_AJ$KCAL)
summary(tot_mean_m_QCed_VD$KCAL)

# Calculate the min, quantiles, mean, etc. for a variable in your dataset
# in the same way we did with the items.
SummaryStats(inputdf = tot_mean_m_QCed_AJ,
             outfn = "AJdata/MCTs_23887_tot_mean_m_QCed_summ.txt")
SummaryStats(inputdf = tot_mean_m_QCed_VD,
             outfn = "VDMTdata/asa24_tot_mean_m_QCed_summ.txt")

# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.

# Create a vector named "Diet_by_median" containing the INDEPENDENT VAR in a desired order
# (by median in this case).
Diet_by_median_AJ <- with(tot_mean_m_QCed_AJ, reorder(Supplement, KCAL, median, na.rm=T))
Diet_by_median_VD <- with(tot_mean_m_QCed_VD, reorder(Intervention, KCAL, median, na.rm=T))

# Diet_by_median is a factor, which contains the diets of all the 15 participants and the median values of
# each Diet group. "Levels" show the order of them.
Diet_by_median_AJ
Diet_by_median_VD

# Show the levels of this factor. This will be useful in plotting a factor in a desired order.
levels(Diet_by_median_AJ)
levels(Diet_by_median_VD)

# Generate a boxplot of KCAL by diet of the participants.
diet_KCAL_t_AJ <- ggplot(tot_mean_m_QCed_AJ,
                         aes(x=Diet_by_median_AJ, y=KCAL, fill=Diet_by_median_AJ)) +
  geom_boxplot() + labs(x="Intervention") +
  theme(legend.position = "none") + # hide legend
  scale_fill_manual(values=diet_colors) +
  no_grid + space_axes + rotate_X_labels
diet_KCAL_t_AJ

diet_KCAL_t_VD <- ggplot(tot_mean_m_QCed_VD,
                         aes(x=Diet_by_median_VD, y=KCAL, fill=Diet_by_median_VD)) +
  geom_boxplot() + labs(x="Intervention") +
  theme(legend.position = "none") + # hide legend
  scale_fill_manual(values=diet_colors) +
  no_grid + space_axes + rotate_X_labels
diet_KCAL_t_VD

# Save it as a .pdf file.
ggsave("output/MCTs_23887_tot_mean_m_QCed_int_KCAL.jpeg", diet_KCAL_t_AJ,
       device="jpeg", width=5, height=4.5)
ggsave("output/asa24_tot_mean_m_QCed_int_KCAL.jpeg", diet_KCAL_t_VD,
       device="jpeg", width=5, height=4.5)

# Boxplot of KCAL by Diet, with each datapoint.
# [NOTE] geom_boxplot must have outlier.shape = NA when plotted with geom_jitter.
# Otherwise, outlier points will be duplicated and will be misleading.
diet_KCAL_t_dots_AJ <- ggplot(tot_mean_m_QCed_AJ,
                           aes(x=Diet_by_median_AJ, y=KCAL, fill=Diet_by_median_AJ)) +
  geom_boxplot(outlier.shape = NA) + labs(x="Intervention") +
  geom_jitter(width=0.3) +
  theme(legend.position = "none") + # hide legend
  scale_fill_manual(values=diet_colors) +
  no_grid + space_axes + rotate_X_labels
diet_KCAL_t_dots_AJ

diet_KCAL_t_dots_VD <- ggplot(tot_mean_m_QCed_VD,
                              aes(x=Diet_by_median_VD, y=KCAL, fill=Diet_by_median_VD)) +
  geom_boxplot(outlier.shape = NA) + labs(x="Intervention") +
  geom_jitter(width=0.3) +
  theme(legend.position = "none") + # hide legend
  scale_fill_manual(values=diet_colors) +
  no_grid + space_axes + rotate_X_labels
diet_KCAL_t_dots_VD

# Save it as a .pdf file.
ggsave("output/MCTs_23887_tot_mean_m_QCed_int_KCAL_dots.jpeg", diet_KCAL_t_dots_AJ,
         device="jpeg", width=5, height=4.5)
ggsave("output/asa24_tot_mean_m_QCed_int_KCAL_dots.jpeg", diet_KCAL_t_dots_VD,
       device="jpeg", width=5, height=4.5)

# ---------------------------------------------------------------------------------------------------------------
# Scatterplot

# Generate a scatterplot of two variables, color-coded by Diet.
# Show the diets in the order of median, based on the boxplot that was generated above.
TFAT_KCAL_t_AJ <- ggplot(tot_mean_m_QCed_AJ, aes(x=TFAT, y=KCAL, fill=Diet_by_median_AJ)) +
    geom_point(shape=21, size=3, color="black") + no_grid + space_axes +
    scale_fill_manual(values= diet_colors[2:3]) +
    labs(fill="Diet Intervention") + theme(aspect.ratio = 1)
TFAT_KCAL_t_AJ
ggsave("output/MCTs_23887_tot_mean_m_QCed_TFAT_KCAL.jpeg", TFAT_KCAL_t_AJ,
       device="jpeg", width=5.5, height=4)

TFAT_KCAL_t_VD <- ggplot(tot_mean_m_QCed_VD, aes(x=TFAT, y=KCAL, fill=Diet_by_median_VD)) +
  geom_point(shape=21, size=3, color="black") + no_grid + space_axes +
  scale_fill_manual(values= diet_colors[2:3]) +
  labs(fill="Diet Intervention") + theme(aspect.ratio = 1)
TFAT_KCAL_t_VD
ggsave("output/asa24_tot_mean_m_QCed_TFAT_KCAL.jpeg", TFAT_KCAL_t_VD,
       device="jpeg", width=5.5, height=4)

# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient.
cor.test(x=tot_mean_m_QCed_AJ$TFAT, y=tot_mean_m_QCed_AJ$KCAL, method="pearson")
cor.test(x=tot_mean_m_QCed_VD$TFAT, y=tot_mean_m_QCed_VD$KCAL, method="pearson")

# in both datasets, kcal and tfat are correlated significantly,
# which is consistent with what was tested above

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory before you start running another script.
setwd(main_wd)
