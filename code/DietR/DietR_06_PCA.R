# ===============================================================================================================
# PCA analysis with ASA24 data.
# Version 1
# Created on 12/16/2021 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as to the main directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# Load necessary pacages.
library(ggplot2)
library(ggfortify)

# Import source code to run the analyses to follow.
source("GitHub_tutorial/DietR/lib/specify_data_dir.R")
source("GitHub_tutorial/DietR/lib/ggplot2themes.R")
source("GitHub_tutorial/DietR/lib/PCA.R")

# Call color palette.
distinct100colors <- readRDS("GitHub_tutorial/DietR/lib/distinct100colors.rda")

# You can come back to the main directory by:
setwd(main_wd)

# Before you proceed to perform PCA, create folders called "PCA_Nut_asis", "PCA_Nut_ave",
# "PCA_Cat_asis", "PCA_Cat_asis" in your VVKAJ directory to save output.

# I created two of each within AJdata & VDMTdata

# THE FIRST HALF OF THIS SCRIPT IS AJ THEN SECOND IS VDMT

# ===============================================================================================================
# Nutrient data as is, processed for clustering analyses.
# ===============================================================================================================

# Load the Nut_asis data.
Tot_m_QCed_Nut_asis_AJ <- read.table(file="AJdata/MCTs_23887_Tot_m_QC_Nut_asis_c_rv.txt",
                                     sep="\t", header=T)
Tot_m_QCed_Nut_asis_VD <- read.table(file="VDMTdata/asa24_Tot_m_QC_Nut_asis_c_rv.txt",
                                     sep="\t", header=T)

# Name your input data.
pca_input_AJ <- Tot_m_QCed_Nut_asis_AJ
pca_input_VD <- Tot_m_QCed_Nut_asis_VD

# Ensure your input file has the correct number of rows and columns.
dim(pca_input_AJ)
dim(pca_input_VD)

# Scale the data and perform PCA.
scaled_pca_AJ <- prcomp(x = pca_input_AJ, scale=TRUE)
scaled_pca_VD <- prcomp(x = pca_input_VD, scale=TRUE)

# Specify the directory (folder) to save the results.
res_dir_nut_asis_AJ = "AJdata/PCA_Nut_asis"
res_dir_nut_asis_VD = "VDMTdata/PCA_Nut_asis"

# Specify the prefix of filenames to be saved.
res_prefix_nut_asis_AJ = "MCTs_23887_Nut_asis"
res_prefix_nut_asis_VD = "asa24_Nut_asis"

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
pca_input <- pca_input_AJ # necessary to run OutputPCA
scaled_pca <- scaled_pca_AJ # necessary to run OutputPCA
OutputPCA(pca.data = pca_input_AJ, pca.result = scaled_pca_AJ,
          out.dir = res_dir_nut_asis_AJ, out.prefix = res_prefix_nut_asis_AJ)

# Combine the input (Nut before processing) with all the variables and the PC results.
SaveInputAndPCs(input="AJdata/MCTs_23887_Tot_m_QC_Nut_asis_c.txt",
                pca.results = scaled_pca_AJ,
                out.dir = res_dir_nut_asis_AJ,
                out.prefix = res_prefix_nut_asis_AJ)

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
pca_input <- pca_input_VD # necessary to run OutputPCA
scaled_pca <- scaled_pca_VD # necessary to run OutputPCA
OutputPCA(pca.data = pca_input_VD, pca.result = scaled_pca_VD,
          out.dir = res_dir_nut_asis_VD, out.prefix = res_prefix_nut_asis_VD)

# Combine the input (Nut before processing) with all the variables and the PC results.
SaveInputAndPCs(input="VDMTdata/asa24_Tot_m_QC_Nut_asis_c.txt",
                pca.results = scaled_pca_VD,
                out.dir = res_dir_nut_asis_VD,
                out.prefix = res_prefix_nut_asis_VD)

# [Note] Even though the input file has both nutrients (Nut) and food categories (Cat) data,
# PCA was done with only either Nut or Cat, not both.

# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - Supplement/Intervention, in this case.
# NUT_ASIS

# Load the complete Nutrients data. (before filtering variables)
Nut_asis_c_AJ <- read.table("AJdata/MCTs_23887_Tot_m_QC_Nut_asis_c.txt", sep="\t", header=T)
Nut_asis_c_VD <- read.table("VDMTdata/asa24_Tot_m_QC_Nut_asis_c.txt", sep="\t", header=T)

# Change Diet to a factor so that factor levels will be displayed in order.
Nut_asis_c_AJ$Supplement <- factor(Nut_asis_c_AJ$Supplement, levels= c("EVOO", "MCT"))
Nut_asis_c_VD$Intervention <- factor(Nut_asis_c_VD$Intervention, levels= c("Yes", "No"))

# Use the autoplot function. Specify which PC in the x and y arguments.
# The 'data' argument needs the original input for PCA, not after selecting specific variables.
Nut_asis_PC12_diet_AJ <-
  autoplot(scaled_pca_AJ, x = 1, y = 2,
           loadings = T, loadings.label = T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = Nut_asis_c_AJ, size = 3) +
  geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill = Supplement)) +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1) +
  no_grid + space_axes +
  scale_fill_manual(values= distinct100colors)
Nut_asis_PC12_diet_AJ

ggsave("output/MCTs_23887_Nut_asis_PC12_diet5.jpeg",
       Nut_asis_PC12_diet_AJ, device="jpeg", width=7, height=6.5)

# Use the autoplot function. Specify which PC in the x and y arguments.
# The 'data' argument needs the original input for PCA, not after selecting specific variables.
Nut_asis_PC12_diet_VD <-
  autoplot(scaled_pca_VD, x = 1, y = 2,
           loadings = T, loadings.label = T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = Nut_asis_c_VD, size = 3) +
  geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill = Intervention)) +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1) +
  no_grid + space_axes +
  scale_fill_manual(values= distinct100colors)
Nut_asis_PC12_diet_VD

ggsave("output/asa24_Nut_asis_PC12_diet5.jpeg",
       Nut_asis_PC12_diet_VD, device="jpeg", width=7, height=6.5)

# asis is as is, meaning that the dataset of totals (not means)
# ave is the totals means, looking at nutrients or categories

# ===============================================================================================================
# Nutrient data averaged and processed for clustering analyses.
# ===============================================================================================================

# Load Nut_ave data.
Tot_m_QCed_Nut_ave_AJ <- read.table(file="AJdata/MCTs_23887_Tot_mean_m_QC_Nut_ave_c_rv.txt",
                                    sep="\t", header=T)
Tot_m_QCed_Nut_ave_VD <- read.table(file="VDMTdata/asa24_Tot_mean_m_QC_Nut_ave_c_rv.txt",
                                    sep="\t", header=T)

# Name your input data.
pca_input_AJ <- Tot_m_QCed_Nut_ave_AJ
pca_input_VD <- Tot_m_QCed_Nut_ave_VD

# Ensure your input file has the correct number of rows and columns.
dim(pca_input_AJ)
dim(pca_input_VD)

# Scale the data and perform PCA.
scaled_pca_AJ <- prcomp(x=pca_input_AJ, scale = TRUE)
scaled_pca_VD <- prcomp(x=pca_input_VD, scale = TRUE)

# Specify the directory (folder) to save the results.
res_dir_nut_ave_AJ = "AJdata/PCA_Nut_ave"
res_dir_nut_ave_VD = "VDMTdata/PCA_Nut_ave"

# Specify the prefix of filenames to be saved.
res_prefix_nut_ave_AJ = "MCTs_23887_Nut_ave"
res_prefix_nut_ave_VD = "asa24_Nut_ave"

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
# Input is your items/Nut input file before any prep for clustering, from which you derived the input for the PCA.
OutputPCA(pca.data = pca_input_AJ, pca.result = scaled_pca_AJ,
            out.dir = res_dir_nut_ave_AJ, out.prefix = res_prefix_nut_ave_AJ)

# Combine the input (before processing) with all the variables and the PC results.
# In the case of averaged data / user, the input file used here is xxx_ave_c.txt, which
# has all the variables before filtering out by correlation or zero variance.
SaveInputAndPCs(input="AJdata/MCTs_23887_Tot_mean_m_QC_Nut_ave_c.txt",
                pca.results = scaled_pca_AJ,
                out.dir= res_dir_nut_ave_AJ,
                out.prefix= res_prefix_nut_ave_AJ)

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
# Input is your items/Nut input file before any prep for clustering, from which you derived the input for the PCA.
OutputPCA(pca.data = pca_input_VD, pca.result = scaled_pca_VD,
          out.dir = res_dir_nut_ave_VD, out.prefix = res_prefix_nut_ave_VD)

# Combine the input (before processing) with all the variables and the PC results.
# In the case of averaged data / user, the input file used here is xxx_ave_c.txt, which
# has all the variables before filtering out by correlation or zero variance.
SaveInputAndPCs(input="VDMTdata/asa24_Tot_mean_m_QC_Nut_ave_c.txt",
                pca.results = scaled_pca_VD,
                out.dir= res_dir_nut_ave_VD,
                out.prefix= res_prefix_nut_ave_VD)

# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - Diet, in this case.

# Load the complete Nut average data. (before filtering variables)
Nut_ave_c_AJ <- read.table("AJdata/MCTs_23887_Tot_mean_m_QC_Nut_ave_c.txt", sep="\t", header=T)
Nut_ave_c_VD <- read.table("VDMTdata/asa24_Tot_mean_m_QC_Nut_ave_c.txt", sep="\t", header=T)

# Change Diet to a factor so that factor levels will be displayed in order.
Nut_ave_c_AJ$Supplement <- factor(Nut_ave_c_AJ$Supplement, levels = c("EVOO", "MCT"))
Nut_ave_c_VD$Intervention <- factor(Nut_ave_c_VD$Intervention, levels = c("Yes", "No"))

# Use the autoplot function. Specify which PC in the x and y arguments.
# The 'data' argument needs the original input for PCA, not after selecting specific variables.
Nut_ave_PC12_diet_AJ <-
  autoplot(scaled_pca_AJ, x = 1, y = 2,
           loadings = T, loadings.label = T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = Nut_ave_c_AJ, size = 3) +
  geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Supplement)) +
  theme_bw(base_size = 12) + theme(aspect.ratio = 1) +
  no_grid + space_axes +
  scale_fill_manual( values= distinct100colors)
Nut_ave_PC12_diet_AJ

ggsave("output/MCTs_23887_Nut_ave_PC12_diet.jpeg",
       Nut_ave_PC12_diet_AJ, device="jpeg", width=7, height=6.5)

Nut_ave_PC12_diet_VD <-
  autoplot(scaled_pca_VD, x = 1, y = 2,
           loadings = T, loadings.label = T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = Nut_ave_c_VD, size = 3) +
  geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Intervention)) +
  theme_bw(base_size = 12) + theme(aspect.ratio = 1) +
  no_grid + space_axes +
  scale_fill_manual( values= distinct100colors)
Nut_ave_PC12_diet_VD

ggsave("output/asa24_Nut_ave_PC12_diet.jpeg",
       Nut_ave_PC12_diet_VD, device="jpeg", width=7, height=6.5)

# ===============================================================================================================
# Food Category data as is, processed for clustering analyses.
# ===============================================================================================================

# Load Cat_asis data.
Tot_m_QCed_Cat_asis_AJ <- read.table(file="AJdata/MCTs_23887_Tot_m_QC_Cat_asis_c_rv.txt",
                                     sep="\t", header=T)
Tot_m_QCed_Cat_asis_VD <- read.table(file="VDMTdata/asa24_Tot_m_QC_Cat_asis_c_rv.txt",
                                     sep="\t", header=T)

# Name your input data.
pca_input_AJ <- Tot_m_QCed_Cat_asis_AJ
pca_input_VD <- Tot_m_QCed_Cat_asis_VD

# Ensure your input file has the correct number of rows and columns.
dim(pca_input_AJ)
dim(pca_input_VD)

# Scale the data and perform PCA.
scaled_pca_AJ <- prcomp(x = pca_input_AJ, scale = TRUE)
scaled_pca_VD <- prcomp(x = pca_input_VD, scale = TRUE)

# Specify the directory (folder) to save the results.
res_dir_cat_asis_AJ = "AJdata/PCA_Cat_asis"
res_dir_cat_asis_VD = "VDMTdata/PCA_Cat_asis"

# Specify the prefix of filenames to be saved.
res_prefix_cat_asis_AJ = "MCTs_23887_Cat_asis"
res_prefix_cat_asis_VD = "asa24_Cat_asis"

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
OutputPCA(pca.data = pca_input_AJ, pca.result = scaled_pca_AJ,
          out.dir = res_dir_cat_asis_AJ, out.prefix = res_prefix_cat_asis_AJ)
OutputPCA(pca.data = pca_input_VD, pca.result = scaled_pca_VD,
          out.dir = res_dir_cat_asis_VD, out.prefix = res_prefix_cat_asis_VD)

# Combine the input (Cat before processing) with all the variables and the PC results.
SaveInputAndPCs(input="AJdata/MCTs_23887_Tot_m_QC_Cat_asis_c.txt",
                pca.results = scaled_pca_AJ,
                out.dir = res_dir_cat_asis_AJ,
                out.prefix = res_prefix_cat_asis_AJ)
SaveInputAndPCs(input="VDMTdata/asa24_Tot_m_QC_Cat_asis_c.txt",
                pca.results = scaled_pca_VD,
                out.dir = res_dir_cat_asis_VD,
                out.prefix = res_prefix_cat_asis_VD)

# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - Diet, in this case.
# Load the complete Cat data. (before filtering variables)
Cat_asis_c_AJ <- read.table("AJdata/MCTs_23887_Tot_m_QC_Cat_asis_c.txt", sep="\t", header=T)
Cat_asis_c_VD <- read.table("VDMTdata/asa24_Tot_m_QC_Cat_asis_c.txt", sep="\t", header=T)

# Change Diet to a factor so that factor levels will be displayed in order.
Cat_asis_c_AJ$Supplement <- factor(Cat_asis_c_AJ$Supplement, levels = c("EVOO", "MCT"))
Cat_asis_c_VD$Intervention <- factor(Cat_asis_c_VD$Intervention, levels = c("Yes", "No"))

# Use the autoplot function. Specify which PC in the x and y arguments.
# The 'data' argument needs the original input for PCA, not after selecting specific variables.
Cat_asis_PC12_diet_AJ <-
  autoplot(scaled_pca_AJ, x = 1, y = 2,
           loadings = T, loadings.label = T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = Cat_asis_c_AJ,  size= 3) +
  geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Supplement)) +
  theme_bw(base_size = 12.5) + theme(aspect.ratio = 1) +
  no_grid + space_axes +
  scale_fill_manual(values = distinct100colors)
Cat_asis_PC12_diet_AJ

ggsave("output/MCTs_23887_Cat_asis_PC12_diet.jpeg",
       Cat_asis_PC12_diet_AJ, device="jpeg", width=7, height=6.5)

Cat_asis_PC12_diet_VD <-
  autoplot(scaled_pca_VD, x = 1, y = 2,
           loadings = T, loadings.label = T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = Cat_asis_c_VD,  size= 3) +
  geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Intervention)) +
  theme_bw(base_size = 12.5) + theme(aspect.ratio = 1) +
  no_grid + space_axes +
  scale_fill_manual(values = distinct100colors)
Cat_asis_PC12_diet_VD

ggsave("output/asa24_Cat_asis_PC12_diet.jpeg",
       Cat_asis_PC12_diet_VD, device="jpeg", width=7, height=6.5)

# ===============================================================================================================
# Food category data averaged and processed for clustering analyses.
# ===============================================================================================================

# Load Cat_ave data.
Tot_m_QCed_Cat_ave_AJ <- read.table(file="AJdata/MCTs_23887_Tot_mean_m_QC_Cat_ave_c_rv.txt", sep="\t", header=T)
Tot_m_QCed_Cat_ave_VD <- read.table(file="VDMTdata/asa24_Tot_mean_m_QC_Cat_ave_c_rv.txt", sep="\t", header=T)

# Name your input data.
pca_input_AJ <- Tot_m_QCed_Cat_ave_AJ
pca_input_VD <- Tot_m_QCed_Cat_ave_VD

# Ensure your input file has the correct number of rows and columns.
dim(pca_input_AJ)
dim(pca_input_VD)

# Scale the data and perform PCA.
scaled_pca_AJ <- prcomp(x = pca_input_AJ, scale = TRUE)
scaled_pca_VD <- prcomp(x = pca_input_VD, scale = TRUE)

# Specify the directory (folder) to save the results.
res_dir_cat_ave_AJ = "AJdata/PCA_Cat_ave"
res_dir_cat_ave_VD = "VDMTdata/PCA_Cat_ave"

# Specify the prefix of filenames to be saved.
res_prefix_cat_ave_AJ = "MCTs_23887_Cat_ave"
res_prefix_cat_ave_VD = "asa24_Cat_ave"

# Save PCA output files in a specified folder (out.dir) and a prefix (out.prefix).
# Input is your items/Nut input file before any prep for clustering, from which you derived the input for the PCA.
OutputPCA(pca.data = pca_input_AJ, pca.result = scaled_pca_AJ,
          out.dir = res_dir_cat_ave_AJ, out.prefix = res_prefix_cat_ave_AJ)
OutputPCA(pca.data = pca_input_VD, pca.result = scaled_pca_VD,
          out.dir = res_dir_cat_ave_VD, out.prefix = res_prefix_cat_ave_VD)

# Combine the input (Nut before processing) with all the variables and the PC results.
# In the case of averaged Nut data / user, the input file used here is xxx_ave_c.txt, which
# has all the variables before filtering out by correlation or zero variance.
SaveInputAndPCs(input="AJdata/MCTs_23887_Tot_mean_m_QC_Cat_ave_c.txt",
                pca.results= scaled_pca_AJ,
                out.dir= res_dir_cat_ave_AJ,
                out.prefix= res_prefix_cat_ave_AJ)
SaveInputAndPCs(input="VDMTdata/asa24_Tot_mean_m_QC_Cat_ave_c.txt",
                pca.results= scaled_pca_VD,
                out.dir= res_dir_cat_ave_VD,
                out.prefix= res_prefix_cat_ave_VD)

# ---------------------------------------------------------------------------------------------------------------
# Color-code datapoints on a biplot by a factor - Diet, in this case.

# Load the complete Cat average data. (before filtering variables)
Cat_ave_c_AJ <- read.table("AJdata/MCTs_23887_Tot_mean_m_QC_Cat_ave_c.txt", sep="\t", header=T)
Cat_ave_c_VD <- read.table("VDMTdata/asa24_Tot_mean_m_QC_Cat_ave_c.txt", sep="\t", header=T)

# Change Diet to a factor so that factor levels will be displayed in order.
Cat_ave_c_AJ$Supplement <- factor(Cat_ave_c_AJ$Supplement, levels = c("EVOO", "MCT"))
Cat_ave_c_VD$Intervention <- factor(Cat_ave_c_VD$Intervention, levels = c("Yes", "No"))

# Use the autoplot function. Specify which PC in the x and y arguments.
# The 'data' argument needs the original input for PCA, not after selecting specific variables.
Cat_ave_PC12_diet_AJ <-
  autoplot(scaled_pca_AJ, x = 1, y = 2,
           loadings = T, loadings.label = T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = Cat_ave_c_AJ,
           size= 3 ) +
  geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Supplement)) +
  theme_bw(base_size = 12) + theme(aspect.ratio = 1) +
  no_grid + space_axes +
  scale_fill_manual(values = distinct100colors)
Cat_ave_PC12_diet_AJ

ggsave("output/MCTs_23887_Cat_ave_PC12_diet.jpeg",
       Cat_ave_PC12_diet_AJ, device="jpeg", width=7, height=6.5)

Cat_ave_PC12_diet_VD <-
  autoplot(scaled_pca_VD, x = 1, y = 2,
           loadings = T, loadings.label = T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = Cat_ave_c_VD,
           size= 3 ) +
  geom_point(size = 3, alpha = 1, na.rm = T, shape = 21, aes(fill= Intervention)) +
  theme_bw(base_size = 12) + theme(aspect.ratio = 1) +
  no_grid + space_axes +
  scale_fill_manual(values = distinct100colors)
Cat_ave_PC12_diet_VD

ggsave("output/asa24_Cat_ave_PC12_diet.jpeg",
       Cat_ave_PC12_diet_VD, device="jpeg", width=7, height=6.5)

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory
setwd(main_wd)

# Ella ignored the following section:

# ===============================================================================================================
# Code to create and make adjustments to each plot/file, if desired.
# ===============================================================================================================

# You can specify different directory and prefix to avoid overwriting files
# produced by the OutputPCA function.

  res_dir =    "PCA_Nut_asis_2"
  res_prefix = "VVKAJ_Nut_asis_2"

# Create a scree plot.
  screep <- LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)
  screep
  ggsave( paste(res_dir, paste(res_prefix, "_scree.pdf"), sep= .Platform$file.sep),
          screep, device="pdf", width=5, height=5, units="in")

# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  biplotdots <- BiplotDots(pca.result = scaled_pca, pca.data = pca_input, alpha = 0.5)
  biplotdots
  ggsave( paste(res_dir, paste(res_prefix, "_biplotdots.pdf"), sep= .Platform$file.sep),
          biplotdots, device="pdf", width=5, height=5, units="in")

# A biplot with the individuals labeled.
  biplotlabeled <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=T)
  biplotlabeled
  ggsave( paste(res_dir, paste(res_prefix, "_biplotlabeled.pdf"), sep= .Platform$file.sep),
          biplotlabeled, device="pdf", width=5, height=5, units="in")

# A biplot with the individuals labeled without the variables' arrows.
  biplotlabeledwoarrows <- BiplotLabeledwoArrows(pca.result=scaled_pca, pca.data=pca_input,
                                                 individuals.label=T)
  biplotlabeledwoarrows
  # Zoom in to a particular area of interest in the plot
  biplotlabeledwoarrows + coord_cartesian(xlim=c(-0.1, 0.1), ylim=c(0.05, 0.1))

  ggsave( paste(res_dir, paste(res_prefix, "_biplotlabeledwoarrows.pdf"), sep= .Platform$file.sep),
          biplotlabeledwoarrows, device="pdf", width=5, height=5, units="in")

# Plot the directions of the variables.
  directions <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=F)
  directions
  ggsave( paste(res_dir, paste(res_prefix, "_directions.pdf"), sep= .Platform$file.sep),
          directions, device="pdf", width=5, height=5, units="in")

# Plot the contribution of the variables to a given PC: Change the PC and the file name as desired.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1",
               positive.color="green2", negative.color="grey70", sort.variables = T)
  loadings_plot
  ggsave( paste(res_dir, paste(res_prefix, "_loadings_PC1.pdf"), sep= .Platform$file.sep),
          loadings_plot, device="pdf", width=8, height=4.8, units="in")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
  setwd(main_wd)
