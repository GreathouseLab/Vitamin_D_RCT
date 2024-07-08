# ===============================================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Use foodtrees gengerated Without using FilterDBByDiets.
# After running formatfoods first and keeping ".0" in FoodIDs.
# Version 2
# Created on 02/16/2022 by Rie Sadohara
# 06/26/2023 replaced "OTU" with "IFC".
# ===============================================================================================================

# In this section, we will take the phylogeny of food items into account in clustering individuals according to
# their dietary data. In order to do so, we will use the phyloseq package.(https://joey711.github.io/phyloseq/index.html),
# which uses phylogeny of microbes and their abundance. We will replace microbes with food items consumed by
# our dietary study participants.

# Set your working directory to the main directory.
#Session --> Set working directory --> Choose directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# If you have not downloaded and installed the phyloseq package yet:
# You can do so by first installing BiocManager (if you have not done so):
#if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")

# Then download and install the phyloseq package.
  # BiocManager::install("phyloseq")

# Install the devtools package necessary for installing the pairwiseAdonis package.
if (!require("devtools",    quietly = TRUE))install.packages("devtools")

# Install pairwise adonis function from Github. (https://github.com/pmartinezarbizu/pairwiseAdonis)
  # devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

# ---------------------------------------------------------------------------------------------------------------
# load the necessary packages.
library(phyloseq)
library(ggtree)
library(pairwiseAdonis)

# Load necessary functions and ggplot formatting themes
source("GitHub_tutorial/DietR/lib/specify_data_dir.R")
source("GitHub_tutorial/DietR/lib/ordination.R")
source("GitHub_tutorial/DietR/lib/ggplot2themes.R")
source("GitHub_tutorial/DietR/lib/sort_IFC_by_ID.R")
source("GitHub_tutorial/DietR/lib/plot.axis.1to4.by.factor.R")

# Load the distinct 100 colors for use.
distinct100colors <- readRDS("GitHub_tutorial/DietR/lib/distinct100colors.rda")

# ===============================================================================================================
# Create a phyloseq object for ordination.
# ===============================================================================================================

# Load the necessary files for creating a phyloseq object.

# Food
# Load IFC table, and sort the columnnames (userID), leaving the last column (taxonomy) intact.
# This dataframe will be saved as "food".
# Also, save "food" as a .txt file to be used in the "correlation between Axes and foods" section.
SortIFCByID(ifc.input =           "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.food.ifc.txt",
            outfn.for.corr.axis = "AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.food.ifc_sorted.txt")

# "food" is a matrix of Food descriptions (rows) x SampleID (columns).
head(food)[1:6, 1:4]

# Format the food object and create an ifc_table called IFC.
PrepFood(data = food)

# Taxonomy (tax)
tax <- read.delim("AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.tax.txt")

# remove 1280, which doesn't have a Main.food.description (not NA, just blank)
# which invites further NAs and errors
#tax_AJ <- tax_AJ[-1280,]
tax <- tax[-1280,]

PrepTax(data= tax)

#PrepTax # THIS DOESN'T EVEN WORK WITH HER SAMPLE DATA

######################################### USING THIS WHOLE PrepTax FUNCTION
# PrepTax function # i ran with tax1 after fixing the taxonomy column, and some seeming typos

# Make the food description as row names
### row.names(tax_AJ) <- tax_AJ$Main.food.description

# remove the FoodID column
### woFoodID_AJ <- tax_AJ[, !colnames(tax_AJ) == "FoodID"]

# Split taxonomy L1, L2, L3 etc. by a semicolon, in lieu of tidyr::separate
### splittax_AJ <- strsplit(as.character(woFoodID_AJ$taxonomy), split=";")
# How many levels were created after splitting?
### max_n_levels_AJ <- max(lengths(splittax_AJ))

### tax1_AJ <- woFoodID_AJ
# delete last row with has NA and is generally incomplete
#tax1 <- tax1[-1280, ] # the loop kept stopping here bc there's no food description
#splittax <- splittax[-1280]
#tax <- tax[-1280,]

### n <- 1

### if(max_n_levels_AJ==5){
###   for(i in strsplit(as.character(tax_AJ$taxonomy), split=';')){
###     #print(n)
###     tax1_AJ[n, 'L1'] <- i[[1]]
###     tax1_AJ[n, 'L2'] <- i[[2]]
###     tax1_AJ[n, 'L3'] <- i[[3]]
###     tax1_AJ[n, 'L4'] <- i[[4]]
###     tax1_AJ[n, 'L5'] <- i[[5]]
###     n <- n + 1
###   }
### }
# that there are max_n_levels = 7, and if you see the error that levels should be 1-6,
# this seems to prove that the last "level" is really just the food description

# remove Main.food.description column, because it's already made into row names.
### woFoodID2_AJ <- tax1_AJ[, !colnames(tax1_AJ) == "Main.food.description"]

# Transform to matrix, then to a tax_table object.
### tax_mat_AJ <- as.matrix(woFoodID2_AJ)
### TAX <<- phyloseq::tax_table(tax_mat_AJ) # TAX is created

######################################### END OF PrepTax FUNCTION

# Sample

### i found this on github: https://github.com/knights-lab/dietstudy_analyses/blob/master/data/maps/UserName_map.txt
meta_AJ <- read.table('AJdata/UserName_map.txt', sep = "\t", header = T)
meta_AJ_s <- subset(meta_AJ, select = c(UserName, Gender, Age, Weight, Height, BMI,
                                        Waist.Circumference, Supplement, Dietary.Supp))

# Format the metadata file and save it as 'SAMPLES'.
# PrepMeta(data = meta_AJ_s) ...

# MCTs_23887_Items_QC.txt is the only thing that has the AJ0001 names
og_items <- read.delim('AJdata/MCTs_23887_Items_QC.txt', sep = "\t", header = T)
users_names <- subset(og_items, select = c(UserName, SampleID))
users_names_u <- unique(users_names)

# add AJ00001 notations to metadata
meta_AJ_s_m <- merge(meta_AJ_s, users_names_u, by = 'UserName', all.x = TRUE)

# some meta_AJ_s_m$SampleID s are NA bc the particiants were removed: remove these
meta_AJ_s_m_n <- meta_AJ_s_m[!is.na(meta_AJ_s_m$SampleID),]

######################################### USING THIS WHOLE PrepMeta FUNCTION
# make UserName as rownames of meta.
rownames(meta_AJ_s_m_n) <- meta_AJ_s_m_n[, "SampleID"]

# subset metadata to the correct samples.
# colnames(food) has users.
meta2 <- meta_AJ_s_m_n[colnames(food), ]

# Transform meta2 to sample_data object.
SAMPLES <<- phyloseq::sample_data(meta2)
######################################### END OF PrepMeta FUNCTION

# Foodtree

foodtree_AJ <- read_tree("AJdata/Foodtree/MCTs_23887_Items_f_id_s_m_QCed_4Lv.tree.nwk")
# It is OK to see a message saying that
# "Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
# Also defined by 'tidytree'".
# SAW THIS

# Format foodtree and save it as 'TREE'.
PrepTree(data = foodtree_AJ)
# Again, it is OK to see the same message as the previous line.

# ---------------------------------------------------------------------------------------------------------------
# Make a phyloseq object with IFC, TAX, SAMPLES, and TREE.
phyfoods_AJ <- phyloseq(IFC, TAX, SAMPLES, TREE)
  # It is OK to see a message (or multiple of them) saying that
    # Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'.

# Check your metadata
# Show the sample names and ensure they are vvkaj.00xxx.
sample_names(phyfoods_AJ)

# Show metadata.
head(sample_data(phyfoods_AJ), n=3)

# Check the level 1 foods in your food tree.
L1s_AJ = tax_table(phyfoods_AJ)[, "L1"]
as.vector(unique(L1s_AJ))

# ===============================================================================================================
# Use your phyloseq object and perform ordination
# ===============================================================================================================

# Change to the folder called "Ordination" in your "VVKAJ" folder.
SpecifyDataDirectory(directory.name = "AJdata/Ordination")

# Perform Principal Coordinate Analysis (PCoA) with weighted unifrac distance of your food data.
# Ordination by UNweighted unifrac distances can be done by having the "weighted" argument as FALSE.
# This may take a few minutes depending on your data size.
# e.g. a large phyloseq object (7.9 MB) takes ~ 1 min.
ordinated_AJ <- phyloseq::ordinate(phyfoods_AJ, method="PCoA", distance="unifrac", weighted=TRUE)

# Save the percent variance explained as a txt file.
Eigen(eigen.input = ordinated_AJ$values$Relative_eig,
      output.fn="MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_eigen_percent.txt")

# Merge the first n axes to the metadata and save it as a txt file.
# This will be used for plotting ordination results.
MergeAxesAndMetadata(ord.object=ordinated_AJ, number.of.axes=10, meta.data= meta_AJ_s_m_n,
                     output.fn= "MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt")

# ===============================================================================================================
# Plot your ordination results
# ===============================================================================================================

# Read in the eigenvalues for axis labels of biplots.
eigen_loaded_AJ <- read.table("MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_eigen_percent.txt", header=T)

# Make a vector that contains the variance explained.
eigen_loaded_vec_AJ <- eigen_loaded_AJ[, 2]

# Read in the metadata and users' Axis values.
meta_usersdf_AJ <- read.table("MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt", header=T)

# Change Diet to a factor so that factor levels will be displayed in order.
meta_usersdf_AJ$Supplement <- factor(meta_usersdf_AJ$Supplement,
                            levels= c('EVOO', 'MCT'))

# Take a look at meta_usersdf that has been loaded.
head(meta_usersdf_AJ, 3)

# ---------------------------------------------------------------------------------------------------------------
# Save Axes 1 & 2, 1 & 3, 2 & 3, 3 & 4, 2 & 4 biplots with and without ellipses with specified confidence interval.
# The results are saved with filenames with the specified "prefix_AxisXY.pdf" or "prefix_AxisXY_ellipses.pdf".
# You need to supply the same number of colors in the order of the factor level to be used.
# dot.colors are for datapoints, and ellipses.colors are for ellipses outlines.
PlotAxis1to4ByFactor(axis.meta.df    = meta_usersdf_AJ,
                     factor.to.color = "Supplement",
                     eigen.vector    = eigen_loaded_vec_AJ,
                     dot.colors      = distinct100colors,
                     ellipses.colors = distinct100colors,
                     ellipses.cflevel = 0.95,
                     out.prefix = "MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_diet"
)

# ---------------------------------------------------------------------------------------------------------------
# Permanova tests

# Some of the Diet groups seem to form distinct clusters. Use beta-diversity and adonis (permanova) tests
# to see if they are actually distinct from one another.

# Generate a weighted unifrac distance matrix.
dist_matrix_AJ <- phyloseq::distance(phyfoods_AJ, method = "wunifrac")

# Perform dispersion test.
# vegan::betadisper computes centeroids and distance of each datapoint from it.
dispr_AJ <- vegan::betadisper(d=dist_matrix_AJ, phyloseq::sample_data(phyfoods_AJ)$Supplement)

# Show the centroids and dispersion of each group.
plot(dispr_AJ)

# Use dispr to do a permutation test for homogeneity of multivariate dispersion.
# The set.seed function ensures the same permutation results will be obtained every time;
# otherwise, the p-values will slightly differ each run, as it is a permutation test.

set.seed(123)
vegan::permutest(dispr_AJ, perm=5000)
# If p>0.05, the dispersion of each group are not different, and the assumption for adonis is met.
# The results here indicate that the dispersion of each group may be different, so we should consider
# this information in discussion. Nevertheless, we will proceed for demonstration purposes.

# Use adonis to test whether there is a difference between groups' composition.
# i.e., composition among groups (food they consumed) is similar or not.
set.seed(123)
vegan::adonis2(dist_matrix_AJ ~ phyloseq::sample_data(phyfoods_AJ)$Supplement, permutations = 5000)

# If overall adonis is significant, which is true in this case,
# you can run pairwise adonis to see which group pairs are different.
pairwise.adonis(dist_matrix_AJ, phyloseq::sample_data(phyfoods_AJ)$Supplement,
                perm = 5000, p.adjust.m = "none")


# ===============================================================================================================
# Save weighted unifrac distance matrix
# ===============================================================================================================
# Generate and save a weighted unifrac distance matrix of "Samples".
phyfoods <- phyfoods_AJ # phyfoods needed to be defined as phyfoods
WeightedUnifracDis(input.phyloseq.obj = phyfoods_AJ,
                   output.fn = "MCTs_23887_Items_f_id_s_m_QCed_4Lv_WEIGHTED_uni_dis.txt")


# ===============================================================================================================
# Use unweighted unifrac distance.
# ===============================================================================================================
# You can perform Principal Coordinate Analysis (PCoA) with UNweighted unifrac distance of your food data.
ordinated_u_AJ = phyloseq::ordinate(phyfoods_AJ, method="PCoA", distance="unifrac", weighted=FALSE)

# Use the same code above for creating plots, but now with ordinated_u for the ord.object argument, and
# change WEIGHTED to UNweighted, or an appropriate name for the method you selected.

# Save the percent variance explained as a txt file.
Eigen(eigen.input = ordinated_u_AJ$values$Relative_eig,
      output.fn="MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_eigen_percent.txt")

# Merge the first n axes to the metadata and save it as a txt file.
# This will be used for plotting ordination results.
MergeAxesAndMetadata(ord.object=ordinated_u_AJ, number.of.axes=10, meta.data= meta_AJ_s_m_n,
                       output.fn= "MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_meta_users.txt")

# ===============================================================================================================
# Plot your ordination results
# ===============================================================================================================

# Read in the eigenvalues for axis labels of biplots.
eigen_loaded_AJ <- read.table("MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_eigen_percent.txt", header=T)

# Make a vector that contains the variance explained.
eigen_loaded_vec_AJ <- eigen_loaded_AJ[, 2]

# Read in the metadata and users' Axis values.
meta_usersdf_AJ <- read.table("MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_meta_users.txt", header=T)

# Change Diet to a factor so that factor levels will be displayed in order.
meta_usersdf_AJ$Supplement <- factor(meta_usersdf_AJ$Supplement,
                              levels= c('EVOO', 'MCT'))

# Take a look at meta_usersdf that has been loaded.
head(meta_usersdf_AJ, 3)

# ---------------------------------------------------------------------------------------------------------------
# Save Axes 1 & 2, 1 & 3, 2 & 3, 3 & 4, 2 & 4 biplots with and without ellipses with specified confidence interval.
# The results are saved with filenames with the specified "prefix_AxisXY.pdf" or "prefix_AxisXY_ellipses.pdf".
# You need to supply the same number of colors in the order of the factor level to be used.
# dot.colors are for datapoints, and ellipses.colors are for ellipses outlines.
PlotAxis1to4ByFactor(axis.meta.df    = meta_usersdf_AJ,
                     factor.to.color = "Supplement",
                     eigen.vector    = eigen_loaded_vec_AJ,
                     dot.colors      = distinct100colors,
                     ellipses.colors = distinct100colors,
                     ellipses.cflevel = 0.95,
                     out.prefix = "MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_UNweighted_diet"
)

# ---------------------------------------------------------------------------------------------------------------
# Permanova tests

# Some of the Diet groups seem to form distinct clusters. Use beta-diversity and adonis tests
# to see if they are actually distinct from one another.

# Generate a weighted unifrac distance matrix.
dist_matrix_AJ <- phyloseq::distance(phyfoods_AJ, method = "unifrac")  # UNweighted

# Dispersion test and plot
# vegan::betadisper computes centeroids and distance of each datapoint from it.
dispr_AJ <- vegan::betadisper(d=dist_matrix_AJ, phyloseq::sample_data(phyfoods_AJ)$Supplement)

# Show the centroids and dispersion of each group.
plot(dispr_AJ)

# Use dispr to do a permutation test for homogeneity of multivariate dispersion.
set.seed(123)
vegan::permutest(dispr_AJ, perm=5000)

# If p>0.05, the dispersion of each group are not different, and the assumption for adonis is met.
# The results here indicate that the dispersion of each group may be different, so we should consider
# this information in discussion. Nevertheless, we will proceed for demonstration purposes.

# Use adonis to test whether there is a difference between groups' composition.
# i.e., composition among groups (food they consumed) is similar or not.
set.seed(123)
vegan::adonis2(dist_matrix_AJ ~ phyloseq::sample_data(phyfoods_AJ)$Supplement,
              permutations = 5000)

# If overall adonis is significant, which is true in this case,
# you can run pairwise adonis to see which group pairs are different.
pairwise.adonis(dist_matrix_AJ, phyloseq::sample_data(phyfoods_AJ)$Supplement,
                perm = 5000, p.adjust.m = "none")

# ===============================================================================================================
# Save unweighted unifrac distance matrix
# ===============================================================================================================

# Generate and save an unweighted unifrac distance matrix of "Samples".
UnweightedUnifracDis(input.phyloseq.obj = phyfoods_AJ,
                     output.fn = "MCTs_23887_Items_f_id_s_m_QCed_4Lv_UNweighted_uni_dis.txt")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory.
setwd(main_wd)

