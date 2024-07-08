# ===============================================================================================================
# Visualize ordination results - color-code individuals and highlight specific individuals if desired. 
# Version 1 
# Created on 01/19/2023 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

# Name your main directory for future use.
main_wd <- file.path(getwd())

# load the necessary packages.
library(ggplot2)

# Load necessary functions and ggplot formatting themes
source("GitHub_tutorial/DietR/lib/specify_data_dir.R")
source("GitHub_tutorial/DietR/lib/ggplot2themes.R")
  
# Load the distinct 100 colors for use.   
distinct100colors <- readRDS("GitHub_tutorial/DietR/lib/distinct100colors.rda")

# ===============================================================================================================
# Load ordination results - whether weighted or unweighted Unifrac distance results. 
# ===============================================================================================================

# In the ordination section, biplots were generated with the users color-coded by their Diet. 
# However, they can also be color-coded individually. In addition, specific users can be highlighted 
# with a thicker outline if desired. All of these can be done by loading the saved ordination results - 
# Axis values and metadata combined, and the proportion of variance explained.

# Change to the folder called "Ordination" in your "VVKAJ" folder.
SpecifyDataDirectory(directory.name = "AJdata/Ordination")
  
# Read in the metadata and users' Axis values. 
meta_usersdf_AJ <- read.table("MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt", header=T)
  
# Read in the eigenvalues for axis labels of biplots.
eigen_loaded_AJ <- read.table("MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_eigen_percent.txt", header=T)
  
# Make a vector that contains the variance explained.
eigen_loaded_vec_AJ <- eigen_loaded_AJ[, 2]

# ===============================================================================================================
# Plot Axis 1 and Axis 2 to show the separation of individuals colored by UserName.
# ===============================================================================================================

# Create a folder called "Viz_Ordination" to save the plots to be produced here.
  
# Color-code by UserName.
by_user_AJ <- ggplot(meta_usersdf_AJ, aes(x=Axis.1, y=Axis.2, fill=UserName)) +
  geom_point(shape=21, aes(color= UserName), size=3, color="black") + 
  scale_fill_manual(values = distinct100colors) + # OR use viridis theme.
  # scale_color_viridis_d() +
  xlab(paste("Axis.1 (", paste(round(eigen_loaded_vec_AJ[1]*100, 1)), "%)", sep="") ) +
  ylab(paste("Axis.2 (", paste(round(eigen_loaded_vec_AJ[2]*100, 1)), "%)", sep="") ) +
  no_grid + space_axes + theme(aspect.ratio = 1)
by_user_AJ
  
# Save by_user plot as a pdf. 
ggsave("Viz_Ordination/MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_users_Axis12.jpeg", 
       by_user_AJ, device="jpeg", height=5, width=7, unit="in", dpi=300)
  
# Add lines to connect samples in the order in which they appear in the data using geom_path. 
# [NOTE] A similar-sounding function, geom_line, connects in the order of the variable (small to large) 
# on the x axis, so it could be misleading. We want to use geom_path here.
by_user_pathconnected_AJ <- by_user_AJ + geom_path(aes(color = UserName)) +
  scale_color_manual(values=distinct100colors)
  
by_user_pathconnected_AJ
  
# Save by_user_pathconnected as a pdf.
ggsave("Viz_Ordination/MCTs_23887_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_users_Axis12_pathconnected.jpeg", 
       by_user_pathconnected_AJ, device="jpeg", height=5, width=7, unit="in", dpi=300)
  
# ---------------------------------------------------------------------------------------------------------------

# - # - # - # - # - # - # - # - # VDMT WORK # - # - # - # - # - # - # - # - #

# ===============================================================================================================
# Load ordination results - whether weighted or unweighted Unifrac distance results. 
# ===============================================================================================================

# In the ordination section, biplots were generated with the users color-coded by their Diet. 
# However, they can also be color-coded individually. In addition, specific users can be highlighted 
# with a thicker outline if desired. All of these can be done by loading the saved ordination results - 
# Axis values and metadata combined, and the proportion of variance explained.

# Change to the folder called "Ordination" in your "VVKAJ" folder.
SpecifyDataDirectory(directory.name = "VDMTdata/Ordination")

# Read in the metadata and users' Axis values. 
meta_usersdf_VD <- read.table("asa24_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_meta_users.txt", header=T)

# Read in the eigenvalues for axis labels of biplots.
eigen_loaded_VD <- read.table("asa24_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_eigen_percent.txt", header=T)

# Make a vector that contains the variance explained.
eigen_loaded_vec_VD <- eigen_loaded_VD[, 2]

# ===============================================================================================================
# Plot Axis 1 and Axis 2 to show the separation of individuals colored by UserName.
# ===============================================================================================================

# Create a folder called "Viz_Ordination" to save the plots to be produced here.

# Color-code by UserName.
by_user_VD <- ggplot(meta_usersdf_VD, aes(x=Axis.1, y=Axis.2, fill=UserName)) +
  geom_point(shape=21, aes(color= UserName), size=3, color="black") + 
  scale_fill_manual(values = distinct100colors) + # OR use viridis theme.
  # scale_color_viridis_d() +
  xlab(paste("Axis.1 (", paste(round(eigen_loaded_vec_VD[1]*100, 1)), "%)", sep="") ) +
  ylab(paste("Axis.2 (", paste(round(eigen_loaded_vec_VD[2]*100, 1)), "%)", sep="") ) +
  no_grid + space_axes + theme(aspect.ratio = 1)
by_user_VD

# Save by_user plot as a jepg. 
ggsave("Viz_Ordination/asa24_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_users_Axis12.jpeg", 
       by_user_VD, device="jpeg", height=5, width=7, unit="in", dpi=300)

# Add lines to connect samples in the order in which they appear in the data using geom_path. 
# [NOTE] A similar-sounding function, geom_line, connects in the order of the variable (small to large) 
# on the x axis, so it could be misleading. We want to use geom_path here.
by_user_pathconnected_VD <- by_user_VD + geom_path(aes(color = UserName)) +
  scale_color_manual(values=distinct100colors)

by_user_pathconnected_VD

# Save by_user_pathconnected as a pdf.
ggsave("Viz_Ordination/asa24_Items_f_id_s_m_QCed_4Lv_ord_WEIGHTED_users_Axis12_pathconnected.jpeg", 
       by_user_pathconnected_VD, device="jpeg", height=5, width=7, unit="in", dpi=300)
