# Ella von Dohlen 
# Spring 2023
# Objective: to clean, visualize, and prepare a Taxonomy of Food
# to calculate diet unifrac 
# to create a kcal matrix of food (weighing foods by the prop of kcal intake per day)

library(tidyr)

# setwd("~/Desktop/Greathouse/VitaminDstudy")

################### ABBY JOHNSON'S TAXONOMIC LEVELS ######################

# not sure where it originated, but this file came from knights-lab/dietstudy_analyses/data/diet
taxonomy_raw <- read.csv(file = "diet.taxonomy.txt", header = T)
colnames(taxonomy_raw) # "FoodID.taxonomy.Main.food.description"

# to view some examples of what the file looks like:
taxonomy_raw[125:130,]

# to split into three columns:
taxonomy <- separate(taxonomy_raw,
                     col = FoodID.taxonomy.Main.food.description,
                     into = c('FoodID', 'taxonomy', 'description'),
                     sep = '\t')

# to view an example of what the entries now look like:
taxonomy[1,]

nrow(taxonomy) # 8767

#taxonomy1 <- extract(taxonomy,
#                     col = FoodID,
#                     into = c("FoodCode", "ModCode"),
#                     regex = "([[:alnum:]]+).([[:alnum:]]+)")
#typeof(taxonomy1$FoodCode) # character --> integer to match asa24
#taxonomy1$FoodCode <- as.integer(taxonomy1$FoodCode)
#taxonomy1["ModCode"][taxonomy1["ModCode"] == 0] <- NA
#colnames(taxonomy1)
#taxonomy1[1,]

#length(unique(taxonomy1$FoodCode)) #7622
#sum(duplicated(taxonomy1$FoodCode)) #1145
#length(which(is.na(taxonomy1$ModCode))) #7622
#length(which(!is.na(taxonomy1$ModCode))) # 1145 mod codes

###################### LOOKING INTO OUR PROJECT'S DATA #####################

asa24items <- read.csv(file = "asa24ITEMS.csv", header = T)
colnames(asa24items)

# selecting for significant variables
imp_variables <- c("UserName", "FoodCode", "ModCode", "Food_Description", "FoodAmt")
asa24items1 <- asa24items[imp_variables]

nrow(asa24items1) # 10084

# to view an example entry
asa24items1[1,]

# change format: combine foodcode with modcode, in order to match Abby's data (which is stored in taxonomy)
asa24items1$FoodID <- paste(asa24items1$FoodCode, asa24items1$ModCode, sep='.')
colnames(asa24items1)
asa24items2 <- asa24items1[c("UserName", "FoodAmt", "FoodID", "Food_Description")]
colnames(asa24items2)

# to view an example entry
asa24items2[1,]

################ COMBINE OUR ASA24 DATA WITH ABBY'S TAXONOMY ##############

# merge our asa24 data with abby's taxonomy
data <- merge(x = asa24items2, y = taxonomy, by = "FoodID", all.x = TRUE)

# characteristics of new dataframe
nrow(data) # 10084
data[1:6,]
colnames(data)

# break up abby's five taxonomic levels (warning can be ignored)
data1 <- separate(data,
                  col = taxonomy,
                  into = c('Level1', 'Level2', 'Level3', 'Level4', 'Level5'),
                  sep = ";")
colnames(data1)

data1[1:6,]

# (only needed to be executed once)
# write.csv(data,"~/Desktop/Greathouse/VitaminDstudy/NetworkEdges.csv", row.names = TRUE)

########################### MISSING TAXONOMY LABELS ###########################

# count NAs in (combined) data
colSums(is.na(data))
# there are 1647 taxonomy lines and 1647 description lines missing

# place lines with missing taxonomy in a new df
missing_taxonomy <- data[is.na(data$taxonomy), ]
missing_taxonomy[1:6,]

length(unique(missing_taxonomy$FoodID))
# there are 326 food codes that are missing

length(unique(missing_taxonomy$UserName))
# all 43 participants have at least one missing

length(unique(missing_taxonomy$Food_Description))
# there are 325 unique food descriptions that are missing

# organize missing taxonomy df by replacing the NA taxonomy column with 5 NA level columns
missing_taxonomy_data <- separate(missing_taxonomy,
                                  col = taxonomy,
                                  into = c('Level1', 'Level2', 'Level3', 'Level4', 'Level5'),
                                  sep = ";")

# characteristics of df
colnames(missing_taxonomy_data)
missing_taxonomy_data[1:6,]
nrow(missing_taxonomy_data) #1647

# (only needed to be executed once)
# write.csv(missing_taxonomy_data,"~/Desktop/Greathouse/VitaminDstudy\\Missing_Taxonomy.csv")

# *these levels are assigned in the file TaxonomyLevels.R*

# Ankan said: look into doing network in R, with ppl and foods as nodes, and maybe edges pertaining to food amount

# brainstorming:
# random but i could look into correlation between dietary vitamins and dietary fat bc vitamins stored in fat
# look into diet compliance (HEI scores) vs no bowel movements?

################### CIRCULAR DENDROGRAM JUST BASED ON FOODID ###################

# install.packages("dendextend")
# install.packages("circlize")
library(dendextend)
library(circlize)

# Distance matrix
matrix <- data.frame(x = data$FoodID, y = data$FoodID)
distance_matrix <- dist(matrix)

# Hierarchical clustering dendrogram
dendrogram <- as.dendrogram(hclust(distance_matrix))

# Circular dendrogram without labels
circlize_dendrogram(dendrogram,
                    dend_track_height = 0.8,
                    labels = FALSE)

# hmm this was weird

############## APPLYING MAUNALLY ASSIGNED TAX LEVELS TO ASA DATA ###############

library(dplyr)

# this entered_taxonomy is the list of 326 manual assignments (no longer relevant):
#entered_taxonomy <- read.csv(file = "Entered_Taxonomy.csv", header = T)
#colnames(entered_taxonomy)
#levels_entered_tax <- entered_taxonomy[ , c("FoodID", "Level1", "Level2", "Level3",
#                                            "Level4", "Level5")]

# filled in taxonomic levels
all_assigned <- read.csv(file = "all_assigned.csv", header = T)
all_assigned[1:6,]

# to deposit my missing data into the existing data1 df

# !!! This makes FoodID in data1 to be transferred from char to double so that they can be merged
data1$FoodID <- as.double(data1$FoodID) # this only needs to be used once

# !!! this adds an original_index column to data1 so that it may be combined with all_assigned
data1$original_index <- 1:nrow(data1) # this only needs to be used once

# data1 is abby's taxonomy, and all_assigned are all of the levels I manually assigned
merged_df <- full_join(data1, all_assigned, by = "original_index")
colnames(merged_df)

#rename columns for clarity
names(merged_df)[names(merged_df) == "Level1.x"] <- "Level1"
names(merged_df)[names(merged_df) == "Level2.x"] <- "Level2"
names(merged_df)[names(merged_df) == "Level3.x"] <- "Level3"
names(merged_df)[names(merged_df) == "Level4.x"] <- "Level4"
names(merged_df)[names(merged_df) == "Level5.x"] <- "Level5"
names(merged_df)[names(merged_df) == "Level1.y"] <- "Level1_old"
names(merged_df)[names(merged_df) == "Level2.y"] <- "Level2_old"
names(merged_df)[names(merged_df) == "Level3.y"] <- "Level3_old"
names(merged_df)[names(merged_df) == "Level4.y"] <- "Level4_old"
names(merged_df)[names(merged_df) == "Level5.y"] <- "Level5_old"

#replace NAs with the new levels
condensed_df <- merged_df

# delete salt(1091), taco seasoning (1093), ((and garlic (7009)-nevermind))
condensed_df <- condensed_df[-c(1091, 1093),]

# delete demo users
condensed_df <- condensed_df[condensed_df$UserName.x != "VDMT_DemoUser", ]

colnames(condensed_df)

#place all new assignments in the original NA places
condensed_df$Level1[is.na(condensed_df$Level1)] <- condensed_df$Level1_old[is.na(condensed_df$Level1)]
condensed_df$Level2[is.na(condensed_df$Level2)] <- condensed_df$Level2_old[is.na(condensed_df$Level2)]
condensed_df$Level3[is.na(condensed_df$Level3)] <- condensed_df$Level3_old[is.na(condensed_df$Level3)]
condensed_df$Level4[is.na(condensed_df$Level4)] <- condensed_df$Level4_old[is.na(condensed_df$Level4)]
condensed_df$Level5[is.na(condensed_df$Level5)] <- condensed_df$Level5_old[is.na(condensed_df$Level5)]

# keep useful columns, minimizing repetitions
combined_tax_levels <- paste(condensed_df$Level1, condensed_df$Level2, condensed_df$Level3,
                             condensed_df$Level4, condensed_df$Level5)

######################### TO BE USED IN CYTOSCAPE #########################

nodes <- data.frame(combined_tax_levels, condensed_df$UserName.x, condensed_df$Level1)
nrow(nodes)

write.csv(nodes,"~/Desktop/Greathouse/VitaminDstudy/nodes.csv")

# make the nodes have an index column, and not a level1 column
# make a df with these matching indexes and the level1 column
# this second df can be imported as a table for the sake of color coding

############################ TREE PREPARATION #############################

# to be used for unifrac

tree <- condensed_df[c('Level1', 'Level2', 'Level3', 'Level4', 'Level5', 'FoodID.x')]
#as.vector(tree)
#tree_matrix <- matrix(tree)
# tree along with abundance matrix that Ankan helped me complete (he added 0s) --> unifrac

tree[1:6, ]

# my own testing to get tree from df:
#install.packages("treemap")
#library(treemap) # im not sure this was necessary at all

# Downloaded the Phyloseq package
# this took awhile
install.packages("BiocManager")
BiocManager::install("phyloseq")
#biocLite("phyloseq")

# first method for making a tree object from my dataframe
library(data.tree)

tree$pathString <- paste("FoodTree", tree$Level1, tree$Level2, tree$Level3,
                         tree$Level4, tree$Level5, tree$FoodID.x, sep = "/")

colnames(tree)
head(tree)

sum(duplicated(tree))
nrow(tree)

tree_imp <- unique(tree)
class(tree_imp)

# what about this
#new_tree <- as.node()

# this more or less worked, but it's now dataframefortree.numbers
#write.csv(tree_imp, "~/Desktop/Greathouse/VitaminDstudy\\dataframefortree.csv")

library(data.tree)
FOOD_TREE <- FromDataFrameTable(
  tree_imp,
  pathName = "pathString",
  pathDelimiter = "/",
  colLevels = NULL,
  na.rm = FALSE,
  check = c("check", "no-warn", "no-check")
)

class(FOOD_TREE)
class(FOOD_TREE$root)

FOOD_TREE$root

# another method- didn't work
#library("tree")
#tree_obj <- tree(tree)
#class(tree_obj)

# another method - not used
library("ggtree")
tree_chart <- toTree(FOOD_TREE$root, column_order = NULL)

# another method

# code copied from Stack Overflow https://stackoverflow.com/questions/15343338/how-to-convert-a-data-frame-to-tree-structure-object-such-as-dendrogram

# recursion function (copied from Martin Turjak)
traverse <- function(a,i,innerl){
  if(i < (ncol(df))){
    alevelinner <- as.character(unique(df[which(as.character(df[,i])==a),i+1]))
    desc <- NULL
    if(length(alevelinner) == 1) (newickout <- traverse(alevelinner,i+1,innerl))
    else {
      for(b in alevelinner) desc <- c(desc,traverse(b,i+1,innerl))
      il <- NULL; if(innerl==TRUE) il <- a
      (newickout <- paste("(",paste(desc,collapse=","),")",il,sep=""))
    }
  }
  else { (newickout <- a) }
}

# data.frame to newick function (copied from Martin Turjak)
df2newick <- function(df, innerlabel=FALSE){
  alevel <- as.character(unique(df[,1]))
  newick <- NULL
  for(x in alevel) newick <- c(newick,traverse(x,1,innerlabel))
  (newick <- paste("(",paste(newick,collapse=","),");",sep=""))
}

# in use of the above code:
df <- tree_imp
myNewick <- df2newick(tree_imp, )

library(ape)
mytree <- read.tree(text=myNewick)
class(mytree)
# this will take forever... 
plot(mytree)
mytree

TipLabels(mytree)

# microbiome stability measured by change in CST (delta cluster state transition?), IAD, PAM

# vitamin D is training set, and metadata that we'll pull is validation set
#  we just need microbiome data (at least baseline data with 2+ timepoints) with an SRA file of their data, a good metadata file (w sample names), good diet data
#  we'll have to extract reads and process bioinformatics

# does baseline HEI , food div, food pattern, or nutrients predct MS

# does baseline diet predict MS

# Ankan's idea: cluster diets into groups- does any microbiome feature (alpha div, etc) predict the clusters
# this is correlative, not predictive, so it goes both ways


# ? # ape::write.tree(FOOD_TREE, file='FOOD_TREE.txt')

########################## UNIFRAC ###########################

week1_weighted_df <- read.csv("Weighed_food_matrix_week1.csv")
week2_weighted_df <- read.csv("Weighed_food_matrix_week2.csv")

week1_weighted_matrix <- data.matrix(week1_weighted_df, rownames.force = NA)
week2_weighted_matrix <- data.matrix(week2_weighted_df, rownames.force = NA)

ncol(week1_weighted_matrix)
week1_weighted_matrix <- week1_weighted_matrix[,-c(1:2)]
week2_weighted_matrix <- week2_weighted_matrix[,-c(1:2)]
ncol(week1_weighted_matrix)
week1_weighted_matrix[1,1]

columns <- colnames(week1_weighted_matrix)
columns_n <- substring(columns, 2, 9)

columns2 <- colnames(week2_weighted_matrix)
columns_n2 <- substring(columns2, 2, 9)

colnames(week1_weighted_matrix) <- columns_n
colnames(week2_weighted_matrix) <- columns_n2
class(columns_n) # character

library("rbiom")

class(mytree)
#mytree$tip.label <- as.character(mytree$tip.label) #character

#transpose the matrix
week1_weighted_matrix <- t(week1_weighted_matrix)

#remove some rows with bad food codes (like vinegar and taco seasoning) 
row_names <- row.names(week1_weighted_matrix)
row_names <- as.numeric(row_names)
which(is.na(row_names)) #114, 115, 842
week1_weighted_matrix[114]
week1_weighted_matrix <- week1_weighted_matrix[-c(114, 115, 842),]

unifrac(week1_weighted_matrix, weighted = TRUE, tree = mytree)

########################## K-CAL MATRIX ###########################
############################ TAKE 1

#install.packages("xlsx")
library(readxl)
#library(xlsx)
library(dplyr)
library(tidyr)

kcal_matrix <- read_excel('food_matrix.xlsx', sheet = "Kcal_R") # colNames = TRUE, rowNames = TRUE)

data.frame(kcal_matrix)

# just to see
kcal_matrix[1:6, ]
colnames(kcal_matrix)

# make the first column the col headers, then delete first col of headers
#rownames(kcalmatrix)
#kcalMatrix <- kcalmatrix
#rownames(kcalMatrix) <- kcalmatrix$`Row Labels`
#kcalMatrix[1:6, ]
#kcalMatrix <- kcalMatrix[,-1] (only run once)
#kcalMatrix[1:6, ]

#str(kcalMatrix)

# make data as numeric
#kcalmatrix <- kcal_matrix %>% mutate_at(c(1:1416), ~replace_na(.,0))

# is.character, as.numeric)
#as.numeric(kcalmatrix)

#fill in NAs with Os
kcal_matrix[is.na(kcal_matrix)] <- as.numeric(0)

# 5 duplicates found
sum(duplicated(kcal_matrix[,2:687])) # 5
duplicated(kcal_matrix[,2:687]) #indexes 301, 302, 543, 544, 546

kcal_matrix[301,1] #VDMT19 2/28/2022
kcal_matrix[302,1] #VDMT19 3/1/2022
kcal_matrix[543,1] #VDMT34 6/19/2022
kcal_matrix[544,1] #VDMT34 6/30/2022
kcal_matrix[546,1] #VDMT34 7/1/2022

# calculate relative kcal of each food

kcal_matrix[,1] # 'Row Labels'
kcal_matrix[,1416] # 'Grand Total'

totcal_matrix <- kcal_matrix[,1]

# add new columns according to the percentage of kcal of that day
for (x in 2:1415) {
  totcal_matrix <- cbind(totcal_matrix, (kcal_matrix[,x])/(kcal_matrix[,1416]))
}

totcal_matrix

############################ TAKE 2
kcal_matrix1 <- read_excel('food_matrix.xlsx', sheet = "matrix_wSedits")
data.frame(kcal_matrix1)
ncol(kcal_matrix1)
nrow(kcal_matrix1)

#fill in blanks with 0s
kcal_matrix1[is.na(kcal_matrix1)] <- as.numeric(0)

#check for duplicates:
sum(duplicated(kcal_matrix1[,2:578])) #there are 6 that exist?
which(duplicated(kcal_matrix1[,2:687], arr.ind = TRUE))
#458, 577
kcal_matrix1[458, 1] # VDMT34 2022-07-01 - this matches up with participant 12, day 7
kcal_matrix1[577, 1] # this is blank and can be deleted...

# this isn't making much sense --> new method done and described in FoodMatrix.R

############################ IMP LINKS #############################

# ABBY PAPER
# https://www.sciencedirect.com/science/article/pii/S1931312819302501?via%3Dihub#sec4

# CYTOSCAPE HELP
# https://github.com/cytoscape/cytoscape-tutorials/wiki
# https://manual.cytoscape.org/en/stable/Creating_Networks.html

# ABBY JOHNSON GITHUB
# https://github.com/knights-lab/dietstudy_analyses/blob/master/lib/results_scripts/2_Recovery%20of%20dietary%20diversity%20from%20food/1_Figure2A/food.edge.table.txt
# https://github.com/knights-lab/dietstudy_analyses/blob/master/lib/results_scripts/2_Recovery%20of%20dietary%20diversity%20from%20food/1_Figure2A/make_edge_and_node_table_L3.R
# https://github.com/knights-lab/dietstudy_analyses/tree/master/data/diet/raw_and_preprocessed_ASA24_data
# https://github.com/knights-lab/dietstudy_analyses/blob/master/data/maps/UserName_map.txt
# https://github.com/abbycole/food_graphlan
# https://github.com/knights-lab/Food_Tree/blob/master/R/data/MCT/MCTdatabase.txt
# https://github.com/knights-lab/Food_Tree/blob/master/R/lib/make.food.tree.r


# ANKAN SHOOWED ME THIS:
# talked of hierarchical clustering and sequence alignment and
# clustal omega https://www.ebi.ac.uk/Tools/msa/clustalo/


