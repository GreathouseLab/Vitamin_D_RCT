# Ella von Dohlen 
# 09/2024
# Cleaning Data Tree Levels

library(tidyr)

################### ABBY JOHNSON'S TAXONOMIC LEVELS ######################

# from knights-lab/dietstudy_analyses/data/diet
taxonomy_raw <- read.csv(file = "diet.taxonomy.txt", header = T)
colnames(taxonomy_raw)

taxonomy_raw[125:130,]

# to split into three columns:
taxonomy <- separate(taxonomy_raw,
                     col = FoodID.taxonomy.Main.food.description,
                     into = c('FoodID', 'taxonomy', 'description'),
                     sep = '\t')

# to view an example of what the entries now look like:
taxonomy[1,]

################################# VDMT DATA ################################

ASA24_items <- read.csv('ASA24items_cleaned.csv')
colnames(ASA24_items)
dim(ASA24_items)

# selecting for significant variables
imp_vars <- c("UserName", "FoodCode", "ModCode", "Food_Description", "FoodAmt")
ASA24_items1 <- ASA24_items[imp_vars]

nrow(ASA24_items1) # 8591

# to view an example entry
ASA24_items1[1,]

# change format: combine foodcode with modcode, in order to match Abby's data (which is stored in taxonomy)
ASA24_items1$FoodID <- paste(ASA24_items1$FoodCode, ASA24_items1$ModCode, sep='.')
colnames(ASA24_items1)
ASA24_items2 <- ASA24_items1[c("UserName", "FoodAmt", "FoodID", "Food_Description")]
colnames(ASA24_items2)

################ COMBINE OUR ASA24 DATA WITH ABBY'S TAXONOMY ##############

# merge our asa24 data with abby's taxonomy
data <- merge(x = ASA24_items2, y = taxonomy, by = "FoodID", all.x = TRUE)

# characteristics of new dataframe
nrow(data) # 8591
data[1:6,]
colnames(data)

# break up abby's five taxonomic levels (warning can be ignored)
data1 <- separate(data,
                  col = taxonomy,
                  into = c('Level1', 'Level2', 'Level3', 'Level4', 'Level5'),
                  sep = ";")
colnames(data1)

data1[1:6,]

colSums(is.na(data1)) #1397 are missing

################ USE MY ASSIGNMENTS TO FILL IN MISSING DATA ################

# filled in taxonomic levels that I manually assigned
all_assigned <- read.csv(file = "all_assigned.csv", header = T)
all_assigned[1:6,]
colnames(all_assigned)

# FoodID from char to double so it can be merged w all_assigned
data1$FoodID <- as.double(data1$FoodID)

# data1 is VD data w abby's taxonomy
merged_df <- merge(data1, all_assigned, by = "FoodID", all.x = TRUE)
colnames(merged_df)
head(merged_df)

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

# delete demo users
merged_df <- merged_df[merged_df$UserName.x != "VDMT_DemoUser", ]

#place all new assignments in the original NA places
merged_df$Level1[is.na(merged_df$Level1)] <- merged_df$Level1_old[is.na(merged_df$Level1)]
merged_df$Level2[is.na(merged_df$Level2)] <- merged_df$Level2_old[is.na(merged_df$Level2)]
merged_df$Level3[is.na(merged_df$Level3)] <- merged_df$Level3_old[is.na(merged_df$Level3)]
merged_df$Level4[is.na(merged_df$Level4)] <- merged_df$Level4_old[is.na(merged_df$Level4)]
merged_df$Level5[is.na(merged_df$Level5)] <- merged_df$Level5_old[is.na(merged_df$Level5)]

colnames(merged_df)
merged_df[1:6,]

drop_vars <- c('UserName.x', 'FoodAmt', 'X', 'original_index', 'UserName.y',
               'Level1_old', 'Level2_old', 'Level3_old', 'Level4_old',
               'Level5_old', 'description', 'Food_Description.y')

merged_df1 <-  merged_df[,!(names(merged_df) %in% drop_vars)]

head(merged_df1)

names(merged_df1)[names(merged_df1) == "Food_Description.x"] <- "Food_Description"

colSums(is.na(merged_df1)) # only the second line- taco seasoning
merged_df2 <- merged_df1[!duplicated(merged_df1),]

dim(merged_df1) # 36387 to
dim(merged_df2) # 1339

write.csv(merged_df2, '~/Desktop/Greathouse/VitaminDstudy/FoodTreeLevels.csv')
