# Ella von Dohlen
# Macronutrient Data Visualizations

library(ggplot2)
library(dplyr)
install.packages("qwraps2")
library(qwraps2)
install.packages("ggpubr")
library(ggpubr)
install.packages('readxl')
library("readxl")

#data <- read.csv(file="asa24total.csv", header=T)
data<-read_xlsx("EDITED VDMT_2022-07-07_81282_Totals (1) (version 1).xlsx")
colnames(data)

excel_sheets("EDITED VDMT_2022-07-07_81282_Totals (1) (version 1).xlsx")
# only one sheet exists


############## CORRECTING THE ID COLUMN ################
data[2]
names(data)[2] <- 'ID'
colnames(data)[2]

num_rows <- length(data$ID)

# to reformat the IDs- from VDMT01 to VDMT001
for (i in 1:num_rows){
  new_string <- gsub("^(.{4})(.*)$",         # Apply gsub
                       "\\10\\2",
                       data$ID[i])
  data$ID[i] <- new_string
}

data$ID

#################### HANDLING NAs #####################
sum(is.na(data)) ## 116
which(is.na(data), arr.ind=TRUE) # all NAs are in row 364, for the entire row
data <- data[-364,]

################ CODEBOOK #################################
data1<-read_xlsx("Code book-VDMT.xlsx")
sheet_names<-excel_sheets("Code book-VDMT.xlsx")
list_all<- lapply(sheet_names, function(x){
  as.data.frame(read_excel("Code book-VDMT.xlsx", sheet = x))})
names(list_all) <- sheet_names

demograph<-list_all$DEMOGRAPH
colnames(demograph)
demo_macro <- demograph[, c("ID", "DOB", "AGE", "SEX", "RACE", 
                           "ETHN", "ORIGIN", "OCCU")]

################ MACRO OVERVIEW #################################

data_macro <- data[ , c("KCAL", "PROT", "TFAT", "CARB", "VITD")]

#vioin_overview <- ggplot(data_macro, inherit.aes=TRUE) + 
#  geom_violin(trim=FALSE)

# maybe i can find something to graph them all?

################ MACRO HISTOGRAMS #################################

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

par(mar=c(0, 4, 1.1, 2.1))
boxplot(data$KCAL, ylim = c(0,6000), horizontal=TRUE, frame=F, xaxt="n", col = rep(2))

par(mar=c(4, 4, 1.1, 2.1))
kcal_hist <- hist(data$KCAL,
                  main = "Recorded KiloCalories",
                  xlab = "Kilocalories",
                  ylab = "Number of ASA records",
                  breaks = 20,
                  xlim = c(0, 6000),
                  ylim = c(0, 100),
                  col = rep(2))

text(kcal_hist$mids, kcal_hist$counts, labels = kcal_hist$counts, adj = c(0.5, -0.5))

#### to make boxplot below histogram ####

#layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(8, 1))

#par(mar=c(4, 4, 1.1, 2.1))
#kcal_hist <- hist(data$KCAL,
#                  main = "KiloCalories Recorded",
#                  xlab = "Kilocalories",
#                  ylab = "Number of ASA records",
#                  breaks = 20,
#                  xlim = c(0, 8000),
#                  ylim = c(0, 250),
#                  col = rep(2))

#text(kcal_hist$mids, kcal_hist$counts, labels = kcal_hist$counts, adj = c(0.5, -0.5))

#par(mar=c(0, 4, 0, 2.1))
#boxplot(data$KCAL, ylim = c(0,8000), horizontal=TRUE, frame=F, xaxt="n", col = rep(2))

### end of testing layout



### table rough drafts

#KCAL_SEX_df <- table(data$KCAL, data$SEX)

cbind(summary(data$KCAL)) # missing range, standard dev
# make quantile table, 
# connect as dataframe both the summary and quantile 

summary_vals_KCAL <- list("min         " = ~min(data$KCAL), 
                          "1st quartile" = ~quantile(data$KCAL, prob = .25), 
                          "median      " = ~quantile(data$KCAL, prob = .5), 
                          "3rd quartile" = ~quantile(data$KCAL, prob = .75), 
                          "max         " = ~max(data$KCAL))

summary_KCAL <- summary_table(data, summary_vals_KCAL)

summary(data$KCAL)

write.table(summary_KCAL, file = "KCALsummarystats.txt") #, sep = "\t", quote = FALSE, row.names = F)
# DOESNT WORK

#par(mar=c(0, 0, 0, 0))
#(bottom, left, top, right)

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

par(mar=c(0, 4, 0, 2.1))
boxplot(data$PROT, ylim = c(0,600), horizontal=TRUE, frame=F, xaxt="n", col = rep(3))

par(mar=c(4, 4, 1.1, 2.1))
prot_hist <- hist(data$PROT,
                  main = "Protein Recorded",
                  xlab = "Protein (grams)",
                  ylab = "Number of ASA Records",
                  breaks = 20,
                  xlim = c(0, 600),
                  ylim = c(0,150),
                  col = rep(3))

text(prot_hist$mids, prot_hist$counts, labels = prot_hist$counts, adj = c(0.5, -0.5), cex = .65)

# investigating possible outlier/ mistake?
high_prot <- (data$PROT >= 500) == TRUE # row 204 has high prot
data$PROT[204]

################ 546.634 grams from person 44 - # 204 element # should be 66.66263 


par(mar=c(0, 4, 1.1, 2.1))
boxplot(data$TFAT, ylim = c(0,250), horizontal=TRUE, frame=F, xaxt="n", col = rep(4))

par(mar=c(4, 4, 1.1, 2.1))
fat_hist <- hist(data$TFAT,
                 main = "Fat Recorded",
                 xlab = "Fat (grams)",
                 ylab = "Number of ASA Records",
                 breaks = 20,
                 xlim = c(0, 250),
                 ylim = c(0,100),
                 col = rep(4))

text(fat_hist$mids, fat_hist$counts, labels = fat_hist$counts, adj = c(0.5, -0.5))


par(mar=c(0, 4, 1.1, 2.1))
boxplot(data$CARB, ylim = c(0,600), horizontal=TRUE, frame=F, xaxt="n", col = rep(5))

par(mar=c(4, 4, 1.1, 2.1))
carb_hist <- hist(data$CARB,
                  main = "Carbohydrates Recorded",
                  xlab = "Carbohydrates (grams)",
                  ylab = "Number of ASA Records",
                  breaks = 20,
                  xlim = c(0, 600),
                  ylim = c(0,80),
                  col = rep(5))

text(carb_hist$mids, carb_hist$counts, labels = carb_hist$counts, adj = c(0.5, -0.5))


par(mar=c(0, 4, 1.1, 2.1))
boxplot(data$VITD, ylim = c(0,70), horizontal=TRUE, frame=F, xaxt="n", col = rep(6))

par(mar=c(4, 4, 1.1, 2.1))
vitd_hist <- hist(data$VITD,
                  main = "Dietary Vitamin D intake",
                  xlab = "Vitamin D (micrograms)",
                  ylab = "Number of ASA Records",
                  breaks = 30,
                  xlim = c(0, 70),
                  ylim = c(0,250),
                  col = rep(6))

text(vitd_hist$mids, vitd_hist$counts, labels = vitd_hist$counts, adj = c(0.5, -0.5))

################ NEW HISTOGRAMS WITH SEXES BROKEN UP ##############

### joining data to get sex and macros
data["ID"]

VDMT_diet<- full_join(demo_macro, data, by="ID")
colnames(VDMT_diet) # 4 SEX, 17 KCAL, 18 PROT, 19 TFAT, 20 CARB ...

data_macro_sex <- VDMT_diet[ ,c("SEX", "KCAL", "PROT", "TFAT", "CARB", "VITD")]
data_macro_sex

# CHANGING SEX IDENTIFICATION: 1 to male, 2 to female

#male_macros <- data_macro_sex[data_macro_sex$SEX == 1, ]
#female_macros <- data_macro_sex[data_macro_sex$SEX == 2, ]

data_macro_sex["SEX"][data_macro_sex["SEX"] == 1] <- "male"
data_macro_sex["SEX"][data_macro_sex["SEX"] == 2] <- "female"

### kilocalories

kcal_sexes <- data_macro_sex[ ,c("SEX", "KCAL")]
#kcal_sexes[which(kcal_sexes$SEX == 1)]
ggplot(kcal_sexes, aes(x=KCAL, fill = as.factor(SEX), color = SEX)) +
  geom_histogram(color = 2, 
                 alpha = 0.3, 
                 position = 'identity') + 
  labs(title = "Kilocalories per Sex", 
       x = "KiloCalories", 
       y = "Number of ASA records", 
       fill = "sex")

# PROT

prot_sexes <- data_macro_sex[ , c("SEX", "PROT")]
ggplot(prot_sexes, aes(x=PROT, fill = as.factor(SEX), color = SEX)) +
  geom_histogram(color = 3, 
                 alpha = 0.3, 
                 position = 'identity') + 
  labs(title = "Protein per Sex", 
       x = "Protein (grams)", 
       y = "Number of ASA records", 
       fill = "sex")

# TFAT

tfat_sexes <- data_macro_sex[ , c("SEX", "TFAT")]
ggplot(tfat_sexes, aes(x=TFAT, fill = as.factor(SEX), color = SEX)) +
  geom_histogram(color = 4, 
                 alpha = 0.3, 
                 position = 'identity') + 
  labs(title = "Fat per Sex", 
       x = "Fat (grams)", 
       y = "Number of ASA records", 
       fill = "sex")

# CARB

carb_sexes <- data_macro_sex[ , c("SEX", "CARB")]
ggplot(carb_sexes, aes(x = CARB, fill = as.factor(SEX), color = SEX)) +
  geom_histogram(color = 5, 
                 alpha = 0.3, 
                 position = 'identity') +
  labs(title = "Carbohydrates per Sex", 
       x = "Carbohydrates (grams)", 
       y = "Number of ASA records", 
       fill = "sex")

# VITD

vitd_sexes <- data_macro_sex[ , c("SEX", "VITD")]
ggplot(vitd_sexes, aes(x = VITD, fill = as.factor(SEX))) +
  geom_histogram(color = 6, 
                 alpha = 0.3, 
                 position = 'identity') + 
  labs(title = "Dietary Vitamin D Intake per Sex", 
       x = "Vitamin D (micrograms)", 
       y = "Number of ASA records", 
       fill = "sex")


################ PIE GRAPH DEMOGRAPHICS ################


par(mfrow=c(1, 1))

codebook_data <- read.csv(file="Code book-VDMT.csv", header=T)
colnames(codebook_data)

# participants 1, 45, 46, and 47 have NA for these variables
#demographics_data <- codebook_data[2:44,c("DOB", "RACE", "RACE_OTHER", "ETHN", "ETHN_OTHER", "ORIGIN", "OCCU")]
demo_macro
colnames(demo_macro)



race_lbls <- c("white", "black", "asian", "indian", "other", "na")
pie(demographics_data$RACE, labels = race_lbls)

length(which(demographics_data$RACE == 1)) # 28 white
length(which(demographics_data$RACE == 2)) # 4 black
length(which(demographics_data$RACE == 3)) # 9 asian
length(which(demographics_data$RACE == 4)) # 0 indian
length(which(demographics_data$RACE == 5)) # 2 other
length(which(demographics_data$RACE == 6)) # 0 preferred not to answer

race_slices <- c(28, 4, 9, 2)
race_lbls <- c("White", "African American", "Asian", "Other")
pie(race_slices, labels = race_lbls, main = "Recorded Races")



#demographics_data$DOB
#as.Date(demographics_data$DOB)



sex_lbls <- c("male", "female")
pie(c(length(which(demo_macro$SEX == 1)), length(which(demo_macro$SEX == 2))), 
    labels = sex_lbls, main = "PARTICIPANT SEXES")



occu_lbls <- unique(demo_macro$OCCU)
length(which(demo_macro$OCCU == "Student"))                # 40
length(which(demo_macro$OCCU == "Office Administrative"))  # 1
length(which(demo_macro$OCCU == "Pastor"))                 # 1
length(which(demo_macro$OCCU == "Scientist"))              # 1

occu_slices <- c(40, 1, 1, 1)
pie(occu_slices, labels = occu_lbls, main = "Recorded Occupations")



origin_lbls <- unique(demo_macro$ORIGIN)
length(which(demo_macro$ORIGIN == origin_lbls[1])) # 36 United States
length(which(demo_macro$ORIGIN == origin_lbls[2])) # 1 Spain and Lebanon
length(which(demo_macro$ORIGIN == origin_lbls[3])) # 1 Nigerian
length(which(demo_macro$ORIGIN == origin_lbls[4])) # 1 India
length(which(demo_macro$ORIGIN == origin_lbls[5])) # 1 Republic of Korea
length(which(demo_macro$ORIGIN == origin_lbls[6])) # 1 Colombia
length(which(demo_macro$ORIGIN == origin_lbls[7])) # 1 Brazil
length(which(demo_macro$ORIGIN == origin_lbls[8])) # 1 USA

origin_slices <- c(37, 1, 1, 1, 1, 1, 1)
origin_lbls[1:7]
pie(origin_slices, labels = origin_lbls[1:7], main = "Recorded Countries of Origin")
