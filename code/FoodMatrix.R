# Ella von Dohlen 
# Spring 2023
# objective: created food matrix, addressing missing data

# use ASA24 items to get participant, timepoints, food code, mod code, and description?
# link dates to day of study
# link participants with timepoints- probs from condensed_df
# use dataset to make IDs the column headers
# the first row needs to be the participant-time points

# https://www.youtube.com/watch?v=emROUw3rT8o

# EITHER
# get unique foods
# make a dictionary
  # make df of unique foods and add indexes
  # dictionary will be by setting the two columns equal to each other
# then use dict to make a list for each timepoint of the number of 0s equal to INDEX - 1, then add 1, then fill in with 0s
# make all lists equal to each other

# OR
# fill in (probably) logically
# change from logical to binary (https://www.statology.org/r-convert-true-false-to-1-0/)ryuiotiotry
# how to fill in 0s and 1s

# OR
# https://stackoverflow.com/questions/62195502/how-i-can-compare-in-r-two-columns-of-a-matrix-with-two-others-and-generate-at-t

#install.packages('lubridate')
library('lubridate')
library('xlsx')
library('dplyr')

# this was done once for food_matrix, and again for food_matrix_withIDs
# which includes RecallRecIDs which are included in Sudili's edited sheet

### make dataset for Victoria and Madhur of ASA items with study days
asa24items <- read.csv(file = "asa24ITEMS.csv", header = T)
cross_ref <- read.xlsx(file = "CrossRefSudili.xlsx", header = T, sheetIndex = 1)

colnames(asa24items)
colnames(cross_ref)

head(cross_ref)
imp_cols <- c("RecallRecId", "Ella.s.assignments")
cross_ref <- cross_ref[imp_cols]

colnames(cross_ref)[2] = "StudyDay"

ASA24ITEMS <- full_join(cross_ref, asa24items, by = "RecallRecId")
head(ASA24ITEMS)
nrow(ASA24ITEMS)
sum(is.na(ASA24ITEMS$StudyDay))
no_study_day <- ASA24ITEMS[is.na(ASA24ITEMS$StudyDay),]
length(unique(no_study_day$RecallRecId)) #118
length(unique(ASA24ITEMS$RecallRecId)) # 694

# to double check - further investigate what the NAs are
part1 <- ASA24ITEMS[ASA24ITEMS$UserName == 'VDMT01', ]
nrow(part1)
unique(part1$RecallRecId)
unique(part1$StudyDay)
sum(unique(is.na(part1$StudyDay))) # 1 -means I'm deleting one of participant 1's entries
# I'm ok with deleting all the NAs for StudyDay

## I believe these were disregarded by Sudili, like as in repeats or just mistake entries
## (I don't think I'd have these if I had done an inner join)


ASA24items <- ASA24ITEMS[!is.na(ASA24ITEMS$StudyDay),]
colnames(ASA24items)
count_per_part <- aggregate(data = ASA24items,
                            StudyDay ~ UserName,
                            function(StudyDay) length(unique(StudyDay)))
write.csv(ASA24items, "~/Desktop/Greathouse/VitaminDstudy/ASA24items_cleaned.csv")


################################################### I later received this EDITED sheet from Sudili


asa24items <- read.csv(file = "asa24ITEMS.csv", header = T)
sudilis_edits <- read.xlsx(file = "EDITED VDMT_2022-07-07_81282_Totals (1) (version 1).xlsx", sheetIndex = 1, header = T)

# create list of kept ASA entries according to Sudili
ASAs_kept <- data.frame(paste(sudilis_edits$UserName, sudilis_edits$ReportingDate, sudilis_edits$RecallRecId))
names(ASAs_kept)[1] = "participant_time"

#extract important features from asa24items
select_items <- subset(asa24items, select = c(UserName, ReportingDate, RecallRecId, FoodCode, ModCode, Food_Description, KCAL))
select_items[1:6, ]

# get date in yyyy-mm-dd format to match sudili's
select_items$ReportingDate <- as.Date(select_items$ReportingDate, "%m/%d/%Y")
select_items$ReportingDate[1:6]

#create new columns, combining participant with time, and food with description
participant_time <- paste(select_items$UserName, select_items$ReportingDate, select_items$RecallRecId)
food <- paste(select_items$FoodCode, select_items$Food_Description)

data_for_matrix <- data.frame(participant_time, food, select_items$KCAL)

data_for_matrix[1:6, ]
colnames(data_for_matrix)

# only keep asa24items (which is in data_for_matrix), if kept by Sudili
edited_data_for_matrix <- subset(data_for_matrix, participant_time %in% ASAs_kept$participant_time)
edited_data_for_matrix[1:6,]

#needed to download java to get the package to work
#install.packages("xlsx", dependencies=TRUE)
library(readxl)
# only once? write.xlsx(data_for_matrix, "~/Desktop/Greathouse/VitaminDstudy/food_matrix.xlsx")

# only once? write.xlsx(edited_data_for_matrix, "~/Desktop/Greathouse/VitaminDstudy/food_matrix_editedperSudili.xlsx")
#this was then copied into the food_matrix.xlsx as withSudili'sedits, and then made into a pivot table and copied into matrix_wSedits

# this was cross referenced with sudili's EDITED via ASA-IDs --> I changed dates to days manually (using CrossRefSudili)

# now read in matrix with days, fill in 0s, change to relative kcals
# also make visual of missing forms


# this is most recent and best matrix according to Sudili's edits (take 1 and 2 on Food.Tree.R are older)
abundance_kcal_matrix <- read_xlsx("food_matrix.xlsx", sheet = "matrix_wSedits_studyDay")

# fill in 0s for NAs
abundance_kcal_matrix[is.na(abundance_kcal_matrix)] <- as.numeric(0)

# EVERYTHING SEEMS TO HAVE BEEN DELETED, SO THIS IS SAVED:
## was trying to change part 44 day 15 --> part 44 day 78
#write.csv(abundance_kcal_matrix, "~/Desktop/Greathouse/VitaminDstudy/abundance_kcal_matrix.csv")
#write.xlsx(abundance_kcal_matrix, "~/Desktop/Greathouse/VitaminDstudy/abundance_kcal_matrix.xlsx")

#change to relative Kcals

#place first column of Part-Day# into a new df
proportional_kcal_matrix <- abundance_kcal_matrix[,1]

# column by column, adding proportion of kcal per days from abundance_kcal_matrix
for (x in 2:1327) {
  proportional_kcal_matrix <- cbind(proportional_kcal_matrix, (abundance_kcal_matrix[,x])/(abundance_kcal_matrix[,1328]))
}
# left with proportional_kcal_matrix finished

# to fix a small error I found:
proportional_kcal_matrix[526,1] #"VDMT40--Day11"
proportional_kcal_matrix[526,1] <- "VDMT40-Day11"

write.csv(proportional_kcal_matrix, "~/Desktop/Greathouse/VitaminDstudy/proportional_kcal_matrix.csv")
# then ankan helped make all these proportional to weights


######################### to visualize missing days
library(stringr)

# if necessary:
proportional_kcal_matrix <- read.csv("~/Desktop/Greathouse/VitaminDstudy/proportional_kcal_matrix.csv", header = T)

#split VDMTXX-DayX into two columns in PartDay dataframe
PartDay1 <- as.data.frame(proportional_kcal_matrix[,2])
PartDay1[c('Participant', 'Day')] <- str_split_fixed(PartDay1$`proportional_kcal_matrix[, 2]`, '-', 2)

# to see
head(PartDay1)

#delete original column- because we added participant and day
PartDay1 <- PartDay1[-1]

# we are left with only participant and day
head(PartDay1)

# create 3rd and 4th cols of just the digits
PartDay1['Part_num'] <- substr(PartDay1$Participant, 5, 6)
PartDay1['Day_num'] <- substr(PartDay1$Day, 4, 5)
PartDay1$Part_num <- as.numeric(PartDay1$Part_num)
PartDay1$Day_num <- as.numeric(PartDay1$Day_num)

plot(PartDay1$Part_num[PartDay1$Day_num != 78], PartDay1$Day_num[PartDay1$Day_num != 78])
plot(PartDay1$Part_num, PartDay1$Day_num)

library(wesanderson)
library(plotrix)
gap.plot(PartDay1$Part_num, PartDay1$Day_num, gap = c(15,77), col = PartDay1$Part_num,
         main = "ASA Records Available for Use", xlab = "Participants in Order (organized by fives)", ylab = "Day of Study")
abline(v = 1)
abline(v = 5)
abline(v = 10)
abline(v = 15)
abline(v = 20)
abline(v = 25)
abline(v = 30)
abline(v = 35)
abline(v = 40)

#abline(PartDay1$Part_num[PartDay1$Part_num = c(1,2,3,4,5,6,7,8,9,10)], col = PartDay1$Part_num)

## make list of ideal Part-Day numbers
## if what we have is present --> true
## else --> NA
## then can plot

 # below is now irrelevant
#### NEW METHOD WITH KCAL # this was accounted for when I re-did above using IDs only that were in Sudili's copy
select_kcal <- subset(asa24items, select = c(UserName, ReportingDate, FoodCode, Food_Description, KCAL))

select_kcal$participant_time <- paste(select_kcal$UserName, select_items$ReportingDate)
select_kcal$food <- paste(select_kcal$FoodCode, select_items$Food_Description)

colnames(select_kcal)
select_kcal <- subset(select_kcal, select = c(participant_time, food, KCAL))
class(select_kcal)

library("writexl")
#write_xlsx(select_kcal, "food_matrix_kcal.xlsx") # this was then manually copied into another sheet of the food_matrix.xlsx

