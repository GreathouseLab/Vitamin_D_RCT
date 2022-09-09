# Set appropriate working directory (WD). WD is where all your data files live.

# Packages installed
install.packages("dplyr")
install.packages('readxl')
install.packages("writexl")

# Package library
library("dplyr")
library("readxl")
library("writexl")

#Pull in Diet data (Diet data is Totals file from ASA24 output folder)
diet_data <-  read.csv("asa24total.csv")
colnames(diet_data)[2]<- "ID"

#Pull in Demographic data from clinical codebook for VDMT.
# Remember- Clinical Codebook has many excel sheets.
# One of the sheets is demographic. Look at the steps to do this. 

#############      Reading VDMT data into R      #############

data1<-read_xlsx("Code book-VDMT.xlsx")


############  looking up how many excel sheets are there in the workbook   #####

excel_sheets("Code book-VDMT.xlsx")



###############  create a vector of character strings for names of different spreadsheets in our data    ####

sheet_names<-excel_sheets("Code book-VDMT.xlsx")


list_all<- lapply(sheet_names, function(x){
  as.data.frame(read_excel("Code book-VDMT.xlsx", sheet = x))})
names(list_all)<-sheet_names




############  Importing Demographics data from the entire workbook    ########

demograph<-list_all$DEMOGRAPH

range(demograph$AGE, na.rm = TRUE)       #youngest- 18 years, oldest- 53 years



############ Selecting only a few columns from demographic data file for manageability #######
demo_macro<- demograph[, c(1,3,4,5)]



######## Joining Demographic (truncated file) and diet data 
VDMT_diet<- full_join(demo_macro, diet_data, by="ID")

Age_range<-range(unique(VDMT_diet$AGE))




################################################################
# Starting Data cleaning as per ASA-24 protocol
# CLEANING No.1------------Missing Data
###############################################################
# https://epi.grants.cancer.gov/asa24/resources/asa24-data-cleaning-2020.pdf

# rows in table- Items/INS with no kcal, nutrients, or other components
# Less than 10% data missing is acceptable


# Reading in ASA-24 output data files from WD
asaINS <-  read.csv("asa24INS.csv")
asaITEMS<- read.csv("asa24ITEMS.csv")
asaRESPONSE<- read.csv("asa24response.csv")
asaTS<- read.csv("asa24TS.csv")
asaTNS<- read.csv("asa24TNS.csv")

missing_ITEMS<- asaITEMS[asaITEMS$KCAL== 0,]

missing_item_percent<- (nrow(missing_ITEMS)/nrow(asaITEMS))*100
print(missing_item_percent)

# 13.73% of data in ITEMS missing as per kcal column

#investigating the matter deeper? Why 13.73% are missing. 
# Look at the food codes and probe?
# FNDDS database-- https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fndds-download-databases/
# Food code-- 94000100 in FNDDS database is tap water and kcal will be 0
# Find out how many rows are tap water and other baverages with no kcal?

nrow(missing_ITEMS[missing_ITEMS$FoodCode == 94000100, ])

# 826 rows out of 1385 are tap waters
1385-826

# what is remaining 559 rows have for food code? List of food codes present

foodcodes_missing_list<-unique(missing_ITEMS$FoodCode)

names(foodcodes_missing_list)<- c("Tap water","Bottled unsweet water", "Carbonated unsweetetend",
                                  "Tea, hot, leaf, green, decaffeinated", "unrecognized food code", "Coffee, brewed, decaffeinated",
                                  "Sports drink, low calorie (Powerade Zero)",
                                  "Soft drink, fruit flavored, caffeine containing, diet",
                                  "Coffee, NS as to brewed or instant, decaffeinated",
                                  "Coffee, brewed, blend of regular and decaffeinated")

# number of rows with foodcode- 2047 (unrecognized)= 1

nrow(missing_ITEMS[missing_ITEMS$FoodCode == 2047, ])

# 1384 rows are water and coffee's that may not have any calories if consumed without creamers



(10083/10084)*100
# we have 99.9% data available. We passed the missing data 
#test as per ITEMS output and kcal column. 

#############################################################################
# CLEANING No.2--------- Remove incomplete ASA-24 responses
########################################################################


#First Pull in Diet data, excel file that has all info (Total)
diet_data <-  read.csv("asa24total.csv")


#display list of colnames
colnames(diet_data)


# number of completed ASA24 and number of incompleted ASA 24 records
# recall status of 2= complete, 5=incomplete
completion<-table(diet_data$RecallStatus)


#extracting the rows with incomplete ASA24
diet_data[diet_data$RecallStatus == 5, ]

# Remove the rows from ASA-24 totals file that contains recallstatus=5
# Note diet_data_1 is subset of diet data with 7 rows removed that contained
# incomplete ASA-24 records. 
diet_data_1<-subset(diet_data, RecallStatus != 5,)  



######################################################################
# CLEANING No.3-------Outlier information
######################################################################
############################################
#Selecting specific columns from demographic data set to make things manageable
############################################

selected_demogr_data<- demograph[, c(1,2,4,5,6)]
colnames(selected_demogr_data)
head(selected_demogr_data)




############################################
# Join Diet data and truncated demographic data (selected_demogr_data)
############################################

colnames(diet_data_1)[2]<- "ID"
demo_dietdata<- full_join(selected_demogr_data, diet_data_1, by = "ID")

colnames(demo_dietdata)


#############################################
# Look for for dietary data for days when the participant 
# reported macroonutrient intakes as follows:
#NUTRIENT OUTLIER 
#############################################
# Kcal:
# Women <600 and >4400
# Men <650 and >5700
# Protein: 
# Women <10 and >180
# Men <25 and >240
# Fat:
# Women <15 >185
# Men <25 >230


macronutrient_outliers <- demo_dietdata %>% 
  filter((SEX == "2" & (KCAL < 600 | KCAL > 4400)) | 
           (SEX == "1" & (KCAL < 650 | KCAL > 5700)) |
           (SEX == "2" & (PROT < 10 | PROT > 180)) | 
           (SEX == "1" & (PROT < 25 | PROT > 240)) |
           (SEX == "2" & (TFAT < 15 | TFAT > 185)) | 
           (SEX == "1" & (TFAT < 25 | TFAT > 230)))

nrow(macronutrient_outliers)

# Exporting macronutrient outlier information in excel file in WD.  

#Storing the macronutrient outlier information in a dataframe called macro_outliers 
write_xlsx(macronutrient_outliers, "C:\\Users\\madhu\\Box\\M_COLON\\Madhur\\VDMT\\VDMT Data\\analysis\\macronutrient_outlier.xlsx")


# What to do with this macronutrient outlier information?
# Go over each record and try to understand why these records are outliers





##########################################################
# Portion Outlier
##########################################################







##########################################################
# Outliers by statistical thresholds
##########################################################
# intakes above the 75th percentile plus 
#two or three times the interquartile range might be flagged for review.



#Compute the number of columns in diet data.
M=ncol(diet_data_1)
M


#Now, let's create a list of all the numerical variables.

numeric.index=c()
for(j in 1:M){
  if(is.numeric(diet_data_1[,j])){numeric.index=c(numeric.index,j)}
}

numeric.index  #This vector has the column numbers for the numeric variables


# Listing column names for the data in a vertical list
cbind(colnames(diet_data_1))


################################################################################
#Now, let's do your three tasks for each of the numeric columns.

#We need to store the IQR data in a matrix.
IQR.data<-matrix(0,nrow=M,ncol=8)
IQR.data<-data.frame(colnames(diet_data_1),IQR.data)
colnames(IQR.data)=c("Variable","5%","25%","50%","75%","90%","IQR",
                     "2*IQR",
                     "3*IQR")


#Compute quartile information for all the quantitative data.
for(j in numeric.index){
  IQR.data[j,2:6]=quantile(diet_data_1[,j],
                           probs= c(0.05,0.25,0.50,0.75,0.90),
                           na.rm= TRUE)
  
}

IQR.data[,7]=IQR.data[,5]-IQR.data[,3]
IQR.data[,8]=2*IQR.data[,7]
IQR.data[,9]=3*IQR.data[,7]

#Test this for column 14
m<-quantile(diet_data_1[14],
            probs= c(0.10,0.25,0.50,0.75,0.90),
            na.rm= TRUE)
m

IQR.data[14,]   #The 14th row of IQR.data contains info 
#for the 14th column of diet_data_1


#See all results in one table.  There will be all zeroes for categorical variables.
View(IQR.data)


#Show results only for numeric variables.
View(IQR.data[numeric.index,])

#Export to a csv file, so you can open this in Excel or paste it into a report.
write.csv(IQR.data[numeric.index,],"IQR.data.csv",row.names=FALSE)




################################################################################
#Plots

#The most suitable plot for displaying quantile information is a boxplot. This code
#will produce box plots for all your numerical variables.  They will show up in
#your plot window in R, and you can use the back/forward buttons (arrows) to see them.

for(j in numeric.index){
  boxplot(diet_data_1[,j],main=colnames(diet_data_1[j]))
}


#You can combine several plots into one.  Let's say you want to do a 2x2 with 
#your first four plots.

par(mfrow=c(2,2))
plot.index=1:4

for(j in numeric.index[plot.index]){
  boxplot(diet_data_1[,j],main=colnames(diet_data_1)[j])
}

#Sometimes you get the error "Margins too small".  If that happens, just make your
#plot window in Rstudio bigger.  For instance, try making it big enough to do a 4x2
#plot with eight plots.

dev.off()  #This closes previous plot.

par(mfrow=c(4,2),mar=c(1,4.1,4.1,1))
plot.index=8:11

for(j in numeric.index[plot.index]){
  boxplot(diet_data_1[,j],main=colnames(diet_data_1)[j])
}




#Plots in R sometimes look pretty bad in the default plot window.  Hit the Zoom
#button to see a better plot.  You can export the plot as an image or pdf of any
#size, e.g. the 6in x 8in pdf from my email.




###########################
#Individual Boxplots of Macronutrients
###########################

boxplot(diet_data_1$KCAL,
        main = "Kcal outliers",
        ylab = "Calories",
        col = "orange",
        border = "brown",
        notch = TRUE)


boxplot(diet_data_1$PROT,
        main = "Protein outliers",
        ylab = "Protein (in grams)",
        col = "orange",
        border = "brown",
        notch = TRUE)



boxplot(diet_data_1$CARB,
        main = "Carbohydrate outliers",
        ylab = "Carbs (in grams)",
        col = "orange",
        border = "brown",
        notch = TRUE)



boxplot(diet_data_1$TFAT,
        main = "Fat outliers",
        ylab = "Fats (in grams)",
        col = "orange",
        border = "brown",
        notch = TRUE)







##################################################
#CLEANING No.6 --------- Duplicate Data
##################################################
# No duplicate data found
diet_data_1[duplicated(diet_data_1$RecallRecId),]
















############################
##ASA_24.GGPlots
############################
#This section will focus on creating a visual representation of KCAL, PROT, CARB, and TFAT for each participant at each time point. 

diet_data


#KCAL_GGPLOT
library(ggplot2)
ggplot(data=diet_data, #macronutrient_outliers_completedata, 
       aes(x=UserName, y=KCAL), 
       shape=Intervention, color= ID) +
  geom_point(size=4) +
  labs(title="Total KCAL") +
  facet_wrap(vars(diet_data$ID)) #+ _completedata$ID))+

#ggsave(filename="TotalKCAL.pdf")

ggplot(data=macronutrient_outliers,
       aes(x=RecallNo, y=PROT, 
           shape=Intervention, color= ID))+
  geom_point(size=4)+
  labs(title="Total Protein")+
  facet_wrap(vars(macronutrient_outliers$ID))

ggplot(data=macronutrient_outliers,
       aes(x=RecallNo, y=TFAT, 
           shape=Intervention, color= ID))+
  geom_point(size=4)+
  labs(title="Total Fat")+
  facet_wrap(vars(macronutrient_outliers$ID))

ggplot(data=macronutrient_outliers,
       aes(x=RecallNo, y=CARB, 
           shape=Intervention, color= ID))+
  geom_point(size=4)+
  labs(title="Total Carbs")+
  facet_wrap(vars(macronutrient_outliers$ID))

