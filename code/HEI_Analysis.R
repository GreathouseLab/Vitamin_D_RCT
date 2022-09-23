# HEI Score Analysis

data <- read.csv(file="vitamin_d_microbiome_trial.results.csv", header=T, skip = 1)
colnames(data)
colnames(data)[276]
data[276]
#total_HEI_scores <- data[276]

################################# HISTOGRAM #################################
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

par(mar=c(0, 4, 1.1, 2.1))
boxplot(data[276], ylim = c(0,100), horizontal=TRUE, frame=F, xaxt="n")

par(mar=c(4, 4, 1.1, 2.1))
kcal_hist <- hist(data$Total.HEI.2015.Score,
                  main = "Total HEI Scores",
                  xlab = "HEI Score",
                  ylab = "Number of DHQIII records",
                  breaks = 15,
                  xlim = c(0, 100),
                  ylim = c(0, 10))

min(data$Total.HEI.2015.Score) # 29.89 score from participant 26

########################## OVERALL RADAR PLOT ############################
# https://epi.grants.cancer.gov/hei/interpret-visualize-hei-scores.html
# https://r-graph-gallery.com/142-basic-radar-chart.html

#install.packages("fmsb")
library(fmsb)

# collect relevant data- all HEI component scores- and rename columns
HEI_component_scores <- data[, 277:289]
colnames(HEI_component_scores)

ncol(HEI_component_scores)              # 13
nrow(HEI_component_scores)              # 41
which(is.na(HEI_component_scores))      # 0

list_colnames <- c('TotalVegetables', 'GreensBeans', 'TotalFruits', 'WholeFruits',
                   'WholeGrains', 'Dairy', 'TotalProteins', 'SeafoodPlantProteins',
                   'FattyAcids', 'Sodium', 'RefinedGrains', 'SaturatedFats', 'AddedSugars')

names(HEI_component_scores) <- c(list_colnames)
colnames(HEI_component_scores)

#calculate component scores as percentages- create df and populate with %s
# this process standardizes all categories, so the specific min and max are no longer significant
HEI_component_scores_decimals <- data.frame(matrix(ncol = 13, nrow = 41))
colnames(HEI_component_scores_decimals) <- c(list_colnames)

HEI_component_scores_decimals$TotalVegetables <- (HEI_component_scores$TotalVegetables) / 5
HEI_component_scores_decimals$GreensBeans <- (HEI_component_scores$GreensBeans) / 5
HEI_component_scores_decimals$TotalFruits <- (HEI_component_scores$TotalFruits) / 5
HEI_component_scores_decimals$WholeFruits <- (HEI_component_scores$WholeFruits) / 5
HEI_component_scores_decimals$WholeGrains <- (HEI_component_scores$WholeGrains) / 10
HEI_component_scores_decimals$Dairy <- (HEI_component_scores$Dairy) / 10
HEI_component_scores_decimals$TotalProteins <- (HEI_component_scores$TotalProteins) / 5
HEI_component_scores_decimals$SeafoodPlantProteins <- (HEI_component_scores$SeafoodPlantProteins) / 5
HEI_component_scores_decimals$FattyAcids <- (HEI_component_scores$FattyAcids) / 10
HEI_component_scores_decimals$Sodium <- (HEI_component_scores$Sodium) / 10
HEI_component_scores_decimals$RefinedGrains <- (HEI_component_scores$RefinedGrains) / 10
HEI_component_scores_decimals$SaturatedFats <- (HEI_component_scores$SaturatedFats) / 10
HEI_component_scores_decimals$AddedSugars <- (HEI_component_scores$AddedSugars) / 10
HEI_component_scores_decimals

HEI_component_scores_percentages <- HEI_component_scores_decimals * 100
HEI_component_scores_percentages

max(HEI_component_scores_percentages) # 100
min(HEI_component_scores_percentages) # 0

# add max and min for each category to the first 2 lines to % df
min_row <- rep(0, 13)
max_row <- rep(100, 13)

HEI_component_scores_PercentMinMax <- rbind(min_row, max_row, HEI_component_scores_percentages)
HEI_component_scores_PercentMinMax

# make chart
dev.off(dev.list()["RStudioGD"])
par(mar = c(2, 2, 2, 2))
radarchart(HEI_component_scores_PercentMinMax, axistype=1, vlcex = 0.8,
           cglcol = "grey", cglty = 1, axislabcol = "grey",
           caxislabels = seq(0,100,25), cglwd = 0.8)

######################### INDIVIDUAL RADAR PLOT ###########################
# https://statisticsglobe.com/r-layout-function-arrange-plots/
# https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/

# splitting screen into 6 rows by 8 columns
par(mfrow=c(2, 4))

# screen 1 (1 - 8)
# participant 1
participant1 <- HEI_component_scores_PercentMinMax[1:3,]
radarchart(participant1, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.4,0.8,0.8),  # change per graph
           pfcol = rgb(0.2,0.4,0.8,0.5), # change per graph
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 1"))

# participant 2
participant2 <- HEI_component_scores_PercentMinMax[c(1:2, 4),]
radarchart(participant2, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.6,0.8,0.8),
           pfcol = rgb(0.2,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 2"))

# participant 3
participant3 <- HEI_component_scores_PercentMinMax[c(1:2, 5),]
radarchart(participant3, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.8,0.8,0.8),
           pfcol = rgb(0.2,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 3"))

# participant 4
participant4 <- HEI_component_scores_PercentMinMax[c(1:2, 6),]
radarchart(participant4, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,1,0.8,0.8),
           pfcol = rgb(0.2,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 4"))

# participant 5
participant5 <- HEI_component_scores_PercentMinMax[c(1:2, 7),]
radarchart(participant5, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.4,0.8,0.8),
           pfcol = rgb(0.4,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 5"))

# participant 6
participant6 <- HEI_component_scores_PercentMinMax[c(1:2, 8),]
radarchart(participant6, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.6,0.8,0.8),
           pfcol = rgb(0.4,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 6"))

# participant 7
participant7 <- HEI_component_scores_PercentMinMax[c(1:2, 9),]
radarchart(participant7, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.8,0.8,0.8),
           pfcol = rgb(0.4,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 7"))

# participant 8
participant8 <- HEI_component_scores_PercentMinMax[c(1:2, 10),]
radarchart(participant8, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,1,0.8,0.8),
           pfcol = rgb(0.4,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 8"))


#dev.off(dev.list()["RStudioGD"])
# i dont think this is working:
#for (i in range(5)){
#  par(mfrow=c(2, 4))
#  for (j in 1:8){
#       participant <- HEI_component_scores_PercentMinMax[c(1:2, (j+2)),]
#       radarchart(participant, axistype=1, vlcex = 0.5,
#                  pcol = rgb(0.4,1,0.8,0.8),
#                  pfcol = rgb(0.4,1,0.8,0.5),
#                  cglcol = "grey",
#                  cglty = 1,
#                  axislabcol = "grey",
#                  caxislabels = seq(0,100,25),
#                  cglwd = 0.8)
#  }
#}

# new screen (9 - 16)
# participant 9
participant9 <- HEI_component_scores_PercentMinMax[c(1:2, 11),]
radarchart(participant9, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.4,0.8,0.8),
           pfcol = rgb(0.4,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 9"))

# participant 10
participant10 <- HEI_component_scores_PercentMinMax[c(1:2, 12),]
radarchart(participant10, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.6,0.8,0.8),
           pfcol = rgb(0.4,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 10"))

# participant 11
participant11 <- HEI_component_scores_PercentMinMax[c(1:2, 13),]
radarchart(participant11, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.8,0.8,0.8),
           pfcol = rgb(0.4,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 11"))

# participant 12
participant12 <- HEI_component_scores_PercentMinMax[c(1:2, 14),]
radarchart(participant12, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,1,0.8,0.8),
           pfcol = rgb(0.4,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 12"))

# participant 13
participant13 <- HEI_component_scores_PercentMinMax[c(1:2, 15),]
radarchart(participant13, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.4,0.8,0.8),
           pfcol = rgb(0.6,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 13"))

# participant 14
participant14 <- HEI_component_scores_PercentMinMax[c(1:2, 16),]
radarchart(participant14, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.6,0.8,0.8),
           pfcol = rgb(0.6,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 14"))

# participant 15
participant15 <- HEI_component_scores_PercentMinMax[c(1:2, 17),]
radarchart(participant15, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.8,0.8,0.8),
           pfcol = rgb(0.6,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 15"))

# participant 16
participant16 <- HEI_component_scores_PercentMinMax[c(1:2, 18),]
radarchart(participant16, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,1,0.8,0.8),
           pfcol = rgb(0.6,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 16"))

# new screen (17 - 24)
# participant 17
participant17 <- HEI_component_scores_PercentMinMax[c(1:2, 19),]
radarchart(participant17, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.4,0.8,0.8),
           pfcol = rgb(0.8,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 17"))

# participant 18
participant18 <- HEI_component_scores_PercentMinMax[c(1:2, 20),]
radarchart(participant18, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.6,0.8,0.8),
           pfcol = rgb(0.8,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 18"))

# participant 19
participant19 <- HEI_component_scores_PercentMinMax[c(1:2, 21),]
radarchart(participant19, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.8,0.8,0.8),
           pfcol = rgb(0.8,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 19"))

# participant 20
participant20 <- HEI_component_scores_PercentMinMax[c(1:2, 22),]
radarchart(participant20, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,1,0.8,0.8),
           pfcol = rgb(0.8,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 20"))

# participant 21
participant21 <- HEI_component_scores_PercentMinMax[c(1:2, 23),]
radarchart(participant21, axistype=1, vlcex = 0.5,
           pcol = rgb(1,0.4,0.8,0.8),
           pfcol = rgb(1,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 21"))

# participant 22
participant22 <- HEI_component_scores_PercentMinMax[c(1:2, 24),]
radarchart(participant22, axistype=1, vlcex = 0.5,
           pcol = rgb(1,0.6,0.8,0.8),
           pfcol = rgb(1,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 22"))

# participant 23
participant23 <- HEI_component_scores_PercentMinMax[c(1:2, 25),]
radarchart(participant23, axistype=1, vlcex = 0.5,
           pcol = rgb(1,0.8,0.8,0.8),
           pfcol = rgb(1,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 23"))

# participant 24
participant24 <- HEI_component_scores_PercentMinMax[c(1:2, 26),]
radarchart(participant24, axistype=1, vlcex = 0.5,
           pcol = rgb(1,0.9,0.8,0.8),
           pfcol = rgb(1,0.9,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 24"))

# new screen (25 - 32)
# participant 25
participant25 <- HEI_component_scores_PercentMinMax[c(1:2, 27),]
radarchart(participant25, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.4,1,0.8),
           pfcol = rgb(0.8,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 25"))

# participant 26
participant26 <- HEI_component_scores_PercentMinMax[c(1:2, 28),]
radarchart(participant26, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.6,1,0.8),
           pfcol = rgb(0.8,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 26"))

# participant 27
participant27 <- HEI_component_scores_PercentMinMax[c(1:2, 29),]
radarchart(participant27, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.8,1,0.8),
           pfcol = rgb(0.8,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 27"))

# participant 28
participant28 <- HEI_component_scores_PercentMinMax[c(1:2, 30),]
radarchart(participant28, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,1,1,0.8),
           pfcol = rgb(0.8,1,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 28"))

# participant 29
participant29 <- HEI_component_scores_PercentMinMax[c(1:2, 31),]
radarchart(participant29, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.4,1,0.8),
           pfcol = rgb(0.6,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 29"))

# participant 30
participant30 <- HEI_component_scores_PercentMinMax[c(1:2, 32),]
radarchart(participant30, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.6,1,0.8),
           pfcol = rgb(0.6,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 30"))

# participant 31
participant31 <- HEI_component_scores_PercentMinMax[c(1:2, 33),]
radarchart(participant31, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.8,1,0.8),
           pfcol = rgb(0.6,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 31"))

# participant 32
participant32 <- HEI_component_scores_PercentMinMax[c(1:2, 34),]
radarchart(participant32, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,1,1,0.8),
           pfcol = rgb(0.6,1,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 32"))

# new screen (33 - 40)
# participant 33
participant33 <- HEI_component_scores_PercentMinMax[c(1:2, 35),]
radarchart(participant33, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.4,1,0.8),
           pfcol = rgb(0.4,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 33"))

# participant 34
participant34 <- HEI_component_scores_PercentMinMax[c(1:2, 36),]
radarchart(participant34, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.6,1,0.8),
           pfcol = rgb(0.4,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 34"))

# participant 35
participant35 <- HEI_component_scores_PercentMinMax[c(1:2, 37),]
radarchart(participant35, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.8,1,0.8),
           pfcol = rgb(0.4,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 35"))

# participant 36
participant36 <- HEI_component_scores_PercentMinMax[c(1:2, 38),]
radarchart(participant36, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,1,1,0.8),
           pfcol = rgb(0.4,1,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 36"))

# participant 37
participant37 <- HEI_component_scores_PercentMinMax[c(1:2, 39),]
radarchart(participant37, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.4,1,0.8),
           pfcol = rgb(0.2,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 37"))

# participant 38
participant38 <- HEI_component_scores_PercentMinMax[c(1:2, 40),]
radarchart(participant38, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.6,1,0.8),
           pfcol = rgb(0.2,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 38"))

# participant 39
participant39 <- HEI_component_scores_PercentMinMax[c(1:2, 41),]
radarchart(participant39, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.8,1,0.8),
           pfcol = rgb(0.2,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 39"))

# participant 40
participant40 <- HEI_component_scores_PercentMinMax[c(1:2, 42),]
radarchart(participant40, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,1,1,0.8),
           pfcol = rgb(0.2,1,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 40"))

# new screen (41)
# participant 41
participant41 <- HEI_component_scores_PercentMinMax[c(1:2, 43),]
radarchart(participant41, axistype=1, vlcex = 0.5,
           pcol = rgb(0,0.4,1,0.8),
           pfcol = rgb(0,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = paste("Participant 41"))







