# HEI Score Analysis


data <- read.csv(file="vitamin_d_microbiome_trial.results.csv", header=T, skip = 1)
colnames(data)
colnames(data)[276]
data[276]

data[1]   # no participant 39

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
HEI_component_scores <- data[, c(1, 277:289)]
colnames(HEI_component_scores)

ncol(HEI_component_scores)              # 14 (IDs plus 13 categories)
nrow(HEI_component_scores)              # 43
which(is.na(HEI_component_scores))      # 0

list_colnames <- c('ID', 'TotalVegetables', 'GreensBeans', 'TotalFruits', 'WholeFruits',
                   'WholeGrains', 'Dairy', 'TotalProteins', 'SeafoodPlantProteins',
                   'FattyAcids', 'Sodium', 'RefinedGrains', 'SaturatedFats', 'AddedSugars')

names(HEI_component_scores) <- c(list_colnames)
colnames(HEI_component_scores)

# calculate component scores as percentages- create df and populate with %s
# this process standardizes all categories, so the specific min and max are no longer significant
HEI_component_scores_percents <- data.frame(matrix(ncol = 14, nrow = 43))
colnames(HEI_component_scores_percents) <- c(list_colnames)

HEI_component_scores_percents$ID <- HEI_component_scores$ID
HEI_component_scores_percents$TotalVegetables <- ((HEI_component_scores$TotalVegetables) / 5) * 100
HEI_component_scores_percents$GreensBeans <- ((HEI_component_scores$GreensBeans) / 5) * 100
HEI_component_scores_percents$TotalFruits <- ((HEI_component_scores$TotalFruits) / 5) * 100
HEI_component_scores_percents$WholeFruits <- ((HEI_component_scores$WholeFruits) / 5) * 100
HEI_component_scores_percents$WholeGrains <- ((HEI_component_scores$WholeGrains) / 10) * 100
HEI_component_scores_percents$Dairy <- ((HEI_component_scores$Dairy) / 10) * 100
HEI_component_scores_percents$TotalProteins <- ((HEI_component_scores$TotalProteins) / 5) * 100
HEI_component_scores_percents$SeafoodPlantProteins <- ((HEI_component_scores$SeafoodPlantProteins) / 5) * 100
HEI_component_scores_percents$FattyAcids <- ((HEI_component_scores$FattyAcids) / 10) * 100
HEI_component_scores_percents$Sodium <- ((HEI_component_scores$Sodium) / 10) * 100
HEI_component_scores_percents$RefinedGrains <- ((HEI_component_scores$RefinedGrains) / 10) * 100
HEI_component_scores_percents$SaturatedFats <- ((HEI_component_scores$SaturatedFats) / 10) * 100
HEI_component_scores_percents$AddedSugars <- ((HEI_component_scores$AddedSugars) / 10) * 100
HEI_component_scores_percents

max(HEI_component_scores_percents[, 2:14]) # 100
min(HEI_component_scores_percents[, 2:14]) # 0

# add max and min for each category to the first 2 lines to % df
max_row <- c(rep(100, 14))
min_row <- c(rep(0, 14))

HEI_component_scores_PercentMinMax <- rbind(max_row, min_row, HEI_component_scores_percents)
HEI_component_scores_PercentMinMax

# make chart
dev.off(dev.list()["RStudioGD"])
par(mar = c(2, 2, 2, 2))
radarchart(HEI_component_scores_PercentMinMax[,2:14], axistype=1, vlcex = 0.8,
           cglcol = "grey", cglty = 1, axislabcol = "grey",
           caxislabels = seq(0,100,25), cglwd = 0.8,
           title = 'All Participants')

######################### INDIVIDUAL RADAR PLOT ###########################
# https://statisticsglobe.com/r-layout-function-arrange-plots/
# https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/

# splitting screen into 6 rows by 8 columns
par(mfrow=c(2, 4))

# THERE IS NO PARTICIPANT 18, 36, AND 39 IN THIS DATAs # there is no 39, we will add 18 and 36

# screen 1 (1 - 8)
# participant 1
participant1 <- HEI_component_scores_PercentMinMax[1:3,2:14]
radarchart(participant1, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.4,0.8,0.8),  # change per graph
           pfcol = rgb(0.2,0.4,0.8,0.5), # change per graph
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[3])

# participant 2
participant2 <- HEI_component_scores_PercentMinMax[c(1:2, 4),2:14]
radarchart(participant2, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.6,0.8,0.8),
           pfcol = rgb(0.2,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[4])

# participant 3
participant3 <- HEI_component_scores_PercentMinMax[c(1:2, 5),2:14]
radarchart(participant3, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.8,0.8,0.8),
           pfcol = rgb(0.2,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[5])

# participant 4
participant4 <- HEI_component_scores_PercentMinMax[c(1:2, 6),2:14]
radarchart(participant4, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,1,0.8,0.8),
           pfcol = rgb(0.2,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[6])

# participant 5
participant5 <- HEI_component_scores_PercentMinMax[c(1:2, 7),2:14]
radarchart(participant5, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.4,0.8,0.8),
           pfcol = rgb(0.4,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[7])

# participant 6
participant6 <- HEI_component_scores_PercentMinMax[c(1:2, 8),2:14]
radarchart(participant6, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.6,0.8,0.8),
           pfcol = rgb(0.4,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[8])

# participant 7
participant7 <- HEI_component_scores_PercentMinMax[c(1:2, 9),2:14]
radarchart(participant7, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.8,0.8,0.8),
           pfcol = rgb(0.4,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[9])

# participant 8
participant8 <- HEI_component_scores_PercentMinMax[c(1:2, 10),2:14]
radarchart(participant8, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,1,0.8,0.8),
           pfcol = rgb(0.4,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[10])


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
participant9 <- HEI_component_scores_PercentMinMax[c(1:2, 11),2:14]
radarchart(participant9, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.4,0.8,0.8),
           pfcol = rgb(0.4,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[11])

# participant 10
participant10 <- HEI_component_scores_PercentMinMax[c(1:2, 12),2:14]
radarchart(participant10, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.6,0.8,0.8),
           pfcol = rgb(0.4,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[12])

# participant 11
participant11 <- HEI_component_scores_PercentMinMax[c(1:2, 13),2:14]
radarchart(participant11, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.8,0.8,0.8),
           pfcol = rgb(0.4,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[13])

# participant 12
participant12 <- HEI_component_scores_PercentMinMax[c(1:2, 14),2:14]
radarchart(participant12, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,1,0.8,0.8),
           pfcol = rgb(0.4,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[14])

# participant 13
participant13 <- HEI_component_scores_PercentMinMax[c(1:2, 15),2:14]
radarchart(participant13, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.4,0.8,0.8),
           pfcol = rgb(0.6,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[15])

# participant 14
participant14 <- HEI_component_scores_PercentMinMax[c(1:2, 16),2:14]
radarchart(participant14, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.6,0.8,0.8),
           pfcol = rgb(0.6,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[16])

# participant 15
participant15 <- HEI_component_scores_PercentMinMax[c(1:2, 17),2:14]
radarchart(participant15, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.8,0.8,0.8),
           pfcol = rgb(0.6,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[17])

# participant 16
participant16 <- HEI_component_scores_PercentMinMax[c(1:2, 18),2:14]
radarchart(participant16, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,1,0.8,0.8),
           pfcol = rgb(0.6,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[18])

# new screen (17 - 24)
# participant 17
participant17 <- HEI_component_scores_PercentMinMax[c(1:2, 19),2:14]
radarchart(participant17, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.4,0.8,0.8),
           pfcol = rgb(0.8,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[19])

# participant 18
participant18 <- HEI_component_scores_PercentMinMax[c(1:2, 20),2:14]
radarchart(participant18, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.6,0.8,0.8),
           pfcol = rgb(0.8,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[20])

# participant 19
participant19 <- HEI_component_scores_PercentMinMax[c(1:2, 21),2:14]
radarchart(participant19, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.8,0.8,0.8),
           pfcol = rgb(0.8,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[21])

# participant 20
participant20 <- HEI_component_scores_PercentMinMax[c(1:2, 22),2:14]
radarchart(participant20, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,1,0.8,0.8),
           pfcol = rgb(0.8,1,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[22])

# participant 21
participant21 <- HEI_component_scores_PercentMinMax[c(1:2, 23),2:14]
radarchart(participant21, axistype=1, vlcex = 0.5,
           pcol = rgb(1,0.4,0.8,0.8),
           pfcol = rgb(1,0.4,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[23])

# participant 22
participant22 <- HEI_component_scores_PercentMinMax[c(1:2, 24),2:14]
radarchart(participant22, axistype=1, vlcex = 0.5,
           pcol = rgb(1,0.6,0.8,0.8),
           pfcol = rgb(1,0.6,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[24])

# participant 23
participant23 <- HEI_component_scores_PercentMinMax[c(1:2, 25),2:14]
radarchart(participant23, axistype=1, vlcex = 0.5,
           pcol = rgb(1,0.8,0.8,0.8),
           pfcol = rgb(1,0.8,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[25])

# participant 24
participant24 <- HEI_component_scores_PercentMinMax[c(1:2, 26),2:14]
radarchart(participant24, axistype=1, vlcex = 0.5,
           pcol = rgb(1,0.9,0.8,0.8),
           pfcol = rgb(1,0.9,0.8,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[26])

# new screen (25 - 32)
# participant 25
participant25 <- HEI_component_scores_PercentMinMax[c(1:2, 27),2:14]
radarchart(participant25, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.4,1,0.8),
           pfcol = rgb(0.8,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[27])

# participant 26
participant26 <- HEI_component_scores_PercentMinMax[c(1:2, 28),2:14]
radarchart(participant26, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.6,1,0.8),
           pfcol = rgb(0.8,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[28])

# participant 27
participant27 <- HEI_component_scores_PercentMinMax[c(1:2, 29),2:14]
radarchart(participant27, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,0.8,1,0.8),
           pfcol = rgb(0.8,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[29])

# participant 28
participant28 <- HEI_component_scores_PercentMinMax[c(1:2, 30),2:14]
radarchart(participant28, axistype=1, vlcex = 0.5,
           pcol = rgb(0.8,1,1,0.8),
           pfcol = rgb(0.8,1,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[30])

# participant 29
participant29 <- HEI_component_scores_PercentMinMax[c(1:2, 31),2:14]
radarchart(participant29, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.4,1,0.8),
           pfcol = rgb(0.6,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[31])

# participant 30
participant30 <- HEI_component_scores_PercentMinMax[c(1:2, 32),2:14]
radarchart(participant30, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.6,1,0.8),
           pfcol = rgb(0.6,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[32])

# participant 31
participant31 <- HEI_component_scores_PercentMinMax[c(1:2, 33),2:14]
radarchart(participant31, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,0.8,1,0.8),
           pfcol = rgb(0.6,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[33])

# participant 32
participant32 <- HEI_component_scores_PercentMinMax[c(1:2, 34),2:14]
radarchart(participant32, axistype=1, vlcex = 0.5,
           pcol = rgb(0.6,1,1,0.8),
           pfcol = rgb(0.6,1,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[34])

# new screen (33 - 40)
# participant 33
participant33 <- HEI_component_scores_PercentMinMax[c(1:2, 35),2:14]
radarchart(participant33, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.4,1,0.8),
           pfcol = rgb(0.4,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[35])

# participant 34
participant34 <- HEI_component_scores_PercentMinMax[c(1:2, 36),2:14]
radarchart(participant34, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.6,1,0.8),
           pfcol = rgb(0.4,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[36])

# participant 35
participant35 <- HEI_component_scores_PercentMinMax[c(1:2, 37),2:14]
radarchart(participant35, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,0.8,1,0.8),
           pfcol = rgb(0.4,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[37])

# participant 36
participant36 <- HEI_component_scores_PercentMinMax[c(1:2, 38),2:14]
radarchart(participant36, axistype=1, vlcex = 0.5,
           pcol = rgb(0.4,1,1,0.8),
           pfcol = rgb(0.4,1,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[38])

# participant 37
participant37 <- HEI_component_scores_PercentMinMax[c(1:2, 39),2:14]
radarchart(participant37, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.4,1,0.8),
           pfcol = rgb(0.2,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[39])

# participant 38
participant38 <- HEI_component_scores_PercentMinMax[c(1:2, 40),2:14]
radarchart(participant38, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.6,1,0.8),
           pfcol = rgb(0.2,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[40])

# no participant 39 data

# participant 40
participant40 <- HEI_component_scores_PercentMinMax[c(1:2, 41),2:14]
radarchart(participant40, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,0.8,1,0.8),
           pfcol = rgb(0.2,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[41])

# participant 41
participant41 <- HEI_component_scores_PercentMinMax[c(1:2, 42),2:14]
radarchart(participant41, axistype=1, vlcex = 0.5,
           pcol = rgb(0.2,1,1,0.8),
           pfcol = rgb(0.2,1,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[42])

# new screen (42-44)
# participant 42
participant42 <- HEI_component_scores_PercentMinMax[c(1:2, 43),2:14]
radarchart(participant42, axistype=1, vlcex = 0.5,
           pcol = rgb(0,0.4,1,0.8),
           pfcol = rgb(0,0.4,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[43])

# participant 43
participant43 <- HEI_component_scores_PercentMinMax[c(1:2, 44),2:14]
radarchart(participant43, axistype=1, vlcex = 0.5,
           pcol = rgb(0,0.6,1,0.8),
           pfcol = rgb(0,0.6,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[44])

# participant 44
participant44 <- HEI_component_scores_PercentMinMax[c(1:2, 45),2:14]
radarchart(participant44, axistype=1, vlcex = 0.5,
           pcol = rgb(0,0.8,1,0.8),
           pfcol = rgb(0,0.8,1,0.5),
           plwd = 4 ,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0,100,25),
           cglwd = 0.8,
           title = HEI_component_scores_PercentMinMax$ID[45])


################### AVERAGE OF ALL PARTICIPANTS RADAR PLOT ###################

tot_avgs <- colMeans(HEI_component_scores_PercentMinMax[,2:14])

max_13_row <- c(rep(100, 13))
min_13_row <- c(rep(0, 13))

HEI_component_scores_tot_avgs <- rbind(max_13_row, min_13_row, tot_avgs)
HEI_component_scores_tot_avgs_df <- as.data.frame(HEI_component_scores_tot_avgs)

dev.off(dev.list()["RStudioGD"])
par(mar = c(2, 2, 2, 2))

radarchart(HEI_component_scores_tot_avgs_df, axistype=1, vlcex = 0.8,
           cglcol = "grey", cglty = 1, axislabcol = "grey",
           caxislabels = seq(0,100,25), cglwd = 0.8,
           title = paste("Average of all HEI Component Scores"))


################### AVERAGES BY TREATMENT GROUP RADAR PLOT ###################

# using treatment data
library(readxl)
supplements <- read_xlsx("Randomization.xlsx")
colnames(supplements)
names(supplements)[1] <- 'ID'

# reformat IDs of of supplements from VDMT001 to VDMT00001
num_IDs <- nrow(supplements)

for (i in 1:num_IDs){
  new_string <- gsub("^(.{4})(.*)$",
                     "\\100\\2",
                     supplements$ID[i])
  supplements$ID[i] <- new_string
}

supplements$ID

# joining HEI scores with treatment designation
library(dplyr)
HEI_component_scores_trtmt <- full_join(HEI_component_scores_PercentMinMax, supplements, by="ID")

# get rid of last two rows full of NAs representing participants 18 and 36
HEI_component_scores_trtmt_full <- HEI_component_scores_trtmt[1:43,]


# PLOTTING VITAMIN D TREATMENT GROUP
vitd_HEI_component_scores <- subset(HEI_component_scores_trtmt_full, Cohort == "Treatment")

vitd_avgs <- colMeans(vitd_HEI_component_scores[,2:14])
vitd_avgs_minmax <- rbind(max_13_row, min_13_row, vitd_avgs)
vitd_avgs_minmax_df <- as.data.frame(vitd_avgs_minmax)

dev.off(dev.list()["RStudioGD"])
par(mar = c(2, 2, 2, 2))

radarchart(vitd_avgs_minmax_df, axistype=1, vlcex = 0.8,
           cglcol = "grey", cglty = 1, axislabcol = "grey",
           caxislabels = seq(0,100,25), cglwd = 0.8,
           title = paste("Average of all Treatment Component Scores"))


# PLOTTING PLACEBO TREATMENT GROUP
placebo_HEI_component_scores <- subset(HEI_component_scores_trtmt_full, Cohort == "Placebo")

placebo_avgs <- colMeans(placebo_HEI_component_scores[,2:14])
placebo_avgs_minmax <- rbind(max_13_row, min_13_row, placebo_avgs)
placebo_avgs_minmax_df <- as.data.frame(placebo_avgs_minmax)

dev.off(dev.list()["RStudioGD"])
par(mar = c(2, 2, 2, 2))

radarchart(placebo_avgs_minmax_df, axistype=1, vlcex = 0.8,
           cglcol = "grey", cglty = 1, axislabcol = "grey",
           caxislabels = seq(0,100,25), cglwd = 0.8,
           title = paste("Average of all Placebo Component Scores"))

# add 18 and 36 from new data DONE
# new repo for food tree
# inversed radar charts??     DONE
