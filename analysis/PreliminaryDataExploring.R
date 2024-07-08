# Ella von Dohlen
# Spring 2022
# Preliminary Data Exploring


# Data Cleaning

install.packages("ggplot2")
library(ggplot2)

data <- read.csv(file="Code book-VDMT.csv", header=T)
VitD_data <- read.csv(file= "vitaminDlevels.csv", header = T)

colnames(data)

# COUNT NAs ?

# CREATE SUBSETS OF DATA
ID <- data[,1]
generalhealth_data <- data[,2:37]
perceivedstress_data <- data[,38:67]
hospitaldepanx_data <- data[,68:83]
undergradstress_data <- data[,84:135]
who_data <- data[,136:157]
stoolcollectionlog1_data <- data[,158:164]
stoolcollectionlog2_data <- data[,165:171]
bodycomp1_data <- data[,172:184]
bodycomp2_data <- data[,253:265]
demographics_data <- data[,198:229]
globalphysicalactivity_data <- data[,230:245]
uvskinrisk_data <- data[,246:250]
sleep_data <- data[,251:252]

colnames(demographics_data)
data[,229]

# LOOK AT OUTLIERS ACCORDING TO SUBSETS ?

#par(mar = c(1, 1, 1, 1))
#plot(generalhealth_data)

# BOXPLOTS PER CATEGORIES #########################################
# par(mar = c(bottom, left, top, right)

# GENERAL HEALTH
colnames(generalhealth_data)

plot(generalhealth_data$GH, generalhealth_data$GH_YEARAGO,
     main = "Comparing General Health Scores Between Years",
     xlab = "Current Score",
     ylab = "Score from One Year Ago")

generalhealth_dailyactivity_data <- generalhealth_data[,3:12]

boxplot(generalhealth_dailyactivity_data, col = rep(2:8),
        main = "Daily Acitivity Scores",
        xlab = "Types of Activities",
        ylab = "Score")

# to estimate an overview, I summed up these daily activity scores
sumDailyActivity_data <- rowSums(generalhealth_data[,3:12])
length(sumDailyActivity_data) # 47
# to compare to vit D levels, i omitted the general health data
# of individuals without vitamin d levels
sumDailyActivity_data <- sumDailyActivity_data[c(1:38,41:44)]
length(sumDailyActivity_data) # 42

length(VitD_data$VITD.Baseline) # 42

old.par <- par(mar = c(4.5, 4.5, 4.5, 4.5))
plot(sumDailyActivity_data, VitD_data$VITD.Baseline,
     main = "Daily Activity Sum vs. Vitamin D Levels",
     xlab = "Limitations on Daily Activity",
     ylab = "Baseline Vitamin D Levels")
# maybe look into misanswers

generalhealth_healthlimits_data <- generalhealth_data[,13:19]
boxplot(generalhealth_healthlimits_data,
        main = "Physical and Emotional Problems Limiting Activity",
        xlab = "Daily Activity Type",
        ylab = "Limitations on Activity",
        col = rep(2:4))

generalhealth_feelings_data <- generalhealth_data[,23:31]
boxplot(generalhealth_feelings_data,
        main = "Energetic Feelings",
        xlab = "Feelings",
        ylab = "Frequency (from all-the-time, 1, to never, 6)",
        col = rep(2:4))

# PERCEIVED STRESS

perceivedstress_data
nrow(perceivedstress_data) # 47

# for now i'll omit all rows with any NAs, although i'd like to come back and only delete rows with all NAs!
# what i will do is separate each variable into their own dataframe (with all three sets), and then get rid of NAs
#perceivedstress_data <- na.omit(perceivedstress_data)
#nrow(perceivedstress_data) # 39

# to rename the second set of data to have '2' attached instead of .1
names(perceivedstress_data)[11] <- "UPSET2"
names(perceivedstress_data)[12] <- "NOCONTROL2"
names(perceivedstress_data)[13] <- "STRESSED2"
names(perceivedstress_data)[14] <- "CONFIDENT2"
names(perceivedstress_data)[15] <- "YOURWAY2"
names(perceivedstress_data)[16] <- "NOCOPE2"
names(perceivedstress_data)[17] <- "IRR_CONTROL2"
names(perceivedstress_data)[18] <- "TOT2"
names(perceivedstress_data)[19] <- "ANGRY2"
names(perceivedstress_data)[20] <- "OVERCOME2"

colnames(perceivedstress_data)

perceivedstress_upset_data <- perceivedstress_data[, c(1,11,21)]
perceivedstress_upset_data <- na.omit(perceivedstress_upset_data)
perceivedstress_upset1_data <- c(perceivedstress_upset_data$UPSET,
                                 perceivedstress_upset_data$UPSET2,
                                 perceivedstress_upset_data$UPSET3)
perceivedstress_upset1_data
y_upset <- rep(1:3, each = 42)
index <- rep(1:42, times = 3)
perceivedstress_upset_graphdata <- cbind(perceivedstress_upset1_data, y_upset, index)

plot(y_upset, perceivedstress_upset1_data)

# in order to see the lines
# ggplot(perceivedstress_upset_graphdata, aes(x = perceivedstress_upset1_graphdata,
#                                             y = y_upset)
                                            #col = index)) + geom_line()

# completeTOTAL_STRESS <- na.omit(data$TOTAL_STRESS)
# data$TOTAL_STRESS


############################################################

