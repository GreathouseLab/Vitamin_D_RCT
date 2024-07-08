# Ella von Dohlen 
# Completed as part of Honors Thesis Project SPRING 2024

# OBJECTIVE OF FILE: 
# Calculation of Diversity Measurements 
#   (alpha and beta divs, for diet and for microbiome)
# and Correlation tests between such measurements

library(ggtree)
library(abdiv)
library(picante)
library(dplyr)
library(tibble)
library(tidyr)
library(phyloseq)
library(purrr)
library(readxl)

setwd("/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/DietR_package")

dyd_gram_matrix <- read.delim('VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.dhydrt.ifc.txt')

# make the food IDs the rownames
dyd_gram_matrix1 <- dyd_gram_matrix[,-1]
row.names(dyd_gram_matrix1) <- dyd_gram_matrix[,1]
row.names(dyd_gram_matrix1) <- gsub(" ", "_", row.names(dyd_gram_matrix1))
# transpose to make variables the columns and ASAs the rows
dyd_gram_matrix2 <- t(dyd_gram_matrix1)

tree <- read.tree("VDMTdata/Foodtree/asa24_Items_f_id_s_m_QCed_4Lv.tree.nwk")
#plot(tree)

################################### FAITH'S ####################################
faiths <- pd(dyd_gram_matrix2, tree, include.root=TRUE)

# to convert ASA numbers to participant/day
user_ids <- read.delim('VDMTdata/asa24_Items_f_id.txt')
user_ids <- subset(user_ids, select = c(SampleID, StudyDay, UserName))

user_ids <- user_ids |>
  distinct() |>
  mutate(PartDay = paste(UserName, StudyDay, sep = '_'))

head(user_ids)

# to add study day and participant id to faiths:
faiths$SampleID <- rownames(faiths)
faiths <- full_join(faiths, user_ids, by = 'SampleID')

faiths <- faiths |> arrange(UserName, StudyDay)

#write.csv(faiths, '/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/faiths_diversity.csv')

################################## SHANNONS ##################################

shannons <- diversity(dyd_gram_matrix2, index = 'shannon')
shannons <- as.data.frame(shannons)
shannons$SampleID <- rownames(shannons)
shannons <- full_join(shannons, user_ids, by = 'SampleID')

################################## AITCHISONS ##################################

#aitchisons <- vegan::vegdist(dyd_gram_matrix2, method = 'robust.aitchison')

# to change entry ids to PartDay
dyd_gram_matrix3 <- as.data.frame(dyd_gram_matrix2)

dyd_gram_matrix3 <- dyd_gram_matrix3 |>
  mutate(SampleID = row.names(dyd_gram_matrix3)) # adds a column SampleID

user_ids <- user_ids |>
  select(SampleID, PartDay)

dyd_gram_matrix3 <- full_join(dyd_gram_matrix3, user_ids, by = 'SampleID')

# set PartDay as rowname, then delete SampleID and PartDay from dyd_gram_matrix3
rownames(dyd_gram_matrix3) <- dyd_gram_matrix3$PartDay

dyd_gram_matrix3 <- dyd_gram_matrix3 |>
  arrange(PartDay) |>
  select(-c(SampleID, PartDay)) |>
  as.matrix()

aitchisons2 <- vegan::vegdist(dyd_gram_matrix3, method = 'robust.aitchison')

aitchisons3 <- as.matrix(aitchisons2)

#write.csv(aitchisons3, '/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/aitchisons.csv')

###################### MEDIAN OF CONSECUTIVE DAYS OF FOOD #######################

# this takes the aitchinson's distances of just the consecutive days
aitch_df <- aitchisons3 %>%
  as.data.frame() %>%
  rownames_to_column("Sample1") %>%
  pivot_longer(-Sample1,names_to="Sample2",values_to="aitch") %>%
  filter(Sample1<Sample2) %>%
  mutate(Subject1=as.character(map(strsplit(Sample1,"_"),1)),
         Subject2=as.character(map(strsplit(Sample2,"_"),1)),
         Day1=as.numeric(substr(map(strsplit(Sample1,"_"),2), 1, 2)),
         Day2=as.numeric(substr(map(strsplit(Sample2,"_"),2), 1, 2))) %>%
  filter(Subject1==Subject2,Day1==Day2-1)

# take median value per person
medians_diet <- aitch_df |>
  group_by(Subject1) |>
  mutate(median_diet = median(as.numeric(aitch))) |>
  select(Subject1, median_diet) |>
  unique()

#################################### UNIFRAC ####################################

for_unifrac = phyloseq(
  otu_table(dyd_gram_matrix3,taxa_are_row=FALSE),
  tree)

unifrac <- UniFrac(for_unifrac, weighted=TRUE)
unifrac1 <- as.matrix(unifrac)

# considering only consecutive days
# this takes the unifrac's distances of just the consecutive days
unifrac1_df <- unifrac1 %>%
  as.data.frame() %>%
  rownames_to_column("Sample1") %>%
  pivot_longer(-Sample1,names_to="Sample2",values_to="unifrac") %>%
  filter(Sample1<Sample2) %>%
  mutate(Subject1=as.character(map(strsplit(Sample1,"_"),1)),
         Subject2=as.character(map(strsplit(Sample2,"_"),1)),
         Day1=as.numeric(substr(map(strsplit(Sample1,"_"),2), 1, 2)),
         Day2=as.numeric(substr(map(strsplit(Sample2,"_"),2), 1, 2))) %>%
  filter(Subject1==Subject2,Day1==Day2-1)

# take median value per person
medians_unifrac_diet <- unifrac1_df |>
  group_by(Subject1) |>
  mutate(median_unifrac_diet = median(as.numeric(unifrac))) |>
  select(Subject1, median_unifrac_diet) |>
  unique()

# should I look into taking the mean?
means_unifrac_diet <- unifrac1_df |>
  group_by(Subject1) |>
  mutate(median_unifrac_diet = mean(as.numeric(unifrac))) |>
  select(Subject1, median_unifrac_diet) |>
  unique()

# will also take the 4,5,6, & 11,12,13 diet unifracs to compoare against MB...?

############################# MICROBIOME DATA PREP #############################

# Ankan sent microbiome aitch distances --> AitchMicrobiome.csv
# inversed each of the three values and took median --> AitchMicrobiomeStability.csv

mb_aitch <- read.csv('/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/AitchMicrobiomeStability.csv')

mb_aitch <- mb_aitch |>
  mutate(Subject1 = gsub('VDMT0', 'VDMT', Subject)) |>
  select(Subject1, Cohort, Stability)

cohorts <- mb_aitch |>
  select(Subject1, Cohort)

# PREP WITH FOOD AITCH:
both_aitch <- full_join(medians_diet, mb_aitch, by = 'Subject1')

both_aitch <- both_aitch |>
  rename(Subject = Subject1,
         median_diet_aitch = median_diet,
         inv_aitch_mb = Stability)

# instead of median, I will take average, because there are only 3 values anyway
mb_aitch_tot <- read.csv('/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/AitchMicrobiome.csv')

mb_avg_aitch <- mb_aitch_tot |>
  mutate(inv_aitch = (1/value)) |>
  group_by(Subject.x) |>
  mutate(avg_inv_aitch = mean(inv_aitch)) |>
  distinct(avg_inv_aitch) |>
  mutate(Subject1 = gsub('VDMT0', 'VDMT', Subject.x)) |>
  ungroup() |>
  select('Subject1', 'avg_inv_aitch')

mean_aitch_both <- full_join(medians_diet, mb_avg_aitch, by = 'Subject1')
mean_aitch_both <- full_join(mean_aitch_both, cohorts, by = 'Subject1')

# PREP WITH UNIFRAC:
mb_dietUF <- full_join(medians_unifrac_diet, mb_aitch, by = 'Subject1')

mb_dietUF <- mb_dietUF |>
  rename(Subject = Subject1,
         inv_aitch_mb = Stability)

# CORRELATION BTWN MICROBIOME STABILITY (REVERSE AITCH) & DIET DIVERSITY (AITCH) #

ggplot(both_aitch, aes(median_diet_aitch, inv_aitch_mb, color = Cohort)) +
  geom_point(na.rm = TRUE)

cor.test(both_aitch$median_diet_aitch, both_aitch$inv_aitch_mb,
    method = "spearman", use = 'complete.obs')
# 0.002674013, p-value = 0.9869

## separate by cohort:
both_aitch_placebo <- both_aitch |>
  filter(Cohort == 'Placebo')
both_aitch_treatment <- both_aitch |>
  filter(Cohort == 'Treatment')

ggplot(both_aitch_placebo, aes(median_diet_aitch, inv_aitch_mb)) +
  geom_point(na.rm = TRUE)
ggplot(both_aitch_treatment, aes(median_diet_aitch, inv_aitch_mb)) +
  geom_point(na.rm = TRUE)

cor.test(both_aitch_placebo$median_diet_aitch, both_aitch_placebo$inv_aitch_mb,
         method = "spearman", use = 'complete.obs')
# 0.002823264, p-value = 0.992

cor.test(both_aitch_treatment$median_diet_aitch, both_aitch_treatment$inv_aitch_mb,
         method = "spearman", use = 'complete.obs')
# 0.01052632, p-value = 0.967


# CORRELATION BTWN MICROBIOME STABILITY (REVERSE AITCH *by mean*) & DIET DIVERSITY (AITCH) #

ggplot(mean_aitch_both, aes(median_diet, avg_inv_aitch)) +
  geom_point(na.rm = TRUE)

cor.test(mean_aitch_both$median_diet, mean_aitch_both$avg_inv_aitch,
         method = "spearman", use = 'complete.obs')

## separate by cohort:
both_aitch_mean_placebo <- mean_aitch_both |>
  filter(Cohort == 'Placebo')
both_aitch_mean_treatment <- mean_aitch_both |>
  filter(Cohort == 'Treatment')

ggplot(both_aitch_mean_placebo, aes(median_diet, avg_inv_aitch)) +
  geom_point(na.rm = TRUE)
ggplot(both_aitch_mean_treatment, aes(median_diet, avg_inv_aitch)) +
  geom_point(na.rm = TRUE)

cor.test(both_aitch_mean_placebo$median_diet, both_aitch_mean_placebo$avg_inv_aitch,
         method = "spearman", use = 'complete.obs')
# -0.05364201
cor.test(both_aitch_mean_treatment$median_diet, both_aitch_mean_treatment$avg_inv_aitch,
         method = "spearman", use = 'complete.obs')
# 0.02857143


###### CORRELATION BTWN MICROBIOME DIVERSITY & DIET DIVERSITY (BOTH AITCH) ######

# use mb_aitch_tot


# CORRELATION BTWN MICROBIOME STABILITY (REVERSE AITCH) & DIET DIVERSITY (UNIFRAC) #
# for food stability/DIVERSITY, unifrac is better than even robust aitch bc there are so many 0s so it only focuses on main food items, but unifrac uses tree

ggplot(mb_dietUF, aes(median_unifrac_diet, inv_aitch_mb, color = Cohort)) +
  geom_point(na.rm = TRUE)
# participant 21 doesn't have a MB stability value

cor.test(mb_dietUF$median_unifrac_diet, mb_dietUF$inv_aitch_mb,
         method = "spearman", use = 'complete.obs')
# -0.1937444 --> perhaps super weak negative association

# separated by cohort:
mb_dietUF_placebo <- mb_dietUF |>
  filter(Cohort == 'Placebo')
mb_dietUF_treatment <- mb_dietUF |>
  filter(Cohort == 'Treatment')

ggplot(mb_dietUF_placebo, aes(median_unifrac_diet, inv_aitch_mb)) +
  geom_point(na.rm = TRUE)
ggplot(mb_dietUF_treatment, aes(median_unifrac_diet, inv_aitch_mb)) +
  geom_point(na.rm = TRUE)

cor.test(mb_dietUF_placebo$median_unifrac_diet, mb_dietUF_placebo$inv_aitch_mb,
         method = "spearman", use = 'complete.obs')
# 0.002823264

cor.test(mb_dietUF_treatment$median_unifrac_diet, mb_dietUF_treatment$inv_aitch_mb,
         method = "spearman", use = 'complete.obs')
# -0.487218 --> negative association!! with a p-val of 0.0309

######## MS FROM MEAN VS UNIFRAC DIET DIV (MEDIAN OF CONS DAYS) ##########
DDuni_MSmean <- full_join(median_unifrac_diet, mb_avg_aitch, by = 'Subject1')
DDuni_MSmean <- full_join(DDuni_MSmean, cohorts, by = 'Subject1')

cor.test(DDuni_MSmean$median_unifrac_diet, DDuni_MSmean$avg_inv_aitch,
         method = "spearman", use = 'complete.obs')

# separated by cohort:
DDuni_MSmean_placebo <- DDuni_MSmean |>
  filter(Cohort == 'Placebo')
DDuni_MSmean_treatment <- DDuni_MSmean |>
  filter(Cohort == 'Treatment')

cor.test(DDuni_MSmean_placebo$median_unifrac_diet, DDuni_MSmean_placebo$avg_inv_aitch,
         method = "spearman", use = 'complete.obs')

cor.test(DDuni_MSmean_treatment$median_unifrac_diet, DDuni_MSmean_treatment$avg_inv_aitch,
         method = "spearman", use = 'complete.obs')

##### CORRELATION BTWN MICROBIOME DIVERSITY & DIET DIVERSITY (BOTH FAITHS) #####

mb_alphas <- read_xlsx('/Users/gabriellavondohlen/Desktop/Greathouse/VitaminDstudy/ALPHADIVERSIT.XLSX')
mb_alphas <- mb_alphas |>
  as.data.frame() |>
  mutate(Subject1 = gsub('VDMT0', 'VDMT', Subject),
         PartDay = paste(Subject1, Day, sep = '_'))

alphas_tot <- right_join(faiths, mb_alphas, by = 'PartDay')

ggplot(alphas_tot, aes(PD, faith)) +
  geom_point()

# 14 points were removed... 
cor.test(alphas_tot$PD, alphas_tot$faith, method = "spearman", use = 'complete.obs')

# separate by Cohort
alphas_tot_placebo <- alphas_tot |>
  filter(Cohort == 'Placebo')
alphas_tot_treatment <- alphas_tot |>
  filter(Cohort == 'Treatment')

cor.test(alphas_tot_placebo$PD, alphas_tot_placebo$faith,
         method = "spearman", use = 'complete.obs')
# -0.001617333
cor.test(alphas_tot_treatment$PD, alphas_tot_treatment$faith,
         method = "spearman", use = 'complete.obs')
# 0.2215438 * this is almost significant with a p-val of 0.06334

## SHANNONS 
cor.test(alphas_tot$PD, alphas_tot$shannon, method = "spearman", use = 'complete.obs')

cor.test(alphas_tot_placebo$PD, alphas_tot_placebo$shannon,
         method = "spearman", use = 'complete.obs')
# -0.235259 with p-val of 0.03228!
cor.test(alphas_tot_treatment$PD, alphas_tot_treatment$shannon,
         method = "spearman", use = 'complete.obs')
# -0.05867799

# REALLY I SHOULD COMPARE THE DAYS BEFORE AND THE CURRENT DAY
# to copy Ankan: correlate food faith's 4,5,6 with MB faith's day 7
#                correlate food faith's 11,12,13 with MB faith's day 14

present_days <- faiths |>
  group_by(UserName) |>
  mutate(all_days = paste(StudyDay, collapse = ',')) |>
  select(UserName, all_days) |>
  unique()

# to prep for the weighing decay avg, keeping only the relevant days
faiths_WDA <- faiths |>
  group_by(UserName) |>
  select(UserName, StudyDay, PD) |>
  spread(key = StudyDay, value = PD) |>
  as.data.frame() |>
  select(UserName, '4', '5', '6', '11', '12', '13') |>
  rename(day4 = '4', day5 = '5', day6 = '6',
         day11 = '11', day12 = '12', day13 = '13')

# to identify the NAs
table(faiths$UserName, faiths$StudyDay) # used to make the following lists

participants_miss1st_7 <- c('VDMT09', 'VDMT21', 'VDMT26', 'VDMT38')
participants_miss2nd_7 <- c('VDMT11', 'VDMT15', 'VDMT17', 'VDMT18',
                            'VDMT19', 'VDMT24', 'VDMT27', 'VDMT40')
participants_miss1st_14 <- c('VDMT04', 'VDMT10', 'VDMT15', 'VDMT17',
                             'VDMT38', 'VDMT41', 'VDMT42', 'VDMT43')
participants_miss2nd_14 <- c('VDMT07', 'VDMT35')
participants_miss3rd_14 <- c('VDMT11', 'VDMT18', 'VDMT21', 'VDMT36')

# to do the weighing decayed averages
faiths_WDA1 <- faiths_WDA |>
  mutate(day_7_wda = (day4 * (1/6)) + (day5 * (2/6)) + (day6 * (3/6))) |>
  mutate(day_7_wda1 = ifelse(UserName %in% participants_miss1st_7,
                            (day5 * (1/3)) + (day6 * (2/3)),
                            day_7_wda)) |>
  mutate(day_7_wda2 = ifelse(UserName %in% participants_miss2nd_7,
                             (day4 * (1/4)) + (day6 * (3/4)),
                             day_7_wda1)) |>
  mutate(day_14_wda = (day11 * (1/6)) + (day12 * (2/6)) + (day13 * (3/6))) |>
  mutate(day_14_wda1 = ifelse(UserName %in% participants_miss1st_14,
                              (day12 * (1/3)) + (day13 * (2/3)),
                              day_14_wda)) |>
  mutate(day_14_wda2 = ifelse(UserName %in% participants_miss2nd_14,
                             (day11 * (1/4)) + (day13 * (3/4)),
                             day_14_wda1)) |>
  mutate(day_14_wda3 = ifelse(UserName %in% participants_miss3rd_14,
                             (day11 * (1/3)) + (day12 * (2/3)),
                             day_14_wda2)) |>
  mutate(day_14_wda4 = ifelse(UserName == 'VDMT29',
                              day12, day_14_wda3)) |>
  select(UserName, day_7_wda2, day_14_wda4) |>
  rename(day7_wda_final = day_7_wda2,
         day14_wda_final = day_14_wda4)

faiths_WDA2 <- faiths_WDA1 |>
  gather(key = 'day', value = diet_faiths, 2:3) |>
  mutate(corresponding_day = as.numeric(gsub("\\D", "", day))) |>
  mutate(PartDay = paste(UserName, corresponding_day, sep = '_')) |>
  select(UserName, diet_faiths, PartDay)

# compare with MB data
alphas_tot_wda <- left_join(faiths_WDA2, mb_alphas, by = 'PartDay')

ggplot(alphas_tot_wda, aes(diet_faiths, faith)) +
  geom_point()

cor.test(alphas_tot_wda$diet_faiths, alphas_tot_wda$faith, method = "spearman", use = 'complete.obs')

# separate by Cohort
alphas_tot_wda_placebo <- alphas_tot_wda |>
  filter(Cohort == 'Placebo')
alphas_tot_wda_treatment <- alphas_tot_wda |>
  filter(Cohort == 'Treatment')

cor.test(alphas_tot_wda_placebo$diet_faiths, alphas_tot_wda_placebo$faith,
         method = "spearman", use = 'complete.obs')

cor.test(alphas_tot_wda_treatment$diet_faiths, alphas_tot_wda_treatment$faith,
         method = "spearman", use = 'complete.obs')

##### CORRELATION BTWN MICROBIOME STABILITY & AVG DIET DIVERSITY (FAITHS) #####

# average faiths from food across all days
faiths1 <- faiths |>
  group_by(UserName) |>
  mutate(averagePD_diet = mean(PD)) |>
  select(UserName, averagePD_diet) |>
  unique() |>
  rename(Subject1 = UserName)

mbSTAB_avgdietPD <- full_join(faiths1, mb_aitch, by = 'Subject1')

cor.test(mbSTAB_avgdietPD$averagePD_diet, mbSTAB_avgdietPD$Stability, method = "spearman", use = 'complete.obs')

# separate by Cohort
mbSTAB_avgdietPD_placebo <- mbSTAB_avgdietPD |>
  filter(Cohort == 'Placebo')
mbSTAB_avgdietPD_treatment <- mbSTAB_avgdietPD |>
  filter(Cohort == 'Treatment')

cor.test(mbSTAB_avgdietPD_placebo$averagePD_diet, mbSTAB_avgdietPD_placebo$Stability,
         method = "spearman", use = 'complete.obs')

cor.test(mbSTAB_avgdietPD_treatment$averagePD_diet, mbSTAB_avgdietPD_treatment$Stability,
         method = "spearman", use = 'complete.obs')

##### CORRELATION BTWN MICROBIOME STABILITY & WEIGHTED AVG DIET DIVERSITY (FAITHS) #####
# mb_aitch_tot vs faiths_WDA2 --> no bc mb_aitch_tot is comparing bwtn days 1 and 7, then 7 and 14, and 14 and 78, while faiths just has 7 and 14

######### DIET SHANNONS vs MB SHANNONS ########
alphas_shannons <- right_join(shannons, mb_alphas, by = 'PartDay') # food is shannons, mb is shannon

cor.test(alphas_shannons$shannons, alphas_shannons$shannon,
         method = "spearman", use = 'complete.obs')

alphas_shannons_placebo <- alphas_shannons |>
  filter(Cohort == 'Placebo')
alphas_shannons_treatment <- alphas_shannons |>
  filter(Cohort == 'Treatment')

cor.test(alphas_shannons_placebo$shannons, alphas_shannons_placebo$shannon,
         method = "spearman", use = 'complete.obs')

cor.test(alphas_shannons_treatment$shannons, alphas_shannons_treatment$shannon,
         method = "spearman", use = 'complete.obs')

######### DIET SHANNONS vs MB FAITHS ########

cor.test(alphas_shannons$shannons, alphas_shannons$faith,
         method = "spearman", use = 'complete.obs')

cor.test(alphas_shannons_placebo$shannons, alphas_shannons_placebo$faith,
         method = "spearman", use = 'complete.obs')

cor.test(alphas_shannons_treatment$shannons, alphas_shannons_treatment$faith,
         method = "spearman", use = 'complete.obs')
