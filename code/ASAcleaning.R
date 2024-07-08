# Ella von Dohlen 
# last updated: 9/29/2023
# objective: to make a cleaned ASA items dataset 

library(xlsx)

sudilis_edits <- read.xlsx(file = "EDITED VDMT_2022-07-07_81282_Totals (1) (version 1).xlsx", sheetIndex = 1, header = T)

DaysOfStudy <- read.xlsx(file = "CrossRefSudili.xlsx", sheetIndex = 1, header = T)

colnames(DaysOfStudy)
DaysOfStudy <- subset(DaysOfStudy, select = c(RecallRecId, Ella.s.assignments))

colnames(DaysOfStudy) <- c("RecallRecId", "StudyDay")

cleanedASA <- full_join(DaysOfStudy, sudilis_edits, by = "RecallRecId")

# look for NAs
which(is.na(cleanedASA$RecallRecId))
head(cleanedASA$RecallRecId[363])

rowSums(is.na(cleanedASA))
ncol(cleanedASA)
# row 364 has all NAs --> delete row

cleanedASA <- cleanedASA[-364,]

# save
write.csv(cleanedASA, "cleanedASA.csv")

# on Sept 29, 2023, I changed the title of this to cleanedASA_totals.csv (and there's also an excel cleanedASA_totals.xlsx)
