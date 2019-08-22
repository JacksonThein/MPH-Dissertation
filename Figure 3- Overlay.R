#Import Datasets
WorkingSpreadsheet <- read.csv("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Analyses/Tree_of_Life/WS10KB.csv", comment.char="#")
View(WorkingSpreadsheet)
WS<-WorkingSpreadsheet
rm(WorkingSpreadsheet)

MLST <- read.csv("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Originals/From_Bryan/Population_structrue/MLST/mlst_1233tx.csv")
View(MLST)

BAPS <- read.delim("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Analyses/Tree_of_Life/Originals/BAPS/itol_hierbaps_run2_max200_baps3.txt", header=FALSE)
View(BAPS)

#Redefine BAPS Variable Names
BAPS$Genome<-BAPS$V1
BAPS$Code<-BAPS$V2
BAPS<-BAPS[,-1]
BAPS<-BAPS[,-1]

#Remove Leading R
WS$Iso1 <- gsub("^R_", "", WS$Iso1)
WS$Iso2 <- gsub("^R_", "", WS$Iso2)

#Match MLST to Genotype
WS$Iso1MLST.ST<-vlookup_df(WS$Iso1,MLST,result_column = 2, lookup_column = 1)
WS$Iso2MLST.ST<-vlookup_df(WS$Iso2,MLST,result_column = 2, lookup_column = 1)

#Match BAPS to Genotype
WS$Iso1BAPS<-vlookup_df(WS$Iso1,BAPS,result_column = 2, lookup_column = 1)
WS$Iso2BAPS<-vlookup_df(WS$Iso2,BAPS,result_column = 2, lookup_column = 1)

#Calculate Dashed Line if Different Compartments
WS$SameSource<-ifelse(WS$Iso1Source==WS$Iso2Source,TRUE,FALSE)
WS$Style<-factor(ifelse(WS$SameSource==FALSE,"normal","normal"))

#Different Color if Different MLST Type
WS$Color<-factor(ifelse(WS$SameSource==FALSE,"#E64B35FF","#4DBBD5FF"))

#Different Line Width if Different BAPS Group
WS$Weight<-ifelse((WS$Iso1BAPS)==(WS$Iso2BAPS), "3","3")

#Prepare to Export- Delete Unnecessary Columns
WSEx<-WS[,-4]
WSEx<-WSEx[,-4]
WSEx<-WSEx[,-4]
WSEx<-WSEx[,-4]
WSEx<-WSEx[,-4]
WSEx<-WSEx[,-4]
WSEx<-WSEx[,-4]
WSEx<-WSEx[,-6]
WSEx<-WSEx[,-7]
WSEx<-WSEx[,-7]
WSEx<-WSEx[,-7]
WSEx$CombinationB<-WSEx$Combination
WSEx<-WSEx[,-3]
WSEx$Combination<-WSEx$CombinationB
WSEx<-WSEx[,-6]

#Merge Columns for Label Column
WSEx$Label<-paste(WSEx$Combination,WS$Iso1Source)
WSEx$Label<-paste(WSEx$Label,WS$Iso2Source)
WSEx<-WSEx[,-6]


#Export as Tab Separated
write.table(WSEx, file = "10KBWS.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE)

#Retriving Colors for Above
library(ggsci)
mypal<-pal_npg("nrc")(9)
mypal
library("scales")
show_col(mypal)
