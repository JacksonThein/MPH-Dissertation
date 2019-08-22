##### START WORKING SPREADSHEET SCRIPT #####

#Import Datasets
WSMax <- read.csv("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Analyses/Tree_of_Life/Maximum/WSMax.csv")
WS<-WSMax
rm(WSMax)

MLST <- read.csv("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Originals/From_Bryan/Population_structrue/MLST/mlst_1233tx.csv")

BAPS <- read.delim("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Analyses/Tree_of_Life/Originals/BAPS/itol_hierbaps_run2_max200_baps3.txt", header=FALSE)

#Redefine BAPS Variable Names
BAPS$Genome<-BAPS$V1
BAPS$Code<-BAPS$V2
BAPS$V1<-NULL
BAPS$V2<-NULL

#Remove Leading R in Working Spreadsheet
WS$Iso1 <- gsub("^R_", "", WS$Iso1)
WS$Iso2 <- gsub("^R_", "", WS$Iso2)

#Install and Download "expss" package to have excel and SPSS-like commands avaialble for use in R
install.packages(expss)
library(expss)

#Match MLST to Genotype
WS$Iso1MLST<-vlookup_df(WS$Iso1,MLST,result_column = 2, lookup_column = 1)
WS$Iso2MLST<-vlookup_df(WS$Iso2,MLST,result_column = 2, lookup_column = 1)

#Match BAPS to Genotype
WS$Iso1BAPS<-vlookup_df(WS$Iso1,BAPS,result_column = 2, lookup_column = 1)
WS$Iso2BAPS<-vlookup_df(WS$Iso2,BAPS,result_column = 2, lookup_column = 1)

#Calculate Dashed Line if Different Compartments
WS$Style<-factor(ifelse(WS$SameSource==FALSE,"dashed","normal"))

#Different Color if Different MLST Type
WS$Color<-ifelse((WS$Iso1MLST)==(WS$Iso2MLST),"rgb(16,208,115)","rgb(165,76,3)")

#Different Line Width if Different BAPS Group
WS$Weight<-ifelse((WS$Iso1BAPS)==(WS$Iso2BAPS), "3","5")

#Remove Line Column
WS<-WS[,-13]

#Remove Width Column
WS<-WS[,-11]

#Make Columns for Same MLST and Same BAPS Proportion Analysis 
WS$SameMLST<-ifelse(WS$Iso1MLST==WS$Iso2MLST,TRUE,FALSE)
WS$SameBAPS<-ifelse(WS$Iso1BAPS==WS$Iso2BAPS,TRUE,FALSE)
WS$SameSourceIndicator<-ifelse(WS$SameSource==TRUE,1,0)
WS$SameMLSTIndicator<-ifelse(WS$SameMLST==TRUE,1,0)
WS$SameBAPSIndicator<-ifelse(WS$SameBAPS==TRUE,1,0)
WS$SameSourceSameMLST<-as.factor(WS$SameSourceIndicator+WS$SameMLSTIndicator)


#Summary for Proportions of Intercompartment, Inter-MLST, Inter-BAPS, Inter
summary(WS$SameSource)
summary(WS$SameMLST)
summary(WS$SameBAPS)
summary(WS$SameSourceSameMLST)

#Import Livestock Source Information
`LivestockSource` <- read.csv("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Originals/From_Bryan/STARCS_Metadata/for_itol_1233tx_20190325.csv", header=FALSE)
LivestockSource<-`LivestockSource`

#Change Variable Names
install.packages(tidyverse)
library(tidyverse)
colnames(LivestockSource)
names(LivestockSource)[1]<-"Genome"
names(LivestockSource)[2]<-"Color"
names(LivestockSource)[3]<-"Source"

#Change Source Name to Lowercase
LivestockSource$Source<-tolower(LivestockSource$Source)
LivestockSource$Source<-factor(LivestockSource$Source)
summary(LivestockSource$Source)

#Add in Specific Sourece Information
WS$Iso1SpecificSource<-vlookup_df(WS$Iso1,LivestockSource,result_column = 3, lookup_column = 1)
WS$Iso2SpecificSource<-vlookup_df(WS$Iso2,LivestockSource,result_column = 3, lookup_column = 1)

#Make Source Combination Column
WS$SpecificSource<-factor(paste(WS$Iso1SpecificSource$Source,WS$Iso2SpecificSource$Source,sep = "-"))
summary(WS$SpecificSource)
WS$SpecificSource<-as.character(WS$SpecificSource)
#6 Rows- Cattle
WS$SpecificSource[WS$SpecificSource == "companion-cattle"] <- "cattle-companion"
WS$SpecificSource[WS$SpecificSource == "herbs-cattle"] <- "cattle-herbs"
WS$SpecificSource[WS$SpecificSource == "human-cattle"] <- "cattle-human"
WS$SpecificSource[WS$SpecificSource == "meat-cattle"] <- "cattle-meat"
WS$SpecificSource[WS$SpecificSource == "pig-cattle"] <- "cattle-pig"
WS$SpecificSource[WS$SpecificSource == "poultry-cattle"] <- "cattle-poultry"
#5 Rows- Human
WS$SpecificSource[WS$SpecificSource == "companion-human"] <- "human-companion"
WS$SpecificSource[WS$SpecificSource == "herbs-human"] <- "human-herbs"
WS$SpecificSource[WS$SpecificSource == "meat-human"] <- "human-meat"
WS$SpecificSource[WS$SpecificSource == "pig-human"] <- "human-pig"
WS$SpecificSource[WS$SpecificSource == "poultry-human"] <- "human-poultry"
#4 Rows- Meat
WS$SpecificSource[WS$SpecificSource == "companion-meat"] <- "meat-companion"
WS$SpecificSource[WS$SpecificSource == "herbs-meat"] <- "meat-herbs"
WS$SpecificSource[WS$SpecificSource == "pig-meat"] <- "meat-pig"
WS$SpecificSource[WS$SpecificSource == "poultry-meat"] <- "meat-poultry"
#3 Rows- Pig
WS$SpecificSource[WS$SpecificSource == "companion-pig"] <- "pig-companion"
WS$SpecificSource[WS$SpecificSource == "herbs-pig"] <- "pig-herbs"
WS$SpecificSource[WS$SpecificSource == "poultry-pig"] <- "pig-poultry"
WS$SpecificSource<-as.factor(WS$SpecificSource)

#Rename Repeated Combination
WS$Combination<-as.character(WS$Combination)
WS$Combination[WS$Combination == "aac6Ibcr-aac6Ibcr-blaOXA1-catB3"] <- "aac6Ibcr-blaOXA1-catB3"
WS$Combination<-as.factor(WS$Combination)

##### END  WORKING SPREADSHEET SCRIPT #####

library(readr)
fmeta <- read_delim("OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Originals/From_Bryan/STARCS_Metadata/metadata2id_1233tx.20190219.tsv", 
                    +     "\t", escape_double = FALSE, trim_ws = TRUE)
summary(fmeta$host2)
fmeta$host2[fmeta$host2 == "organ"] <- "meat"
meatmeta<-subset(fmeta, fmeta$host2=="meat")
meatmeta$host<-as.factor(meatmeta$host)
summary(meatmeta$host)

