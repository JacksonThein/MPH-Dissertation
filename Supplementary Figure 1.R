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

#####  START METADATA SPREADSHEET SCRIPT #####

#Metadata File Creation
#Import Dataset
Meta <- read.csv("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Analyses/RStudio/General Metadata/for_itol_1233tx_20190325.csv", header=FALSE)
View(Meta)

#Remove Color Variable
Meta<-Meta[,-2]

#New Variables for Genome and Source
Meta$Genome<-Meta$V1
Meta$Source<-Meta$V3

#Remove Original Variables
Meta<-Meta[,-1]
Meta<-Meta[,-1]

#Remove Blank Row
Meta$Genome<-as.character(Meta$Genome)
Meta<-Meta[-c(1263),]

#Combine Names of Sources
summary(Meta$Source)
Meta$Source<-as.character(Meta$Source)
Meta$Source[Meta$Source == "Cattle"] <- "cattle"
Meta$Source[Meta$Source == "Herbs"] <- "herbs"
Meta$Source[Meta$Source == "Human"] <- "human"
Meta$Source[Meta$Source == "Pig"] <- "pig"
Meta$Source[Meta$Source == "Poultry"] <- "poultry"
Meta$Source<-factor(Meta$Source)
summary(Meta$Source)

#Install and Download "expss" package to have excel and SPSS-like commands avaialble for use in R
install.packages(expss)
library(expss)

#Add MLST to Metadata File
MLST <- read.csv("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Originals/From_Bryan/Population_structrue/MLST/mlst_1233tx.csv")
Meta$MLST<-vlookup_df(Meta$Genome,MLST,result_column = 2, lookup_column = 1)
#Summarize MLST
summary(Meta$MLST)

#Add  BAPS to Metadata File
BAPS <- read.delim("~/OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Analyses/Tree_of_Life/Originals/BAPS/itol_hierbaps_run2_max200_baps3.txt", header=FALSE)
View(BAPS)

#Redefine BAPS Variable Names
BAPS$Genome<-BAPS$V1
BAPS$Code<-BAPS$V2
BAPS<-BAPS[,-1]
BAPS<-BAPS[,-1]
BAPS$Code<-factor(BAPS$Code)

#Add Baps to Metadata File
Meta$BAPS<-vlookup_df(Meta$Genome,BAPS,result_column = 2, lookup_column = 1)
summary(Meta$BAPS)

#Remove Values Without MLST and  BAPS Data
Meta<-subset(Meta, Meta$MLST!="NA")
summary(Meta$MLST)

##### END METATDATA SPREADSHEET SCRIPT #####

WS<-subset(WS, WS$SpecificSource=="cattle-cattle" | WS$SpecificSource=="cattle-human" |WS$SpecificSource=="cattle-poultry"| WS$SpecificSource=="human-human" | WS$SpecificSource=="human-poultry" | WS$SpecificSource=="poultry-poultry")

#Genes Invovled with Human-Human, Human-Livestock, Livestock-Livesstock
step1<-WS$Iso1
step2<-WS$Iso1BAPS$Code
WS$Iso1SpecificSource$Source<-as.character(WS$Iso1SpecificSource$Source)
step3<-WS$Iso1SpecificSource$Source
step4<-as.matrix(cbind(step1,step2,step3))
step5<-WS$Iso2
step6<-WS$Iso2BAPS$Code
WS$Iso2SpecificSource$Source<-as.character(WS$Iso2SpecificSource$Source)
step7<-WS$Iso2SpecificSource$Source
step8<-as.matrix(cbind(step5,step6,step7))
merged<-as.data.frame(rbind(step4,step8))
mergedunique<-unique(merged)
summary(mergedunique)

#Rename Variables
mergedunique$Genome<-mergedunique$step1
mergedunique$step1<-NULL
mergedunique$BAPS<-mergedunique$step2
mergedunique$step2<-NULL
mergedunique$SpecificSource<-mergedunique$step3
mergedunique$step3<-NULL

#Rename Animal Names for Plot
mergedunique$SpecificSource<-as.factor(mergedunique$SpecificSource)
summary(mergedunique$SpecificSource)
mergedunique$SpecificSource<-as.character(mergedunique$SpecificSource)
mergedunique$SpecificSource[mergedunique$SpecificSource == "cattle"] <- "cattle (n=45)"
mergedunique$SpecificSource[mergedunique$SpecificSource == "human"] <- "human (n=180)"
mergedunique$SpecificSource[mergedunique$SpecificSource == "poultry"] <- "poultry (n=59)"


#Plot
library(forcats)
mergedunique$SpecificSource<-fct_relevel(mergedunique$SpecificSource, "cattle (n=45)", "human (n=180)", "poultry (n=59)")
mergedunique$BAPS<-fct_infreq(mergedunique$BAPS)
mergedplotgenomes<-ggplot(mergedunique, aes(x=mergedunique$BAPS, group=mergedunique$SpecificSource)) + geom_bar(aes(fill=mergedunique$SpecificSource,y = (..prop..*100))) + coord_flip() + 
  labs(title="Isolates per BAPS Group", x="BAPS Group", y="Relative Isolate Frequency (%)") + 
  theme(plot.title=element_text(size=0, hjust=.5),
        axis.text.x=element_text(size=24),
        axis.title.x=element_text(size=24, face="bold"),
        axis.text.y=element_text(size=24),
        axis.title.y=element_text(size=24, face="bold"), 
        strip.text.x =element_text(size = 32),  legend.position = "none", legend.title = element_blank()) 
library(ggsci)
facetplot<- mergedplotgenomes + facet_grid(. ~ SpecificSource) + scale_fill_npg() 
print(mergedplotgenomes)
print(facetplot)

