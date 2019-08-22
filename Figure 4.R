FWPrportions <- read_excel("OneDrive - University of Edinburgh/Dissertation/Thein -Dissertation Shared Folder/Analyses/RStudio/Contig_Combinations/Network Analysis/Maxmimum/Spreadsheets/FWPrportions.xlsx", 
                           +     col_types = c("text", "text", "blank", 
                                               +         "blank", "blank", "numeric", "numeric", 
                                               +         "numeric"))

install.packages(reshape2)
library(reshape2)
DF<-melt(FWPrportions, id.vars = c("Combination", "Same Source"))
DF$SameSource<-DF$`Same Source`

inter<-subset(DF, SameSource== "Intercompartment (n=267)")
intra<-subset(DF, SameSource== "Intracompartment (n=412)")
inter$`Same Source`<-NULL
inter$SameSource<-NULL
intra$`Same Source`<-NULL
intra$SameSource<-NULL
library(ggplot2)
library(ggsci)
library(dplyr)
library(tidyr)
library(scales)
library(forcats)

#Intracompartment Plot
intra$Combination<-as.factor(intra$Combination)
intra$Combination<-fct_relevel(intra$Combination,"aac6Ibcr-blaOXA1-catB3", "aadA5-dfrA17-mphA-sul1", "blaTEM1B", "blaCTXM15", "tetB", "blaCTXM1-mphA", "mphA", "catA1", "blaCTXM1", "tetA", "aadA5-dfrA17-sul1", "blaTEM52C", "ermB", "blaCTXM1-sul2", "blaCMY2", "sul2", "blaTEM52B", "aph3Ib-aph6Id", "aadA1-cmlA1-sul3", "mphB", "aadA1-aadA2-cmlA1-dfrA12-sul3", "mcr11", "dfrA5", "catA1-tetA", "aph3Ib-aph6Id-sul2", "aph3Ib-aph6Id-blaTEM1B-sul2", "lnuG", "blaCTXM65", "aadA5-catA1-dfrA17", "dfrA14-mphA", "aadA5-dfrA17-sul2", "aadA5-dfrA17", "aadA1-dfrA1-sul1", "aac3IV-aph4Ia", "tetD", "sul2-tetA", "blaSHV12", "aadA1-dfrA1", "tetM", "sul3", "sul1", "qnrS1", "floR-tetA", "floR", "dfrA14", "catA1-dfrA14-mphA", "catA1-dfrA12", "blaTEM1B-tetD", "blaTEM1A", "blaCTXM27", "blaCTXM2", "blaCTXM15-blaTEM1B-qnrS1-tetA", "blaCTXM15-blaTEM1B", "blaCTXM14", "blaCTXM1-blaTEM1B", "aph3Ib", "ant3Ia-dfrA1-mphB-sul1", "aadA5-aph3Ib-aph6Id-dfrA17", "aadA2-dfrA12-mphA-sul1", "aadA2-cmlA1", "aadA1-blaTEM1B-dfrA1-sul1-tetA", "aadA1-aph3Ib-aph6Id-dfrA1-mphB-sul1-sul2", "aadA1-aadA2-blaSHV12-cmlA1-sul3-tetA", "aac3IId-blaTEM1B")
intraplot<-ggplot(intra, aes(x=intra$Combination, y=intra$value))  + geom_bar(aes(fill=intra$variable), stat='identity') + coord_flip() + 
  labs(title="Intracompartment (n=412)", x="Gene Combination", y="Relative Frequency of Isolates (%)") + 
  theme(plot.title=element_text(size=10, hjust = .5),
        axis.text.x=element_text(size=10),
        axis.title.x=element_text(size=10, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=10, face="bold"), 
        legend.position = "bottom", 
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        strip.text.x = element_text(size=10, face="bold")) 
print(intraplot)
intraplotfin<-intraplot + scale_fill_npg() 
print(intraplotfin)

#Intercompartment Plot
inter$Combination<-as.factor(inter$Combination)
inter$Combination<-fct_relevel(inter$Combination,"aac6Ibcr-blaOXA1-catB3", "aadA5-dfrA17-mphA-sul1", "blaTEM1B", "blaCTXM15", "tetB", "blaCTXM1-mphA", "mphA", "catA1", "blaCTXM1", "tetA", "aadA5-dfrA17-sul1", "blaTEM52C", "ermB", "blaCTXM1-sul2", "blaCMY2", "sul2", "blaTEM52B", "aph3Ib-aph6Id", "aadA1-cmlA1-sul3", "mphB", "aadA1-aadA2-cmlA1-dfrA12-sul3", "mcr11", "dfrA5", "catA1-tetA", "aph3Ib-aph6Id-sul2", "aph3Ib-aph6Id-blaTEM1B-sul2", "lnuG", "blaCTXM65", "aadA5-catA1-dfrA17", "dfrA14-mphA", "aadA5-dfrA17-sul2", "aadA5-dfrA17", "aadA1-dfrA1-sul1", "aac3IV-aph4Ia", "tetD", "sul2-tetA", "blaSHV12", "aadA1-dfrA1", "tetM", "sul3", "sul1", "qnrS1", "floR-tetA", "floR", "dfrA14", "catA1-dfrA14-mphA", "catA1-dfrA12", "blaTEM1B-tetD", "blaTEM1A", "blaCTXM27", "blaCTXM2", "blaCTXM15-blaTEM1B-qnrS1-tetA", "blaCTXM15-blaTEM1B", "blaCTXM14", "blaCTXM1-blaTEM1B", "aph3Ib", "ant3Ia-dfrA1-mphB-sul1", "aadA5-aph3Ib-aph6Id-dfrA17", "aadA2-dfrA12-mphA-sul1", "aadA2-cmlA1", "aadA1-blaTEM1B-dfrA1-sul1-tetA", "aadA1-aph3Ib-aph6Id-dfrA1-mphB-sul1-sul2", "aadA1-aadA2-blaSHV12-cmlA1-sul3-tetA", "aac3IId-blaTEM1B")
interplot<-ggplot(inter, aes(x=inter$Combination, y=inter$value))  + geom_bar(aes(fill=inter$variable), stat='identity') + coord_flip() + 
  labs(title="Intercompartment (n=267)", x="Gene Combination", y="Relative Frequency of Isolates (%)") + 
  theme(plot.title=element_text(size=10, hjust=.5),
        axis.text.x=element_text(size=10),
        axis.title.x=element_text(size=10, face="bold"),
        axis.text.y=element_text(size=10),
        axis.title.y=element_text(size=10, face="bold"), 
        legend.position = "bottom", 
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        strip.text.x = element_text(size=10, face="bold"))
print(interplot) 
interplotfin<-interplot + scale_fill_npg()
print(interplotfin)





