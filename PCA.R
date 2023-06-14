#PCA






##ggbiplot is a good way to visualize PCA data 
install_github("vqv/ggbiplot")
install.packages("devtools")
library(devtools)
library(ggbiplot)
library('gdata')
library('readxl')
library('tidyverse')
library(tidyselect)
library('readxl')

###PCA  WITH JOHNS RAW DATA
FILE3<-"C:/Caraway Project/RAW DATA/12July2019_A1_HexLact_Graziotto with groups.xlsx"



##read in core data
x<-readxl::read_xlsx(path = FILE3, sheet = 1, skip = 1)
x<-as.data.frame(x)
class(df)


class(x[,1])
colnames(x)<-x[1,]
lapply(x[,2],is.na)
na_removal_list<-which(lapply(x[,2],is.na) == TRUE)

x<- x[-c(na_removal_list),]

which(x[,1] ,contains("MatrixBlk", "Sample ID"))
x<-x[-matches(match = c("Sample ID","Matrix","Lipidomics"), ignore.case = TRUE, vars = x[,1]), ]   

x[,-c(1,28,29)]<-lapply(x[,-c(1,28,29)],as.numeric)

x<-ungroup(x)

johns_pca_data<-prcomp(x[,-c(1,28,29)], center = TRUE, scale. = TRUE)
johns_pca_data
summary(johns_pca_data)
x<-unite(x,col = "Treat_rep", c("Treatment","Rep"), sep = "_", remove = FALSE)

pca.groups<- interaction(x$Rep,x$Treatment)

pca.groups<-x$Treat_rep
plot1<-ggbiplot(johns_pca_data, labels=x$Treatment,  groups = x$Treatment)
plot1



###THIS is a good pakage for three deminsional graphing 
###PROBABLY BETTER TO USE it also happend to be interactive
install.packages("pca3d")
library(pca3d)
PCA<-prcomp((x[,-c(1,28,29,30)]), scale. = TRUE)
gr<-factor(x$Treat_rep)
pca3d(PCA, group = gr, radius = 3, show.labels = TRUE, show.group.labels = TRUE)

x[,-c(1,28,29)]








###LETS TRY WITH JOHNS normalized DATA from df3 of the lipdomics engine. youll need to get a df3 into memory to do this.
library(ggbiplot)
df3<-ungroup(df3)
matche
matches(df3,match = "Matrix", ignore.case = TRUE, vars = colnames(df3))
df3[,-matches(df3,match = "Matrix", ignore.case = TRUE, vars = colnames(df3))]
shit<-pivot_wider(df3[,-matches(df3,match = "Matrix", ignore.case = TRUE, vars = colnames(df3))],names_from = Sample_ID, values_from = Value )
johns_PCA_DATA_norm<-prcomp(shit[,c(6:31)], center = TRUE, scale. = TRUE)

shit<-unite(shit,col = "Treat_rep", c("Treatment","rep"), sep = "_", remove = FALSE)

pca.groups<-shit$Treat_rep
plot2<-ggbiplot(johns_PCA_DATA_norm, labels=shit$Treatment, groups = shit$Treatment)

install.packages("gridExtra")               # Install gridExtra package
library("gridExtra")                        # Load gridExtra package
grid.arrange(plot1,plot2, ncol =2)
