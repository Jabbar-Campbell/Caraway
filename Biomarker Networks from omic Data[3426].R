#THIS SCRIPTS TAKES FOLD CHANGE DATA FROM CONTROL AND TREATMENT GROUPS AND  AND CREATES A NETWORK 
# USING PEARSON(LINEAR) OR SPEARMAN(NOLINEAR) CORRELATION
# I THINK IT DOES THIS BY MAKING A DISTANCE MATRIX FOLLOWED BY AN ADJANCEY MATRIX
#https://www.bioconductor.org/packages/release/bioc/html/INDEED.html
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("INDEED")

abrowseVignettes("INDEED")
install.packages("VennDiagram")   

 
install.packages("dplyr")
#library(glue)
packageVersion("vctrs")
update.packages("vctrs")
remove.packages("dplyr")

library("VennDiagram")    
library(dplyr)
library(INDEED)
library(INDEED)

install.packages("glasso")

#Data set that has 60 controls and 60 patients for expression levels of 39 metabolites (ask john)
Met_GU
str(Met_GU)

#Kegg ID's for each metabolite ie C03046(ask john)
Met_name_GU
str(Met_name_GU)


#Group information for all patients 0 is control 1 is patient
Met_Group_GU
class(Met_Group_GU)


#This looks at how much each metabolite influences the others. The result is stored in a data frames activity_score and diff_network
#non partial corelation uses the covariance matrix which confoinds direct and indirect associations. 
#partial correlation uses the inverse covariance matrix and removes the affect of other biomolecular pairs
#If euclidian is the distance between to points....there's also a Covariance for a pair of points as well
result<-non_partial_cor(data = Met_GU, class_label = Met_Group_GU, id=Met_name_GU, method = '')

#select_rho_partial()
#partial_cor()

#display the result as a Network
network_display(results = result, nodesize = 'Node_Degree', nodecolor = 'Activity_Score', edgewidth = 'NO', layout = 'nice')




################Now lets try with a DATA set from our EVS...
##############First we'll need to get the data into a similar format
#Data set of 15 control and 15 patients for expression of ~400 metabolites or EVs
jabbar_GU<-read.csv(file = "C:/Users/jabba/Desktop/Bioinformatic/091822_Mouse_and_Rat_Combined_after_Perseus.csv")
colnames(jabbar_GU)

#looks for duplicates....Go back to the file and address these
data.frame(table(jabbar_GU$Gene_Name))

#Gene ID's for each metabolite..strip this from the orignal data and make a matrix
jabbar_name_GU<-matrix(jabbar_GU$Gene_Name,nrow=length(jabbar_GU$Gene_Name), ncol = 1)

###all ctls and treatments this seems to work 
#jabbar_GU<-jabbar_GU[,7:41] 



####THE TROUBLE IS HERE WHEN I ATTEMPT TO SUBSET TREATMENT GROUPS AND FEED THESE INTO A PARTIAL CORRELATION
###ctl vs 807
jabbar_GU<-jabbar_GU[,c(7:10, 22:25)]

###ctl vs 1364
#jabbar_GU_total[,c(1:4, 20:23)]

###ctl vs 2821
#jabbar_GU_total[,c(5:8, 24:27)]

###ctl vs 1993
#jabbar_GU_total[,c(8:15, 28:35)]


#Group information for all patients 0 is control 1 is patient
#just make a key of zeros and ones to match the groups...make them numeric
#jabbar_group_GU<-rbind(colnames(jabbar_GU),  c(rep(0,15), rep(1,20))  ) #ALL TREATMENT GROUPS
jabbar_group_GU<-rbind(colnames(jabbar_GU),  c(rep(0,4), rep(1,4))  )    #ctl vs 807 :(
 str(jabbar_group_GU)         

colnames(jabbar_group_GU)<-jabbar_group_GU[1,]
jabbar_group_GU<-as.data.frame(jabbar_group_GU)
jabbar_group_GU[1,]<-jabbar_group_GU[2,]
jabbar_group_GU<-jabbar_group_GU[1,]
jabbar_group_GU<-jabbar_group_GU %>% mutate_if(is.character, as.numeric)


#I believe Pearson came first and his alogrithem is based on a linear relationship between values. 
#spearman came later and his alorithem can handle variables that change non linearly
#both total and subset data works here
result<- non_partial_cor(data = jabbar_GU, class_label = jabbar_group_GU, id=jabbar_name_GU, method = 'pearson')
network_display(results = result, nodesize = 'Node_Degree', nodecolor = 'Activity_Score', edgewidth = 'NO', layout = 'nice')


unlist(which(result$diff_network$Weight>0))

#The Partial Correlation allows one to discern between direct and indirect influence 
##FOR SOME REASON SUSETTING THE DATA INTO TREATMENT GROUPS UP ABOVE THROWS AN ERROR HERE!!!!!!  n   
rho_partial_output<-select_rho_partial(data=jabbar_GU, class_label=jabbar_group_GU ,id=jabbar_name_GU)
result2<-partial_cor(data_list=rho_partial_output,
            rho_group1 = "min",
            rho_group2 = "min",
            permutation = 1000,
            p_val = NULL,
            permutation_thres = .05)
network_display(results = result2, nodesize = 'Node_Degree', nodecolor = 'Activity_Score', edgewidth = 'NO', layout = 'nice')



############################################VENN Diagram of overlap between the 2 methods#############
my_spearman<-c("ATP6v1a", "Aadat","Abhd14b","Slc5A1","Fuca1","Gapdh","ATP5f1b","Myo7b","Arsb")

my_pearson<-c("Glb1","Naca","Dlst","Xpnpep2","Abltd14b","Gapdh","Aadat","Pdcd6","Ctsa","Fuca1","Arsb","Prdx2","Myo7b")

my_partial<-c("Atp5me","Glb1","Aadat","Slc5A","Ctsa","Pdcd6","Dlst","Arsb","Xpnpep2","Gapdh","Metap2","Chmp4b","Anxa2","Cst3","Prdx6","Xpnpep2","lap3","GAPDH")
v<-venn.diagram(x=list(my_spearman=my_spearman, my_pearson=my_pearson, my_partial = my_partial), 
                 fill = c("orange", "blue", "green"),
                 category.names = c("Spearman","Pearson" ,"Partial"), 
                 filename = NULL,ext.text = FALSE 
                  )
 


ngrid.newpage()
grid.draw(v)

install.packages("ggvenn")
library(ggvenn)
library(RColorBrewer)
 
 
ggvenn(list(my_spearman=my_spearman, my_pearson=my_pearson, my_partial = my_partial),
       show_elements = T,
       label_sep = "\n", 
       fill_color = brewer.pal(name="Set2",n=3))