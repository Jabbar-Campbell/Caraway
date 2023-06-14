#install.packages("readxl")
#install.packages('tidyverse')
#install.packages('gdata')
#install.packages("ggplot2")
#install.packages("officer") 
#install.packages("writexl")


library('gdata')
library('readxl')
library('tidyverse')
library('writexl')

##This is similar to the original lipdimics script from the first week in caraway except that
#it writes out the format for prism as well calculates DMSO percentages


FILE4<-"C:/Caraway Project/RAW DATA/Copy of 18Dec2019_A1_HexLact_Graziotto raw non calc.xlsx"

FILE5<-"C:/Caraway Project/RAW DATA/16Mar2020_A1_Hex_Graziotto.xlsx"

FILE6<-"C:/Caraway Project/RAW DATA/09April2020_A4_GluGal_Graziotto_Neurons.xlsx"

FILE7<-"C:/Caraway Project/RAW DATA/13April2020_A1_Lact_Graziotto_Neurons.xlsx"

FILE8<-"C:/Caraway Project/RAW DATA/09April2020_A4_GluGal_Graziotto_mouse_only.xlsx"

FILE8<-"C:/Users/Jabbar/Desktop/Sandbox/09April2020_A4_GluGal_Graziotto_mouse_plasma.xlsx"
FILE9<-"C:/Users/Jabbar/Desktop/Sandbox/09April2020_A4_GluGal_Graziotto_mouse_liver.xlsx"
FILE10<-"C:/Users/Jabbar/Desktop/Sandbox/09April2020_A4_GluGal_Graziotto_mouse_brain.xlsx"


FILE11<-"C:/Users/Jabbar/Desktop/Sandbox/13April2020_A1___Lact_Graziotto_mouse_plasma.xlsx"
FILE12<-"C:/Users/Jabbar/Desktop/Sandbox/13April2020_A1___Lact_Graziotto_mouse_liver.xlsx"
FILE13<-"C:/Users/Jabbar/Desktop/Sandbox/13April2020_A1___Lact_Graziotto_mouse_brain.xlsx"

##read in core data

FILE<-FILE13
x<-readxl::read_xlsx(path = FILE, sheet = 1, skip = 1)
x<-as.data.frame(x)


###pick a start AND STOP  and end point for the normalized data
###find a reference point with the which() or where() function
indice_list<- x[,1]
start_main_data<-which(indice_list == "MatrixBlk", arr.ind = TRUE )
Stop_main_data<-which(indice_list == "Comments:", arr.ind = TRUE )-2
df<-x[c(start_main_data:Stop_main_data),]
colnames(df)<-x[1,]
df<-df[-1,]



##find where last column is 
column_list<-x[1,]
last_column<-which(column_list == "Rep")
df<-df[,c(1:last_column)]

#colnames(df)[last_column-1]<-"Treatment"
#colnames(df)[last_column]<-"Cell"




###pick a start AND STOP  and end point for the normalized data
###find a reference point with the which() or where() function
indice_list<- x[,1]
start_norm_data<-which(indice_list == "Lipidomics ID", arr.ind = TRUE )
Stop__norm_data<-end(indice_list)[1]

#make a table of the normalized data
#NA isng needed and since i couldnt use omit na i reversed filtered where NA wasnt present
norm_tble<-x[c(start_norm_data:Stop__norm_data),c(1:3)]
colnames(norm_tble)<-norm_tble[1,]
norm_tble<-norm_tble[-1,]
norm_tble$`Client ID`<-NULL
colnames(norm_tble)[1]
#norm_tble<-filter(norm_tble, norm_tble$`Phosphate (Pi) Normalization [nmole/sample]` != "NA")


##turn and NA to a 1 so the raw data still gets normalized
norm_tble$`Phosphate (Pi) Normalization [nmole/sample]`<-gsub(norm_tble$`Phosphate (Pi) Normalization [nmole/sample]`,pattern = "NA", replacement = "1")

#merge the normalization table with the raw value table4
df1<-merge(norm_tble, df, by.x= "Lipidomics ID", by.y = "Sample ID", all.y = FALSE)

##check the class of each column and turn what is needed into numeric
##to find the end and omit treatment and cell columns lets use y as a place holder
#y<-length(colnames(df1))
dim(df1)[2]
lapply(df1[,c(2:dim(df1)[2])],class)
y=which(colnames(df1)== "Cell_Line")-1
##turn the class of each column to a numeric
df1[,2:y]<-lapply(df1[,c(2:y)],as.numeric)


####IN THIS NEW FORMAT DMSO MUST BE GROUPED AND AVERAGED BEFORE ANY NORMALIZATION TAKES PLACE
##df1 is gathered and grouped
df1<-df1 %>% 
  gather(., key = "lipid", value = value, c(3:y )) %>% 
  group_by(.,lipid,TYPE, Treatment, Cell_Line)

###here we summarize df1 and merge that info back into itself to create df2
df2<-df1%>% 
  summarize(avg_value = mean(value) ) %>%  
  merge(.,df1) %>% 
  unite(.,col=Treatment_rep,c(Treatment,Rep),sep = "_", remove = FALSE)

##filtering df2 we get only media values can can calculate percenatages based on group averages
#youll need to spread this out for prism as some point
#per_DMSO_in_media<-df2 %>% 
#  filter(.,Treatment== "DMSO") %>% 3  mutate(., percentage = value/avg_value *100)




###here a DMSO block is generated after phosphate normalization
per_DMSO_in_media<-mutate(df2,Pi_normalized = value/`Phosphate (Pi) Normalization [nmole/sample]`)
per_DMSO_in_media<-filter(per_DMSO_in_media,Treatment == 'DMSO')

per_DMSO_in_media<-per_DMSO_in_media %>%   
  group_by(.,TYPE,lipid,Treatment,Cell_Line) %>% 
  summarize(avg_PI_norm_DMSO = mean(Pi_normalized) ) %>% 
  merge(.,per_DMSO_in_media, by = c("TYPE","lipid","Cell_Line")) %>%  
  mutate(., percentage=  (Pi_normalized/avg_PI_norm_DMSO*100))






##FUNCTION TESTING GROUND
##Here we spread the raw data out based on only the parameters we need so we dont get NA values below this step
#is functionalized
#  prism<-per_DMSO_in_media%>% 
#  ungroup() %>% 
# filter(.,TYPE == "Liver Homogenate"  & Cell_Line == "Liver") %>%
 # pivot_wider(.,id_cols = c("Treatment_rep","lipid", "Genotype"), names_from = Treatment_rep, values_from = percentage)
 
  
        

#####
###THESE  FUNCTIONS TAKE 3 ARGUMENTS TO FILTER THE DATA BY ON SPREADS VALUE THE OTHER PERCENTAGES...
#"dataframe" 
#"type"
#"cell line" 
## ONLY THE VARIABLE WE NEED WE CAN SPREAD THE DATA without NA's
##BUT FIRST LETS MAKE A LIST TO FEED INTO OUR FUNCTIONS
#cell_lines<-unique(df2$Cell_Line)
#types<-unique(df2$TYPE)

prism_raw_function<-function(data_frame,type,Cell){
  prism<-mutate(df2,Pi_normalized = value/`Phosphate (Pi) Normalization [nmole/sample]`)%>% 
    ungroup() %>% 
    filter(.,TYPE == type & Cell_Line == Cell) %>%
    pivot_wider(.,id_cols = c("Treatment_rep","lipid","Genotype"), names_from = Treatment_rep, values_from = Pi_normalized)
  prism$Cell_line<- Cell
  prism$TYPE<- type
  prism$block<-"Pi adjusted"
  prism  
}

prism_DMSO_function<-function(data_frame,type,Cell){
  prism2<-per_DMSO_in_media%>% 
    ungroup() %>% 
    filter(.,TYPE == type & Cell_Line == Cell  ) %>%
    pivot_wider(.,id_cols = c("Treatment_rep","lipid","Genotype"), names_from = Treatment_rep, values_from = percentage)
    prism2$Cell_line<- Cell
    prism2$TYPE<- type
    prism2$block<-"dmso"
    prism2  
}



prism_normalized_function<-function(data_frame,type,Cell){
  prism3<-df4%>% 
    ungroup() %>% 
    filter(.,TYPE == type & Cell_Line == Cell  ) %>%
    pivot_wider(.,id_cols = c("Treatment_rep","percent_to_DMSO","lipid","Genotype"), names_from = Treatment_rep, values_from = percent_to_DMSO)
    prism3$Cell_line<- Cell
    prism3$TYPE<- type
    prism3$block<-"norm_to_dmso"
    prism3  
}



####create a list of matrices to write to once we loop thru our funciton
#the number of dataframes should be should be the number of "cell_lines" times the "number types"
#the dimensions of each dataframe will need to be extracted from a sample run
#now we lapply a matrix of the above dimensions to each element of our new list,
#this will hold the data from our loop
cell_lines<-unique(df2$Cell_Line)
types<-unique(df2$TYPE)

empty<-letters[1:(length(cell_lines)*length(types))]#number of dataframes

matrix_dim<-prism_raw_function(data_frame = df2, type = types[1], Cell = cell_lines[1])#we have the diminsions we need
dim(matrix_dim)


empty<-lapply(empty, function(x){
  x=matrix(1:dim(matrix_dim)[1]*dim(matrix_dim)[2], dim(matrix_dim)[1],dim(matrix_dim)[2])
})






##here run our function thru the nested loop and reset the counter if you want to run this on the DMSO DATA
##change the function values_from argument to correct column percentage
counter<-1
for (i in 1:length(types)){
  for(j in 1:length(cell_lines)){
     
     
     empty[[counter]]<-prism_raw_function(data_frame = df2, type = types[i], Cell = cell_lines[j])
     print(empty[counter])
     empty<-empty
     counter<-counter+1
                                }
}
counter<-1


###lets do this again for our DMSO percentage data but this time use the prism_DMSO_fuction 
#which is essentially
#the same
cell_lines<-unique(per_DMSO_in_media$Cell_Line)

types<-unique(per_DMSO_in_media$TYPE)

empty2<-letters[1:(length(cell_lines)*length(types))]#number of dataframes

matrix_dim<-prism_DMSO_function(data_frame = per_DMSO_in_media, type = types[1], Cell = cell_lines[1])#we have the diminsions we need
dim(matrix_dim)

empty2<-lapply(empty2, function(x){
  x=matrix(1:dim(matrix_dim)[1]*dim(matrix_dim)[2], dim(matrix_dim)[1],dim(matrix_dim)[2])
  
})

counter<-1
for (i in 1:length(types)){
  for(j in 1:length(cell_lines)){
    
    empty2[[counter]]<-prism_DMSO_function(data_frame = per_DMSO_in_media, type = types[i], Cell = cell_lines[j])
    print(empty[counter])
    empty2<-empty2
    counter<-counter+1
  }
}
counter<-1




#####NOW THAT the RAW  DMSO DATA CAN BE FORMATTED FOR PRISM LETS WORK ON NORMALIZING THE
##DATA WE ALREADY HAVE THE pI DATA IN DF1 AND DF2  SO LETS START MUTATING IT
##THE idea is to group the data and average each treatment set. All data will be normalized 
#to there DMSO control. here we summarized just the DMSO DATA and merge those averages back in
#for calcuation purposes :)))) were good on the math up til here
df3<-mutate(df2,Pi_normalized = value/`Phosphate (Pi) Normalization [nmole/sample]`)

df4<-df3%>% 
  group_by(.,TYPE,lipid,Treatment,Cell_Line) %>% 
  filter(.,Treatment == 'DMSO' & TYPE == unique(df3$TYPE)) %>% 
  summarize(avg_PI_norm_DMSO = mean(Pi_normalized) ) %>% 
  merge(.,df3, by = c("TYPE","lipid","Cell_Line")) 
  
df4<-mutate(filter(df4, TYPE ==unique(df3$TYPE)), percent_to_DMSO=  (Pi_normalized/avg_PI_norm_DMSO*100))









####we can now use a third function that spreads out "percent_to_DMSO"
cell_lines<-unique(df4$Cell_Line)

types<-unique(df4$TYPE)

empty3<-letters[1:(length(cell_lines)*length(types))]#number of dataframes

matrix_dim<-prism_normalized_function(data_frame = df4, type = types[1], Cell = cell_lines[1])#we have the diminsions we need
dim(matrix_dim)

empty3<-lapply(empty3, function(x){
  x=matrix(1:dim(matrix_dim)[1]*dim(matrix_dim)[2], dim(matrix_dim)[1],dim(matrix_dim)[2])
})

counter<-1
for (i in 1:length(types)){
  for(j in 1:length(cell_lines)){
    
    empty3[[counter]]<-prism_normalized_function(data_frame = df4, type = types[i], Cell = cell_lines[j])
    print(empty3[counter])
    empty3<-empty3
    counter<-counter+1
  }
}
counter<-1






###mAYBE MOVE THIS INTO EACH FUCNTION???????????????
what<-bind_rows(empty)
what<-what[,sort(names(what))]

what2<-bind_rows(empty2)
what2<-what2[,sort(names(what2))]

what3<-bind_rows(empty3)
what3<-what3[,sort(names(what3))]



output<-lapply(ls(pattern="what"), function(x) get(x))##FINAL OUTPUT FILE


##WRITE TO EXCEL THIS PACKAGE DOES REQUIRE JAVA DEPENDENTCY
output_name<-paste0( "C:/Caraway Project/RAW DATA/", substr(FILE, start = 34, stop = 43),"_",substr(FILE, start = 65, stop = 76),"_output" ,".xlsx")


write_xlsx(x = output, path = output_name, col_names = TRUE)























#*************************FOR QCING THE PI NORMILZATION DATA


###FINDING WHERE the PI NORMALIZED DATA STARTS AND ENDS
indice_list<- x[,1]
start_Pi_Normalized<-which(indice_list == "Pi Normalized", arr.ind = TRUE )
Stop_Pi_Normalized<-which(indice_list == "Normalized to DMSO average", arr.ind = TRUE )-1

###reset the columns headings and remove rows with NA values
Pi_Normalized<-df<-x[c(start_Pi_Normalized:Stop_Pi_Normalized),]
colnames(Pi_Normalized)<-Pi_Normalized[2,]
colnames(Pi_Normalized)[1]<-"Cell_Line"
Pi_Normalized<-Pi_Normalized[-c(1,2),]
Pi_Normalized<-na.omit(Pi_Normalized)

#CONVERT the relevant columns to numeric
Pi_Normalized[,c(6:dim(Pi_Normalized)[2])]<-lapply(Pi_Normalized[,c(6:dim(Pi_Normalized)[2])],as.numeric)


##group the data gather it and even in merge this information with the original spread sheet
##or set up a box plot littile g is gathered and grouped
Pi_Normalized_g<-Pi_Normalized %>% 
  gather(.,key = "lipid", value = "value", c(6:dim(Pi_Normalized)[2]))%>% 
  group_by(.,TYPE,lipid,Treatment,Cell_Line) 
    
    
#little g has been summarized and merged back to itself to create big G, lets include lower and upper quartiles as well
Pi_Normalized_G<-Pi_Normalized_g%>% 
  summarize(avg_value = mean(value), std_dev = sd(value),std_err = std_dev/ sqrt(length(value)),IQR = IQR(value) , 
            lowerq = quantile(value)[2],
            upperq = quantile(value)[4] ) %>% 
  merge(.,Pi_Normalized_g)


##now that we have quartile information we can calculate upper and lower limit
Pi_Normalized_G<-Pi_Normalized_G %>% 
  mutate(.,upper_limit =(IQR * 1.5) + upperq, lower_limit = lowerq - (IQR * 1.5) )
 
##lets add a column that test if value is between these new established limits
##for some there are only 2 replicates so IQR wont reveal the outlyer ultimately there needs to be some other
##way
Pi_Normalized_G<-  Pi_Normalized_G %>% 
  #filter(.,value>lowerq & value < upperq)
  mutate(.,pass = value>lower_limit & value<upper_limit )








##plot1
ggplot(Pi_Normalized_G,aes(x=lipid ,y=value, colour = Cell_Line) )+
  geom_boxplot(alpha=5,
               
               # custom outliers
               #outlier.colour="Black",
               #outlier.fill="Black",
               outlier.size=NA)+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point()+
  facet_wrap(Treatment~Cell_Line, scales = "free_y")



#plot2
plot2<-ggplot(Pi_Normalized_G,aes(x=Treatment ,y=value, colour = Cell_Line) )+
  geom_boxplot(alpha=5,
               
               # custom outliers
               #outlier.colour="Black",
               #outlier.fill="Black",
               outlier.size=NA)+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_point()+
  facet_wrap(Cell_Line~lipid, scales = "free_y")


install.packages("ggplot2")
library(ggplot2)
#ggsave(filename = "C:/Caraway Project/RAW DATA/plot2", plot2, device = "pdf", height = 20, width = 20)


