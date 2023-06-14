library(ggplot2)
library(plotly)
library(tidyverse)
library(directlabels)


df<- read.csv(file = "C:/Caraway Project/INFORMATICS/SASHA/PrimaryNeuron aSyn CellBind v4_5_2022-Nov-03-11-40-26_Organelles_Indv_Target_Data.csv")#read in CSV
df<-df[,c(1:8,12:15)]#subset dataframe
colnames(df)#list colnames


#rep(1:12)
#rep(c("WT","TMEMKO"), each=6)
CELL_KEY<-as.data.frame(cbind(rep(1:12),rep(c("WT","TMEMKO"), each=6)))

colnames(CELL_KEY)<- c("Column","Cell")


df<-merge(df,CELL_KEY)
#probably ned to merge column/cell type info

#probably need to group by row and generate a data frame of average values on a per cell but also FOV type basis
 
#this column can be used to mutate relevant data columns.another way might be to filter by ctls and then summarize all columns
#then merge this data frame in 


ggplot(data=df, mapping = aes(x=Organelles.Total.Intensity.wv3), color = Row)+
geom_histogram()+
  geom_density()
 
p1<-ggplot(df, aes(x=Organelles.Total.Intensity.wv3, color=Row, fill=Row)) +
  geom_histogram( alpha=0.2, position ="identity", bins=50)+
  geom_density()
ggplotly(p1)


p2<-ggplot(df, aes(x=Organelles.Total.Intensity.wv3, color=Row, fill=Row)) +
  geom_histogram( alpha=0.2, position ="dodge2", bins=50)
  ggplotly(p2)
     
p3<-ggplot(df, aes(x=Organelles.Total.Intensity.wv3, color=Row, fill = Row)) +
  geom_density(alpha = 0.6,position = position_dodge2(width = 1, padding = .1, preserve = 'total'))
ggplotly(p3)


p4<-ggplot(df, aes(x=Organelles.Total.Intensity.wv3, color=Row, fill=Row)) +
    geom_freqpoly()
  ggplotly(p4)
  
  
p5<-ggplot(df, aes(x=Organelles.Total.Intensity.wv3, color=Row, fill = Row)) +
    geom_density(alpha = 0.6,position = position_dodge2(width = 1, padding = .1, preserve = 'total'))+
    facet_grid(rows = vars(Row))+theme_bw()
  ggplotly(p5)
  
p6<-ggplot(df, aes(x=Organelles.Intensity.wv3, color=Row, fill = Row)) +
    geom_density(alpha = 0.6,position = position_dodge2(width = 1, padding = .1, preserve = 'total'))+
    facet_grid(rows = vars(Row))+theme_bw()
  ggplotly(p6)
  
p7<-ggplot(df, aes(x=Organelles.Area.wv3, color=Row, fill = Row)) +
    geom_density(alpha = 0.6,position = position_dodge2(width = 1, padding = .1, preserve = 'total'))+
    facet_grid(rows = vars(Row), cols = vars(Cell))+theme_bw()
  ggplotly(p7)
  
p8<-ggplot(df, aes(x=Organelles.Area.wv3, color=Cell, fill = Cell)) +
    geom_density(alpha = 0.5,position = position_dodge2(width = 1, padding = .1, preserve = 'total'))+
    facet_grid(rows = vars(Row))+theme_bw()
  ggplotly(p8  )


 ## The cumulative distribution is like a logistic regression and instead of ploting everthing between bewtween zero
  # and 1 a s a sigmoid it transforms as it likely hood being below 1....
p8<-ggplot(df, aes(x=Organelles.Area.wv3, color=Cell)) +
  stat_ecdf(geom = "point", alpha= 0.3)+
  facet_grid(rows = vars(Row))+theme_bw()
ggplotly(p8)


ks.ts <- ks.test(filter(df1, Cell == "TMEMKO")$Organelles.Area.wv3,
                 filter(df1, Cell == "WT")$Organelles.Area.wv3, alternative = "two.sided")


#To get the Kolomogorov smirnov test on each graph i think you will have to spread the data back out
#This can be done in several ways but the final one i think is the best way
  
df1<-df %>% 
  group_by(.,Row)
  filter(df1, Cell == "TMEMKO")$Organelles.Area.wv3
  filter(df1, Cell == "WT")$Organelles.Area.wv3

                  #each FOV has a different number of objects
                    df1<- pivot_wider(data = df, id_cols = c(Row,Organelles.Area.wv3,Cell,FOV,Column), 
                    names_from = c(Cell),
                    values_from = Organelles.Area.wv3 )


                  #each object has 6 fields of  6 columns
                  df1<- pivot_wider(data = df, id_cols = c(Row,Organelles.Area.wv3,Cell, OBJECT.ID), 
                  names_from = c(Cell),
                  values_from = Organelles.Area.wv3 )

                   #each Row has 6 fields of  6 columns and different number of objects 
                  df1<- pivot_wider(data = df, id_cols = c(Row,Organelles.Area.wv3,Cell), 
                  names_from = c(Cell),
                  values_from = Organelles.Area.wv3 )



##This should make the KS table for mergeing into the orignal ggplot dataframe. Then hopefully we can display these values
#on the the Graph
ks_values<- 1: count(df1)[[1]][1]
p_values<- 1: count(df1)[[1]][1]
Row<- 1: count(df1)[[1]][1]

for (i in 1: count(df1)[[1]][1]){
  #ks<- 1:1: count(df1)[[1]][1]
  #ks<-ks.test(df1$WT[[i]],df1$TMEMKO[[i]])
  ks<-ks.test(df1$WT[[i]],df1$TMEMKO[[i]])
  #p<-ks[1]
  #d<-ks[2]
  print(ks)
  ks_values[i]<-ks[[1]]
  p_values[i]<-ks[[2]]
  Row[i]<-df1$Row[i]
  #ks<-results_table[i]
  #print(results_table)
}

p_values<-as.data.frame(p_values)
ks_values<-as.data.frame(ks_values)
row_table<-as.data.frame(Row)

k_table<-cbind(ks_values,p_values,Row)
 
df<-merge(k_table,df )


##ggplot with statistical values printed on it 
p8<-ggplot(df, aes(x=Organelles.Area.wv3, color=Cell)) +
  stat_ecdf(geom = "point", alpha= 0.3)+
  geom_text(aes(x=200 ,y=.75,label=paste("ks", round(ks_values,3),sep = "="))  , y= Inf,vjust=2)+
  geom_text(aes(x=160 ,y=.5,label= paste("p", round(p_values,3), sep = "=")) , y= Inf,vjust=2)+
  facet_grid(rows = vars(Row))+
  theme_bw()+xlim(-10,225)
ggplotly(p8)


 
# or display the table next to hte ggplot
library(gridExtra)
grid.arrange(p8,tableGrob(k_table) ,ncol=2)


#this is similar to paired plots from python's seaborn
install.packages("GGally")
library('GGally')
ggpairs(df, columns = 11:13, aes(color = Cell, alpha = 0.5),
        lower = list(continuous = "smooth"))
