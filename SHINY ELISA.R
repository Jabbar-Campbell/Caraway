#There can be no data past the last sample
#Columns containing  treatment must be in first column
#names  Dilution adjusted,  % should not appear more than once

library(readxl)
library(dplyr)
library(ggplot2)
library(shiny)

##SINGLE FILE
#FILE<-"C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Sandbox2/20211207_24 Hydroxycholesterol ELISA  abcam_Inotiv_Rat Plasma_study----_treated with 1604 day3 and 6 part 2 cornoil.xlsx"
#df<-read_excel(path = FILE, sheet = 1)
#x<-which((as.data.frame(df))[[1]] == "Treatment")  #find the word treatment in first column
#df<-df[x:dim(df)[1],]
#colnames(df)#<-df[1,]  
#df<-df[-1,]
#substr(FILES[1],153,156)

#substr(FILES[4],unlist( gregexpr( "with",FILES[4])[1])+5,##start of compound after "with"
#                unlist( gregexpr( "with",FILES[4])[1])+8)##stop of compound after "with"







##MULTIPLE FILES
FILES<-list.files("C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Sandbox2", full.names = TRUE)

All <- lapply(FILES,function(i){
  df<-read_excel(path = i, sheet = 1)
  x<-which((as.data.frame(df))[[1]] == "Treatment")  #find the word treatment in first column
  df<-df[x:dim(df)[1],]
  colnames(df)<-df[1,]  
  df<-df[-1,]
  df<-select (as.data.frame(df), contains(c("treatment","Dilution adjusted", "%")))
  df$filename<-substr(i,61,200)
  #unlist( gregexpr( "with",FILES[1])[1])##gives the position of the "with"
  df$compound<-substr(i,unlist( gregexpr( "with",i)[1])+5,##start of compound after "with"
                        unlist( gregexpr( "with",i)[1])+8)##stop of compound after "with"
  print(i)
  df
})


#colnames(ELISA)[6]
unique(ELISA$Study)

ELISA<-bind_rows(All)
ELISA<-na.omit(ELISA)
ELISA<-tidyr::separate(ELISA,col = filename,into = c("Date","Analyte","Company","species","Tissue","Study","Notes"), sep = "_", remove = FALSE)
 
   
 #####SHINY APP BEGINS   
ui <- fluidPage(
 
  
 titlePanel("ELISA"),
  
 sidebarPanel(
   
          #First input#
             selectInput(inputId = "Biomarker", 
                label = "Choose a Biomarker", 
              choices =  unique(ELISA$Analyte),
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL),
   
          #Second input#
             selectInput(inputId = "Study", 
              label = "Choose a study", 
              choices =  unique(ELISA$Study),
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL),
               
          #third input#
            selectInput(inputId = "Species", 
             label = "Select a species", 
             choices =  unique(ELISA$species),
             multiple = FALSE,
             selectize = TRUE,
             width = NULL,
             size = NULL),
          
          #fourth input#
          selectInput(inputId = "Tissue", 
                      label = "Select a Tissue", 
                      choices =  unique(ELISA$Tissue),#????white space
                      multiple = FALSE,
                      selectize = TRUE,
                      width = NULL,
                      size = NULL),
          
          #fifth input#
          selectInput(inputId = "Compound", 
                      label = "Select a Compound", 
                      choices =  unique(ELISA$compound),
                      multiple = FALSE,
                      selectize = TRUE,
                      width = NULL,
                      size = NULL)
            
            ), #end of side bar Panel
 
 
 
     #Output#
       mainPanel(
         "mainpanel",
         plotOutput(outputId="bar_plot", height = "900px"), #needs to match name call in server function below
         tableOutput(outputId = "Filtered_ELISA")
         )

 
 )#end of page


colnames(ELISA)

#Shiny
server <- function(input, output) {
  

  
  
dat1<-reactive({       filter(ELISA, Analyte == input$Biomarker & 
                                     Study == input$Study &
                                     species == input$Species &
                                     Tissue == input$Tissue &
                                     compound == input$Compound )
                                           
                        
                          })



 

  output$bar_plot<-renderPlot(height = "100%"{ 
                              
                              ggplot(dat1(), aes(x = Treatment ,y =sort(as.numeric(`% of ctl`)), color = Treatment))+
                              #geom_point(stat = "identity")+
                              geom_jitter(aes(size =2))+
                              #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
                              facet_wrap(~Date, scales = "free")+
                              theme_bw()+theme(axis.text.x = element_text(angle = 90))
                              })
  
  #output$Filtered_ELISA<-renderTable({dat1 =dat1() })

    
}

shinyApp(ui, server)




#considering trellis by date





 
  ggplot (filter(ELISA, Analyte == "24 Hydroxycholesterol ELISA" & 
                        Study ==  "study 3093-211228N1" & 
                        species == "Rat" & 
                        Tissue == "Plasma" & 
                        compound == "1944"), aes(x=Treatment, y =as.numeric(`% of ctl`), color = Treatment ))+
    #geom_point(stat = "identity")+
    geom_jitter(aes(size=2))+
    facet_wrap(~Date)+
    theme_bw()+theme(axis.text.x = element_text(angle = 90))
  

