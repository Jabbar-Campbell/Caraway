

####gives me all the UID numbers for records in pubmed associated with TMEM175
https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=<TMEM175>
  
  
  
  
  
 
  
  
###great way to compose searches and then copy the search bar, doing so we see that geoprofiles is a searchable 
###database https://www.ncbi.nlm.nih.gov/gds/advanced. the default is 20 so we needed to increase the max
  
 https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=geoprofiles&term="TMEM175"&retmax=10000
 https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=geoprofiles&term="TMEM175"&retmax=10000&usehistory=y
 https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=geoprofiles&term="TMEM175"&retmax=10000&usehistory=y
  
  
###There are a number of fields that can be used to triage a search such as accession number, EXTREMLY USEFUL those can be found 
#at https://www.ncbi.nlm.nih.gov/geo/info/qqtutorial.html
https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=geoprofiles&term="TMEM175"+GPL1352[GEO Accession]

  

how to obtain this list??????
  https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=geoprofiles&term="TMEM175"+GPL1352&restart=


###if we want the summary of each record we can use esummary
https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=geoprofiles&id=132756542&version=2.0 



##looks like the gds of this record is 6248 lets summarize that
https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=gds&id=6248&version=2.0 

###there are many accession numbers in this GDS which can be used in GEOquery.
















##apparently reutils is a library within R that that can do the same

install.packages("reutils")
install.packages("XML")
library(reutils)

#SEARCH GEO for TMEM and save it as an XML object
TMEM<-esearch(term = 'TMEM175', db = 'geoprofiles', rettype = 'uilist', retmode = 'xml', retmax = '10000')


#extract UID numbers from the object 
TMEM<-uid(TMEM)

#get summary info on 1 or all UID numbers as a new object
TMEM_SUMMARY<-esummary(TMEM[1:5], db = 'geoprofiles')


        ##each UID number will have A GDS numbers. this is an XML object you can navigate thru
        #or turn into a table using content, this will give us a list of UID and GSM numbers
         TMEM_SUMMARY_GDS<-content(TMEM_SUMMARY, 'parsed')
         
        
                     #Each GDS number will have many Accession numbers starting with GSM perhaps we can summarize them
                     #in another table
                     TMEM_GSM<-esummary(uid =  c('6248','6247'), db= 'gds')
                     TMEM_SUMMARY_GSM<-content(TMEM_GSM, 'parsed')
                     ##or##### for p values and meta data
                     library(GEOquery)
                     gset<-getGEO("GDS6248", GSEMatrix = TRUE)
                     

                    
                     #navigation an XML object to the Accession numbers 
                     #gives us a list of GSE or GSM numbers from out above summary table
                     #for GSE numbers which hold GSM accession we can navigaet the XML object and use GEOquery on that.
                     ###EXPRESSION DATA   https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM524151
                     #                    https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?view=data&acc=GSM524151&id=26712&db=GeoDb_blob43
                     #GEOquery works at the GSE level we can get a gz file containing GSM numbers
                      
                     TMEM_GSM['//eSummaryResult/DocumentSummarySet/DocumentSummary/GSE']  
                     gset<-getGEO("GSE39549", GSEMatrix = TRUE)
                     exprs(gset[[1]])..##should give us expression data here
                     dictionary = gset@featureData@data[,c('ID', 'ORF')]###looktable for ID and ORF
                     
                     
                     
                               
                           
                                 ###or we can drill down further to get the GSM numbers
                                 wtf<-TMEM_GSM['//eSummaryResult/DocumentSummarySet/DocumentSummary/Samples/Sample/Accession']
                     
                     
                                #To further work with XML data we need the XML package this will extract GSM numbers
                               
                                 library(XML)
                                 xmlValue(wtf[[88]])

                                 x<-c(1:88)
                                 for (i in 1:88) {
                                 x[i]<-xmlValue(wtf[[i]])
                                 x
                                  }
                                  
                                 
                                 ###chip_seq data
                                  getGEO("GSM994797", GSEMatrix = TRUE)
                
                      
                        
