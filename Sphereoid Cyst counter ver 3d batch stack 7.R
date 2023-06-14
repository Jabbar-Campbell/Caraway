install.packages("imager")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("EBImage")



library(dbplyr)
library(imager)
library(tidyr)
library(ggplot2)
library(EBImage)
library(tidyverse)

 


Folders<-list.files(path = "C:/Caraway Project/INFORMATICS/spheroid/Stack 7", full.names = TRUE)
FILES<-list.files(path = Folders[1], pattern = "- DAPI", full.names = TRUE)


#reads  images from a single folder
#slice<-lapply(FILES, function(i){
#  slice<-readImage(files = i, all = TRUE, type = 'TIFF')
#  slice
#})



   

     


####this reads all filenames from each folder 64 for each folder
slice_meta<-lapply(Folders, function(i){
  slice_meta<-list.files(i, pattern = "DAPI", full.names = TRUE)
              as.data.frame(slice_meta)
  slice_meta
})
 

###reads all images from each folder pretty sure this is right but memory wont allow me to run
#slices<-lapply(Folders, function(i){
#            FILES<-list.files(path = i,pattern = "DAPI", full.names = TRUE) 
#                   slice<-lapply(FILES, function(j){
#                   slice<-readImage(files = j, all = TRUE, type = 'TIFF')  
#                   slice
#                    })
#                   slice
#})



###take file names and formats
#slice_meta<-as.data.frame(FILES) %>% 
#  separate(., col = FILES, into =c("Company","path","Experiment/indication","Object",
#                                   "Magnification/date/level/row",
#                                   "seperator",
#                                   "well",
#                                   "field",
#                                   "wv",
#                                   "UV",
#                                   "seperator2","channel") , sep = " ") %>% 
#   select(.,c(1:5,7,8,12)) %>% 
#   separate(.,col= 'Magnification/date/level/row',into = c("Mag","Month","day","level", "Row"),sep= "_") %>% 
#   separate(.,col= 'Experiment/indication',into = c("Experiment","Indication"),sep= "/") %>% 
#   separate(.,col= well ,into = c("Well","x"),sep= "\\(") %>% 
#   separate(.,col= channel ,into = c("Channel","y"),sep= "\\).") %>% 
#   separate(.,col= Row ,into = c("Exp","Row"),sep= "/") %>%
#   unite(., col='Path', c('Company', 'path'), sep='/') %>% 
#   unite(., col='Path', c('Path', 'Experiment'), sep=' ') 
#  select(.,c(Mag,Level,Row,Well,Experiment,Channel))
#slice_meta<-cbind(slice_meta,as.data.frame(FILES))


##This takes the files names from each folder and then extrats meta data and makes a giant table out of them
slice_meta<-lapply(slice_meta, function(i){
   meta<-i
   extra<-as.data.frame(meta) %>% 
         separate(., col = 1, into =c("Company","path","Experiment/indication","Object",
                                      "Magnification/date/level/row",
                                      "seperator",
                                      "well",
                                      "field",
                                      "wv",
                                      "UV",
                                      "seperator2","channel") , sep = " ") %>% 
     select(.,c(1:5,7,8,12)) %>% 
     separate(.,col= 'Magnification/date/level/row',into = c("Mag","Month","day","level", "Row"),sep= "_") %>% 
     separate(.,col= 'Experiment/indication',into = c("Experiment","Indication"),sep= "/") %>% 
     separate(.,col= well ,into = c("Well","x"),sep= "\\(") %>% 
     separate(.,col= channel ,into = c("Channel","y"),sep= "\\).") %>% 
     separate(.,col= Row ,into = c("Exp","Row"),sep= "/") %>%
     unite(., col='Path', c('Company', 'path'), sep='/') %>% 
     unite(., col='Path', c('Path', 'Experiment'), sep=' ') 
   #  select(.,c(Mag,Level,Row,Well,Experiment,Channel))
  
   slice_meta<-cbind(meta,extra)
  slice_meta
  
})




p<-slice_meta[[1]][1,1]
q<-slice_meta[[2]][1,1]
r<-slice_meta[[3]][1,1]
s<-slice_meta[[4]][1,1]
t<-slice_meta[[5]][1,1]
u<-slice_meta[[6]][1,1]
v<-slice_meta[[7]][1,1]
w<-slice_meta[[8]][1,1]
x<-slice_meta[[8]][1,1]
y<-slice_meta[[8]][1,1]
#z<-slice_meta[[8]][49,1]

Stack<-readImage(files = c(p,q,r,s,t,u,v,w,x,y),names = c("p","q","r","s","t","u","v","w","x","y"))
#Stack<-readImage(files = c(p,q,r,s),names = c("p","q","r","s"))
Stack<-Stack*2
EBImage::display(Stack, method = "raster" ,all = TRUE)
 

Stack = apply(Stack, 1:2, max)

 



#str(Stack)
#dim(Stack)#2048*2048*4
#as.data.frame(getFrames(Stack, 1:4,type = 'total' ))


#imageData(Stack)[(2048*2048*4)]
#getFrame(y=Stack,5)
#hist(Stack)
#range(Stack)

#print(Stack, short = TRUE)



# Magick allows you to flatten an  image stack
#Magick_Stack<-magick::image_read(Stack)

#magick::image_flatten(Magick_Stack)
#print(magick::image_flatten(Magick_Stack))

#Spheres<-magick::image_flatten(Magick_Stack)


 
########################################################EBImage#############################
library(EBImage)
library(imager)

#list.files(path=".", pattern = "DAPI", full.names = TRUE)
#grep("DAPI",FILES)
#Spheres= readImage("C:/Caraway Project/INFORMATICS/spheroid/A - 14(fld 1 wv UV - DAPI).tif", type = "tiff")
#Spheres= readImage(files = "C:/Caraway Project/INFORMATICS/Deirdre.png" )#[,,1]
#class(Spheres)
#magick2cimg(Spheres)
#as.Image(magick2cimg(Spheres))

#stack need to be converted to a cimg and an EBImage with proper dimensions 3, and 4 are not neccesary
#Spheres<-Spheres %>% magick2cimg(.) %>% as.Image(.) %>% Image(., dim = c(2048,2048))

Spheres<-Stack
EBImage::display(Spheres)
#dim(Spheres)
#hist(Spheres)
#colorMode(Spheres)=Grayscale
  

#Grayscale
#EBImage::display(Image(Spheres, colormode = 'Grayscale'))
 

#Negative
#EBImage::display(max(Spheres)) 
#EBImage::display(max(Spheres)-Spheres)

#


#Brightnes
EBImage::display(Spheres)
EBImage::display(Spheres*1 > .76) 
#combine(Spheres, Spheres * 3 > .08) %>% EBImage::display(.,all=TRUE)




#crop
Spheres=Spheres[260:1600, 260:1600]
EBImage::display(cropped)

#Threshhold
#range(Spheres)
#EBImage::display(Spheres > .12)


#Fourier blurs??????????????????
w = makeBrush(size = 31, shape = 'gaussian', sigma = 3)
plot(w[(nrow(w)+1)/2, ], ylab = "w", xlab = "", cex = 0.7)
EBImage::display(filter2(Spheres>.76,w))


#laplace sharpens !!!!THIS ACTUALLY WILL PROVE TO BE USEFUL
fhi = matrix(1, nrow = 3, ncol = 3)
fhi[2, 2] = -7.1
EBImage::display(filter2(Spheres > .76, fhi))
laplace=filter2(Spheres > .76, fhi)##########################################################useful######################################

#we can make a kernel nad use it to Erode or Dilate
kern = makeBrush(5, shape = 'Box')########################################################useful######################################
EBImage::display(kern, interpolate = FALSE)


#Spheres_erode = EBImage::erode(Spheres, kern) %>% EBImage::display()
#Spheres_dilate = EBImage::dilate(Spheres, kern) %>% EBImage::display()
#laplace_erode = EBImage::erode(laplace, kern) %>% EBImage::display()
laplace_dilate = EBImage::dilate(laplace, kern) %>% EBImage::display()  ###################useful#####################################

##Adaptive thresholding works by comparing each pixel's intensity to the background 
#determined from a local neighbourhood. This can be achieved by comparing the image 
#to its smoothed version, where the filtering window is bigger than the typical size of 
#objects we want to capture 
#disc = makeBrush(3, "diamond")
#disc = disc / sum(disc)
#offset = 0.005
#nuc_bg = filter2( Spheres, disc )
#nuc_th = Spheres > nuc_bg + offset
#EBImage::display(nuc_th, all=TRUE)


#adaptive thresholding using a linear filter with a rectangular box is provided by thresh
#Sph_thr<-EBImage::display( thresh(Spheres, offset=0.005), all=TRUE )



#EBImage::display(laplace >.2)
#EBImage::display(laplace ^3 >.2)
#EBImage::display(laplace_dilate > .2)

#Water shedding   
#nmask = EBImage::watershed( EBImage::distmap(laplace ^3>.2),tolerance = 4 ,ext=1 )
#EBImage::display(colorLabels(nmask), all=TRUE)



#fill holes then dilate
#count= EBImage::fillHull(Spheres *5) %>%
 #       EBImage::dilate(.,kern) 
#count= EBImage::fillHull(Spheres >.3) %>% 
   #EBImage::erode(.,kern) %>% 
  #EBImage::erode(.,kern) %>%   
  #EBImage::erode(.,kern) 
#EBImage::display(count * 1.2)
 


#this labels all objects found that arent touching something else other than background
#my_counts<-bwlabel(count)

#this table shows the pixels falling into each object
table(bwlabel(count))
max(bwlabel(count))


#This color codes each of the 11 objects
#after=EBImage::display( colorLabels(Spheres) )

#before=EBImage::display(Spheres) 

 


#############################


#Water shedding seems to work well and is better than eroding the image it basically takes the 
#inverse of the image so bright areas are low elevation and background is high elevation. 
#Watershedding erodes the water level so objects cease to merge or overflow into each other   
nmask = EBImage::watershed( EBImage::distmap(Spheres>.8),tolerance = 3 ,ext=1)#############useful###########################
EBImage::display(colorLabels(nmask), all=TRUE, method = 'raster')#######################################################useful###########################
 
EBImage::display(normalize(nmask), title = 'Objects')
 


 
Spheroid_shape<-as.data.frame(EBImage::computeFeatures.shape(x=nmask,ref=Spheres, xname = "Spheres")) #:)
#Spheroid_basic<-as.data.frame(EBImage::computeFeatures.basic(x=nmask,ref=Spheres, xname = "Spheres") )
Spheroid_axis<-as.data.frame(EBImage::computeFeatures.moment(x=nmask,ref=Spheres, xname = "Spheres")) #:)

#Haralick texture features are used to describe the "texture" of an image. If you are trying to quantify 
#and represent the consistency of a surface using the Gray-Level Co-occurrence Matrix (GLCM)
#this is basically a county of how many pairs of pixels appear next to each other and stored in a table
#Spheroid_haralick<-as.data.frame(EBImage::computeFeatures.haralick(x=nmask,ref=Spheres, xname = "Spheres") )

 

#Cell size (per pixel) = Physical length of a pixel on the CCD / total magnification
#incel uses
my_objects<-cbind(Spheroid_axis[,1:2],Spheroid_shape)

dplyr::filter(my_objects, my_objects$'s.area' > 400 )
 
#Labels each object using my_objects as a reference
EBImage::display(colorLabels(nmask), all=TRUE, method = "raster", title='Labeled Objects',margin = 50)
for (i in 1:dim(dplyr::filter(my_objects, my_objects$'s.area' > 400 ))[1]) {
     x <- my_objects[i,1] 
     y <-my_objects[i,2] 
     label<- row.names(my_objects)[i]#we could slap the area in there if we wanted
     text(x = x , y = y, label = label,adj = NULL,pos = NULL, col = "White", cex = 1)
}

#dev.off()  

class(my_objects)


PATH<-paste0("C:/Caraway Project/INFORMATICS/spheroid/Stack 7 output/",i," Mag ",
             slice_meta[[1]][1,5],"_","Well ",
             slice_meta[[1]][1,10],
             slice_meta[[1]][1,11],"_","merge",
             #slice_meta[[1]][1,8],
             ".xlsx")


write.table(my_objects, file = PATH,sep = "\t", col.names = TRUE)


devEval("jpeg", name = "overlay", path = substring(PATH, 1, gregexpr(pattern = "ut/",PATH)[[1]][1]+1) )
 
"C:\Caraway Project\INFORMATICS\spheroid\Stack 7 output"
    

#for 3D rendering our best bet is NAPARI which is a python package
#we can switch over and emulate python with in R using the reticulate package



