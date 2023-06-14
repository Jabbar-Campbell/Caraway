install.packages("imager")

library(dbplyr)
library(imager)
library(tidyr)
library(ggplot2)

#or one can be read in
GM<-load.image(file = "C:/Users/Jabbar/Downloads/Example.png")
GM<-load.image(file = "C:/Users/Jabbar/Downloads/Example.png")



#lets us display our image on a scaled plot
plot(GM)

#we have a object of class cimg with is a numeric array
class(GM)

#indeed numerical vectors can be made into a cimg
plot(as.cimg(rep(1:100,3),x=10,y=10,cc=3))

#give the dimenesions of the  array ours has r,g,b and a for alpha
dim(GM)

# we can remove the alpha dimension 
GM<-rm.alpha(GM)


#a dataframe with x y and z values can be turned into an iamge.....
df<-expand.grid(x=1:10,y=1:10) %>% 
 dplyr:: mutate(value = 1)
 as.cimg(df,dims=c(10,10,1,1)) %>% plot 


 
hist(GM, main = "Luminance values to lysosome image")
hist(R(GM), main = "just the Red channel of lysosome image")
hist(G(GM), main = "just the Green channel of lysosome image")
hist(B(GM), main = "just the Blue channel of lysosome image")

#the image can be turned into a dataframe
bdf<-as.data.frame(GM)
class(bdf)


levels(as.factor(bdf$cc))

#if we make a new column that captures each channel as a level of the cc column we
# can plot each in ggplot
bdf <- dplyr::mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)




                       ######HISTOGRAM EQUILIAZTION OF GREYSCALE####
#hopefully this will equalize the histogram GM by first converting to grayscale and then
#running the Emerical cumulative density function
GM.g<-grayscale(GM)

f<-ecdf(GM.g)

#this is what the what the transformed ECDF picture looks like
plot(f,main = "Emperical cumulative density of luminance values")
 
#the histogram where instensty values are more evenly distrubted
f(GM.g) %>% hist(main = "Transformed luminance values")

#converting this transformed dataframed back into an image we get an image where contrast are
#more obvious which is the point of this kinds of transformation
f(GM.g) %>% as.cimg(dim=dim(GM.g)) %>% plot(main="GM With histogram equalisation grayscale")

 





                         ######HISTOGRAM EQUILIAZTION OF RGB###
#makes a funciton of histogram equalization 
hist.eq<- function(GM) as.cimg(ecdf(GM)(GM),dim = dim(GM))


#breaks outimage into a list of 3 layers for R G and B
cn<-imsplit(GM,"c")


#applies/maps historgram equalization function to each element of this newly created list
cn.eq<-map_il(cn,hist.eq)


 #as opposed to split R G and B this recombines them
imappend(cn.eq,"c") %>% 
  plot(main="GM With histogram equalisation R,G B")

#makes an image gradient
imgradient(GM,"xyz") %>% 
  plot()

dx <- imgradient(GM,"x")
dy <- imgradient(GM,"y")
grad.mag <- sqrt(dx^2+dy^2) %>% 
  plot(.,main= "Gradient magnitude")





                                           ########THRESHOLDING WHAT YOU DONT WANT####
plot(GM)
GM.threshold<-threshold(GM,.7) %>% 
              plot(main= "1% of highest values")


##Label convert to a dataframe and then find the coordinates where value is greater than 0
labeled.threshold.dataframe<- GM.threshold %>% label %>% as.data.frame() %>% subset(value>0) 

#regions that are unique
unique(labeled.threshold.dataframe$value)

#split dataframe into regions and find mean coordiantes for each
centers <- labeled.threshold.dataframe %>% 
           dplyr::group_by(.,value) %>%
           dplyr::summarise(mx=mean(x),my=mean(y))


#using unique regions from that threshold circle in red
plot(GM.threshold)
with(centers,points(mx,my,col= "red"))






###THRESHOLDING WHAT YOU DO WANT####
 GM.threshold_2<-threshold(GM, .4) %>% 
                plot(.,)


labeled.threshold.dataframe_2<- GM.threshold_2 %>% 
                              label %>% 
                              as.data.frame() %>% 
                              subset(value>0) 


#split dataframe into regions and find mean coordiantes for each
centers_2 <- labeled.threshold.dataframe_2 %>% 
           dplyr::group_by(.,value) %>%
           dplyr::summarise(mx=mean(x),my=mean(y))



#regions that are unique
unique(labeled.threshold.dataframe_2$value)



plot(GM.threshold_2 - GM.threshhold)
with(centers_2,points(mx,my,col= "green"))
with(centers,points(mx,my,col= "red"))


GM.threshold + GM.threshold_2 %>% plot()##does work can;t seem to add or deduct images in this way





##################################      FUNCTIONILZED GET CENTERS###
get.centers <- function(im,thr="99%")
{
  dt <-  threshold(im=im,thr=thr) %>% 
         label %>%  
         as.data.frame() %>% 
         subset(value>0) 
  dc<-dt %>% 
      dplyr::group_by(.,value) %>% 
      dplyr::summarise(mx=mean(x),my=mean(y))
  dc
}


#########circle what gets labeled based on threshold
plot(GM)
with(get.centers(GM,.8) ,points(mx,my,col="red"))
##this is nice but its picking up any and every object we need to find circles
                                     
 





                                  
                                     
                                     
       #convert to pixel set                              
  px<-GM > .08
  
                                       
                                   
                                     
#find boundarys works best on a  a pixel set
  boundary(GM) %>% plot##nope!
boundary(px) %>% plot()
highlight(px) #highlights pixel set default is red  
     
#another way to find edges
cannyEdges(GM, alpha =.4)  %>% plot  

##groups the pixel set and highlight in different color   
shrink(px,5)   px %>% highlight(col="blue")                                    










################                               MAGICK####

#lets see what the magick package can do for us
#install.packages("image.ContourDetector")
#install.packages("magick")
library(image.ContourDetector)
library(magick)
GM_magic<-"C:/Users/Jabbar/Downloads/Example.png" %>% image_read() 

image_info(GM_magic) 
##read in image not sure what the tranpose integer step is but it allows to see contours
GM_magic %>%   
  image_data(.,channels = "gray") %>% 
  as.integer(.,transpose = TRUE) %>% 
  drop %>% 
  image_contour_detector(Q = -.5) %>% 
  plot

###increasing the brightness didnt really help the algotrhem
GM_magic %>% image_modulate(.,brightness = 200, saturation = 120, hue = 90) %>% 
  image_data(.,channels = "gray") %>% 
  as.integer(., transpose = TRUE) %>% 
  drop %>% 
  image_contour_detector(Q= -.8) %>% 
  plot
                              
                    
OH<-image_charcoal(GM_magic) 
My<-image_noise(GM_magic) %>% image_fill(color = "black", point = +1+1, fuzz = 20) %>% 
    image_threshold(type = "black", threshold = .8) %>% plot

good<-image_oilpaint(GM_magic) 
ness<-image_negate(GM_magic)                                :)
sake<-image_convolve(GM_magic, 'Sobel')

stack<-c(OH,My,good,ness,sake)

image_append(stack)





morphology_types()
kernel_types()###each shape has adjustable size



junk<-GM_magic %>% image_enhance() %>% 
             image_crop( geometry_area(475,425,380,0), gravity = NULL, repage = TRUE) %>% 
             
             #image_morphology('Thicken', 'ConvexHull') %>% 
              
             #image_morphology('EdgeIn', 'Ridges:2') %>% 
             image_fill(color = "black", fuzz = 90) %>%
             image_morphology('Dilate', 'Disk:10') %>% ####maybe this can be used as a composite ot cancel out what you dont what!!!!!!??????
             image_negate %>%
             image_colorize(opacity = 1, color = "white") %>% 
             image_transparent(color = "white",fuzz = 30)  
            #image_morphology('EdgeIn', 'cross:2') %>% 
            #image_morphology('EdgeIn', 'Ring:2.5,4.3') %>% 
            #image_morphology('EdgeIn', 'LineJunctions:5') %>% 
            #image_morphology('EdgeIn', 'Edges') %>% 
            #image_morphology('EdgeIn', 'Peaks:1.9') %>% 
            #image_morphology('EdgeIn', 'Disk:5.9') %>% 
            #image_morphology('EdgeIn', 'Plus')%>% 
            #image_morphology('EdgeIn', 'Rectangle  8x5+7+3') %>%
            #image_contrast(sharpen = 1) %>% 
            #image_canny(geometry = '3x2+40%+10%') %>%  #geometry is W x H + threshold
            #image_morphology('Convolve', 'Octagon') %>% 
            #image_morphology('Close', 'Octagon')
            #image_morphology('Thicken', 'Octagon', iterations = 1)
             
            #image_morphology('Edge', 'Octagon')
            #image_morphology('Erode', 'Octagon')
            #image_morphology('Convolve', 'Octagon')
            #image_morphology('Convolve', 'Octagon')
            #image_morphology('Convolve', 'Octagon')
            #image_morphology('EdgeOut', 'Octagon')
            #image_morphology('Thicken', 'ConvexHull', iterations = 1)
            #image_morphology('Edge', 'Octagon')
            #image_hough_draw(geometry ='50x50+10', color = "red" ,bg = "transparent", size = 1, overlay = FALSE) %>% plot
 #ness looks better           
           


keep<-GM_magic %>% image_enhance() %>% 
  image_crop( geometry_area(475,425,380,0), gravity = NULL, repage = TRUE) %>% 
  #image_transparent(color = "black",fuzz = 30) %>% 
  #image_morphology('Thicken', 'ConvexHull') %>% 
  image_negate %>% 
  #image_morphology('EdgeIn', 'Ridges:2') %>% 
  image_fill(color = "black", fuzz = 10) %>%
  image_morphology('Dilate', 'Disk:5') %>% ####maybe this can be used as a composite ot cancel out what you dont what!!!!!!??????
  image_colorize(opacity = 1, color = "white")
                         
                                     
                                     

                            
  

###Now that we've blended what we dont want with what we want lets invoke HOUGH DRAW 
####
get_circle<-ness %>% #image_crop( geometry_area(475,425,380,0), gravity = NULL, repage = TRUE) %>% 
  image_threshold(threshold = "80%", type = "white") %>% 
  image_fill(color = "white") %>% 
  image_transparent(color = "black",fuzz = 20) %>% 
  #image_morphology('EdgeIn', 'Ring:2.5,4.3') %>% 
  image_canny(geometry = '22x3+80%+10%') %>% 
  image_morphology('Thicken', 'ConvexHull', iterations = 1) %>% 
  #image_composite(.,junk, operator = "Add")  %>% 
  magick2cimg() %>% plot()

watershed(im, priority, fill_lines = TRUE)###PLAY with this water shedding and see what happens
plot(GC)
plot(get_circle)



###After conversion ti pixel set we can run a hough transformation and clean up out image 
GC<- get_circle > .0000001 
gc<-  hough_circle(GC,3) %>% plot #turn into pixle set

#Clean up, run non-maxima suppression
nms <- function(im,sigma) { im[dilate_square(im,sigma) != im] <- 0; im}
hc.clean <- isoblur(gc,5,neumann = TRUE, gaussian = TRUE) %>% nms(20) %>% plot###gives the centers of those circles
#Top ten matches
df <- as.data.frame(hc.clean) %>%
  dplyr::arrange(desc(value)) %>% head(12)
with(df,circles(x,y,5,fg="red",lwd=2))


info<-image_info(GM_magic)  




