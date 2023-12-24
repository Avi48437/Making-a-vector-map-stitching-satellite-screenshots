setwd("D:/D/Avinandan Roy/Semister-5/Endsem/Projects/Linear Model/Final/Frames")
Name_data=read.csv(file.choose())

install.packages("jpeg")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("plotly")
library(jpeg)
library(ggplot2)
library(plotly)
library(ggrepel)


my_data=data.frame()
construct=function(data,name,k){
  
    n=length(name)
    map = readJPEG(paste("ss",k,".JPG", sep = ""))
    d = dim(map)[1:2]
    cat("Choose these places in order from the map as shown below-\n")
    for(i in 1:n)
    {
      cat(i,'.',name[i],"\n")
    }
    plot(NULL, xlim=c(0,d[2]), ylim=c(0,d[1]), ty='n', xlab="x", ylab="y", asp=1.4)
    rasterImage(as.raster(map),0,0,d[2],d[1])
    pointer = locator(n)
    temp=data.frame(k,name,pointer$x,pointer$y,d[2],d[1])
    names(temp) = c("ss", "place", "x", "y","xlim","ylim")
    temp2=rbind(data,temp)
    my_data=temp2
    return(my_data)
}
for(k in 1:9)
{
  my_data=construct(my_data,na.omit(Name_data[,k]),k)
}
write.csv(my_data,file.choose())

clicks=read.csv(file.choose())

my_plot<-function(clicks)
{
  fitx = lm(x~place+ss-1, clicks)
  fity = lm(y~place+ss-1, clicks)
  m = length(unique(clicks$place))
  n = length(unique(clicks$ss))
  x_ordinate = fitx$coef[1:m]
  xerr = summary(fitx)$coef[1:m,2]
  y_ordinate = fity$coef[1:m]
  yerr = summary(fity)$coef[1:m,2]
  df<-data.frame(x_ordinate,y_ordinate)
  plot(x_ordinate, y_ordinate, ylab="y", xlab="x", col="blue", asp=1)
  text(x_ordinate, y_ordinate, sort(unique(clicks$place)), cex=0.65, pos=1, col="red")
  rect(x_ordinate-xerr,y_ordinate-yerr,x_ordinate+xerr,y_ordinate+yerr)
  u = abs((fitx$res)/as.numeric(clicks$xlim))
  v = abs((fity$res)/as.numeric(clicks$ylim))
  if(fitx$rank < (m+n-1)) print("Error:the sreecshots don't make a connected map.")
  if(max(u)>0.05 | max(v)>0.05) print(paste("Warning:the fiited map is no reliable"))
}
my_plot(clicks)

clicks$ss=factor(clicks$ss)
my_plot(clicks)


new2=data.frame(x_ordinate,y_ordinate)
write.csv(new2,file.choose())
new=read.csv(file.choose())


library(ggplot2)
sp<-ggplot(new,aes(x_ordinate,y_ordinate,label = X))+
  geom_point(colour='darkblue',size=3)
sp
data_text<-data.frame(location=new$X,
                      x=new$x_ordinate,
                      y=new$y_ordinate)
sp=sp+
  geom_text(data=data_text,
            mapping=aes(x=x,
                        y=y,
                        label=location)
            ,colour='red')
q <- ggplotly(sp, dynamicTicks = TRUE)
config(q, scrollZoom = TRUE)%>%layout(plot_bgcolor='#e5ecf6',
                                      xaxis = list(
                                        zerolinecolor = '#ffff',
                                        zerolinewidth = 2,
                                        gridcolor = 'ffff'),
                                      yaxis = list(
                                        zerolinecolor = '#ffff',
                                        zerolinewidth = 2,
                                        gridcolor = 'ffff')
)
name=sort(unique(clicks$place))
k=1
while(k>0)
{
  print(sp)
  cat("Write the two places' name you want to connect by road")
  place1=readline("place1-")
  place2=readline("place2-")
  index1=which(name==place1)
  index2=which(name==place2)
  if(length(index1)==0||length(index2)==0)
  {
    cat("Location Entered does not exist in the map")
    flag=readline("If you want to add more road press y or If you want to stop here press n")
    if(flag=='y')k=k+1
    if(flag=='n')k=0
  }
  else{sp=sp+geom_segment(x=x_ordinate[index1],y=y_ordinate[index1],xend=x_ordinate[index2],yend=y_ordinate[index2],size=1.3)
  flag=readline("If you want to add more road press y or If you want to stop here press n")
  if(flag=='y')k=k+1
  if(flag=='n')k=0}
}


