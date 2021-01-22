library(tidyverse)
library(taRifx)
library(pls)
library(readxl)
#0 southwest, 1 northeast
data<-read_csv("bus_dat.csv")
colnames(data)<-v$name
#Size of bounding box, +- n_x needs to be larger than +- n_y 
n_y<-.001
n_x<-.002


getDistance<-function(x1,y1,x2,y2){
  return(sqrt((x2-x1)^2 + (y2-y1)^2))
}

# Clear NA values and subsitute with -1 (needed to clean dataset)
na.zero <- function (x) {
  x[is.na(x)] <- -1
  return(x)
}


createLot<-function(transits_df){
  if(nrow(transits_df)<2){return(list("mean" = 0, "count" = 0))}
  n<-0
  lot<-0
  for(i in 1:(nrow(transits_df)-1)){

    n<-n+1
    if(transits_df[i+1,1] - transits_df[i,1] > 300 | getDistance(x,y,transits_df[i,3],transits_df[i,4]) - getDistance(x,y,transits_df[i+1,3],transits_df[i+1,4]) > .0001){
      lot<-c(n,lot)
      n<-0
    }
  }
  r <- list("mean" = mean(lot), "count" = length(lot))
  lot<<-lot
  return(r)
  
}

getCounts<-function(d,t,p){
  loi<-read.xlsx("intersections.xlsx",1)
  date_string<-as.POSIXct(paste(as.Date(d),t,sep=" "),tz="",format="%Y-%m-%d %H:%M:%S")
  
  for(i in 1:nrow(loi)){

    x<-loi[i,3]
    y<-loi[i,4]

    filtered_data <- filter(data,attributes.latitude >x-n_x & attributes.latitude < x+n_x & attributes.longitude > y - n_y & attributes.longitude < y + n_y & attributes.updated_at >date_string & attributes.updated_at < date_string+(p*60))
    print(nrow(filtered_data))
    intersection<-data.frame(datetime = filtered_data$attributes.updated_at, direction = filtered_data$attributes.direction_id, latitude = filtered_data$attributes.latitude, longitude = filtered_data$attributes.longitude)
    r<-createLot(na.zero(filter(intersection,intersection$direction==0)))
    loi[i,5]<-r$mean
    loi[i,7]<-r$count
    r<-createLot(na.zero(filter(intersection,intersection$direction==1)))
    
    loi[i,6]<-r$mean
    loi[i,8]<-r$count
    #loi[i,7]<-nrow(filter(intersection,intersection$direction==0))
    #loi[i,8]<-nrow(filter(intersection,intersection$direction==1))
    print(mean(lot))
    print(loi[i,])
    
  }
  return(loi)

}



loi<-getCounts("2020-01-30","08:00:00",60)


overTimes<-function(){
  loi<-read.xlsx("intersections.xlsx",1)

  for(i in 1:nrow(loi)){
    
    x<-loi[i,3]
    y<-loi[i,4]
    lor<-0
    loavg<--1
    filtered_data <- filter(data,attributes.latitude >x-n_x & attributes.latitude < x+n_x & attributes.longitude > y - n_y & attributes.longitude < y + n_y )
    intersection<-unique(data.frame(datetime = filtered_data$attributes.updated_at, direction = filtered_data$attributes.direction_id, latitude = filtered_data$attributes.latitude, longitude = filtered_data$attributes.longitude))
    for(h in 0:23){
      date_string<-as.POSIXct(paste("2020-01-27","21:00:00",sep=" "),tz="",format="%Y-%m-%d %H:%M:%S")+(h*3600)
      for(d in 0:4){
        test<-filter(intersection,datetime >date_string + (d*24*60*60) & datetime < date_string+(3600)+ (d*24*60*60))
        r<-createLot(na.zero(filter(test,intersection$test==0)))
        
        lor<-c(lot,lor)
      }
      #print(length(lor))
      loavg<-c(mean(lor),loavg)
      #print(loavg)
    }
    #print(loavg)
    print(loavg)
    #loi[i,5]<-loavg
  }
}


testTimes<-function(){
  loi<-remove.factors(read.xlsx("intersections.xlsx",1))
  lodf<-0
  for(i in 1:nrow(loi)){
    x<-loi[i,3]
    y<-loi[i,4]
    lor<-0
    loavg<--1

    filtered_data <- filter(cleanData,attributes.latitude >x-n_x & attributes.latitude < x+n_x & attributes.longitude > y - n_y & attributes.longitude < y + n_y )
    intersection<-unique(data.frame(datetime = filtered_data$attributes.updated_at, direction = filtered_data$attributes.direction_id, latitude = filtered_data$attributes.latitude, longitude = filtered_data$attributes.longitude))
    
    for(h in 0:23){
      date_string<-as.POSIXct(paste("2020-01-27","12:00:00",sep=" "),tz="",format="%Y-%m-%d %H:%M:%S")+(h*3600)
      for(d in 0:4){
        test<-filter(intersection,datetime >date_string + (d*24*60*60) & datetime < date_string+(3600)+ (d*24*60*60))
        r<-createLot(na.zero(filter(test,test$direction==0)))
        lor<-c(r$mean,lor)
      }
      loavg<-c(loavg,mean(lor))
    }
    
    final_loavg<-c(loi[i,1],loi[i,2],loavg)
    print(final_loavg)
    lodf<-rbind(lodf,final_loavg)
  }
  return(lodf)
}


getNeighborhood<-function(){
  lodf<-0
  #for(s in 1:length(excel_sheets("hypotesting.xlsx"))){
  for(s in 1:10){
    loi<-remove.factors(read_xlsx("hypotesting.xlsx",s))#,col_types = c("text", "text", "text", "text", "numeric", "numeric","numeric", "numeric")))




    loavg<--1
    
    #filtered_data <- filter(data,attributes.latitude >x-n_x & attributes.latitude < x+n_x & attributes.longitude > y - n_y & attributes.longitude < y + n_y )
    #intersection<-unique(data.frame(datetime = filtered_data$attributes.updated_at, direction = filtered_data$attributes.direction_id, latitude = filtered_data$attributes.latitude, longitude = filtered_data$attributes.longitude))
    
    for(h in 0:23){
      lor<-0
      date_string<-as.POSIXct(paste("2020-01-27","12:00:00",sep=" "),tz="",format="%Y-%m-%d %H:%M:%S")+(h*3600)
      for(d in 0:4){
        for(i in 1:nrow(loi)){
          
          x<-loi[i,3]
          y<-loi[i,4]
          x<-as.numeric(x[1,1])
          y<-as.numeric(y[1,1])
          filtered_data <- filter(data,attributes.latitude >x-n_x & attributes.latitude < x+n_x & attributes.longitude > y - n_y & attributes.longitude < y + n_y )
          intersection<-unique(data.frame(datetime = filtered_data$attributes.updated_at, direction = filtered_data$attributes.direction_id, latitude = filtered_data$attributes.latitude, longitude = filtered_data$attributes.longitude))
          
        
          test<-filter(intersection,datetime >date_string + (d*24*60*60) & datetime < date_string+(3600)+ (d*24*60*60))
          r<-createLot(na.zero(filter(test,test$direction==0)))
          lor<-c(r$mean,lor)
        }
        
        #loavg<-c(loavg,mean(lor))
      }
      length(lor)
      final_loavg<-c(excel_sheets("hypotesting.xlsx")[s],h,mean(lor))
      print(final_loavg)
      lodf<-rbind(lodf,final_loavg)
    }
  

  }
  return(lodf)
}

t.test(as.numeric(unlist(flatten(remove.factors(filter(q,q$V1=="newton1")[3])[1]))),as.numeric(unlist(flatten(remove.factors(filter(q,q$V1=="newton1")[3])[1]))))

tab<-tibble(n1=remove.factors(filter(as.data.frame(hypotest),as.data.frame(hypotest)$V1=="n1"))$V3,n2=remove.factors(filter(as.data.frame(hypotest),as.data.frame(hypotest)$V1=="n2"))$V3,m1=remove.factors(filter(as.data.frame(hypotest),as.data.frame(hypotest)$V1=="m1"))$V3,m2=remove.factors(filter(as.data.frame(hypotest),as.data.frame(hypotest)$V1=="m2"))$V3)



plotData<-function(df){
  meanDf<-remove.factors(as.data.frame(t(summarize_all(group_by(final_df,id),mean))))
  meanDf<-cbind(meanDf,row.names(meanDf))
  colnames(meanDf)<-c("newton","medford","names")
  meanDf<-meanDf[2:25,1:3]
  
  ggplot(meanDf, aes(newton,medford)) + geom_point() + geom_text(aes(label=names))
}

plotIntersections<-function(){
a<-melt(final_df[25:28],id=c("neighborhood","id"))
plotData2<-data.frame("id"=final_df$id,"var"=filter(a,a$neighborhood=="Station")$variable,"Station"=filter(a,a$neighborhood=="Station")$value,"No_Station"=filter(a,a$neighborhood=="No Station")$value)
ggplot(plotData2, aes(x=Station, y=No_Station, col=var)) + geom_point() + geom_text(aes(label=plotData2$id),nudge_y=.1)
}

runProg<-function(){
rm(final_df)
rm(lordf)
for(s in 1:2){
  loi<-remove.factors(read_xlsx("hypotesting.xlsx",s))#,col_types = c("text", "text", "text", "text", "numeric", "numeric","numeric", "numeric")))
  for(i in 1:nrow(loi)){
    
    lor<-0
    x<-loi[i,4]
    y<-loi[i,5]
    x<-as.numeric(x[1,1])
    y<-as.numeric(y[1,1])
    filtered_data <- filter(data,attributes.latitude >x-n_x & attributes.latitude < x+n_x & attributes.longitude > y - n_y & attributes.longitude < y + n_y )
    intersection<-unique(data.frame(datetime = filtered_data$attributes.updated_at, direction = filtered_data$attributes.direction_id, latitude = filtered_data$attributes.latitude, longitude = filtered_data$attributes.longitude))
    
    
    for(h in 0:23){
      date_string<-as.POSIXct(paste("2020-01-27","12:00:00",sep=" "),tz="",format="%Y-%m-%d %H:%M:%S")+(h*3600)
      for(d in 0:4){
        test<-filter(intersection,datetime >date_string + (d*24*60*60) & datetime < date_string+(3600)+ (d*24*60*60))
        r<-createLot(na.zero(filter(test,test$direction==0)))
        lor<-c(r$mean,lor)
        
      }
      ifelse(!exists("lordf"),lordf<-data.frame(mean(lor)),lordf<-cbind(lordf,data.frame(mean(lor))))
      
      colnames(lordf)[h+1] <- str_c("h",as.character(h))
      
      lor<-0
      
    }
    nbh<-excel_sheets("hypotesting.xlsx")[s]
    lordf<-cbind(lordf,data.frame("neighborhood"=nbh))
    lordf<-cbind(lordf,data.frame("id"=loi[i,1]))
    print(as.character(loi[[i,1]]))
    
    
    if(!exists("final_df")){
      final_df<-lordf
    } else {
      final_df<-rbind(final_df,lordf)
      
    }
 
    rownames(final_df)[i+(nrow(loi)*(s-1))]<-str_c(nbh,as.character(loi[[i,1]]))
    rm(lordf)
    
    
  }

}
k<-0
for(i in 1:(nrow(final_df)/2)){
  
  p<-as.data.frame(final_df[i,])[1:24]
  row.names(p)<-final_df$neighborhood[i]
  colnames(p)<-str_c(colnames(p),final_df$id[i])
  k<-cbind(k,p)
  
  
}
l<-0
for(i in ((nrow(final_df)/2)+1):nrow(final_df)){
  p<-as.data.frame(final_df[i,])[1:24]
  row.names(p)<-final_df$neighborhood[i]
  colnames(p)<-str_c(colnames(p),final_df$id[i])
  l<-cbind(l,p)
  
  
}
final_df$PMRUSH<-rowMeans(final_df[,5:8])
final_df$AMRUSH<-rowMeans(final_df[,20:22])
a<-melt(final_df[25:28],id=c("neighborhood","id"))
#Filters need to be changed if going station/not or n1/n2
plotData2<<-data.frame("id"=final_df$id,"var"=filter(a,a$neighborhood=="Station")$variable,"Station"=filter(a,a$neighborhood=="Station")$value,"No_Station"=filter(a,a$neighborhood=="No Station")$value)
ggplot(plotData2, aes(x=No_Station, y=Station, col=var)) + geom_point() + geom_text(aes(label=plotData2$id),nudge_y=.1)
}


ggplot(plotData2, aes(x=id, y=delta, col=var)) + geom_point() + geom_text(aes(label=plotData2$id),nudge_y=.1)+scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9),labels=c(1,2,3,4,5,6,7,8,9))


