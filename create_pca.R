library(tidyverse)
library(taRifx)
library(pls)
library(readxl)
runProg<-function(){
  rm(final_df)
  rm(lordf)
  for(s in 1:1){
    loi<-remove.factors(read_xlsx("for_pca.xlsx",s))#,col_types = c("text", "text", "text", "text", "numeric", "numeric","numeric", "numeric")))
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
      nbh<-excel_sheets("for_pca.xlsx")[s]
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

final_df$day<-rowMeans(final_df[,1:24])
#final_df$AMRUSH<-rowMeans(final_df[,20:22])
#a<-melt(final_df[25:28],id=c("neighborhood","id"))
  #Filters need to be changed if going station/not or n1/n2 
plotData2<<-data.frame("id"=final_df$id,"var"=filter(a,a$neighborhood=="Station")$variable,"Station"=filter(a,a$neighborhood=="Station")$value,"No_Station"=filter(a,a$neighborhood=="No Station")$value)
}


data_pca<-read_xlsx("for_pca.xlsx",4)
pcr_model<-pcr(Traffic_Score~.,data=data_pca[2:13],scale=TRUE,validation="CV")
summary(pcr_model)
validationplot(pcr_model)
