
##FUNCTIONS
##HW8 Data Analysis

#Separate a string with ","
x<-c("n","a","i","v","y")
stc<- function(x){
  str_c(x, collapse=",")
}
stc(x)


#Finding the daily mean grouping by site and date
o3summarize<-function(x){
  out<- group_by(x, site=as.factor(site), date)%>%
    summarize(o3=mean(obs, na.rm=TRUE))
}


#Summarize data with year and month 
o3summarize2<-function(x){
  out2<- group_by(x, date=as.factor(date), date)%>%
    summarize(meano3=mean(o3))
}
yearl<-daily%>%
  map(o3summarize2)
yearl


#Function that shows diurnal pattern

o3summarize3<-function(x){
  out3<- group_by(x, start_hour=as.factor(start_hour[0:12|12:24]), date)%>%
    summarize(o3d=mean(obs, na.rm=TRUE))
}
diurnal<-o3.filelist%>%
  map(o3summarize3)
diurnal
 
or


diurnal <- o3.filelist %>%
  rbind_list() %>%
  filter((start_hour > 7) & (start_hour <19 )) %>%
  group_by(site, date) %>%
  summarize(meanobs = mean(obs, rm.na=TRUE))
diurnal



#Annual daily mean for O3 in Merced, CA

o3.annual.mean<- function(x){
  out<-group_by(x, year, `County Name`)%>%
    summarize(annual.mean=mean(o3, na.rm=TRUE))
}

groupdaily<-group_by(daily.site, year, `County Name`)

groupdaily<- groupdaily%>%
  filter(str_detect(`County Name`, 'Merced'))

o33<- o3.annual.mean(groupdaily)
o33



