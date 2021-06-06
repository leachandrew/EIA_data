if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/EIA_data_pulls")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/EIA_data_pulls")
print(getwd())
source("../andrew_base.R")

library(tidyverse)
library(lubridate)
library(readxl)
library(reshape2)
library(scales)


eia_fix_dates<-function(data_sent)
{
  data_sent$date=ymd(rownames(data_sent))
  rownames(data_sent)<-NULL
  data_sent[]
}
 
steo_data_fetch<-function(date_sent){
#for any month and year, get the STEO Price outlook
#testing
  #date_sent<-ymd("2019-9-01")
  #month_sent should be lower case month.abb

#convert date to month/year notation used by eia
month_sent<-tolower(month.abb[month((date_sent))])
year_sent<-sprintf('%02d', year(date_sent)%% 100)

#before jun2013 they are xls files
file_date<-ymd(paste(year_sent,month_sent,1,sep="-"))
excel_file<-ifelse(file_date>ymd("2013-06-01"),
                   paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xlsx"),
                   paste0("https://www.eia.gov/outlooks/steo/archives/",month_sent,year_sent,"_base.xls"))
temp_file<-ifelse(file_date>ymd("2013-06-01"),
                  paste0("steo_",month_sent,"_",year_sent,".xlsx"),
                  paste0("steo_",month_sent,"_",year_sent,".xls"))
download.file(excel_file,mode = "wb",destfile = temp_file)
  dates<-read_excel(path=temp_file,sheet = "2tab",range = "C3:C4",col_names = F)
  names(dates)[1]<-"X__1"
year_start<-dates$X__1[1]
month_start<-grep(dates$X__1[2],month.abb)

#process price outlook
price_outlook<-read_excel(path=temp_file,sheet = "2tab",range = "A5:BV40",na="n/a")
names(price_outlook)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
#drop electricity and refined product headers
price_outlook<-price_outlook[-c(4,26),]
#rows which could be headers
headers<-grep("TRUE",is.na(price_outlook[,1]))
#for each header, the next x rows get a concatenated header
price_outlook$Header<-NA
price_outlook$Header[1]<-"Crude Oil"
for(j in headers){
  #print(price_outlook$Region[j])
  price_outlook$Header[[j]]<-price_outlook$Region[[j]]
  }
price_outlook<-price_outlook %>% fill(Header)
price_outlook<-price_outlook[!is.na(price_outlook$code),]
price_outlook<-melt(price_outlook,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
price_outlook$table<-"2tab"
price_outlook<-price_outlook %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)
#file ends up with columns code, Region, Header, Date, value, forecast, version

#process non_opec_supply
crude_supply_data<-read_excel(path=temp_file,sheet = "3atab",range = "A5:BV47",na="n/a")
names(crude_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
crude_supply_data<-crude_supply_data[rowSums(is.na(crude_supply_data)) != ncol(crude_supply_data),]
headers<-grep("TRUE",is.na(crude_supply_data[,1]))
#for each header, the next x rows get a concatenated header
crude_supply_data$Header<-NA
crude_supply_data$Header[1]<-"Supply (million barrels per day) (a)"
for(j in headers){
  #print(price_outlook$Region[j])
  crude_supply_data$Header[[j]]<-crude_supply_data$Region[[j]]
}
crude_supply_data<-crude_supply_data %>% fill(Header)
crude_supply_data<-crude_supply_data[!is.na(crude_supply_data$code),]
crude_supply_data<-melt(crude_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
crude_supply_data$table<-"3atab"
crude_supply_data<-crude_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)
#file ends up with columns code, Region, Header, Date, value, forecast, version

#process non_opec_supply
non_opec_supply_data<-read_excel(path=temp_file,sheet = "3btab",range = "A5:BV50",na="n/a")
names(non_opec_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
non_opec_supply_data<-non_opec_supply_data[rowSums(is.na(non_opec_supply_data)) != ncol(non_opec_supply_data),]
non_opec_supply_data$Header<-"Petroleum Supply  (million barrels per day)"
non_opec_supply_data<-melt(non_opec_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
non_opec_supply_data$table<-"3btab"
non_opec_supply_data<-non_opec_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)

#process opec_data
opec_supply_data<-read_excel(path=temp_file,sheet = "3ctab",range = "A4:BV55",na=c("n/a","-"))
names(opec_supply_data)<-c("code","Region",format(seq.Date(from=ymd(paste(year_start,month_start,1,sep="-")),by="1 month",length.out=72)))
opec_supply_data<-opec_supply_data[rowSums(is.na(opec_supply_data)) != ncol(opec_supply_data),]
opec_supply_data$Header<-NA
headers<-grep("TRUE",is.na(opec_supply_data[,1]))
#for each header, the next x rows get a concatenated header
for(j in headers){
  #print(price_outlook$Region[j])
  opec_supply_data$Header[[j]]<-opec_supply_data$Region[[j]]
}
opec_supply_data<-opec_supply_data %>% fill(Header)
opec_supply_data<-opec_supply_data[!is.na(opec_supply_data$code),]
opec_supply_data<-melt(opec_supply_data,id=c("code","Region","Header"),variable.name = "Date",value.name ="value")
opec_supply_data$table<-"3ctab"
opec_supply_data<-opec_supply_data %>% mutate(
  Date=ymd(Date),
  forecast=ifelse(Date>=file_date,1,0),
  version=file_date)


#stack everthing

steo_data<-rbind(price_outlook,crude_supply_data,non_opec_supply_data,opec_supply_data)

steo_data
}


#steo_data<-steo_data_fetch(ymd("2018-12-1"))

#get historic data
#steo_data<-steo_data_fetch(ymd("2018-12-1"))

#find current issue of STEO - latest it could be out is the 12th, but let's use the 15th
#steo_date<-as.Date(ifelse(day(Sys.Date())>=11,Sys.Date(),Sys.Date()-months(1)))
steo_date<-Sys.Date()
steo_data0<-filter(steo_data_fetch(steo_date),Date>=ymd("2019-1-01"),forecast==0)
get_history<-0
if(get_history==1)
  {
  steo_data1<-filter(steo_data_fetch(ymd("2019-1-1")),Date>=ymd("2015-01-01"),forecast==0)
  #2011-2014 histories
  steo_data2<-filter(steo_data_fetch(ymd("2015-1-1")),forecast==0)
  #2007-2010 histories
  steo_data3<-filter(steo_data_fetch(ymd("2011-1-1")),forecast==0)
  #2004-2007 histories
  steo_data4<-filter(steo_data_fetch(ymd("2008-1-1")),Date<ymd("2007-01-01"),forecast==0)
  steo_history<-rbind(steo_data4,steo_data3,steo_data2,steo_data1)
  save(steo_history,file="steo_history.RData")
  }

load("steo_history.RData")
steo_history<-rbind(steo_history,steo_data0)

#add forecasts

steo_forecast<-filter(steo_data_fetch(steo_date),forecast==1)
steo_data<-rbind(steo_history,steo_forecast)




#global supply and demand
#the brackets mess up filter, so this is a fix
supply_demand<-steo_data %>%filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")))
#find forecast dates
min_forecast<-min(supply_demand$Date[supply_demand$forecast==1])
max_forecast<-max(supply_demand$Date[supply_demand$forecast==1])

#historical demand forecasts
steo_old_sd_forecasts<-filter(steo_data_fetch(ymd("2020-1-1")),Date>=ymd("2015-01-01"),forecast==1) %>%
  rbind(filter(steo_data_fetch(ymd("2020-2-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-3-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-4-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  rbind(filter(steo_data_fetch(ymd("2020-5-1")),Date>=ymd("2015-01-01"),forecast==1))%>%
  filter(code %in% c("patc_world","papr_world"))%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))

graph_df<-supply_demand%>%
  mutate(Region=as_factor(Region),
         Region=fct_collapse(Region,`Total World Supply` = c("Total World Supply", "Total World Production")),
         version=factor(paste(format(max(supply_demand$version), "%b %Y"), "forecast"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"forecast")))%>%
  bind_rows(steo_old_sd_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")),
         version=factor(paste(month.abb[month(version)],year(version),"forecast"),
                        levels=paste(month.abb[sort(month(unique(version)))],sort(year(unique(version))),"forecast")))


forecast_label<-paste(format(max(supply_demand$version), "%b %Y"), "forecast")
other_versions<-graph_df %>% filter(forecast==1,version!=forecast_label) %>% select(version) %>% unique()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("demand_only.png")
ggplot(filter(graph_df,Region=="Total World Consumption",forecast==0,Date>ymd("2018-01-01")))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.5)+
  geom_line(data=filter(graph_df,Region=="Total World Consumption",forecast==1),
            aes(Date,value,group=version,colour=version,linetype="STEO Forecast"),size=1.5)+
  scale_y_continuous(breaks=pretty_breaks())+
  scale_linetype_manual("",values=c(1,2))+
  #scale_color_manual("",values=tail(brewer.pal(6,"Greys"),4))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y",expand=c(0,0),limits = c(ymd("2018-01-01"),ymd("2022-12-01")))+
  guides(col = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y =element_text(size = 14,face = "bold", colour="black"),
    axis.title.y.right = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.y = element_text(margin = margin(l = 10,r=10),size = 14,face = "bold", colour="black"),
    axis.title.x = element_text(margin = margin(l = 10,r=10,t=10,b=10),size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(y="Global Consumption (MMbbl/d)",x="Year",
       title=paste(" International Petroleum and Other Liquids Consumption"),
       subtitle=paste("Historic Values and EIA Short Term Energy Outlook (STEO) Forecasts"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("demand_new.png")
ggplot(filter(graph_df,Region=="Total World Consumption",forecast==0,Date>ymd("2018-01-01")))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.25)+
  geom_line(data=filter(graph_df,Region=="Total World Consumption",forecast==1),
            aes(Date,value,group=version,colour=version,linetype="STEO Forecast"),size=1.25)+
  geom_point(data=filter(graph_df,Region=="Total World Consumption",forecast==1),
             aes(Date,value,group=version,shape=version,colour=version,fill=version),size=2.5)+
  
  #geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y")+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  theme_minimal()+weekly_graphs()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  labs(y="Global Liquids Demand (million barrels per day)",x="",
       title=paste("Global Liquids Demand and EIA Forecasts"),
       subtitle=paste("Historic Values and Short Term Energy Outlook Forecasts"),
       caption="Source: Data via EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("supply_new.png")
ggplot(filter(graph_df,Region=="Total World Supply",forecast==0,Date>ymd("2018-01-01")))+
  geom_line(aes(Date,value,group=version,linetype="Historic Data"),size=1.25)+
  geom_line(data=filter(graph_df,Region=="Total World Supply",forecast==1),
            aes(Date,value,group=version,colour=version,linetype="STEO Forecast"),size=1.25)+
  geom_point(data=filter(graph_df,Region=="Total World Supply",forecast==1),
             aes(Date,value,group=version,shape=version,colour=version,fill=version),size=2.5)+
  
  #geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y")+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  theme_minimal()+weekly_graphs()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  labs(y="Global Liquids Demand (million barrels per day)",x="",
       title=paste("Global Liquids Supply and EIA Forecasts"),
       subtitle=paste("Historic Values and Short Term Energy Outlook Forecasts"),
       caption="Source: Data via EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#WTI PRICE FORECASTS

#historical demand forecasts
steo_old_WTI_forecasts<-filter(steo_data_fetch(steo_date-months(5)),forecast==1) %>%
    bind_rows(filter(steo_data_fetch(steo_date-months(4)),forecast==1))%>%
  bind_rows(filter(steo_data_fetch(steo_date-months(3)),forecast==1))%>%
  bind_rows(filter(steo_data_fetch(steo_date-months(2)),forecast==1))%>%
  bind_rows(filter(steo_data_fetch(steo_date-months(1)),forecast==1))%>%
  filter(code %in% c("WTIPUUS"))%>%
  mutate(value=as.numeric(value),
         Region=as_factor(Region),
         version=factor(paste(month.abb[month(version)],year(version),"STEO"),
                        levels=paste(month.abb[month(unique(version))],year(unique(version)),"STEO")))
  
#Oct 2013 STEO Apr 2017 STEO Jan 2020 STEO

wti_fc<-steo_data %>%filter(code %in% c("WTIPUUS"))%>%
  mutate(Region=as_factor(Region),
           version=factor(paste(format(max(supply_demand$version), "%b %Y"), "STEO"),
                          levels=paste(month.abb[month(unique(version))],year(unique(version)),"STEO")))%>%
  as.data.frame()%>%
  bind_rows(steo_old_WTI_forecasts)%>%
  mutate(version=mdy(paste(substr(as.character(version),1,3),1,substr(as.character(version),4,8),sep=" ")))%>%
  mutate(version=factor(paste(month.abb[month(version)],year(version),"STEO forecast",sep = " "),
  levels=paste(month.abb[month(unique(sort(version)))],year(unique(sort(version))),"STEO forecast"))
  )%>%
  filter((forecast==0)|(mod(month(Date),2)==0)) #keep every second month of forecasts



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("wti_fcast.png")
ggplot(filter(wti_fc,Date>ymd("2013-01-01")))+
  geom_line(aes(Date,value,colour=version,group=version,linetype="B"),size=1.5)+
  geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
  geom_point(data=filter(wti_fc,Date>ymd("2013-01-01"),mod(month(Date),2)==0),
             aes(Date,value,group=version,shape=version,colour=version,fill=version),size=2.5)+
  
  #geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
  #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
  #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y")+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2))+
  scale_size_manual("",values=c(0,rep(2.5,6)))+
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  theme_minimal()+weekly_small()+
  guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
         fill = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2))+
  labs(y="WTI Spot Monthly Average ($/bbl)",x="",
       title=paste("WTI Monthly Average Spot Price Forecast"),
       subtitle=paste("Historic Values and EIA Short Term Energy Outlook (STEO) Forecasts"),
       caption="Source: EIA STEO, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

nymex<-read_csv("ftp://ftp.cmegroup.com/pub/settle/nymex_future.csv")%>%clean_names()
nymex_test<-nymex %>%
  filter(product_description=="Crude Oil Last Day Financial Futures")%>%
  mutate(forecast=3,Date=ymd(paste(contract_year,contract_month,15)),
         version=paste("NYMEX WTI Futures",format(max(mdy(tradedate)),"%b %d, %Y")))%>%
  rename("value"="settle")%>%
  select(Date,value,version,forecast)%>%
  filter(year(Date)<2023,mod(month(Date),2)==0) #keep every second month


#wti_futs<-read_csv("nymex_wti_rec.csv")
#nymex_test<-wti_futs %>% filter(biz_dt==ymd("2020-04-29")) %>%
#  mutate(Date=ymd(paste(substr(mmy,1,4),substr(mmy,5,6),1,sep = "-")),
#         version=paste("NYMEX WTI Futures",format(biz_dt,"%b %d, %Y")),
#         forecast=3)%>%
#  rename("value"="settle_price")%>%
#  select(Date,value,version,forecast)%>% as.data.frame()

forecasts<-levels(wti_fc$version)
forwards<-unique(nymex_test$version)

wti_fcg<-wti_fc %>% bind_rows(nymex_test) %>%
  mutate(version=ifelse(forecast==0,"Historical Data",version),
        version=factor(version,levels=c("Historical Data",forecasts,forwards)),
         forecast=factor(forecast),
         )
  
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("wti_fcast_nymex.png")

  ggplot(filter(wti_fcg,Date>ymd("2013-01-01"),Date<ymd("2022-1-1")))+
  geom_line(aes(Date,value,colour=version,group=version),size=0.85)+
  
  geom_point(aes(Date,value,group=version,shape=version,colour=version),size=2.5)+
    
    #geom_line(data=filter(wti_fc,Date>ymd("2013-01-01"),forecast==0),aes(Date,value,linetype="A"),size=1.5,colour="black")+
    #geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020",linetype="AB_Budget_2020"),size=1.5)+
    #geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")+
    scale_x_date(breaks = "12 months",date_labels = "%b\n%Y")+
    scale_shape_manual("",values=c(32,15,16,17,18,0,1,2))+
    scale_size_manual("",values=c(0,rep(2.5,6)))+
    scale_y_continuous(breaks=pretty_breaks())+
    #scale_linetype_manual("",values=c(1,1))+
    scale_color_viridis("",discrete = T,option="A",direction = 1,end = .9)+
    scale_fill_viridis("",discrete = T,option="A",direction = 1,end=.9)+
    #scale_linetype_manual("",values=c(1,2,4),labels=c("Historical Data","Forecast","Futures Market Settlement"))+
    scale_linetype_manual("",values=c(1,2,4))+
    #scale_fill_manual("",values=colors_tableau10()[2])+
    #ajl_line()+
    theme_minimal()+weekly_small()+
    guides(shape = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
           linetype = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2),
           colour = guide_legend(keywidth = unit(1.6,"cm"),nrow = 2)
           )+
    labs(y="WTI Spot Monthly Average ($/bbl)",x="",
         title=paste("WTI Monthly Average Spot Price Forecast"),
         subtitle=paste("Historic Values, EIA Short Term Energy Outlook (STEO) Forecasts and Futures Market Settlements"),
       caption="Source: Data via CME Group and EIA, graph by Andrew Leach.")
dev.off()
  


budget_2020 <- data.frame("Date" = c("2017-10-1"	,"2018-10-01","2019-10-01","2020-10-01","	2021-10-01","	2022-10-01"), 
                          "WTI_CAD" = c(68.83,82.27,76.82,75.82,80.52,81.29),
                          "WTI" = c(53.69,62.77,58.00,58.00,62.00,63.00),stringsAsFactors = F)

budget_2020$Date<-ymd(budget_2020$Date)

budget_2020$version<-"Alberta Budget 2020"
budget_2020$version<-factor(budget_2020$version)


budget_2020 <- budget_2020 %>% rename("value"="WTI") %>% select(-WTI_CAD) %>%filter(Date>=ymd("2019-10-01"))%>%
  mutate(forecast=3)

wti_AB<-bind_rows(wti_fc,budget_2020)%>%
  mutate(version=factor(version,levels = c(levels(wti_fc$version),"Alberta Budget 2020")))

wti_AB<- bind_rows(wti_AB,nymex_test)%>%
  mutate(version=factor(version,levels = c(levels(wti_AB$version),unique(nymex_test$version))))

#library(RSelenium)
#checkForServer()
#startServer()
##startServer(javaargs="\Users\aleach\Documents\R Files\chromedriver") #path to where chromedriver is located on local hard (downloaded from: https://sites.google.com/a/chromium.org/chromedriver/downloads)
#remDr <- remoteDriver(browserName = "chrome") 

#remDr <- remoteDriver(browserName="firefox")
#remDr$open()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("wti_fcast_AB.png")
ggplot(filter(wti_AB,Date>ymd("2014-01-01"),forecast!=0))+
  geom_line(aes(Date,value,colour=version,group=version),size=1.25)+
  geom_line(data=filter(wti_AB,Date>ymd("2014-01-01"),forecast==0),aes(Date,value),size=1.25,colour="black")+
  geom_point(data=filter(wti_AB,Date>ymd("2014-01-01"),forecast!=0,mod(month(Date),5)==0),
             aes(Date,value,group=version,shape=version,colour=version,fill=version),size=2.5)+
  
  scale_x_date(breaks = "12 months",date_labels = "%b\n%Y")+
  scale_shape_manual("",values=c(15,16,17,18,0,1,2,9,7))+
  
  scale_y_continuous(breaks=pretty_breaks())+
  #scale_linetype_manual("",values=c(1,1))+
  scale_color_viridis("",discrete = T,option="A",direction = -1,end = .9)+
  scale_fill_viridis("",discrete = T,option="A",direction = -1,end=.9)+
  
  #scale_linetype_manual("",values=c(1,2),labels=c("Historical Data","Forecast"))+
  #scale_fill_manual("",values=colors_tableau10()[2])+
  #ajl_line()+
  theme_minimal()+weekly_small()+
  guides(col = guide_legend(keywidth = unit(1.6,"cm")))+
  labs(y="WTI Price ($/bbl)",x="",
       title=paste("WTI outlook based on institutional forecasts and forward markets"),
       subtitle=paste("Historic values, EIA Short Term Energy Outlook (STEO) forecasts, NYMEX futures prices, and Alberta Budget 2020 assumptions"),
       caption="Sources: EIA STEO, Alberta Budget 2020, and CME. Graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#END WTI PRICE
