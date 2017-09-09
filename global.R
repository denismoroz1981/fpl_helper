#Fantasy Premier League

#Adding libraries
library(shiny)
library(RCurl)
library(jsonlite)
library(ggplot2)
#library(beepr)

#Sourcing current data
link<-getURL("https://fantasy.premierleague.com/drf/bootstrap-static") 
db_PL <-fromJSON(link)
#Sourcing 2016/2017 data
fileTable<-file.path("stat2016_2017.json")
db_PLplayers2017<-fromJSON(fileTable)

#Preprocessing data table
preprocTable<-function(dataTable) {
db_PLplayersProc<-data.frame(dataTable[2]) 
db_teams<-data.frame(dataTable$teams[c("code","short_name")])
# addition of position and team names instead of codes
db_PLplayersProc<-merge(x=db_PLplayersProc,y=db_teams,by.x="elements.team_code",by.y = "code",all.x = TRUE)
db_positions<-data.frame(c(1,2,3,4),c("Goalkeeper","Defender","Midfielder","Forward"))
names(db_positions)<-c("elements.element_type","position")
db_PLplayersProc<-merge(x=db_PLplayersProc,y=db_positions,by="elements.element_type",all.x = TRUE)
# change of colunm names
colnames(db_PLplayersProc)<-sub("elements.","",colnames(db_PLplayersProc))
#colnames(db_PLplayersGEN)[colnames(db_PLplayersGEN)=="elements.web_name"]<-"web_name"
#cost is divided by 10
db_PLplayersProc$now_cost<-db_PLplayersProc$now_cost/10
db_PLplayersProc$points_per_million<-db_PLplayersProc$total_points/db_PLplayersProc$now_cost
#b_PLplayersProc$season<-season
db_PLplayersProc$points_per_game<-as.numeric(db_PLplayersProc$points_per_game)
db_PLplayersProc$influence<-as.numeric(db_PLplayersProc$influence)
db_PLplayersProc$creativity<-as.numeric(db_PLplayersProc$creativity)
db_PLplayersProc$threat<-as.numeric(db_PLplayersProc$threat)
db_PLplayersProc$ict_index<-as.numeric(db_PLplayersProc$ict_index)
return(db_PLplayersProc)
}
#Preprocessing FPL players' statistics
db_PLplayers2018<-preprocTable(db_PL) #processing 2017/2018 statistics
db_PLplayers2018$season<-"2017/2018"
db_PLplayers2017<-preprocTable(db_PLplayers2017) #processing 2016/2017 statistic
db_PLplayers2017$season<-"2016/2017"
db_PLplayersGEN<-rbind(db_PLplayers2017,db_PLplayers2018)

db_PLplayersCUR<-db_PLplayersGEN[db_PLplayersGEN$season=="2017/2018",]

costmin=min(db_PLplayersCUR$now_cost)
costmax=max(db_PLplayersCUR$now_cost)

metrics<-c("now_cost","points_per_million","total_points","goals_scored","assists","clean_sheets","goals_conceded","penalties_saved","penalties_missed","yellow_cards","red_cards","saves","influence","creativity","threat","ict_index","ea_index","minutes","cost_change_start")
seasons<-c("2016/2017","2017/2018")
# Minimal values by position
min_costGK<-sort(db_PLplayersCUR[db_PLplayersCUR$position=="Goalkeeper",]$now_cost,decreasing = FALSE)[1:5]
min_costDF<-sort(db_PLplayersCUR[db_PLplayersCUR$position=="Defender",]$now_cost,decreasing=FALSE)[1:5]
min_costMF<-sort(db_PLplayersCUR[db_PLplayersCUR$position=="Midfielder",]$now_cost,decreasing=FALSE)[1:5]
min_costFW<-min(db_PLplayersCUR[db_PLplayersCUR$position=="Forward",]$now_cost,decreasing=FALSE)[1:5]
min_costTable<-setNames(data.frame(list(min_costGK,min_costDF,min_costMF,min_costMF)),c("Goalkeeper","Defender","Midfielder","Forward"))
min_costTable$Goalkeeper<-cumsum(min_costTable$Goalkeeper)
min_costTable$Defender<-cumsum(min_costTable$Defender)
min_costTable$Midfielder<-cumsum(min_costTable$Midfielder)
min_costTable$Forward<-cumsum(min_costTable$Forward)

