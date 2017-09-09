#Fantasy Premier League
library(shiny)

shinyServer(function(input,output,session) {
 
db_PLplayersSel<-reactive({
  tinfo<-subset(db_PLplayersGEN,short_name %in% input$cbg_teams & position %in% input$rb_positions & now_cost>input$sli_cost_range[1] & now_cost<input$sli_cost_range[2]&season=="2017/2018")[,c("first_name","short_name","web_name","form","in_dreamteam","news")]
  tinfo$full_name<-paste(tinfo$first_name,tinfo$web_name)
  tx<-subset(db_PLplayersGEN,short_name %in% input$cbg_teams & position %in% input$rb_positions & season %in% input$si_x_season)[,c("web_name","first_name",input$si_x)]
  names(tx)[3]<-paste(input$si_x_season,input$si_x)
  tx$full_name<-paste(tx$first_name,tx$web_name)
  ty<-subset(db_PLplayersGEN,short_name %in% input$cbg_teams & position %in% input$rb_positions & season %in% input$si_y_season)[,c("web_name","first_name",input$si_y)]
  names(ty)[3]<-paste(input$si_y_season,input$si_y)
  ty$full_name<-paste(ty$first_name,ty$web_name)
  db_PLplayersSel<-merge(tinfo,tx,by="full_name",all.x=TRUE)
  db_PLplayersSel<-merge(db_PLplayersSel,ty,by="full_name",all.x=TRUE)
  db_PLplayersSel<-db_PLplayersSel[order(db_PLplayersSel[,3],db_PLplayersSel[,4],decreasing = TRUE),]
  db_PLplayersSel[,!names(db_PLplayersSel) %in% c("first_name","web_name.x","first_name.x","web_name.y","first_name.y")]
  })   
  #Output figures
output$t_PLplayers<-renderDataTable({
  db_PLplayersSel<-db_PLplayersSel()
 db_PLplayersSel<-db_PLplayersSel[,!names(db_PLplayersSel) %in% c("web_name")]
  db_PLplayersSel })
#output$info<-renderPrint({
 #   nearPoints(db_PLplayersGEN,input$plot_hover)
  
#})

#Output figures


output$p_PLplayers<-renderPlot({
  database<<-db_PLplayersSel()
  ggplot(database,aes(x=database[,6],y=database[,8],hjust=0,vjust=0))+geom_point(aes(colour=factor(short_name)))+geom_text(aes(label=web_name))+theme(legend.title = element_blank())+labs(x=colnames(database)[6],y=colnames(database)[8])+geom_smooth(method = "lm",se=FALSE,linetype="dashed",size=0.5)
  #plot(db_PLplayersSelect[,3],db_PLplayersSelect[,4],col=factor(db_PLplayersSelect$short_name),pch=19)
  #text(db_PLplayersSelect[,1],db_PLplayersSelect[,2],labels = db_PLplayersSelect[,"elements.web_name"],cex=0.7,pos=3)
  #legend("right",unique(db_PLplayersSelect$short_name),col=1:lenth(1:unique(db_PLplayersSelect$short_name),pch=19))
  })

#Output figures
output$Columns<-renderDataTable(
  setNames(do.call(rbind.data.frame,as.list(names(db_PLplayersGEN))),"Columns")
             )

#Uncheck button event
output$checkbox<-renderUI(
  checkboxGroupInput("cbg_teams",label="Choose team:",
                     choices=unique(db_PLplayersCUR$short_name),
                      selected=unique(db_PLplayersCUR$short_name)
                        ))

observeEvent(input$uncheck, {
 output$checkbox<-renderUI({
  checkboxGroupInput("cbg_teams",label="Choose team:",
                       choices=unique(db_PLplayersGEN$short_name))
    
                          })
                  })
 #Metrics
output$text_metrics<-renderDataTable(
  setNames(do.call(rbind.data.frame,as.list(metrics)),"Metrics"),options = list(searching=FALSE,paging=FALSE)
  )

#Total fund
output$text_fundTotal<-renderText({
  paste("Your total fund is £",input$ni_fundGK+input$ni_fundDF+input$ni_fundMF+input$ni_fundFW,"mln")
  })
  
#Generate team button

observeEvent(input$ab_Generate,{
  #min_fund<-min_costTable[input$si_numGK,"Goalkeeper"]+min_costTable[input$si_numDF,"Defender"]+min_costTable[input$si_numMF,"Midfielder"]+min_costTable[input$si_numFW,"Forward"]
  ptm<-Sys.time()
  if(input$si_numDF>0) min_fund<-min_costTable[input$si_numDF,"Defender"] else min_fund<-0
  if(input$si_numFW>0)  min_fund<-min_fund+min_costTable[input$si_numFW,"Forward"]
  if (input$si_numGK>0) min_fund<-min_fund+min_costTable[input$si_numGK,"Goalkeeper"]
  if (input$si_numMF>0) min_fund<-min_fund+min_costTable[input$si_numMF,"Midfielder"]
  
  
  if (min_fund>input$ni_fund) {
    showNotification(paste("You money fund is too little. Needed more than £ mln.",min_fund))
    return()
  }
  db_PLplayersSelect<-db_PLplayersGEN[db_PLplayersGEN$season==input$si_season_metric,c("web_name","short_name","position","now_cost","form","in_dreamteam","news",input$si_metric,"id")]
  db_PLplayersSelect$metric<-as.integer(db_PLplayersSelect[,8])/db_PLplayersSelect$now_cost
  db_PLplayersSelect<-db_PLplayersSelect[order(db_PLplayersSelect$metric,decreasing = TRUE),]
  #db_PLplayersSelect<-db_PLplayersSelect[round(db_PLplayersSelect$metric,digits = 0)>5,]
  numTeam<-as.integer(input$si_numGK)+as.integer(input$si_numDF)+as.integer(input$si_numMF)+as.integer(input$si_numFW)
  #min_costTable$Selection<-c(input$si_numGK,input$si_numDF,input$si_numMF,input$si_numFW)
  limits<-setNames(c(as.integer(input$si_numDF),as.integer(input$si_numFW),as.integer(input$si_numGK),as.integer(input$si_numMF)),c("Defender","Forward","Goalkeeper","Midfielder"))

  #f_stop<-function() {browser()}
  
    #functions
    f_checkTeam<-function(team) {
      list_teams_sum<-table(db_PLplayersChosen$short_name)
      list_teams_sum<-unlist(labels(list_teams_sum[list_teams_sum>2]))
      
      #if (nrow(db_PLplayersChosen[db_PLplayersChosen$short_name=="CHE",])==3 & team=="CHE") {f_stop()}
      if (team %in% list_teams_sum) {return(TRUE)} else {return(FALSE)}
    }
    f_checkPosition<-function(position,list_positions) {
      #limits<-setNames(c(as.integer(input$si_numDF),as.integer(input$si_numFW),as.integer(input$si_numGK),as.integer(input$si_numMF)),c("Defender","Forward","Goalkeeper","Midfielder"))
      #list_positions<-table(db_PLplayersChosen$position)
      positions_intersect<-intersect(names(list_positions),names(limits))
      list_positions<-list_positions[list_positions[positions_intersect]>limits[positions_intersect]]
      list_positions<-names(list_positions)
      if (position %in% list_positions) {return(TRUE)} else {return(FALSE)}
    }
    #f_checkLimit<-function(position) {
                              #checkSum<-sum(db_PLplayersChosen$now_cost)
     #                         if (checkSum >= input$ni_fund) {return(TRUE)} else {return(FALSE)}
      #                        }
    f_lookover<-function(v_start) {
      for (i in seq(v_start,nrow(db_PLplayersSelect))) {
        #withProgress(message = i, detail="This may take a while...",value=0,{
        checkSum<-sum(db_PLplayersChosen$now_cost)+db_PLplayersSelect[i,"now_cost"]
        if (input$ni_fund<=checkSum) {next}
        
        if (f_checkTeam(db_PLplayersSelect[i,"short_name"])) {next}
        list_positions<-c(Defender=0,Forward=0,Goalkeeper=0,Midfielder=0)
        list_positions["Defender"]<-length(db_PLplayersChosen$position[db_PLplayersChosen$position=="Defender"])
        list_positions["Forward"]<-length(db_PLplayersChosen$position[db_PLplayersChosen$position=="Forward"])
        list_positions["Goalkeeper"]<-length(db_PLplayersChosen$position[db_PLplayersChosen$position=="Goalkeeper"])
        list_positions["Midfielder"]<-length(db_PLplayersChosen$position[db_PLplayersChosen$position=="Midfielder"])
        
        #if (f_checkPosition(db_PLplayersSelect[i,"position"],list_positions)) {next} 
        
        list_positions[db_PLplayersSelect[i,"position"]]<-list_positions[db_PLplayersSelect[i,"position"]]+1
        rest_positions<-limits-list_positions
        if(rest_positions[db_PLplayersSelect[i,"position"]]<0) {next}
        rest_fund<-ifelse(rest_positions["Defender"]>0,min_costTable[rest_positions["Defender"],"Defender"],0)
        rest_fund<-rest_fund+ifelse(rest_positions["Forward"]>0,min_costTable[rest_positions["Forward"],"Forward"],0)
        rest_fund<-rest_fund+ifelse(rest_positions["Goalkeeper"]>0,min_costTable[rest_positions["Goalkeeper"],"Goalkeeper"],0)
        rest_fund<-rest_fund+ifelse(rest_positions["Midfielder"]>0, min_costTable[rest_positions["Midfielder"],"Midfielder"],0)
        if (checkSum+rest_fund>input$ni_fund) {next}
        db_PLplayersChosen<<-rbind(db_PLplayersChosen,db_PLplayersSelect[i,])
        if (nrow(db_PLplayersChosen)==numTeam) {break}
        
      }
      
    }
    
    db_PLplayersChosen<<-setNames(data.frame(matrix(ncol=9,nrow=0)),c("web_name","short_name","position","now_cost","form","in_dreamteam","news",input$si_metric,"id"))
    #looking through
    v_start<-1
    v_lastItemInSelect<<-1
    f_lookover(v_start)
    k<-0
    #browser()
    
    
    #trial with apply
    #f_lookApply<-function(v_start) {
    #  apply(db_PLplayersSelect[(db_PLplayersSelect),1,function(x)
     #   {
        
      #checkSum<-sum(db_PLplayersSelect[db_PLplayersSelect$IsSelected]$now_cost)+as.numeric(x["now_cost"])
      #list_positions<-table(db_PLplayersSelect[db_PLplayersSelect$IsSelected]$position)
      #if (!(f_checkTeam(x["short_name"])) & !(f_checkPosition(x["position"],list_positions)) & !(f_checkSum(x["now_cost"],checkSum)))
      #{x["IsSelected"]<-TRUE}
      
    #})
    #}
    #----------------------------------
   
    #withProgress(message = "Generation in progress", detail="This may take a while...",value=0,
                 
                 #f_lookApply(v_start)
    # If less than number needed, first iteration once more
      #if (nrow(db_PLplayersChosen)<numTeam) {
        #GK
        #db_GK<-db_PLplayersSelect[db_PLplayersSelect$position=="Goalkeeper",]
        #db_GK<-db_GK[order(db_GK$now_cost,decreasing=TRUE),]
        #db_GK$cost_rank<-seq(1:nrow(db_GK))
        #db_GK<-db_GK[order(db_GK$metric,decreasing=TRUE),]
        #db_GK$metric_rank<-seq(1:nrow(db_GK))
        #db_GK<-db_GK[db_GK$now_cost>db_GK$metric,]
        
      #}
    #Selecting the best team
  
    
   
    withProgress(message = "Selection is in process.", detail="This may take up to 5 min.",value=0,{
      
      while (nrow(db_PLplayersChosen)<numTeam) {
      #browser()
      #withProgress(message = paste(nrow(db_PLplayersChosen)," team members selected"), detail="This may take a while...",value=0,{
      v_lastItem<-nrow(db_PLplayersChosen)
      v_lastItemInSelect<-which(db_PLplayersSelect$id==db_PLplayersChosen[v_lastItem,"id"])
      
      k<-ifelse(k==14999,14999,k+1)
      incProgress(k/15000)
       #remove the last row
      if (nrow(db_PLplayersChosen)==0) {
       
        db_PLplayersChosen<-NULL
        showNotification("You money fund is not enough to acquire such number of team members selected!")
        break}
      db_PLplayersChosen<<-db_PLplayersChosen[-v_lastItem,]
      # start looking over starting from the nest item after deleted one
      if(v_lastItemInSelect==nrow(db_PLplayersSelect)) {
        v_lastItem<-nrow(db_PLplayersChosen)
        v_lastItemInSelect<-which(db_PLplayersSelect$id==db_PLplayersChosen[v_lastItem,"id"])
        db_PLplayersChosen<<-db_PLplayersChosen[-v_lastItem,]
        if (nrow(db_PLplayersChosen)==0) {
          db_PLplayersChosen<-NULL
          showNotification("You money fund is not enough to acquire such number of team members selected!")
          break}
        db_PLplayersChosen<<-db_PLplayersChosen[-v_lastItem,]
        v_start<-v_lastItemInSelect+1
        f_lookover(v_start)
      } else {
      v_start<-v_lastItemInSelect+1
      f_lookover(v_start)}
      # Optimization
      if (k==200) {
        showNotification("Optimization!")
       
        db_PLplayersSelect<-db_PLplayersSelect[order(db_PLplayersSelect$now_cost,-db_PLplayersSelect[,8]),]
        
        # Wrangling (replace "total_points")
        db_PLplayersBest<-setNames(data.frame(matrix(ncol=9,nrow=0)),c("web_name","short_name","position","now_cost","form","in_dreamteam","news",input$si_metric,"id"))
        for (position in c("Goalkeeper","Defender","Midfielder","Forward")){
          #for (team in db_teams$short_name) {
          db_PLplayersPos<-db_PLplayersSelect[db_PLplayersSelect$position==position,]
          db_PLplayersPos$level[1]<-db_PLplayersPos[1,8]
          for (i in seq(2,nrow(db_PLplayersPos))) {
            db_PLplayersPos$level[i]<-ifelse(db_PLplayersPos[i,8]>=db_PLplayersPos$level[i-1],db_PLplayersPos[i,8],db_PLplayersPos$level[i-1])
          }
          db_PLplayersPos$isBest<-db_PLplayersPos[,8]>=db_PLplayersPos$level
          db_PLplayersBest<-rbind(db_PLplayersBest,db_PLplayersPos[db_PLplayersPos$isBest,])
        }
        db_PLplayersSelect<-merge(db_PLplayersChosen,db_PLplayersBest,by=names(db_PLplayersChosen),all=TRUE)
        db_PLplayersSelect<-db_PLplayersSelect[order(db_PLplayersSelect$metric,decreasing = TRUE),]
        db_PLplayersChosen<<-db_PLplayersChosen[0,]
        #browser()
        f_lookover(1)
        
    }
       
    
    }
    }) 
output$t_team<-renderDataTable({db_PLplayersChosen[,1:10]})    
    #r_table$table<-db_PLplayersChosen
    #Selected team output
output$text_sum_cost<-renderText({paste("Total amount is £ mln ",sum(db_PLplayersChosen$now_cost))})
output$text_sum_metric<-renderText(paste("Total ",input$si_metric," are ",sum(db_PLplayersChosen[,8])))
output$text_time<-renderText(paste("Process time is",round(Sys.time()-ptm,digits = 2)," sec."))
#output$t_full_table<-renderDataTable({db_PLplayersSelect})
#browser()
session$sendCustomMessage(type = "scrollCallback", message=1) 
#beep(sound=11)
                       

 




               
})


})
