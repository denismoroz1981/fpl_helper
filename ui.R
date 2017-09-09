  #Fantasy Premier League
library(shiny)



bootstrapPage(
  
tags$head(
  
  tags$script(src ="scroll.js")
  ),
  

  tags$div(class="container-fluid",style="background-color:green;color:white;height:80px", 
           em(slyle="font-style:italic","Statistics from Fantasy Premier League"),
           titlePanel("Denim team selection helper")),
           h6("This is beta version and mistakes are possible, so no responsibility for any negative implications. This website and its domain name are private property of person which has no relations with Premier League. Publicly available statistics are imported and analysed. Looking forward for your feedback: ",span("denismoroz81@gmail.com",style="font-weight:bold")),
 
         tabsetPanel("chapters",type="pills",
                      #tab panel - Pick you team
                            tabPanel("Pick up your team",
                                     sidebarLayout(
                                       #sidebar panel   
                                       sidebarPanel(width=1),
                                       #main panel     
                                       mainPanel(
                                column(12, 
                                div(
                                h5("GREETINGS!",style="font-family:sans-serif;font-style:italic"),
                                h5("You are a ",a("Fantasy Premier League",href="https://fantasy.premierleague.com"), "manager and wish to pick up your team but you don't now know how to start. 
                                Or you need rationale for your choice. 
                                E.g. you need a combination of 15 players, having maximum possible total points together, for,say, 100 mln. pounds.
                                This page will help you to select the best combination for a defined amount of money, maximising a metric selected.
                                "),class="well",style="background-color:pink")
                                       ),
                             fluidRow(
                               column(6,
                               div(
                               h4("1. Enter your budget"),
                               h6("FPL proposes £100 mln as starting budget. But you may allocate more or less money."),
                               numericInput("ni_fund",label = "money fund, £ mln", value = 100)
                               ), class="well"),
                               
                               column(6,
                               div(
                               h4("2. Select metric you what to maximise"),
                               h6("E.g. you team should have maximum possible aggregated total points earned by players during 2016/2017 season"),
                               selectInput("si_metric",label = h6("metric"),
                                                    choices = metrics,selected="total_points"),
                               selectInput("si_season_metric",label=h6("season of the metric above"),choices=seasons,selected = "2016/2017")
                                                              ),class="well")
                               ),
                               
                               h4("3. Select how many players you need"),
                               h6("In FPL teams consist of 15 players, however you may select less players. 
                                  E.g. you have already selected your favourite players who will definitely be in your squard. 
                                  Here you may select rest number of players by position. Do not forget to adjust your budget properly."),
                               
                               column(3,
                                      selectInput("si_numGK",label = h6("goalkeepers"),choices=seq(0,2),selected = 2)
                                      ),
                               column(3,
                                      selectInput("si_numDF",label = h6("defenders"),choices=seq(0,5),selected = 5)
                                      ),
                               column(3,
                                      selectInput("si_numMF",label = h6("midfielders"),choices=seq(0,5),selected = 5)
                                      ),
                               column(3,
                                      selectInput("si_numFW",label = h6("forwards"),choices=seq(0,3),selected = 3)
                                      ),
                             h4("4. Click the button below"),
                             actionButton("ab_Generate","Generate the best combination"),
                             br(),
                             h4("4. Look at the best combination of players"),
                             br(),
                             # Custom shiny to javascript binding
                             # scrolls "outDiv" to bottom once called
                             
                             tags$div(id="outDiv",dataTableOutput("t_team")),
                             textOutput("text_sum_cost"),
                             textOutput("text_sum_metric"),
                             textOutput("text_time"),
                             #dataTableOutput("t_full_table")
                            tags$div(id="bottom")               
                                           
                                           
                                           )
                               
                             )
                             ),
#tab panel Statistica
tabPanel("Statistica",
         sidebarLayout(
           
           sidebarPanel(width=2,
                        htmlOutput("checkbox"),
                        actionButton("uncheck","reset all")
           ),
           mainPanel(                     
             
             wellPanel(
               radioButtons("rb_positions",label = "Choose position:",choices = unique(db_PLplayersGEN$position),inline = TRUE)
             ),
             fluidRow(
               
               column(9,    
                      plotOutput("p_PLplayers")),                       
               div(column(3, 
                          selectInput("si_x",label = h6("choose metric for x axis:"),
                                      choices = metrics,selected="now_cost"),
                          selectInput("si_x_season",label = h6("choose season for x axis:"),
                                      choices = seasons,selected="2017/2018"),
                          selectInput("si_y",label = h6("choose metric for y axis:"),
                                      choices = metrics,selected="total_points"),
                          selectInput("si_y_season",label = h6("choose season for y axis:"),
                                      choices = seasons,selected="2017/2018"),
                          sliderInput("sli_cost_range",label=h6("choose players' cost range:"),min=costmin,
                                      max=costmax,step=0.1,value=c(costmin,costmax))
                          
                          
               ),style="font-size:80%")),
               # ),
               h4("Table format"),
               div(dataTableOutput("t_PLplayers"),style="font-size:100%")
               #h4("Metrics for selection"),
               #div(dataTableOutput("text_metrics"),style="font-size:80%")
               
               
             
           )))                        
                             
                             )
                      )
      
           
  

  
  

