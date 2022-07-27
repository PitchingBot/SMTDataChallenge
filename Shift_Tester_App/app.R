# A shiny app to explore BABIP and other predictions for a particular shift
# alignment, batter spray tendency, and fielder ability

# Use clicks to position the fielders

# Find the batter spray distribution which best fits the target

# Assign fielder abilities too

# Produce stats such as babip, groundball vs flyball outs
# plots of the spray chart for GB and FB
# relative fraction of outs by different fielders

options(warn = -1)


library(beepr)
library(shiny)
library(tidyverse)
library(sigmoid)
library(shinybusy)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(plotly)
library(purrr)

init_value = 0
source("functions.R")
batter_name_data = readRDS("batter_spray_mlb.rds")


ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 4, bootswatch = "litera"),
  titlePanel("The Shift Tester"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(HTML("Test out a fielding alignment while varying batter tendencies and individual fielder ability")),
      helpText(HTML("<br>Before using the app, check if it is busy using the button below")),
      actionButton(inputId = "test_busy",label="Click to Test"),
      uiOutput("test_busy_out"),
      helpText("The button should display the current date and time, if it doesn't respond then the application is currently busy"),
      helpText(HTML("<br>Please try a mirrored site if this application is busy:")),
      helpText(HTML("<a href='https://pitching.shinyapps.io/ShiftTester_Mirror1/'>Mirror 1</a><br><a href='https://pitching.shinyapps.io/ShiftTester_Mirror2/'>Mirror 2</a>"))),
    
    
    mainPanel(fluidPage(
      h2("Adjust Batter Style"),
      selectInput("batter_hand",
                  label ="Batter Handedness",
                  choices = c("R","L"),
                  selected = "R"),
      sliderInput("batter_ev","Average Batter Exit Velocity (mph)", min = 75,max = 95,value = 87),
      sliderInput("batter_la","Average Batter Launch Angle", min = -5,max = 35,value = 12),
      sliderInput("batter_gb","Average Batter Groundball Pull%", min = 0.4,max = 0.85,value = 0.7),
      sliderInput("batter_fb","Average Batter Flyball Pull%", min = 0.25,max = 0.8,value = 0.45),
      sliderInput("batter_speed","Batter Home-to-First Time (ms)",min = 3900,max = 5000,value = 4500),
      selectInput("batter_names","Alternatively, update the sliders above with an existing MLB hitter",choices = append("",unique(batter_name_data$name))),
      actionButton("update_hitter","Update using MLB hitter values"),
      actionButton("go_batter","Create Spray Distribution Graphs"),
      h2("Exit Velocity"),
      plotOutput("EV"),
      h2("Launch Angle"),
      plotOutput("LA"),
      h2("Spray Angles"),
      plotOutput("Pull"),
      
      h2("Adjust Outfielder Ability"),
      
      fluidRow(column(width=4,sliderInput("lf_catch","LF Range",min=-2,max=2,value=0,step = 0.1)#,
                      #numericInput("fieldx_lf","LF X Position",min = -200, max = 200,value=-135),
                      #numericInput("fieldy_lf","LF Y Position",min = 0, max = 400,value=265)
                      ),
               column(width=4,sliderInput("cf_catch","CF Range",min=-2,max=2,value=0,step = 0.1)#,
                      #numericInput("fieldx_cf","CF X Position",min = -200, max = 200,value=0),
                      #numericInput("fieldy_cf","CF Y Position",min = 0, max = 400,value=322)
                      ),
               column(width=4,sliderInput("rf_catch","RF Range",min=-2,max=2,value=0,step=0.1)#,
                      #numericInput("fieldx_rf","RF X Position",min = -200, max = 200,value=130),
                      #numericInput("fieldy_rf","RF Y Position",min = 0, max = 400,value=265)
                      )),
      h2("Adjust Infielder Ability"),
      fluidRow(column(width=3,
                      sliderInput("range_3b","3B Range",min = -2, max = 2,value=0,step = 0.1),
                      sliderInput("transfer_3b","3B Transfer Time (s)",min = 0.5, max = 2,value=1.1,step = 0.1),
                      sliderInput("arm_3b","3B Arm Strength (ft/s)",min = 70, max = 145,value=110,step = 5)#,
                      #numericInput("fieldx_3b","3B X Position",min = -200, max = 200,value=-60),
                      #numericInput("fieldy_3b","3B Y Position",min = 0, max = 400,value=105)
                      ),
               column(width=3,
                      sliderInput("range_ss","SS Range",min = -2, max = 2,value=0,step = 0.1),
                      sliderInput("transfer_ss","SS Transfer Time (s)",min = 0.5, max = 2,value=0.9,step = 0.1),
                      sliderInput("arm_ss","SS Arm Strength (ft/s)",min = 70, max = 145,value=105,step = 5)#,
                      #numericInput("fieldx_ss","SS X Position",min = -200, max = 200,value=-30),
                      #numericInput("fieldy_ss","SS Y Position",min = 0, max = 400,value=140)
                      ),
               column(width=3,
                      sliderInput("range_2b","2B Range",min = -2, max = 2,value=0,step = 0.1),
                      sliderInput("transfer_2b","2B Transfer Time (s)",min = 0.5, max = 2,value=0.9,step = 0.1),
                      sliderInput("arm_2b","2B Arm Strength (ft/s)",min = 70, max = 145,value=90,step = 5)#,
                      #numericInput("fieldx_2b","2B X Position",min = -200, max = 200,value=25),
                      #numericInput("fieldy_2b","2B Y Position",min = 0, max = 400,value=150)
                      ),
               column(width=3,
                      sliderInput("range_1b","1B Range",min = -2, max = 2,value=-0.5,step = 0.1),
                      sliderInput("transfer_1b","1B Transfer Time (s)",min = 0.5, max = 2,value=1.1,step = 0.1),
                      sliderInput("arm_1b","1B Arm Strength (ft/s)",min = 70, max = 145,value=80,step = 5)#,
                      #numericInput("fieldx_1b","1B X Position",min = 50, max = 130,value=60),
                      #numericInput("fieldy_1b","1B Y Position",min = 50, max = 130,value=90)
                      )),
      h2("Drag Points to Change Fielder Positions"),
      plotlyOutput("Draggable_Graph",width="500px",height="500px"),
      fluidRow(column(width=6,actionButton("go","Try This Shift!")),column(width=6,actionButton("go_algo","Produce Suggested Shift (Warning: Takes a few minutes!)"))),
      uiOutput("babip"),
      h2("Flyouts"),
      h4("Catch Probability By Launch Angle and Landing Point"),
      plotOutput("catch_plot"),#,height="1200px"),
      uiOutput("catch_info"),
      h2("Groundouts"),
      h4("Groundout Probability by Ball Landing Position"),
      plotOutput("infield_plot"),
      uiOutput("infield_info",height="600px"),
    )
    )
  )
)






server <- function(input, output, session) {
  
  observeEvent(input$test_busy,{
    output$test_busy_out = renderUI(HTML(paste(date(),
                                               "<br>",
                                               "Application is not busy",
                                               sep="")))
  })
  
  observeEvent(input$update_hitter,{
    if(input$batter_names==""){
      return()
    }
    print(input$batter_names)
    hitter_spec = batter_name_data %>% filter(name==input$batter_names)
    
    updateSliderInput(session,"batter_ev","Average Batter Exit Velocity (mph)", min = 75,max = 95,value = hitter_spec$launch_speed_av)
    updateSliderInput(session,"batter_la","Average Batter Launch Angle", min = -5,max = 35,value =  hitter_spec$launch_angle_av)
    updateSliderInput(session,"batter_gb","Average Batter Groundball Pull%", min = 0.4,max = 0.85,value =  hitter_spec$gb_pull)
    updateSliderInput(session,"batter_fb","Average Batter Flyball Pull%", min = 0.25,max = 0.8,value =  hitter_spec$fb_pull)
    updateSliderInput(session,"batter_speed","Batter Home-to-First Time (ms)",min = 3900,max = 5000,value =  hitter_spec$home_first*1000)
    updateSelectInput(session,"batter_hand",
                label ="Batter Handedness",
                choices = c("R","L"),
                selected = hitter_spec$handedness)
  })
  
  batter_spray_ev <- eventReactive(input$go_batter,{
    input$batter_ev
  },ignoreNULL = FALSE)
  
  batter_spray_la <- eventReactive(input$go_batter,{
    input$batter_la
  },ignoreNULL = FALSE)
  
  batter_spray_gb <- eventReactive(input$go_batter,{
    input$batter_gb
  },ignoreNULL = FALSE)
  
  batter_spray_fb <- eventReactive(input$go_batter,{
    input$batter_fb
  },ignoreNULL = FALSE)
  
  
  

  observeEvent(input$go_batter,{
    shinybusy::show_modal_spinner(spin="orbit")
    batter_spray_new = find_batter_spray(batter_spray,avg_speed = input$batter_ev,average_launch = input$batter_la,gb_pull = input$batter_gb,fb_pull = input$batter_fb,handedness = input$batter_hand,tabulated_spray = spray_chart_tabulate)
    
    output$EV = renderPlot({
      EV_hist(batter_spray_new)
    })
    output$LA = renderPlot({
      LA_hist(batter_spray_new)
    })
    output$Pull = renderPlot({
      Pull_hist(batter_spray_new)
    })
    
    shinybusy::remove_modal_spinner()
  })
  
  
  
  circle3 <- reactiveValues(x = -70, y = 125)
  circle4 <- reactiveValues(x = -40, y = 150)
  circle2 <- reactiveValues(x = 5, y = 155)
  circle1 <- reactiveValues(x = 50, y = 100)
  circle5 <- reactiveValues(x = -125, y = 305)
  circle6 <- reactiveValues(x = 0, y = 340)
  circle7 <- reactiveValues(x = 95, y = 265)
  
  #All the points in between
  observe(
    {
      if(exists("player_position_data")){
        circle3 <- reactiveValues(x = filter(player_position_data,player_position==5)$field_x, y = filter(player_position_data,player_position==5)$field_y)
        circle4 <- reactiveValues(x = filter(player_position_data,player_position==6)$field_x, y = filter(player_position_data,player_position==6)$field_y)
        circle2 <- reactiveValues(x = filter(player_position_data,player_position==4)$field_x, y = filter(player_position_data,player_position==4)$field_y)
        circle1 <- reactiveValues(x = filter(player_position_data,player_position==3)$field_x, y = filter(player_position_data,player_position==3)$field_y)
        circle5 <- reactiveValues(x = filter(player_position_data,player_position==7)$field_x, y = filter(player_position_data,player_position==7)$field_y)
        circle6 <- reactiveValues(x = filter(player_position_data,player_position==8)$field_x, y = filter(player_position_data,player_position==8)$field_y)
        circle7 <- reactiveValues(x = filter(player_position_data,player_position==9)$field_x, y = filter(player_position_data,player_position==9)$field_y)
      }
      ed <- event_data("plotly_relayout")
      req(ed)
      # print(names(ed))
      isolate(
        {
          if (grepl("shapes\\[0\\]", names(ed)[1])) {
            circle1$x <- ed[[1]]
            circle1$y <- ed[[2]]
          }
          if (grepl("shapes\\[1\\]", names(ed)[1])) {
            circle2$x <- ed[[1]]
            circle2$y <- ed[[2]]
          }
          if (grepl("shapes\\[2\\]", names(ed)[1])) {
            circle3$x <- ed[[1]]
            circle3$y <- ed[[2]]
          }
          if (grepl("shapes\\[3\\]", names(ed)[1])) {
            circle4$x <- ed[[1]]
            circle4$y <- ed[[2]]
          }
          if (grepl("shapes\\[4\\]", names(ed)[1])) {
            circle5$x <- ed[[1]]
            circle5$y <- ed[[2]]
          }
          if (grepl("shapes\\[5\\]", names(ed)[1])) {
            circle6$x <- ed[[1]]
            circle6$y <- ed[[2]]
          }
          if (grepl("shapes\\[6\\]", names(ed)[1])) {
            circle7$x <- ed[[1]]
            circle7$y <- ed[[2]]
          }
        }
      )
    
      field_x_1b = circle1$x
      field_x_2b = circle2$x
      field_x_3b = circle3$x
      field_x_ss = circle4$x
      field_x_lf = circle5$x
      field_x_cf = circle6$x
      field_x_rf = circle7$x
      
      field_y_1b = circle1$y
      field_y_2b = circle2$y
      field_y_3b = circle3$y
      field_y_ss = circle4$y
      field_y_lf = circle5$y
      field_y_cf = circle6$y
      field_y_rf = circle7$y
      
      player_position_data = data.frame(
        match = c(1,1,1,1,1,1,1),
        player_position = c(3,4,5,6,7,8,9),
        depth = c(112,155,143,155,330,340,282),
        angle = c(30,-1,-34,-15,-23,4,31),
        field_x = c(field_x_1b,field_x_2b,field_x_3b,field_x_ss,field_x_lf,field_x_cf,field_x_rf),
        field_y = c(field_y_1b,field_y_2b,field_y_3b,field_y_ss,field_y_lf,field_y_cf,field_y_rf),
        catch_factor = c(-1,-1,-1,-1,input$lf_catch,input$cf_catch,input$rf_catch)# extra ability of catching a flyball
      )
      
      player_position_data = player_position_data %>% mutate(depth = sqrt((field_x)**2 + (field_y)**2))
      
      player_position_data <<- player_position_data
      
      
      
      }
  )

  #Create plot
  output$Draggable_Graph <- renderPlotly(
    {

      circles <- map(
        list(circle1, 
             circle2, 
             circle3,
             circle4,
             circle5,
             circle6,
             circle7),
        ~{
          list(
            type = "circle",
            xanchor = floor(.x$x),
            yanchor = .x$y,
            x0 = -5,
            y0 = -5,
            x1 = 5,
            y1 = 5,
            xsizemode = "pixel",
            ysizemode = "pixel",
            # other visual properties
            fillcolor = "red",
            line = list(color = "transparent"),
            layer = "below"
          )
        }
      )
      
      
      
      #X <- seq.int(floor(circle1$x), floor(circle2$x))
      
      #FillinYs <- seq(from = circle1$y, to = circle2$y, length.out = length(X))
      
      
      plot_ly() %>%
        add_paths(x = c(-200,0,200), y = c(200,0,200), opacity = 1,showlegend = F) %>%
        add_paths(x = c(0,90/sqrt(2),0,-90/sqrt(2),0), y = c(0,90/sqrt(2),90*sqrt(2),90/sqrt(2),0), opacity = 1,showlegend = F) %>%
        add_paths(x = arc_data$x,y = arc_data$y,opacity=1,showlegend=F) %>% 
        add_markers(x = c(0,90/sqrt(2),0,-90/sqrt(2)), y = c(0,90/sqrt(2),90*sqrt(2),90/sqrt(2)), opacity = 1,showlegend = F,size=5) %>%
        layout(shapes = circles, xaxis = list(range = c(-200, 200)), yaxis = list(range = c(0, 400))) %>%
        config(edits = list(shapePosition = TRUE))
    })
  
  # observe({
  #   
  # player_position_data = data.frame(
  #   match = c(1,1,1,1,1,1,1),
  #   player_position = c(3,4,5,6,7,8,9),
  #   depth = c(110,151,118,145,298,322,294),
  #   angle = c(30,-1,-34,-15,-23,4,31),
  #   field_x = c(field_x_1b,field_x_2b,field_x_3b,field_x_ss,field_x_lf,field_x_cf,field_x_rf),
  #   field_y = c(field_y_1b,field_y_2b,field_y_3b,field_y_ss,field_y_lf,field_y_cf,field_y_rf),
  #   catch_factor = c(0,0,0,0,input$lf_catch,input$cf_catch,input$rf_catch)# extra ability of catching a flyball
  # )
  # player_position_data = player_position_data %>% mutate(depth = sqrt((field_x)**2 + (field_y)**2))
  # 
  # output$fielder_input = renderPlot({
  #   plot_positions(player_position_data)
  # })
  # })
  
  observeEvent(input$go,{
    # print(player_position_data)
    if(exists("player_position_data")){
      # print("exists")
      circle3 <- reactiveValues(x = filter(player_position_data,player_position==5)$field_x, y = filter(player_position_data,player_position==5)$field_y)
      circle4 <- reactiveValues(x = filter(player_position_data,player_position==6)$field_x, y = filter(player_position_data,player_position==6)$field_y)
      circle2 <- reactiveValues(x = filter(player_position_data,player_position==4)$field_x, y = filter(player_position_data,player_position==4)$field_y)
      circle1 <- reactiveValues(x = filter(player_position_data,player_position==3)$field_x, y = filter(player_position_data,player_position==3)$field_y)
      circle5 <- reactiveValues(x = filter(player_position_data,player_position==7)$field_x, y = filter(player_position_data,player_position==7)$field_y)
      circle6 <- reactiveValues(x = filter(player_position_data,player_position==8)$field_x, y = filter(player_position_data,player_position==8)$field_y)
      circle7 <- reactiveValues(x = filter(player_position_data,player_position==9)$field_x, y = filter(player_position_data,player_position==9)$field_y)

    }

    shinybusy::show_modal_spinner(spin = "orbit")
    
 
    
    
    
    
    field_x_1b = circle1$x
    field_x_2b = circle2$x
    field_x_3b = circle3$x
    field_x_ss = circle4$x
    field_x_lf = circle5$x
    field_x_cf = circle6$x
    field_x_rf = circle7$x
    
    field_y_1b = circle1$y
    field_y_2b = circle2$y
    field_y_3b = circle3$y
    field_y_ss = circle4$y
    field_y_lf = circle5$y
    field_y_cf = circle6$y
    field_y_rf = circle7$y
    
    player_position_data = data.frame(
      match = c(1,1,1,1,1,1,1),
      player_position = c(3,4,5,6,7,8,9),
      depth = c(112,155,143,155,330,340,282),
      angle = c(30,-1,-34,-15,-23,4,31),
      field_x = c(field_x_1b,field_x_2b,field_x_3b,field_x_ss,field_x_lf,field_x_cf,field_x_rf),
      field_y = c(field_y_1b,field_y_2b,field_y_3b,field_y_ss,field_y_lf,field_y_cf,field_y_rf),
      catch_factor = c(-1,-1,-1,-1,input$lf_catch,input$cf_catch,input$rf_catch)# extra ability of catching a flyball
    )
    
    player_position_data = player_position_data %>% mutate(depth = sqrt((field_x)**2 + (field_y)**2))
    
    player_position_data <<- player_position_data
    
    
    batter_speed = input$batter_speed
    
    

    infield_ability = data.frame(
      player_position = c(3,4,5,6),
      range_factor = c(input$range_1b,input$range_2b,input$range_3b,input$range_ss), # extra ability of reaching a groundball
      transfer_time = c(input$transfer_1b,input$transfer_2b,input$transfer_3b,input$transfer_ss),
      throw_speed = c(input$arm_1b,input$arm_2b,input$arm_3b,input$arm_ss)
    )
    
    
    print(player_position_data)
    print(infield_ability)
    
    
    output$fielder_input = renderPlot({
      plot_positions(player_position_data)
    })
    
    #batter_spray %>% group_by(gb = as.numeric(launch_angle<=10)) %>% summarise(pulls = sum(frac[pull<=0]),
    #                                                                               spray = sum(frac[pull>0])) %>% mutate(new_frac = pulls / (pulls+spray)) %>% print()
    #batter_spray %>% summarise(la = weighted.mean(launch_angle,frac,na.rm=TRUE)) %>% print()
    #batter_spray %>% summarise(ls = weighted.mean(launch_speed,frac,na.rm=TRUE)) %>% print()
    
    batter_spray_new = find_batter_spray(batter_spray,avg_speed = input$batter_ev,average_launch = input$batter_la,gb_pull = input$batter_gb,fb_pull = input$batter_fb,handedness = input$batter_hand,tabulated_spray = spray_chart_tabulate)
    #batter_spray_new %>% group_by(gb = as.numeric(launch_angle<=10)) %>% summarise(pulls = sum(frac[pull<=0]),
    #                                                                               spray = sum(frac[pull>0])) %>% mutate(new_frac = pulls / (pulls+spray)) %>% print()
    #batter_spray_new %>% summarise(la = weighted.mean(launch_angle,frac,na.rm=TRUE)) %>% print()
    #batter_spray_new %>% summarise(ls = weighted.mean(launch_speed,frac,na.rm=TRUE)) %>% print()
    results_summary = fielding_model_all_info_function(batter_spray_new,batter_speed,player_position_data,infield_ability)
    # print(results_summary)
    
    output$catch_plot = renderPlot({
      fig = plot_output_catch(results_summary$catch_data,player_position_data)
      fig
    })
    
    output$infield_plot = renderPlot({
      fig = plot_output_infield(results_summary$infield_data,player_position_data)
      fig
    })
    
    output$babip = renderUI({
      HTML(paste0("<h3>BABIP = <b>",round(results_summary$BABIP,3),"</b></h3><br><br>",
                  "<h3>Flyout% = <b>",round(100*results_summary$`FO%`,0),"</b></h3><br><br>",
                  "<h3>Groundout% = <b>",round(100*results_summary$`GO%`,0),"</b></h3><br><br>"))
    })
    
    print(paste0("BABIP =",round(results_summary$BABIP,3)))
    print(paste0("Flyout% =",round(100*results_summary$`FO%`,0)))
    print(paste0("Groundout% =",round(100*results_summary$`GO%`,0)))
    
    output$catch_info = renderUI({
      HTML(paste0("<h3>Flyout to 1B = <b>",round(filter(results_summary$catch_summary,player_position==3)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to 2B = <b>",round(filter(results_summary$catch_summary,player_position==4)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to 3B = <b>",round(filter(results_summary$catch_summary,player_position==5)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to SS = <b>",round(filter(results_summary$catch_summary,player_position==6)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to LF = <b>",round(filter(results_summary$catch_summary,player_position==7)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to CF = <b>",round(filter(results_summary$catch_summary,player_position==8)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to RF = <b>",round(filter(results_summary$catch_summary,player_position==9)$catch*100,0),"%</b></h3>","<br>"))
    })
    shinybusy::remove_modal_spinner()
    output$infield_info = renderUI({
      HTML(paste0("<h3>Groundout to 1B = <b>",round(filter(results_summary$infield_summary,player_position==3)$out_made*100,0),"%</b></h3>","<br>",
                  "<h3>Groundout to 2B = <b>",round(filter(results_summary$infield_summary,player_position==4)$out_made*100,0),"%</b></h3>","<br>",
                  "<h3>Groundout to 3B = <b>",round(filter(results_summary$infield_summary,player_position==5)$out_made*100,0),"%</b></h3>","<br>",
                  "<h3>Groundout to SS = <b>",round(filter(results_summary$infield_summary,player_position==6)$out_made*100,0),"%</b></h3>","<br>",
                  "<h3>Infield Hit = <b>",round(sum(results_summary$infield_summary$infield_hit)*100,0),"%</b></h3>","<br>"))
    })
    
  })
  
  
  
  
  observeEvent(input$go_algo,{
    shinybusy::show_modal_spinner(spin = "orbit",text = "Fitting Iteration 1 (Max. 15)")
    
    circle3 <- reactiveValues(x = -70, y = 125)
    circle4 <- reactiveValues(x = -40, y = 150)
    circle2 <- reactiveValues(x = 5, y = 155)
    circle1 <- reactiveValues(x = 50, y = 100)
    circle5 <- reactiveValues(x = -125, y = 305)
    circle6 <- reactiveValues(x = 0, y = 340)
    circle7 <- reactiveValues(x = 95, y = 265)
    
    
    field_x_1b = circle1$x
    field_x_2b = circle2$x
    field_x_3b = circle3$x
    field_x_ss = circle4$x
    field_x_lf = circle5$x
    field_x_cf = circle6$x
    field_x_rf = circle7$x
    
    field_y_1b = circle1$y
    field_y_2b = circle2$y
    field_y_3b = circle3$y
    field_y_ss = circle4$y
    field_y_lf = circle5$y
    field_y_cf = circle6$y
    field_y_rf = circle7$y
    
    player_position_data = data.frame(
      match = c(1,1,1,1,1,1,1),
      player_position = c(3,4,5,6,7,8,9),
      depth = c(112,155,143,155,330,340,282),
      angle = c(30,-1,-34,-15,-23,4,31),
      field_x = c(field_x_1b,field_x_2b,field_x_3b,field_x_ss,field_x_lf,field_x_cf,field_x_rf),
      field_y = c(field_y_1b,field_y_2b,field_y_3b,field_y_ss,field_y_lf,field_y_cf,field_y_rf),
      catch_factor = c(-1,-1,-1,-1,input$lf_catch,input$cf_catch,input$rf_catch)# extra ability of catching a flyball
    )
    
    player_position_data = player_position_data %>% mutate(depth = sqrt((field_x)**2 + (field_y)**2))
    

    
    
    batter_speed = input$batter_speed
    
    
    infield_ability = data.frame(
      player_position = c(3,4,5,6),
      range_factor = c(input$range_1b,input$range_2b,input$range_3b,input$range_ss), # extra ability of reaching a groundball
      transfer_time = c(input$transfer_1b,input$transfer_2b,input$transfer_3b,input$transfer_ss),
      throw_speed = c(input$arm_1b,input$arm_2b,input$arm_3b,input$arm_ss)
    )
    
    print("Fitting")
    print(player_position_data)
    print(infield_ability)
    
    
    batter_spray_new = find_batter_spray(batter_spray,avg_speed = input$batter_ev,average_launch = input$batter_la,gb_pull = input$batter_gb,fb_pull = input$batter_fb,handedness = input$batter_hand,tabulated_spray = spray_chart_tabulate)
    #batter_spray_new %>% group_by(gb = as.numeric(launch_angle<=10)) %>% summarise(pull = sum(frac[pull<=0])) %>% mutate(new_frac = pull / sum(pull)) %>% print()
    test_outputs = multiple_iterations_fast(player_position_data,batter_spray_new,input$batter_speed,infield_ability,15)
    
    #print(test_outputs)
    
    player_position_data <- test_outputs %>% group_by(player_position) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(-iter_counter)
    player_position_data <<- test_outputs %>% group_by(player_position) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(-iter_counter)
    
    print(player_position_data)
    
    circle3 <- reactiveValues(x = filter(player_position_data,player_position==5)$field_x, y = filter(player_position_data,player_position==5)$field_y)
    circle4 <- reactiveValues(x = filter(player_position_data,player_position==6)$field_x, y = filter(player_position_data,player_position==6)$field_y)
    circle2 <- reactiveValues(x = filter(player_position_data,player_position==4)$field_x, y = filter(player_position_data,player_position==4)$field_y)
    circle1 <- reactiveValues(x = filter(player_position_data,player_position==3)$field_x, y = filter(player_position_data,player_position==3)$field_y)
    circle5 <- reactiveValues(x = filter(player_position_data,player_position==7)$field_x, y = filter(player_position_data,player_position==7)$field_y)
    circle6 <- reactiveValues(x = filter(player_position_data,player_position==8)$field_x, y = filter(player_position_data,player_position==8)$field_y)
    circle7 <- reactiveValues(x = filter(player_position_data,player_position==9)$field_x, y = filter(player_position_data,player_position==9)$field_y)
    
    
    output$Draggable_Graph <- renderPlotly(
      {}
      )
    output$Draggable_Graph <- renderPlotly(
        {

        circles <- map(
          list(circle1, 
               circle2, 
               circle3,
               circle4,
               circle5,
               circle6,
               circle7),
          ~{
            list(
              type = "circle",
              xanchor = floor(.x$x),
              yanchor = .x$y,
              x0 = -5,
              y0 = -5,
              x1 = 5,
              y1 = 5,
              xsizemode = "pixel",
              ysizemode = "pixel",
              # other visual properties
              fillcolor = "red",
              line = list(color = "transparent"),
              layer = "below"
            )
          }
        )
        
        
        
        #X <- seq.int(floor(circle1$x), floor(circle2$x))
        
        #FillinYs <- seq(from = circle1$y, to = circle2$y, length.out = length(X))
        
        
        plot_ly() %>%
          add_paths(x = c(-200,0,200), y = c(200,0,200), opacity = 1,showlegend = F) %>%
          add_paths(x = c(0,90/sqrt(2),0,-90/sqrt(2),0), y = c(0,90/sqrt(2),90*sqrt(2),90/sqrt(2),0), opacity = 1,showlegend = F) %>%
          add_paths(x = arc_data$x,y = arc_data$y,opacity=1,showlegend=F) %>% 
          add_markers(x = c(0,90/sqrt(2),0,-90/sqrt(2)), y = c(0,90/sqrt(2),90*sqrt(2),90/sqrt(2)), opacity = 1,showlegend = F,size=5) %>%
          layout(shapes = circles, xaxis = list(range = c(-200, 200)), yaxis = list(range = c(0, 400))) %>%
          config(edits = list(shapePosition = TRUE))
      })
    
    
    
    results_summary = fielding_model_all_info_function(batter_spray_new,batter_speed,player_position_data,infield_ability)
    # print(results_summary)
    
    output$babip = renderUI({
      HTML(paste0("<h3>BABIP = <b>",round(results_summary$BABIP,3),"</b></h3><br><br>",
                  "<h3>Flyout% = <b>",round(100*results_summary$`FO%`,0),"</b></h3><br><br>",
                  "<h3>Groundout% = <b>",round(100*results_summary$`GO%`,0),"</b></h3><br><br>"))
    })
    output$catch_info = renderUI({
      HTML(paste0("<h3>Flyout to 1B = <b>",round(filter(results_summary$catch_summary,player_position==3)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to 2B = <b>",round(filter(results_summary$catch_summary,player_position==4)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to 3B = <b>",round(filter(results_summary$catch_summary,player_position==5)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to SS = <b>",round(filter(results_summary$catch_summary,player_position==6)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to LF = <b>",round(filter(results_summary$catch_summary,player_position==7)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to CF = <b>",round(filter(results_summary$catch_summary,player_position==8)$catch*100,0),"%</b></h3>","<br>",
                  "<h3>Flyout to RF = <b>",round(filter(results_summary$catch_summary,player_position==9)$catch*100,0),"%</b></h3>","<br>"))
    })
    
    output$infield_info = renderUI({
      HTML(paste0("<h3>Groundout to 1B = <b>",round(filter(results_summary$infield_summary,player_position==3)$out_made*100,0),"%</b></h3>","<br>",
                  "<h3>Groundout to 2B = <b>",round(filter(results_summary$infield_summary,player_position==4)$out_made*100,0),"%</b></h3>","<br>",
                  "<h3>Groundout to 3B = <b>",round(filter(results_summary$infield_summary,player_position==5)$out_made*100,0),"%</b></h3>","<br>",
                  "<h3>Groundout to SS = <b>",round(filter(results_summary$infield_summary,player_position==6)$out_made*100,0),"%</b></h3>","<br>",
                  "<h3>Infield Hit = <b>",round(sum(results_summary$infield_summary$infield_hit)*100,0),"%</b></h3>","<br>"))
    })
    
    Sys.sleep(0.05)
    output$catch_plot = renderPlot({
      fig = plot_output_catch(results_summary$catch_data,player_position_data)
      fig
    })
    
    output$infield_plot = renderPlot({
      fig = plot_output_infield(results_summary$infield_data,player_position_data)
      fig
    })
    
    print(paste0("BABIP =",round(results_summary$BABIP,3)))
    print(paste0("Flyout% =",round(100*results_summary$`FO%`,0)))
    print(paste0("Groundout% =",round(100*results_summary$`GO%`,0)))
    
    shinybusy::remove_modal_spinner()
    beepr::beep(sound="coin")
    
    
    
    
  })
  
}



shinyApp(ui, server)