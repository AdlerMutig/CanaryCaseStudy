#############################################################
# TH Koeln - AIT Master Course
# Andreas Bodendorfer
# 11071015
#############################################################
# This code defines a shiny app running in RStudio 
# Version 1.0.136 ??? ?? 2009-2016 RStudio, Inc.??
# The code is a part of a case study to replicate a EDS
# (event detection system). 
#############################################################


#!!! If the code is run for the first time, please install
#!!! the following packages
#############################################################
#install.packages("shinyjs")
#install.package("shiny")
#install.packages("plotly")
#install.packages("ggplot2")
#install.packages("imputeTS")

# load librarys
library(shiny)
library(plotly)
library(ggplot2)

# load algorithm - r-file
source("function_lm.R")
source("setPointAlgv04.R") #tell andreas to change to version 4

# read csv input files for algorithms
algorithm_config <<- read.csv(file = "algorithm_config.csv", header = TRUE)
algorithm_config_default <<- read.csv(file = "algorithm_config_default.csv", header = TRUE)
csvdata_tail <- read.csv(file = "algorithm_config.csv", header = TRUE)

# counter variables of for loops
i <<- 0
ii <<- 0
iii <<- 0

# variables to locate the position in algorithm config file
var_name <<- (1)
var_para_name <<- (-5) # e.g. window size, distribution
var_type <<- (-4) # e.g. NUMERIC, BOOL, RADIO, RANGE
var_min <<- (-3)
var_max <<- (-2)
var_step <<- (-1)
var_min_default <<- (0) 
var_max_default <<- (1)
temp_value <<- FALSE
csv_column_number <<- 0


# Define UI (user interface) for app
# this defines the GUI for the user
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # define page style, the code in HTML is real HTML code
  tags$head(
    tags$style(
      HTML('
           hr {border-top: 2px solid #000000;}
           body, label, input, button, select {font-family: "Arial";}
           ')
      )
      ), # end tags$head
  
  # title of the app in the headline
  titlePanel("Open Source Event Detection System"),
  
  # Sidebar layout on the left side of the page
  sidebarLayout(
    
    # Sidebar panel 
    sidebarPanel(
      
      # define background color
      tags$style(".well {background-color:#efefef}"),
      
      # define the button to start the EDS
      tags$div(title="Click here to start the EDS", #tool tip for start button
               actionButton("startEDS", "Start EDS") 
      ),
      
      # layout seperator
      br(), tags$hr(),
      
      
      # define input file selection with browse button
      tags$div(title="Click here to select input file", #tool tip for start button
               fileInput("input_data", "Input CSV-File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")) ),
      
      # define checkbox for csv input options
      checkboxInput("CSV_options", "show input options", FALSE),
      
      # if the ceckbox "CSV_options" is true, a new panel will pop up
      conditionalPanel(
        
        condition = "input.CSV_options == true", # condition for the panel opening
        
        # layout seperator
        tags$hr(),
        
        helpText("select options for csv file reading"),
        
        # Input: Checkbox if CSV input file has header
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator for csv interpretation
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # Input: Select quotes style for csv input
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        
        # Input: Select number of rows to display, just the first, or all
        radioButtons("disp", "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head")
      ), #end conditional panel of csv input options
      
      # layout seperator
      tags$hr(), br(),
      
      # define checkbox for selecting specific columns of the input csv 
      checkboxInput("column_options", "select specific columns", FALSE),
      
      # layout seperator
      tags$hr(),
      
      
      # if the ceckbox "Column_options" is true, a new panel will pop up
      conditionalPanel(
        
        condition = "input.column_options == true",
        helpText("select specific rows for EDS"),
        
        # quick buttons for fast select -> actions are defined in server part
        actionButton("select_all", "select ALL"),
        actionButton("unselect_all", "unselect All"),
        
        # dynamic list for generating checkboxes is created in the server function
        uiOutput("csvCheckBoxes"),
        
        # layout seperator
        tags$hr()
        
      ),# end conditional Panel column selection 
      
      # layout seperator
      br(),
      
      # numeric input for threshold
      #numericInput("threshold", "Threshold", 0.5, min = 0, max = 1, step = 0.05, width = NULL),
      sliderInput("threshold", "Threshold",
                  min = (0), 
                  max = (1),
                  step = (0.01),
                  value = (0.5)
      ),
      
      # layout seperator
      tags$hr(),
      
      uiOutput("user_interface_elements"),
      
      # define button to save current config
      actionButton("save_config", "Save current configuration"),
      
      # define button to restore algorithms config file form backup
      actionButton("restore_default", "Restore default configuration"),
      
      # layout seperator
      br(),br(),
      
      # define about button
      actionButton("about", "About")
      
    ), # end sidebar panel
    
    
    # Main panel for displaying information
    ############################################################################################
    mainPanel(
      # main page has several tabs
      tabsetPanel(type = "tabs",
                  # first tab is the display of input data
                  tabPanel("Table", tableOutput("contents")),
                  # second tab are the plots of input data
                  tabPanel(#"Plots",
                    title = "Plots",
                    value = "tab_plots",
                    
                    # define numeric input for last observations
                    uiOutput("plot_number_input"),
                    
                    # define action button to refesh plots
                    actionButton("refresh_plots", "refresh plots"), 
                    
                    # layout seperator
                    tags$hr(),
                    
                    # the dynamic list for plots is generated in the server function
                    # because the degree of freedom is higher there
                    uiOutput("plotOutputs")
                    
                  ), # end panel plot
                  
                  
                  # second tab are the plots of input data
                  tabPanel("Prob_Plots",
                           #h4("Probabilities"),
                           
                           #numericInput("obs", "Number of last observations to view:", 100),
                           
                           tags$hr(),
                           
                           plotlyOutput("prob_plot1"),
                           br(), tags$hr()
                           
                  ),
                  
                  
                  # third tab is display of CSV config data
                  tabPanel("algorithm_config", tableOutput("csv_config"))
                  
                  
      ) # end tabsetPanel
    )# end main panel
  ) # end sidebar layout
      ) # end fluid page of user interface


# Define server logic
################################################################################################
server <- function(input, output, session) {
  
  # use shiny-java-skript to deactivate buttons before loading an input csv file
  shinyjs::disable("column_options")
  shinyjs::disable("startEDS")
  shinyjs::disable("refresh_plots")
  
  #define a variable for data set in server function 
  csvdata <- read.csv(text="timestap_unix")
  
  # define start EDS button actions 
  observeEvent(input$startEDS, {
    
    # create list for selected column names
    column_select <- list()
    
    # set count as pointer of the list counter
    count <- 1
    
    for(i in 2:(ncol(csvdata)) ) { 
      if ( input[[paste("header",i, sep = "" )]] == TRUE){
        column_select[count] = colnames(csvdata[i])
        count= count +1
      } 
    }
    
    # after preparing the algorithm input data, show message
    
    showModal(modalDialog(
      title = "Starting EDS",
      "Please wait...",
      footer = NULL # no closing button available,
      # the user has to wait until the computation is done
    ))
    
    # insert algorithms here
    #######################################################################################
    
    # LM Algorithms by Christian Hoeffer - 03/2018 
    if ( input$algorithmLPCF == TRUE) {
      used_algo <- lm # <----------------------- select here an other algorithm
      lm_final_results <<- function_lm(used_algo,csvdata,input$input.algorithmLPCFwindow_size,input$input.algorithmLPCFthreshold,column_select)
    }
    #print(tail(lm_final_results,25))
    
    
    # SET_POINT_PROXIMITY by Paolo Aguilar - 03/2018
    if (input$algorithmSET_POINT_PROXIMITY == TRUE ) {
      
      #print(input$input.algorithmSET_POINT_PROXIMITYdistribution) # <- beta
      #print(input$input.algorithmSET_POINT_PROXIMITYsigma) # <- value from 0..10 
      #print(column_select) # <- columns selected by userinterface from data frame
      #setPointAlg( beta/exponential , 0..10 , dataframe , list of column-names )
      #print("chode master andreas")
      #print(csvdata)
      result_SET_POINT_PROXIMITY <- setPointAlg(input$input.algorithmSET_POINT_PROXIMITYdistribution,input$input.algorithmSET_POINT_PROXIMITYsigma,csvdata,column_select)
      
    }
    
    #######################################################################################
    #
    
    # if computation is done show next dialog
    showModal(modalDialog(
      title = "Computation ready",
      "Computation is done, please close this message", br(),
      "The results will be displayed in the result tabs",
      easyClose = TRUE # closing by pressing any-case
    ))
    
  })
  
  # define about pop up window form about button 
  observeEvent(input$about, {
    showModal(modalDialog(
      title = "About",
      "Open Source Event Detection System - version 1.0", br(),
      "by SPOT7 - TH Koeln - 2018", br(), br(),
      "Andreas Bodendorfer", br(), "Paolo Aguilar" , br(), "Akbar Shadakov", br(), "Christian Hoeffer",
      easyClose = TRUE
    ))
  })
  
  # define the action button unselect all, all select checkbuttons will be set to false
  observeEvent(input$unselect_all, {
    
    # for loop for length of input csv cloumn length minus time stamp
    for(i in 1:(ncol(csvdata)-1) ) { 
      updateCheckboxInput(session, paste0("header",i+1), value =FALSE)
    } 
    
  })
  
  # define the action button select all, all select checkbuttons will be set to true
  observeEvent(input$select_all, {
    
    #for loop for length of input csv cloumn length minus time stamp
    for(i in 1:(ncol(csvdata)-1) ) { 
      updateCheckboxInput(session, paste0("header",i+1), value =TRUE)
    } 
    
  })
  
  # render tabel for csv data form input_data, loaded by user
  output$contents <- renderTable({
    # input$input_data will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$input_data)
    
    # after the input csv file is available the disabled buttons will now be activated
    shinyjs::enable("column_options")
    shinyjs::enable("startEDS")
    shinyjs::enable("refresh_plots")
    
    # use <<- to write to the server variable, rather then to the local renderTable({ ... }) variable
    csvdata <<- read.csv(input$input_data$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
     
    output$plot_number_input <- renderUI({
      csv_column_number <<- nrow(csvdata)-1
      numericInput("obs", paste("There are ", csv_column_number ," observations in the loaded data frame.", "Number of last observations to view:"), 100)
    })
    
    
    
    # after loading the input data -> create the checkboxel to select specific columns
    output$csvCheckBoxes <- renderUI({
      checkBoxList = list()
      # amntChechboxes will get the number of columns minus the time stamp
      amntCheckboxes = ncol(csvdata)-1
      for(i in 1:amntCheckboxes){
        checkBoxList[[i]] <- checkboxInput(paste0("header",i+1), colnames(csvdata[i+1]), TRUE)
      }
      # execute the list with generated checkboxes
      checkBoxList
    })  
    
    updateCheckboxInput(session,"column_options", "select specific columns", value = TRUE)
    
    # if head option is on choose this output format
    if(input$disp == "head") {
      return(head(csvdata))
    }
    else {
      return(csvdata)
    }
    
  }) #end renderTable input data
  
  
  # render tabel for csv config
  output$csv_config <- renderTable({algorithm_config
    
    return(algorithm_config)
  })
  
  # generate dynamic structure
  # this list will be loaded in the ui main panel plots, but it is generated in the server part
  # because in the server part, the degeree of fredom is greater
  output$plotOutputs <- renderUI({
    
    # amount of plots is the number of columns of input csv minus time stemp
    amntPlots <- ncol(csvdata)-1
    
    uiOutputs <- list()
    listPlots <- list()
    for(i in 1:amntPlots){
      plotName <- paste("plot",i,sep="")
      listPlots[[length(listPlots)+1]] <- plotName
      uiOutputs[[length(uiOutputs)+1]] <- plotlyOutput(plotName)
      uiOutputs[[length(uiOutputs)+1]] <- br()
      uiOutputs[[length(uiOutputs)+1]] <- tags$hr()
    }
    do.call(tagList, listPlots)
    uiOutputs
  })
  
  output$user_interface_elements <- renderUI({
    # generate object-list to collect all algorithms of the algorithms config csv
    objectList <- vector("list", length(algorithm_config[,1]))
    sub_objectList <- vector("list" , length(ncol(algorithm_config)))
    
    # go through lines
    for (i in 1:length(algorithm_config[,1])){
      
      objectList[[i]] <- list( 
        
        # generate checkboxes with algorithem names
        checkboxInput(paste0("algorithm",toString(algorithm_config[i,var_name])), (toString(algorithm_config[i,var_name])), FALSE),
        
        # for every checkbox there is a conditional panel to be extendet
        conditionalPanel(
          
          # define condition for panel extention
          condition = paste0("input.algorithm",toString(algorithm_config[i,var_name])," == true"),
          
          # go though columns of algorithm config file
          for (ii in 1:abs( (length( algorithm_config[ii,] ) - 1) / 7) ){
            
            # column list, for every algorithms this list will be generated newly
            sub_objectList[[ii]] <- list(
              
              # detect what type of parameter the algorithm needs BOOL/NUMBER/RANGE
              if (algorithm_config[i,(ii*7)+var_type] == "BOOL"){
                
                # check what the default configuration for checkbox is
                if( algorithm_config[i,(ii*7)+var_min] == 1) {temp_value <- TRUE}
                if( algorithm_config[i,(ii*7)+var_min] == 0) {temp_value <- FALSE}
                
                checkboxInput( paste0("input.algorithm",toString(algorithm_config[i,var_name]),toString(algorithm_config[i,(ii*7)+var_para_name])),
                               (toString(algorithm_config[i,(ii*7)+var_para_name])), 
                               value = temp_value
                )
              },
              
              if (algorithm_config[i,(ii*7)+var_type] == "NUMERIC"){
                sliderInput(paste0("input.algorithm",toString(algorithm_config[i,var_name]),toString(algorithm_config[i,(ii*7)+var_para_name])), (toString(algorithm_config[i,(ii*7)+var_para_name])),
                            min = (algorithm_config[i,(ii*7)+var_min]), 
                            max = (algorithm_config[i,(ii*7)+var_max]),
                            step = (algorithm_config[i,(ii*7)+var_step]),
                            value = (algorithm_config[i,(ii*7)+var_min_default])
                )
              },
              
              if (algorithm_config[i,(ii*7)+var_type] == "RANGE"){
                sliderInput(paste0("input.algorithm",toString(algorithm_config[i,var_name]),toString(algorithm_config[i,(ii*7)+var_para_name])), (toString(algorithm_config[i,(ii*7)+var_para_name])),
                            "Range:",
                            min = (algorithm_config[i,(ii*7)+var_min]), 
                            max = (algorithm_config[i,(ii*7)+var_max]),
                            step = (algorithm_config[i,(ii*7)+var_step]),
                            value = c((algorithm_config[i,(ii*7)+var_min_default]),(algorithm_config[i,(ii*7)+var_max_default]))
                )
              },
              
              if ( algorithm_config[i,(ii*7)+var_type] == "RADIO"){
                
                # create list for RADIO button choises
                choices_list <- list() 
                
                # the strings for the radio button names will be in the var_min, seperated with "."
                # go throug all choises
                for (iii in 1:length(unlist(strsplit(toString(algorithm_config[i,(ii*7)+var_min]), "[.]" ))))
                {
                  # write choise into list
                  choices_list[[iii]] <- assign( unlist(strsplit(toString(algorithm_config[i,(ii*7)+var_min]), "[.]" ))[[iii]] , unlist(strsplit(toString(algorithm_config[i,(ii*7)+var_min]), "[.]" ))[[iii]])
                }
                
                radioButtons(paste0("input.algorithm",toString(algorithm_config[i,var_name]),toString(algorithm_config[i,(ii*7)+var_para_name])), (toString(algorithm_config[i,(ii*7)+var_para_name])),
                             choices = c(choices_list),
                             selected = unlist(strsplit(toString(algorithm_config[i,(ii*7)+var_min]), "[.]" ))[[algorithm_config[i,(ii*7)+var_max]]] #internal name 
                )# end radioButtons
                
              } # edn if RADIO
              
            ) # end sub_object list
          }, # end for loop columns
          
          # fill sub object list commands into object list 
          sub_objectList
          
        ), # end conditionalPanel
        
        # layout seperator
        tags$hr()
        
      )  # end  of object list
    } # end  for (i in 1:length(algorithm_config[,1])){ - for loop
    
    # execute list of collected commands for alogrithms config file to generate buttons, checkboxes and sliders
    objectList # execute collected commands
    
  })
  
  # plots for main page panel  
  #########################################################  
  
  observeEvent(input$refresh_plots, {  
    amntPlots <- ncol(csvdata)-1
    #csvdata_tail <<- tail(csvdata, input$obs)
    for(i in 1:amntPlots){ 
      local({
        internalI <- i
        #print("executing render plotly")
        csvdata_tail <<- tail(csvdata, input$obs)
        output[[paste0("plot",internalI)]] <- renderPlotly({
          #print(internalI)
          #print(colnames(csvdata[(internalI+1)]))
          plot_ly(
            x = ~csvdata_tail[,1],
            y = ~csvdata_tail[,(internalI+1)], 
            type = 'scatter',
            name = colnames(csvdata_tail[(internalI+1)]),
            mode = "lines+markers"
          )%>%
            layout(title = colnames(csvdata_tail[(internalI+1)]),
                   xaxis = list(title = ''),
                   yaxis = list(title = '')
            )%>%
            add_trace(x = ~csvdata_tail[,1] , y= c(mean(csvdata_tail[,(internalI+1)])), mode = "lines", name = "mean") %>%
            add_trace(x = ~csvdata_tail[,1] , y= c(mean(csvdata_tail[,(internalI+1)])+sd(csvdata_tail[,(internalI+1)])), mode = "lines", name = "+sigma")%>%
            add_trace(x = ~csvdata_tail[,1] , y= c(mean(csvdata_tail[,(internalI+1)])-sd(csvdata_tail[,(internalI+1)])), mode = "lines", name = "-sigma")
          
        }) # end render plot
      })
    } # end for loop
    
    #plotlist
  }) #end observe Event refresh plots
  ############################################################
  # end plots
  
  # define action button save current configuration
  observeEvent(input$save_config, {
    
    # to save current configuration we look for all parameters in algorithm_config.csv
    # the current configuration will be copied into the dataframe algorithm_config 
    # in the end the dataframe will be wirtten to the *.csv file
    for (i in 1:length(algorithm_config[,1])){
      
      # go though columns of algorithm config file
      for (ii in 1:abs( (length( algorithm_config[ii,] ) - 1) / 7) ){
        
        # detect what type of parameter the algorithm needs BOOL/NUMBER/RANGE
        #  
        if (algorithm_config[i,(ii*7)+var_type] == "BOOL"){
          
          if (input[[paste0("input.algorithm",algorithm_config[i,var_name],algorithm_config[i,(ii*7)+var_para_name])]] == TRUE){
            algorithm_config[i,(ii*7)+var_min] <- 1
          }
          
          if (input[[paste0("input.algorithm",algorithm_config[i,var_name],algorithm_config[i,(ii*7)+var_para_name])]] == FALSE){
            algorithm_config[i,(ii*7)+var_min] <- 0
          }
          
        }
        
        # if it is a NUMERIC variable var_min_default has to be saved
        if (algorithm_config[i,(ii*7)+var_type] == "NUMERIC"){
          algorithm_config[i,(ii*7)+var_min_default] <- input[[paste0("input.algorithm",algorithm_config[i,var_name],algorithm_config[i,(ii*7)+var_para_name])]]
        }
        
        # if it is a RANGE variable both min_default and max_default has to be saved
        if (algorithm_config[i,(ii*7)+var_type] == "RANGE"){
          
          # copie the whole information into a temp variable
          temp_default <- input[[paste0("input.algorithm",algorithm_config[i,var_name],algorithm_config[i,(ii*7)+var_para_name])]]
          
          # convert to string and split into min and max default
          temp_min <- toString(temp_default[[1]])
          temp_max <- toString(temp_default[[2]])
          
          # copie values into dataframe 
          algorithm_config[i,(ii*7)+var_min_default] <- temp_min
          algorithm_config[i,(ii*7)+var_max_default] <- temp_max
          
        }
        
        if (algorithm_config[i,(ii*7)+var_type] == "RADIO"){
          
          for (iii in 1:length(unlist(strsplit(toString(algorithm_config[i,(ii*7)+var_min]), "[.]" ))))
          {
            # if the current value == value of RADIO chioses
            if (unlist(strsplit(toString(algorithm_config[i,(ii*7)+var_min]), "[.]" ))[[iii]] == input[[paste0("input.algorithm",algorithm_config[i,var_name],algorithm_config[i,(ii*7)+var_para_name])]]){
              algorithm_config[i,(ii*7)+var_max] <- iii
            }
            
          }
          
        } # end if RADIO    
      } # end for loop columns
    } # end  for (i in 1:length(algorithm_config[,1])){ - for loop
    
    # write to csv file
    write.csv(algorithm_config, file = "algorithm_config.csv",row.names=FALSE, quote = FALSE)
  })
  
  # define action button restore backup configuration
  observeEvent(input$restore_default, {
    write.csv(algorithm_config_default, file = "algorithm_config.csv",row.names=FALSE, quote = FALSE) 
    stopApp(returnValue = invisible())
  })
  
  ########### From here Prob plots (Akbar)  ############
  
  output$prob_plot1 <- renderPlotly({
    csvdata_tail <<- tail(csvdata, input$obs)
    plot_ly(
      x = ~csvdata_tail[,1],
      y = ~setP[,2], 
      type = 'scatter'
    )%>%
      layout(title = colnames(csvdata_tail[2]),
             xaxis = list(title = ''),
             yaxis = list(title = '')
      )
    
  })#end render plot
  
  ###### End Akbar #######
  
}

# Create Shiny app ----
shinyApp(ui, server)


