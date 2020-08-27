#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinyalert)
library(tidyverse)
library(shiny)
filter <- dplyr::filter

make_channel_vec <- function( df ) { # make the vector which specifies channel for each reading
    channels <- df %>% 
        group_by(`0`) %>%
        filter(n() == 1) %>%
        select(`0`) %>%
        as_vector()
    
    n_meas <- df %>%  # the number of wells measured per channel (always the same for all channels )
        group_by(`0`) %>%
        filter(n() > 1) %>%
        tally() %>% 
        nrow() 
    
    rep(channels , each = n_meas + 1) # add one, for the row which will still contain the channel itself
}

read_qTower_to_channel <- function( file_path ) {
    
    df_raw <- read_csv(file_path, 
                       col_names = c(0:500) %>% as.character()) %>% # read in 500 columns; this should exceed any actual run, and fill in columsn to right as NAs
        select_if(~sum(!is.na(.)) > 0) #%>% # remove all columns which are all NAs
    
    
    df <- df_raw %>%
        drop_na(ncol(.)) %>% # drop the header, which is NA in the tailing columns
        mutate( channel = make_channel_vec(.))
    
    
    out <- list(df = df, 
                channels = df %>% pull(channel) %>% unique())
}

read_qTower_by_channel <- function(df, channel_choice) {
    
    df %>% 
        filter(channel == channel_choice) %>%
        rename(well = "0") %>%
        filter(!well %in% .$channel) %>%
        mutate_at(vars(-well, -channel), as.numeric) %>%
        pivot_longer(-c("well", "channel"), names_to = "Temperature", values_to = "value") %>%
        pivot_wider( -channel, names_from = "well", values_from = "value")
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(useShinyalert(),

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("uploaded_file", p("Browse or drag-and-drop raw (RFU) DSF data", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), # Input: Select a file
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".xls",
                                 ".xlsx"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # tags$style(type='text/css', "#instructions {font-size: 18px; line-height: +2;} "),
            #HTML("instructions"),
           dataTableOutput("input_file") #%>% withSpinner(color="#525252"), style = "overflow-x: scroll;"
        ) # end main panel
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
new_upload <- "new"
    observeEvent(input$uploaded_file$datapath, {
            new_upload <- NULL
    })
    

    observeEvent(input$alert_pick_channel1, {
            new_upload <- "new"
    })
    
    qTower_raw_list <- reactive({
        req(input$uploaded_file)
        file <- input$uploaded_file$datapath
        
            qTower_start_list <- read_qTower_to_channel(file)  
            shinyalert(inputId = "alert_pick_channel1", "Which channel would you like to analyze?", qTower_start_list$channels, type = "input")
           # new_upload <- "new"
           # if (input$alert_pick_channel1)
            
               # print(input$shinyalert)
            # } else {
            #     shinyalert("This file needs pre-formatting")
            # }
           # print(channel_choice())
            # df <- df %>% # in case someone has a file that reads in as characters
            #     mutate_if(is.factor, as.character) %>% # make any factors characters
            #     mutate_all(as.numeric)  %>% # make all numeric
            #     filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
            #     discard(~all(is.na(.x))) # drop any columns which are all NAs
            
      
            # shinyalert("This file needs pre-formatting", "Please select your instrument from 'Reformat raw from instrument'. If you don't see your instrument there, please format your data as shown in the downloadable template and upload again.")
            # values$data_raw <<- NULL
            qTower_start_list
    }) # read the input file
    
 data_raw <- eventReactive({input$alert_pick_channel1
                           input$uploaded_file
                           }, {
     print("entering data raw assignment")
                               
     req(new_upload)                       
     req(qTower_raw_list())
     req(input$alert_pick_channel1)
     
     if (input$alert_pick_channel1 %in% qTower_raw_list()$channels) {
         df <- read_qTower_by_channel(qTower_raw_list()$df, input$alert_pick_channel1)  
         print("read qTower by channel")
     } else {
         shinyalert(inputId = "alert_pick_channel2","Selected channel is not in the data. Please re-upload data and try again!")
         #shinyalert(inputId = "alert_pick_channel2","Selected channel is not in the data. Which of the following channels would you like to analyze?", qTower_raw_list()$channels, type = "input")
         #df <- read_qTower_by_channel(qTower_raw_list()$df, input$alert_pick_channel2)
     }
     
     df
     
 })
    
    
    
    output$input_file <- renderDataTable({
        req(data_raw())
        tryCatch(
            data_raw(),
            error = function(e) {
                shinyalert("File needs pre-formatting!", "Please select your instrument from 'Supported Reformatting'. Or, if you don't see your instrument there, please format your data as shown in the downloadable template and upload again.")
            }
        )
    }, options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't')
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
