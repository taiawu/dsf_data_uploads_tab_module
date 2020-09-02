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

# Define server logic required to draw a histogram
selectVarServer <- function(id, data, filter = is.numeric) {
    stopifnot(is.reactive(data))
    stopifnot(!is.reactive(filter))
    
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            updateSelectInput(session, "var", choices = find_vars(data(), filter))
        })
        
        list(
            name = reactive(input$var),
            value = reactive(data()[[input$var]])
        )
    })
}

qTowerUploadServer_to_channel <- function(id, file_in) {
    moduleServer(id,  function(input, output, session)  {
        file <- reactive(file_in)
        observeEvent(file(), {
            qTower_raw_list <-  reactive(read_qTower_to_channel(file))
            shinyalert(inputId = "alert_pick_channel1", "Which channel would you like to analyze?", qTower_raw_list()$channels, type = "input")
        })
        print("in upload server")
       # print(is.reactive(input$uploaded_file$datapath))

        
        
        df_out <- eventReactive(input$alert_pick_channel1, {
            print("shinyalter channel pciked")
            if (!input$alert_pick_channel1 %in% qTower_raw_list()$channels) {
                shinyalert(inputId = "alert_pick_channel1", "Selected channel is not in the data. Please enter one of the following channels:", qTower_raw_list()$channels, type = "input")

            } else {
                            df_out <- reactive(read_qTower_by_channel(qTower_raw_list()$df, input$alert_pick_channel1) %>%
                                        mutate_if(is.factor, as.character) %>% # make any factors characters
                                        mutate_all(as.numeric)  %>% # make all numeric
                                        filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
                                        discard(~all(is.na(.x)))) # drop any columns which are all NAs
                            
                            print(head(df_out()))
            #qTower_final_out <- reactive(qTower_raw_list()$df)
            }
            print(head(df_out))
            df_out
            })
        
        reactive(df_out())
        
        # observeEvent(input$alert_pick_channel1, {
        #     if (!input$alert_pick_channel1 %in% qTower_raw_list()$channels) {
        #         shinyalert(inputId = "alert_pick_channel1", "Selected channel is not in the data. Please enter one of the following channels:", qTower_raw_list()$channels, type = "input")
        # 
        #     } else {
        #                     df_out <- reactive(read_qTower_by_channel(qTower_raw_list()$df, input$alert_pick_channel1) %>%
        #                                 mutate_if(is.factor, as.character) %>% # make any factors characters
        #                                 mutate_all(as.numeric)  %>% # make all numeric
        #                                 filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
        #                                 discard(~all(is.na(.x)))) # drop any columns which are all NAs
        #     #qTower_final_out <- reactive(qTower_raw_list()$df)
        # }
        #     })
        
        #print(head(qTower_raw_list$df))
        # observeEvent(df_out(), {
        #     outlist <- reactive(list(df = df_out(),
        #                              channel_choice = input$alert_pick_channel1))  
        # })
        
        
        #outlist()
    })}
# 
#         # req(input$alert_pick_channel1)
#         # if (!input$alert_pick_channel1 %in% qTower_raw_list$channels) {
#         #     shinyalert(inputId = "alert_pick_channel2","Selected channel is not in the data. Please re-upload data and try again!")
#         # } else {
#         #     df <- read_qTower_by_channel(qTower_raw_list$df, input$alert_pick_channel1) %>%
#         #         mutate_if(is.factor, as.character) %>% # make any factors characters
#         #         mutate_all(as.numeric)  %>% # make all numeric
#         #         filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
#         #         discard(~all(is.na(.x))) # drop any columns which are all NAs   
#         #     head(df)
#         #     }
#      
#         # df_out <- eventReactive(input$alert_pick_channel1, { 
#         #         # if (input$alert_pick_channel1 %in% qTower_raw_list$channels) {
#         #         #     print("read qTower by channel")
#         #                         df <- read_qTower_by_channel(qTower_raw_list$df, input$alert_pick_channel1) %>%
#         #                             mutate_if(is.factor, as.character) %>% # make any factors characters
#         #                             mutate_all(as.numeric)  %>% # make all numeric
#         #                             filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
#         #                             discard(~all(is.na(.x))) # drop any columns which are all NAs
#         #                         #print(df %>% head())
#         #                         print("qtower read")
#         #                         df
#         #                         #return(reactive(df))
#         # 
#         #                     # } else {
#         #                     #     print("bad channel")
#         #                     #     shinyalert(inputId = "alert_pick_channel2","Selected channel is not in the data. Please re-upload data and try again!")
#         #                     #    # return(NULL)
#         #                     # }
#         #     
#         #     })
#         observeEvent(input$alert_pick_channel1, {
#             if (input$alert_pick_channel1 %in% qTower_raw_list$channels) {
#                 print("read qTower by channel")
#                             df_out <- reactive({read_qTower_by_channel(qTower_raw_list$df, input$alert_pick_channel1) %>%
#                                 mutate_if(is.factor, as.character) %>% # make any factors characters
#                                 mutate_all(as.numeric)  %>% # make all numeric
#                                 filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
#                                 discard(~all(is.na(.x)))})  # drop any columns which are all NAs
#                             print(df_out() %>% head())
#                             print("qtower read")
#                             #return(reactive(df))
#                             #head(df_out)
#                         } else {
#                             print("bad channel")
#                             shinyalert(inputId = "alert_pick_channel2","Selected channel is not in the data. Please re-upload data and try again!")
#                            # return(NULL)
#                         }
#         })
#         
#         # print("outside observe")
#         # #print(head(Df))
#         print(head(df_out()))
#         #reactive(df_out) # return this reactive value from the module
#         # 
#         # print(df %>% head())
#         # reactive(df) # return this reactive value from the module
#         
#         
#     #     #qTower_raw_list <- reactive({
#     #        # req(input$uploaded_file$datapath)
#     #         print("passed file path")
#     #         
#     #         print(file)
#     #         
#     #         qTower_start_list <- read_qTower_to_channel(file)  
#     #         print(names(qTower_start_list))
#     #         shinyalert(inputId = "alert_pick_channel1", "Which channel would you like to analyze?", qTower_start_list$channels, type = "input")
#     #         qTower_start_list
#     #     }) # read the input file
#     #     
#     #     data_raw <- eventReactive({input$alert_pick_channel1
#     #         input$uploaded_file$datapath
#     #         #file_path
#     #     }, {
#     #         print("entering data raw assignment")
#     #         
#     #         #req(new_upload)                       
#     #         req(qTower_raw_list())
#     #         req(input$alert_pick_channel1)
#     #         
#     #         if (input$alert_pick_channel1 %in% qTower_raw_list()$channels) {
#     #             df <- read_qTower_by_channel(qTower_raw_list()$df, input$alert_pick_channel1)  
#     #             print("read qTower by channel")
#     #         } else {
#     #             shinyalert(inputId = "alert_pick_channel2","Selected channel is not in the data. Please re-upload data and try again!")
#     #         }
#     #         
#     #         df <- df %>% # in case someone has a file that reads in as characters
#     #             mutate_if(is.factor, as.character) %>% # make any factors characters
#     #             mutate_all(as.numeric)  %>% # make all numeric
#     #             filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
#     #             discard(~all(is.na(.x))) # drop any columns which are all NAs
#     #         
#     #         
#     #         df 
#     #     })
#     #     
# 
#     #     
#      })
# }

# Define UI for application that draws a histogram
ui <- fluidPage(useShinyalert(),

    # Application title
    titlePanel("qTower uploader"),

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

# server <- function(input, output, session) {
#     # 
#     # observeEvent(input$uploaded_file, {
#     #     data_raw <- qTowerUploadServer("uploaded_file", file = input$uploaded_file$datapath)
#     # })
#     # 
#     data_raw <-  eventReactive(input$uploaded_file, {
#         print("entring upload server")
#         #qTowerUploadServer("uploaded_file")
#         #qTowerUploadServer("uploaded_file", file = input$uploaded_file$datapath)
#         qTowerUploadServer_to_channel("uploaded_file", file_in = input$uploaded_file$datapath)
#     })
#     
#     observeEvent(data_raw(), {
#         print("data_raw changed")
#         print(data_raw() %>% str())
#     })
#     
#      
#     
#     output$input_file <- renderDataTable({
#         req(data_raw())
#         tryCatch(
#             data_raw(),
#             error = function(e) {
#                 shinyalert("File needs pre-formatting!", "Please select your instrument from 'Supported Reformatting'. Or, if you don't see your instrument there, please format your data as shown in the downloadable template and upload again.")
#             }
#         )
#     }, options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't'))
#     
# }
# 
# 
# 
# 
# 
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

# 
# Define server logic required to draw a histogram
server <- function(input, output) {
    new_file <- FALSE
    observeEvent(input$uploaded_file, {
        new_file <- FALSE
        print("uploaded a file!")
        #req(input$uploaded_file)
        
       # qTower_raw_list <- reactive({

            file <- input$uploaded_file$datapath
            
            qTower_start_list <- read_qTower_to_channel(file)
            shinyalert(inputId = "alert_pick_channel1", "Which channel would you like to analyze?", qTower_start_list$channels, type = "input")
           # qTower_start_list()
       # }) # read the input file
        
    })

    # qt_data <- NULL
    # observeEvent(input$alert_pick_channel1, {
    #   #  if (input$alert_pick_channel1) {
    #             if (input$alert_pick_channel1 %in% qTower_start_list$channels) {
    #                 qt_data <- read_qTower_by_channel(qTower_start_list$df, input$alert_pick_channel1) %>%
    #                             mutate_if(is.factor, as.character) %>% # make any factors characters
    #                             mutate_all(as.numeric)  %>% # make all numeric
    #                             filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
    #                             discard(~all(is.na(.x))) # drop any columns which are all NAs
    #                 print("read qTower by channel")
    #             } else {
    #                 shinyalert(inputId = "alert_pick_channel2","Selected channel is not in the data. Please re-upload data and try again!")
    #                 qt_data <- NULL
    #             }
    #     
    # 
    #             data_raw <- reactive(qt_data) #<- reactive(df)
    # 
    #    # }
    # })
    
# 
# observeEvent(qTower_start_list, {
#     print("changing new file")
#     new_file <- TRUE
# })

    data_raw <- eventReactive({input$alert_pick_channel1
        input$uploaded_file
    }, {
        print("entering data raw assignment")

        ##req(new_upload)
       # req(qTower_raw_list())
        req(input$alert_pick_channel1)

        if (input$alert_pick_channel1 %in% qTower_start_list$channels) {
            df <- read_qTower_by_channel(qTower_start_list$df, input$alert_pick_channel1) %>%
                mutate_if(is.factor, as.character) %>% # make any factors characters
                mutate_all(as.numeric)  %>% # make all numeric
                filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
                discard(~all(is.na(.x))) # drop any columns which are all NAs
            print("read qTower by channel")
        } else {
            shinyalert(inputId = "alert_pick_channel2","Selected channel is not in the data. Please re-upload data and try again!")
            df <- NULL
        }
       # new_file <- TRUE
        df

    })



    output$input_file <- renderDataTable({
       # print(new_file)
        
       # req(new_file == TRUE)
       req(data_raw())
        tryCatch(
           # df,
            data_raw(),
            error = function(e) {
                shinyalert("File needs pre-formatting!", "Please select your instrument from 'Supported Reformatting'. Or, if you don't see your instrument there, please format your data as shown in the downloadable template and upload again.")
            }
        )
    }, options = list(scrollX = TRUE, scrollY = 500, scrollCollapse = TRUE, paging = FALSE, dom = 't')
    )

}

# # Run the application 
shinyApp(ui = ui, server = server)
