#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("scripts/upload_formatting.R") # functions to assist with raw data uploading
library(shinycssloaders) # spinning plot loading icon
library(tidyverse)
library(shinyBS)
library(shinyalert)
library(vroom)
library(shiny)

# generate vectors of possible well names, for matching to layouts
WELLS1 <- make_well_names("ROWS", "1") # create well names, used in the uploading page 
wells_any <- c(WELLS1, # e.g. A1 .... P24
               make_well_names("ROWS", "01"), # e.g. A01 .... P24
               make_well_names("rows", "1"), # e.g. a1 .... p24
               make_well_names("rows", "01") # e.g. a01 .... p24
)

# Define UI for application that draws a histogram
ui <- navbarPage(useShinyalert(),
                 tabPanel( p("1 | upload data", style = "font-family: 'Avenir Next'; font-size: 15px; color: black",align = "center"), value = "uploads_tab",
                 
                 ###### begin UI from uploads applet 
                 sidebarLayout( # Sidebar layout with input and output definitions
                     sidebarPanel(# Sidebar panel for inputs
                         #uploading---------------------------
                         fileInput("uploaded_file", p("Browse or drag-and-drop raw (RFU) DSF data", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"), # Input: Select a file
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv",
                                              ".xls",
                                              ".xlsx")),
                         
                         bsTooltip("help1", HTML("To analyze data, please upload it as a .tsv, .csv, .xls, or .xlsx, formatted with Temperature in the first column and raw fluorescence measurements in the remaining columns. A correctly-formatted example file can be downloaded at left. Minor reformatting can be done after uploading using the Reformatting assistance options. DSFworld can accept and reformat data files exactly as they are exported from the instruments listed in under Supported Reformatting (at left). See the Instructions tab for more information. Incompatible with Explorer versions 9 and earlier."),
                                   "right", options = list(container = "body"), trigger = "hover"),
                         bsCollapse(id = "upload_help", open = "Panel 1",
                                    bsCollapsePanel(p("Uploading instructions", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                    p("Upload raw RFU data by either of the following two methods:", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                    p("(i) exactly as exported from the instruments listed under 'reformat raw from instrument', below", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                    p("(ii) as formatted in the example file, (download it below): a UTF-8 csv file, with Temperature in the first column and RFU data in the columns to the right. ", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                    p(" ", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                    p("After uploading, your data will appear in its current format in a table at right. Minor adjustments can then be made, such as replacing cycle number values with Temperature.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                    p("To use the plate-layout capabilities (e.g. setting replicates and making custom plots) in the analysis window, each data column must be named by well. Most instruments automatically export data with wells as column names, but if necessary, you can artifically write well names onto your data under 'alter delminiters, headers', below", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "center"),
                                                    downloadButton("download_sample_input", "Download example file", width = '50%',style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
                                    )),
                         bsCollapse(id = "file_parse_types", open = "Panel 1",
                                    bsCollapsePanel(p("Alter delimiters, headers", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                    checkboxInput("header", "Header", TRUE), # Input: Checkbox if file has header
                                                    checkboxInput("name_to_well", "Overwrite column names with wells", FALSE), # Input: Checkbox if file has header
                                                    # radioButtons("sep", "Separator",  # Input: Select separator
                                                    #              choices = c(Comma = ",",
                                                    #                          Semicolon = ";",
                                                    #                          Tab = "\t"),
                                                    #              selected = ","),
                                                    radioButtons("quote", "Quote",  # Input: Select quotes
                                                                 choices = c(None = "",
                                                                             "Double Quote" = '"',
                                                                             "Single Quote" = "'"),
                                                                 selected = '"'))),
                         
                         bsCollapse(id = "instrument_reformatting", open = "Panel 1",
                                    bsCollapsePanel(p("Reformat raw from instrument", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                                    p("Select your instrument from the list below, and upload data exactly as exported.", style = "font-family: 'Avenir Next'; font-size: 12px; color: black",align = "left"),
                                                    radioButtons("reformat", "", # Input: Select type of reformatting necessary
                                                                 choices = c(None = "none",
                                                                             Biorad = "biorad",
                                                                             Stratagene = "stratagene",
                                                                             quantStudio = "quantStudio",
                                                                             qTower = "qTower" # will have to figure out how to deal with multiple reader errors
                                                                 ),
                                                                 selected = "none"))),
                         
                         bsCollapse(id = "cycle_to_T_panel", open = "Panel 1",
                                    bsCollapsePanel(p("Convert cycle number to temperature", style = "font-family: 'Avenir Next'; font-size: 14px; color: black", align = "center"),
                                                    checkboxInput("cycle_to_T", "Convert cycle number to temperature? (if yes, specify below)", FALSE),
                                                    textInput(inputId="start_T", label="Starting Temp (C)", value = 25),
                                                    textInput(inputId="increment_T", label="Increase per cycle (C)", value = 1)
                                    )
                         ),
                         actionButton('jumpToAnalysis', p("Analyze", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                      icon("chart-area"), width = '100%', style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
                         
                         
                     ),  # end sidebar panel
                     
                     # Main panel for displaying outputs
                     mainPanel(
                         tags$style(type='text/css', "#instructions {font-size: 18px; line-height: +2;} "),
                         #HTML("instructions"),
                         dataTableOutput("input_file") %>% withSpinner(color="#525252"), style = "overflow-x: scroll;"
                     ) # end main panel
                 ) # end sidebarLayout
                 
                 ###### end UI from uploads 
                 

                 )
               ) # end navbar page

   


# Define server logic required to draw a histogram
server <- function(input, output) {

    ####### data upoading functions ####
    output$download_sample_input <- downloadHandler(
        filename = function() {
            paste('dsfworld_sample_raw_data_format.csv', sep='')
        },
        content = function(file) {
            vroom_write(vroom("dsfworld_sample_raw_data_format.csv"), file, delim = ",")
        }
    )
    
    data_raw <- reactive({
        req(input$uploaded_file)
        file <- input$uploaded_file$datapath
        
        tryCatch({
            
            df_raw <- vroom(input$uploaded_file$datapath, # file
                        col_names = input$header, # colnames
                        quote = input$quote)
            
            if (input$reformat == "none" ) { df <- df_raw
            } else if (input$reformat == "biorad") { df <- df_raw %>% format_biorad()
            } else if (input$reformat == "stratagene") { df <- df_raw  %>% format_stratagene()
            } else if (input$reformat == "quantStudio") {df <-  df_raw  <- read_quantStudio(input$uploaded_file$datapath)
            } else if (input$reformat == "qTower") { df <- read_qTower(input$uploaded_file$datapath) %>% pivot_wider(-channel_f, names_from = c("well", "channel"), values_from = "value") # take path bc read.table will error on this file
            }
            
            df <- df %>% # in case someone has a file that reads in as characters
                    mutate_if(is.factor, as.character) %>% # make any factors characters
                    mutate_all(as.numeric)  %>% # make all numeric
                    filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
                    discard(~all(is.na(.x))) # drop any columns which are all NAs
            
            if (input$name_to_well == TRUE) {
                df <- df %>%
                    set_names(c("Temperature", WELLS1[c(1:(ncol(.)-1))]))
            }
            
            # cycle number to temperature
            if (input$cycle_to_T == TRUE) {
                Temps_calc <- cycle_to_T_func(as.numeric(input$start_T), as.numeric(input$increment_T), df)
                df_cycle <- dplyr::bind_cols(Temps_calc, df)[-2] 
                names(df_cycle)[1] <- "Temperature"
                return(df_cycle)
                
            } else {
                names(df)[1] <- "Temperature"
                df
            }
        },   
        error = function(e){
            shinyalert("This file needs pre-formatting", "Please select your instrument from 'Reformat raw from instrument'. If you don't see your instrument there, please format your data as shown in the downloadable template and upload again.")
            values$data_raw <<- NULL
        }
        )
    }) # read the input file
    
    
    ### the first steps of the analysis app, to make sure they stitch together ok
    output$input_file <- renderDataTable({
        req(input$uploaded_file)
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


#### things which i'm not sure if we need to put here or not
# output$sample_layout_file <- downloadHandler(
#     filename = function() {
#         paste('dsfworld_example_layout.csv', sep='')
#     },
#     content = function(file) {
#         vroom(vroom("dsfworld_example_layout.csv"), file, delim = ",")
#     }
# )

# move_to_analysis <- FALSE # set this as false to start
# 
# # observeEvent(input$inTabset_analysis , {
# panel <- reactive({ input$inTabset_analysis  }) 
# observeEvent(input$inTabset_analysis, {
#     print("panel changed")
#     print(panel())
# })
# 
# counter <- reactiveValues(data_upload = 0,
#                           data_process = 0)
# 
# toListen <- reactive({
#     list(input$inTabset_analysis, counter$data_upload)
# })
# 
# observeEvent( toListen()        , {
#     print("triggered win3d calc")
#     print(input$inTabset_analysis)
#     req(input$inTabset_analysis == "analysis_tab") # hwo to require that something be true, rather than existing? # only proceed if the user is on the analysis tab
#     req(data_raw())
#     print("passed the req tabset")
#     #values$data_raw <- data_raw()
#     
#     # set the following values based on the data
#     tryCatch({
#         low_T <- isolate( data_raw()$Temperature %>% min() )
#         high_T <- isolate( data_raw()$Temperature %>% max() )
#         n_meas <- isolate( data_raw() %>% nrow() )
#         
#         n2r <<- make_temp_n2r(range(low_T:high_T)) #make_temp_n2r(range(values$data$Temperature)) # an example of how this could be used
#         # win3d <<- floor(3/((n2r(1) - n2r(0))/n_meas))  # originally forgot to ensure this was odd--thanks, and great community catch, SL!!
#         win3d <<- floor(3/((n2r(1) - n2r(0))/n_meas)) + (1 - ( floor(3/((n2r(1) - n2r(0))/n_meas)) %% 2 )) # ensure that win3d is always odd
#         
#         if ( win3d < 5 ) { win3d <<- 5 }
#         sgfilt_nest <<- sgfilt_set_n(n_ = find_sgolay_width( win3d ))
#         counter$data_process <- counter$data_process + 1  #data_raw_counter() <- data_raw_counter() + 1 ##### THIS SEEMS TO FAIL FIX THIS FIRST
#         move_to_analysis <<- TRUE
#     },   
#     error = function(e) {
#         print("win3 errored! setting win3d to 7")
#         win3d <<- 7
#         sgfilt_nest <<- sgfilt_set_n( n_ = find_sgolay_width( 7 ) )
#         move_to_analysis <<- FALSE
#     }, warning = function(w) {
#         print("win3 warning! setting win3d to 7")
#         win3d <<- 7
#         sgfilt_nest <<- sgfilt_set_n( n_ = find_sgolay_width( 7 ) ) 
#         move_to_analysis <<- FALSE
#     })
# }) # write to values
# 
# 
# #observeEvent({ data_raw() }, { # ultimately, observe the transfer to the analysis page
# observeEvent(counter$data_process, {
#     req(data_raw()) # but leave this requirement as is
#     if (move_to_analysis == FALSE) {
#         shinyalert("Please ensure that your data is formatted correctly", "In the 'upload data' tab, you data should be displayed with Temperature in the first column, and RFU data in the columns to the right.")  
#     }
#     req(move_to_analysis == TRUE)
#     tryCatch({
#         print("assigning values$df")
#         values$df <- nest_raw(data_raw()) %>%  add_standardized_wells() # REVISIT1 # active dataframe, used for plotting and calculations
#         values$df_1 <- nest_raw(data_raw()) %>%  add_standardized_wells()  # REVISIT1 # needs an option for when there are non-well names in the data
#         min(unnest(values$df)$Temperature) # this fails if there is something wrong with data_raw()
#     }, error = function(e) {
#         shinyalert("Please ensure that your data is formatted correctly", "In the 'upload data' tab, you data should be displayed with Temperature in the first column, and RFU data in the columns to the right.")
#     })
# })
