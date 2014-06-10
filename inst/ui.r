rm(list=ls())
gc()


shinyUI(fluidPage(
  fluidRow(column(3,
                  sidebarPanel(width=12,
                               titlePanel(title="Inputs"),
                               conditionalPanel(condition="input.navbar1=='panel1' & input.navbar11=='raw_data'",    
                                                fileInput('file1', 'Choose CSV File',
                                                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                                tags$hr(),
                                                checkboxInput('header', 'Header', TRUE),
                                                radioButtons('sep', 'Separator',
                                                             c(Comma=',',
                                                               Semicolon=';',
                                                               Tab='\t'),
                                                             ','),
                                                radioButtons('quote', 'Quote',
                                                             c(None='',
                                                               'Double Quote'='"',
                                                               'Single Quote'="'"),
                                                             '"'),
                                                sliderInput("rand_samp","Random Sample of Data (%)",0,100,100,step=1),
                                                selectInput("lanes_choice","Number of Lanes to Construct",c(1,2,3))
                                                ),
                               conditionalPanel(condition="input.navbar1=='panel1' & input.navbar11=='data_load'",    
                                                fileInput('settings_file', 'Load Previous Settings?',
                                                          accept=c('RData'))
                               ),
                               
                               
                               conditionalPanel(condition="input.navbar1=='panel1' & input.navbar11=='api_data'",    
                                                fileInput('api_file', 'Choose API Data File (otherwise hit update)',
                                                          accept=c('RData')),
                                                actionButton("refresh","Update API Data"),
                                                textInput("API_SAVE_NAME","Save API to File Name:",value="API_SAVE_NAME"),
                                                downloadButton("API_SAVE","Download API Data and Tables")
                                                
                               ),
                             
                               
                               conditionalPanel(condition="input.navbar1=='panel1' & input.navbar11=='outlier'",
                                                uiOutput("date"),
                                                uiOutput("cost_lower"),
                                                uiOutput("cost_upper"),
                                                uiOutput("miles_lower"),
                                                uiOutput("miles_upper"),
                                                uiOutput("RPM_lower"),
                                                uiOutput("RPM_upper")
                               ),
                               conditionalPanel(condition="input.navbar1=='panel1' & input.navbar11=='lane_1_construct'",
                                                textInput("lane1_id","Name of Lane 1",value="Lane_1"),
                                                h4("Select Stop Count"),
                                                uiOutput("l1_stop_ct")
                                                #h4("Select Lanes To Include"),
                                                #uiOutput("l1_lane_desc")
                               ),
                               conditionalPanel(condition="input.navbar1=='panel1' & input.navbar11=='lane_2_construct'",
                                                textInput("lane2_id","Name of Lane 2",value="Lane_2"),
                                                h4("Select Origin Zips"),
                                                h4("Select Stop Count"),
                                                uiOutput("l2_stop_ct")
                                                #h4("Select Lanes To Include"),
                                                #uiOutput("l2_lane_desc")
                                                
                               ),
                               conditionalPanel(condition="input.navbar1=='panel1' & input.navbar11=='lane_3_construct'",
                                                textInput("lane3_id","Name of Lane 3",value="Lane_3"),
                                                h4("Select Stop Count"),
                                                uiOutput("l3_stop_ct")
                                                #h4("Select Lanes To Include"),
                                                #uiOutput("l3_lane_desc")
                               ),
                               conditionalPanel(condition="input.navbar1=='panel1' && input.navbar11=='weather'",    
                                                uiOutput("noaa_key"),uiOutput("weather_addresses"),actionButton("kick_weather","Go: Lookup Weather For New Addresses")
                               ),
                               conditionalPanel(condition="input.navbar1=='panel4' && input.navbar14=='bcst_pred'",    
                                                textInput("settings_name","Save Settings to File Name:",value="settings_name")
                               ),
                               conditionalPanel(condition="input.navbar1=='panel4' && input.navbar14=='bcst_pred'",
                                                downloadButton('downloadData','Download')
                                                
                               ),
                               conditionalPanel(condition = "input.navbar1=='panel2'", uiOutput("select_radio")),
                               conditionalPanel(condition = "input.navbar1=='panel3'|| input.navbar1=='panel4'", uiOutput("min_lead_slider")),
                               conditionalPanel(condition = "input.navbar1=='panel3'|| input.navbar1=='panel4'", uiOutput("max_lead_slider")),
                               conditionalPanel(condition = "input.navbar1=='panel3'|| input.navbar1=='panel4'", uiOutput("max_model_slider")),
                               conditionalPanel(condition = "input.navbar1=='panel3'|| input.navbar1=='panel4'", uiOutput("gamma_numeric")),
                               conditionalPanel(condition = "input.navbar1=='panel3'|| input.navbar1=='panel4'", uiOutput("backcast_ahead_slider")),
                               conditionalPanel(condition = "input.navbar1=='panel3' && (input.navbar13=='var_import' | input.navbar13=='cond_effect')", uiOutput("pick_numeric")),
                               conditionalPanel(condition = "input.navbar1=='panel3' && (input.navbar13=='var_import' | input.navbar13=='cond_effect')", uiOutput("linear")),
                               conditionalPanel(condition = "input.navbar1=='panel3' && (input.navbar13=='var_import' | input.navbar13=='cond_effect')", uiOutput("seasonality")),
                               conditionalPanel(condition = "input.navbar1=='panel3' && (input.navbar13=='var_import' | input.navbar13=='cond_effect')", uiOutput("interaction_check")),
                               conditionalPanel(condition = "input.navbar1=='panel3' && (input.navbar13=='var_import' | input.navbar13=='cond_effect')", uiOutput("interaction_split")),
                               conditionalPanel(condition = "input.navbar1=='panel3' && (input.navbar13=='var_import' | input.navbar13=='cond_effect')", uiOutput("response_radio")),
                               conditionalPanel(condition = "input.navbar1=='panel3' && (input.navbar13=='var_import' | input.navbar13=='cond_effect')", uiOutput("predictors_checkgroup")),
                               conditionalPanel(condition = "input.navbar1=='panel3' && input.navbar13=='GAM_effects'", downloadButton("effects","Download Conditional Effects (CSV)")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && (input.navbar14=='preds' || input.navbar14=='vol_quote')", uiOutput("LCL_percentile")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && (input.navbar14=='preds' || input.navbar14=='vol_quote')", uiOutput("UCL_percentile")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && (input.navbar14=='pred_fwd' | input.navbar14=='preds')", uiOutput("carry_forward")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && (input.navbar14=='pred_fwd' | input.navbar14=='preds')", uiOutput("matrix_values")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && input.navbar14=='gam_pred'", downloadButton("predictions_GAM","Download GAM Predictions (CSV)")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && input.navbar14=='vol_quote'", uiOutput("quote_date")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && input.navbar14=='vol_quote'", uiOutput("volume_checkgroup")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && input.navbar14=='bcst_pred'", uiOutput("backcast_length_slider")),
                               conditionalPanel(condition = "input.navbar1=='panel4' && input.navbar14=='vol_quote'", uiOutput("matrix_volume"))
                  )),
           column(9,navbarPage(title = "Version 2.0",id = "navbar1",
                               tabPanel("Dataset Selection", value = "panel1",
                                        navbarPage(title = "", id = "navbar11",
                                                   tabPanel("Raw Data",dataTableOutput("raw_data"),value="raw_data"),
                                                   tabPanel("Data Selector",fluidPage(fluidRow(column(6,h4("Select Stop Count"),uiOutput("stop_chooser"),
                                                                                                      h4("Select Total Load Cost"),uiOutput("cost_chooser"),
                                                                                                      h4("Select Origin zip"),uiOutput("origin_chooser"),
                                                                                                      h4("Select Destination zip"),uiOutput("destination_chooser"),
                                                                                                      h4("Select Load Region"),uiOutput("load_region"),
                                                                                                      h4("Select Delivery State"),uiOutput("delivery_state")
                                                   ),
                                                   column(6,h4("Select Date (Numeric Format)"),uiOutput("date_chooser"),
                                                          h4("Select Load Mileage"),uiOutput("mileage_chooser"),
                                                          h4("Select Lane Descriptions"),uiOutput("lane_chooser"),
                                                          h4("Select Origin State"),uiOutput("orig_state"),
                                                          h4("Select Delivery Region"),uiOutput("delivery_region")
                                                   ))),value="data_load"),
                                                   tabPanel("Selected Data",dataTableOutput("selected_data"),value="selected_data"),
                                                   tabPanel("Outlier Removal",fluidPage(fluidRow(plotOutput("outlier_rpm_plot")),
                                                                                        fluidRow(dataTableOutput("outlier_rpm")))
                                                            ,value="outlier"),
                                                   tabPanel("Lane 1 Constructor",fluidPage(fluidRow(column(12,plotOutput("l1_raw_plot"))),
                                                                                           fluidRow(column(6,h4("Origin Parameters"),
                                                                                                           h4("Select Origin Zips"), uiOutput("l1_orig_zip"),
                                                                                                           h4("Select Origin States to Include"),uiOutput("l1_orig_state"),
                                                                                                           h4("Select Load Regions to Include"),uiOutput("l1_load_region")
                                                   ),
                                                   column(6,h4("Destination Parameters"),
                                                          h4("Select Destination Zips"), uiOutput("l1_dest_zip"),
                                                          h4("Select Delivery States to Include"),uiOutput("l1_delivery_state"),
                                                          h4("Select Delivery Regions to Include"),uiOutput("l1_delivery_region")
                                                   ))),value="lane_1_construct"),
                                                   tabPanel("Lane 2 Constructor",fluidPage(fluidRow(column(12,plotOutput("l2_raw_plot"))),
                                                                                           fluidRow(column(6,h4("Origin Parameters"),
                                                                                                           h4("Select Origin Zips"), uiOutput("l2_orig_zip"),
                                                                                                           h4("Select Origin States to Include"),uiOutput("l2_orig_state"),
                                                                                                           h4("Select Load Regions to Include"),uiOutput("l2_load_region")
                                                                                           ),
                                                                                           column(6,h4("Destination Parameters"),
                                                                                                  h4("Select Destination Zips"), uiOutput("l2_dest_zip"),
                                                                                                  h4("Select Delivery States to Include"),uiOutput("l2_delivery_state"),
                                                                                                  h4("Select Delivery Regions to Include"),uiOutput("l2_delivery_region")
                                                                                           ))),value="lane_2_construct"),
                                                   tabPanel("Lane 3 Constructor",fluidPage(fluidRow(column(12,plotOutput("l3_raw_plot"))),
                                                                                           fluidRow(column(6,h4("Origin Parameters"),
                                                                                                           h4("Select Origin Zips"), uiOutput("l3_orig_zip"),
                                                                                                           h4("Select Origin States to Include"),uiOutput("l3_orig_state"),
                                                                                                           h4("Select Load Regions to Include"),uiOutput("l3_load_region")
                                                                                           ),
                                                                                           column(6,h4("Destination Parameters"),
                                                                                                  h4("Select Destination Zips"), uiOutput("l3_dest_zip"),
                                                                                                  h4("Select Delivery States to Include"),uiOutput("l3_delivery_state"),
                                                                                                  h4("Select Delivery Regions to Include"),uiOutput("l3_delivery_region")
                                                                                           ))),value="lane_3_construct"),
                                                   tabPanel("All Lanes Raw Data",dataTableOutput("lanes"),value="all_lanes"),
                                                   tabPanel("API Data",uiOutput("raw_api"),uiOutput("API_choice"),dataTableOutput("raw_indicators"),value="api_data"),
                                                   tabPanel("Weather Data",uiOutput("w_maps"),value="weather"),
                                                   tabPanel("Weekly Averages",dataTableOutput("weekly_averages"),value="weekly_avgs")
                                        )
                               ),
                               tabPanel("Raw/Imputed Data", value = "panel2",
                                        navbarPage(title = "", id = "navbar12",
                                                   tabPanel("Raw Data",plotOutput("Raw"), dataTableOutput("raw_indicators_2"),value = "raw"),
                                                   tabPanel("Imputed Data", plotOutput("Impute"),dataTableOutput("raw_indicators_3"),value = "impute")
                                        )
                               ),
                               tabPanel("Model Selection", value = "panel3",
                                        navbarPage(title = "", id = "navbar13",
                                                   tabPanel("Variable Importance", showOutput("Var_Import","highcharts"),value="var_import"),
                                                   tabPanel("Conditional Effects", plotOutput("cond_effect"),value="cond_effect"),
                                                   tabPanel("Diagnostics", plotOutput("diagnostic"),value="diagnostic"),
                                                   tabPanel("Model Fit", showOutput("fit","highcharts"),value="fit"),
                                                   tabPanel("ARIMA Error Stream", plotOutput("ts_error"),value="ts_error"),
                                                   tabPanel("Table of Effects", dataTableOutput("GAM_effects"),value="GAM_effects")
                                        )
                               ),
                               tabPanel("Prediction", value = "panel4",
                                        navbarPage(title = "", id = "navbar14",
                                                   tabPanel("Predictor Values", uiOutput("pred_fwd"), value="pred_fwd"),
                                                   tabPanel("Model Predictions", showOutput("preds","highcharts"), value = "preds"),
                                                   tabPanel("Integrated Volume Quote",fluidPage(fluidRow(h3(textOutput("quote_final"))),fluidRow(column(6,showOutput("quote_value","highcharts")),
                                                                                                         column(6,showOutput("quote_volume","highcharts"))),
                                                                                                fluidRow(dataTableOutput("vol_quote"))),value = "vol_quote"),
                                                   tabPanel("Table of Predictions", dataTableOutput("GAM_predictions"), value = "gam_pred"),
                                                   tabPanel("Backcasting Predictions", showOutput("Backcast_graph","highcharts"), value = "bcst_pred")
                                        )
                               )
           )
           ))))
