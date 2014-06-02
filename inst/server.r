rm(list=ls())
gc()
options(shiny.maxRequestSize=500*1024^2)###500 megabyte file upload limit set


#################################
shinyServer(function(input, output, session) { # server is defined within these parentheses
  
  
  # render UI input elements for use in UI wrapper - come back to this section to calculate length dynamically
  
  #new UI elements to insert into UI for bundle 1
  
  ####UI elements for API Choices####
  output$API_choice<-renderUI({
    DATA_I<-API()[["DATA_I"]]
    dest=names(DATA_I)[-1]
    cc=dest
    for (i in 1:length(dest)){
      cc[i]=colnames(DATA_I[[dest[i]]])
    }
    selectedcc <- cc
    if (!is.null(Read_Settings())){
      selectedcc <- cc[which(cc%in%Read_Settings()[["API_choice"]])]}
    #data.frame(Abbreviations=dest,Full_Names=cc)
    checkboxGroupInput("API_choice", "Choose Economic Indicators",
                       cc,selected=selectedcc)
    
  })
  
  
  output$date_chooser<-renderUI({
    chooserInput("date", "Available", "Selected",
                 choices()[-2], choices()[2], size = 10, multiple = TRUE
    )
    
  })
  
  
  output$stop_chooser<-renderUI({
    chooserInput("stop", "Available", "Selected",
                 choices()[-18],choices()[18], size = 10, multiple = TRUE
    )
    
  })
  
  
  output$cost_chooser<-renderUI({
    chooserInput("cost", "Available", "Selected",
                 choices()[-28], choices()[28], size = 10, multiple = TRUE
    )
    
  })
  
  output$mileage_chooser<-renderUI({
    chooserInput("mileage", "Available", "Selected",
                 choices()[-24], choices()[24], size = 10, multiple = TRUE
    )
    
  })
  
  output$destination_chooser<-renderUI({
    chooserInput("destination", "Available", "Selected",
                 choices()[-9], choices()[9], size = 10, multiple = TRUE
    )
    
  })
  
  output$origin_chooser<-renderUI({
    chooserInput("origin", "Available", "Selected",
                 choices()[-6], choices()[6], size = 10, multiple = TRUE
    )
    
  })
  
  output$lane_chooser<-renderUI({
    chooserInput("lane_choices", "Available", "Selected",
                 choices()[-13], choices()[13], size = 10, multiple = TRUE
    )
    
  })
  
  output$orig_state<-renderUI({
    chooserInput("orig_state", "Available", "Selected",
                 choices()[-8], choices()[8], size = 10, multiple = TRUE
    )
    
  })
  
  output$load_region<-renderUI({
    chooserInput("load_region", "Available", "Selected",
                 choices()[-7], choices()[7], size = 10, multiple = TRUE
    )
    
  })
  
  output$delivery_state<-renderUI({
    chooserInput("delivery_state", "Available", "Selected",
                 choices()[-10], choices()[10], size = 10, multiple = TRUE
    )
    
  })
  
  output$delivery_region<-renderUI({
    chooserInput("delivery_region", "Available", "Selected",
                 choices()[-11], choices()[11], size = 10, multiple = TRUE
    )
    
  })
  
  output$date<-renderUI({
    a=min(CHR()$Date)
    b=max(CHR()$Date)
    out1 <- dateRangeInput("date_range", "Date Cutoff Ranges:", 
                           start =a, end =b )
    if (!is.null(Read_Settings()[["date_range"]])) updateDateRangeInput(session, inputId= "date_range", start = Read_Settings()[["date_range"]][1], end = Read_Settings()[["date_range"]][2])
    return(out1)
  })
  
  
  
  output$cost_lower<-renderUI({
    a=min(CHR()$Total_Cost)
    b=max(CHR()$Total_Cost)
    c = a
    out1 <- sliderInput("cost_lower", "Lower Total Cost Cutoff:", 
                        min =a , max = b, value = c, step= NULL)
    if (!is.null(Read_Settings()[["cost_lower"]])) updateSliderInput(session, inputId= "cost_lower", value = Read_Settings()[["cost_lower"]])
    return(out1)
  })
  
  output$cost_upper<-renderUI({
    a=min(CHR()$Total_Cost)
    b=max(CHR()$Total_Cost)
    c = b
    out1 <- sliderInput("cost_upper", "Upper Total Cost Cutoff:", 
                        min = a, max = b, value = c, step= NULL)
    if (!is.null(Read_Settings()[["cost_upper"]])) updateSliderInput(session, inputId= "cost_upper", value = Read_Settings()[["cost_upper"]])
    return(out1)
  })
  
  output$miles_lower<-renderUI({
    a=min(CHR()$Total_Mileage)
    b=max(CHR()$Total_Mileage)
    c = a
    out1 <- sliderInput("miles_lower", "Lower Mileage Cutoff:", 
                        min =a , max = b, value = c, step= NULL)
    if (!is.null(Read_Settings()[["miles_lower"]])) updateSliderInput(session, inputId= "miles_lower", value = Read_Settings()[["miles_lower"]])
    return(out1)
  })
  
  output$miles_upper<-renderUI({
    a=min(CHR()$Total_Mileage)
    b=max(CHR()$Total_Mileage)
    c = b
    out1 <- sliderInput("miles_upper", "Upper Mileage Cutoff:", 
                        min = a, max = b, value = c, step= NULL)
    if (!is.null(Read_Settings()[["miles_upper"]])) updateSliderInput(session, inputId= "miles_upper", value = Read_Settings()[["miles_upper"]])
    return(out1)
  })
  
  
  output$RPM_lower<-renderUI({
    a=min(CHR()$RPM,na.rm=T)
    b=max(CHR()$RPM,na.rm=T)
    c = 1
    out1 <- sliderInput("RPM_lower", "Lower RPM Cutoff:", 
                        min =a, max = b, value = c, step= NULL)
    if (!is.null(Read_Settings()[["RPM_lower"]])) updateSliderInput(session, inputId= "RPM_lower",  value = Read_Settings()[["RPM_lower"]])
    return(out1)
  })
  
  output$RPM_upper<-renderUI({
    a=min(CHR()$RPM,na.rm=T)
    b=max(CHR()$RPM,na.rm=T)
    c = 5
    
    out1 <- sliderInput("RPM_upper", "Upper RPM Cutoff:", 
                        min = a, max = b, value = c, step= NULL)
    if (!is.null(Read_Settings()[["RPM_upper"]])) updateSliderInput(session, inputId= "RPM_upper",value = Read_Settings()[["RPM_upper"]])
    return(out1)
  })
  
  #Lane1 UI element rendering
  
  output$l1_orig_zip<-renderUI({
    dat<-FIL()$Origin_Zip
    orig<-unique(dat)
    orig<-sort(orig)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(orig), "NA")
    orig<-c(orig,NA)
    selected <- c()
    if (!is.null(Read_Settings()[["l1_orig_zip"]])){
      selected <- orig[which(comp%in%Read_Settings()[["l1_orig_zip"]][["right"]])]
      orig <- orig[which(!comp%in%Read_Settings()[["l1_orig_zip"]][["right"]])]}
    chooserInput("l1_orig_zip", "Available", "Selected",
                 orig, selected, size = 10, multiple = TRUE
    )
  })
  
  output$l1_dest_zip<-renderUI({
    dat<-FIL()$Destination_Zip
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(dest), "NA")
    dest<-c(dest,NA)
    selected <- c()
    if (!is.null(Read_Settings()[["l1_dest_zip"]])){
      selected <- dest[which(comp%in%Read_Settings()[["l1_dest_zip"]][["right"]])]
      dest <- dest[which(!comp%in%Read_Settings()[["l1_dest_zip"]][["right"]])]}
    chooserInput("l1_dest_zip", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l1_stop_ct<-renderUI({
    dat<-FIL()$Stop_Count
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(dest), "NA")
    dest<-c(dest,NA)
    unselected <- c()
    if (!is.null(Read_Settings()[["l1_stop_ct"]])){
      unselected <- dest[which(!comp%in%Read_Settings()[["l1_stop_ct"]][["right"]])]
      dest <- dest[which(comp%in%Read_Settings()[["l1_stop_ct"]][["right"]])]}
    chooserInput("l1_stop_ct", "Available", "Selected",
                 unselected, dest, size = 10, multiple = TRUE
    )
    
  })
  
  output$l1_lane_desc<-renderUI({
    dat<-FIL()$Lane
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l1_lane_desc"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l1_lane_desc"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l1_lane_desc"]][["right"]])]}
    chooserInput("l1_lane_desc", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l1_orig_state<-renderUI({
    dat<-FIL()$Orig_State
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l1_orig_state"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l1_orig_state"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l1_orig_state"]][["right"]])]}
    chooserInput("l1_orig_state", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l1_load_region<-renderUI({
    dat<-FIL()$Load_Region
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l1_load_region"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l1_load_region"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l1_load_region"]][["right"]])]}
    chooserInput("l1_load_region", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l1_delivery_state<-renderUI({
    dat<-FIL()$Delivery_State
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l1_delivery_state"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l1_delivery_state"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l1_delivery_state"]][["right"]])]}
    chooserInput("l1_delivery_state", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l1_delivery_region<-renderUI({
    dat<-FIL()$Delivery_Region
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l1_delivery_region"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l1_delivery_region"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l1_delivery_region"]][["right"]])]}
    chooserInput("l1_delivery_region", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l2_orig_zip<-renderUI({
    dat<-FIL()$Origin_Zip
    orig<-unique(dat)
    orig<-sort(orig)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(orig), "NA")
    orig<-c(orig,NA)
    selected <- c()
    if (!is.null(Read_Settings()[["l2_orig_zip"]])){
      selected <- orig[which(comp%in%Read_Settings()[["l2_orig_zip"]][["right"]])]
      orig <- orig[which(!comp%in%Read_Settings()[["l2_orig_zip"]][["right"]])]}
    chooserInput("l2_orig_zip", "Available", "Selected",
                 orig, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l2_dest_zip<-renderUI({
    dat<-FIL()$Destination_Zip
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(dest), "NA")
    dest<-c(dest,NA)
    selected <- c()
    if (!is.null(Read_Settings()[["l2_dest_zip"]])){
      selected <- dest[which(comp%in%Read_Settings()[["l2_dest_zip"]][["right"]])]
      dest <- dest[which(!comp%in%Read_Settings()[["l2_dest_zip"]][["right"]])]}
    chooserInput("l2_dest_zip", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l2_stop_ct<-renderUI({
    dat<-FIL()$Stop_Count
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(dest), "NA")
    dest<-c(dest,NA)
    unselected <- c()
    if (!is.null(Read_Settings()[["l2_stop_ct"]])){
      unselected <- dest[which(!comp%in%Read_Settings()[["l2_stop_ct"]][["right"]])]
      dest <- dest[which(comp%in%Read_Settings()[["l2_stop_ct"]][["right"]])]}
    chooserInput("l2_stop_ct", "Available", "Selected",
                 unselected, dest, size = 10, multiple = TRUE
    )
    
  })
  
  output$l2_lane_desc<-renderUI({
    dat<-FIL()$Lane
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l2_lane_desc"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l2_lane_desc"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l2_lane_desc"]][["right"]])]}
    chooserInput("l2_lane_desc", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l2_orig_state<-renderUI({
    dat<-FIL()$Orig_State
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l2_orig_state"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l2_orig_state"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l2_orig_state"]][["right"]])]}
    chooserInput("l2_orig_state", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l2_load_region<-renderUI({
    dat<-FIL()$Load_Region
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l2_load_region"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l2_load_region"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l2_load_region"]][["right"]])]}
    chooserInput("l2_load_region", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l2_delivery_state<-renderUI({
    dat<-FIL()$Delivery_State
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l2_delivery_state"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l2_delivery_state"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l2_delivery_state"]][["right"]])]}
    chooserInput("l2_delivery_state", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l2_delivery_region<-renderUI({
    dat<-FIL()$Delivery_Region
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l2_delivery_region"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l2_delivery_region"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l2_delivery_region"]][["right"]])]}
    chooserInput("l2_delivery_region", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l3_orig_zip<-renderUI({
    dat<-FIL()$Origin_Zip
    orig<-unique(dat)
    orig<-sort(orig)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(orig), "NA")
    orig<-c(orig,NA)
    selected <- c()
    if (!is.null(Read_Settings()[["l3_orig_zip"]])){
      selected <- orig[which(comp%in%Read_Settings()[["l3_orig_zip"]][["right"]])]
      orig <- orig[which(!comp%in%Read_Settings()[["l3_orig_zip"]][["right"]])]}
    chooserInput("l3_orig_zip", "Available", "Selected",
                 orig, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l3_dest_zip<-renderUI({
    dat<-FIL()$Destination_Zip
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(dest), "NA")
    dest<-c(dest,NA)
    selected <- c()
    if (!is.null(Read_Settings()[["l3_dest_zip"]])){
      selected <- dest[which(comp%in%Read_Settings()[["l3_dest_zip"]][["right"]])]
      dest <- dest[which(!comp%in%Read_Settings()[["l3_dest_zip"]][["right"]])]}
    chooserInput("l3_dest_zip", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l3_stop_ct<-renderUI({
    dat<-FIL()$Stop_Count
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    comp <- c(as.character(dest), "NA")
    dest<-c(dest,NA)
    unselected <- c()
    if (!is.null(Read_Settings()[["l3_stop_ct"]])){
      unselected <- dest[which(!comp%in%Read_Settings()[["l3_stop_ct"]][["right"]])]
      dest <- dest[which(comp%in%Read_Settings()[["l3_stop_ct"]][["right"]])]}
    chooserInput("l3_stop_ct", "Available", "Selected",
                 unselected, dest, size = 10, multiple = TRUE
    )
    
  })
  
  output$l3_lane_desc<-renderUI({
    dat<-FIL()$Lane
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l3_lane_desc"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l3_lane_desc"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l3_lane_desc"]][["right"]])]}
    chooserInput("l3_lane_desc", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l3_orig_state<-renderUI({
    dat<-FIL()$Orig_State
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l3_orig_state"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l3_orig_state"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l3_orig_state"]][["right"]])]}
    chooserInput("l3_orig_state", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l3_load_region<-renderUI({
    dat<-FIL()$Load_Region
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    chooserInput("l3_load_region", "Available", "Selected",
                 dest, c(), size = 10, multiple = TRUE
    )
    
  })
  
  output$l3_delivery_state<-renderUI({
    dat<-FIL()$Delivery_State
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l3_load_region"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l3_load_region"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l3_load_region"]][["right"]])]}
    chooserInput("l3_delivery_state", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  output$l3_delivery_region<-renderUI({
    dat<-FIL()$Delivery_Region
    dest<-unique(dat)
    dest<-sort(dest)###this removes NA's from the list so we need to add an option in the list for NA's
    selected <- c()
    if (!is.null(Read_Settings()[["l3_delivery_state"]])){
      selected <- dest[which(dest%in%Read_Settings()[["l3_delivery_state"]][["right"]])]
      dest <- dest[which(!dest%in%Read_Settings()[["l3_delivery_state"]][["right"]])]}
    chooserInput("l3_delivery_region", "Available", "Selected",
                 dest, selected, size = 10, multiple = TRUE
    )
    
  })
  
  
  #Working UI elements for bundles 3, 4, & 5
  
  output$min_lead_slider <- renderUI({
    out1 <- sliderInput(inputId = "min_lead",
                        label = "Minimum Lead to Fit (in weeks)",
                        min = 0,
                        max = 12,
                        value = 0,
                        step = 1
    )
    if (!is.null(Read_Settings()[["min_lead"]])) updateSliderInput(session, inputId= "min_lead", value = Read_Settings()[["min_lead"]])
    return(out1)
  })
  
  output$max_lead_slider <- renderUI({
    out1 <- sliderInput(inputId="max_lead",
                        label="Maximum Lead to Fit (in weeks)",
                        min=0,
                        max=12,
                        value=0,
                        step=1                
    )
    if (!is.null(Read_Settings()[["max_lead"]])) updateSliderInput(session, inputId= "max_lead", value = Read_Settings()[["max_lead"]])
    return(out1)
  })
  
  output$max_model_slider <- renderUI({
    out1 <- sliderInput(inputId="max_model",
                        label="Number of Variables in Model",
                        min=1,
                        max=10,
                        value=1,
                        step=1                
    )
    if (!is.null(Read_Settings()[["max_model"]])) updateSliderInput(session, inputId= "max_model", value = Read_Settings()[["max_model"]])
    return(out1)
  })
  
  output$backcast_ahead_slider <- renderUI({
    out1 <- sliderInput(inputId="backcast_ahead",
                        label="How Many Weeks to Predict Ahead?",
                        min=1,
                        max=52,
                        value=52,
                        step=1                
    )
    if (!is.null(Read_Settings()[["backcast_ahead"]])) updateSliderInput(session, inputId= "backcast_ahead", value = Read_Settings()[["backcast_ahead"]])
    return(out1)
  })
  
  output$backcast_length_slider <- renderUI({
    out1 <- sliderInput(inputId = "backcast_length",
                        label = "Length of Backcast Fit (in weeks)",
                        min = 1,
                        max = 52,
                        value = 52,
                        step = 1                
    )
    if (!is.null(Read_Settings()[["backcast_length"]])) updateSliderInput(session, inputId= "backcast_length", value = Read_Settings()[["backcast_length"]])
    return(out1)
  })
  
  output$gamma_numeric <- renderUI({
    numericInput(inputId="gamma", 
                 label="smooth penalty:", 
                 value=1.4
                 
    )
  })
  
  output$interaction_split <- renderUI({
    numericInput(inputId="interaction_split", 
                 label="Day to Split First/Second Half of Season for Interaction:", 
                 value=182,min=1,max=365,step=1
                 
    )
  })
  
  output$pick_numeric <- renderUI({
    numericInput(inputId="pick", 
                 label="Variable to Pick to Add to Model:", 
                 value=1
                 
    )
  })
  
  output$interaction_check <- renderUI({
    checkboxInput(inputId = "interaction",
                  label = "1/2 year Interaction for Volume Terms?",
                  value=FALSE
    )
  })
  
  output$seasonality <- renderUI({
    checkboxInput(inputId = "seasonality",
                  label = "Estimate Seasonality?",
                  value=TRUE
    )
  })
  
  output$linear <- renderUI({
    checkboxInput(inputId = "linear",
                  label = "Estimate Inflation?",
                  value=TRUE
    )
  })
  
  output$predictors_checkgroup <- renderUI({
    x <-data.frame(LANE_BUNDLE()[['DATA_FILL']])
    date <-x$Align_date
    date_cols<-grep("date",colnames(x))
    x<-x[,-date_cols]
    colnames(x)<-gsub("_data","",colnames(x))
    
    choice<-colnames(x)
    sel<-choice[which(choice %in% "Oil_Fuel.Weekly.U.S..No.2.Diesel.Retail.Prices...Dollars.per.Gallon.")]
    if (!is.null(Read_Settings())){
      sel <- choice[which(choice%in%Read_Settings()[["predictors"]])]}
    checkboxGroupInput(inputId = "predictors",
                       label = "Select Predictors",
                       choices = choice, selected=sel
    )
  })
  
  
  output$volume_checkgroup <- renderUI({
    x <-data.frame(LANE_BUNDLE()[['DATA_FILL']])
    date <-x$Align_date
    date_cols<-grep("date",colnames(x))
    x<-x[,-date_cols]
    colnames(x)<-gsub("_data","",colnames(x))
    choice<-colnames(x)
    choice<-choice[grep("volume",choice)]
    sel<- choice[1]
    if (!is.null(Read_Settings())){
      sel <- choice[which(choice%in%Read_Settings()[["volume"]])]}
    checkboxGroupInput(inputId = "volume",
                       label = "Select Prediction Lane Volume",
                       choices = choice, selected=sel
    )
  })
  
  output$response_radio <- renderUI({
    x <-data.frame(LANE_BUNDLE()[['DATA_FILL']])
    date <-x$Align_date
    date_cols<-grep("date",colnames(x))
    x<-x[,-date_cols]
    colnames(x)<-gsub("_data","",colnames(x))
    
    choice<-colnames(x)
    choice_r<-choice[grep("RPM",colnames(x))]
    sel_r<-choice_r[which(choice_r %in% "CHR_Lane_1.RPM")]
    
    if (!is.null(Read_Settings()[["response"]])){
      sel_r <- choice_r[which(choice_r%in%Read_Settings()[["response"]])]}
    radioButtons(inputId = "response",
                 label = "Select Response",
                 choices = choice_r, selected=sel_r
    )
  })
  
  output$select_radio <- renderUI({
    PLOT=LANE_BUNDLE()[['PLOT']]
    choice3<-as.list(unique(PLOT$group))
    names(choice3)<-unique(PLOT$group)
    choice3 <- names(choice3)
    sel <- choice3[1]
    if (!is.null(Read_Settings()[["select"]])){
      sel <- choice3[which(choice3%in%Read_Settings()[["select"]])]}
    radioButtons(inputId = "select",
                 label = "Data Selection for Plot",
                 choices = choice3,
                 selected = sel
    )
  })
  
  output$CI_percentile<- renderUI({
    out1 <- sliderInput(inputId = "CI_percentile",
                        label = "Percentile for Confidence Inerval (%)",
                        min = 60,
                        max = 99,
                        value = 95,
                        step = 1                
    )
    if (!is.null(Read_Settings()[["CI_percentile"]])) updateSliderInput(session, inputId= "CI_percentile", value = Read_Settings()[["CI_percentile"]])
    return(out1)
  })
  
  
  ######################################################################################
  ########################### FUCK           ###################################
  ######################################################################################
  output$quote_date<-renderUI({
    smooth_vals<-mod()[["smooth_data"]]
    idxx<-smooth_vals[[5]]=="observed"
    datevect <- smooth_vals[[1]][!idxx]
    a=min(datevect)
    b=max(datevect)
    out1 <- dateRangeInput("quote_date", "Date Cutoff Ranges for Integrated Quote:", 
                   start =a, end =b )
    if (!is.null(Read_Settings()[["quote_date"]])) updateDateRangeInput(session, inputId= "quote_date", start = Read_Settings()[["quote_date"]][1], end = Read_Settings()[["quote_date"]][2])
    return(out1)
  })
  
  
  output$matrix_volume <- renderUI({
    smooth_vals<-mod()[["smooth_data"]]
    vol_vals<-vol_integrator()
    datevect <- smooth_vals[[1]]
    idx<-datevect>=input$quote_date[1] & datevect<=input$quote_date[2]
    idxx<-smooth_vals[[5]]=="observed"
    datatrans <- matrix(NA,nrow=nrow(smooth_vals),ncol = 3)
    #datatrans[idxx,1] <- smooth_vals[[2]][idxx]
    datatrans[idxx,1] <- vol_vals[[2]][idxx]
    datatrans[!idxx,3] <- smooth_vals[[2]][!idxx]
    datatrans[!idxx,2] <- vol_vals[[2]][!idxx]
    matrix_preds<-data.frame("Date"=datevect[!idxx],"RPM"=round(datatrans[!idxx,3],2),"Volume"=round(datatrans[!idxx,2],2))
    if (!is.null(Read_Settings()[["matrix_volume"]])){
      matrix_preds<-data.frame(Read_Settings()[["matrix_volume"]])
    }
    matrixCustom('matrix_volume', 'Future Values For Quote Construction',matrix_preds)
    ###you can access these values with input$matrix_volume as the variable anywhere in the server side file
    
  })
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  #################################################################################################################################
  #################################### Bundle 1 data processing insertion point#########
  ##################################################################################################################################
  
  Read_Settings <- reactive({
    inFile <- input$settings_file
    if (is.null(inFile)) return(NULL)
    load(inFile$datapath)
    return(saved_settings)
  })
  
  Change_static_settings <- observe({
    if (is.null(Read_Settings())) return(NULL)
    updateSliderInput(session, inputId= "rand_samp",value = Read_Settings()[["rand_samp"]])
    updateNumericInput(session, inputId= "gamma", value = Read_Settings()[["gamma"]])
    updateNumericInput(session, inputId= "interaction_split", value = Read_Settings()[["interaction_split"]])
    updateNumericInput(session, inputId= "pick", value = Read_Settings()[["pick"]])
    
    updateSelectInput(session, inputId="lanes_choice", selected = Read_Settings()[["lanes_choice"]])
    
    updateCheckboxInput(session, inputId="interaction", value = Read_Settings()[["interaction"]])
    updateCheckboxInput(session, inputId="seasonality", value = Read_Settings()[["seasonality"]])
    updateCheckboxInput(session, inputId="linear", value = Read_Settings()[["linear"]])
    updateCheckboxInput(session, inputId="carry_forward", value = Read_Settings()[["carry_forward"]])
    
    updateTextInput(session, inputId= "lane1_id", value = Read_Settings()[["lane1_id"]])
    updateTextInput(session, inputId= "lane2_id", value = Read_Settings()[["lane2_id"]])
    updateTextInput(session, inputId= "lane3_id", value = Read_Settings()[["lane3_id"]])
  })
  
  output$downloadData<-downloadHandler(
    filename = function(){paste(input$settings_name,".RData",sep = "")},
    content = function(file){
      saved_settings <- reactiveValuesToList(input)
      save(saved_settings, file = file)
    })
  
  Data<-reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dat<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, na.strings=c("n/a","XXX"),nrows=-1)
    idx<-sample(1:nrow(dat),size=floor(input$rand_samp*nrow(dat)/100))
    dat<-dat[idx,]
  })
  
  
  READ_API <- reactive({###use tmp file from disk or specify names of variables to extract
    CENSUS=NULL
    FUEL_DATA=NULL
    path=NULL
    inFile <- input$api_file
    if (is.null(inFile)){
      if(file.exists("census_api.csv")){
        api_readtable<-read.csv(header = TRUE, file = "census_api.csv",sep=",")
        apicolnames <- colnames(api_readtable)
        path=NULL
      }else{return(NULL)}
    }else{
      load(inFile$datapath)
      CENSUS=saved_api[["CENSUS"]]
      FUEL_DATA=saved_api[["FUEL_DATA"]]
      api_readtable=saved_api[["api_readtable"]]
      apicolnames <- colnames(api_readtable)
      path=inFile$datapath
    }
    return(structure(list("api_readtable" = api_readtable,"CENSUS"=CENSUS,"FUEL_DATA"=FUEL_DATA,"apicolnames" = apicolnames,"path"=path)))
  })
  
  
  API_Update<-reactive({
    if(input$refresh==0){
      api_readtable <- READ_API()[["api_readtable"]]
    }
    else{
      isolate({
        api_readtable <- data.frame(input$api_names)
        api_readtable<-api_readtable[2:nrow(api_readtable),]
        colnames(api_readtable) <- READ_API()[["apicolnames"]]
      })}
    return(api_readtable)
  })
  
  API<-reactive({
    if(is.null(READ_API())){return(NULL)}
    CHR<-FINAL()
    CENSUS<-READ_API()[["CENSUS"]]
    FUEL_DATA<-READ_API()[["FUEL_DATA"]]
    api_readtable<-API_Update()
    path<-READ_API()[["path"]]
    
    
    
    fuel_choice<-c("PET.EMD_EPD2D_PTE_NUS_DPG.W","PET.EMD_EPD2D_PTE_R10_DPG.W","PET.EMD_EPD2D_PTE_R1X_DPG.W",
                   "PET.EMD_EPD2D_PTE_R1Y_DPG.W","PET.EMD_EPD2D_PTE_R1Z_DPG.W","PET.EMD_EPD2D_PTE_R20_DPG.W",
                   "PET.EMD_EPD2D_PTE_R30_DPG.W","PET.EMD_EPD2D_PTE_R40_DPG.W","PET.EMD_EPD2D_PTE_R50_DPG.W ",
                   "PET.EMD_EPD2D_PTE_R5XCA_DPG.W","PET.EMD_EPD2D_PTE_SCA_DPG.W")
    
    if (input$refresh!=0 | is.null(path)){
    FUEL_DATA<-vector("list",length(fuel_choice))
    fuel_names<-character(length(fuel_choice))
    for (beta in 1:length(fuel_choice)){
    Json_fuel<-EPA_API(series=fuel_choice[beta],key="A9BCC61DA44BA0C0ECA4A42D622D7D44")
    date<-c()
    fuel<-c()
    for (i in 1:length(Json_fuel$series[[1]][['data']])){
      date<-c(date,Json_fuel$series[[1]][['data']][[i]][[1]])
      fuel<-c(fuel,Json_fuel$series[[1]][['data']][[i]][[2]])
    }
    FUEL<-data.frame(Date=as.Date(date,format="%Y%m%d"),X=as.numeric(fuel))
    if (beta>=2){
      indx<-FUEL_DATA[[1]][,1] %in% FUEL[,1] 
      FUEL[,2]<-FUEL[,2]-FUEL_DATA[[1]][indx,2]
      colnames(FUEL)[2]<-paste("Diff_from_us_avg",Json_fuel$series[[1]][['name']],sep="_")
    }
    if (beta==1){colnames(FUEL)[2]<-Json_fuel$series[[1]][['name']]}
    FUEL_DATA[[beta]]<-FUEL
    }}
    
    
    
    series<-api_readtable
    if (input$refresh!=0 | is.null(path)){CENSUS<-CENSUS_API(series=series,key="cf2bc020b12d020f8ee3155f74198a21dc585845")}
    fuel_length<-length(FUEL_DATA)
    out<-vector("list",length(CENSUS)+fuel_length)##add in a slots for fuel data
    out[1:fuel_length]<-FUEL_DATA
    names(out)<-NA
    names(out)[1:length(FUEL_DATA)]<-fuel_choice
    i=1
    for (i in 1:length(CENSUS)){
      rows<-length(CENSUS[[i]])
      a<-series$path_desc[i]
      b<-series$category_desc[i]
      c<-series$data_type_desc[i]
      name<-paste(a,":",b,"(",c,")",sep="")
      temp_matrix<-t(matrix(unlist(CENSUS[[i]][2:rows]),nrow=5))
      data<-as.numeric(temp_matrix[,1])
      if (length(grep("Q",temp_matrix[1,4]))==0){
        date<-as.Date(paste0(temp_matrix[,4],"-01"),format="%Y-%m-%d")} else{
          tt<-temp_matrix[,4]
          tt<-gsub("Q1","01-01",tt)
          tt<-gsub("Q2","04-01",tt)
          tt<-gsub("Q3","07-01",tt)
          tt<-gsub("Q4","10-01",tt)
          date<-as.Date(tt,format="%Y-%m-%d")
        }
      tp<-data.frame(Date=date,X=data)
      colnames(tp)[2]<-name
      out[[i+fuel_length]]<-tp
      names(out)[i+fuel_length]<-paste(CENSUS[[i]][[2]][2:3],collapse="_")
    }
    Indicators<-out
    
    for (i in names(Indicators)){
      tmpcmd<-paste(i,"=averages(Indicators[[\"",i,"\"]],d_index=1)",sep="")
      eval(parse(text=tmpcmd))
    }
    
    y=names(Indicators)
    tmpcmd<-paste("\"",y,"\"=",y,"$WEEK[,-c(1,4)],",sep="")
    tmpcmd<-paste(tmpcmd,collapse="")
    tmpcmd<-substr(tmpcmd,1,nchar(tmpcmd)-1)
    
    tmpcmd<-paste("X=structure(list(",tmpcmd,"))",sep="")
    eval(parse(text=tmpcmd))
    

    START=format(min(CHR$Date),format="%Y-%m-%d")
    END=format(Sys.time(),format="%Y-%m-%d")
    DATA_I<-align_week(X,d_index=rep(1,length(ls(X))),start=START,end=END)
    for (i in 2:length(DATA_I)){
      DATA_I[[i]]<-DATA_I[[i]][-c(1)]
    }
    
    #DATA_FILL_I<-loess_fill(DATA_I,t_index=1,span=c(10:1/10),folds=5)
    DATA_FILL_I<-PIECE_fill(DATA_I,t_index=1)
    
    list(DATA_I=DATA_I,DATA_FILL_I=DATA_FILL_I,CENSUS=CENSUS,FUEL_DATA=FUEL_DATA,path=path,api_readtable=api_readtable)
  })
  
  output$API_SAVE<-downloadHandler(
    filename = function(){paste(input$API_SAVE_NAME,".RData",sep = "")},
    content = function(file){
      saved_api<-API()
      #saved_api <- reactiveValuesToList(tmp)
      save(saved_api, file = file)
    })

  
  

  
  choices<-reactive({
    tmp<-colnames(Data())
    return(tmp)
  })
  
  CHR<-reactive({
    CHR<-data.frame(
      Date=Data()[,colnames(Data()) %in% input$date$right],
      Stop_Count=Data()[,colnames(Data()) %in% input$stop$right],
      Total_Cost=Data()[,colnames(Data()) %in% input$cost$right],
      Total_Mileage=Data()[,colnames(Data()) %in% input$mileage$right],
      Destination_Zip=Data()[,colnames(Data()) %in% input$destination$right],
      Origin_Zip=Data()[,colnames(Data()) %in% input$origin$right],
      Lane=Data()[,colnames(Data()) %in% input$lane_choices$right],
      Orig_State=Data()[,colnames(Data()) %in% input$orig_state$right],
      Load_Region=Data()[,colnames(Data()) %in% input$load_region$right],
      Delivery_State=Data()[,colnames(Data()) %in% input$delivery_state$right],
      Delivery_Region=Data()[,colnames(Data()) %in% input$delivery_region$right])
    CHR$Date<-as.POSIXlt((CHR$Date-1)*24*60*60,origin="1900-01-01")
    CHR$Date<-format(CHR$Date,format="%m/%d/%Y")
    CHR$Date<-as.Date(CHR$Date,format="%m/%d/%Y")
    CHR$RPM<-CHR$Total_Cost/CHR$Total_Mileage
    CHR$RPM[is.infinite(CHR$RPM)]=NA
    CHR$RPM[CHR$RPM<=0 | CHR$RPM>=10]=NA
    CHR=CHR[!is.na(CHR$RPM),]
    return(CHR)
    
  })
  
  FIL<-reactive({
    FIL<-CHR()
    FIL<-FIL[FIL$Date>=input$date_range[1] & FIL$Date<=input$date_range[2],]
    FIL<-FIL[FIL$Total_Cost>=input$cost_lower & FIL$Total_Cost<=input$cost_upper,]
    FIL<-FIL[FIL$Total_Mileage>=input$miles_lower & FIL$Total_Mileage<=input$miles_upper,]
    FIL<-FIL[FIL$RPM>=input$RPM_lower & FIL$RPM<=input$RPM_upper,]
    
    for (i in 1:ncol(FIL)){
      if(class(FIL[[i]]) %in% c("factor")){
        levels(FIL[[i]])[which(levels(FIL[[i]])=="")]="UNKNOWN"
        
      }
      
    }
    
    
    
    
    return(FIL)
    
  })
  
  L1<-reactive({
    a=FIL()$Origin_Zip %in% as.numeric(input$l1_orig_zip$right) 
    b=FIL()$Destination_Zip %in% as.numeric(input$l1_dest_zip$right)
    #c=FIL()$Lane %in% input$l1_lane_desc$right
    d=FIL()$Orig_State %in% input$l1_orig_state$right
    e=FIL()$Load_Region %in% input$l1_load_region$right
    f=FIL()$Delivery_State %in% input$l1_delivery_state$right
    g=FIL()$Delivery_Region %in% input$l1_delivery_region$right
    h=FIL()$Stop_Count %in% as.numeric(input$l1_stop_ct$right)
    
    LANE1<-FIL()[ ((a | d | e) & (b | f | g)) & h,]
    LANE1$Constructed_Lane<-input$lane1_id 
    LANE1
    
  })
  
  L2<-reactive({
    a=FIL()$Origin_Zip %in% as.numeric(input$l2_orig_zip$right) 
    b=FIL()$Destination_Zip %in% as.numeric(input$l2_dest_zip$right)
    #c=FIL()$Lane %in% input$l2_lane_desc$right
    d=FIL()$Orig_State %in% input$l2_orig_state$right
    e=FIL()$Load_Region %in% input$l2_load_region$right
    f=FIL()$Delivery_State %in% input$l2_delivery_state$right
    g=FIL()$Delivery_Region %in% input$l2_delivery_region$right
    h=FIL()$Stop_Count %in% as.numeric(input$l2_stop_ct$right)
    
    LANE2<-FIL()[ ((a | d | e) & (b | f | g)) & h,]
    LANE2$Constructed_Lane<-input$lane2_id 
    LANE2
    
  })
  
  L3<-reactive({
    a=FIL()$Origin_Zip %in% as.numeric(input$l3_orig_zip$right)
    b=FIL()$Destination_Zip %in% as.numeric(input$l3_dest_zip$right)
    #c=FIL()$Lane %in% input$l3_lane_desc$right
    d=FIL()$Orig_State %in% input$l3_orig_state$right
    e=FIL()$Load_Region %in% input$l3_load_region$right
    f=FIL()$Delivery_State %in% input$l3_delivery_state$right
    g=FIL()$Delivery_Region %in% input$l3_delivery_region$right
    h=FIL()$Stop_Count %in% as.numeric(input$l3_stop_ct$right)
    
    LANE3<-FIL()[ ((a | d | e) & (b | f | g)) & h,]
    LANE3$Constructed_Lane<-input$lane3_id 
    LANE3
    
  })
  
  FINAL<-reactive({
    
    CHR<-switch(input$lanes_choice,
                "1"={L1()},
                "2"={rbind(L1(),L2())},
                "3"={rbind(L1(),L2(),L3())})
    
    TARGET_NAME<- input$lane1_id 
    return(CHR)
    
  })
  
  LANE_BUNDLE<-reactive({
    CHR<-FINAL()
    DATA_I<-API()[["DATA_I"]]
    DATA_FILL_I<-API()[["DATA_FILL_I"]]
    roll<-unique(CHR$Constructed_Lane)
    for (i in 1:length(roll)){
      y<-roll[i]
      tmpcmd<-paste("CHR_",y,"=averages(CHR[which(CHR$Constructed_Lane==\"",y,"\"),c(1,2,12)],d_index=1)",sep="")
      eval(parse(text=tmpcmd))
    }
    
    y=roll
    tmpcmd<-paste("\"CHR_",y,"\"=CHR_",y,"$WEEK[,-c(1)],",sep="")
    tmpcmd<-paste(tmpcmd,collapse="")
    tmpcmd_1<-substr(tmpcmd,1,nchar(tmpcmd)-1)
    
    tmpcmd<-paste("X=structure(list(",tmpcmd_1,"))",sep="")
    eval(parse(text=tmpcmd))
    str(X)
    length(ls(X))
    DATA<-align_week(X,d_index=rep(1,length(ls(X))),start=format(min(CHR$Date),format="%Y-%m-%d"),end=format(Sys.time(),format="%Y-%m-%d"))
    for (i in 2:length(DATA)){
      DATA[[i]]<-DATA[[i]][-c(1)]
    }
    
    dest=names(DATA_I)
    cc=dest
    for (i in 2:length(dest)){
      cc[i]=colnames(DATA_I[[dest[i]]])
    }
    keep<-cc %in% input$API_choice
    DATA_T<-c(DATA,DATA_I[keep])
    PLOT<-raw_plot_data(DATA_T,t_index=c(1))
    method="loess"
    if (method=="loess"){
      #DATA_FILL<-loess_fill(DATA,t_index=1,span=c(10:1/10),folds=5)
      DATA_FILL<-PIECE_fill(DATA,t_index=1)###use piecewise linear to preserve convex hull
      
      } else{
        DATA_FILL<-GAM_fill(DATA,t_index=1,gamma=0.5)}
    
    
    DATA_FILL<-c(DATA_FILL,DATA_FILL_I[keep])
    PLOT_FILL<-raw_plot_data(DATA_FILL,t_index=c(1))
    PLOT<-raw_plot_data(DATA_T,t_index=c(1))
    miss_index<-rep("observed",nrow(PLOT))
    miss_index[which(is.na(PLOT$values))]="imputed"
    PLOT_FILL<-data.frame(PLOT_FILL,miss_index)
    TARGET_NAME<- "Nothing for Now"
    tst <- structure(list("DATA_FILL"=DATA_FILL,
                          "PLOT_FILL"=PLOT_FILL,
                          "PLOT"=PLOT,
                          "TARGET_NAME"=TARGET_NAME))
    return(tst)
    
  })
  
  #############################################################################################################################################
  ############################### Starting point for original bundle 3 prep data once and then pass around the program used to be bundle 3
  #################################################################################################################################################
  item <- reactive({
    unlist(input$select)
  })
  
  
  
  
  
  # prep data once and then pass around the program used to be bundle 4
  
  mod1 <- reactive({
    x <-data.frame(LANE_BUNDLE()[['DATA_FILL']])
    date <-x$Align_date
    date_cols<-grep("date",colnames(x))
    x<-x[,-date_cols]
    colnames(x)<-gsub("_data","",colnames(x))
    
    response<-input$response
    max_model_size=input$max_model#number of variables to add
    min_lead=input$min_lead
    max_lead=input$max_lead
    predictors=input$predictors
    interaction_flag=input$interaction
    gamma=input$gamma##tuning parameter in fit
    backcast_ahead=input$backcast_ahead ##Prediction length ahead 
    pick=input$pick
    hold_out_data=0#amount of data to hold back for assessing model fit
    fixed=c()#172 is fuel
    lead_lag_store=c(0)###include fixed after a zero
    SEASON=input$seasonality##adjust for seasonality 365 day period (TRUE,FALSE)
    LINEAR=input$linear##adjust for inflation (TRUE,FALSE)
    interaction_split_day=input$interaction_split##cutpoint for interaction
    
    
    ############################################
    ############################################
    #####used to be mod1_source.r
    ############################################
    ############################################
    
    ####model kernel for variable selection and initial future value seeding
    ####This also fits the chosen model
    response<-which(colnames(x) %in% response)
    lead_lag=c(min_lead:max_lead)##reactive, positive is lead
    consider<-c(response,fixed,which(colnames(x) %in% predictors))
    colnames(x)[consider]
    interaction<-rep(FALSE,length(consider))
    interaction[grep("volume",colnames(x)[consider])]=interaction_flag
    
    
    ####remove all values where NA response###
    take_out<-!is.na(apply(x[response],1,mean))
    x<-x[take_out,]
    date<-date[take_out]
    
    ####Use the carry-forward rule to fill missing values###
    for (s in consider[-1]){
      x[is.na(x[,s]),s]=x[max(which(!is.na(x[,s]))),s]
    }
    
    
    
    #year<-as.numeric(format(date,"%Y"))
    #month<-format(date,"%B")
    #reference<-as.Date(strptime(paste(year,"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
    #week<-as.double(floor(difftime(date,reference,units="weeks")))
    #week[which(week==52)]=51 ##cut off last couple of days in year and group with last week
    #week[week<0]=0###deal with some where there are slightly negative numbers that get pushed to -1 with the floor command
    #week<-factor(week)###why is this here???
    
    
    ###cut out future data for backcasting after model fitting###
    
    
    ZZ<-x
    dateZZ<-date
    x<-ZZ[1:(nrow(ZZ)-hold_out_data),]
    date<-dateZZ[1:(nrow(ZZ)-hold_out_data)]
    XX<-x###store data for later use
    datexx<-date
    ################################################
    #######Screening Routine
    ################################################
    
    output<-data.frame(matrix(nrow=max_model_size,ncol=2))
    colnames(output)<-c("Model Terms","Model AIC")
    j=1
    for (j in 1:max_model_size){
      pull<-c(response,fixed)
      consider_tmp<-consider[-which(consider %in% pull)]
      MSEp<-matrix(ncol=3,nrow=length(consider_tmp)*length(lead_lag))
      rownames(MSEp)<-rep("NA",nrow(MSEp))
      colnames(MSEp)<-c("index","AIC","Lead Lag")
      t=1
      i=consider_tmp[1]
      for (i in consider_tmp){
        a=lead_lag[1]
        for (a in lead_lag){
          MSEp[t,1]=i
          MSEp[t,3]=a
          print(paste("model loop=",t," of ",nrow(MSEp)," model size=",j,"lead lag=",a))
          run<-c(response,fixed,i)
          lead_lagg<-c(lead_lag_store,a)
          x<-Lead_lag(XX,run,lead_lagg)
          max_l<-max(lead_lag)
          min_l<-min(lead_lag)
          keep<-rep(TRUE,nrow(x))
          if (max_l>0) {keep[1:max_l]=FALSE}
          if (min_l<0) {keep[(length(keep)+min_l+1):length(keep)]=FALSE}
          x<-x[keep,]
          date<-datexx
          date<-date[keep]
          u=1
          days=as.numeric(format(date,"%j"))
          season<-rep("first_half",length(days))
          season[which(days>=interaction_split_day)]="second_half"
          GAM_DATA<-cbind(days,"date"=as.numeric(date),season,x[run])
          preds<-c()
          for (z in run[-u]){
            if (interaction[which(consider %in% z)]==TRUE){
              preds<-c(preds,paste("s(",colnames(x[z]),",by=season)",sep=""))} else
              {preds<-c(preds,paste("s(",colnames(x[z]),")",sep=""))}
          }
          #preds<-c("s(date,sp=1000)","s(days,bs=\"cc\")",preds)
          if(SEASON==TRUE){preds<-c("s(days,bs=\"cc\")",preds)}
          if(LINEAR==TRUE){preds<-c("s(date,sp=1000)",preds)}
          preds<-paste(preds,collapse="+")
          tmpcmd<-paste("fit=gam(",colnames(x[run[u]]),"~",preds,",data=GAM_DATA,gamma=",gamma,")",sep="")
          eval(parse(text=tmpcmd))
          #y_hat<-predict(fit)
          #MSE<-sum((x[run[u]]-y_hat)^2)/nrow(x)
          MSEp[t,2]<-AIC(fit)
          rownames(MSEp)[t]<-colnames(x)[i]
          t=t+1
        }##close lead lag loop
      }##close variable run loop
      best<-MSEp[which.min(MSEp[,2]),1]
      lagg<-MSEp[which.min(MSEp[,2]),3]
      fixed<-c(fixed,best)
      lead_lag_store=c(lead_lag_store,lagg)
      x<-Lead_lag(XX,c(response,fixed),lead_lag_store)
      names<-colnames(x)
      output[j,2]<-MSEp[which.min(MSEp[,2]),2]
      output[j,1]<-paste(names[c(response,fixed)],collapse=" + ")
      
    }###end major loop%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    x<-XX###reset data
    date=datexx###reset date
    print(output)
    pt_data<-MSEp[order(MSEp[,2],decreasing=F),2]
    if(length(pt_data)==1){names(pt_data)=rownames(MSEp)}
    bb<-min(25,length(pt_data))
    pt_data<-pt_data[1:bb]
    pt_data<-pt_data[order(pt_data,decreasing=T)]
    MSEp<-MSEp[order(MSEp[,2],decreasing=F),]
    term_interest=pick
    if (length(nrow(MSEp))!=0){
      pred_index<-MSEp[term_interest,1]
      lead_lag_store[length(lead_lag_store)]<-MSEp[term_interest,3]
    }else{
      pred_index<-MSEp[1]
      lead_lag_store[length(lead_lag_store)]<-MSEp[3]}
    pred_index<-c(fixed[-length(fixed)],pred_index)
    run<-c(response,pred_index)
    interaction<-interaction[which(consider %in% run)]
    consider<-consider[consider %in% run]
    
    ####Select Lead/Lags For X
    x<-Lead_lag(x,run,lead_lag_store)
    keep=!is.na(apply(x[run],1,mean))
    x<-x[keep,]
    date<-date[keep]
    
    
    ##################################
    #####GAM Model
    #####For Multivariate Effect Adjustment
    #################################
    ######Begin Full Gam Fitting#####
    mean_vector<-numeric(length(run))
    error.stream<-data.frame(matrix(nrow=nrow(x),ncol=length(run)))
    days=as.numeric(format(date,"%j"))
    season<-rep("first_half",length(days))
    season[which(days>=interaction_split_day)]="second_half"
    GAM_DATA<-cbind(days,"date"=as.numeric(date),season,x[run])
    u=1
    for (u in 1:length(run)){
      
      preds<-c()
      for (z in run[-u]){
        if (interaction[which(consider %in% z)]==TRUE){
          preds<-c(preds,paste("s(",colnames(x[z]),",by=season)",sep=""))} else
          {preds<-c(preds,paste("s(",colnames(x[z]),")",sep=""))}
      }
      #preds<-c("s(date,sp=1000)","s(days,bs=\"cc\")",preds)
      if(SEASON==TRUE){preds<-c("s(days,bs=\"cc\")",preds)}
      if(LINEAR==TRUE){preds<-c("s(date,sp=1000)",preds)}
      preds<-paste(preds,collapse="+")
      tmpcmd<-paste("fit=gam(",colnames(x[run[u]]),"~",preds,",data=GAM_DATA,gamma=",gamma,")",sep="")
      eval(parse(text=tmpcmd))
      summary(fit)
      error.stream[u]<-residuals(fit)
      colnames(error.stream)[u]<-colnames(x[run[u]])
      if (u==1) fit_1=fit
    }
    
    
    ############################################
    ############################################
    ####Fit the future values for the predictors
    ############################################
    ############################################
    ymin<-min(min(predict(fit_1)),min(x[[response]]))
    ymax<-max(max(predict(fit_1)),max(x[[response]]))
    date_b<-datexx
    
    #####
    year_ahead<-as.numeric(format(date_b[length(date_b)],"%Y"))
    reference_ahead<-as.Date(strptime(paste(year_ahead,"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
    week_ahead<-as.double(floor(difftime(date_b[length(date_b)],reference_ahead,units="weeks")))
    ###make list of future weeks to pick from since dates suck to work with, make it 2X as long as needed then extract elements
    min<-as.POSIXlt(date_b[length(date_b)],origin="1970-01-01")
    end<-paste(year_ahead+ceiling(backcast_ahead/52)*2,format(date_b[length(date_b)],format="%m-%d"),sep="-")
    end<-as.Date(end,format="%Y-%m-%d")
    max<-as.POSIXlt(end,origin="1970-01-01")
    series<-min+(0:difftime(max,min,units="days"))*24*60*60
    series<-as.Date(series,format="%Y-%m-%d")
    year<-as.numeric(format(series,format="%Y"))
    reference<-as.Date(strptime(paste(year,"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
    week<-as.double(floor(difftime(series,reference,units="weeks")))
    week[which(week==52)]=51 ##cut off last couple of days in year and group with last week
    year_week<-paste(year,week,sep="-")
    year_week<-unique(year_week)
    concat<-strsplit(year_week,"-")
    year<-numeric(length(year_week))
    week<-numeric(length(year_week))
    reference<-numeric(length(year_week))
    class(reference)<-"Date"
    for (s in 1:length(year_week)){
      year[s]=as.numeric(concat[[s]][1])
      week[s]=as.numeric(concat[[s]][2])
      reference[s]=as.Date(strptime(paste(year[s],"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
    }
    week_ahead<-week[2:(backcast_ahead+1)]
    year_ahead<-year[2:(backcast_ahead+1)]
    reference_ahead<-reference[2:(backcast_ahead+1)]
    time_ahead<-as.Date(as.POSIXct(reference_ahead,origin="1970-01-01")+week_ahead*7*24*60*60,format="%Y-%m-%d")
    
    
    method_pred="Loess" ###set prediciton method between gam and loess
    if (method_pred=="Loess"){
      LM<-linear_detrend(XX[run[-1]],datexx)
      X_linear<-detrend_linear(XX[run[-1]],LM)
      LOESS<-loess_detrend(X_linear,datexx,folds=5,span=1:10/10)
      X_loess<-detrend_loess(X_linear,LOESS)
      Z<-X_loess
      FUTURE<-mean_future(time_ahead,Z,LM,LOESS)} else{
        FUTURE<-mean_future_GAM(time_ahead,datexx,XX[run[-1]],gamma)  
      }
    
    plot_dat<-rbind(XX[run[-1]],FUTURE)
    plot_time<-c(datexx,time_ahead)
    plot_group<-rep("Predicted",length(plot_time))
    plot_group[1:length(datexx)]="Observed"
    plot_group<-rep(plot_group,length(ls(FUTURE)))
    plot_time<-rep(plot_time,length(ls(FUTURE)))
    tmp_dat<-stack(plot_dat)
    tmp_dat<-cbind(tmp_dat,plot_time,plot_group)
    
    
    for (b in 1:ncol(FUTURE)){
      maxx<-max(x[run[b+1]])
      minx<-min(x[run[b+1]])
      FUTURE[which(FUTURE[,b]>=maxx),b]=maxx
      FUTURE[which(FUTURE[,b]<=minx),b]=minx
    }
    pull_future<-FUTURE
    pull_time_ahead<-time_ahead
    
    
    ############################################
    ############################################
    #####used to be mod1_source.r
    ############################################
    ############################################
    
    
    
    tst<-structure(list("pt_data"=pt_data,"response"=response,"interaction"=interaction,"names"=names,"fixed"=fixed,"pull_future"=pull_future,
                        "fit_1"=fit_1,"ymin"=ymin,"ymax"=ymax,"x"=x,"run"=run,"date"=date,"pull_time_ahead"=pull_time_ahead,
                        "tmp_dat"=tmp_dat,"ZZ"=ZZ,"FUTURE"=FUTURE,"XX"=XX,"error.stream"=error.stream,
                        "dateZZ"=dateZZ,"time_ahead"=time_ahead,"lead_lag_store"=lead_lag_store,"consider"=consider,
                        "backcast_ahead"=backcast_ahead,"datexx"=datexx,"gamma"=gamma))
    
  })
  
  
  output$carry_forward <- renderUI({
    XX<-mod1()[['XX']]
    run<-mod1()[['run']]
    choice<-colnames(XX)[run[-1]]
    

    checkboxGroupInput(inputId = "carry_forward",
                       label = "Carry Last Observation Forward?",
                       choices = choice)
  })
  
  
  
  output$matrix_values <- renderUI({
    matrix_preds<-data.frame("Future Date"=mod1()[['pull_time_ahead']],"Future Values"=round(mod1()[['pull_future']],2))
    
    if (!is.null(Read_Settings()[["table_values"]])){
      matrix_preds<-data.frame(Read_Settings()[["table_values"]])
    }
    
    if(!is.null(input$carry_forward)){###put in carry forward values here if selected
      XX<-mod1()[['XX']]
      idx<-input$carry_forward
      NEW<-mod1()[['pull_future']]
      NEW[,idx]<-XX[nrow(XX),idx]
      matrix_preds<-data.frame("Future Date"=mod1()[['pull_time_ahead']],"Future Values"=round(NEW,2))
    }
      
      
    matrixCustom('table_values', 'Future Values Needed For Prediction',matrix_preds)
    ###you can access these values with input$table_values as the variable anywhere in the server side file
    
  })
  
  
  
  mod<-reactive({###had to break here to handle reactive data items for the Future values
    TARGET_NAME=LANE_BUNDLE()[['TARGET_NAME']]
    pt_data=mod1()[['pt_data']]
    response=mod1()[['response']]
    names=mod1()[['names']]
    fixed=mod1()[['fixed']]
    pull_future=mod1()[['pull_future']]
    fit_1=mod1()[['fit_1']]
    ymin=mod1()[['ymin']]
    ymax=mod1()[['ymax']]
    x=mod1()[['x']]
    run=mod1()[['run']]
    date=mod1()[['date']]
    pull_time_ahead=mod1()[['pull_time_ahead']]
    tmp_dat=mod1()[['tmp_dat']]
    ZZ=mod1()[['ZZ']]
    dateZZ=mod1()[['dateZZ']]
    time_ahead=mod1()[['time_ahead']]
    datexx=mod1()[['datexx']]
    backcast_ahead=mod1()[['backcast_ahead']]
    FUTURE=mod1()[['FUTURE']]
    XX=mod1()[['XX']]
    error.stream=mod1()[['error.stream']]
    lead_lag_store=mod1()[['lead_lag_store']]
    consider=mod1()[['consider']]
    interaction=mod1()[['interaction']]
    gamma=mod1()[['gamma']]
    table_values=input$table_values
    interaction_split_day=input$interaction_split##cutpoint for interaction
    CI_pct<-input$CI_percentile
    CI_Z_score=abs(qnorm((1-CI_pct/100)/2))

    
    
    #########################################################################
    ###used to be mod_source.r
    #########################################################################
    
    ###Performs the future value prediction
    ####use input table
    if (!is.null(table_values)){###don't active first pass (reactive data goes first in shiny)
      if((ncol(table_values)-1)==ncol(FUTURE) & nrow(FUTURE)==nrow(table_values)){
        pf=table_values
        pf<-unclass(pf[,-1])
        class(pf)<-"numeric"
        pf=data.frame(pf)
        colnames(pf)<-colnames(FUTURE)
        FUTURE<-pf}}
    
    
    
    plot_dat<-rbind(XX[run[-1]],FUTURE)
    plot_time<-c(datexx,time_ahead)
    plot_group<-rep("Predicted",length(plot_time))
    plot_group[1:length(datexx)]="Observed"
    plot_group<-rep(plot_group,length(ls(FUTURE)))
    plot_time<-rep(plot_time,length(ls(FUTURE)))
    tmp_dat<-stack(plot_dat)
    tmp_dat<-cbind(tmp_dat,plot_time,plot_group)
    
    
    ###Look at GAM error structure###
    #acf(error.stream,lag.max=52)
    #VARselect(error.stream,lag.max=20,type="const")
    fit<-VAR(error.stream,ic=c("SC"),lag.max=10,type="const")
    summary(fit)
    fit<-restrict(fit,method="ser",thresh=1)
    summary(fit)
    predicted_vals=predict(fit,n.ahead=backcast_ahead)
    
    
    
    mean_delta=predicted_vals$fcst[[1]][,1]
    lcl_delta=predicted_vals$fcst[[1]][,4]
    ucl_delta=predicted_vals$fcst[[1]][,4]
    
    
    plot_dat<-rbind(XX[run[-1]],FUTURE)
    plot_time<-c(datexx,time_ahead)
    FUTURE<-Lead_lag(plot_dat,1:length(plot_dat),lead_lag_store[-1])
    keep=!is.na(apply(FUTURE,1,mean))
    tmp=colnames(FUTURE)
    FUTURE<-data.frame(FUTURE[keep,])
    FUTURE<-data.frame(FUTURE[(nrow(FUTURE)-backcast_ahead+1):nrow(FUTURE),])
    colnames(FUTURE)<-tmp
    time_ahead<-plot_time[keep]
    time_ahead<-time_ahead[(length(time_ahead)-backcast_ahead+1):length(time_ahead)]
    
    
    
    
    days=as.numeric(format(time_ahead,"%j"))
    season<-rep("first_half",length(days))
    season[which(days>=interaction_split_day)]="second_half"
    GAM_DATA<-cbind(days,"date"=as.numeric(time_ahead),season,FUTURE)
    predicted<-predict(fit_1,newdata=GAM_DATA,se.fit=T,pred.var=0)
    predicted_adj<-as.numeric(predicted$fit)+mean_delta
    lcl=predicted_adj-CI_Z_score*sqrt((lcl_delta/1.96)^2+(as.numeric(predicted$se.fit))^2)
    ucl=predicted_adj+CI_Z_score*sqrt((lcl_delta/1.96)^2+(as.numeric(predicted$se.fit))^2)
    char<-paste("GAM_Predictions_",ls(x[response]),sep="")
    tmpcmd=paste("GAM_predictions=data.frame(time_ahead,",char,"=predicted_adj,lcl,ucl,FUTURE)",sep="")
    eval(parse(text=tmpcmd))
    GAM_predictions
    #write.csv(GAM_predictions,file=paste(TARGET_NAME,"GAM Predictions.csv"))
    
    ########################add in daily interploation ##########################
    
    smooth_data<-data.frame(date=c(dateZZ,GAM_predictions[[1]]),values=c(ZZ[[response]],GAM_predictions[[2]]),
                            LCL=c(rep(NA,nrow(ZZ)),GAM_predictions[[3]]),UCL=c(rep(NA,nrow(ZZ)),GAM_predictions[[4]]),
                            group=c(rep("observed",length(dateZZ)),rep("predicted",length(GAM_predictions[[1]]))))
    min<-as.POSIXlt(min(smooth_data[[1]]),origin="1970-01-01")
    max<-as.POSIXlt(max(smooth_data[[1]]),origin="1970-01-01")+3.5*24*60*60
    series<-min+(0:difftime(max,min,units="days"))*24*60*60
    series<-as.Date(series,format="%Y-%m-%d")
    obs_dates<-as.Date(as.POSIXlt(smooth_data[[1]],origin="1970-01-01")+ c(diff(smooth_data[[1]])/2,3.5)*24*60*60,format="%Y-%m-%d")
    smooth_values<-numeric(length(series))
    smooth_values[1:length(smooth_values)]<-NA
    smooth_values[series %in% obs_dates]<-smooth_data[[2]]
    smooth_group<-factor(rep(NA,length(series)),levels=levels(smooth_data[[5]]))
    smooth_group[series %in% obs_dates]<-smooth_data[[5]]
    smooth_LCL<-numeric(length(series))
    smooth_LCL[1:length(smooth_LCL)]<-NA
    smooth_LCL[series %in% obs_dates]<-smooth_data[[3]]
    smooth_UCL<-numeric(length(series))
    smooth_UCL[1:length(smooth_UCL)]<-NA
    smooth_UCL[series %in% obs_dates]<-smooth_data[[4]]
    smooth_data<-data.frame(date=series,values=smooth_values,LCL=smooth_LCL,UCL=smooth_UCL,group=smooth_group)
    transition<-floor(mean(c(max(which(smooth_data[[5]]=="observed")),min(which(smooth_data[[5]]=="predicted")))))
    smooth_data[[5]][1:transition]<-"observed"
    smooth_data[[5]][(transition+1):nrow(smooth_data)]<-"predicted"
    input_dat<-list(date=smooth_data[,1],data=smooth_data[,2,drop=F])
    #ttmmpp<-PIECE_fill(input_dat,t_index=1)
    #smooth_data[2]<-ttmmpp[[2]]
    smooth_data[2] <- na.approx(smooth_data[2], na.rm = FALSE)

    
    
    work<-smooth_data[[5]]=="predicted"
    band<-smooth_data[[4]][work]-smooth_data[[2]][work]
    ttime<-smooth_data[[1]][work]
    non_empty<-which(!is.na(band))
#     for (g in 2:length(non_empty)){###piecewise linear fit
#       ff<-(non_empty[g-1]+1):(non_empty[g]-1)
#       dat<-data.frame(y=band[non_empty[(g-1):g]],x=ttime[non_empty[(g-1):g]])
#       qf<-lm(y~x,data=dat)
#       band[ff]<-predict(qf,newdata=data.frame(x=ttime[ff]))
#     }
    band <- na.approx(band, na.rm = FALSE)
    band[1:(non_empty[1]-1)]<-band[non_empty[1]]
    smooth_data[[3]][work]<-smooth_data[[2]][work]-band
    smooth_data[[4]][work]<-smooth_data[[2]][work]+band
    
    ###set CI limits in names
    colnames(smooth_data)[3:4]<-c(paste0("LCL_",CI_pct),paste0("UCL_",CI_pct))
    
    
    ##########################################################################
    
    
    
    Conditional_effects<-data.frame("Intercept"=attr(predict(fit_1,type="terms",newdata=GAM_DATA),"constant"),predict(fit_1,type="terms",newdata=GAM_DATA),"Autoregressive_error"=mean_delta)
    Conditional_effects<-data.frame(Conditional_effects,"Sum of Effects"=apply(Conditional_effects,1,sum))
    Conditional_effects<-data.frame("For the Week Starting"=time_ahead,Conditional_effects)
    Conditional_effects
    #write.csv(Conditional_effects,file=paste(TARGET_NAME,"GAM Effects.csv"))
    
    #########################################################################
    ###used to be mod_source.r
    #########################################################################
    
    
    
    #save(backcast_ahead,run,lead_lag_store,ZZ,dateZZ,TARGET_NAME,consider,interaction,gamma,response,file="Bundle_3.RData")
    tst<-structure(list("pt_data"=pt_data,"response"=response,"names"=names,"fixed"=fixed,"pull_future"=pull_future,
                        "fit_1"=fit_1,"ymin"=ymin,"ymax"=ymax,"x"=x,"run"=run,"date"=date,"pull_time_ahead"=pull_time_ahead,
                        "tmp_dat"=tmp_dat,"predicted_vals"=predicted_vals,"ZZ"=ZZ,
                        "dateZZ"=dateZZ,"time_ahead"=time_ahead,"GAM_predictions"=GAM_predictions,
                        "backcast_ahead"=backcast_ahead,"datexx"=datexx,"Conditional_effects"=Conditional_effects,
                        "lead_lag_store"=lead_lag_store,"TARGET_NAME"=TARGET_NAME,"consider"=consider,"interaction"=interaction,
                        "gamma"=gamma,"smooth_data"=smooth_data))
    
  })  
  
  vol_integrator<-reactive({
    TARGET_NAME=LANE_BUNDLE()[['TARGET_NAME']]
    pt_data=mod1()[['pt_data']]
    response=mod1()[['response']]
    names=mod1()[['names']]
    fixed=mod1()[['fixed']]
    pull_future=mod1()[['pull_future']]
    fit_1=mod1()[['fit_1']]
    ymin=mod1()[['ymin']]
    ymax=mod1()[['ymax']]
    x=mod1()[['x']]
    run=mod1()[['run']]
    date=mod1()[['date']]
    pull_time_ahead=mod1()[['pull_time_ahead']]
    tmp_dat=mod1()[['tmp_dat']]
    ZZ=mod1()[['ZZ']]
    dateZZ=mod1()[['dateZZ']]
    time_ahead=mod1()[['time_ahead']]
    datexx=mod1()[['datexx']]
    backcast_ahead=mod1()[['backcast_ahead']]
    FUTURE=mod1()[['FUTURE']]
    XX=mod1()[['XX']]
    error.stream=mod1()[['error.stream']]
    lead_lag_store=mod1()[['lead_lag_store']]
    consider=mod1()[['consider']]
    interaction=mod1()[['interaction']]
    gamma=mod1()[['gamma']]
  
    
    ####identify volume lane
    vol_idx<-which(colnames(x) %in% input$volume)
    
    ####predict future lane volume
    method_pred="Loess" ###set prediciton method between gam and loess
    if (method_pred=="Loess"){
      LM<-linear_detrend(XX[vol_idx],datexx)
      X_linear<-detrend_linear(XX[vol_idx],LM)
      LOESS<-loess_detrend(X_linear,datexx,folds=5,span=1:10/10)
      X_loess<-detrend_loess(X_linear,LOESS)
      Z<-X_loess
      FUTURE<-mean_future(time_ahead,Z,LM,LOESS)} else{
        FUTURE<-mean_future_GAM(time_ahead,datexx,XX[vol_idx],gamma)  
      }
    
    plot_dat<-rbind(XX[vol_idx],FUTURE)
    plot_time<-c(datexx,time_ahead)
    plot_group<-rep("Predicted",length(plot_time))
    plot_group[1:length(datexx)]="Observed"
    plot_group<-rep(plot_group,length(ls(FUTURE)))
    plot_time<-rep(plot_time,length(ls(FUTURE)))
    tmp_dat<-stack(plot_dat)
    tmp_dat<-cbind(tmp_dat,plot_time,plot_group)
    
    
    for (b in 1:ncol(FUTURE)){
      maxx<-max(x[vol_idx])
      minx<-min(x[vol_idx])
      FUTURE[which(FUTURE[,b]>=maxx),b]=maxx
      FUTURE[which(FUTURE[,b]<=minx),b]=minx
    }
    pull_future<-FUTURE
    pull_time_ahead<-time_ahead
    
    
    

    #####daily smoothing of lane volume####
    smooth_data<-data.frame(date=c(dateZZ,pull_time_ahead),values=c(ZZ[[vol_idx]],FUTURE[[1]]),
                            group=c(rep("observed",length(dateZZ)),rep("predicted",length(FUTURE[[1]]))))
    min<-as.POSIXlt(min(smooth_data[[1]]),origin="1970-01-01")
    max<-as.POSIXlt(max(smooth_data[[1]]),origin="1970-01-01")+3.5*24*60*60
    series<-min+(0:difftime(max,min,units="days"))*24*60*60
    series<-as.Date(series,format="%Y-%m-%d")
    obs_dates<-as.Date(as.POSIXlt(smooth_data[[1]],origin="1970-01-01")+ c(diff(smooth_data[[1]])/2,3.5)*24*60*60,format="%Y-%m-%d")
    smooth_values<-numeric(length(series))
    smooth_values[1:length(smooth_values)]<-NA
    smooth_values[series %in% obs_dates]<-smooth_data[[2]]
    smooth_group<-factor(rep(NA,length(series)),levels=levels(smooth_data[[3]]))
    smooth_group[series %in% obs_dates]<-smooth_data[[3]]
    smooth_data<-data.frame(date=series,values=smooth_values,group=smooth_group)
    transition<-floor(mean(c(max(which(smooth_data[[3]]=="observed")),min(which(smooth_data[[3]]=="predicted")))))
    smooth_data[[3]][1:transition]<-"observed"
    smooth_data[[3]][(transition+1):nrow(smooth_data)]<-"predicted"
#     input_dat<-list(date=smooth_data[,1],data=smooth_data[,2,drop=F])
#     ttmmpp<-PIECE_fill(input_dat,t_index=1)
    smooth_data[2]<-na.approx(smooth_data[2], na.rm = FALSE)
    
    return(smooth_data)
    
  })  



  # prep data once and then pass around the program bundle 5 drop in
  
  mod2 <- reactive({
    
    backcast_ahead=mod()[['backcast_ahead']]
    run=mod()[['run']]
    lead_lag_store=mod()[['lead_lag_store']]
    ZZ=mod()[['ZZ']]
    dateZZ=mod()[['dateZZ']]
    TARGET_NAME=mod()[["TARGET_NAME"]]
    consider=mod()[["consider"]]
    interaction=mod()[["interaction"]]
    gamma=mod()[["gamma"]]
    response=mod()[["response"]]
    SEASON=input$seasonality##adjust for seasonality 365 day period (TRUE,FALSE)
    LINEAR=input$linear##adjust for inflation (TRUE,FALSE)
    interaction_split_day=input$interaction_split##cutpoint for interaction
    
    backcast_length=input$backcast_length-1#length of backcast interval
    
    ##############################################
    ######used to be mod_bcst_source.r
    #############################################
    
    
    
    print(run)#check model vars
    print(lead_lag_store)#lead lag for model vars
    output_backcast<-data.frame()#initialize the output vector
    
    
    
    m=backcast_length
    for (m in backcast_length:0){
      x<-ZZ[1:(nrow(ZZ)-m-backcast_ahead),]
      date<-dateZZ[1:(nrow(ZZ)-m-backcast_ahead)]
      XX<-x
      datexx<-date
      
      
      ####Select Lead/Lags For X
      x<-Lead_lag(x,run,lead_lag_store)
      keep=!is.na(apply(x[run],1,mean))
      x<-x[keep,]
      date<-date[keep]
      ##################################
      #####GAM Model
      #####For Multivariate Effect Adjustment
      #################################
      ######Begin Full Gam Fitting#####
      mean_vector<-numeric(length(run))
      error.stream<-data.frame(matrix(nrow=nrow(x),ncol=length(run)))
      days=as.numeric(format(date,"%j"))
      season<-rep("first_half",length(days))
      season[which(days>=interaction_split_day)]="second_half"
      GAM_DATA<-cbind(days,"date"=as.numeric(date),season,x[run])
      u=1
      for (u in 1:length(run)){
        
        preds<-c()
        for (z in run[-u]){
          if (interaction[which(consider %in% z)]==TRUE){
            preds<-c(preds,paste("s(",colnames(x[z]),",by=season)",sep=""))} else
            {preds<-c(preds,paste("s(",colnames(x[z]),")",sep=""))}
        }
        #preds<-c("s(date,sp=1000)","s(days,bs=\"cc\")",preds)
        if(SEASON==TRUE){preds<-c("s(days,bs=\"cc\")",preds)}
        if(LINEAR==TRUE){preds<-c("s(date,sp=1000)",preds)}
        preds<-paste(preds,collapse="+")
        tmpcmd<-paste("fit=gam(",colnames(x[run[u]]),"~",preds,",data=GAM_DATA,gamma=",gamma,")",sep="")
        eval(parse(text=tmpcmd))
        summary(fit)
        error.stream[u]<-residuals(fit)
        colnames(error.stream)[u]<-colnames(x[run[u]])
        if (u==1) fit_1=fit
      }
      
      
      
      #####Asssess Multivariate GAM fits######
      #####Perform Mean function Predictions####
      #par(op)
      ymin<-min(min(predict(fit_1)),min(x[[response]]))
      ymax<-max(max(predict(fit_1)),max(x[[response]]))
      date_b<-datexx
      
      
      #####
      year_ahead<-as.numeric(format(date_b[length(date_b)],"%Y"))
      reference_ahead<-as.Date(strptime(paste(year_ahead,"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
      week_ahead<-as.double(floor(difftime(date_b[length(date_b)],reference_ahead,units="weeks")))
      ###make list of future weeks to pick from since dates suck to work with, make it 2X as long as needed then extract elements
      min<-as.POSIXlt(date_b[length(date_b)],origin="1970-01-01")
      end<-paste(year_ahead+ceiling(backcast_ahead/52)*2,format(date_b[length(date_b)],format="%m-%d"),sep="-")
      end<-as.Date(end,format="%Y-%m-%d")
      max<-as.POSIXlt(end,origin="1970-01-01")
      series<-min+(0:difftime(max,min,units="days"))*24*60*60
      series<-as.Date(series,format="%Y-%m-%d")
      year<-as.numeric(format(series,format="%Y"))
      reference<-as.Date(strptime(paste(year,"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
      week<-as.double(floor(difftime(series,reference,units="weeks")))
      week[which(week==52)]=51 ##cut off last couple of days in year and group with last week
      year_week<-paste(year,week,sep="-")
      year_week<-unique(year_week)
      concat<-strsplit(year_week,"-")
      year<-numeric(length(year_week))
      week<-numeric(length(year_week))
      reference<-numeric(length(year_week))
      class(reference)<-"Date"
      for (s in 1:length(year_week)){
        year[s]=as.numeric(concat[[s]][1])
        week[s]=as.numeric(concat[[s]][2])
        reference[s]=as.Date(strptime(paste(year[s],"01","01",sep="-"),"%Y-%m-%d"),format="%Y-%m-%d")
      }
      week_ahead<-week[2:(backcast_ahead+1)]
      year_ahead<-year[2:(backcast_ahead+1)]
      reference_ahead<-reference[2:(backcast_ahead+1)]
      time_ahead<-as.Date(as.POSIXct(reference_ahead,origin="1970-01-01")+week_ahead*7*24*60*60,format="%Y-%m-%d")
      
      y=colnames(XX[run[-1]])
      FUTURE<-data.frame(matrix(nrow=length(time_ahead),ncol=length(y)))
      colnames(FUTURE)<-y
      plot_dat<-rbind(XX[run[-1]],FUTURE)
      plot_time<-c(datexx,time_ahead)
      plot_group<-rep("Predicted",length(plot_time))
      plot_group[1:length(datexx)]="Observed"
      plot_group<-rep(plot_group,length(ls(FUTURE)))
      plot_time<-rep(plot_time,length(ls(FUTURE)))
      tmp_dat<-stack(plot_dat)
      tmp_dat<-cbind(tmp_dat,plot_time,plot_group)
      
      
      
      
      ###Construct True Future during backcasting routine
      zz<-Lead_lag(ZZ,run,lead_lag_store)
      keep=!is.na(apply(zz[run],1,mean))
      zz<-zz[keep,]
      tmp<-data.frame(zz[(nrow(XX)+1):(nrow(XX)+backcast_ahead),run[-1]])
      not_null<-!is.na(apply(tmp,1,mean))
      FUTURE[which(not_null),1:ncol(FUTURE)]<-tmp[which(not_null),]
      
      
      plot_dat<-rbind(XX[run[-1]],FUTURE)
      plot_time<-c(datexx,time_ahead)
      plot_group<-rep("Predicted",length(plot_time))
      plot_group[1:length(datexx)]="Observed"
      plot_group<-rep(plot_group,length(ls(FUTURE)))
      plot_time<-rep(plot_time,length(ls(FUTURE)))
      tmp_dat<-stack(plot_dat)
      tmp_dat<-cbind(tmp_dat,plot_time,plot_group)
      
      
      ###Look at GAM error structure###
      fit<-VAR(error.stream,ic=c("SC"),lag.max=10,type="const")
      fit<-restrict(fit,method="ser",thresh=0.02)
      predicted_vals=predict(fit,n.ahead=backcast_ahead)
      
      
      mean_delta=predicted_vals$fcst[[1]][,1]
      lcl_delta=predicted_vals$fcst[[1]][,4]
      ucl_delta=predicted_vals$fcst[[1]][,4]
      
      
      plot_dat<-rbind(XX[run[-1]],FUTURE)
      plot_time<-c(datexx,time_ahead)
      FUTURE<-Lead_lag(plot_dat,1:length(plot_dat),lead_lag_store[-1])
      keep=!is.na(apply(FUTURE,1,mean))
      tmp=colnames(FUTURE)
      FUTURE<-data.frame(FUTURE[keep,])
      FUTURE<-data.frame(FUTURE[(nrow(FUTURE)-backcast_ahead+1):nrow(FUTURE),])
      colnames(FUTURE)<-tmp
      time_ahead<-plot_time[keep]
      time_ahead<-time_ahead[(length(time_ahead)-backcast_ahead+1):length(time_ahead)]
      
      
      
      
      days=as.numeric(format(time_ahead,"%j"))
      season<-rep("first_half",length(days))
      season[which(days>=interaction_split_day)]="second_half"
      GAM_DATA<-cbind(days,"date"=as.numeric(time_ahead),season,FUTURE)
      predicted<-predict(fit_1,newdata=GAM_DATA,se.fit=T,pred.var=0)
      predicted_adj<-as.numeric(predicted$fit)+mean_delta
      lcl=predicted_adj-1.96*sqrt((lcl_delta/1.96)^2+(as.numeric(predicted$se.fit))^2)
      ucl=predicted_adj+1.96*sqrt((lcl_delta/1.96)^2+(as.numeric(predicted$se.fit))^2)
      char<-paste("GAM_Predictions_",ls(x[response]),sep="")
      tmpcmd=paste("GAM_predictions=data.frame(time_ahead,",char,"=predicted_adj,lcl,ucl,FUTURE)",sep="")
      eval(parse(text=tmpcmd))
      output_backcast<-rbind(output_backcast,GAM_predictions[nrow(GAM_predictions),])
    }
    obs<-ZZ[which(dateZZ %in% output_backcast[[1]]),response]
    pred<-output_backcast[2]
    percent_error<-abs((obs-pred)/obs)
    
    R<-cor(obs,pred)^2
    percent_error<-mean(percent_error[[1]])
    
    
    
    ################Linear interpolation of daily values ########################################
    
    smooth_data<-output_backcast
    min<-as.POSIXlt(min(smooth_data[[1]]),origin="1970-01-01")
    max<-as.POSIXlt(max(smooth_data[[1]]),origin="1970-01-01")+3.5*24*60*60
    series<-min+(0:difftime(max,min,units="days"))*24*60*60
    series<-as.Date(series,format="%Y-%m-%d")
    obs_dates<-as.Date(as.POSIXlt(smooth_data[[1]],origin="1970-01-01")+ c(diff(smooth_data[[1]])/2,3.5)*24*60*60,format="%Y-%m-%d")
    smooth_values<-numeric(length(series))
    smooth_values[1:length(smooth_values)]<-NA
    smooth_values[series %in% obs_dates]<-smooth_data[[2]]
    smooth_data <- data.frame(date=series,matrix(nrow=length(series),ncol=ncol(output_backcast)-1))
    colnames(smooth_data) <- colnames(output_backcast)
    smooth_data[series %in%obs_dates,2:ncol(smooth_data)] <- output_backcast[,2:ncol(smooth_data)]
    smooth_data[,2:ncol(smooth_data)] <- na.approx(smooth_data[,2:ncol(smooth_data)], na.rm = FALSE)
    backcast_daily <- smooth_data
    
    #write.csv(output_backcast,file=paste(TARGET_NAME,backcast_ahead," Weeks_Ahead_Backcasting.csv")) 
    ########end backcasting source
    
    
    ##############################################
    ######used to be mod_bcst_source.r#####
    #############################################
    
    
    
    tst<-structure(list("output_backcast"=output_backcast,
                        "percent_error"=percent_error,"R"=R,"backcast_daily"=backcast_daily))
    
  })
  


  ########################################################
  ################ New outputs from bundle 1####
  ############################################################

output$raw_api <- renderUI({
  api_readtable <- API_Update()
  api_readtable<-data.frame(rbind(toupper(colnames(api_readtable)),as.matrix(api_readtable)))
  if (!is.null(Read_Settings()[["api_names"]])){
  api_readtable<-data.frame(Read_Settings()[["api_names"]])
  }
    
  matrixCustom('api_names', 'API Indicators to Read',api_readtable)
  ###you can access these values with input$api_names as the variable anywhere in the server side file
  
})


  output$raw_data <- renderDataTable({
    Data()
    
  })
  
  
  output$raw_indicators <- renderDataTable({
    DATA_I<-API()[["DATA_I"]]
    dest=names(DATA_I)
    cc=dest
    for (i in 2:length(dest)){
      cc[i]=colnames(DATA_I[[dest[i]]])
    }
    keep<-cc %in% input$API_choice
    data.frame(Abbreviations=dest[keep],Full_Names=cc[keep])
    
  })
  
  output$raw_indicators_2 <- renderDataTable({
    DATA_I<-API()[["DATA_I"]]
    dest=names(DATA_I)
    cc=dest
    for (i in 2:length(dest)){
      cc[i]=colnames(DATA_I[[dest[i]]])
    }
    keep<-cc %in% input$API_choice
    data.frame(Abbreviations=dest[keep],Full_Names=cc[keep])
    
  })
  
  output$raw_indicators_3 <- renderDataTable({
    DATA_I<-API()[["DATA_I"]]
    dest=names(DATA_I)
    cc=dest
    for (i in 2:length(dest)){
      cc[i]=colnames(DATA_I[[dest[i]]])
    }
    keep<-cc %in% input$API_choice
    data.frame(Abbreviations=dest[keep],Full_Names=cc[keep])
    
  })
  
  output$selected_data <- renderDataTable({
    CHR()##if you don't use clean dates (above), this will crash...
    
  })
  
  output$outlier_rpm<-renderDataTable({
    FIL()
    
  })
  
  output$outlier_rpm_plot<-renderPlot({
    if(!is.null(FIL())){
      dat=FIL()
      sp<-ggplot(dat,aes(x=Date,y=RPM))+geom_point(alpha=0.1,size=1)+
        ggtitle("RPM by Date after Filtering")
      print(sp)} else{
        return(NULL)}
    
  })
  
  output$l1_raw_plot<-renderPlot({
    if(!is.null(L1())){
      dat=L1()
      sp<-ggplot(dat,aes(x=Date,y=RPM))+geom_point(alpha=0.1,size=1)+
        ggtitle("Raw Data Selected for Lane")
      print(sp)} else{
        return(NULL)}
    
  })
  
  output$l2_raw_plot<-renderPlot({
    if(!is.null(L2())){
      dat=L2()
      sp<-ggplot(dat,aes(x=Date,y=RPM))+geom_point(alpha=0.1,size=1)+
        ggtitle("Raw Data Selected for Lane")
      print(sp)} else{
        return(NULL)}
    
  })
  
  output$l3_raw_plot<-renderPlot({
    if(!is.null(L3())){
      dat=L3()
      sp<-ggplot(dat,aes(x=Date,y=RPM))+geom_point(alpha=0.1,size=1)+
        ggtitle("Raw Data Selected for Lane")
      print(sp)} else{
        return(NULL)}
    
  })
  
  output$lanes<-renderDataTable({
    FINAL()
    
  })
  
  output$weekly_averages<-renderDataTable({
    data.frame(LANE_BUNDLE()[['DATA_FILL']])
    
  })
  
  ###############################################
  ############### Current working outputs for bundles 3 - 5#####
  #########################################################
  
  output$Var_Import <- renderChart({
    pt_data<-mod()[["pt_data"]]
    names<-mod()[["names"]]
    response<-mod()[["response"]]
    fixed<-mod()[["fixed"]]
    #     theGraph<-barchart(pt_data,main=paste("Importance of Added Variable",sep=""),
    #                        sub=paste("Current Model: ",paste(names[c(response)]),"=",paste(names[c(fixed[-length(fixed)])],collapse=" + "),sep=""),
    #                        xlab="AIC of Model (shown below) with added Variable")
    #     print(theGraph)
    
    datatrans <- data.frame(pt_data[length(pt_data):1])
    datatrans <- data.frame(names(pt_data), datatrans, "top 25 selected_variables")
    colnames(datatrans) <- c("names","values", "selected_variables")
    levels(datatrans[[1]])<-datatrans[[1]]
    levels(datatrans[[1]])<-levels(datatrans[[1]])[nrow(datatrans):1]
    datatrans[[2]]<-datatrans[[2]][as.numeric(datatrans[[1]])]

    
  
    
    
    
    theGraph <- hPlot(values ~ names, data = datatrans, type = 'bar', group = 'selected_variables', group.na = 'NA\'s', title = "Importance of Added Variable")
    theGraph$xAxis(categories = c(levels(datatrans[,1])))
    theGraph$yAxis(title = (list(text = paste("AIC of Model (shown below) with added Variable <br> Current Model:",paste(names[c(response)]), "=",paste(names[c(fixed[-length(fixed)])],collapse=" + "), sep =""))))
    theGraph$addParams(dom = 'Var_Import')
    
    return(theGraph)
  })
  
  output$cond_effect <- renderPlot({
    fit_1<-mod()[["fit_1"]]
    x<-mod()[["x"]]
    run<-mod()[["run"]]
    theGraph<-plot(fit_1,pages=1,residuals=TRUE,shade=T)
    title(paste("Conditional Effects Plot:",colnames(x[run[1]])))
    print(theGraph)
    
  })
  
  output$diagnostic <- renderPlot({
    fit_1<-mod()[["fit_1"]]
    theGraph<-gam.check(fit_1)
    print(theGraph)
    
  })
  
  output$fit <- renderChart({
    fit_1<-mod()[["fit_1"]]
    date<-mod()[["date"]]
    ymin<-mod()[["ymin"]]
    ymax<-mod()[["ymax"]]
    x<-mod()[["x"]]
    response<-mod()[["response"]]
#     theGraph<-plot(predict(fit_1)~date,pch=16,ylim=c(ymin,ymax),ylab=colnames(x)[response])
#     lines(x[[response]]~date)
#     lines(predict(fit_1)~date,lty=2)
#     title("fitted versus observed")
#     legend(x="topleft",legend=c("fitted","observed"),
#            lty=c(2,1),lwd=c(0,1),pch=c(16,NA))
#     print(theGraph)

datatrans <- data.frame(date, predict(fit_1),x[[response]])
colnames(datatrans) <- c("date","fitted","observed")
datatrans <- reshape2::melt(datatrans,id= 'date', na.rm = TRUE)
datatrans[,1] <- as.numeric(as.POSIXct((as.numeric(datatrans[,1])*1000*24*60*60), origin = "1970-01-01"))     

theGraph <- hPlot(value ~ date, group = 'variable', data = datatrans, type = 'line', title = "Fitted vs. Observed")
theGraph$yAxis(title = (list(text = colnames(x)[response])))
theGraph$chart(zoomType = "x")
theGraph$addParams(dom = 'fit')
theGraph$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'), title = list(text = "Date"))

return(theGraph)

    
  })
  
  output$pred_fwd <- renderPlot({
    tmp_dat<-mod()[["tmp_dat"]]
    theGraph<-xyplot(values~plot_time|ind,group=plot_group,data=tmp_dat,scales=list(relation="free"),ylab=NULL,type=c("l","l"),
                     lwd=5,pch=19,cex=0.01,col=c("gray","blue"),main="Observed and Predicted Values for Co-Variates",xlab="Date",
                     distribute.type=TRUE)
    print(theGraph)

# tmp_dat[["plot_time"]] <- as.numeric(as.POSIXct((as.numeric(tmp_dat[["plot_time"]])*1000*24*60*60), origin = "1970-01-01"))
# theGraph <- hPlot(values ~ plot_time, group = 'plot_group', data = tmp_dat, type = 'line', title = "Observed and Predicted Values for Co-Variates")
# theGraph$yAxis(title = (list(text = NULL)))
# theGraph$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'), title = list(text = "Date"))
# theGraph$chart(zoomType = "x")
# theGraph$addParams(dom = 'pred_fwd')
# 
# return(theGraph)
    print(theGraph)
  })
  
  output$ts_error <- renderPlot({
    predicted_vals<-mod()[["predicted_vals"]]
    theGraph<-fanchart(predicted_vals)
    print(theGraph)
    
  })
  
output$preds<- renderChart({
  predicted_vals<-mod()[["predicted_vals"]]
  smooth_vals<-mod()[["smooth_data"]]
  ZZ<-mod()[["ZZ"]]
  response<-mod()[["response"]]
  dateZZ<-mod()[["dateZZ"]]
  time_ahead<-mod()[["time_ahead"]]
  GAM_predictions<-mod()[["GAM_predictions"]]
  backcast_ahead<-mod()[["backcast_ahead"]]
  datexx<-mod()[["datexx"]]
  datatrans <- matrix(NA,nrow=nrow(smooth_vals),ncol = 4)
  idx<-smooth_vals[[5]]=="observed"
  
  datatrans[idx,1] <- smooth_vals[[2]][idx]
  datatrans[!idx,2] <- smooth_vals[[2]][!idx]
  datatrans[!idx,3] <- smooth_vals[[3]][!idx]
  datatrans[!idx,4] <- smooth_vals[[4]][!idx]
  datevect <- smooth_vals[[1]]
  datatrans <- data.frame(datevect,datatrans)
  
  
  CI_labs<-colnames(smooth_vals)[3:4]
  colnames(datatrans) <- c("date","observed","predicted",CI_labs)
  datatrans <- reshape2::melt(datatrans,id= 'date', na.rm = TRUE)
  datatrans[,1] <- as.numeric(as.POSIXct((as.numeric(datatrans[,1])*1000*24*60*60), origin = "1970-01-01"))
  theGraph <- hPlot(value ~ date, group = 'variable', data = datatrans, type = 'line')
  theGraph$chart(zoomType = "x")
  theGraph$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'), title = list(text = "Date"))
  theGraph$addParams(dom = 'preds')
  theGraph$yAxis(title = list(text = names(predicted_vals$fcst[1])))
  
  return(theGraph)
  
}) 

######################################################################################
########################### FUCK           ###################################
######################################################################################
output$quote_final<-renderText({
  smooth_vals<-mod()[["smooth_data"]]
  vol_vals<-vol_integrator()
  datevect <- smooth_vals[[1]]
  idx<-datevect>=input$quote_date[1] & datevect<=input$quote_date[2]
  idxx<-smooth_vals[[5]]=="observed"
  
  
  datatrans <- matrix(NA,nrow=nrow(smooth_vals),ncol = 2)
  datatrans[idxx,1] <- smooth_vals[[2]][idxx]
  #datatrans[idxx,2] <- vol_vals[[2]][idxx]
  datatrans[!idxx,2] <- as.numeric(input$matrix_volume[,2])
  #datatrans[!idxx,4] <- vol_vals[[2]][!idxx]
  datevect <- smooth_vals[[1]]
  datatrans <- data.frame(datevect,datatrans)
  rpm <- smooth_vals[[2]][idx]
  volume <- vol_vals[[2]][idx]
  quote<-round(sum(rpm*volume,na.rm=T)/sum(volume,na.rm=T),2)
  
  paste("Volume Integrated Quote for: ",input$quote_date[1]," - ",input$quote_date[2]," is $",quote,sep="")
  
  
  
})



######################################################################################
########################### FUCK           ###################################
######################################################################################
output$quote_value<- renderChart({
  smooth_vals<-mod()[["smooth_data"]]
  vol_vals<-vol_integrator()
  datevect <- smooth_vals[[1]]
  idx<-datevect>=input$quote_date[1] & datevect<=input$quote_date[2]
  idxx<-smooth_vals[[5]]=="observed"
  
  
  datatrans <- matrix(NA,nrow=nrow(smooth_vals),ncol = 2)
  datatrans[idxx,1] <- smooth_vals[[2]][idxx]
  #datatrans[idxx,2] <- vol_vals[[2]][idxx]
  datatrans[!idxx,2] <- as.numeric(input$matrix_volume[,2])
  #datatrans[!idxx,4] <- vol_vals[[2]][!idxx]
  datevect <- smooth_vals[[1]]
  datatrans <- data.frame(datevect,datatrans)
  rpm <- smooth_vals[[2]][idx]
  volume <- vol_vals[[2]][idx]
  quote<-round(mean(rpm,na.rm=T),2)
  colnames(datatrans) <- c("date","Rate_observed","Rate_predicted")
  datatrans <- reshape2::melt(datatrans,id= 'date', na.rm = TRUE)
  datatrans[,1] <- as.numeric(as.POSIXct((as.numeric(datatrans[,1])*1000*24*60*60), origin = "1970-01-01"))
  theGraph <- hPlot(value ~ date, group = 'variable', data = datatrans, type = 'line', title = paste("Average Rate Per Mile:$",quote,sep=""))
  theGraph$chart(zoomType = "x")
  min_band<-as.numeric(as.POSIXct((as.numeric(input$quote_date[1])*1000*24*60*60), origin = "1970-01-01"))
  max_band<-as.numeric(as.POSIXct((as.numeric(input$quote_date[2])*1000*24*60*60), origin = "1970-01-01"))
  theGraph$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'), title = list(text = "Date"),
                 plotBands = list(color='rgba(68, 170, 213, 0.2)',from=min_band,to=max_band))
  theGraph$yAxis(title = list(text = "Rate Per Mile"),labels = list(format = '${value}'))
  theGraph$addParams(dom = 'quote_value')
  return(theGraph)
  
})   





######################################################################################
########################### FUCK           ###################################
######################################################################################
output$quote_volume<- renderChart({
  smooth_vals<-mod()[["smooth_data"]]
  vol_vals<-vol_integrator()
  datevect <- smooth_vals[[1]]
  idx<-datevect>=input$quote_date[1] & datevect<=input$quote_date[2]
  idxx<-smooth_vals[[5]]=="observed"
  
  
  datatrans <- matrix(NA,nrow=nrow(smooth_vals),ncol = 2)
  #datatrans[idxx,1] <- smooth_vals[[2]][idxx]
  datatrans[idxx,1] <- vol_vals[[2]][idxx]
  #datatrans[!idxx,3] <- smooth_vals[[2]][!idxx]
  datatrans[!idxx,2]<-as.numeric(input$matrix_volume[,3])
  #datatrans[!idxx,2] <- vol_vals[[2]][!idxx]
  datevect <- smooth_vals[[1]]
  datatrans <- data.frame(datevect,datatrans)
  rpm <- smooth_vals[[2]][idx]
  volume <- vol_vals[[2]][idx]
  quote<-round(mean(volume,na.rm=T),2)
  colnames(datatrans) <- c("date","Volume_observed","Volume_predicted")
  datatrans <- reshape2::melt(datatrans,id= 'date', na.rm = TRUE)
  datatrans[,1] <- as.numeric(as.POSIXct((as.numeric(datatrans[,1])*1000*24*60*60), origin = "1970-01-01"))
  theGraph <- hPlot(value ~ date, group = 'variable', data = datatrans, type = 'line', title = paste("Average Volume:",quote,sep=""))
  theGraph$chart(zoomType = "x")
  min_band<-as.numeric(as.POSIXct((as.numeric(input$quote_date[1])*1000*24*60*60), origin = "1970-01-01"))
  max_band<-as.numeric(as.POSIXct((as.numeric(input$quote_date[2])*1000*24*60*60), origin = "1970-01-01"))
  theGraph$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'), title = list(text = "Date"),
                 plotBands = list(color='rgba(68, 170, 213, 0.2)',from=min_band,to=max_band))
  theGraph$yAxis(title = list(text = "Transactional Volume"))
  theGraph$addParams(dom = 'quote_volume')
  return(theGraph)
  
}) 




  output$GAM_effects<-renderDataTable({
    tmp<-mod()[["Conditional_effects"]]
    #tmp$time_ahead<-format(as.POSIXct(tmp$time_ahead,origin="1970-01-01"),format="%m/%d/%Y")

  })
  
  output$effects <- downloadHandler(
    filename = c('effects.csv'),
    content = function(file) {
      write.csv(mod()[["Conditional_effects"]], file)
    })
  
  
  output$GAM_predictions<-renderDataTable({
    tmp<-mod()[["smooth_data"]]
    idx<-tmp[[5]]=="predicted"
    #tmp$time_ahead<-format(as.POSIXct(tmp$time_ahead,origin="1970-01-01"),format="%m/%d/%Y")
    tmp[idx,]
    
  })


output$vol_quote<-renderDataTable({
  volume<-vol_integrator()
  rate<-mod()[["smooth_data"]]
  data.frame(rate,volume)
})
  


  output$predictions_GAM <- downloadHandler(
    filename = c('predictions.csv'),
    content = function(file) {
      write.csv(mod()[["smooth_data"]], file)
    })
  
  
  
  
  output$Raw <- renderPlot({
    item=item()##identify reactive data
    PLOT=LANE_BUNDLE()[['PLOT']]
    
    #Original lattice based graphing    
        theGraph<-xyplot(values~as.POSIXlt(time,origin="1970-01-01")|ind,data=PLOT[which(PLOT$group %in% item),],scales=list(relation="free"),ylab=NULL,
                         lwd=1,pch=19,cex=0.01,xlab="Time",main=paste(item,sep=""))
    
    theGraph <- ggplot(PLOT[which(PLOT$group %in% item),], aes(time, values)) + geom_point(size = 1) + facet_wrap(~ind, scales = "free") + xlab("Time") + ylab(NULL)
    print(theGraph)
    
#     p1 <- rPlot(values~time|ind, data = PLOT[which(PLOT$group %in% item),], type = "point")

  })
  
  
  output$Impute <- renderPlot({
    item=item()##identify reactive data
    PLOT_FILL=LANE_BUNDLE()[['PLOT_FILL']]
    
    #Original Lattice-based graphing
    #    theGraph<-xyplot(values~as.POSIXlt(time,origin="1970-01-01")|ind,group=miss_index,data=PLOT_FILL[which(PLOT_FILL$group %in% item),],scales=list(relation="free"),ylab=NULL,
    #                     pch=c(19,19),col=c("black","red"),cex=c(0.05,0.5),xlab="Time",main=paste(item,sep=""),
    #                     key=list(text=list(c("imputed","observed")),columns=2,points=list(pch=c(19,19),col=c("black","red"))))
    
    theGraph <- ggplot(PLOT_FILL[which(PLOT_FILL$group %in% item),], aes(time, values, color = miss_index)) + geom_point(size= 1) + facet_wrap(~ind, scales = "free") + xlab("Time") + ylab(NULL)
    
    print(theGraph)
  })
  
  output$Backcast_graph <- renderChart({
    backcast_daily<-mod2()[["backcast_daily"]]
    percent_error<-mod2()[["percent_error"]]
    R<-mod2()[["R"]]
    smooth_vals<-mod()[["smooth_data"]]
    
    ZZ=mod()[['ZZ']]
    dateZZ=mod()[['dateZZ']]
    response=mod()[["response"]]
    TARGET_NAME=mod()[["TARGET_NAME"]]
    backcast_ahead=mod()[['backcast_ahead']]    
    
#     theGraph<-plot(ZZ[[response]]~dateZZ,type="l",xlim=c(min(c(dateZZ)),max(output_backcast[[1]])),
#                    ylim=c(min(ZZ[[response]],output_backcast[[2]],output_backcast[[3]],output_backcast[[4]]),
#                           max(ZZ[[response]],output_backcast[[2]],output_backcast[[3]],output_backcast[[4]])),
#                    ylab=colnames(ZZ)[response],main=paste(TARGET_NAME,":  Backasting Predictions",backcast_ahead,"Weeks Ahead"))
#     points(output_backcast[[2]]~output_backcast[[1]],pch=16,cex=1)
#     lines(output_backcast[[2]]~output_backcast[[1]],lwd=2,lty=1)
#     legend(x="topleft",legend=c("observed","predicted"),
#            lty=c(1,1),lwd=c(1,2),pch=c(NA,16))
#     title(sub=paste("Average Prediction Error=",round(percent_error,digits=4)*100,"%; R-Squared=",round(R,2),sep=""))
#     print(theGraph)
    
    observed <- smooth_vals[which(smooth_vals[,5] %in% "observed"),1:2]
    datatrans <- matrix(NA, nrow=nrow(observed)+nrow(backcast_daily),ncol = 2)
    datatrans[1:nrow(observed),1] <- observed[,2]
    datatrans[(nrow(observed)+1):nrow(datatrans),2] <- backcast_daily[,2]
    datevect <- c(observed[,1],backcast_daily[,1])
    datatrans <- data.frame(datevect,datatrans)
    
    colnames(datatrans) <- c("date","observed","predicted")
    datatrans <- reshape2::melt(datatrans,id= 'date', na.rm = TRUE)
    datatrans[,1] <- as.numeric(as.POSIXct((as.numeric(datatrans[,1])*1000*24*60*60), origin = "1970-01-01"))
    theGraph <- hPlot(value ~ date, group = 'variable', data = datatrans, type = 'line', title = paste(TARGET_NAME,":  Backasting Predictions",backcast_ahead,"Weeks Ahead"))
    theGraph$chart(zoomType = "x")
    theGraph$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'), title = list(text = paste("Date <br>","Average Prediction Error=",round(percent_error,digits=4)*100,"%; R-Squared=",round(R,2),sep="")))
    theGraph$addParams(dom = 'Backcast_graph')
    theGraph$yAxis(title = list(text = colnames(ZZ)[response]))
    
    return(theGraph)
    
    
  })

  
})    
