function(input, output, session) {
  

#########################################################  
    # Side Bar Options
  {
  ###
    
    
    internal_options <- reactive({
      
      if(!is.null(DataSet())) {
        
      lvl_01 <- sort(unique(paste0(DataSet()[[1]]$"ship_type", " (Cod: ", 
                                   DataSet()[[1]]$"SHIPTYPE", ")")))
      
      lvl_02 <- c("Ship Name", "Ship Id", "Ship Name (Id)" )
      
      
      
      
      # Return
      my_exit <- list(lvl_01, lvl_02)
      return(my_exit)
      
      
      } else return(NULL)
    })
    
    
    # Ships Types and others options...
    output$inVar1 <- renderUI({
      
      
      if(!is.null(input$file1$datapath)) {
        
        lvl_ship_type_fusion <- internal_options()[[1]]
        
   
        
        fluidRow(
          selectInput(inputId = 'my_ship_type_fusion', label = 'Ship Type', choices = lvl_ship_type_fusion),
          br(),
          radioButtons(inputId = "orden01", label = "Order Type", 
                       choices = c("Ship Name", "Ship Id", "Ship Name (Id)" ),
                       selected = "Ship Name (Id)"),
        )
        
      } else return(NULL)
    })
    
    # Ships Names and Id
    output$inVar2 <- renderUI({
      
        if(!is.null(input$my_ship_type_fusion)) {

              
        # My type ship
        my_ship_type <- as.character(as.vector(strsplit(input$my_ship_type_fusion, " [(]")[[1]][1]))

        data <- filter(DataSet()[[1]], ship_type == my_ship_type)

        
        
        # Options...
        lvl_ship_name <- unique(data$"SHIPNAME")
        lvl_ship_id <- unique(data$"SHIP_ID")
        lvl_fusion <-  paste0(lvl_ship_name, " (", lvl_ship_id, ")")
        
        if (input$orden01 == "Ship Name")  names(lvl_ship_name) <- lvl_ship_name else
          if (input$orden01 == "Ship Id") names(lvl_ship_name) <- lvl_ship_id  else
            if (input$orden01 == "Ship Name (Id)") names(lvl_ship_name) <- lvl_fusion
       
        lvl_ship_name <- lvl_ship_name[order(names(lvl_ship_name), decreasing = F)]
        
        
        first_example <- NA
        if (input$orden01 == "Ship Name")  first_example <- lvl_ship_name[names(lvl_ship_name) == "THURKUS"] else
          if (input$orden01 == "Ship Id") first_example <- lvl_ship_name[names(lvl_ship_name) == "122562"]  else
            if (input$orden01 == "Ship Name (Id)") first_example <- lvl_ship_name[names(lvl_ship_name) == "THURKUS (122562)"]
        
        selectInput(inputId = "inVar2", label = h4(input$orden01), choices =  lvl_ship_name,
                    selected = first_example)
        
     

      } else return(NULL)
    })  
 

    # Complete SideBar
    output$CompleteSideBar <- renderUI({
      
      div(
      conditionalPanel(
        'input.GameMaster === "Load Data"',
        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        
        # Horizontal line ----
        tags$hr()
        
       
      ),
      conditionalPanel(
        'input.GameMaster != "Load Data"',
        uiOutput("inVar1"),
        br(),
        uiOutput("inVar2"),
        br()
      )
      )
      
      
    })
    
    
   
    
    
    
  ###  
  }
#######################
  

  
########################################################    
    # Data, Selected Data and internal objects...
  {
  ###
    
    # All Record - Full Data Set
    DataSet <- reactive({
     data_set <-  read.csv(input$file1$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote,
               stringsAsFactors = F)
      
     # # # # WARNING!
     # # # # The ship "AMANDA" has two ID: "153800" and "323355"
     # # # # I decided to change the second id from "AMANDA" to "AMANDA V2".
     # # # #   "AMANDA" ----> "153800"
     # # # #   "AMANDA V2" ----> "323355"
     # # # # 
     # # # # This action give the posibility to have one name for each id.
     
     #Return
     ChangeDataSet(data_set = data_set)
     
  #   "FinalData"     "id_problems"   "name_problems"
     
    })
 
    # Fusion: name (id) for selected ship
    fusion_ship_name <- reactive({
    
    if (!is.null(input$inVar2)) {
    lvl_ship_name <- unique(DataSet()[[1]]$"SHIPNAME")
    lvl_ship_id <- unique(DataSet()[[1]]$"SHIP_ID")
    lvl_fusion <-  paste0(lvl_ship_name, " (", lvl_ship_id, ")")
    dt_ship_name <- lvl_ship_name == input$inVar2
    the_fusion <- lvl_fusion[dt_ship_name]
    the_fusion[1]
    
    } else return(NULL)
  })
    
    # My ship information
    selectedData2 <- reactive({
      
      if(!is.null(input$file1$datapath)){
        if(!is.null(input$inVar2)){
          
          ### ShipName
          my_ship_name <- input$inVar2
          
    
          ### First and Last Record
          # Chronologic order
          data <- filter(DataSet()[[1]], SHIPNAME == my_ship_name)
          input_date <- data$"DATETIME"
          mod_date <- parse_date_time(input_date, orders = "ymd HMS")
          date_order <- order(mod_date, decreasing = T)
          data <- data[date_order, ]
          
          # First and Last Records and total time
          first_record <- data$DATETIME[nrow(data)]
          last_record <- data$DATETIME[1]
          
          internal_fr <- parse_date_time(first_record, orders = "ymd HMS")
          internal_lr <- parse_date_time(last_record, orders = "ymd HMS")
          
          total_time_minute <- round(abs(as.numeric(difftime(internal_lr, internal_fr, units = "mins"))), 1)
          total_time_hour <- round(abs(as.numeric(difftime(internal_lr, internal_fr, units = "hours"))), 1)
          
          # Total Sailed Distance
          coords <- data[,c("LON", "LAT")]
          shipe_distance <- DistCalculator(input_coords = coords, input_round = 2)
          total_sailed_distance_meter <- sum(shipe_distance)
          total_sailed_distance_km <- round(total_sailed_distance_meter/1000, 1)
          
          # Table
          new_columnsNames <- c("Fist Record", "Last Record", "Total Sailed Distance Estimated (m)",
                                "Total Sailed Distance Estimated (Km)",
                                "Total Time Estimated (min)", "Total Time Estimated (hour)")
          
          my_table01 <- as.data.frame(matrix(NA, 1, length(new_columnsNames)))
          my_table01[1,] <- c(first_record, last_record, total_sailed_distance_meter,
                              total_sailed_distance_km, total_time_minute, total_time_hour)
          
          colnames(my_table01) <- new_columnsNames
          
          ### Ship Information
          selected_cols <- c("SHIPNAME", "SHIP_ID", "FLAG", "ship_type", "SHIPTYPE", "LENGTH", "WIDTH", "DWT")
          my_table02 <- data[1 ,selected_cols]
          colnames(my_table02) <- c("Ship name", "Ship ID", "Flag", "Ship Type (Category)", 
                                    "Ship Type (Cod)", "Length (meter)", "Width - Beam (meter)", "Weigth (Ton)")
          
          
          # Return
          my_exit <- list(my_ship_name, my_table01, my_table02)
          my_exit
          
        } else return(NULL)
      } else return(NULL)
    })
   
 
    
    # Only the largest distance between two consecutive observations of my ship
    selectedData4 <- reactive({
      
      if(!is.null(input$file1$datapath)){
        if(!is.null(input$inVar2)){
        
        ### ShipName
      #  my_ship_name <- as.character(as.vector(strsplit(input$inVar2, " [(]")[[1]][1]))
          my_ship_name <-  input$inVar2
        ### Only records of my ship
        data <- filter(DataSet()[[1]], SHIPNAME == my_ship_name)
        
        # Step 1: we must order data for data_order: new to older...
        # Step 2: We can calculated the distance between two consecutive observation
        #         and select now the largest distance...
        #         And we select ever the first option becouse this is the most recent
        
        
        ### Step 1: we must order data for date_order: new to older...
        ### Date Format for Date
        input_date <- data$"DATETIME"
        mod_date <- parse_date_time(input_date, orders = "ymd HMS")
        date_order <- order(mod_date, decreasing = T)
        data <- data[date_order, ]
        
        
        ### Step 2: We can calculated the distance between two consecutive observation
        ###         and select now the largest distance...
        ###         And we select ever the first option becouse this is the most recent
        
        # Distance Calculator
        coords <- data[,c("LON", "LAT")]
        shipe_distance <- DistCalculator(input_coords = coords, input_round = 4)
        
        
        pos_max_distance <- which.max(shipe_distance)[1]
        
        selected_rows <- c(pos_max_distance, pos_max_distance - 1)
        data <- data[selected_rows, ]
        Status <- c("Beginning", "End")
        data <- cbind(Status, data)
        
        my_distance_meters <- round(shipe_distance[pos_max_distance], 1)
        my_distance_km <- round(my_distance_meters/1000, 1)
        
        
        beg_time <- parse_date_time(data$"DATETIME"[1], orders = "ymd HMS")
        end_time <- parse_date_time(data$"DATETIME"[2], orders = "ymd HMS")
        
        my_time_hours <- round(abs(as.numeric(difftime(end_time, beg_time, units = "hours"))), 1)
        my_time_mins <- round(abs(as.numeric(difftime(end_time, beg_time, units = "mins"))), 1)
        my_time_secs <- round(abs(as.numeric(difftime(end_time, beg_time, units = "secs"))), 1)
        
        
        
        ###
        my_speed_ms <- round(my_distance_meters/my_time_secs, 1)
        my_speed_kmH <- round(my_distance_km/my_time_hours, 2)
        my_speed_knots <- round(my_speed_ms*1.94, 1)
        
        if( my_speed_ms == "Inf")  my_speed_ms <- 0
        if( my_speed_kmH == "Inf")  my_speed_kmH <- 0
        if( my_speed_knots == "Inf")  my_speed_knots <- 0
        
        my_names <- c("Longest Distance", "Time", "Mean Speed")
        
        my_values <- matrix(NA, 3, length(my_names))
        colnames(my_values) <- my_names
        my_values[1,] <- c(paste0(my_distance_meters, " (m)"), 
                           paste0(my_time_secs, " (sec)"), 
                           paste0(my_speed_ms, " (m/sec)")) 
        
        my_values[2,] <- c(paste0(my_distance_km, " (Km)"), 
                           paste0(my_time_hours, " (hour)"), 
                           paste0(my_speed_kmH, " (Km/hour)"))
        
        
        my_values[3,] <- c(paste0(my_distance_meters, " (m)"), 
                           paste0(my_time_secs, " (secs)"), 
                           paste0(my_speed_knots, " (knots)"))
        
        
        #### All toguether...1
        my_exit <- list(data, my_values)
        
        my_exit
        
        
      } else return(NULL)
      } else return(NULL)
    })
  
  ###    
  }
#######################  
    

########################################################    
  # Contens01 -  "Load Data"
  {
  ###
    
    # Table01_A - Head of DataSet
    output$table01_A <- renderTable({
      
      if(!is.null(input$file1$datapath)){
        
        return(head(DataSet()[[1]]))
         
      } else return(NULL)
    
    })
    
    # Text01_A - Details of DataSet
    output$text01_A <- renderUI({
      
      if (!is.null(input$file1$datapath)){
      if (!is.null(DataSet()[[1]])) {
      div(
        strong("Data File: "), input$file1[1], br(),
        strong("Columns: "), ncol(DataSet()[[1]]), " vairables.", br(),
        strong("Rows: "), nrow(DataSet()[[1]]), " records.", 
        br()
      )
      } else return(NULL)
      } else return(NULL)
      
      
    })
    
   
    
    # All contents01
    output$contents01 <- renderUI({
      if (!is.null(input$file1$datapath)){
        if (!is.null(DataSet()[[1]])) {
      div(
        h2("DataSet Details"),
        uiOutput("text01_A"),
        br(),
        h2("View (Only 6 first Records)"),
        tableOutput("table01_A")
      )
        } else   fluidRow(br(), h3("Select your Full Data (400Mb) and wait between 40 and 60 seconds!"))
      } else   fluidRow(br(), h3("Select your Full Data (400Mb) and wait between 40 and 60 seconds!"))
    })
    
    
    
    
    
  ###  
  }
#######################
  
  
  
  
########################################################    
  # Contens02 -  "My Ship"
  {
  ###
    
    # Information about only my ship
    output$table02_A <- renderTable({
  
      selectedData2()[[2]]
    })
    
    
    # Image of selected ship
    output$plot02_A <- renderImage({
      
      if(!is.null(input$file1$datapath)){
        
        my_ship_name <-  selectedData2()[[1]]
        ship_photo <- paste0(my_ship_name, ".jpg")
        
        my_photos <- list.files("www/ships/")
        dt_my_photo <- my_photos == ship_photo
        
        if (sum(dt_my_photo) == 1) the_photo <- ship_photo else the_photo <- my_photos[length(my_photos)]
        
        parts <- paste0("www/ships/", the_photo)
        list(src = parts,
             width = 500,
             height = 200)
        #   width = width,
        #   height = height)
        
      } else return(NULL)
      
    }, deleteFile=F)
    
    
    # Selected Ship V02 (Text)
    output$text02_A <- renderText({
      
    
    
      
      paste0("Selected ship: ", fusion_ship_name())
      
    })
    
    
    
    
    # Information about only my ship
    output$table02_B <- renderTable({
 
      selectedData2()[[3]]
    })
    
    # Selected Ship V01 (Text)
    output$webpage02_A <- renderUI({
      
      my_ship_name <-  selectedData2()[[1]]
      dt_page <- web_page$Ship.Name == my_ship_name
      
      if (sum(dt_page) == 1) the_page <- web_page[dt_page,4] else the_page <- web_page[nrow(web_page),4]
      
      
      url <-  a(the_page, href = the_page)
      url
    })
    
    # Selected Ship V02 (Text)
    output$text02_C <- renderText({
      
      my_ship_name <-  input$inVar2
      dt_comment <- web_page$Ship.Name == my_ship_name
      
      the_comment <- "No Comments"
      if (sum(dt_comment) == 1) the_comment <- web_page[dt_comment,5] 
      
      the_comment
      
    
    })
    
    observe(
    output$contents02 <- renderUI({
      
      if (!is.null(input$inVar2)) {
        if (!is.null(selectedData2())) {
          
          div(
            fluidRow(
              br(),
            h2(textOutput("text02_A")),br(), br(),
            h2("General Details"),
            tableOutput("table02_B"),
            h2("Extra Information"),
            tableOutput("table02_A")
            ), 
            br(),
            fluidRow(
              column(6,
              h2("Ship Image"), br(), plotOutput('plot02_A')
              ),
              column(6,
              h2("Comments"),
              textOutput("text02_C"), br(), br(), br(),
              h2("Oficial Page"),
              uiOutput("webpage02_A")
                   )
            )
         )
           
          
        } else return(NULL)
      } else return(NULL)
    })
    )
  ###  
  }
#######################   
  
  
  
########################################################    
  # Contens03 -  "All Records of My Ship"
  {
  ###
    
    # Selected Ship  (Text)
    output$text03_A <- renderText({
      
      
      paste0("Selected ship: ", fusion_ship_name())
      
    })
    
    # All Records of my ship
    output$table03_A <- DT::renderDataTable(DT::datatable({
      
      if (!is.null(input$inVar2)) {
        my_ship_name <- input$inVar2
        filter(DataSet()[[1]], SHIPNAME == my_ship_name)
      } else return(NULL)
      
    }))
    
    
    output$contents03 <- renderUI({
      
      if(!is.null(input$file1$datapath)) {
        
        div(
          fluidRow(h2(textOutput("text03_A"))), br(), br(),
          h2("All Records of the ship"), br(),
          fluidRow(
            column(11, DT::dataTableOutput("table03_A")),
            column(1)
          )
        )
        
      } else return(NULL)
      
    })
    
  ###  
  }
#######################   
  
 
   
  
  
########################################################    
  # Contens04 -  "Longest Distance of My Ship"
  {
    ###
    
    # Selected Ship V4 (Text - A)
    output$text04_A <- renderText({
      
      paste0("Selected ship: ", fusion_ship_name())
      
    })
    
    # Selected Ship V4 (Text - B)
    output$text04_B <- renderUI ({
      
     
      date1 <- parse_date_time(selectedData4()[[1]]$"DATETIME"[1], orders = "ymd HMS")
      date2 <- parse_date_time(selectedData4()[[1]]$"DATETIME"[2], orders = "ymd HMS")
      distance <- selectedData4()[[2]][1,1]
      time <- selectedData4()[[2]][2,2]
      speed <- selectedData4()[[2]][3,3]
    
      div(
        "Between records ", strong(date1), " and ", strong(date2), " the ship ", strong(fusion_ship_name()),
        " traveled a distance of ", strong(distance), " in ", strong(time) , " with Average Speed of ", strong(speed), "."
      )  
    })
    
    
    # Longest Distances
    output$table04_A <- DT::renderDataTable(DT::datatable({
      selectedData4()[[1]]
    }))
    
    # Geoposition Info
    output$table04_B <- renderTable({
      selectedData4()[[2]]
    })
    
    
    
    # Map
    output$mymap04_A <- renderLeaflet({
      
      if (!is.null(selectedData4())) {
        
        final_coords <- selectedData4()[[1]][,c("LON", "LAT")]
        status <- selectedData4()[[1]][,c("Status")]
        
        my_distance <- selectedData4()[[2]][1,1]
        my_time <- selectedData4()[[2]][1,2]
        mean_spead_knots <-  selectedData4()[[2]][1,3]
        
        my_text <- paste0("Distance: ", my_distance, " meters", "<br>",
                          "Time: ", my_time, " sec", "<br>", 
                          "Mean Speed:", mean_spead_knots, " knots")
        leaflet() %>% 
          addTiles() %>%
          addMarkers(data = final_coords, popup = status) %>%
          addPolylines(lng = final_coords[,1], lat = final_coords[,2],
                       popup = my_text, col = "orange") # %>%
        #    addPolylines(lng = lado1[,1], lat = lado1[,2], popup = c("Mean"))  %>%
        #    addPolylines(lng = lado2[,1], lat = lado2[,2], popup = c("Mean"))
        
      } else return(NULL)
    })
    
    # All Contents04
    output$contents04 <- renderUI({
      
      if (!is.null(selectedData4())) {
        
        div(
          fluidRow(h2(textOutput("text04_A"))), br(), br(),
          fluidRow(
            column(7, leafletOutput("mymap04_A", width = 700, height = 400),
                   "Coordinate Reference System (CRS): WGS84"),
            column(5, h2("Longest Distance"), tableOutput("table04_B"),
                   br(), uiOutput("text04_B"))
          ),
          br(), br(),
          h2("Geoposition for Longest Distance"),
          fluidRow(
            column(11, DT::dataTableOutput("table04_A")),
            column(1)
          ),
          br()
     
          
        )
        
      } else return(NULL)
      
    })
    
    
    ###  
  }
#######################   
      
  
  ########################################################    
  # Contens05 -  "Automatic Error Detection on DataSet"
  {
    ###

    output$list05_names_error <- renderPrint({
      
      if (!is.null(DataSet())) {
        
        DataSet()[[3]]
      } else return(NULL)
      
      
    })
    
    
    output$text05_names <- renderText({
      
      if (!is.null(DataSet())) {
        if (length(DataSet()[[2]]) > 0) {
          details <-  paste0("Total Ship Names errors: ", length(DataSet()[[2]]), 
                             " ship names related to ", length(unlist(DataSet()[[2]])), " IDs.")
        }  else  details <- "No errors in Ship names."
        details
      } else return(NULL)
    })
  
      
    output$list05_ids_error <- renderPrint({
      
      if (!is.null(DataSet())) {
        
        DataSet()[[2]]
      } else return(NULL)
      
      
    })
    
    
    output$text05_ids <- renderText({
      
      if (!is.null(DataSet())) {
      
      if (length(DataSet()[[3]]) > 0) {
        details <- paste0("Total ID errors: ", length(DataSet()[[3]]), 
                         " IDs related to ", length(unlist(DataSet()[[3]])), " ship names.")
      
      }  else   details <- "No errors in IDs."
      details
      } else return(NULL)
    })
    
    
   
    
    output$contents05 <- renderUI({
      
      if(!is.null(input$file1$datapath)) {
        
        div(
          br(),
          fluidRow(
            column(6,
              h2("Ship Name with 2 or more ID"),
              textOutput("text05_names"),
              verbatimTextOutput("list05_names_error")
            ),
            column(6, 
              h2("ID with 2 or more Ship Names"),
              textOutput("text05_ids"),
              verbatimTextOutput("list05_ids_error")
            )
          )
        )
      } else return(NULL)
      
    })
    
    ###  
  }
  #######################   
  
  
  
}