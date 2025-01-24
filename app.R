#Load Libraries ----
library(shiny)
library(DT)
library("bslib")
#library("shinyWidgets")
library("shinyalert")
#library("shinydashboard")
#library(shinyjs)
#library(bootstraplib)
library("dplyr")
library("stringr")
library("tidyr")
library("readr")
library("tibble")
#library("ggplot2")
#library("readxl")
#library("openxlsx")
library("lubridate")
#library("showtext")
#library("formattable")
library("data.table")
library("openair")

# Purple Air Data Merger V1.0.6
# Define UI for application ----
ui <- fluidPage(
  theme = bs_theme(version = 4, secondary = "#210749", bootswatch = "pulse"),
 #useShinydashboard(),
  # App title ----
  titlePanel("Purple Air Data Merger"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      tabsetPanel(
      tabPanel("Main", 
        # Input: Select a file 
      # fileInput("rawdata", "Choose Raw CSV File",
      #           multiple = TRUE,
      #           accept = c("text/csv",
      #                      "text/comma-separated-values,text/plain",
      #                      ".csv")),
      # Horizontal line ----
      tags$hr(),
      fileInput("rawdata", "Choose Raw CSV File(s):",
                multiple = TRUE,
                accept = ".csv"),
      
      # Horizontal line ----
      tags$hr(),
      
      # # Input: Select number of rows to display ----
      # radioButtons("disp", "Display",
      #              choices = c(Head = "head",
      #                          All = "all"),
      #              selected = "all"),
      # 
      # # Horizontal line ----
      # tags$hr(),

      # Input: Select number of rows to display ---- 
      radioButtons("zonetime", "Timezone Format for Data Download:",
                   choices = c(UTC = "UTC",
                               EST = "EST"),
                   selected = "UTC"),
      # Horizontal line ----
      tags$hr(),
      
      # Input: Choose dataset ----
      selectInput("dataset", "Choose Dataset to Download:",
                  choices = c("Raw Data", "Tidy Data", "Average Data", "Corrected Data", "Frequency Data")),
      
      # Input: Select number of rows to display ----
      downloadButton("downloadData", "Download"),
      
      tags$hr(),
      actionButton("helpbutton", "", icon = icon("question-circle"))
      ),
      tabPanel("Optional",
               
               # Horizontal line ----
               tags$hr(),
  
               # Input: Choose Smoke Calculation Optional ----
               checkboxInput("smokeset", "Optional: Use Smoke (High PM2.5) Conversion", value = FALSE),
               
               # Input: Use only Single Channel Values for Correction Optional ----
               checkboxInput("channelset", "Optional: Compress Channels together to remove NA", value = FALSE),
               
               # Input: Include PM1 and PM10 Channels in Average Data Optional ----
               checkboxInput("pmset", "Optional: include PM1 and PM10 Channels in Average Data", value = FALSE),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Manually Select a specific Column for Corrected Data Optional ----
               radioButtons("letterset", "Optional: Manually set Column for Corrected Data",
                            choices = c(Both = "Both",
                                        CHA = "Channel A",
                                        CHB = "Channel B"),
                            selected = "Both"),
      )
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      #tabsetPanel(
      navbarPage(
        #inverse = TRUE,
        title = 'Data Table Selection',
        #id = 'dataset',
        id = 'panelset',
        tabPanel("Raw Data", DT::dataTableOutput("contents")),
        tabPanel("Tidy Data", DT::dataTableOutput("tidyfile")),
        tabPanel("Average Data", DT::dataTableOutput("avgfile")),
        tabPanel("Corrected Data", DT::dataTableOutput("corrfile")),
        tabPanel("Frequency Data", DT::dataTableOutput("freqfile")),
        tabPanel("File names", DT::dataTableOutput("rawfile")),
        tabPanel("Data Complete", verbatimTextOutput("Percomp")),
        tabPanel("Output Test", verbatimTextOutput("Test1"))
        #tabPanel("Tidy Data", DT::dataTableOutput("mytable2")),
        #tabPanel("iris", DT::dataTableOutput("mytable3"))
      
    )
    
  )
)
)
#Create Function for Debugging Files ----
alertNoData <- function(data) # Creates and alert to showcase that you cannot download the data
{
  tryCatch(
    data,
    error = function(e)
    { 
      
      shinyalert(
        "Alert! No data available!",
        # paste("
        #   <br/>
        #   <p>
        #    Selected dataframe cannot be downloaded. Please check error printed out when selected in Data Table Selection.
        #   </p>
        #   <br/>
        #   <p>
        #     NOTE: An HTML file will be downloaded containing an error message.
        #   </p>
        #   "),

        text = paste("Selected dataframe cannot be downloaded. Please check error printed out when selected in Data Table Selection.",
          br(),
          "Review data to ensure there are no missing Columns or corrupted data and fix .csv files that have issues",
          br(),
          tags$h4("NOTE: An HTML file will be downloaded containing an error message.")),
        
        # paste("
        #   <br/>
        #   <p>
        #     Ensure you have selected a region and have clicked
        #     <b>Apply Geographic Filter</b>
        #     and
        #     <b>Apply Map Settings</b>
        #   </p>
        #   <br/>
        #   <p>
        #     NOTE: An HTML file will be downloaded containing an error message.
        #   </p>
        #   "),
        type = "error",
        html = TRUE
      )
      # showModal(modalDialog(
      #   title = "Alert!",
      #   "Selected dataframe cannot be downloaded. Please check error printed out when selected in Data Table Selection.",
      #   p(),
      #   "Additonally Review data to ensure there are no missing Columns or corrupted data and fix .csv files that have issues",
      #   #Options for Modal
      #   easyClose = TRUE,
      #   footer = modalButton("", icon = icon("times-circle")),
      #   #footer = modalButton("", icon = icon("github")),
      #   size = "m"
      # ))
      stop(safeError("Error"))
    }
  )
}


# Define server logic to read selected file ----
server <- function(input, output, session) {
  #bslib::bs_themer()
  # Define Raw Data Reactive Function ----
  Raw_data <- reactive({
    req(input$rawdata)
    Raw_input <- input$rawdata # Creates File
    Tidy_input <- Raw_input %>% filter(grepl('.csv', name)) # Selects only files that end in ".csv"
    Rawdf <- do.call(rbind, lapply(Tidy_input$datapath, fread, fill = TRUE)) # Combines files into a df
    #Rawdf <- do.call(rbind, lapply(Tidy_input$datapath, read_csv)) # Combines files into a df
    return(Rawdf)
  })
  
  # Prob_data <- reactive({
  #   req(input$rawdata)
  #   Raw_data <- Raw_data()
  #   Probdf <- problems(Raw_data)
  #   return(Probdf)
  # })  
  
  Mac_val <- reactive({
    req(input$rawdata)
    Raw_data <- Raw_data()
    Mac_val <- as.character(Raw_data[1, 'mac_address'])
    return(Mac_val)
  })
  
  Tz_val <- reactive({
    req(input$rawdata)
    if(input$zonetime == "EST") {
      Tz_val <- as.character("EST")
      return(Tz_val)
    }
    else {
      Tz_val <- as.character("UTC")
      return(Tz_val)
    }
  })

  
  
  # Define Tidy Data Reactive Function ----
  Tidy_data <- reactive({
    req(input$rawdata)
    Tidy_data <- Raw_data()
    if(input$pmset == TRUE) {
    Tidy_data <- select(Tidy_data, "UTCDateTime", "pm1_0_cf_1", "pm1_0_cf_1_b", "pm2_5_cf_1", "pm2_5_cf_1_b", "pm10_0_cf_1", "pm10_0_cf_1_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
    #Tidy_data <- select(Tidy_data, "UTCDateTime", "pm2_5_atm", "pm2_5_atm_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
    Tidy_data$UTCDateTime <- str_replace_all(Tidy_data$UTCDateTime, "[:alpha:]", " ") #Removes any letters in Date column
    #Tidy_data$UTCDateTime <- str_remove_all(Tidy_data$UTCDateTime, '[Tz]') #Removes any letters in Date column
    Tidy_data$Date_UTC <- as.POSIXct(Tidy_data$UTCDateTime, tz = "UTC", format = "%Y/%m/%d %H:%M:%S") # Convert to UTC
    # Tidy_data <- Tidy_data %>% 
    #   mutate(Date_EST = as_datetime(Date_UTC, tz = "EST"))
    #Tidy_data$Date_UTC <- as.POSIXct(Tidy_data$UTCDateTime, tz = "EST", format = "%Y/%m/%d %H:%M:%S", usetz = T) # Convert to UTC
    Tidy_data <- select(Tidy_data, "Date_UTC", "pm1_0_cf_1", "pm1_0_cf_1_b", "pm2_5_cf_1", "pm2_5_cf_1_b", "pm10_0_cf_1", "pm10_0_cf_1_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
    #Tidy_data <- select(Tidy_data, "Date_UTC", "pm2_5_atm", "pm2_5_atm_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
    colnames(Tidy_data) = c("date", "PM1_A", "PM1_B", "PM2.5_A", "PM2.5_B", "PM10_A", "PM10_B", "Temperature", "Humidity", "Dewpoint", "Pressure")
    #return(Tidy_data)
    #colSums(is.na(Tidy_data)) #prints the number of NA in each column
    }
    else {
      Tidy_data <- select(Tidy_data, "UTCDateTime", "pm2_5_cf_1", "pm2_5_cf_1_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
      #Tidy_data <- select(Tidy_data, "UTCDateTime", "pm2_5_atm", "pm2_5_atm_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
      Tidy_data$UTCDateTime <- str_replace_all(Tidy_data$UTCDateTime, "[:alpha:]", " ") #Removes any letters in Date column
      Tidy_data$Date_UTC <- as.POSIXct(Tidy_data$UTCDateTime, tz = "UTC", format = "%Y/%m/%d %H:%M:%S") # Convert to UTC
      Tidy_data <- select(Tidy_data, "Date_UTC", "pm2_5_cf_1", "pm2_5_cf_1_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
      #Tidy_data <- select(Tidy_data, "Date_UTC", "pm2_5_atm", "pm2_5_atm_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
      colnames(Tidy_data) = c("date","PM2.5_A", "PM2.5_B", "Temperature", "Humidity", "Dewpoint", "Pressure")
    }
    
    # New Portion for MAC address
    Mac_val <- Mac_val()
    Tz_val <- Tz_val()
    Tidy_data <- Tidy_data %>% 
      mutate(mac_address = if_else(!is.na(date), Mac_val, NA_character_)) %>%
      mutate(Timezone = if_else(!is.na(date), Tz_val, NA_character_))
  # Conversion to EST for Data Download
  if(input$zonetime == "EST") {
    Tidy_data <- Tidy_data %>% 
       mutate(date = as_datetime(date, tz = "EST"))
    return(Tidy_data)
  }
  else {
    return(Tidy_data)
  }
  })
  
  # Define Average Data Reactive Function ----
  Avg_data <- reactive({
    req(input$rawdata)
    Avg_data <- timeAverage(
      Tidy_data(),
      avg.time = "hour",
      data.thresh = 0,
      statistic = "mean",
      type = "default",
      percentile = NA,
      start.date = NA,
      end.date = NA,
      interval = NA,
      vector.ws = FALSE,
      fill = FALSE,
    )
  # New Portion for MAC address
  Mac_val <- Mac_val()
  Tz_val <- Tz_val()
  Avg_data <- Avg_data %>% 
    mutate(mac_address = if_else(!is.na(date), Mac_val, NA_character_)) %>%
    mutate(Timezone = if_else(!is.na(date), Tz_val, NA_character_))
  return(Avg_data)
  })

  Freq_data <- reactive({
    req(input$rawdata)
    Freq_data <- timeAverage(
      Tidy_data(),
      avg.time = "hour",
      data.thresh = 0,
      statistic = "frequency",
      type = "default",
      percentile = NA,
      start.date = NA,
      end.date = NA,
      interval = NA,
      vector.ws = FALSE,
      fill = FALSE,
    )
  # New Portion for MAC address
  Mac_val <- Mac_val()
  Tz_val <- Tz_val()
  Freq_data <- Freq_data %>% 
    mutate(mac_address = if_else(!is.na(date), Mac_val, NA_character_)) %>%
    mutate(Timezone = if_else(!is.na(date), Tz_val, NA_character_))
  return(Freq_data)
  })
  
  # Define Corrected Data Reactive Function ----  
  Corr_data <- reactive({
    Purple_Data <- Avg_data()
    if(input$channelset == TRUE){
      Purple_Data <- Purple_Data %>%
        mutate(AB_dif_per = (abs(PM2.5_A - PM2.5_B)/((PM2.5_A + PM2.5_B)/2))) %>% # Calculates percent diff
        mutate(AB_dif_val = abs(PM2.5_A - PM2.5_B))  %>% #Checks difference between Channels
        mutate(PM2.5_AVG = ((PM2.5_A + PM2.5_B)/2))  %>% #Calculates AVG between A & B Channels
        mutate(PM_Check = ifelse(is.na(PM2.5_A) | is.na(PM2.5_B), coalesce(PM2.5_A,PM2.5_B), PM2.5_AVG))
#      mutate(PM_Check = ifelse(is.na(PM2.5_A) | is.na(PM2.5_B), coalesce(PM2.5_A,PM2.5_B), paste0(PM2.5_A,PM2.5_B)))
      Purple_Data$AB_dif <- round(Purple_Data$AB_dif_per, 2)
    } else if(input$letterset == "Channel A"){
      # Sets the channel to Channel A
      Purple_Data <- Purple_Data %>%
        mutate(PM_Check = PM2.5_A)
    } else if(input$letterset == "Channel B"){
      # Sets the channel to Channel B
      Purple_Data <- Purple_Data %>%
        mutate(PM_Check = PM2.5_B)
    } else{
    # Validation for correction is 70% difference or a difference of 5 
    #Purple_Data <- Avg_data()
    Purple_Data <- Purple_Data %>%
      mutate(AB_dif_per = (abs(PM2.5_A - PM2.5_B)/((PM2.5_A + PM2.5_B)/2))) %>% # Calculates percent diff
      mutate(AB_dif_val = abs(PM2.5_A - PM2.5_B))  %>% #Checks difference between Channels
      mutate(PM2.5_AVG = ((PM2.5_A + PM2.5_B)/2))  %>% #Calculates AVG between A & B Channels
      mutate(PM_Check  = if_else(AB_dif_per <= 0.7 | AB_dif_val <= 5, PM2.5_AVG, NA_real_)) # Performs Validation for correction
    Purple_Data$AB_dif <- round(Purple_Data$AB_dif_per, 2)
    }

    corr_data <- Purple_Data
    corr_data = rename(corr_data, c(PM = PM_Check, RH = Humidity)) # Rename for equation

    if(input$smokeset == TRUE) {
      # #correction using EPA simple correction
      corr_data <- corr_data %>% mutate(PM_corr_EPA = case_when(is.numeric(PM) ~ 0.524*PM - 0.0862*RH + 5.75,
                                                                TRUE ~ NA_real_
                                                                )
      )
    }
    else {
      #Other Method using EPA Multi Point Correction for High PM2.5 (smoke) *Newer Equation (Dec 2022)*
      corr_data <- corr_data %>% mutate(PM_corr_EPA = case_when((0 <= PM) & (PM < 570) ~ PM*0.524 - 0.0862*RH + 5.75,
                                                            (570 <= PM) & (PM < 611) ~ ((0.0244 * PM - 13.9) * (((PM)^{2}) * (4.21*10^{-4}) + (PM*0.392) + 3.44) + (1-(0.0244 * PM - 13.9)) * (PM*0.524 - 0.0862*RH + 5.75)),
                                                            (611 <= PM) ~ ((PM)^{2}) * (4.21*10^{-4}) + (PM*0.392) + 3.44,
                                                            TRUE ~ NA_real_
                                                            )
      )
    }
    
    corr_data = select(corr_data,"date", "PM", "PM_corr_EPA","RH", "Temperature", "Pressure") #Reorder df
    
    #Other Method using EPA Multi Point Correction *Older method for PM2.5_atm*
    # corr_data <- corr_data %>% mutate(PM_corr_EPA = case_when((0 <= PM) & (PM < 30) ~ 0.524*PM - 0.0862*RH + 5.75,
    #                                                       (30 <= PM) & (PM < 50) ~ (0.786*((PM/20) - (3/2)) + 0.524*(1-((PM/20) - (3/2)))*PM - 0.0862*RH + 5.75),
    #                                                       (50 <= PM) & (PM < 210) ~ 0.786*PM - 0.0862*RH + 5.75,
    #                                                       (210 <= PM) & (PM < 260) ~ (0.69*((PM/50) - (21/5)) + 0.786*(1-((PM/50) - (21/5)))*PM - 0.0862*RH*(1-((PM/50) - (21/5))) + 2.966*((PM/50) - (21/5)) + 5.75*(1-((PM/50) - (21/5))) + 8.84*(10^{-4})*(PM^{2})*((PM/50) - (21/5))),
    #                                                       (260 <= PM) ~ 2.966 + (0.69*PM) + (8.84*10^{-4}*PM^{2}),
    #                                                       TRUE ~ NA_real_
    # )
    # )
    
    # New Portion for MAC address
    Mac_val <- Mac_val()
    Tz_val <- Tz_val()
    corr_data <- corr_data %>% 
      #mutate(mac_address = if_else(is.POSIXct(date), Mac_val, NA_character_))
      mutate(mac_address = if_else(!is.na(date), Mac_val, NA_character_)) %>%
      mutate(Timezone = if_else(!is.na(date), Tz_val, NA_character_))
    return(corr_data)
  })
    
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Raw Data" = Raw_data(),
           "Tidy Data" = Tidy_data(),
           "Average Data" = Avg_data(),
           "Corrected Data" = Corr_data(),
           "Frequency Data" = Freq_data())
  })
  
  #Set global options for Datatables 
  #options(DT.options = list())) #Place options inside List
Zone_sel <- reactive({if(input$zonetime == "EST") {
  #Zone_selc <- "EST"
  #return(Zone_selc)
  Tbloption <- list(year = 'numeric', 
                    month = 'long', 
                    day = 'numeric', 
                    hour = 'numeric', 
                    minute = 'numeric', 
                    second = 'numeric', 
                    timeZoneName = "short", 
                    timeZone = "EST")
  return(Tbloption)
}
  else {
    #Zone_selc <- "UTC"
    #return(Zone_selc)
    Tbloption <- list(year = 'numeric', 
                      month = 'long', 
                      day = 'numeric', 
                      hour = 'numeric', 
                      minute = 'numeric', 
                      second = 'numeric', 
                      timeZoneName = "short", 
                      timeZone = "UTC")
    return(Tbloption)
  }
  })
#Tbloption <- Zone_sel()

# ZoneSelect = Zone_sel()
#  Tbloption <- list(year = 'numeric', 
#                    month = 'long', 
#                    day = 'numeric', 
#                    hour = 'numeric', 
#                    minute = 'numeric', 
#                    second = 'numeric', 
#                    timeZoneName = "short", 
#                    timeZone = ZoneSelect)

# Tbloption2 <-formatDate(columns = 'date',
#                         method = 'toLocaleString',
#                         #params = list('en-KR', list(year = 'numeric', month = 'long', day = 'numeric')))
#                         params = list('en-US', list(year = 'numeric', 
#                                                     month = 'long', 
#                                                     day = 'numeric', 
#                                                     hour = 'numeric',
#                                                     minute = 'numeric',
#                                                     second = 'numeric',
#                                                     timeZoneName = "short",
#                                                     timeZone = "UTC")))
  
  output$contents <- DT::renderDataTable({
    req(input$rawdata)
    tryCatch(
      {
        Rawdf <- Raw_data()
        Rawdf
      },
      error = function(e) {
        errorsf <- as.character(safeError(e))
        validate(
          need(exists("Rawdf"), paste("Data frame not generated please check error:", errorsf, sep = "\n"))
        )
        stop(safeError(e))
      }
    )
    
  })

  output$rawfile <- DT::renderDataTable({
    req(input$rawdata)
    tryCatch(
       {
    Raw_input <- input$rawdata # Creates File
    return(Raw_input)
      },
      error = function(e) {
        errorsf <- as.character(safeError(e))
        validate(
          need(exists("Raw_input"), paste("Data frame not generated please check error:", errorsf, sep = "\n"))
        )
        stop(safeError(e))
      }
    )

  })
  
  output$tidyfile <- DT::renderDataTable({
    req(input$rawdata)
    tryCatch(
      {
        Tidydf <- Tidy_data()
        
        validate(need(ncol(Tidydf) == 9|11,  "Dataframe is missing columns please review data for errors/corruption"))
        Tbloption <- Zone_sel()
        Tidydf <- datatable(Tidydf) %>% 
          formatDate(columns = 'date',
                     method = 'toLocaleString',
                     params = list('en-us', Tbloption))
        # Tidydf <- datatable(Tidydf) %>% 
        #   formatDate(columns = 'date',
        #              method = 'toUTCString')
        return(Tidydf)
      },
      error = function(e) {
        errorsf <- as.character(safeError(e))
        validate(
          need(exists("Tidydf"), paste("Data frame not generated please check error:", errorsf, sep = "\n"))
        )        
        stop(safeError(e))
      }
    )
  })
  
  output$avgfile <- DT::renderDataTable({
    req(input$rawdata)
      
      tryCatch(
        {
          Avgdf <- Avg_data()
          # Validation for confirming Dataframe completeness 
          validate(need(ncol(Avgdf) == 9|11,  "Dataframe is missing columns please review data for errors/corruption."))
          
          Tbloption <- Zone_sel()
          Avgdf <- datatable(Avgdf) %>% 
            formatDate(columns = 'date',
                       method = 'toLocaleString',
                       #params = list('en-KR', list(year = 'numeric', month = 'long', day = 'numeric')))
                       params = list('en-US', Tbloption)
                       # params = list('en-US', list(year = 'numeric', 
                       #                             month = 'long', 
                       #                             day = 'numeric', 
                       #                             hour = 'numeric',
                       #                             minute = 'numeric',
                       #                             second = 'numeric',
                       #                             #dateStyle = "Full",
                       #                             #timeStyle = "Full", 
                       #                             timeZoneName = "short",
                       #                             timeZone = "EST"
                       #                             )
                       #               )
                       )  
        },
        error = function(e) {
          errorsf <- as.character(safeError(e))
          validate(
            need(exists("Avgdf"), paste("Data frame not generated please check error:", errorsf, sep = "\n"))
            #need(ncol("Avgdf") != 7,  "Dataframe is missing columns please review data for errors/corruption")
          )    
          stop(safeError(e))
        }
      )
  })
  
    output$freqfile <- DT::renderDataTable({
      req(input$rawdata)
      
      tryCatch(
        {
          Freqdf <- Freq_data()
          validate(need(ncol(Freqdf) == 9,  "Dataframe is missing columns please review data for errors/corruption."))          
          Tbloption <- Zone_sel()
          Freqdf <- datatable(Freqdf) %>% 
            formatDate(columns = 'date',
                       method = 'toLocaleString',
                       params = list('en-us', Tbloption))
        },
        error = function(e) {
          errorsf <- as.character(safeError(e))
          validate(
            need(exists("Freqdf"), paste("Data frame not generated please check error:", errorsf, sep = "\n"))
          ) 
          stop(safeError(e))
        }
      )
  })
    
  output$corrfile <- DT::renderDataTable({
    req(input$rawdata)
    
    tryCatch(
      {
        Corrdf <- Corr_data()
        validate(need(ncol(Corrdf) == 8,  "Dataframe is missing columns please review data for errors/corruption."))
        #Corrdf
        Tbloption <- Zone_sel()
        Corrdf <- datatable(Corrdf) %>% 
          formatDate(columns = 'date',
                     method = 'toLocaleString',
                     params = list('en-us', Tbloption))
        return(Corrdf)
      },
      error = function(e) {
        errorsf <- as.character(safeError(e))
        validate(
          need(exists("Corrdf"), paste("Data frame not generated please check error:", errorsf, sep = "\n"))
        )
        stop(safeError(e))
      }
    )
  })

  # Renders % of Complete Data collected ----
  output$Percomp <- renderPrint({
    req(input$rawdata)
    Avg_data <- Avg_data()
    Total_NA <- sum(is.na(Avg_data$PM2.5_A) | is.na(Avg_data$PM2.5_B) | is.na(Avg_data$Humidity)) #Total number of rows containing NA
    Total_row <- nrow(Avg_data) # Total number of rows
    Data_coll <- (1 - (Total_NA /Total_row)) * 100 # percentage of complete Data
    return(print(paste("The percentage of Complete Data collected for the file is: ", round(Data_coll,1),"%", sep = "")))
  })  
    
  # Renders Output of Variables for Debugging ----
  output$Test1 <- renderPrint({
    req(input$rawdata)
    #return(Prob_data())
    return(str(Tidy_data()))
  })

    
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      alertNoData(datasetInput())
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

  # Change Navbar tab based on input 
  observeEvent(input$dataset, {
    updateNavbarPage(session, "panelset", selected = input$dataset)
  })
  
  # Help Button Modal Info
  observeEvent(input$helpbutton, {
    showModal(modalDialog(
      title = "Help",
      "This is a R shiny App to tidy Purple Air SD card data into one file.",
      p(),
      h5("Steps:"),
      tags$ol(
      tags$li("Browse for the SD card data and select the raw .csv files"),
      #br(),
      tags$li("Select the Timezone that the data will be in."),
      tags$li("Check smoke conversion if data was collected on smoke days"),
      tags$li("Select the Dataset to export and click Download.")
        ),
      tags$hr(),
      h5("About:"),
      "Purple Air Data Merger Version 1.0.6",
      br(),
      "More info and updates can be be found on GitHub:",
      a(href = "https://github.com/SebAire/Purple-Air-Data-Merger", "Link"),
      br(),
      "Info about SD card data headers can be found here:",
      a(href = "https://community.purpleair.com/t/sd-card-file-headers/279", "Link"),
      
      #Options for Modal
      easyClose = TRUE,
      footer = modalButton("", icon = icon("times-circle")),
      #footer = modalButton("", icon = icon("github")),
      size = "m"
    ))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
# Run with Bootstrapper Themer
#run_with_themer(shinyApp(ui = ui, server = server))
