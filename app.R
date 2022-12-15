#Load Libraries ----
library(shiny)
library(DT)
library(bslib)
#library(bootstraplib)
library("dplyr")
library("stringr")
library("tidyr")
library("tibble")
#library("ggplot2")
#library("readxl")
#library("openxlsx")
library("lubridate")
#library("showtext")
#library("formattable")
library("data.table")
library("openair")


# Define UI for application ----
ui <- fluidPage(
  theme = bs_theme(version = 4, secondary = "#210749", bootswatch = "pulse"),

  # App title ----
  titlePanel("Purple Air Data Merger"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file 
      # fileInput("rawdata", "Choose Raw CSV File",
      #           multiple = TRUE,
      #           accept = c("text/csv",
      #                      "text/comma-separated-values,text/plain",
      #                      ".csv")),
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
      downloadButton("downloadData", "Download")
      
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

# Define server logic to read selected file ----
server <- function(input, output) {
  #bslib::bs_themer()
  # Define Raw Data Reactive Function ----
  Raw_data <- reactive({
    req(input$rawdata)
    Raw_input <- input$rawdata # Creates File
    Tidy_input <- Raw_input %>% filter(grepl('.csv', name)) # Selects only files that end in ".csv"
    Rawdf <- do.call(rbind, lapply(Tidy_input$datapath, fread, fill = TRUE)) # Combines files into a df
    return(Rawdf)
  })
  
  # Define Tidy Data Reactive Function ----
  Tidy_data <- reactive({
    req(input$rawdata)
    Tidy_data <- Raw_data()
    Tidy_data <- select(Tidy_data, "UTCDateTime", "pm2_5_cf_1", "pm2_5_cf_1_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
    #Tidy_data <- select(Tidy_data, "UTCDateTime", "pm2_5_atm", "pm2_5_atm_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
    Tidy_data$UTCDateTime <- str_replace_all(Tidy_data$UTCDateTime, "[:alpha:]", " ") #Removes any letters in Date column
    #Tidy_data$UTCDateTime <- str_remove_all(Tidy_data$UTCDateTime, '[Tz]') #Removes any letters in Date column
    Tidy_data$Date_UTC <- as.POSIXct(Tidy_data$UTCDateTime, tz = "UTC", format = "%Y/%m/%d %H:%M:%S") # Convert to UTC
    # Tidy_data <- Tidy_data %>% 
    #   mutate(Date_EST = as_datetime(Date_UTC, tz = "EST"))
    #Tidy_data$Date_UTC <- as.POSIXct(Tidy_data$UTCDateTime, tz = "EST", format = "%Y/%m/%d %H:%M:%S", usetz = T) # Convert to UTC
    Tidy_data <- select(Tidy_data, "Date_UTC", "pm2_5_cf_1", "pm2_5_cf_1_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
    #Tidy_data <- select(Tidy_data, "Date_UTC", "pm2_5_atm", "pm2_5_atm_b", "current_temp_f", "current_humidity", "current_dewpoint_f", "pressure")
    colnames(Tidy_data) = c("date", "PM2.5_A", "PM2.5_B", "Temperature", "Humidity", "Dewpoint", "Pressure")
    #return(Tidy_data)
    #colSums(is.na(Tidy_data)) #prints the number of NA in each column
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
    return(Freq_data)
  })
  
  # Define Corrected Data Reactive Function ----  
  Corr_data <- reactive({
    Purple_Data <- Avg_data()
    Purple_Data <- Purple_Data %>%
      mutate(AB_dif = (abs(PM2.5_A - PM2.5_B)/((PM2.5_A + PM2.5_B)/2))) %>% # Calculates percent diff
      mutate(PM2.5_AVG = ((PM2.5_A + PM2.5_B)/2)) %>% #Calculates AVG between A & B Channels
      mutate(PM_Check  = if_else(AB_dif <= 0.7, PM2.5_AVG, NA_real_)) # Performs Validation for correction
    Purple_Data$AB_dif <- round(Purple_Data$AB_dif, 2)
    corr_data <- Purple_Data
    corr_data = rename(corr_data, c(PM = PM_Check, RH = Humidity)) # Rename for equation

    #correction using EPA simple correction:
    corr_data <- corr_data %>% mutate(PM_corr_EPA = case_when(is.numeric(PM) ~ 0.524*PM - 0.0862*RH + 5.75,
                                                          TRUE ~ NA_real_
                                                         )
                                     )
    #Other Method using EPA Multi Point Correction
    # corr_data <- corr_data %>% mutate(PM_corr_EPA = case_when((0 <= PM) & (PM < 30) ~ 0.524*PM - 0.0862*RH + 5.75,
    #                                                       (30 <= PM) & (PM < 50) ~ (0.786*((PM/20) - (3/2)) + 0.524*(1-((PM/20) - (3/2)))*PM - 0.0862*RH + 5.75),
    #                                                       (50 <= PM) & (PM < 210) ~ 0.786*PM - 0.0862*RH + 5.75,
    #                                                       (210 <= PM) & (PM < 260) ~ (0.69*((PM/50) - (21/5)) + 0.786*(1-((PM/50) - (21/5)))*PM - 0.0862*RH*(1-((PM/50) - (21/5))) + 2.966*((PM/50) - (21/5)) + 5.75*(1-((PM/50) - (21/5))) + 8.84*(10^{-4})*(PM^{2})*((PM/50) - (21/5))),
    #                                                       (260 <= PM) ~ 2.966 + (0.69*PM) + (8.84*10^{-4}*PM^{2}),
    #                                                       TRUE ~ NA_real_
    # )
    # )
    corr_data = select(corr_data,"date", "PM", "PM_corr_EPA","RH", "Temperature", "Pressure") #Reorder df
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
        
        stop(safeError(e))
      }
    )

  })
  
  output$tidyfile <- DT::renderDataTable({
    req(input$rawdata)
    tryCatch(
      {
        Tidydf <- Tidy_data()
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
        
        stop(safeError(e))
      }
    )
  })
  
  output$avgfile <- DT::renderDataTable({
    req(input$rawdata)
      
      tryCatch(
        {
          Avgdf <- Avg_data()
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
          
          stop(safeError(e))
        }
      )
  })
  
    output$freqfile <- DT::renderDataTable({
      req(input$rawdata)
      
      tryCatch(
        {
          Freqdf <- Freq_data()
          Tbloption <- Zone_sel()
          Freqdf <- datatable(Freqdf) %>% 
            formatDate(columns = 'date',
                       method = 'toLocaleString',
                       params = list('en-us', Tbloption))
        },
        error = function(e) {

          stop(safeError(e))
        }
      )
  })
    
  output$corrfile <- DT::renderDataTable({
    req(input$rawdata)
    
    tryCatch(
      {
        Corrdf <- Corr_data()
        #Corrdf
        Tbloption <- Zone_sel()
        Corrdf <- datatable(Corrdf) %>% 
          formatDate(columns = 'date',
                     method = 'toLocaleString',
                     params = list('en-us', Tbloption))
        return(Corrdf)
      },
      error = function(e) {
        
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
    Data_coll <- (1 - (Total_NA /Total_row)) * 100 # pecetnage of complete Data
    return(print(paste("The percentage of Complete Data collected for the file is: ", round(Data_coll,1),"%", sep = "")))
  })  
    
  # Renders Output of Variables for Debugging ----
  output$Test1 <- renderPrint({
    req(input$rawdata)
    return(str(Tidy_data()))
  })

    
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
# Run with Bootstrapper Themer
#run_with_themer(shinyApp(ui = ui, server = server))
