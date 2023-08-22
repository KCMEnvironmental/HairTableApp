# TrichAnalytics Shiny App: Hair Data Table App - creates summary pivot table 
# Created 21 August 2023 KMill 
#
# 
# To run App: 
#   Click the 'Run App' button above
#
## TO DO IF TIME - add reactive checkbox group that will populate once file uploaded, but automatically select iron, zinc, mercury 
## TO DO IF TIME - combine all apps to date into one app with separate headers/tabs

# Load Libraries ----
library(shiny)
library(shinythemes) 
library(shinyWidgets)
library(shinyFiles)
library(tidyverse)
library(openxlsx)

# Define UI for application ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Trich Hair Data Table - Pivot"),
  hr(),
  h3("Upload Data File"),
  fileInput("file",
            label = NULL), 
  numericInput("len.sum", 
               label = h5("Enter summary interval"), 
               value = 20),
  downloadButton(outputId = "add_pivot",
                 label = "Add Pivot Table",
                 icon = icon("table")))
  
# Define server logic ----
server <- function(input, output) {
  
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  output$add_pivot <- downloadHandler(
    
    filename = function() {
      paste("Pivot_", input$file, sep = "")
    },
    
    content = function(file) {
    
    wb <- loadWorkbook(input$file$datapath)
    addWorksheet(wb, "Pivot")
    
    data <- readxl::read_excel(input$file$datapath, sheet = "corrected") %>% 
      subset(Time != "MED") %>% 
      mutate(id = row_number()) %>% 
      mutate(ints = cut(id, breaks = seq(0, nrow(.), input$len.sum), 
                        include.lowest = TRUE)) %>% 
      group_by(ints) %>% 
      mutate(Time_min = min(Time), Time_max = max(Time)) %>%
      mutate(Time_bin = paste(round(as.numeric(Time_min), 0), "-", round(as.numeric(Time_max), 0))) %>% 
      ungroup() %>% 
      group_by(ints, Time_bin) %>% 
      summarise(across(c("57Fe", "66Zn", "63Cu", "202Hg" ), median, .names = "{.col}_median")) %>% 
      mutate(Copper_adjusted = `63Cu_median`*6.6/`57Fe_median`) %>% 
      ungroup()
    
    data <- as.data.frame(data)
    
    writeData(wb, "Pivot", data)
    saveWorkbook(wb, file)
   
    #write.xlsx(as.data.frame(data), "Pivot.xlsx", sheetName = "Pivot Table", append = TRUE)
  
    })}


# Run the application ----
shinyApp(ui = ui, server = server)