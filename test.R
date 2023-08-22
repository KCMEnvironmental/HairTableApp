# TrichAnalytics Shiny App: Hair Data Table App - creates summary pivot table 
# Created 21 August 2023 KMill 

# TEST SCRIPT

library(xlsx)
# Name data
data <- readxl::read_excel("044D1.xlsx", sheet = "corrected") %>% # replace ".xlsx" with input$file
  subset(Time != "MED") %>% 
  mutate(id = row_number()) %>% 
  mutate(ints = cut(id, breaks = seq(0, nrow(.), 20), # replace '20' with 'input$len.sum'
                    include.lowest = TRUE)) %>% 
  group_by(ints) %>% 
  mutate(Time_min = min(Time), Time_max = max(Time)) %>%
  mutate(Time_bin = paste(round(as.numeric(Time_min), 0), "-", round(as.numeric(Time_max), 0))) %>% 
  ungroup() %>% 
  group_by(ints, Time_bin) %>% 
  summarise(across(c("57Fe", "66Zn", "63Cu", "202Hg" ), median, .names = "{.col}_median")) %>% 
  mutate(Copper_adjusted = `63Cu_median`*6.6/`57Fe_median`)

write.xlsx(as.data.frame(data), "044D1.xlsx", sheetName = "Pivot Table", append = TRUE)
  

  # Add check box option 
  # Format so all apps are accessible at same link 

class(data$Time_min)
