### Shiny app - ###

## Downloading packages
library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)

## Downloading data
#There is room for more data, just continue with same format, and add to combined data
north_sea_d <- read.csv("data/SAU LME 22 v50-1/SAU LME 22 v50-1.csv",
                        stringsAsFactors = FALSE, 
                        fill = TRUE, 
                        na.strings = c("", "NA"))

norwegian_sea_d <- read.csv("data/SAU LME 21 v50-1/SAU LME 21 v50-1.csv", 
                            stringsAsFactors = FALSE, 
                            fill = TRUE, 
                            na.strings = c("", "NA"))

barents_sea_d <- read.csv("data/SAU LME 20 v50-1/SAU LME 20 v50-1.csv", 
                          stringsAsFactors = FALSE, 
                          fill = TRUE, 
                          na.strings = c("", "NA"))

baltic_sea_d <- read.csv("data/SAU LME 23 v50-1/SAU LME 23 v50-1.csv", 
                         stringsAsFactors = FALSE, 
                         fill = TRUE, 
                         na.strings = c("", "NA"))


# Data cleaning and preparation
combined_data <- bind_rows(north_sea_d, norwegian_sea_d, barents_sea_d, baltic_sea_d)

# Check for unique commercial groups
unique(combined_data$commercial_group)

# Assigning commercial groups to colors for stacked area plot 
group_colors <- c(
  "Anchovies"             = "#e41a1c",  
  "Cod-likes"             = "#1f78b4",  
  "Crustaceans"           = "#33a02c",  
  "Flatfishes"            = "#a6cee3",  
  "Herring-likes"         = "#6baed6",  
  "Molluscs"              = "#984ea3",  
  "Other fishes & inverts"= "#bdbdbd",  
  "Perch-likes"           = "#b2df8a",  
  "Salmon, smelts, etc"   = "#fb9a99",  
  "Scorpionfishes"        = "#f781bf",  
  "Sharks & rays"         = "#ff7f00",  
  "Tuna & billfishes"     = "#ffff33"   
)


options(scipen = 999)


# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  titlePanel("Fishery Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Select Filters"),
      
      # Main plot filters
      selectInput("selected_lmes", "Select LMEs:", 
                  choices = unique(combined_data$area_name),
                  multiple = TRUE,
                  selected = "North Sea"),
      
      checkboxGroupInput("catch_types", "Select Catch Types:", 
                         choices = unique(combined_data$catch_type), 
                         selected = c("Discards", "Landings")),
      
      hr(),
      h3("Stacked Area Plot Settings"),
      selectInput("region_area_plot", "Select Region:",
                  choices = unique(combined_data$area_name),
                  selected = "North Sea"),
      
      selectInput("catch_type_area_plot", "Select Catch Type:",
                  choices = unique(combined_data$catch_type),
                  selected = "Landings")
    ),
    
    mainPanel(
      h3("Discards vs Landings by Region"),
      plotOutput("plot"),
      hr(),
      h3("Catch Composition by Commercial Group (Stacked Area)"),
      plotOutput("area_stack_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Filter for main line plot
  filtered_data <- reactive({
    combined_data %>%
      filter(area_name %in% input$selected_lmes,
             catch_type %in% input$catch_types,
             !is.na(tonnes))
  })
  
  # Discards vs Landings plot
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = year, y = tonnes, color = area_name, linetype = catch_type)) +
      geom_line(stat = "summary", fun = sum) +
      scale_y_continuous(labels = function(x) format(x / 1000, big.mark = " ", scientific = FALSE)) +
      labs(title = "Discards vs Landings by Region",
           x = "Year", y = "Catch (Tonnes x 1000", color = "Region", linetype = "Catch Type") +
      theme_minimal()
  })
  
  # Stacked area plot
  output$area_stack_plot <- renderPlot({
    area_data <- combined_data %>%
      filter(area_name == input$region_area_plot,
             catch_type == input$catch_type_area_plot,
             !is.na(commercial_group),
             !is.na(tonnes)) %>%
      group_by(year, commercial_group) %>%
      summarise(total_tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop")
    
    # Reorder commercial groups by size
    order_by_size <- area_data %>%
      group_by(commercial_group) %>%
      summarise(total = sum(total_tonnes)) %>%
      arrange(desc(total)) %>%
      pull(commercial_group)
    
    
    area_data <- area_data %>%
      mutate(commercial_group = factor(commercial_group, levels = rev(order_by_size)))
    
    ggplot(area_data, aes(x = year, y = total_tonnes, fill = commercial_group)) +
      geom_area() +
      scale_fill_manual(values = group_colors) +
      scale_y_continuous(labels = function(x) format(x / 1000, big.mark = " ", scientific = FALSE)) +
      labs(
        title = paste(input$catch_type_area_plot, "by Commercial Group in", input$region_area_plot),
        x = "Year", y = "Catch (Tonnes x 1000)", fill = "Commercial Group"
      ) +
      theme_minimal()
  })
}

# Launch app
shinyApp(ui = ui, server = server)

