# Project: Energy Data Visualisation Shiny App                                 

# Load required libraries
library(shiny)
library(tidyverse)
library(plotly)

#--------------------------------------------------------
# Import datasets
#--------------------------------------------------------
gen_fuelmix        <- read_csv("Datasets/generation_fuelmix.csv")
energy_use         <- read_csv("Datasets/energy_consumption.csv")
energy_mix_recent  <- read_csv("Datasets/energy_mix_2023_24.csv")
energy_gen         <- read_csv("Datasets/energy_production.csv")
trade_data         <- read_csv("Datasets/Import_Export.csv")
renewable_sources  <- read_csv("Datasets/renewables.csv")

#--------------------------------------------------------
# Data cleaning and preprocessing
#--------------------------------------------------------
gen_fuelmix <- gen_fuelmix %>%
    rename(FuelType = `Fuel Type`)  # consistent with plotting

energy_gen <- energy_gen %>%
    mutate(Year = as.numeric(Year))

trade_data <- trade_data %>%
    mutate(Year = as.numeric(Year))

renewable_sources <- renewable_sources %>%
    mutate(Year = as.numeric(Year))

#--------------------------------------------------------
# Shiny UI
#--------------------------------------------------------
ui <- fluidPage(
    titlePanel("Australian Energy Dashboard"),
    tabsetPanel(
        tabPanel("Generation Trends",
                 plotlyOutput("genTrendPlot"),
                 br(),
                 sliderInput("yearRangeGen", "",
                             min = min(energy_gen$Year),
                             max = max(energy_gen$Year),
                             value = c(min(energy_gen$Year), max(energy_gen$Year)),
                             step = 1, sep = "")
        ),
        tabPanel("Renewables Growth",
                 plotlyOutput("renewGrowthPlot"),
                 br(),
                 sliderInput("yearRangeRenew", "",
                             min = min(renewable_sources$Year),
                             max = max(renewable_sources$Year),
                             value = c(min(renewable_sources$Year), max(renewable_sources$Year)),
                             step = 1, sep = "")
        ),
        tabPanel("Fossil vs Renewable",
                 plotlyOutput("fossilRenewPlot"),
                 br(),
                 sliderInput("yearRangeFossil", "",
                             min = min(energy_gen$Year),
                             max = max(energy_gen$Year),
                             value = c(min(energy_gen$Year), max(energy_gen$Year)),
                             step = 1, sep = "")
        ),
        tabPanel("Fuel Mix by Region",
                 selectInput("region_select", "",
                             choices = unique(gen_fuelmix$Region),
                             selected = "NSW"),
                 plotlyOutput("fuelmixPlot")
        ),
        tabPanel("Renewables Overview",
                 plotlyOutput("renewablesOverviewPlot")
        ),
        tabPanel("Imports & Exports",
                 plotlyOutput("tradePlot"),
                 br(),
                 fluidRow(
                     column(6,
                            sliderInput("yearRangeTrade", "",
                                        min = min(trade_data$Year),
                                        max = max(trade_data$Year),
                                        value = c(min(trade_data$Year), max(trade_data$Year)),
                                        step = 1, sep = "")
                     ),
                     column(3,
                            checkboxInput("showGFC", "Show GFC Line", TRUE)
                     ),
                     column(3,
                            checkboxInput("showCOVID", "Show COVID Line", TRUE)
                     )
                 )
        )
    )
)

#--------------------------------------------------------
# Shiny Server
#--------------------------------------------------------
server <- function(input, output) {
    
    # Energy Generation Trend plot
    output$genTrendPlot <- renderPlotly({
        fuel_colors <- c(
            "Black coal"  = "#000000",
            "Brown coal"  = "#D2691E",
            "Oil and LPG" = "#E41A1C",
            "Natural gas" = "#1E90FF",
            "Renewables"  = "#00C853"
        )
        
        filtered_data <- energy_gen %>%
            filter(Year >= input$yearRangeGen[1], Year <= input$yearRangeGen[2])
        
        p <- ggplot(filtered_data, aes(x = Year, y = Energy_PJ, color = FuelType)) +
            geom_line(linewidth = 1.3) +
            scale_color_manual(values = fuel_colors) +
            scale_x_continuous(breaks = seq(1985, 2025, 5)) +
            labs(title = "Energy Generation by Fuel Type", x = "", y = "Energy (PJ)", color = "") +
            theme_minimal(base_size = 13) +
            theme(
                panel.grid = element_blank(),
                axis.line = element_line(color = "grey40"),
                axis.ticks = element_line(color = "grey40"),
                legend.position = "bottom"
            )
        ggplotly(p)
    })
    
    # Renewables Growth plot
    output$renewGrowthPlot <- renderPlotly({
        renewable_colors <- c(
            "Hydro"     = "#0077C8",
            "Wind"      = "#00BFC4",
            "Solar"     = "#FFD300",
            "Bioenergy" = "#00C853"
        )
        
        filtered_data <- renewable_sources %>%
            filter(Year >= input$yearRangeRenew[1], Year <= input$yearRangeRenew[2])
        
        p <- ggplot(filtered_data, aes(x = Year, y = Energy_PJ, color = Source)) +
            geom_line(linewidth = 1.3) +
            scale_color_manual(values = renewable_colors) +
            scale_x_continuous(breaks = seq(2000, 2025, 5)) +
            scale_y_continuous(labels = scales::comma) +
            labs(title = "Renewable Electricity Generation", x = "", y = "Energy (PJ)", color = "") +
            theme_minimal(base_size = 13) +
            theme(
                panel.grid = element_blank(),
                axis.line = element_line(color = "grey40"),
                axis.ticks = element_line(color = "grey40"),
                legend.position = "bottom"
            )
        ggplotly(p)
    })
    
    # Fossil vs Renewable Energy plot
    output$fossilRenewPlot <- renderPlotly({
        grouped_data <- energy_gen %>%
            mutate(Category = ifelse(FuelType == "Renewables", "Renewables", "Fossil Fuels")) %>%
            group_by(Year, Category) %>%
            summarise(Total_Energy = sum(Energy_PJ), .groups = "drop") %>%
            filter(Year >= input$yearRangeFossil[1], Year <= input$yearRangeFossil[2])
        
        category_colors <- c(
            "Fossil Fuels" = "#E41A1C",
            "Renewables"   = "#00C853"
        )
        
        p <- ggplot(grouped_data, aes(x = Year, y = Total_Energy, fill = Category)) +
            geom_area(alpha = 0.9, color = "white", linewidth = 0.3) +
            scale_fill_manual(values = category_colors) +
            labs(title = "Renewables vs Fossil Fuels", x = "", y = "Energy (PJ)", fill = "") +
            theme_minimal(base_size = 13) +
            theme(
                panel.grid = element_blank(),
                axis.line = element_line(color = "grey40"),
                axis.ticks = element_line(color = "grey40"),
                legend.position = "bottom"
            )
        ggplotly(p)
    })
    
    # Fuel Mix plot by region
    output$fuelmixPlot <- renderPlotly({
        data <- gen_fuelmix %>% filter(Region == input$region_select)
        fuel_colors <- c(
            "Coal"             = "#B22222",
            "Natural gas"      = "#1E90FF",
            "Oil"              = "#555555",
            "Hydro"            = "#00BFC4",
            "Other renewables" = "#00C853"
        )
        p <- ggplot(data, aes(x = FuelType, y = Percentage, fill = FuelType)) +
            geom_col(width = 0.7) +
            scale_fill_manual(values = fuel_colors) +
            labs(title = paste("Electricity Generation by Fuel Type:", input$region_select),
                 x = "", y = "Percentage (%)", fill = "") +
            theme_minimal(base_size = 13) +
            theme(
                panel.grid = element_blank(),
                axis.line = element_line(color = "grey40"),
                legend.position = "bottom"
            )
        ggplotly(p)
    })
    
    # Renewables Overview plot
    output$renewablesOverviewPlot <- renderPlotly({
        overview_data <- gen_fuelmix %>%
            group_by(Region) %>%
            summarise(TotalRenewables = sum(Percentage[FuelType %in% c("Hydro", "Other renewables", "Solar", "Wind", "Bioenergy")]))
        
        p <- ggplot(overview_data, aes(x = reorder(Region, -TotalRenewables), y = TotalRenewables, fill = Region)) +
            geom_col(show.legend = FALSE) +
            labs(title = "Renewable Energy Share by Region", x = "", y = "Energy (%)") +
            theme_minimal(base_size = 13) +
            theme(
                panel.grid = element_blank(),
                axis.line = element_line(color = "grey40")
            )
        ggplotly(p)
    })
    
    # Imports & Exports plot
    output$tradePlot <- renderPlotly({
        trade_colors <- c(
            "Exports"    = "#E41A1C",
            "Production" = "#00C853"
        )
        
        filtered_data <- trade_data %>%
            filter(Year >= input$yearRangeTrade[1], Year <= input$yearRangeTrade[2])
        
        forecast_data <- trade_data %>%
            filter(Year == 2024) %>%
            group_by(Supply) %>%
            summarise(Quantity_PJ = mean(Quantity_PJ), .groups = "drop") %>%
            mutate(Year = 2024, Year_next = 2027, Quantity_PJ_next = Quantity_PJ * 1.05) %>%
            pivot_longer(cols = c(Quantity_PJ, Quantity_PJ_next), names_to = "type", values_to = "Quantity_PJ") %>%
            mutate(Year = ifelse(type == "Quantity_PJ", 2024, 2027)) %>%
            select(Supply, Year, Quantity_PJ)
        
        p <- ggplot(filtered_data, aes(x = Year, y = Quantity_PJ, color = Supply)) +
            geom_line(linewidth = 1.3) +
            geom_line(data = forecast_data, aes(x = Year, y = Quantity_PJ, color = Supply),
                      linetype = "dashed", linewidth = 1, alpha = 0.7) +
            scale_color_manual(values = trade_colors) +
            labs(title = "Energy Import/Export Trends (with Forecast)",
                 x = "", y = "Quantity (PJ)", color = "") +
            theme_minimal(base_size = 13) +
            theme(
                panel.grid = element_blank(),
                axis.line = element_line(color = "grey50"),
                legend.position = "bottom"
            )
        
        if (input$showGFC) {
            p <- p +
                geom_vline(xintercept = 2008, linetype = "dotted", color = "blue", linewidth = 0.6) +
                annotate("text", x = 2010.2, y = max(filtered_data$Quantity_PJ) * 1.01,
                         label = "Global Financial Crisis", hjust = 0, color = "blue", size = 3, fontface = "bold")
        }
        if (input$showCOVID) {
            p <- p +
                geom_vline(xintercept = 2020, linetype = "dotted", color = "red", linewidth = 0.6) +
                annotate("text", x = 2022.1, y = max(filtered_data$Quantity_PJ) * 1.01,
                         label = "COVID-19 Pandemic", hjust = 0, color = "red", size = 3, fontface = "bold")
        }
        
        ggplotly(p)
    })
}

#--------------------------------------------------------
# Run the Shiny app
#--------------------------------------------------------
shinyApp(ui, server)
