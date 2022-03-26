library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(reshape2)
library(shinyWidgets)
library(dttr2)
library(plotly)
library(DT)

ret <- read_csv('./output/Plot_Data.csv') %>% 
    mutate(Date = as.yearmon(Date))
annual_ret <- read_csv('./output/Annualized_Return.csv')
portfolio_metrics <- read_csv('./output/Portfolio_Metrics.csv')
factors <- colnames(ret %>% 
                        select(Net_Assets:RF))
date_col <- as.yearmon(unique(ret[['Date']]))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Fundamental Indexing - An Alternative To Market Capitalization Portfolios"),
    
    # Sidebar
    sidebarLayout(position = "left",
                  sidebarPanel(
                      # Select dropdown with the factor being considered
                      selectInput(
                          "factors",
                          "Factor: ",
                          factors,
                          selected = factors[1:length(factors)],
                          multiple = TRUE,
                          selectize = TRUE,
                          width = NULL,
                          size = NULL
                      ),
                      sliderTextInput("yearmon_range", "Year Range",
                                      choices = date_col,
                                      selected = c(min(date_col),max(date_col)),
                                      grid = F,
                                      width = "100%")
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                      tabsetPanel(type = "tab",
                                  tabPanel("Data", dataTableOutput("data")),
                                  tabPanel("Annual Returns", plotlyOutput("annualReturnsPlot")),
                                  tabPanel("Cumulative Returns", plotlyOutput("cumulativeReturnsPlot")),
                                  tabPanel("Portfolio Metrics 1975 to 2020", dataTableOutput("portfolio_metrics"))
                      )
                  )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Function to shade Recession Months
    recession_shade_fun <- function(df, start_date = input$yearmon_range[1], color_chosen = "darkgray"){ 
         df %>% 
            select(Date, USREC) %>% 
            mutate(recession_label = case_when(USREC == 1 & lag(USREC == 0) ~ str_glue("{Date} recession"), 
                                               TRUE ~ NA_character_)) %>% 
            filter(USREC == 1) %>% 
            fill(recession_label, .direction = "down") %>% 
            group_by(recession_label) %>% 
            slice(1, n()) %>% 
            select(-USREC) %>% 
            mutate(start = case_when(Date == min(Date) ~ Date, TRUE ~ as.yearmon(NA)),
                   end = case_when(Date == max(Date) ~ Date, TRUE ~ as.yearmon(NA)),
                   end = lead(end)
            ) %>% 
            filter(!is.na(start)) %>% 
            select(-Date) %>% 
            geom_rect(data = ., 
                      inherit.aes = F, 
                      aes(xmin = start, 
                          xmax = end, 
                          ymin = -Inf, 
                          ymax = +Inf), 
                      fill = color_chosen, 
                      alpha = 0.4)
    }
    
    # Function to Compute Cumulative Returns
    computeCumulativeReturns <- function(port_ret_df){
        port_cum_ret_df <- port_ret_df %>%
            filter(Date >= input$yearmon_range[1], Date <= input$yearmon_range[2]) %>%
            mutate(across(c(-Date), .f = list(cum = function(x) {cumprod(1+x)} )))
        
        # Modifying ugly column names
        port_cum_ret_df <- port_cum_ret_df %>% 
            select(Date, contains('cum'))
        colnames(port_cum_ret_df)<-gsub("_cum","",colnames(port_cum_ret_df))
        
        return(port_cum_ret_df)
    }
    
    # Table of Returns
    output$data <- renderDataTable({
        df <- ret %>% 
            select(Date, input$factors) %>% 
            filter(Date >= input$yearmon_range[1], Date <= input$yearmon_range[2]) %>% 
            mutate(Date = as.character(as.yearmon(Date)))
        
        num_cols <- names(dplyr::select_if(df,is.numeric))
        
        datatable(df) %>%
        formatRound(columns=num_cols, digits=3)
    })
    
    # Table of Portfolio Metrics
    output$portfolio_metrics <- renderDataTable({
        df <- portfolio_metrics
        
        num_cols <- names(dplyr::select_if(df,is.numeric))
        
        datatable(df) %>%
        formatRound(columns=num_cols, digits=3)
    })
    
    # Annual Returns
    output$annualReturnsPlot <- renderPlotly({
        d <- annual_ret %>% 
            rename(Date = Year) %>% 
            pivot_longer(c(-Date), names_to = "Factor", values_to = "Returns")
        
        plot_df <- subset(d, Factor %in% input$factors) %>%
            filter(Date >= year(as.yearmon(input$yearmon_range[1])), Date <= year(as.yearmon(input$yearmon_range[2])))

        p <- plot_df %>%
            ggplot() +
            geom_line(aes(x=Date, y=Returns, color = Factor)) +
            theme_bw() +
            theme(panel.border = element_blank()) +
            ggtitle("Annual Returns") +
            xlab('Date') +
            ylab("Returns") +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p) %>% layout(height = 650)
    })
    
    # Cumulative Returns
    output$cumulativeReturnsPlot <- renderPlotly({
        
        cumu_ret <- computeCumulativeReturns(ret)
        
        data2 <- cumu_ret
        
        d <- data2 %>%
            pivot_longer(c(-Date), names_to = "Factor", values_to = "Portfolio_Value") %>% 
            inner_join(ret[c('Date', 'USREC')], by = c('Date'))
        
        plot_df <- subset(d, Factor %in% input$factors) %>%
            filter(Date >= input$yearmon_range[1], Date <= input$yearmon_range[2])
        
        p <- plot_df %>% 
            ggplot() +
            geom_line(aes(x=Date, y=Portfolio_Value, color = Factor)) +
            recession_shade_fun(plot_df) +
            theme_bw() +
            theme(panel.border = element_blank()) +
            ggtitle("Cumulative Returns") +
            xlab('Date') +
            ylab("Cumulative Returns") +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "bottom",
                  axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p) %>% layout(height = 650)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
#library(rsconnect)
#rsconnect::deployApp('<path to app>')

