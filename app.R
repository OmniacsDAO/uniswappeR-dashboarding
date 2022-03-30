## Load Libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(grid)
library(ggnetwork)
library(scales)
library(magick)
library(shiny)
library(shinythemes)
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(uniswappeR)
library(thematic)
library(shinyjs)
#library(shinyBS)
library(bsplus)

## Load Uniswap Pair Info Dataset
uni <- readRDS("pairs_info.rds") %>%
    as_tibble() %>%
    mutate(across(where(is.character), readr:::parse_guess))

## Remove Outlier
uni <- uni[!(uni$Pr_Token1_Ticker %in% c("LAWA")),]
uni <- uni[!(uni$Pr_Token0_Ticker %in% c("LAWA")),]

## Get the day of infographic and split data set to the new one
current_time <- Sys.time()-hours(1)
attr(current_time,"tzone") <- "GMT"
current_date <- as_date(current_time)
uni$Pr_Date <- as_date(uni$Pr_Created)
new <- uni[uni$Pr_Date == current_date,]
uni <- uni[uni$Pr_Date <= current_date,]

ui <- fluidPage(theme = bs_theme(bootswatch = "quartz", version = 5),
                
                useShinyjs(),
                
                # Application title
                titlePanel(tags$img(src="uniswap_logo_nocorn.png", width = 200), windowTitle = "Uniswap Dashboard"),
                
                sidebarLayout(
                    sidebarPanel(
                        h4("Introduction"),
                        
                        helpText("Welcome to our Prototype Uniswap Trading Dashboard! Here we interface with the `uniswappeR` package to help visualize market data and make trading decisions. Options for your output result will appear below, if available."),
                        hr(),
                        h4("Options"),
                        #conditionalPanel(condition = "input.tabs1 == ''")
                        shinyjs::hidden(textInput("token_address", "Token Address", value = "0x1f9840a85d5af5bf1d1762f925bdaddc4201f984")),
                        shinyjs::hidden(textInput("pair_address", "Pair Address", value = "0xf00e80f0de9aea0b33aa229a4014572777e422ee")),
                        helpText("Input your address, and the visualizations on the User Swap Performance tab will update!"),
                        textInput("user_address", "User Address", value = "0x4d9c274ADF71e4201B4aB1f28BF05D44eE4bA261"),
                        hr(),
                        helpText("Data Updated: 03/25/2022")
                        #shinyInput_label_embed(
                        #  shiny_iconlink() %>%
                        #  bs_embed_popover(
                        #    title = "Letter", content = "Choose a favorite", placement = "right"
                        #  )
                        #)
                        #bsPopover("user_address", "you can choose whatever you want", "bottom")
                    ),
                    
                    mainPanel(
                        tabsetPanel(id="tabs1",
                                    # tabPanel("Dashboard",
                                    #          #tableOutput("dashboard"),
                                    #          fluidRow(
                                    #              column(width=4,
                                    #                     valueBox(
                                    #                         uiOutput("average_contracts_deployed"), "Average Daily Contracts Deployed", icon = icon("group"), width = 12
                                    #                     ),
                                    #                     align = "center"),
                                    #              column(width=4,
                                    #                     valueBox(
                                    #                         uiOutput("median_contracts_deployed"), "Median Daily Contracts Deployed", icon = icon("group"), width = 12
                                    #                     ),
                                    #                     align = "center")
                                    #          ),
                                    #          fluidRow(
                                    #              column(width=4,
                                    #                     valueBox(
                                    #                         uiOutput("average_liquidity"), "Average Amount of Liquidity", icon = icon("group"), width = 12
                                    #                     ),
                                    #                     align = "center"),
                                    #              column(width=4,
                                    #                     valueBox(
                                    #                         uiOutput("median_liquidity"), "Median Amount of Liquidity", icon = icon("calendar"), width = 12
                                    #                     ),
                                    #                     align = "center"),
                                    #          ),
                                    #          fluidRow(
                                    #              column(width=4,
                                    #                     valueBox(
                                    #                         uiOutput("average_daily_liquidity"), "Average Daily Liquidity", icon = icon("calendar"), width = 12
                                    #                     ),
                                    #                     align = "center"),
                                    #              column(width=4,
                                    #                     valueBox(
                                    #                         uiOutput("median_daily_liquidity"), "Median Daily Liquidity", icon = icon("dollar"), width = 12
                                    #                     ),
                                    #                     align = "center")
                                    #          )
                                    # ),
                                    
                                    tabPanel("Platform Growth",
                                             withSpinner(plotOutput("platform_growth", height = "600px"))
                                    ),
                                    tabPanel("Token Growth",
                                             withSpinner(plotOutput("token_growth", height = "600px"))
                                    ),
                                    tabPanel("Token Pair Growth",
                                             withSpinner(plotOutput("token_pair_growth", height = "600px"))   
                                    ),
                                    tabPanel("Pair Growth",
                                             withSpinner(plotOutput("pair_growth", height = "600px"))
                                    ),
                                    tabPanel("Liquidity Token Distribution",
                                             withSpinner(plotOutput("liquidity_token_distribution", height = "600px"))       
                                    ),
                                    tabPanel("User Swap Performance",
                                             withSpinner(plotOutput("swap_viz", height = "600px"))
                                             #plotOutput("swap_perf")
                                    )
                        )
                    )
                )
)

server <- function(input, output) {
    
    output$average_contracts_deployed <- renderText({
        avg_cont <- uni %>%
            mutate(Date = as_date(Pr_Created)) %>%
            group_by(Day = day(Date), Month = month(Date), Year = year(Date)) %>%
            summarise(Count = n()) %>%
            ungroup() %>%
            summarise(`Average Daily Count` = mean(Count)) %>%
            pull(`Average Daily Count`)
        
        round(avg_cont)
    })
    
    output$median_contracts_deployed <- renderText({
        avg_cont <- uni %>%
            mutate(Date = as_date(Pr_Created)) %>%
            group_by(Day = day(Date), Month = month(Date), Year = year(Date)) %>%
            summarise(Count = n()) %>%
            ungroup() %>%
            summarise(`Average Daily Count` = median(Count)) %>%
            pull(`Average Daily Count`)
        
        round(avg_cont)
    })
    
    output$average_liquidity <- renderText({
        avg_liq <- uni %>%
            summarise(`Average Liquidity` = mean(Pr_Liquidity_USD)) %>%
            pull(`Average Liquidity`)
        
        scales::dollar(avg_liq)
    })
    
    output$median_liquidity <- renderText({
        med_liq <- uni %>%
            summarise(`Average Liquidity` = median(Pr_Liquidity_USD)) %>%
            pull(`Average Liquidity`)
        
        scales::dollar(med_liq)
    })
    
    output$average_daily_liquidity <- renderText({
        daily_liq <- uni %>%
            mutate(Date = as_date(Pr_Created)) %>%
            group_by(Day = day(Date), Month = month(Date), Year = year(Date)) %>%
            summarise(Liq = sum(Pr_Liquidity_USD)) %>%
            ungroup() %>%
            summarise(`Average Daily Liquidity` = mean(Liq)) %>%
            pull(`Average Daily Liquidity`)
        
        scales::dollar(daily_liq)
    })
    
    output$median_daily_liquidity <- renderText({
        daily_liq_med <- uni %>%
            mutate(Date = as_date(Pr_Created)) %>%
            group_by(Day = day(Date), Month = month(Date), Year = year(Date)) %>%
            summarise(Liq = sum(Pr_Liquidity_USD)) %>%
            ungroup() %>%
            summarise(`Average Daily Liquidity` = median(Liq)) %>%
            pull(`Average Daily Liquidity`)
        
        scales::dollar(daily_liq_med)
    })
    
    output$platform_growth <- renderPlot({
        vis_uniswap_stats_hist_v2()
    })
    
    output$token_growth <- renderPlot({
        vis_token_stats_hist_v2(token_address = input$token_address)
    })
    
    output$token_pair_growth <- renderPlot({
        vis_token_pair_map_v2(token_address = input$token_address)
    })
    
    output$pair_growth <- renderPlot({
        vis_pair_stats_hist_daily_v2(pair_address = input$pair_address)
    })
    
    output$liquidity_token_distribution <- renderPlot({
        vis_pair_liq_positions_v2(pair_address = input$pair_address)
    })
    
    swap_data <- reactive({
        swap_data <- swaps(input$user_address)
        
        return(swap_data)
    })
    
    output$swap_viz <- renderPlot({
        swap_visualizations(swap_data())
    })
    
    output$swap_perf <- renderPlot({
        swap_performance(swap_data())
    })
    
}

# Run the application 
thematic_shiny()
shinyApp(ui = ui, server = server)