library(shiny)
library(tidyverse)
library(plotly)
data <- read.csv("owid-co2-data.csv")
data <- data %>% filter(!is.na(co2), !is.na(gdp))
min_year <- min(data$year)
max_year <- max(data$year)
n_country <- length(unique(data$country))
max_co2 <- max(data$co2)
min_co2 <- min(data$co2)
avg_co2 <- round(mean(data$co2),2)
max_gdp <- max(data$gdp)
min_gdp <- min(data$gdp)
avg_gdp <- round(mean(data$gdp), 2)
countries <- unique(data$country)
ui <- navbarPage("Climate Change Analysis",
  tabPanel("Introductory",
      p("The dataset is a collection of key metrics maintained by Our World in Data. 
                It is updated regularly and includes data on CO2 emissions
                (annual, per capita, cumulative and consumption-based), 
                other greenhouse gases, energy mix, and other relevant metrics. 
                The dataset contains three sources:"),
      tags$ul(
        tags$li("CO2 emissions: this data is sourced from the Global Carbon Project. The Global Carbon Project typically releases a new update of CO2 emissions annually."),
        tags$li("Greenhouse gas emissions (including methane, and nitrous oxide): this data is sourced from the CAIT Climate Data Explorer, and downloaded from the Climate Watch Portal."),
        tags$li("Energy (primary energy, energy mix and energy intensity): this data is sourced from a combination of two sources. The BP Statistical Review of World Energy is published annually, but it does not provide data on primary energy consumption for all countries. For countries absent from this dataset, we calculate primary energy by multiplying the World Bank, World Development Indicators metric Energy use per capita by total population figures. The World Bank sources this metric from the IEA.")
      ),
      p("We will focus on the following four variables:"),
      tags$ul(
        tags$li("year"),
        tags$li("country"),
        tags$li("co2"),
        tags$li("gdp")
      ),
      p(paste0("After filtering NA values, the dataset contains ", nrow(data), " observations, and ",
               ncol(data), " columns, the data ranges from ", min_year, " to ", max_year, ", the avearge 
               co2 emission is ", avg_co2, " million tons, the avearge GDP is ", 
               avg_gdp, ", the maximum co2 emission is ", max_co2, " million tons, the minimum co2 emission is ",
               min_co2))
      
  ),
  tabPanel("Visualization",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "year",
                    label = "Year",
                    min = min_year,
                    max = max_year,
                    value = c(min_year, max_year)),
        selectInput("country", label="Country",
                    choices = countries,
                    selected="China"),
        
        checkboxInput("fitted", "Add Fitted Line", value=T)
      ),
      mainPanel(
        plotlyOutput(outputId = "plot"),
        p("I created this chart because I want to explore the relationship between the CO2 emission and GDP for a country, 
          and find that they are positively linear associated.")
      )
  )),
  tabPanel("Value Sensitive Design",
           h2("Indirect Stakeholders"),
           tags$ul(
             tags$li("mayor: the mayor may concerns about how the CO2 will affect the GDP"),
             tags$li("officer: the officer wants to increase the GPD"),
             tags$li("Factory: the factory ower may want to reduce the co2 emission")
           ))
)


server <- function(input, output) {
  output$plot <- renderPlotly({
    dat <- data %>% filter(year>=input$year[1] & year<=input$year[2], country==input$country)
    p <- ggplot(dat, aes(x=co2, y=gdp)) + geom_point() +
      labs(x="Annual production-based emissions of co2(million tonnes)",
           y="Gross domestic product($)",
           title="Gross domestic product vs annual emissions of co2")
    if(input$fitted){
      p <- p +geom_smooth(method="lm", se=F, color="red") 
    }
    p
  })
}

shinyApp(ui = ui, server = server)