library(shiny)
library(ggiraph)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Breathe Baltimore Air Quality Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your data file here (.TXT). 
                Processing time will take 2-3 minutes.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("PM2.5",
                 HTML("<br>"),
                 
                 HTML("
              Particulate Matter (PM) 2.5 = very small inhalable mixture of solid particles and liquid droplets found in the air that are <2.5 microns in size.
              There are two sensors measuring PM 2.5 in (µg/m³) at the site, and we are calculating the average of the two for greater accuracy.
                      
                      <br><br>"),
                 uiOutput("pm25_unhealthy_days_output"),
                 girafeOutput("interactive_pm25_plot"),
                 DT::dataTableOutput("pm25_table"),
                 downloadButton("download_pm25", "Download Hourly PM2.5 Data"),
                 br(),
                 br(),
                 DT::dataTableOutput("pm25_table_daily"),
                 downloadButton("download_pm25_daily", "Download Daily PM2.5 Data")),
        tabPanel("PM10",
                 HTML("<br>"),
                 #uiOutput("pm10_unhealthy_days_output"), # Use uiOutput for dynamic rendering
                 HTML("
              Particulate Matter (PM) 10 = very small inhalable mixture of solid particles and liquid droplets found in the air that are <10 microns in size.
              There are two sensors measuring PM 10 in (µg/m³) at the site, and we are calculating the average of the two for greater accuracy.
                      
                      <br><br>"),
                 uiOutput("pm10_unhealthy_days_output"),
                 girafeOutput("interactive_pm10_plot"),
                 DT::dataTableOutput("pm10_table"),
                 downloadButton("download_pm10", "Download Hourly PM10 Data"),
                 br(),
                 br(),
                DT::dataTableOutput("pm10_table_daily"),
                downloadButton("download_pm10_daily", "Download Daily PM10 Data")),
        tabPanel("Ozone",
                 HTML("<br>"),
                 #uiOutput("ozone_unhealthy_days_output"), # Use ui Output for dynamic rendering
                 HTML("
              Ozone = a reactive gas composed of three oxygen molecules (O3) found in the air.
              There is one sensor measuring ozone in parts per million (ppm) at the site.
                      
                      <br><br>"),
                 uiOutput("ozone_unhealthy_days_output"), 
                 girafeOutput("interactive_ozone_plot"),
                 DT::dataTableOutput("ozone_table"),
                 downloadButton("download_ozone", "Download Hourly Ozone Data"),
                 br(),
                 br(),
                 DT::dataTableOutput("ozone_table_daily"),
                 downloadButton("download_ozone_daily", "Download Daily Ozone Data")),
        tabPanel("CO₂", 
                 HTML("<br>"),
                 HTML("
              Carbon Dioxide (CO₂) = a colorless and odorless gas produced by respiration, combustion, and other processes. CO₂ is a major greenhouse gas contributing to climate change.
              There is one sensor measuring CO₂ in PPM at the site. 
              <br>
              *Carbon dioxide is not a standard air pollutant, and the EPA has not created a formula to convert it to AQI.*
                      
                      <br><br>"),
                 girafeOutput("interactive_k30_plot"),
                 DT::dataTableOutput("co2_table"),
                 downloadButton("download_co2", "Download Hourly CO₂ Data"),
                 br(),
                 br(),
                 DT::dataTableOutput("co2_table"),
                 downloadButton("download_co2_daily", "Download Daily CO₂ Data")),
          tabPanel("AQI", 
                   fluidRow(
                     column(12,
                            h3("Air Quality Index (AQI)"),
                            p("The Air Quality Index is a governmental tool that reports outdoor air quality conditions each day. Each of the 6 categories has a number range and a color."),
                            p("Like a rule measuring 0 - 500, higher numbers indicate greater potential health risks."),
                            br(),
                            h4("AQI Scale and Categories"),
                            DTOutput("aqi_table"),
                            br(),
                            h4("What does AQI tell me?"),
                            p("When the government reports AQI, the AQI number refers to just one pollutant across a city. The most common urban pollutants measured are PM2.5 and ozone. Be sure to check which pollutant the AQI is referring to."),
                            p("For this project, we use the AQI scale and conversion charts to convert concentrations of each pollutant measured by the sensors into AQI categories."),
                            br(),
                            h4("Want to learn more?"),
                            p("Check out our project page ", a("here", href = "https://www.ejji.org/air-quality-monitoring-in-baltimore")),
                            br(),
                            br(),
                            br(),
                            br()
                     )
                   )),
        tabPanel("Project Info", 
                 h3("Breathe Baltimore Air Quality Monitoring Data"),
                 p("Breathe Baltimore is a community-based research project addressing air quality issues in Baltimore."),
                 p("Environmental Justice Journalism Initiative (EJJI) and Smithsonian Environmental Research Center (SERC) are co-leading the project together."),
                 br(),
                 h4("What is this data?"),
                 p("Summer and Fall 2024, we are deploying 15 air quality monitoring sensors around Baltimore to measure three pollutants: particulate matter, ozone, and carbon dioxide."),
                 p("For particulate matter, we are measuring two particle sizes (2.5 and 10 microns) and have two sensors for each to increase the accuracy of our measurements."),
                 p("The sensors are measuring these air pollutants in the air immediately around the sensors every 5 minutes, every day."),
                 br(),
                 h4("How is the data being used?"),
                 p("This is the first time we have put the sensors out in the world! Right now, the data is being evaluated and looked at by the Technology in Ecology Lab to make sure the sensors are working correctly."),
                 p("We are making the data collected by these sensors publicly available to anyone who wants to use it."),
                 p("Our goal is to make the data from each sensor available as quickly as possible."),
                 br(),
                 h4("How could the data be used?"),
                 p("The data are hyperlocal in places where there isn't currently outdoor air quality data."),
                 p("These data are not regulatory, decision-making, legally defensible data."),
                 p("Potential uses of the data include the following:"),
                 tags$ul(
                   tags$li("Hotspot identification"),
                   tags$li("Help prioritize future data collection"),
                   tags$li("Display patterns/trends in outdoor air quality in the neighborhood"),
                   tags$li("Support community members' experiences and stories"),
                   tags$li("Education")
                 ),
                 br(),
                 h4("How should I cite these data?"),
                 p("Please reference the project, Breathe Baltimore, and the two organizations who are co-leading it: Environmental Journalism Justice Initiative and the Smithsonian Environmental Research Center."),
                 p("Please also be sure to note that the data are pilot-testing data."),
                 br(),
                 h4("I have a question. Who should I contact?"),
                 p("Email the project team ", a(href = "mailto:breathebaltimoreproject@gmail.com", "breathebaltimoreproject@gmail.com"), " with any data questions."),
                 br(),
                 br(),
                 br()
        )
      )
    )
  )
)
