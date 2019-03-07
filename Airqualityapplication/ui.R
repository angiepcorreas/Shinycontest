library(shiny)
library(shinydashboard)
library(markdown)
library(knitr)
library(dplyr)

load(file="entorno.RData")

# Header ------------------------------------------------------------------


header <- dashboardHeader(title = 'Air Quality - Aburrá Valley', titleWidth = 400,
                          tags$li(a(href = 'https://siata.gov.co/siata_nuevo/',
                                    img(src = 'https://raw.githubusercontent.com/angiepcorreas/Shinycontest/master/SIATA.png', 
                                        title = "Clic to visit the SIATA website",
                                        height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"
                          ),
                          class = "dropdown"),
                          
                          tags$li(a(href = 'https://www.metropol.gov.co/ambiental/calidad-del-aire/Paginas/calidad-del-aire.aspx',
                                    img(src = 'https://raw.githubusercontent.com/angiepcorreas/Shinycontest/master/arealogo.jpg', 
                                        title = "Clic to visit the Área Metropolitana website",
                                        height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"
                          ),
                          class = "dropdown")
                          
)

# Sidebar Menu ------------------------------------------------------------



sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                                menuItem("Introduction", tabName = "intro", icon = icon("align-justify")),
                                menuItem("Data", tabName = "data", icon = icon("table")),
                                menuItem("Descriptive analysis", tabName = "descriptive", icon = icon("bar-chart-o")),
                                menuItem("About", tabName = "about", icon = icon("info-circle")),
                                hr(),
                                sidebarUserPanel(name = a("ANGIE CORREA", target = "_blank_",
                                                          href = "https://angiepcorreas.github.io/"), 
                                                 subtitle = "Industrial Engineering Student",
                                                 image = "https://pbs.twimg.com/profile_images/378800000532546226/dbe5f0727b69487016ffd67a6689e75a_400x400.jpeg"),
                                sidebarUserPanel(name = a("OLGA USUGA", target = "_blank_",
                                                          href = "https://angiepcorreas.github.io/"), 
                                                 subtitle = "Asociated professor at UdeA",
                                                 image = "https://pbs.twimg.com/profile_images/378800000532546226/dbe5f0727b69487016ffd67a6689e75a_400x400.jpeg"),
                                hr(),
                                menuItem("Source code", icon = icon("file-code-o"), 
                                         href = "https://github.com/angiepcorreas/Shinycontest")
                            )                           
)


# Body section ------------------------------------------------------------

body <- dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    tags$head(tags$style(HTML('
      .main-sidebar {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 13px;
      }
    '))),
    
    tags$head(tags$style(HTML("
        .tabbable > .nav > li > a[data-value='PM 2.5'] {color:black}
        .tabbable > .nav > li > a[data-value='Other variables'] {color:black}"))
    ),
    
    tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
    
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green; border-top: 2px green; border-bottom: 2px green;border-left: 2px green}")),
    
    tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:#CFFDAF}")),
    
    tabItems(
        tabItem("intro", includeHTML("Introduction.html")), 
        
        tabItem("data",
                h3(tags$b("The data used to make the statistical analysis is below and it
                 was extracted from the databases of SIATA:")),
                dataTableOutput("datatable")
        ),
        tabItem("descriptive", 
                tabsetPanel(
                    tabPanel("PM 2.5", icon = icon("cloudversify"),
                             
                             fluidRow(
                                 valueBox(round(mean(BDcompleta$Pm25[BDcompleta$Month == "November"]),3),"Average PM 2.5  in November",icon = icon("thumbs-up"), color = ifelse(mean(BDcompleta$Pm25[BDcompleta$Month == "November"]) > 35.5,"yellow", "green"), width = 4),
                                 valueBox(round(mean(BDcompleta$Pm25[BDcompleta$Month == "December"]),3),"Average PM 2.5  in December",icon = icon("thumbs-up"), color = ifelse(mean(BDcompleta$Pm25[BDcompleta$Month == "December"]) > 35.5,"yellow", "green"), width = 4),
                                 valueBox(round(mean(BDcompleta$Pm25[BDcompleta$Month == "January"]),3),"Average PM 2.5  in January",icon = icon("thumbs-up"), color = ifelse(mean(BDcompleta$Pm25[BDcompleta$Month == "January"]) > 35.5,"yellow", "green"), width = 4)
                             ),
                             
                             fluidRow(
                                 valueBox(round(mean(BDcompleta$Pm25[BDcompleta$Month == "February"]),3),"Average PM 2.5  in February",icon = icon("warning"), color = ifelse(mean(BDcompleta$Pm25[BDcompleta$Month == "February"]) > 35.5,"yellow", "green"), width = 4),
                                 valueBox(round(mean(BDcompleta$Pm25[BDcompleta$Month == "March"]),3),"Average PM 2.5  in March",icon = icon("warning"), color = ifelse(mean(BDcompleta$Pm25[BDcompleta$Month == "March"]) > 35.5,"yellow", "green"), width = 4),
                                 valueBox(round(mean(BDcompleta$Pm25[BDcompleta$Month == "April"]),3),"Average PM 2.5  in April",icon = icon("thumbs-up"), color = ifelse(mean(BDcompleta$Pm25[BDcompleta$Month == "April"]) > 35.5,"yellow", "green"), width = 4)
                                 
                             ),
                             
                             fluidRow(
                                 column(width = 8,
                                        fluidRow(box(title = tags$b("PM 2.5 concentration by month"), width = 12, status = "success", solidHeader = TRUE,plotOutput("boxplotmonth")))
                                 ),
                                 
                                 column(width = 4,
                                        fluidRow(box(title = tags$b("Boxplot control box"), width = 12, status = "success", solidHeader = FALSE ,checkboxGroupInput("area","Select the city area to be displayed:", choices = c("Center", "South"), inline = TRUE, selected = c("Center", "South")),
                                                     br(), checkboxGroupInput("month", label = "Select the months to be displayed:", 
                                                                              choices = c(levels(BDcompleta$Month)), selected = c("April", "December", "February", "January", "March", "November")))
                                        ),
                                        fluidRow(valueBox(h3("Type of stations"), "The SIATA currently has two type of monitoring stations: poblacional and traffic.", width = 12, color = "red", icon = icon("info")))
                                 )
                             ),
                             
                             fluidRow(
                                 column(width = 8,
                                        fluidRow(box(title = tags$b("PM 2.5 concentration by hour"), width = 12, status = "success", solidHeader = TRUE, plotOutput("boxplothour"))
                                        )
                                 ),
                                 
                                 column(width = 4,
                                        fluidRow(box(title = tags$b("Boxplot control box"), width = 12, status = "success", solidHeader = FALSE, sliderInput("hour", "Select the hours to be displayed:", dragRange = TRUE,
                                                                                                                                                             min = 0, max = 23, value = c(2,5)))
                                        ),
                                        fluidRow(valueBox(h3("Critical hours"), "In the time lapse from 7 am to 10 am, there is a higher concentration of PM 2.5 pollutant particles recorded by monitoring stations.
                                    This high concentrations can be explained by the effects of solar radiation, which still do not warm the atmospheric surface at this time lapse and therefore 
                                    prevent airborne pollutants from rising and disappearing.", width = 12, color = "red", icon = icon("info"))
                                        )
                                 )
                             ),
                             
                             fluidRow(
                                 column(width = 8,
                                        fluidRow(box(title = tags$b("PM 2.5 densitys by month"), width = 12, status = "success", solidHeader = TRUE, plotOutput("densitymonths"))
                                        )
                                 ),
                                 
                                 column(width = 4,
                                        fluidRow(box(title = tags$b("Density chart control box"), width = 12, status = "success", solidHeader = FALSE , checkboxGroupInput("months2", "Select the months to be displayed:",
                                                                                                                                                                           choices = c(levels(BDcompleta$Month)), 
                                                                                                                                                                           selected = c(levels(BDcompleta$Month))))
                                        ),
                                        fluidRow(valueBox(h3("Critical months"), "November, February and March are the critical months where the PM 2.5 pollutant 
                                    concentration is highter, and this is due to the transition from the dry season to the rainy season
                                    and pollutants accumulate in the atmosphere because of layers of clouds positioned at low altitudes.", width = 12, color = "red", icon = icon("info"))
                                        ) 
                                 )
                             )
                    ),
                    tabPanel("Other variables", icon = icon("cloud"), 
                             
                             h2(tags$b("Factors that increase pollution in the Aburra Valley:")),
                             
                             fluidRow(valueBox(h3("Topography"), "A narrow and semi-closed valley.", width = 4, color = "red", icon = icon("image")),
                                      valueBox(h3("Meteorology"), "Atmospheric stability and low ventilation.", width = 4, color = "red", icon = icon("cloudversify")),
                                      valueBox(h3("Emissions"), "Pollution in a densely populated region.", width = 4, color = "red", icon = icon("bus"))
                             ),
                             
                             h2(tags$b("Average measure of every variable:")),
                             
                             fluidRow(
                                 valueBox(round(mean(BDcompleta$Pm25),3), "Average PM 2.5 (μg/m^3)",icon = icon("info"), color = "green", width = 4),
                                 valueBox(round(mean(BDcompleta$Precipitation),3),"Average Precipitation (mm)",icon = icon("info"), color = "green", width = 4),
                                 valueBox(round(mean(BDcompleta$Wind_direction),3),"Average Wind direction (°)",icon = icon("info"), color = "green", width = 4)
                             ),
                             
                             fluidRow(
                                 valueBox(round(mean(BDcompleta$Wind_speed),3), "Average Wind speed (m/s)",icon = icon("info"), color = "yellow", width = 4),
                                 valueBox(round(mean(BDcompleta$Air_temperature),3),"Average Air temperature (°C)",icon = icon("info"), color = "yellow", width = 4),
                                 valueBox(round(mean(BDcompleta$Air_humedity),3),"Average Air humedity (%)",icon = icon("info"), color = "yellow", width = 4)
                             ),
                             
                             fluidRow(
                                 column(width = 8,
                                        fluidRow(box(title = tags$b("Plot for variables"), width = 12, status = "success", solidHeader = TRUE, uiOutput("plot")))
                                 ),
                                 
                                 column(width = 4,
                                        fluidRow(box(title = tags$b("Control box for the plot"), width = 12, status = "success", solidHeader = FALSE, 
                                                     selectInput("plottype", "Select the plot type:", list(Histogram = "histogram", Boxplot = "boxplot")),
                                                     selectInput("variable", "Select the cuantitative variable to plot:", choices = names(cuantis)),
                                                     
                                                     conditionalPanel(condition = "input.plottype == 'boxplot'",
                                                                      selectInput("variable2", "Select a cualitative variable:", choices = names(cualis))
                                                     )
                                        )
                                        ),
                                        
                                        fluidRow(valueBox(h3("Measures:"), "Traffic control and fines for factories are some measures taken by the goverment to reduce the pollution.", width = 12, color = "red", icon = icon("info"))
                                        )
                                 )
                             ), 
                             
                             fluidRow(
                                 column(width = 8,
                                        fluidRow(box(title = tags$b("Correlations and histograms for the variables"), width = 12, status = "success", solidHeader = TRUE, plotOutput("correlations"))
                                        )
                                 ),
                                 
                                 column(width = 4,
                                        fluidRow(box(title = tags$b("Control box for the correlation graph"), width = 12, status = "success", solidHeader = FALSE, selectInput("var.corr", "select the variables to be displayed:",
                                                                                                                                                                               choices = names(cuantis),
                                                                                                                                                                               selected = names(cuantis), multiple = TRUE))
                                        ),
                                        fluidRow(box(title = tags$b("How could you help to solve this pollution problem?:"), width = 12, status = "danger", solidHeader = TRUE, 
                                                     textInput("text", label = "Write your advice here:"),
                                                     actionButton("send", "Click to send"))
                                        )
                                 )
                             )
                    )
                )
        ),
        tabItem("about", includeHTML("About.html"))
    )
)


# Completing the ui part with dashboardPage -------------------------------

ui <- dashboardPage(title = 'Air Quality - Aburra Valley', header, sidebar, body, skin='green')

