library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(plotly)
library(ggridges)
library(ggpubr)
library(GGally)

load("entorno.RData")

server <- function(input, output, session) {
    
    
# Datatable output --------------------------------------------------------
    
    
    output$datatable <- renderDataTable(BDcompleta)
    
    
# -------------------------------------------------------------------------
# PM 2.5 TAB  -------------------------------------------------------------
# -------------------------------------------------------------------------
    

# Boxplot PM 2.5 by month -------------------------------------------------
    
# Reactive function 
    
    monthszone <- reactive({
        attach(BDcompleta)
        return(subset(BDcompleta, Month == input$month & Area == input$area))
        
    })
    
# Plot
    
    output$boxplotmonth <- renderPlot({
        attach(BDcompleta)
        
        ggplot(data = monthszone(), aes(x = Month , y = Pm25, fill= Station_Type)) +
            geom_boxplot(size = 0.3) + ylab("PM 2.5 concentration") + theme(legend.position="top") +
            scale_fill_discrete(name = "Station Type") 
        
        #ggplotly(a) %>% layout(boxmode = "group")
        
    })
    
    
# Boxplot PM 2.5 by hour  -------------------------------------------------
    
# Reactive function 
    
    reactivehour <- reactive({
        
        return(subset(BDcompleta, Hour == input$hour))
        
    })
    
    
# Plot
    
    output$boxplothour <- renderPlot({
        
        ggplot(data = reactivehour(), aes(x = Hour, y = Pm25, fill= Station_Type)) +
            geom_boxplot(size = 0.3) + ylab("PM 2.5 concentration") + theme(legend.position="top") +
            scale_fill_discrete(name = "Station Type") 
        
        #plotly_build(b) %>% layout(boxmode = "group")
        
    })
    
    
# Density plot by months --------------------------------------------------
    
# Reactive function  
    
    reactivemonths <- reactive({
        
        return(subset(BDcompleta, Month == input$months2))
        
    })  
    
    
# Plot
    
    output$densitymonths <- renderPlot({
        
        ggplot(data = reactivemonths(), aes(x = `Pm25`, y = `Month`)) +
            geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.7) +
            scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "PM 2.5")+
            labs(title = 'PM 2.5 concentration by month')
        
    })
    
    
# -------------------------------------------------------------------------
# OTHER VARIABLES TAB -----------------------------------------------------
# -------------------------------------------------------------------------
    
# Plot for other variables ------------------------------------------------
    
    output$plot <- renderUI({
        plotOutput("p")
    })
    
    output$p <- renderPlot({
        
        plot.obj<<-list()
        
        plot.obj$variable<<-with(cuantis,get(input$variable))
        
        plot.obj$variable2 <<-with(cualis,get(input$variable2))
        
        plottype<-switch(input$plottype,
                         "histogram" =	geom_histogram(aes(y = ..density..,fill = ..count..), binwidth=0.2),
                         "boxplot" 	= 	geom_boxplot(fill = "#E69F00")
                         
        )
        
        .theme<- theme(
            axis.line = element_line(colour = 'gray', size = .75)
        )
        
        if(input$plottype=="histogram") {
            
            p<-ggplot(cuantis, aes(x = plot.obj$variable)) + plottype + geom_density() + xlab(input$variable)
            
        } else {
            
            p<- ggplot(cualis, aes(x= plot.obj$variable2, y = plot.obj$variable)) + plottype + xlab(input$variable2)   + ylab(input$variable) 
            
        }
        
        p<-p+ .theme 
        
        print(p)
        
    })
    
    
# Correlation and histogram plot ------------------------------------------
    
#Updating the selected variables
    
    observe({
        updateSelectInput(session, "var.corr", selected = input$var.corr)
        
    })
    
#Plot  
    
    output$correlations <- renderPlot({
        
        ggpairs(cuantis[, input$var.corr], lower = list(continuous = wrap("smooth", colour = "turquoise4")), 
                diag = list(continuous = "bar"), axisLabels = "none") +
            theme(panel.background = element_rect(fill = "gray98"),
                  axis.line.y = element_line(colour="black"),
                  axis.line.x = element_line(colour="black"))
        
    })
    
# Advice message ----------------------------------------------------------
    
    observeEvent(input$send, {
        
        ms <- 'alert("Thank you for the advice!");'
        
        session$sendCustomMessage(type='jsCode', list(value = ms))
        
        observe({
            
            updateTextInput(session,"text", value="") 
            
        })
        
    })

# END ---------------------------------------------------------------------

    
}

