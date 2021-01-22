library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(rgl)
library(hrbrthemes)
library(viridis)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #Load data
    data<-read_excel("../results.xlsx")
    
    #Filter data for performance
    data_performance <- reactive({
        filter(data, n == input$n, p == input$p, gamma == input$gamma)%>% 
            select(`KRR-Mean`, `Huber-Mean`,`RKR-Mean`,`KGARD-Mean`,`Shift`,`gamma`) %>%
            pivot_longer(., cols = c(`KRR-Mean`, `Huber-Mean`,`RKR-Mean`,`KGARD-Mean`),
                         names_to = "method", values_to = "mean")
    })
    
    #Filter data for 3D plot
    data_performance3d <- reactive({
        filter(data, n == input$n, p == input$p) %>% 
            select(`KRR-Mean`, `Huber-Mean`,`RKR-Mean`,`KGARD-Mean`,`Shift`,`gamma`) %>%
            pivot_longer(., cols = c(`KRR-Mean`, `Huber-Mean`,`RKR-Mean`,`KGARD-Mean`),
                         names_to = "method", values_to = "mean")
    })
    
    
    
    #Filter data for boxplot/kde
    data_boxplot <- reactive({
        #Filter data
        data_filtered <- data %>% rename(`KRR`=`KRR-Mean`, Huber=`Huber-Mean`, RKR=`RKR-Mean`, KGARD = `KGARD-Mean`)
        if(input$n_box!="All"){
            data_filtered <- data_filtered %>% filter(n==input$n_box)
        }
        if(input$p_box!="All"){
            data_filtered <- data_filtered %>% filter(p==input$p_box)
        }
        if(input$gamma_all!=TRUE){
            data_filtered <- data_filtered %>% filter(gamma==input$gamma_box)
        }
        
        data_filtered %>% select(`KRR`, `Huber`,`RKR`,`KGARD`) %>%
            pivot_longer(., cols = c(`KRR`, `Huber`,`RKR`,`KGARD`),
                         names_to = "method", values_to = "mean")
    })
    
    
    #Render Performance plot
    output$plot_performance<-renderPlotly({
        fig <- plot_ly(data_performance(), x = ~`Shift`, y = ~`mean`, color = ~`method`,
                       mode = "lines+markers")
        fig <- fig %>% layout(yaxis = list(title="MSE"), xaxis = list(title="Outlier shift"),
                              legend = list(x=0.05,y=1,title=list(text='<b> Methods </b>')),
                              hovermode="compare")
        fig
    })
    
    #Render boxplot
    output$plot_boxplot<-renderPlotly({
        fig <- plot_ly(data_boxplot(), y =~mean , type = "box", color=~method,
                       boxpoints="suspectedoutliers")
        fig <- fig %>% layout(yaxis = list(title="MSE"), showlegend=FALSE)
        fig
    })
    
    #Render KDE
    output$plot_kde<-renderPlotly({
        fig<-ggplot(data_boxplot(), aes(x = mean, fill=method, group=method))+
                    geom_density(outline.type="upper",
                                 alpha=0.3)+
                    xlab("MSE")+ylab("Density")+
                    theme_bw()+
                    theme(legend.title=element_blank())
        ggplotly(fig) %>% layout(legend = list(x=0.8, y=1))
    })
    
    #3D performance
    output$plot_performance3d<-renderPlotly({
        fig <- plot_ly(data=data_performance3d(),x=~Shift, y=~gamma,
                z=~mean, type="scatter3d", mode = "markers", color=~method)
        fig %>% layout(scene=list(xaxis = list(title = "Outlier shift"),
                                  yaxis = list(title = "Outlier percentage"),zaxis=list(title = "MSE")))
    })

}
