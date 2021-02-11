# Define server logic required to draw a histogram
server <- function(input, output) {
    #Load data
    data <- read_excel("../../data/simulation_results.xlsx")
    data_complexity <-
        read_excel("../../data/complexity_results.xlsx")
    
    #Transform and predict data for complexity
    data_complexity_pred <- reactive({
        data_fit <- select(data_complexity, c(n, KRR, Huber, RKR, KGARD))
        if (input$complexity_asymptotic == FALSE) {
            seq <- seq(100, 3000, 100)
        } else{
            seq <- c(seq(100, 3000, 100), seq(4000, 30000, 1000))
        }
        data_pred <- tibble("n" = seq)
        print(data_pred)
        models <- c("KRR", "RKR", "KGARD", "Huber")
        for (model in models) {
            #Train models
            d <- ifelse (model == "Huber", 2, 3)
            formula <- paste0(model, "~", "poly(n,", d, ",raw=TRUE)")
            model_fit <- lm(formula, data = data_fit)
            #Predict new data
            data_pred[, ncol(data_pred) + 1] <-
                model_fit$coefficients[1] +
                poly(data_pred$n, d, raw = TRUE) %*% model_fit$coefficients[-1]
            #Rename
            data_pred <- rename(data_pred,!!paste0(model) := V1)
        }
        data_pred <- data_pred %>%
            pivot_longer(
                .,
                cols = c(`KRR`, `Huber`, `RKR`, `KGARD`),
                names_to = "method",
                values_to = "time_pred"
            )
        data_pred
    })
    
    #Filter data for performance
    data_performance <- reactive({
        #Aggregate data with mean
        data_mean <-
            filter(data, n == input$n, p == input$p, gamma == input$gamma) %>%
            rename(
                `Huber` = `Huber-Mean`,
                `KRR` = `KRR-Mean`,
                `RKR` = `RKR-Mean`,
                `KGARD` = `KGARD-Mean`
            ) %>%
            select(`KRR`, `Huber`, `RKR`, `KGARD`, `Shift`) %>%
            pivot_longer(
                .,
                cols = c(`KRR`, `Huber`, `RKR`, `KGARD`),
                names_to = "method",
                values_to = "mean"
            )
        #Aggregate data with sd
        data_sd <-
            filter(data, n == input$n, p == input$p, gamma == input$gamma) %>%
            rename(
                `Huber` = `Huber-SD`,
                `KRR` = `KRR-SD`,
                `RKR` = `RKR-SD`,
                `KGARD` = `KGARD-SD`
            ) %>%
            select(`KRR`, `Huber`, `RKR`, `KGARD`, `Shift`) %>%
            pivot_longer(
                .,
                cols = c(`KRR`, `Huber`, `RKR`, `KGARD`),
                names_to = "method",
                values_to = "SD"
            )
        #Merge and return data
        data_mean %>% full_join(data_sd, by = c("Shift", "method"))
    })
    
    #Filter data for 3D plot
    data_performance3d <- reactive({
        filter(data, n == input$n, p == input$p) %>%
            select(`KRR-Mean`,
                   `Huber-Mean`,
                   `RKR-Mean`,
                   `KGARD-Mean`,
                   `Shift`,
                   `gamma`) %>%
            pivot_longer(
                .,
                cols = c(`KRR-Mean`, `Huber-Mean`, `RKR-Mean`, `KGARD-Mean`),
                names_to = "method",
                values_to = "mean"
            )
    })
    
    #Filter data for boxplot/kde
    data_boxplot <- reactive({
        #Filter data
        data_filtered <-
            data %>% rename(
                `KRR` = `KRR-Mean`,
                Huber = `Huber-Mean`,
                RKR = `RKR-Mean`,
                KGARD = `KGARD-Mean`
            )
        if (input$n_box != "All") {
            data_filtered <- data_filtered %>% filter(n == input$n_box)
        }
        if (input$p_box != "All") {
            data_filtered <- data_filtered %>% filter(p == input$p_box)
        }
        if (input$gamma_all != TRUE) {
            data_filtered <- data_filtered %>% filter(gamma == input$gamma_box)
        }
        
        data_filtered %>% select(`KRR`, `Huber`, `RKR`, `KGARD`) %>%
            pivot_longer(
                .,
                cols = c(`KRR`, `Huber`, `RKR`, `KGARD`),
                names_to = "method",
                values_to = "mean"
            )
    })
    
    #Render Performance plot
    output$plot_performance <- renderPlotly({
        data_plot <- data_performance()
        if (input$error_bars == FALSE) {
            fig <-
                plot_ly(
                    data_plot,
                    x = ~ `Shift`,
                    y = ~ `mean`,
                    color = ~ `method`,
                    mode = "lines+markers",
                    colors = "RdBu"
                )
            fig <-
                fig %>% layout(
                    yaxis = list(title = "MSE"),
                    xaxis = list(title = "Outlier shift"),
                    legend = list(
                        x = 0.05,
                        y = 1,
                        title = list(text = '<b> Methods </b>')
                    ),
                    hovermode = "compare"
                )
            fig
        } else{
            fig <-
                plot_ly(
                    data_plot,
                    x = ~ `Shift`,
                    y = ~ `mean`,
                    color = ~ `method`,
                    mode = "lines+markers",
                    error_y =  ~ list(array = data_plot$SD),
                    colors = "RdBu"
                )
            fig <-
                fig %>% layout(
                    yaxis = list(title = "MSE"),
                    xaxis = list(title = "Outlier shift"),
                    legend = list(
                        x = 0.05,
                        y = 1,
                        title = list(text = '<b> Methods </b>')
                    ),
                    hovermode = "compare"
                )
            fig
        }
    })
    
    #Render boxplot
    output$plot_boxplot <- renderPlotly({
        fig <-
            plot_ly(
                data_boxplot(),
                y =  ~ mean ,
                type = "box",
                color =  ~ method,
                boxpoints = "suspectedoutliers",
                colors = "RdBu"
            )
        fig <-
            fig %>% layout(yaxis = list(title = "MSE"),
                           showlegend = FALSE)
        fig
    })
    
    #Render KDE
    output$plot_kde <- renderPlotly({
        c_scale <- RColorBrewer::brewer.pal(4, "RdBu")
        fig <-
            ggplot(data_boxplot(),
                   aes(
                       x = mean,
                       fill = method,
                       group = method
                   )) +
            geom_density(outline.type = "upper",
                         alpha = 0.3) +
            xlab("MSE") + ylab("Density") +
            theme_bw() +
            theme(legend.title = element_blank())
        withr::with_options(list(ggplot2.discrete.fill = c_scale),
                            print(ggplotly(fig) %>% layout(legend = list(
                                x = 0.8, y = 1
                            ))))
    })
    
    #3D performance
    output$plot_performance3d <- renderPlotly({
        fig <- plot_ly(
            data = data_performance3d(),
            x =  ~ Shift,
            y =  ~ gamma,
            z =  ~ mean,
            type = "scatter3d",
            mode = "markers",
            color =  ~ method,
            colors = "RdBu"
        )
        fig %>% layout(scene = list(
            xaxis = list(title = "Outlier shift"),
            yaxis = list(title = "Outlier percentage"),
            zaxis = list(title = "MSE")
        ))
    })
    
    #Plot complexity
    output$plot_complexity <- renderPlotly({
        #Transform data
        data <- data_complexity %>%
            select(`KRR`, `Huber`, `RKR`, `KGARD`, `n`) %>%
            pivot_longer(
                .,
                cols = c(`KRR`, `Huber`, `RKR`, `KGARD`),
                names_to = "method",
                values_to = "time"
            )
        
        data_fit <- data_complexity_pred()
        
        #Check for root transform
        if (input$log_complexity == TRUE) {
            data$time <- (data$time) ^ (1 / 3)
            data_fit$time_pred <- (data_fit$time_pred) ^ (1 / 3)
            y_title = "Cubic rooted time [(s^(1/3))]"
        } else{
            y_title = "Time [s]"
        }
        #Plot results
        fig <-
            plot_ly(
                data = data,
                x = ~ n,
                y = ~ time,
                color = ~ method,
                type = "scatter",
                mode = "markers",
                colors = "RdBu"
            )
        if (input$complexity_fit == TRUE) {
            fig <-
                fig %>% add_trace(
                    data = data_fit,
                    y =  ~ time_pred,
                    type = "scatter",
                    mode = "lines",
                    color =  ~ method
                )
        }
        fig %>% layout(
            yaxis = list(title = y_title),
            xaxis = list(title = "n"),
            legend = list(
                x = 0.05,
                y = 1,
                title = list(text = '<b> Methods </b>')
            ),
            hovermode = "compare"
        )
    })
    
}
