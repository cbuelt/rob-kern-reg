library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(rgl)
library(hrbrthemes)
library(viridis)
library(tidyr)

dashboardPage(
    dashboardHeader(title="Dashboard"),
    #Content of the Sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Performance analysis", tabName = "performance", icon = icon("dashboard")),
            menuItem("Complexity analysis", tabName = "complexity", icon = icon("th"))
        )
    ),
    #Content of the Dashboard body
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "performance",
                    fluidRow(
                        tabBox(
                            title="Performance",
                            width=7,
                            tabPanel(title="2D",
                                     plotlyOutput("plot_performance")
                                     ),
                            tabPanel(title="3D",
                                     plotlyOutput("plot_performance3d")
                            )
                            ),
                        tabBox(
                            title = "Plots",
                            width=5,
                            tabPanel(title="Boxplot",
                                     plotlyOutput("plot_boxplot")
                                     ),
                            tabPanel(title="KDE",
                                     plotlyOutput("plot_kde")
                                     )
                        )

                    ),
                    fluidRow(
                        box(
                            title = "Controls", width = 5, solidHeader = TRUE, status = "primary",
                            fluidRow(
                                column(width=6,
                                    selectInput(inputId = "n",
                                                label = "Dimensions",
                                                choices = c(150,500))
                                        ),
                                column(width=6,
                                    selectInput(inputId = "p",
                                                label="Dimensions",
                                                choices = c(1,2,5))
                                        ),
                            ),
                            fluidRow(
                                column(width=9,
                                       sliderInput(inputId = "gamma",
                                                   label = "Outlier percentage",
                                                   min=0.05,max=0.3,step=0.05,value=0.05)
                                       ),
                                column(width=3,
                                       br(),
                                       br(),
                                       checkboxInput(inputId = "error_bars",
                                                     label= "Show error bars",
                                                     value=FALSE))
                            )
                        ),
                        column(width=3),
                        box(
                            title = "Boxplot controls", width = 4, solidHeader = TRUE, status = "primary",
                            fluidRow(
                                column(width=6,
                                       selectInput(inputId = "n_box",
                                                   label = "Dimensions",
                                                   choices = c("All",150,500))
                                ),
                                column(width=6,
                                       selectInput(inputId = "p_box",
                                                   label="Dimensions",
                                                   choices = c("All",1,2,5))
                                ),
                            ),
                            fluidRow(
                                column(width=9,
                                       sliderInput(inputId = "gamma_box",
                                                   label = "Outlier percentage",
                                                   min=0.05,max=0.3,step=0.05,value=0.05)
                                       ),
                                column(width=3,
                                       br(),
                                       br(),
                                       checkboxInput(inputId = "gamma_all",
                                                     label= "All values",
                                                     value=TRUE))
                            ),

                        )
                    ),
            ),
            
            # Second tab content
            tabItem(tabName = "complexity",
                    fluidRow(
                        box(
                            title = "Controls", width = 2, solidHeader = TRUE,
                            status = "primary",
                            checkboxInput(inputId = "log_complexity",
                                          label= "3rd root transform",
                                          value=FALSE),
                            checkboxInput(inputId = "complexity_fit",
                                          label= "Show fit",
                                          value=FALSE),
                            checkboxInput(inputId = "complexity_asymptotic",
                                          label= "Show asymptotic behavior",
                                          value=FALSE)
                            ),
                        box(
                            title="Complexity",
                            width=10,
                            plotlyOutput("plot_complexity")
                        )
                    )
            )
        )
    ),
    skin="blue",
)
