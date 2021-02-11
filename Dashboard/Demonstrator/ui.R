library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

dashboardPage(
    dashboardHeader(title = "Visualizer"),
    #Content of the Sidebar
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Performance analysis",
            tabName = "performance",
            icon = icon("dashboard")
        ),
        menuItem(
            "Complexity analysis",
            tabName = "complexity",
            icon = icon("dashboard")
        ),
        menuItem("About",
                 tabName = "about",
                 icon = icon("th"))
    )),
    #Content of the Dashboard body
    dashboardBody(tabItems(
        #
        #Content for the overview tab
        #
        tabItem(
            tabName = "about",
            fluidRow(column(width = 1),
                     column(width = 10,
                            div(
                                h3("About", style = "text-align:center;"),
                                p(
                                    "This dashboard accompanies my Bachelor's thesis",
                                    tags$b("Kernel methods: Theory and robust applications, "),
                                    "written at the Chair of Analytics and Statistics at the Karlsruher Institute for Technology (Supervisor: Fabian Kächele).",
                                    br(),
                                    "The visualizations in these dashboards are part of the Simulation section in my thesis and are used to show my results.
                                   By reading my thesis it should be clear what the visualizations are about and how to use them.",
                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
                                ),
                                h3("Contact", style = "text-align:center;"),
                                p(
                                    "If you have any questions about the simulation, the dashboard or my thesis, feel free to contact via ",
                                    a(href = "mailto:christopher.buelte@student.kit.edu", "email"),
                                    " .",
                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
                                )
                            )),
                     column(width = 1),),
            hr(),
            p(em("Developed by"), br("Christopher Bülte"), style = "text-align:center; font-family: times")
        ),
        #
        #Content of the performance tab
        #
        tabItem(
            tabName = "performance",
            fluidRow(
                tabBox(
                    title = "Performance",
                    width = 7,
                    tabPanel(title = "2D",
                             plotlyOutput("plot_performance")),
                    tabPanel(title = "3D",
                             plotlyOutput("plot_performance3d"))
                ),
                tabBox(
                    title = "Plots",
                    width = 5,
                    tabPanel(title = "Boxplot",
                             plotlyOutput("plot_boxplot")),
                    tabPanel(title = "KDE",
                             plotlyOutput("plot_kde"))
                )
                
            ),
            fluidRow(
                box(
                    title = "Controls",
                    width = 5,
                    solidHeader = TRUE,
                    status = "primary",
                    fluidRow(column(
                        width = 6,
                        selectInput(
                            inputId = "n",
                            label = "Dimensions",
                            choices = c(150, 500)
                        )
                    ),
                    column(
                        width = 6,
                        selectInput(
                            inputId = "p",
                            label = "Dimensions",
                            choices = c(1, 2, 5)
                        )
                    ),),
                    fluidRow(
                        column(
                            width = 9,
                            sliderInput(
                                inputId = "gamma",
                                label = "Outlier percentage",
                                min = 0.05,
                                max = 0.3,
                                step = 0.05,
                                value = 0.05
                            )
                        ),
                        column(
                            width = 3,
                            br(),
                            br(),
                            checkboxInput(
                                inputId = "error_bars",
                                label = "Show error bars",
                                value = FALSE
                            )
                        )
                    )
                ),
                column(width = 3),
                box(
                    title = "Boxplot controls",
                    width = 4,
                    solidHeader = TRUE,
                    status = "primary",
                    fluidRow(column(
                        width = 6,
                        selectInput(
                            inputId = "n_box",
                            label = "Dimensions",
                            choices = c("All", 150, 500)
                        )
                    ),
                    column(
                        width = 6,
                        selectInput(
                            inputId = "p_box",
                            label = "Dimensions",
                            choices = c("All", 1, 2, 5)
                        )
                    ),),
                    fluidRow(
                        column(
                            width = 9,
                            sliderInput(
                                inputId = "gamma_box",
                                label = "Outlier percentage",
                                min = 0.05,
                                max = 0.3,
                                step = 0.05,
                                value = 0.05
                            )
                        ),
                        column(
                            width = 3,
                            br(),
                            br(),
                            checkboxInput(
                                inputId = "gamma_all",
                                label = "All values",
                                value = TRUE
                            )
                        )
                    ),
                    
                )
            ),
            hr(),
            p(em("Developed by"), br("Christopher Bülte"), style = "text-align:center; font-family: times"),
        ),
        #
        #Content of the complexity tab
        #
        tabItem(
            tabName = "complexity",
            fluidRow(
                box(
                    title = "Controls",
                    width = 2,
                    solidHeader = TRUE,
                    status = "primary",
                    checkboxInput(
                        inputId = "log_complexity",
                        label = "3rd root transform",
                        value = FALSE
                    ),
                    checkboxInput(
                        inputId = "complexity_fit",
                        label = "Show fit",
                        value = FALSE
                    ),
                    checkboxInput(
                        inputId = "complexity_asymptotic",
                        label = "Show asymptotic behavior",
                        value = FALSE
                    )
                ),
                box(
                    title = "Complexity",
                    width = 10,
                    plotlyOutput("plot_complexity")
                )
            ),
            hr(),
            p(em("Developed by"), br("Christopher Bülte"), style = "text-align:center; font-family: times"),
        )
    )),
    skin = "blue",
)
