#ui

#Set User Interface

bootstrapPage('',
              
              navbarPage(
                title = icon(name = "laptop-code", class = "fa-solid fa-laptop-code"),
                windowTitle = "Shiny Stats",
                fluid = T,
                theme = shinytheme("flatly"), 
                position = "fixed-top",
                
                # Map Tab -------------------------------
                
                tabPanel("Intro", 
                         titlePanel(h2("Shiny Stats for teaching", style = "margin-bottom: 30px;
                                                                     font-size: 36px")),                 
                         # Introduction text 
                         wellPanel(

                           p("Welcome to my Shiny App, a platform designed to teach basic statistics concepts. 
                             The app features several tabs that guide you through different themes, with more 
                             themes to be added in the future. Each tab is packed with interactive content to 
                             help you understand fundamental statistical concepts easily. In the", em("About"),
                             "tab, you can find detailed explanations about the background of the project, while 
                             the", em("Further Links"), "tab provides links to other useful resources for learning
                             statistics, as well as all the data sources used in this app. Whether you're a
                             beginner or an advanced learner, this app is designed to help you master statistical
                             concepts in an engaging and interactive way."
                             )
                        
                         ),
                        
                    
                ),
                
                # Further links ------------------------
                tabPanel("Descriptive stats"
                ),
                
                # Further links ------------------------
                tabPanel("Distributions"
                ),
                
                # Further links ------------------------
                tabPanel("Sample Size"
                ),
                
                # Further links ------------------------
                tabPanel("Futher links"
                         ),
                
                
                # About -------------------------------
                tabPanel("About",
                         
                         # Explanation about project
                         wellPanel(
                           h3("About this project"),
                           p("This project visualizes the spatial distribution of the most common
                          German endings of mountain names in the Alps. Many mountains share common
                          suffixes such as “-horn”, “-spitze” and “-kogel”, however are there any
                          regional differences? Since German is spoken throughout many regions
                          of the Alps, these distributions do not necessarily follow national borders.
                          However, obviously, most mountain peaks with German names can be found in
                          predominantly German speaking areas such as Austria, South-Tyrol,
                          Germany and parts of Switzerland."),
                          
                          p("The analysis is based on the names of mountains as reported in the OpenStreetMap
                          project and therefore might not include every regional variety or secondary summits.
                          Furthermore not all peaks had elevation data included, hence some peaks are missing in
                          the visualization in the plot tab. Additionally some other special names or name add-ons
                          might not have been correctly handled in the data preparation."),
                          
                          p("The R code to recreate the project can be found", 
                            a("here.", href="https://github.com/sebhanika/alpine_peaks", target="_blank")
                          )
                         )
                         
                ),
                # Footer -------------------------------
                footer = tags$div(
                  class = "footer", 
                  hr(style = "border-color: #cbcbcb;"),
                  fluidRow(
                    column(9,
                           # data sources
                           p('Data sources: ', 
                             tags$a(href = "https://ec.europa.eu/eurostat/", 'eurostat',
                                    target = '_blank'), ".", style = "font-size: 80%"),
                           
                           #github info
                           p("Created by Sebastian Hanika in April 2023", HTML("&bull;"),
                             "Find the code on Github:", 
                             tags$a(href = "https://github.com/sebhanika/stats_teaching", 
                                    icon(name = "github", class = 'fa fa-github', style = 'color:#5000a5'),
                                    target = '_blank'), style = "font-size: 80%"),
                           
                           #contact info
                           p("Connect with me on Twitter", 
                             tags$a(href = "https://twitter.com/SebHanika", 
                                    icon(name = "twitter", class = 'fa fa-twitter', style = 'color:#00acee'),
                                    target = '_blank'),
                             HTML("&bull;"), "Or send an email ",
                             tags$a(href = "mailto:hanikasebastian@gmail.com",
                                    icon(name = "envelope", class = 'fa-solid fa-envelope', style = 'color:#516888'),
                                    target = '_blank'), 
                             style = 'font-size:80%' ),
                           
                           # Last changes
                           p(tags$em("Last changes: April 2023"), style = 'font-size:70%')
                    )
                  ),
                  style = "margin-left: 15px;"
                )
                # close navbarPage and UI
              ),
              
              tags$style("* { font-family: Helvetica; font-size: 101%}"),
              tags$style(type="text/css", "body {padding-top: 70px;}")
)

