
source("helpers.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin="yellow",
                    #add title
                    dashboardHeader(title="Statistics on Men's T20 Cricket Batting Records",titleWidth=600),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                        menuItem("About", tabName = "about", icon = icon("archive")),
                        menuItem("Application", tabName = "app", icon = icon("laptop"))
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                        tabItems(
                            # First tab content
                            tabItem(tabName = "about",
                                    fluidRow(
                                        #add in latex functionality if needed
                                        #withMathJax(helpText("Some math here $$\\alpha+\\beta$$")),
                                        
                                        #two columns for each of the two items
                                        column(4,
                                               #Description of App
                                               h2("What does this app do?"),
                                               #box to contain description
                                               box(background="lime",width=12,
                                                   h5("Cricket is a bat-and-ball game played between two teams of eleven players on a field at the centre of which is a 20-metre (22-yard) pitch with a wicket at each end, each comprising two bails balanced on three stumps."),
                                                   h5("One side bats first and scores runs by hitting the ball bowled(or pitched) by another side much like baseball. Multiple bowlers can bowl in an inning but one bowler can bowls 6 legal balls in one go and this is called an over."),
                                                   h5("Side which scores more runs wins."),
                                                   h5("Whle baseball has 4 bases (and potentially 4 batters, cricket has only two wickets and hence only two batters at a time."),
                                                   h5("For more information on Cricket, please read"), a(href="https://en.wikipedia.org/wiki/Cricket", "Wikipedia page"), 
                                                   h5("T20 is one inning is played by each team and each inning is limited to 20 overs"),
                                                   h5("This app will analyze batting records of Individual Men Players in T20 games."),
                                                   h5("App is deployed at "), a(href="https://sushilgupta.shinyapps.io/cricket_t20/", "this URL"), 
                                               )
                                        ),
                                        column(8,
                                               #How to use the app
                                               h2("How to use the app?"),
                                               #box to contain description
                                               box(background="red",width=12,
                                                   h5("Only data for those countries is included that had at least 20 players. We want to analyze only countries where cricket is a regular sport"),
                                                   p(strong("Explore Data")),
                                                   h6("Analyzes which country has most long playing players"),
                                                   h6("Choose country and number of matches from input box on left side of app"),
                                                   h6("Visual and Numeric Tab Shows Histogram and Numeric Summary of how many players are in a Country who have played more then a certain number of matches"),
                                                   h6("Scroll Data shows a tabular view"),
                                                   h6("Both Histogram and Data table can be downloaded"),
                                                   p(strong("Clustering Analysis")),
                                                   h6("Creates grouping of players on aggressiveness and reliability scales "),
                                                   h6("Only those players are considered who have scored more then 1000 Runs"),
                                                   h6("We will fit models to predict number of runs scored based on matches, years played, balls played"),
                                                   p(strong("Modeling")),
                                                   h5("First Model - Linear Regression"),
                                                   withMathJax(helpText("Prediction Equation will be $$Runs=\\beta_0 + \\beta_1(Matches) + \\beta_2(Years) + \\beta_3(Balls)$$")),
                                                   h5("Second Model"),
                                               )
                                        )
                                    )
                            ),
                            
                            #actual app layout      
                            tabItem(tabName = "app",
                                    fluidRow(
                                        column(3,
                                               box(width=12,title="Explore Data",
                                                   selectInput("Country", "Select country:", c("All", unique(as.character(mt20_gt20$Country)))),
                                                   sliderInput("nmatch", "Players with matches greater then or equal to:", value = 10, min = 1,max = max(as.numeric(mt20$Matches))),
                                                   br(),
                                                   downloadButton("downloadData", "Save Data"), 
                                                   br(),
                                                   downloadButton("downloadPlot", "Save Histogram")
                                               ),
                                               br(),
                                               box(width=12,title="Clustering Inputs",
                                                   sliderInput("clst_grps","Divide players in these many groups",min=1,max=10,value=3)
                                               ),
                                               box(width=12,title="Modeling and Prediction",
                                                   h6("Choose Variables"),
                                                   checkboxInput("lm_var_matches",label="Number of Matches",value=TRUE), 
                                                   checkboxInput("lm_var_years",label="Number of Years Played",value=FALSE), 
                                                   checkboxInput("lm_var_balls",label="Number of Balls Faced",value=FALSE), 
                                                   h6("Inputs for Prediction"),
                                                   conditionalPanel(condition="input.lm_var_matches", numericInput("pred_var_matches","Number of Matches",value=20,min=20,max=500)),
                                                   conditionalPanel(condition="input.lm_var_years", numericInput("pred_var_years","Number of Years Played",value=5,min=3,max=25)),
                                                   conditionalPanel(condition="input.lm_var_balls", numericInput("pred_var_balls","Number of Balls Faced",value=100,min=100,max=10000))
                                               )
                                        ),
                                        
                                        column(9,
                                               tabsetPanel(
                                                   tabPanel("Explore Data",
                                                            tabsetPanel(
                                                                tabPanel("Visual & Numeric",
                                                                         fluidRow(
                                                                             h4("Countries with Players and data on Map"),
                                                                             plotlyOutput(outputId = "countryMap"),
                                                                             h4("Number of players by country. Shows popularity of game in country"),
                                                                             plotOutput("allPlayerHist"),
                                                                             h4("Detailed analysis can be done here for country.Provide inputs on left."),
                                                                             h4("Save histogram using Download Histogram Button."),
                                                                             plotOutput("matchHist"), 
                                                                             h4("Modelling will be done on # of matches, years and Balls Faced. Checking numeric summaries."),
                                                                             verbatimTextOutput("num_summary")
                                                                         )
                                                                ),
                                                                tabPanel("Scroll Data",
                                                                         fluidRow(       
                                                                             DT::dataTableOutput("table")
                                                                         )
                                                                )
                                                            )
                                                   ),
                                                   tabPanel("Clustering Analysis", 
                                                            tabsetPanel(
                                                                tabPanel("K Means Clustering",
                                                                         fluidRow(
                                                                             includeHTML("kmean.html"),
                                                                             tableOutput("CA_tab_kmean"),
                                                                             plotOutput("CA_plt_kmean")
                                                                         )                
                                                                ),
                                                                tabPanel("Hierarchial Clustering",
                                                                         fluidRow(
                                                                             plotOutput("CA_plt_hier")
                                                                         )
                                                                )
                                                            ) 
                                                   ),
                                                   tabPanel("Modeling", 
                                                            tabsetPanel(
                                                                tabPanel("Linear Model",
                                                                         fluidRow(
                                                                             uiOutput("lm_mod_pred"),
                                                                             verbatimTextOutput("lm_summary"), 
                                                                             plotOutput("lm_mod_plt")
                                                                         )
                                                                ),
                                                                tabPanel("Decision Tree",
                                                                         fluidRow(
                                                                             uiOutput("dt_mod_pred"),
                                                                             verbatimTextOutput("dt_summary"), 
                                                                             plotOutput("dt_mod_plt")
                                                                         )
                                                                )
                                                            ) #end tabsetPanel
                                                   ) 
                                                   
                                               ) #end tab set
                                        ) #end column
                                    ) #end fluidrow
                            ) #end tabItem
                        ) #end tabItems
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #REACTIVE FUNCTION FOR DATA FILTER 
    d <- reactive({
        data<-mt20_gt20
        if (input$Country != "All") {data <- data[data$Country == input$Country,]}
        data <- data[data$Matches >= input$nmatch,]
        data
    })
    
    # LINEAR MODEL IS REACTIVE ON VARIABLES
    lm_mod <- reactive({
        lmds <- data.frame(mt20_gt20$Runs)
        colnames(lmds)<-c("Runs")
        if (input$lm_var_matches){lmds$Matches<-mt20_gt20$Matches}
        if (input$lm_var_years){lmds$Years<-as.numeric(mt20_gt20$End)-as.numeric(mt20_gt20$Start)}
        if (input$lm_var_balls){lmds$BallsFaced<-mt20_gt20$BallsFaced}
        lm_mod <- lm(lmds$Runs ~ ., data=lmds)
    })
    
    # DECISION TREE IS REACTIVE ON VARIABLES  
    dt_mod <- reactive({
        lmds <- data.frame(mt20_gt20$Runs)
        colnames(lmds)<-c("Runs")
        if (input$lm_var_matches){lmds$Matches<-mt20_gt20$Matches}
        if (input$lm_var_years){lmds$Years<-as.numeric(mt20_gt20$End)-as.numeric(mt20_gt20$Start)}
        if (input$lm_var_balls){lmds$BallsFaced<-mt20_gt20$BallsFaced}
        dt_mod <- tree(lmds$Runs ~ ., data=lmds)
    })
    
    # PREDICTING RUNS BASED ON REACTIVE MODELS AND INPUT VARIABLES  
    pred_runs <- reactive({
        
        pred_inp_matches<-0
        pred_inp_years<-0
        pred_inp_balls<-0
        
        #GATHER INPUTS FROM NUMERIC BOXES FOR PREDICTION 
        if (input$lm_var_matches){pred_inp_matches<-as.numeric(input$pred_var_matches)}
        if (input$lm_var_years){pred_inp_years<-as.numeric(input$pred_var_years)}
        if (input$lm_var_balls){pred_inp_balls<-as.numeric(input$pred_var_balls)}
        
        #COMBINE INPUTS INTO A DATAFRAME FOR PREDICT 
        pred<-data.frame(Matches=pred_inp_matches, Years=pred_inp_years, BallsFaced=pred_inp_balls)
        
        #PREDICT USING BOTH DECISION TREES AND LINEAR MODELS
        predres<-c(0,0)
        predres[1]<-predict(lm_mod(), newdata = pred)
        predres[2]<-predict(dt_mod(), newdata=pred)
        predres
    })
    
    # observe({print(str(pred_runs()))})
    # SLIDER MAX SHOULD ADJUST BASED ON COUNTRY SELECTED 
    observe({
        currmax<-max(as.numeric(d()$Matches))
        updateSliderInput(session, "nmatch", max=currmax)
        if(!input$lm_var_matches & !input$lm_var_years & !input$lm_var_balls){
            updateCheckboxInput(session, "lm_var_matches", value=TRUE)
        }
    })
    
    #CLUSTERS ARE DYNAMIC WITH NUMBER OF GROUPS FROM UI
    clstr <- reactive({
        clstr<-kmeans(cldat, input$clst_grps)
        clstr
    })
    
    #EXPLORE DATA FUNCTIONS
    # MAP USING PLOTLY - DISPLAYS STATUS BAR
    output$countryMap<-renderPlotly({
        #STATUS BAR PROGRESS FUNCTION    
        progress <- shiny::Progress$new(max=1)
        progress$set(message = "Opening App", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
            if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 8
            }
            progress$set(value = value, detail = detail)
        }
        compute_data(updateProgress)
        
        # START DRAWING INTERACTIVE MAP 
        # light grey boundaries
        l <- list(color = toRGB("grey"), width = 0.5)
        
        # specify map projection/options
        g <- list(
            showframe = TRUE,
            showcoastlines =TRUE,
            projection = list(type = 'Mercator')
        )
        
        p <- plot_geo(CountrySummaryWCode) %>%
            add_trace(
                z = ~TotalPlayers, color = ~TotalPlayers, colors = 'Blues',
                text = ~hover, locations = ~CODE, marker = list(line = l)
            ) %>%
            colorbar(title = 'T20 Number of Players') %>%
            layout(
                title = 'T20 Mens Player Analysis by Country<br>Source:<a href="https://docs.ropensci.org/cricketdata/">ESPNCricinfo data</a>',
                geo = g
            )
        p
    })
    
    # HISTOGRAM
    output$allPlayerHist<-renderPlot({
        df <- mt20_gt20 %>% 
            group_by(Country) %>%
            summarise(counts = n()) %>% arrange(desc(counts))
        df$Country <- factor(df$Country, levels = df$Country[order(df$counts, decreasing = TRUE)])
        ggplot(df, aes(x = Country, y = counts)) +
            geom_bar(fill = "#0073C2FF", stat = "identity") +
            geom_text(aes(label = counts), vjust = -0.3) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    # REACTIVE HISTOGRAM FOR COUNTRY ANALYSIS
    output$matchHist<-renderPlot({
        samples<-d()
        #Create histogram     
        hist(samples$Matches,main=paste("# of Players of", input$Country, "Country ", "with # of matches > ", input$nmatch),freq=TRUE,xlab="Number of Matches")
    })
    
    #NUMERIC SUMMARY
    output$num_summary <- renderPrint({
        num_sum<-select(d(), "Runs", "Matches", "Start", "End", "BallsFaced")
        summary(num_sum)
    })
    
    #DATA TABLE 
    output$table <- DT::renderDataTable(DT::datatable({
        d()
    }))
    
    #DOWNLOAD HISTOGRAM
    output$downloadPlot <- downloadHandler(
        filename = "hist.png",
        content = function(file) {
            samples<-d()
            png(file)
            hist(samples$Matches,main=paste("# of Players of", input$Country, "Country ", "with # of matches > ", input$nmatch),freq=TRUE,xlab="Number of Matches")
            dev.off()
        })
    
    #DOWNLOAD DATA TABLE
    output$downloadData <- downloadHandler(
        filename = "t20_players_data.csv",
        content = function(file) {
            write.csv(d(), file, row.names = TRUE)
        }
    )
    
    #CLUSTER CENTERS 
    output$CA_tab_kmean <- renderTable({
        clstr()$centers
    })
    
    #CLUSTER DIAGRAM   
    output$CA_plt_kmean<-renderPlot({
        plot(cldat,
             col = clstr()$cluster,
             pch = 20, cex = 2)
        text(cldat$StrikeRate, cldat$Runs, labels = row.names(cldat), pos = 4, cex=0.6)
        points(clstr()$centers, pch = 4, cex = 2, lwd = 2)
    })
    
    # HIERARCHIAL CLUSTER DENDOGRAM
    output$CA_plt_hier<-renderPlot({
        hierClust <- hclust(dist(cldat))
        plot(hierClust, xlab = "")
    })
    
    #LINEAR MODEL PREDICTION  
    output$lm_mod_pred<-renderUI({
        text<- paste0("Predicted Number of Runs are ",as.character(round(pred_runs()[1])), ".")
        text<- paste(text, "Below are linear model summaries")
        h3(text)
    })
    
    #LINEAR MODEL SUMMARY
    output$lm_summary <- renderPrint({
        summary(lm_mod())
    })
    
    # lINEAR MODEL PLOTS
    output$lm_mod_plt<-renderPlot({
        par(mfrow=c(2,2))
        plot(lm_mod())
    })  
    
    #DECISION TREE PREDICTION
    output$dt_mod_pred<-renderUI({
        text<- paste0("Predicted Number of Runs are ",as.character(round(pred_runs()[2])), ".")
        text<- paste(text, "Below are decision tree summaries")
        h3(text)
    })
    
    #DECISION TREE SUMMARY
    output$dt_summary <- renderPrint({
        summary(dt_mod())
    })
    
    # DECISION TREE GRAPH
    output$dt_mod_plt<-renderPlot({
        plot(dt_mod())
        text(dt_mod())
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
