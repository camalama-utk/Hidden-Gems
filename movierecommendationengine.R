# Load required library
library(shiny)
library(arules)
library(tidyverse)
library(regclass)

load("MOVIERECFall2023.RData")


# Define UI for the Shiny app
ui <- fluidPage(

  titlePanel("Hidden Gem Recommendations App"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting games
      selectizeInput(inputId="movies",label="What movies do you like?",choices=NULL,multiple=TRUE),   
      
      # Numeric input for the number of recommendations
      numericInput("number",
                   label = "Enter the number of recommendations",
                   value = 10, min = 1, max = 100),
      
      # Slider input for confidence of recommendations
      sliderInput("confidence",
                  label = "Minumum Level of Confidence",
                  value = 20, min = 1, max = 100),
      
      # Slider input for popularity cap
      numericInput("popularity",
                  label = "Max % of user who have rated the movie? (0-30)",
                  value = 1, min = 0.01, max = 30),
      
      submitButton("Get recommendations!")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      dataTableOutput("recommendations")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  updateSelectizeInput(session, "movies", choices = sort(POPULARITY$title), server = TRUE)  
  
  output$recommendations <- renderDataTable({
    
    movies.too.popular <- POPULARITY$title[ which(POPULARITY$PercentSeen > input$popularity) ]
    dont.consider.movies <- setdiff( movies.too.popular , input$movies )  

    RULES <- apriori(TRANS,parameter = list(supp=4/length(TRANS),conf=input$confidence/100,minlen=2,maxtime=0),
                     appearance = list(none=dont.consider.movies,lhs=input$movies,default="rhs"),
                     control=list(verbose=FALSE)) 

    if ( length(RULES) == 0 ) { return( data.frame(message="No recommendations with these parameters.  Add more movies, decrease confidence, or increase popularity!") )  }
    
    RULES <- RULES[is.significant(RULES,TRANS)]
    
    RULESDF <- DATAFRAME(RULES, separate = TRUE, setStart = '', itemSep = ' + ', setEnd = '')
    
    legit.recommendations <- setdiff( RULESDF$RHS, input$movies) 
    RULESDF <- RULESDF %>% filter(RHS %in% legit.recommendations)
    
    if ( nrow(RULESDF) == 0 ) { return( data.frame(message="No recommendations with these parameters.  Add more movies, decrease confidence, or increase popularity!") )  }
    
    RECS <- RULESDF %>% group_by(RHS) %>% summarize(max(confidence)) 
    
    RESULTS <- RECS %>%  left_join(POPULARITY,by=c("RHS"="title"))
    names(RESULTS) <- c("Movie","Confidence","PercentSeen","imdbRating","Year")
    RESULTS <- RESULTS %>% arrange( desc(Confidence) )
    RESULTS <- RESULTS %>%  head(input$number)
    RESULTS$Movie <- as.character( RESULTS$Movie )
    RESULTS$Year <- NULL
    row.names(RESULTS) <- NULL
    RESULTS
  })
  
  
    # Add server code here if needed
}

# Run the application
shinyApp(ui, server)
