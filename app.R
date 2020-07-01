library(shiny)
library(tidyverse)
library(RMySQL)
library(glue)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Réponse étudiant"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("etudiant",
                        "Nom de l'étudiant:",
                        value = "Nom de l'étudiant")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("df")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    con <- dbConnect(MySQL(), host="localhost", user="", password="", dbname="evaluation")
    
    etudiant_filter <- reactive({
        input$etudiant
        })
    req <- glue_sql(
        "SELECT session.id,
        concat(lpad(question.id, 3, '0'), ' ', question.libelle) as question,
        reponse.texte,
        reponse.date,
        reponse.score
        FROM reponse
        INNER JOIN question on question.id = reponse.question_id
        inner join session on session.id = reponse.session_id
        WHERE session.id in (select session.id
                             FROM session
                             INNER JOIN reponse AS reponse_nom
                             ON reponse_nom.session_id = session.id
                             AND reponse_nom.question_id = 2
                             INNER JOIN reponse AS reponse_prenom
                             ON reponse_prenom.session_id = session.id
                             AND reponse_prenom.question_id = 3
                             WHERE reponse_nom.texte = {input$etudiant()})",
        .con = con
    )

    output$df <- renderDataTable({
        
        
tbl(con,sql(req))%>%
            collect()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
