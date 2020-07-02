library(shiny)
library(tidyverse)
library(RMySQL)
library(glue)
library(DT)

# Define UI for application that draws a histogram
ui <-shinyUI(
    navbarPage("Bilan Final - CEFIM",
               tabPanel(
                   "Stats par étudiants",
                   sidebarLayout(
                       sidebarPanel(
                           textInput("etudiants", "Nom de l'etudiant", ""),
                           # Make a list of checkboxes
                           
                       ),
                       mainPanel( dataTableOutput("df" ))
                   )
               ),
               tabPanel(
                   "Stats par formation",
                   sidebarLayout(
                       sidebarPanel(
                           selectInput("formations", "Choix de la formation", c("A", "B", "C")),
                           
                       ), 
                       mainPanel( dataTableOutput("zig" ))
                   )
               ))) 

# Define server logic required to draw a histogram
dbGetQuery(con, "SET NAMES 'utf8'")

server<-shinyServer(function(input, output, session) {
    output$value <- renderText({
        input$etudiants
    })
    
    output$df <- renderDataTable({
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
                             WHERE reponse_nom.texte = {input$etudiants})",
            .con = con
        )
        tbl(con, sql(req)) %>%
            
            collect()
    })
    
    observeEvent(input$etudiants, {
        req2 <- glue_sql(
            "SELECT DISTINCT choix.libelle as libelle, choix.id
                    FROM choix
                    inner join reponse on choix_id = choix.id
                    where reponse.question_id = 14",
            
            .con = con
        )
        
        mes_choix <- tbl(con, sql(req2)) %>%
            
            collect() %>%
            
            pull(libelle)
        print(mes_choix)
        
        updateSelectInput(session, "formations", choices = mes_choix)
    })
})


onStop(function() {
    dbDisconnect(con)
    
})    

# Run the application 
shinyApp(ui = ui, server = server)
