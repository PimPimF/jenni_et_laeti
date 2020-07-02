library(shiny)
library(tidyverse)
library(RMySQL)
library(glue)
library(DT)
library(shinythemes)

#ui.R

# connexion avec la base de données



# récupère les libellés des formations
recup_formation <- function() {
    
    tbl(con,
        sql(
            "SELECT id, libelle as formation FROM choix WHERE question_id=14 LIMIT 11"
        )) %>% collect
    
    
    
}


#récup nom de tous les élèves
recup_nom_etudiant <- function() {
   
    
    tbl(
        con,
        sql(
            "SELECT reponse_prenom.session_id as id, reponse_nom.texte AS nom, reponse_prenom.texte as prenom
                FROM reponse
                INNER JOIN reponse AS reponse_nom
                ON reponse_nom.session_id = reponse.session_id
                AND reponse_nom.question_id = 2
                INNER JOIN reponse AS reponse_prenom
                ON reponse_prenom.session_id = reponse.session_id
                AND reponse_prenom.question_id = 3
                WHERE reponse.question_id = 14"
        )
        
    ) %>% collect
    
}




ui<-shinyUI(fluidPage(
    theme = shinytheme("cerulean"),
    headerPanel( "Bilan fin de formation CEFIM"),
    
    sidebarPanel(
        conditionalPanel(
            condition = "input.tabselected ==1",
            
            selectInput("page", "Choisissez une formation",
                        
                        choices = c(
                            setNames(recup_formation()$id, recup_formation()$formation)
                        )),
            
            selectInput(
                "questions",
                "Questions",
                
                choices = "",
                selected = ""
                
            ),
            
            br(),
            
            sliderInput(
                "bins",
                "1. Sélection pour le visuel",
                min = 5,
                max = 25,
                value = 15
            ),
            
            br(),

                    ),
        
        
        
        conditionalPanel(condition = "input.tabselected ==2",
                         selectInput("name2", "Name",
                                     choices =  c(
                                         setNames(
                                             recup_nom_etudiant()$id,
                                             paste(recup_nom_etudiant()$nom, recup_nom_etudiant()$prenom, sep = " ")
                                         )
                                     ))),
        
    ),
    
    mainPanel(tabsetPanel(
        tabPanel(
            "Statistique Formations",
            value = 1,
            plotOutput("hist"),
            dataTableOutput("commentaires")
            
        ),
        
        
        
        tabPanel("Statistiques etudiants", value = 2,
                 dataTableOutput("df")),
        
        id = 'tabselected'
        
    ))
    
))

# server.R
dbGetQuery(con, "SET NAMES 'utf8'")

# tous les libellés des formations
recup_questions_formation <- function(choix_id) {
   
    
    tbl(con, sql(
        paste(
            "select distinct(question.id), libelle from reponse
                                inner join question on question.id = reponse.question_id
                                inner join session on session.id = reponse.session_id
                                WHERE session.id in (SELECT DISTINCT session.id
                                        FROM session
                                        inner join reponse on session.id = reponse.session_id
                                        where reponse.question_id = 14
                                        AND reponse.choix_id = ",
            choix_id,
            ")"
        )
    )) %>% collect
    
}

# toutes les questions score et affichage de l'histogramme

graphique <- function(id_question, bins) {
    #con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
    
    tbl(con, sql(
        paste(
            "select score from question
             inner join reponse
             on question.id = reponse.question_id
             and question.id =",
            id_question
        )
    )) %>% collect() %>%
        
        ggplot(aes(x = score)) +
        
        geom_histogram(bins = bins)
    
}



# récupère toutes les questions d'un étudiant

recup_quest_etudiant <- function(id) {
    #con <- dbConnect(MySQL(), host="localhost", user="root", password="root", dbname="evaluation")
    
    tbl(con, sql(
        paste(
            "SELECT question.libelle as question , concat(coalesce(reponse.texte,''),coalesce(reponse.date,''),coalesce(reponse.score,'')) as reponse from reponse
                        inner join question on question.id = reponse.question_id
                        inner join session on session.id = reponse.session_id
                        WHERE session.id in (SELECT DISTINCT session.id
                        FROM session
                        inner join reponse on session.id = reponse.session_id
                        where reponse.question_id = 14
                        and session.id =",
            id,
            ")"
        )
    )) %>% collect
    
}




# récupère toutes les questions de tous les étudiants
recup_ensemble_quest <- function(id_formation, id_question) {
    tbl(con,
        
        sql(
            paste(
                "select reponse.score, question.libelle, reponse.texte
                    FROM choix
                    INNER JOIN reponse as reponse_formation
                    ON reponse_formation.choix_id = choix.id
                    AND reponse_formation.question_id = 14
                    INNER JOIN reponse
                    ON reponse.session_id = reponse_formation.session_id
                    INNER JOIN question
                    ON question.id = reponse.question_id
                    INNER JOIN page
                    ON page.id = question.page_id
                        and choix.id = ",
                id_formation,
                " and reponse.question_id = ",
                id_question
            )
        )) %>% collect()
    
}



server<-function(session, input, output) {
    observeEvent(
        input$page,
        
        updateSelectInput(
            session,
            "questions",
            "question",
            
            choices =  c(
                setNames(
                    recup_questions_formation(input$page)$id,
                    recup_questions_formation(input$page)$libelle
                )
            ),
            

        )
        
    )
    
    # affichage de l'histogramme
    output$hist <- renderPlot({

        
        graphique(input$questions, input$bins)
        
    })
    
    
    # affichage de la table contenant les questions de l'étudiant avec les boutons permettant les exports dans différents formats
    
    output$df <- renderDataTable({
        datatable(
            recup_quest_etudiant(input$name2),
            extensions = "Buttons",
            options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
        )
        
    })
    
    
    
    # affichage des verbatims
    output$commentaires <- renderDataTable({
        recup_ensemble_quest(input$page, input$questions) %>%
            
            select(texte)
        
    })
    
    
    # permet de couper les connexions   
    onStop(function() {
        dbDisconnect(con)
        
        print("ok")
        
    })
    
    
    
}    

# Run the application 
shinyApp(ui = ui, server = server)
