library(shiny)
library(tidyverse)
library(RMySQL)
library(glue)
library(DT)

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
    headerPanel("Bilan fin de formation CEFIM"),
    
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
