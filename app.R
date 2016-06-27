#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(curl)
library(httr)
library(DT)
source("fonctions_lol.R")



# Define UI for application that draws a histogram
ui <- shinyUI(
  
  #incrementation de la barre de navigation
  navbarPage("barre de navigation",
             
     #premiere page
      tabPanel("player's champion's stat",
            #en tete html
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
               tags$br()
               ),
  
            # header
            tags$header(titlePanel("LoL - Stats"),
            tags$img(height=100, width = 100, src="http://ddragon.leagueoflegends.com/cdn/6.12.1/img/champion/Ashe.png ")),
             # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  textInput(inputId = "pseudo",label = "Pseudo : ",value = "Kazeel"),
                  selectizeInput('serveur', 'Serveur : ', choices = c("euw","na")),
                  selectizeInput('saison', 'Saison : ', choices = c(2016,2015)),
                  actionButton("search", "Search"),
                  width = 2 ),
    
            # Show a plot of the generated distribution
              mainPanel(
                #creation balise article pour visualisation du tableau
                tags$article(dataTableOutput("statsTable")))
              )
            ),
     #deuxieme page
     tabPanel("2player's champion's stat",
          tags$b("2eme page"))
    #fermeture barre naviggation
    )
  #fermeture shiny
  )



# Define server logic required to draw a histogram
server <- shinyServer(function(input, output){
  
  key<- reactive({ 
    key    <- "3492a73d-8bac-44e4-97ca-018d3f9b52fa"
  })
  
  champions.liste <- reactive({
    champions<-lol.basechampions(input$serveur,key())
    champions.noms<-champions[[1]]
    champions.ids<- champions[[2]]
    names(champions.ids)<-champions.noms
    
    liste<-list(champions.noms,champions.ids)
  })
  
  names.stats <- reactive({
    names<- c("champions","Games","Loose", "Win", "kills", "Dmg Dealt", "Dmg Taken",
              "Sbires", "Death", "Gold", "Turret", "Physic Dmg",
              "Magic Dmg", "Assist")
    
  })
  
  id.joueur<-eventReactive(input$search,{
    result.id<-lol.idjoueur(input$pseudo, input$serveur, input$saison, key())
    
    id<-result.id[[1]][[1]]
  })
  
  stats.player<- reactive({
    
    stats<-lol.statsjoueur(id = id.joueur(),serveur = input$serveur,saison = input$saison,key = key())[[3]]
    table.stats<-matrix(data = unlist(stats),nrow = nrow(stats),ncol=length(unlist(stats))/nrow(stats),byrow = FALSE,dimnames = list(c(),names(cbind(stats[1],stats$stats))))
    
    data.stats <-as.data.frame(table.stats[,-c(8, 10:14, 17,21,23:35)])
    names(data.stats)<-names.stats()
    id.champion<-champions.liste()[[2]]
    nom.champion<-champions.liste()[[1]]
    table.champion<-cbind(id.champion,nom.champion)
    nom.champion.play<-match(data.stats[,1],table.champion[,1])
    data.stats[,1]<-table.champion[,2][nom.champion.play]
    data.stats
  })
  
  output$statsTable <- DT::renderDataTable(DT::datatable({
    stats.player()
  }))
  
})
# Run the application 
shinyApp(ui = ui, server = server)
