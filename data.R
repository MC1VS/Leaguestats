################################################
# Liste des champions
################################################

champions<-lol.basechampions(serveur,key)
champions.noms<-champions[[1]]
champions.ids<- champions[[2]]
names(champions.ids)<-champions.noms

data.champions.stats<- reactive({
  table.stats<-data.result.stats()
  
  result.champions.stats <- table.stats[-which(table.stats[,1]!=0),]
  result.total.stats <- table.stats[which(table.stats[,1]==0),]
  result.stats<- list(result.champions.stats,result.total.stats)
})


output$statsPlot <- DT::renderDataTable(DT::datatable( {
  
  champions.all <-data.result.stats()
  #champions.total   <-data.champions.stats()[[2]]
  
  #libelle.champions <- champions.noms[match(champions.all[,1],champions.ids)]
  
  #if (input$data_type != "moy"){data.type <- cbind(libelle.champions,champions.all[,-1])}
  #else {
  # moy<-champions.all[,3:14]/champions.all[,2]
  #  data.type <- cbind(libelle.champions,champions.all[,2],moy,champions.all[,15])
  #}
  
  # affichage <- data.type
  
  # if (length(input$champions) != 0){ affichage <- data.type[data[,1]%in%input$champions,] }
  
  
  
  # affichage
  
  
}))

}) 
stats.table<- observe({
  
  result.stats<-lol.statsjoueur(id = data.id.joueur(),serveur = input$serveur,saison = input$saison,key = key())[[3]]
  table.stats<-matrix(data = unlist(result.stats),nrow = nrow(result.stats),ncol=length(unlist(result.stats))/nrow(result.stats),byrow = FALSE,dimnames = list(c(),names(cbind(result.stats[1],result.stats$stats))))
  data.stats <-as.data.frame(table.stats[,-c(8, 10:14, 17,21,23:34)])
  
})