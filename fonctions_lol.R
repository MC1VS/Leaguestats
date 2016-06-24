#############################################################################
#Fonctions LOL
#############################################################################

######################################################
#lol.idjoueur
######################################################
# Renvois à partir du pseudo l'id du joueur
# Il faut renseigner le serveur comme euw, eune etc...
# La saison est désigné en année
# La key est la clef d'utilisation des api persos
lol.idjoueur <- function(pseudo, serveur, saison, key){
  
  fichier.json<-paste("https://",serveur,".api.pvp.net/api/lol/",serveur,"/v1.4/summoner/by-name/",pseudo,"?api_key=",key,sep="")
  liste<- fromJSON(fichier.json)
  return(liste)
  
}
######################################################
#lol.statsjoueur
#####################################################
lol.statsjoueur <- function(id, serveur, saison, key){
  
  fichier.json <-paste("https://",serveur,".api.pvp.net/api/lol/",serveur,"/v1.3/stats/by-summoner/",id,"/ranked?season=SEASON",saison,"&api_key=",key,sep="")
  liste<-fromJSON(fichier.json)
  return(liste)
  
}
#####################################################
#lol.basechampions
###################################################
#Les infos de base sur un champions

lol.basechampions<- function(serveur, key){
  
  fichier.json <-paste("https://global.api.pvp.net/api/lol/static-data/",serveur,"/v1.2/champion?api_key=",key,sep="")
  liste<-fromJSON(fichier.json)
  champions<-liste$data
  
  names<-c()
  ids<- c()
  for (i in 1:length(champions)){
    names<-append(names,champions[[i]]$name)
    ids<-append(ids,champions[[i]]$id)
  }
  
  return(list(names,ids))
}