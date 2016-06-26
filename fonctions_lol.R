#############################################################################
#Fonctions LOL
#############################################################################
# Les fonctions necessite les packages :

# library(jsonlite)
# library(curl)
# library(httr)
#############################################################################
# lol.idjoueur : renvoie l'id d'un joueur avec son pseudo
# lol.statsjoueur : renvoie les stats d'un joueur avec son id
# lol.statsjoueur.clean : renvoie une matrice propre de stats depuis le 3eme element de resultat de la fonction lol.statsjoueur
# lol.basechampions : renvoie la liste des champions

# lol.staticdata.version : renvoie le numeros de la derniere version du site des donnees fixes

######################################################
#lol.idjoueur
######################################################
# Renvois a partir du pseudo l'id du joueur
# Il faut renseigner le serveur comme euw, eune etc...
# La saison est designe en année
# La key est la clef d'utilisation des api persos

lol.idjoueur <- function(pseudo, serveur, saison, key){
  
  fichier.json<-paste("https://",serveur,".api.pvp.net/api/lol/",serveur,"/v1.4/summoner/by-name/",pseudo,"?api_key=",key,sep="")
  liste<- fromJSON(fichier.json)
  return(liste)
  
}
# Le resultat est une liste avec 1 element qui a pour nom le pseudo du joueur
# Cet element est une liste avec 5 elements
# "id" qui contient l'id du joueur
# "name" qui contient le pseudo du joueur
# "profileIconId" qui contient le numeros de l'icone d'invocateur
# "summonerlevel" qui contient le niveau du joueur
# "revisionDate" contient un nombre qui doit représenter la date

######################################################
#lol.statsjoueur
######################################################
# Renvoie les statistiques sur les champions en partie classee d'un joueur à partir de son id
# Les champions sont designes en fonction de leurs id
# Il faut renseigner le serveur comme euw, eune etc...
# La saison est designe en année
# La key est la clef d'utilisation des api persos

lol.statsjoueur <- function(id, serveur, saison, key){
  
  fichier.json <-paste("https://",serveur,".api.pvp.net/api/lol/",serveur,"/v1.3/stats/by-summoner/",id,"/ranked?season=SEASON",saison,"&api_key=",key,sep="")
  liste<-fromJSON(fichier.json)
  return(liste)
  
}

# La fonction renvoie une liste de 3 elements
# "summinerId" avec l'Id du joueur
# "modifyDate" qui renvoie la date de derniere modification
# "champions" qui renvoie une liste dans une liste, c'est vraiment bordelique, alors il y a une fonction pour gerer cette partie la

#######################################################
#lol.statsjoueur.clean
######################################################
# Le principe est de nettoyer les informations reçus par lol.statjoueur
# Pour cela, il faut y rentrer le tableau "champions" obtenu par lol.statsjoueur

lol.statsjoueur.clean<- function(statsjoueur){
  data <- unlist(statsjoueur)
  nrow <- nrow(statsjoueur)
  ncol <- length(unlist(statsjoueur))/nrow
  dimnames <- list(c(),names(cbind(statsjoueur[1],statsjoueur$stats)))
  stats.table<-matrix(data = data,nrow = nrow,ncol= ncol,byrow = FALSE,dimnames = dimnames)
  
  return(stats.table)
}

# La fonction renvoie une matrice 
# En ligne les champions, le chmapion 0 est le total
# En colonne les différentes variables (voir sur api riot)
#####################################################
#lol.basechampions
###################################################
#La fonction renvoie la liste des champions

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

# La fonction renvoie une liste
# Le premier element est un vecteur de chaine de character avec le nom des champions
# Le deuxieme un vecteur avec le numero des champions dans l'ordre du premier vecteur
#####################################################
#lol.staticdata.version
#####################################################
# La fonction renvoie la derniere version du site de donnéees (images, icones...)

lol.staticdata.version<- function(server, key){
  fichier.json <-paste("https://global.api.pvp.net/api/lol/static-data/",serveur,"/v1.2/versions?api_key=",key,sep="")
  vecteur<-fromJSON(fichier.json)
  
  return(vecteur)
}

# La fonction retroune un vecteur character avec l'ensemble des numeros de versions, le [1] est la version actuelle
###################################################
#lol.staticdata.image
####################################################
# La fonction renvoie l'adresse de l'image d'un champion
# Les parametres sont : 
# champion : le nom du champion (chaine de charactere)
# 
# version : la version de staticdata (utiliser la fonction lol.staticdata.version)


lol.staticdata.square<- function(){
  
}