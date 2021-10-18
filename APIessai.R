library(httr)
library(rjson)

#ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
#ceci permet d'obtenir les valeurs pour 19 avril 2020

# pour une date que l'utilisateur choisira :
date<-"2020-04-19" # devra être modifier lors de l'ajout dans l'ui/server
api_date<- paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=", date)
Brut_api_date<-GET(api_date)
rawToChar(Brut_api_date$content)
liste_date<-fromJSON(rawToChar(Brut_api_date$content), method = "C") #je sais j'étudie la question
names(liste_date) #devrait obtenir "code, nom, date, hospitalisation, reanimation etc
#et non allFranceDataByDate : probleme dans GET et dans le format de Rjson

hos<-liste$allFranceDataByDate%>%sapply("[",4)%>%as.data.frame()%>%sum()
#On obtient la liste des infos par dep pour une date précise

dep<-"Rhône"
api_dep<-paste("https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=", dep)
donneedep<-GET(api_dep)
rawToChar(donneedep$content)
listedep<-fromJSON(rawToChar(donneedep$content))
df_bydep<-listedep$allDataByDepartement %>% sapply("[", 4) %>% as.data.frame() %>% sum()
