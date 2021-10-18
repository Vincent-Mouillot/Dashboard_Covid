library(httr)
library(rjson)

#ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
#ceci permet d'obtenir les valeurs pour 19 avril 2020

# pour une date que l'utilisateur choisira :
date<-"2020-04-19" # devra être modifier lors de l'ajout dans l'ui/server
api_date<- paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=", date)
Brut_api_date<-GET(api_date)
liste_date<-fromJSON(rawToChar(Brut_api_date$content), method = "C") #je sais j'étudie la question
names(liste_date) #devrait obtenir "code, nom, date, hospitalisation, reanimation etc
#et non allFranceDataByDate : probleme dans GET et dans le format de Rjson




ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
donneebr<-GET(ap)
rawToChar(donneebr$content)
liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précise


hos<-liste$allFranceDataByDate%>%sapply("[",4)%>%as.data.frame()%>%sum()

dep<-"Rhône"
apdep<-paste("https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône", dep)
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))
df_bydep<-listedep$allDataByDepartement %>% sapply("[", 4) %>% as.data.frame() %>% sum()
