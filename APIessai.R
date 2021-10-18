library(httr)
library(rjson)

api<-"https://github.com/florianzemma/CoronavirusAPI-France"
brut<-GET(api)
rawToChar(brut$content)

ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
bt<-GET(ap)

apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"


ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
donneebr<-GET(ap)
rawToChar(donneebr$content)
liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précise
names(liste)


apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))
