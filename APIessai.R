library(httr)
library(rjson)

<<<<<<< HEAD
api<-"https://github.com/florianzemma/CoronavirusAPI-France"
brut<-GET(api)
rawToChar(brut$content)

ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
bt<-GET(ap)

apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
=======

ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
donneebr<-GET(ap)
rawToChar(donneebr$content)
liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précise
names(liste)


apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))
>>>>>>> ba2dd41fd002b674a9898f3f8901695ea106e232
