library(httr)
library(rjson)

api<-"https://github.com/florianzemma/CoronavirusAPI-France"
brut<-GET(api)
rawToChar(brut$content)

ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
bt<-GET(ap)

apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
