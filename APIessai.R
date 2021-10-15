library(httr)
library(rjson)


ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
donneebr<-GET(ap)
rawToChar(donneebr$content)
liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précise
names(liste)
