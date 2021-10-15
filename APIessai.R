library(httr)
library(rjson)

api<-"https://github.com/florianzemma/CoronavirusAPI-France"
brut<-GET(api)
