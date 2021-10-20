library(httr)
library(rjson)




ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
donneebr<-GET(ap)

liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précise

attributes(liste)

dat<-liste$allFranceDataByDate[,c(4:7)]

sumna<-function(x){
  sum(x,na.rm = TRUE)
}



sumd<-apply(dat,2,sumna)

select(sumd,reanimation)

attributes(dat)
dat$hospitalises


hos<-liste$allFranceDataByDate$hospitalises%>%sum()
sum(hos$hospitalises)
hos[,4]
liste$allFranceDataByDate[4]%>%unlist()
liste$allFranceDataByDate$hospitalises[4]

liste$allFranceDataByDate[4,4]

rawToChar(donneebr$content)
liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précise


hos<-liste$allFranceDataByDate%>%sapply("[",4)%>%as.data.frame()%>%sum()



apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))


paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=","2020-04-19",sep = "")

as.character.Date(date())
