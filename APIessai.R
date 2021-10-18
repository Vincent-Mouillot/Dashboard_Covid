#structure pour faire appel a l api: date ou dep en input on recupere avec filter(is="")
#la date ou le dep et on paste avec l url avant de faire l action button


library(httr)
library(rjson)


#API pour premier onglet
ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
donneebr<-GET(ap)
rawToChar(donneebr$content)
liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date precise
liste$allFranceDataByDate

dond<-sapply(liste$allFranceDataByDate,"[",c(4:9))
hos<-unlist(dond[1,])
sum(hos)

hos2<-liste$allFranceDataByDate %>%
  sapply("[",c(4:9)) %>%
  t() %>%
  as.data.frame() %>%
  select(hospitalises) %>%
  unlist() %>%
  sum()

hos3<-liste$allFranceDataByDate%>%sapply("[",4)%>%t()%>%unlist()%>%sum() #version a privilegier



#api pour deuxieme onglet
apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))
d<-listedep$allDataByDepartement[[536]]$date
paste(apdep,d,sep = " ")
