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
paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=","2020-04-19",sep = "")

as.character.Date(date())


apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))


sel<-function(x){
  s<-x %>% as.data.frame() %>% select_if(is.numeric)
  s
}

sel_date<-function(x){
  s<-x %>% as.data.frame() %>% select(date)
  s
}

dd<-listedep$allDataByDepartement %>% sapply(sel)
head(dd)
dd[c(1:50)]

da<-listedep$allDataByDepartement %>% sapply(sel_date)
da %>% sapply("[") %>% t()

t(as.data.frame(da[-c(1:34)]))

df<-dd[-c(1:34)]
head(df)
t(df) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()

cbind(t(as.data.frame(da[-c(1:34)])),t(df) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t())

mef_don<-function(x){  #a mettre en reactive dans serveur en ajoutant sel et sel_date
  don<-x %>% sapply(sel)
  dat<-x %>% sapply(sel_date)

  don<-t(don[-c(1:34)]) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()
  dat<-((da[-c(1:34)]))
  #les 33 premieres lignes sont pourries et pas standardisees
  #a partir de la ligne 34 toutes les lignes ont la meme forme donc plus simple
  donnee<-data.frame(don,row.names = dat)
  donnee
}

mef_don(listedep$allDataByDepartement)
is.data.frame(data)
