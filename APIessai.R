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


apdep<-paste("https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=",
             as.character("Rhône"), #changer avec input mais recup liste dep avant
             sep = "")
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))
glimpse(listedep$allDataByDepartement)

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
unlist(da)
t(as.data.frame(da[-c(1:34)]))

df<-dd[-c(1:34)]
head(df)
df<-t(df) %>% sapply("[",c(1:6))%>% as.data.frame()%>% t() %>% unlist()

colnames(df)
head(df[,4])

cbind(t(as.data.frame(da[-c(1:34)])),t(df) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t())

mef_don<-function(x){  #a mettre en reactive dans serveur en ajoutant sel et sel_date
  don<-x %>% sapply(sel)
  dat<-x %>% sapply(sel_date)

  don<-t(don[-c(1:34)]) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()
  cname<-colnames(don)
  don<-unlist(don)
  dat<-((dat[-c(1:34)]))
  #les 33 premieres lignes sont pourries et pas standardisees
  #a partir de la ligne 34 toutes les lignes ont la meme forme donc plus simple
  donnee<-don %>% matrix(ncol = 6) %>% data.frame(row.names = dat)
  colnames(donnee)<- cname

  seqD<-seq.Date(from=as.Date("2020-03-29"),to=as.Date("2020-04-01"),by=1)
  donnee[c(as.character( seqD)),]
}

mf<-mef_don(listedep$allDataByDepartement)
glimpse(mf)

head(mf)

seqD<-seq.Date(from=as.Date("2020-03-29"),to=as.Date("2020-04-01"),by=1)

mf[c(as.character( seqD)),]

as.Date("2020-03-29")


mef_don2<-function(x,date_depart,date_fin){  #a mettre en reactive dans serveur en ajoutant sel et sel_date
  don<-x %>% sapply(sel)
  dat<-x %>% sapply(sel_date)

  don<-t(don[-c(1:34)]) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()
  cname<-colnames(don)
  don<-unlist(don)
  dat<-dat[-c(1:34)]
  #les 33 premieres lignes sont pourries et pas standardisees
  #a partir de la ligne 34 toutes les lignes ont la meme forme donc plus simple
  donnee<-don %>% matrix(ncol = 6) %>% data.frame(row.names = dat)
  colnames(donnee)<- cname

  seqD<-seq.Date(from=as.Date(date_depart),to=as.Date(date_fin),by=1)
  d<-donnee[c(as.character( seqD)),]
  glimpse(d)
  d
}

try<-mef_don2(listedep$allDataByDepartement,"2020-03-28","2021-08-12")

x<-listedep$allDataByDepartement
date_depart<-"2020-03-29"
date_fin<-"2020-04-01"


mef_don_dep<-function(x,date_depart,date_fin){
  x<-x[-c(1:34),c(1,10,8,7,11,12,13)]
  donnee<-x %>% data.frame(row.names = x$date) %>% select(-date)

  seqD<-seq.Date(from=as.Date(date_depart),to=as.Date(date_fin),by=1)
  d<-donnee[c(as.character( seqD)),]
  glimpse(d)
  d
}

ess<-mef_don_dep(listedep$allDataByDepartement,"2020-03-28","2021-08-12")
cg<-as.Date(rownames(ess))
class(cg)
