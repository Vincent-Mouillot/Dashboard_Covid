sel<-function(x){
  s<-x %>% as.data.frame() %>% select_if(is.numeric)
  s
}

sel_date<-function(x){
  s<-x %>% as.data.frame() %>% select(date)
  s
}

mef_don<-function(x,date_depart,date_fin){  #a mettre en reactive dans serveur en ajoutant sel et sel_date
  glimpse(x)
  don<-x %>% sapply(sel)
  glimpse(don)
  dat<-x %>% sapply(sel_date)
  glimpse(dat)

  don<-t(don[-c(1:34)]) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()
  glimpse(don)
  cname<-colnames(don)
  don<-unlist(don)
  dat<-((da[-c(1:34)]))
  #les 33 premieres lignes sont pourries et pas standardisees
  #a partir de la ligne 34 toutes les lignes ont la meme forme donc plus simple
  donnee<-don %>% matrix(ncol = 6) %>% data.frame(row.names = dat)
  colnames(donnee)<- cname

  seqD<-seq.Date(from=as.Date(date_depart),to=as.Date(date_fin),by=1)
  d<-donnee[c(as.character( seqD)),]
  glimpse(d)
  d
}
