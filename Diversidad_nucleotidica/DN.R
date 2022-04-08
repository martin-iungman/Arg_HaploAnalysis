library(tidyverse)
bin<-read_tsv("Haplotipos_M_binario.tsv")
Provincias<-bin$Residencia%>%unique()%>%.[1:24]%>%sort()
Region<-c("Buenos Aires", "CABA", "Noroeste", "Noreste", "Patagonia", "Centro", "Noreste", "Noreste", "Noreste", "Noroeste", "Centro", "Noroeste", "Centro", "Noreste", "Patagonia", "Patagonia", "Noroeste", "Noroeste", "Centro", "Patagonia", "Noreste", "Noroeste", "Patagonia" , "Noroeste")
Prov_Reg<-data.frame(Provincias, Region)
bin<-bin%>%left_join(Prov_Reg, by=c("Nacimiento"="Provincias"))%>%rename(Region_nac=Region)%>%
  left_join(Prov_Reg, by=c("Residencia"="Provincias"))%>%rename(Region_res=Region)
bin<-bin%>%select(Muestra:Origen, Region_nac,Region_res, T3C:A16559G)
calculate_DN<-function(bin, col, value="all"){
  diversidad<-list()
  if(value=="all"){
    value=unique(bin[[{{col}}]])[1]
  }
  for (group in value){
    print(group)
    div_tot=0
    sub_bin<-bin%>%filter({{col}}==group)
    div_vector<-rep(NA,(nrow(sub_bin)^2-nrow(sub_bin))/2)
    n<-nrow(sub_bin)
     for (i in 1:n){
      for (j in 1:n){
        if(i>j){
           div_vector[length(div_vector[!is.na(div_vector)])+1]<-sum(abs(as.vector(sub_bin[i, 11:ncol(sub_bin)])-as.vector(sub_bin[j, 11:ncol(sub_bin)])))
         }
       }
     }
     diversidad[[group]]$DN<-sum(div_vector*(1/n)^2)*(n/(n-1))
     diversidad[[group]]$Mismatch<-div_vector
     diversidad[[group]]$n<-nrow(sub_bin)
   }
  return(diversidad)
}

ncores=detectCores()-1
plan(multisession, workers=ncores)
diversidad<-future_map(unique(bin[["Region_nac"]]), ~calculate_DN(bin, Region_nac, .x, value="all")  )
saveRDS(diversidad, "lista_DN_Region_nac.rds")

df<-left_join(map_dfc(diversidad, ~.x$n)%>%t()%>%as.data.frame()%>%rownames_to_column(var="Provincia_Nacimiento")%>%rename(n=V1),
map_dfc(diversidad, ~.x$DN)%>%t()%>%as.data.frame()%>%rownames_to_column(var="Provincia_Nacimiento")%>%rename(DN=V1), by="Provincia_Nacimiento")

df%>%
  ggplot(aes(Provincia_Nacimiento,DN))+geom_col(fill="darkblue")+theme(axis.text.x = element_text(angle=90))

diversidad[[23]]$DN_vector%>%as.data.frame()%>%rename(DN_pairwise='.')%>%mutate(DN_pairwise=DN_pairwise/((1/diversidad[[23]]$n)^2))%>%ggplot(aes(DN_pairwise))+geom_density()
