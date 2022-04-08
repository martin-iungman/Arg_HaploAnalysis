library(tidyverse)
library(rlist)

base<-read_tsv("2021.08.26 - Haplotipos ADNmt.txt")
names(base)[1]<-"Muestra"
mutaciones<-c()
for (i in seq(9, 40)){
  mutaciones<-append(mutaciones,as_vector(unique(base[,i])))%>%str_squish()%>%unique()
}


unique(mutaciones)%>%length()
mutaciones_f<-mutaciones[-grep(pattern="^-(309|573).", mutaciones)]
posiciones<-mutaciones_f%>%str_extract("[:digit:]+")%>%as.numeric()
posic_df<-cbind(mutaciones_f,posiciones)%>%as_tibble()
posic_df$posiciones<-as.numeric(posic_df$posiciones)
posic_df<-posic_df%>%arrange(posiciones)

base2<-base[,1:42]
nombres<-base2[,1]%>%as_vector()
base_df.list<-map(nombres, ~base2%>%filter(Muestra==.x))
base_mut.list<-map(base_df.list, ~.x[1,9:40]%>%transpose()%>%unlist()%>%as_vector())

base_wide<-tibble()
for(i in seq(1, nrow(base2))){
  mut<-rep(0, times=length(posic_df$mutaciones_f))
  mut[which(posic_df$mutaciones_f%in%base_mut.list[[i]])]<-1
  base_wide<-rbind(base_wide, mut)
}
names(base_wide)<-posic_df$mutaciones_f
base_wide<-cbind(base2[, c(1:6,41,42)], base_wide)
write_tsv(base_wide, "Haplotipos_M_binario.tsv")
