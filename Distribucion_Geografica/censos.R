library(tidyverse)
library(readxl)
path_plot<-"plots/censos"
dir.create(path_plot)
censos<-read_csv("data/Censos 1970-2010.txt")
names(censos)
censos2<-censos%>%mutate(prop_censo_1970=censo_1970*100/sum(censos$censo_1970), prop_censo_2010=censo_2010*100/sum(censos$censo_2010))
haplotipos<-read_tsv("data/2021.08.26 - Haplotipos ADNmt.txt") #sin tildes
Provincias<-haplotipos$Residencia%>%unique()%>%.[1:24]%>%sort()
Region<-c("Buenos Aires", "CABA", "Noroeste", "Noreste", "Patagonia", "Centro", "Noreste", "Noreste", "Noreste", "Noroeste", "Centro", "Noroeste", "Centro", "Noreste", "Patagonia", "Patagonia", "Noroeste", "Noroeste", "Centro", "Patagonia", "Noreste", "Noroeste", "Patagonia" , "Noroeste")
Prov_Reg<-data.frame(Provincias, Region)
haplotipos<-left_join(haplotipos,Prov_Reg, by=c("Nacimiento"="Provincias") )%>%rename(Region_nac="Region")
haplotipos<-left_join(haplotipos,Prov_Reg, by=c("Residencia"="Provincias") )%>%rename(Region_res="Region")

data_prov<-select(haplotipos, Nacimiento, Residencia)
sum_data<-data_prov%>%group_by(Nacimiento, Residencia)%>%summarise(value=n())
sum(sum_data$value)

sum_data<-sum_data%>%group_by(Nacimiento)%>%mutate(sum_nac=round(sum(value)*100/(nrow(haplotipos)),3))%>%ungroup()
sum_data<-sum_data%>%group_by(Residencia)%>%mutate(sum_res=round(sum(value)*100/(nrow(haplotipos)),3))%>%ungroup()
sum_data<-sum_data%>%mutate(migra=Nacimiento!=Residencia)
sum_data<-sum_data%>%group_by(Nacimiento, migra)%>%mutate(migra_nac=sum(value), migra_nac_rel=migra_nac/sum_nac)%>%ungroup()
sum_data<-sum_data%>%group_by(Residencia, migra)%>%mutate(migra_res=sum(value), migra_res_rel=migra_res/sum_res)%>%ungroup()

complete_df<-full_join( sum_data,censos2[,c(1,4)], by=c("Nacimiento"="Provincia"))
complete_df<-full_join( complete_df,censos2[,c(1,5)], by=c("Residencia"="Provincia"))

png(paste0(path_plot, "/1970_nacimientos_prov.png"))
complete_df%>%pivot_longer(cols=c(sum_nac,prop_censo_1970), values_to = "pobl_1970", names_to = "origen")%>%
  ggplot(aes(Nacimiento, pobl_1970, fill=origen))+geom_bar(stat="identity", position="dodge")+theme(axis.text.x   = element_text(angle = 90))
dev.off()
png(paste0(path_plot, "/2010_residencias_prov.png"))
complete_df%>%pivot_longer(cols=c(sum_res,prop_censo_2010), values_to = "pobl_2010", names_to = "origen")%>%
  ggplot(aes(Residencia, pobl_2010, fill=origen))+geom_bar(stat="identity", position="dodge")+theme(axis.text.x   = element_text(angle = 90))
dev.off()

complete_df%>%select(Nacimiento, sum_nac, prop_censo_1970)%>%unique()->Nac
chisq.test(Nac$sum_nac,Nac$prop_censo_1970)
complete_df%>%select(Residencia, sum_res, prop_censo_2010)%>%unique()->Res
chisq.test(Res$sum_res,Res$prop_censo_2010)


nac_df<-haplotipos%>%group_by(Nacimiento)%>%summarise(abs_nac=n())
res_df<-haplotipos%>%group_by(Residencia)%>%summarise(abs_res=n(), Region=Region_res)%>%unique()
nac_res_df<-full_join(nac_df, res_df, by=c("Nacimiento"="Residencia"))%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate(migracion_neta=abs_res-abs_nac, migracion_rel=migracion_neta/abs_res)
nac_res_df<-nac_res_df%>%group_by(Region)%>%
  mutate(abs_nac_reg=sum(abs_nac),abs_res_reg=sum(abs_res), migr_neta_reg=abs_res_reg-abs_nac_reg, migr_rel_reg=migr_neta_reg/abs_res_reg)
nac_res_df<-full_join(censos2, nac_res_df, by=c("Provincia"="Nacimiento"))%>%
  mutate(migr_neta_censo=censo_2010-censo_1970, migr_rel_censo=migr_neta_censo/censo_2010,
         migr_rel2_censo=migr_rel_censo-mean(migr_rel_censo))
nac_res_df%>%pivot_longer(cols=c(migracion_rel, migr_rel2_censo), names_to  = "Origen", values_to = "Migracion_neta_rel")%>%
  ggplot(aes(Provincia, Migracion_neta_rel, fill=Origen))+
  geom_bar(stat="identity", position="dodge")+
  theme(axis.text.x   = element_text(angle = 90))+
  xlab("Provincia")

nac_res_reg<-nac_res_df%>%group_by(Region)%>%summarise(censo_1970_reg=sum(censo_1970), censo_2010_reg=sum(censo_2010), rel1970_reg=100*sum(prop_censo_1970/100), rel2010_reg=100*sum(prop_censo_2010/100), nac_rel_reg=abs_nac_reg*100/2709, res_rel_reg=abs_res_reg*100/2709 )
png(paste0(path_plot, "/1970_nac_reg.png"))
nac_res_reg%>%pivot_longer(cols=c(rel1970_reg,nac_rel_reg ), names_to="Origen", values_to= "Poblacion")%>%
  ggplot(aes(Region, Poblacion, fill=Origen))+geom_bar(position="dodge", stat="identity")
dev.off()
png(paste0(path_plot, "/2010_residencias_reg.png"))
nac_res_reg%>%pivot_longer(cols=c(rel2010_reg,res_rel_reg ), names_to="Origen", values_to= "Poblacion")%>%
  ggplot(aes(Region, Poblacion, fill=Origen))+geom_bar(position="dodge", stat="identity")
dev.off()

######

GF<-read_xlsx("data/Distribucion_GF.xlsx")
GF<-select(GF, LEG., Provincia)%>%filter(!is.na(Provincia))%>%count(Provincia)%>%mutate(pobl_1970=n*100/sum(n))%>%mutate(origen="Lugar_desaparicion")%>%select(-n,Provincia, origen, pobl_1970)%>%rename(Nacimiento=Provincia)
ggplot(GF, aes(Provincia, pobl_1970))+geom_col()
complete_df2<-complete_df%>%select(Nacimiento, sum_nac, prop_censo_1970)%>%pivot_longer(cols=c(sum_nac,prop_censo_1970), values_to = "pobl_1970", names_to = "origen")%>%rbind(GF)
ggplot(complete_df2, aes(Nacimiento, pobl_1970, fill=origen))+geom_col(position="dodge")+theme(axis.text.x = element_text(angle = 90))
