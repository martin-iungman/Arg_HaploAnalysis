library(readxl)
library(tidyverse)
library(networkD3)
library(htmlwidgets)

data<-read_excel("Hg ADNmt vs. Hg CrY.xlsx")[,-c(2, 6,8,9,10,12)]
names(data)<-c("Muestra","Nacimiento", "Residencia","Hg_mat", "origen_mt", "Hg_pat", "origen_Y")
data_net<-select(data, starts_with("Origen"))%>%filter(origen_mt!="Desconocido")
sum_data<-data_net%>%group_by(origen_mt, origen_Y)%>%summarise(value=n())

sum_data<-sum_data%>%group_by(origen_mt)%>%mutate(mean_mt=round(sum(value)*100/sum(sum_data$value),2))%>%ungroup()
sum_data<-sum_data%>%group_by(origen_Y)%>%mutate(mean_Y=round(sum(value)*100/sum(sum_data$value),2))%>%ungroup()
sum_data2<-sum_data%>%mutate(origen_mt=paste0(origen_mt, "\n " , mean_mt, "%" ), origen_Y=paste0(origen_Y," ", mean_Y, "%" ))

nodes <- data.frame(
  name=c(as.character(sum_data2$origen_mt), 
         as.character(sum_data2$origen_Y)) %>% unique()
)
nodes<-cbind(nodes,group=as.factor(c("AF","EU","NA","AF","EU","EU","ND","NA")))


sum_data2$IDsource <- match(sum_data2$origen_mt, nodes$name)-1 
sum_data2$IDtarget <- match(sum_data2$origen_Y, nodes$name)-1
sanki<-sankeyNetwork(Links = sum_data2, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",
                     sinksRight = F, fontSize = 0,fontFamily = "arial" , nodeWidth = 50, NodeGroup = "group",
                     nodePadding = 12, margin = c("top"=40))

sanki

sanki_html<-htmlwidgets::onRender(sanki, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Linaje Materno", "Linaje Paterno"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d-10)
        .attr("y", 30)
        .attr("font-size", "30px")
        .attr("font-weight", "bold")
        .text(labels[i]);
        
    })
  } 
')

sanki_html
saveNetwork(sanki, "plots/Sankey_plot.html")

########

hg_nat<-data%>%filter(origen_mt=="Nativoamericano")
hg_nat<-map_dfr(c("A2","B2","C1","D1","D4"), ~hg_nat[grep(.x,hg_nat$Hg_mat),]%>%mutate(Hg_mt_simpl=.x))

png("plots/hg_nat_barplot.png")
hg_nat%>%ggplot(aes(Hg_mt_simpl))+geom_bar()+xlab("Haplogrupos Nativoamericanos")
dev.off()
sum_nat<-hg_nat%>%group_by(Hg_mt_simpl, origen_Y)%>%summarise(value=n())

sum_nat<-sum_nat%>%group_by(Hg_mt_simpl)%>%mutate(mean_mt=round(sum(value)*100/sum(sum_nat$value),2))%>%ungroup()
sum_nat<-sum_nat%>%group_by(origen_Y)%>%mutate(mean_Y=round(sum(value)*100/sum(sum_nat$value),2))%>%ungroup()
sum_nat2<-sum_nat%>%mutate(Hg_mt_simpl=paste0(Hg_mt_simpl, "\n " , mean_mt, "%" ), origen_Y=paste0(origen_Y," ", mean_Y, "%" ))

nodes_nat <- data.frame(
  name=c(as.character(sum_nat2$Hg_mt_simpl), 
         as.character(sum_nat2$origen_Y)) %>% unique()
)
nodes_nat<-cbind(nodes_nat,group=as.factor(c("A2","B2","C1","D1","D4","AF","EU","EU","NA","ND")))


sum_nat2$IDsource <- match(sum_nat2$Hg_mt_simpl, nodes_nat$name)-1 
sum_nat2$IDtarget <- match(sum_nat2$origen_Y, nodes_nat$name)-1
sanki_nat<-sankeyNetwork(Links = sum_nat2, Nodes = nodes_nat,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",
                     sinksRight = F, fontSize = 0,fontFamily = "arial" , nodeWidth = 50, NodeGroup = "group",
                     nodePadding = 12, margin = c("top"=40))

sanki_nat

sanki_nat_html<-htmlwidgets::onRender(sanki_nat, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
    var labels = ["Linaje Materno", "Linaje Paterno"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d-10)
        .attr("y", 30)
        .attr("font-size", "30px")
        .attr("font-weight", "bold")
        .text(labels[i]);
        
    })
  } 
')

sanki_nat_html
saveNetwork(sanki_nat_html, "plots/Sankey_nat_plot.html")

#############

library("gplots")
# 1. convert the data as a table
dt<-sum_data%>%select(value, contains("origen"))%>%pivot_wider(values_from=value,names_from = origen_Y, values_fill=0)
dt$origen_mt<-as_factor(dt$origen_mt)
column_to_rownames(dt, "origen_mt")->dt
dt <- as.table(as.matrix(dt))
# Graph
png("plots/baloonplot_origen.png")
  balloonplot(t(dt), main ="Origenes", xlab ="Origen Paterno", ylab="Origen Materno",
            label = T, show.margins = T, text.size=0.7)
dev.off()
chi<-chisq.test(dt)
chi$residuals%>%round(2)
library(corrplot)
png("plots/corrplot_residuos.png")
corrplot(chi$residuals, is.cor = FALSE)
dev.off()

##########
dt_nat<-sum_nat%>%select(value, origen_Y, Hg_mt_simpl)%>%pivot_wider(values_from=value,names_from = origen_Y, values_fill=0)
column_to_rownames(dt_nat, "Hg_mt_simpl")->dt_nat
dt_nat <- as.table(as.matrix(dt_nat))
png("plots/nat_baloonplot_origen.png")
balloonplot(t(dt_nat), main ="Origenes", xlab ="Origen Paterno", ylab="Origen Materno",
            label = T, show.margins = T, text.size=0.7)
dev.off()

chi_nat<-chisq.test(dt_nat)
chi_nat
chi_nat$residuals%>%round(2)
library(corrplot)
png("plots/nat_corrplot_residuos.png")
corrplot(chi_nat$residuals, is.cor = FALSE)
dev.off()
