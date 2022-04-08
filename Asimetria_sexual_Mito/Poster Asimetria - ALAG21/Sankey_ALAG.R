library(tidyverse)
library(networkD3)
library(htmlwidgets)
setwd("B:\\Martin\\Distribucion Mito\\Poster ALAG2021")
data<-read_csv("CABA-ADNmt-ChrY2.txt")
data_net<-select(data, origen_mt, origen_Y)
sum_data<-data_net%>%group_by(origen_mt, origen_Y)%>%summarise(value=n())
sum(sum_data$value)

sum_data<-sum_data%>%group_by(origen_mt)%>%mutate(mean_mt=round(sum(value)/2.45,2))%>%ungroup()
sum_data<-sum_data%>%group_by(origen_Y)%>%mutate(mean_Y=round(sum(value)/2.45,2))%>%ungroup()
sum_data2<-sum_data%>%mutate(origen_mt=paste0(origen_mt, "\n " , mean_mt, "%" ), origen_Y=paste0(origen_Y," ", mean_Y, "%" ))

nodes <- data.frame(
  name=c(as.character(sum_data2$origen_mt), 
         as.character(sum_data2$origen_Y)) %>% unique()
)

nodes<-cbind(nodes,group=as.factor(c("AF","EU","NA","EU","EU","NA","AF")))
my_color <- 'd3.scaleOrdinal() .domain(["AF","EU","NA") .range(["#000000", "#111111" , "#222222"])'


sum_data2$IDsource <- match(sum_data2$origen_mt, nodes$name)-1 
sum_data2$IDtarget <- match(sum_data2$origen_Y, nodes$name)-1
sanki<-sankeyNetwork(Links = sum_data2, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name",
              sinksRight = F, fontSize = 17,fontFamily = "arial" , nodeWidth = 20,
              NodeGroup="group", nodePadding = 5, margin = c("top"=40))

sanki
sanki<-htmlwidgets::onRender(sanki, '
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

sanki
saveNetwork(sanki, "sankey_CABA.html")

