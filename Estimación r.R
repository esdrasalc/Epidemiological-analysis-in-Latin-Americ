setwd("C:/Users/njere/OneDrive/proyecto de titulo")
source("./R/packages.R")

datos=openxlsx::read.xlsx("./DATABASES/cv19_regional.xlsx",detectDates=TRUE) %>%
  filter(Fecha<"2020-12-01")
regiones.ordenadas=c("Arica y Parinacota","Tarapaca","Antofagasta",
                     "Atacama","Coquimbo","Valparaiso",
                     "Metropolitana","OHiggins","Maule",
                     "Nuble","Biobio","Araucania",
                     "Los Rios","Los Lagos","Aysen","Magallanes","Total")
datos$Region=factor(datos$Region,levels=regiones.ordenadas)

cuarentena=openxlsx::read.xlsx("./DATABASES/descripcion_cuarentenas.xlsx",detectDates=TRUE)
cuarentena$Region=factor(cuarentena$Region,levels=regiones.ordenadas)

#procedimiento

entrada=
  datos %>% filter(Region==regiones.ordenadas[1] & Fecha>="2020-04-01") %>%
  select(Fecha,Nuevos.Confirmados) %>% rename(dates=Fecha,I=Nuevos.Confirmados)

res=estimate_R(incid = entrada,method = "parametric_si",
               config = make_config(list(si_parametric_distr="L", mean_si = 4.7, std_si = 2.9)))

base.r=data.frame(Region=regiones.ordenadas[1],Fecha=res$dates[-(1:7)],
                  Estimacion=res$R$`Mean(R)`,P025=res$R$`Quantile.0.025(R)`,
                  P975=res$R$`Quantile.0.975(R)`) 

for(i in 2:16){
  entrada=
    datos %>% filter(Region==regiones.ordenadas[i] & Fecha>=Fecha.Primer.Contagiado+10) %>%
    select(Fecha,Nuevos.Confirmados) %>% rename(dates=Fecha,I=Nuevos.Confirmados)
  
  res=estimate_R(incid = entrada,method = "parametric_si",
                 config = make_config(list(si_parametric_distr="L", mean_si = 4.7, std_si = 2.9)))
  
  base.r=data.frame(Region=regiones.ordenadas[i],Fecha=res$dates[-(1:7)],
                    Estimacion=res$R$`Mean(R)`,P025=res$R$`Quantile.0.025(R)`,
                    P975=res$R$`Quantile.0.975(R)`) %>% rbind(base.r)
}

referencia=
  base.r %>% group_by(Region) %>% summarise(Final=min(Fecha)-1) %>%
  mutate(Dias.Faltan=difftime(Final,"2020-03-03",units="days") %>%
           round() %>% as.numeric()) %>% as.data.frame()

base.r=data.frame(
  Region=rep(referencia$Region,referencia$Dias.Faltan+1),
  Fecha=sapply(referencia$Dias.Faltan, 
               function(x) seq(as.Date("2020-03-03"),as.Date("2020-03-03")+x,"days")) %>%
    unlist() %>% as.Date(),
  Estimacion=NA,P025=NA,P975=NA) %>% rbind(base.r) %>% arrange(Region,Fecha)

write.xlsx(base.r,"./DATABASES/r_regional.xlsx")

#graficos

pdf(file="./IMAGES/graph_chile_r_cori_1.pdf", width=15,height=15)
merge(mutate(base.r,Region=factor(Region,levels=regiones.ordenadas[-17])),
      select(datos,Pos,Region) %>% distinct() ,"Region") %>% filter(Pos<=8) %>%
  ggplot() + facet_wrap(~Region,ncol=2,scales = "free") +
  geom_rect(aes(xmin=Fecha.de.Inicio, xmax=Fecha.de.Termino,ymin=-Inf, ymax=Inf),
            data=cuarentena %>% filter(Pos<=8) %>% 
              select(Region,Fecha.de.Inicio,Fecha.de.Termino) %>% distinct(),
            fill="lavender", alpha=I(0.4)) +
  geom_ribbon(aes(x=Fecha,ymin=P025,ymax=P975),fill="gray77",alpha=I(0.7)) +
  geom_line(aes(x=Fecha,y=P025),alpha=I(0.3)) +
  geom_line(aes(x=Fecha,y=P975),alpha=I(0.3)) +
  geom_line(aes(x=Fecha,y=Estimacion),color="black",size=0.5) +
  geom_hline(yintercept = 1,linetype="dashed") + theme_classic()+
  labs(y="",x="Mes",title="Número Reproductivo Instantáneo \n (Parte I)") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme_few() +
  theme(axis.text = element_text( size = 10 ),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 17)) 
dev.off()
#,linetype="dotted"
pdf(file="./IMAGES/graph_chile_r_cori_2.pdf", width=15,height=15)
merge(mutate(base.r,Region=factor(Region,levels=regiones.ordenadas[-17])),
      select(datos,Pos,Region) %>% distinct() ,"Region") %>% filter(Pos>8) %>%
  ggplot() + facet_wrap(~Region,ncol=2,scales = "free") +
  geom_rect(aes(xmin=Fecha.de.Inicio, xmax=Fecha.de.Termino,ymin=-Inf, ymax=Inf),
            data=cuarentena %>% filter(Pos>8) %>% 
              select(Region,Fecha.de.Inicio,Fecha.de.Termino) %>% distinct(),
            fill="lavender", alpha=I(0.4)) +
  geom_ribbon(aes(x=Fecha,ymin=P025,ymax=P975),fill="gray77",alpha=I(0.7)) +
  geom_line(aes(x=Fecha,y=P025),alpha=I(0.3)) +
  geom_line(aes(x=Fecha,y=P975),alpha=I(0.3)) +
  geom_line(aes(x=Fecha,y=Estimacion),color="black",size=0.5) +
  geom_hline(yintercept = 1,linetype="dashed") + theme_classic()+
  labs(y="",x="Mes",title="Número Reproductivo Instantáneo \n (Parte II)") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme_few() +
  theme(axis.text = element_text( size = 10 ),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 17)) 
dev.off()