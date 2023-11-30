setwd("~/Tesis/Databases")

datos=openxlsx::read.xlsx("Series de tiempo nuevos.xlsx",detectDates=TRUE) %>%
  filter(Fecha<"2021-12-31")
regiones.ordenadas=c("Mexico","Argentina","Belize",
                     "Bolivia","Brazil","Chile",
                     "Colombia","Costa Rica","Dominican Republic",
                     "Ecuador","Paraguay","Peru",
                     "Uruguay","Total")
datos$Pais=factor(datos$Pais,levels=regiones.ordenadas)

head(datos)

info.regiones=
  data.frame(Pais="Colombia",ti=55,te=665,N=50882884,p.det=(1-0.56))
#te30,140

#########################################
############### FUNCIONES ###############
###############  PREVIAS  ###############
#########################################

beta=function(t,ti,te,beta.0,d.d,d.i){
  beta.min=beta.0*(1-(d.d*(te-ti)^2)/(1+d.d*(te-ti)^2))
  ifelse(t<=ti, beta.0,
         ifelse(ti<t & t<te,beta.0*(1-(d.d*(t-ti)^2)/(1+d.d*(t-ti)^2)),
                beta.min+(beta.0-beta.min)*(d.i*(t-te)^2)/(1+d.i*(t-te)^2)))
}

#modelo
modelo=function(x,Pais="Colombia",fin=677){
  
  ti=info.regiones$ti[info.regiones$Pais==Pais]
  te=info.regiones$te[info.regiones$Pais==Pais]
  N=info.regiones$N[info.regiones$Pais==Pais]
  p.det=info.regiones$p.det[info.regiones$Pais==Pais]
  
  simulacion = data.frame(time=1:fin, S = (N-5), E = 4, I = 1, R = 0, D = 0, AI = 1)
  
  for(i in 1:(fin-1)){
    simulacion[i+1,2]=
      simulacion[i,2]-beta(i+1,ti=ti,te=te,beta.0=x[1],d.d=x[5],d.i=x[6])*simulacion[i,2]*simulacion[i,4]/N
    simulacion[i+1,3]=
      simulacion[i,3]+beta(i+1,ti=ti,te=te,beta.0=x[1],d.d=x[5],d.i=x[6])*simulacion[i,2]*simulacion[i,4]/N-x[3]*simulacion[i,3]
    simulacion[i+1,4]=
      simulacion[i,4]+x[3]*simulacion[i,3]-(x[4]+x[2])*simulacion[i,4] #I
    simulacion[i+1,5]=
      simulacion[i,5]+x[4]*simulacion[i,4]
    simulacion[i+1,6]=
      simulacion[i,6]+x[2]*simulacion[i,4]
    simulacion[i+1,7]=
      simulacion[i,7]+x[3]*simulacion[i,3]
    #
  }
  return(simulacion)
}

fun.min=function(x,Pais="Colombia",fin=677){
  p.det=
    simulacion=modelo(x,Pais=Pais,fin=fin)
  ss=sum((simulacion[,7] - cumsum(datos$Casos[datos$Pais==Pais][1:nrow(simulacion)]))^2) +
    sum((simulacion[,6]-datos$Muertes[datos$Pais==Pais][1:nrow(simulacion)])^2)
  return(ss)
}

#########################################
################## GO ###################
#########################################

valor.inicial=c(beta.0=0.3049,mu.0=0.0255,sigma=1.702,gamma=0.1453,d.d=0.0005,d.i=0.0005/50)
optimizacion=constrOptim(valor.inicial, fun.min, NULL, ui=diag(1,6,6), ci=rep(0,6))
optimizacion
beepr::beep(8)
estimaciones=optimizacion$par
write.table(optimizacion$par,file = "parametros_estimados.txt")
estimaciones=read.table("parametros_estimados.txt") %>% t()

#salidas latex
xtable(c(estimaciones[1:5],estimaciones[6]/estimaciones[5]) %>% t() %>% as.data.frame(),
       caption="Estimaciones de los parámetros del modelo SEIR ajustado.",
       label="tab:parametros-estimados",digits=4) 


#grafica 1: acumulados


#est.casos=
rbind(modelo(estimaciones,Pais="Mexico",fin=677)[,c(1,7)] %>% 
        melt(id.vars="time"), 
      datos %>% filter(Pais=="Mexico") %>% rename(time=Periodo.Infeccioso) %>%
        mutate(Casos.Confirmados=cumsum(Casos)) %>% select(time,Casos.Confirmados) %>% 
        melt(id.vars="time")) %>% mutate(time=as.Date("2020-02-23")+time) %>%
  ggplot() + geom_line(aes(time,value,color=variable),size=1) + theme_classic() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent")) +
  scale_color_discrete("",label=c("Infectados Acumulados (Modelo)",
                                  "Casos Confirmados Acumulados (Empíricos)")) +
  labs(x="Mes",y="") + scale_x_date(date_breaks = "1 month",date_labels = "%b")


#grafica 2: fallecidos

#pdf(file="./IMAGES/graph_seir_fallecidos_acumulados.pdf",width=8,height=5)
  rbind(modelo(estimaciones,Pais="Mexico",fin=677)[,c(1,6)] %>% 
          melt(id.vars="time"), 
        datos %>% filter(Pais=="Mexico") %>% rename(time=Periodo.Infeccioso) %>%
          select(time,Muertes) %>% 
          melt(id.vars="time")) %>% mutate(time=as.Date("2020-02-23")+time) %>%
  ggplot() + geom_line(aes(time,value,color=variable),size=1) + theme_classic() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent")) +
  scale_color_discrete("",label=c("Número de Fallecidos (Modelos)",
                                  "Número de Fallecidos (Empírico)")) +
  labs(x="Mes",y="") + scale_x_date(date_breaks = "1 month",date_labels = "%b")
#dev.off()

pdf(file="./IMAGES/graph_seir_ajuste.pdf", width=7,height=9)
grid.arrange(est.casos,est.fall)
dev.off()

#grafica 3: otros
pdf(file="./IMAGES/graph_seir_infectados.pdf",width=8,height=5)
c<-rbind(modelo(estimaciones,Pais="Colombia",fin=677)[,c(1,3,4)] %>% 
        melt(id.vars="time"), 
      datos %>% filter(Pais=="Colombia") %>% rename(time=Periodo.Infeccioso) %>%
        select(time,Casos) %>% 
        melt(id.vars="time")) %>% 
  mutate(time=as.Date("2020-02-23")+time,
         variable=factor(variable,c("Casos","E","I"))) %>%
  ggplot() + geom_line(aes(time,value,color=variable),size=1) + theme_classic() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent")) + labs(x="Month",y="") +
  scale_color_discrete("",label=c("Daily confirmed cases (Empirical)",
                                  "Number of exposed (Models)",
                                  "Number of infected (Models)")) +
  scale_x_date(date_breaks = "3 month",date_labels = "%B",
               limits = as.Date(c("2020-02-23","2021-12-31"))) +
  guides(col = guide_legend(ncol = 1,byrow = TRUE,label.hjust=0.4))
c
ggsave("colombia.png", width = 25, height = 25, units = c("cm"), dpi = 300)


#grafica 4: futuro
pdf(file="./IMAGES/graph_seir_futuro.pdf",width=8,height=5)
modelo(estimaciones,Pais="Mexico",fin=677) %>% melt(id.vars="time") %>%
  mutate(time=as.Date("2020-02-23")+time) %>% filter(variable==c("E","I","D")) %>%
  mutate(variable=factor(variable,c("D","E","I"))) %>%
  ggplot() + geom_line(aes(time,value,color=variable),size=1) + theme_classic() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent")) +
  scale_color_discrete("",label=c("Fallecidos","Expuestos","Infectados")) +
  labs(x="Mes - Año",y="") + scale_x_date(date_breaks = "1 month",date_labels = "%b\n%Y")
dev.off()

pdf(file="./IMAGES/graph_funcion_transmision.pdf",width=8,height=5)
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  stat_function(fun = function (x) beta(x,5,20,4,0.1,0.05),aes(colour="line1"),size=1) +
  stat_function(fun = function (x) beta(x,5,20,4,0.1,0.01),aes(colour="line2"),size=1) +
  stat_function(fun = function (x) beta(x,5,20,4,0.1,0.001),aes(colour="line3"),size=1) + 
  stat_function(fun = function (x) ifelse(x<20,beta(x,5,20,4,0.1,0.001),NA),size=1) +
  geom_rect(aes(xmin=5, xmax=20,ymin=-Inf, ymax=Inf),alpha=I(0.1),fill="steelblue") +
  xlim(0,60) + labs(x="t",y=expression(beta(t))) + theme_few() +
  scale_colour_manual("",values=c(line1="red", line2="yellow", line3="blue"),
                      labels=c(expression(paste(delta[i],"=0.050")),
                               expression(paste(delta[i],"=0.010")),
                               expression(paste(delta[i],"=0.005")))) +
  theme(legend.position = "bottom")
dev.off()

#diagnostico (residuos)

residuos.casos=
  merge(modelo(estimaciones,Pais="Mexico",coc=100,fin=240)[,c(1,7)], 
        datos %>% filter(Pais=="Mexico") %>% rename(time=Periodo.Infeccioso) %>%
          mutate(Casos.Confirmados=cumsum(Casos)) %>% select(time,Casos.Confirmados),
        "time") %>% mutate(D=Casos.Confirmados-AI) %>%
  ggplot(aes(time,D)) + geom_point()

residuos.fallecidos=
  merge(modelo(estimaciones,Pais="Mexico",coc=100,fin=240)[,c(1,6)], 
        datos %>% filter(Pais=="Mexico") %>% rename(time=Periodo.Infeccioso) %>%
          select(time,Muertes),"time") %>% mutate(D=Muertes-D) %>%
  ggplot(aes(time,D)) + geom_point()

grid.arrange(residuos.casos,residuos.fallecidos,ncol=2)
