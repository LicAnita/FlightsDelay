library(tidyverse)
library(forcats)
library(nycflights13)

#agrupa los vuelos por carrier
flights_group_c <- group_by(flights, carrier)

#n_vuelos la creé para contar todos los vuelos, y el n() 
#se refiere a todos los vuelos
flights_group_c<-mutate(flights_group_c, n_vuelos=n())

#filtro los que tengan mas de mil vuelos
flights_carrier_1000=filter(flights_group_c, n_vuelos>1000)

#filtro por aerolinea
flights_AA<-filter(flights_carrier_1000, carrier=='AA')

#13 a)Agrupo por meses
flights_months<-group_by(flights_AA, month)

#13b) creo una variable nueva con el tiempo recupero en el aire
flights_months<-mutate(flights_months, tiempo_aire=arr_delay-dep_delay)

#13c) calculo variables de eso
metrics<-summarise(flights_months, mean(tiempo_aire, na.rm=TRUE), median(tiempo_aire, na.rm=TRUE),mean(dep_delay, na.rm=TRUE), median(dep_delay, na.rm=TRUE))
a <- tibble(Métrica = c('Media', 'Mediana'),
            valor = c(mean(flights_months$dep_delay, na.rm=TRUE), median(flights_months$dep_delay, na.rm=TRUE)))
b <- tibble(Métrica = c('Media', 'Mediana'),
            valor = c(mean(flights_months$tiempo_aire, na.rm=TRUE), median(flights_months$tiempo_aire, na.rm=TRUE)))

#14b) graficar vuelos retrasados
delay_plot<-ggplot(data = flights_months) + geom_violin(aes(x=dep_delay, y=factor(month))) + labs(title='Vuelos Retrasados', subtitle = 'American Aerlines', xlab('Demora [min]'), ylab('Cantidad de vuelos')) +
  geom_vline(data=a, aes(xintercept=valor, color=Métrica))

#15) graficar tiempo ganado

air_plot<-ggplot(data = flights_months) + geom_freqpoly(aes(x=tiempo_aire, color=factor(month))) + labs(title='Tiempo recuperado en el aire', subtitle = 'American Aerlines', xlab('Tiempo recuperado [min]'), ylab('Cantidad de vuelos'))+
  xlim(-150,150) + geom_vline(data=b, aes(xintercept=valor, color=Métrica))

#16) ggrides - volando
install.packages('ggridges')
install.packages('viridis')
install.packages('hrbrthemes')
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

delay_plot_new <- ggplot(flights_months, aes(x=dep_delay, y=month, fill=factor(month))) + labs(title='Vuelos Retrasados', subtitle = 'American Aerlines') +
  xlim(-50, 200) + geom_density_ridges(scale = 3, rel_min_height = 0.01) + theme_ipsum() + theme(legend.position = "none") +
  xlab('Demora [min]') + ylab('Cantidad de vuelos')

air_plot_new <- ggplot(flights_months, aes(x=tiempo_aire, y=month, fill=factor(month))) + labs(title='Tiempo Recuperado en el Aire', subtitle = 'American Aerlines') +
  xlim(-100, 100) + geom_density_ridges(scale = 3, rel_min_height = 0.01, calc_ecdf=TRUE, quantile_lines=TRUE) + theme_ipsum() + theme(legend.position = "none") +
  xlab('Tiempo recuperado [min]') + ylab('Cantidad de vuelos')

                         