library(tidyverse)
library(readxl)
library(gganimate)

solicitudes <- read_excel('data/Solicitudes de seguro de desempleo.xlsx',
                          col_names=c('Fecha', 'Montevideo', 'Interior', 'Web', 'Total'),
                          skip=1) %>%
  mutate(Fecha = as.Date(Fecha))

plot <- solicitudes %>%
  ggplot(aes(x=Fecha, y=Total)) +
  geom_line() +
  geom_point() +
  ggtitle('Solicitudes de seguro de desempleo en Uruguay') +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  transition_reveal(Fecha) +
  view_follow() +
  ease_aes('cubic-in-out')

animate(plot)
anim_save("plot.gif")
