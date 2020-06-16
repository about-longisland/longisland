library(sf)
library(sqldf)
library(ggplot2)

shore <- read_sf("NYS_Civil_Boundaries_SHP/State_Shoreline.shp")

nyct <- read_sf("NYS_Civil_Boundaries_SHP/Cities_Towns.shp")

nyct$COUNTY %in% c('Nassau','Suffolk')

li <- nyct[nyct$COUNTY %in% c('Nassau','Suffolk'),]

plot(st_geometry(shore))

plot(st_geometry(li))

lishore = st_intersection(li,shore)

plot(lishore)

liplot <- ggplot() + geom_sf(data=lishore, aes(fill=NAME))
liplot
ggsave('liplot.png', liplot)

for(i in li){
  
  dat <- sqldf(paste0('select * from us_states where state=\'',i,"'")) %>% mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
    mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
    mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
    mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))
  datalist[[i]] <- dat
}

num = 1
for (i in 1:nrow(li)){
  townplot <- ggplot() + geom_sf(data=lishore[i,], aes(fill=NAME))
  ggsave(sprintf("%s.svg",lishore$NAME[i]),townplot)
}

num = 1
for (i in 1:nrow(li)){
  print(sprintf("%s.svg",lishore$NAME[i]))
}


write("#text!\n##more text", "docs/text.md")
sprintf(lishore$NAME[1],format(lishore$POP2010, big.mark = ","))
nrow(li)
