library(sf)
library(sqldf)
library(ggplot2)
library(Hmisc)
library(htmltools)

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
  townplot <- ggplot() + geom_sf(data=lishore[i,], fill="steelblue1")
  svgname <- sprintf("%s.svg",sub(" ", "-", lishore$NAME[i]))
  mdname <- sprintf("docs/%s.md",sub(" ", "-", lishore$NAME[i]))
  ggsave(sprintf('docs/%s',svgname),townplot)
  mdfile <- sprintf("# %s of %s\n## Population in 2010: %s\n![A map of %s](%s)",capitalize(lishore$MUNI_TYPE[i]),lishore$NAME[i],format(lishore$POP2010[i], big.mark = ","),lishore$NAME[i],svgname)
  write(mdfile,mdname)
}

num = 1
for (i in 1:nrow(li)){
  print(sprintf("%s.md",lishore$NAME[i]))
}


write("#text!\n##more text", "docs/text.md")
sprintf("# %s of %s\n## Population in 2010: %s\n![A map of %s](%s.svg)",capitalize(lishore$MUNI_TYPE[i]) ,lishore$NAME[1],format(lishore$POP2010[1], big.mark = ","),lishore$NAME[1],lishore$NAME[1])
nrow(li)

ggplot() + geom_sf(data=lishore[1,], fill="steelblue1")
