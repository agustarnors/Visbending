library(tidyverse)
library(readxl)
library(rio)
library(pxweb)

# Ná í gögn
url<-"https://orkustofnun.is/media/raforka/Endurreikningur_Raforkuspa-2021_Toflur_og_vidaukar-Til_Orkustofnunar_20210630.xlsx"
d1<- rio::import(file = url,which = 6, skip=6) %>% rename(ar=...1, gwh=GWh...10) %>% select(ar, gwh)%>% mutate(spa="raf_spa")
d2<- rio::import(file = url,which = 20, skip=6) %>% rename(ar=...1, gwh=GWh...10) %>% select(ar, gwh) %>% mutate(spa="hf")
d3<- rio::import(file = url,which = 21, skip=6) %>% rename(ar=...1, gwh=GWh...10) %>% select(ar, gwh)%>% mutate(spa="gf")
d4<- rio::import(file = url,which = 22, skip=6) %>% rename(ar=...1, gwh=GWh...10) %>% select(ar, gwh)%>% mutate(spa="as")

df<- bind_rows(d1,d2,d3,d4)
df<- df %>% mutate(ar = as.numeric(str_extract(ar, "\\d+")))

df<- df %>% mutate(spa=factor(spa, levels = c("raf_spa", "hf", "gf", "as"), labels = c("Raforkuspá", "Hægar framfarir", "Græn framtíð", "Aukin stórnotkun")))
df1<- df %>% filter(ar>2019) %>%
  group_by(spa) %>%
  arrange(ar) %>%
  mutate(index = 100 * gwh / first(gwh))


# glimpse(df1)


df1 %>% filter(ar>2019) %>% ggplot(aes(x=ar, y=index, group=spa))+ geom_line(aes(color=spa))+geom_point(aes(color=spa, shape=spa))+
  labs(x="Ár", y="Raforkunotkun, 2020=100", shape="Sviðsmynd",color="Sviðsmynd")+
  scale_color_manual(values=c("orange", "steelblue", "forestgreen", "firebrick4"))+
  theme(legend.position = "bottom", legend.box = "vertizal")+ scale_shape_manual(values=c(3, 5, 16, 17))



## Skipting stóriðju
pxweb_query_list <- 
  list("Ár"=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
       "Tegund"=c("GRAND_TOTAL","HEAVY_INDUSTRY","zALUMINUM_PRODUCTION","zIRON_BLENDE","zALUMINUM_FORMING","zDATA","zFERTILIZER"))
px_data <- 
  pxweb_get(url = "http://px.hagstofa.is/pxis/api/v1/is/Umhverfi/4_orkumal/2_framleidslaognotkun/IDN02103.px",
            query = pxweb_query_list)

px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

dfp<-px_data_frame %>% mutate(Ár=as.numeric(Ár))
dfp1<- dfp %>%   pivot_wider(names_from = Tegund, values_from = `Stóriðja og almenn notkun raforku`)


dfp1<- dfp1 %>% mutate(hl=(`Stóriðja alls`/`Raforkuframleiðsla alls`), storidja=`Stóriðja alls`)
dfp1 %>% arrange(Ár) %>%filter(Ár==last(Ár)) %>% select(Ár, hl, `Stóriðja alls`)


# Stóriðja 2019
s2019<- 15146

df1 %>% filter(ar==2040 | ar==2060) %>% mutate(samdrattur=gwh-s2019) %>%
  mutate(hlutfall=1-samdrattur/gwh)

