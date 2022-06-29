# Pakkar:
library(tidyverse)
library(rio)
library(janitor)
library(tabulizer)

#Raforkuspár
url1<-"https://orkustofnun.is/media/raforka/Endurreikningur_Raforkuspa-2021_Toflur_og_vidaukar-Til_Orkustofnunar_20210630.xlsx"
d1<- rio::import(file = url,which = 6, skip=6) %>% rename(ar=...1, gwh=GWh...10, mw=MW...11) %>% select(ar, gwh, mw)%>% mutate(spa="raf_spa")
d2<- rio::import(file = url,which = 20, skip=6) %>% rename(ar=...1, gwh=GWh...10, mw=MW...11) %>% select(ar, gwh, mw) %>% mutate(spa="hf")
d3<- rio::import(file = url,which = 21, skip=6) %>% rename(ar=...1, gwh=GWh...10, mw=MW...11) %>% select(ar, gwh, mw)%>% mutate(spa="gf")
d4<- rio::import(file = url,which = 22, skip=6) %>% rename(ar=...1, gwh=GWh...10, mw=MW...11) %>% select(ar, gwh, mw)%>% mutate(spa="as")

df<- bind_rows(d1,d2,d3,d4)
df<- df %>% mutate(ar = as.numeric(str_extract(ar, "\\d+")))

df<- df %>% mutate(spa=factor(spa, levels = c("raf_spa", "hf", "gf", "as"), labels = c("Raforkuspá", "Hægar framfarir", "Græn framtíð", "Aukin stórnotkun")))


# Gögn um virkjanakosti
url2<- "https://www.stjornarradid.is/library/02-Rit--skyrslur-og-skrar/St%C3%B6%C3%B0usk%C3%BDrsla%20%C3%A1skoranir%20%C3%AD%20orkum%C3%A1lum%2008032022.pdf"

# F. töflu á bls. 40
# Finna töfluna
#a1<- locate_areas(url2, pages=40)
area1<- list(c(208.38862, 79.24671, 615.16320, 471.01734))

# Merkja dálka
# c40_1<- locate_areas(url2, pages=40)
# c40_2<- locate_areas(url2, pages=40)
# c40_3<- locate_areas(url2, pages=40)
# c40_5<- locate_areas(url2, pages=40)
# c40_6<- locate_areas(url2, pages=40)
sk1<- extract_tables(url2, output = "data.frame",
                    pages=40,
                    area = area1,
                    columns =list(c(80,121,131,209,220,321,380,428,431,475)),
                    guess=FALSE
)

sk1<- reduce(sk1, bind_rows)
sk1<- sk1 %>% select(c(2,4,6,7,8,10))
sk1<- sk1 %>% clean_names() %>%  row_to_names(row_number = 1)%>% clean_names() %>% rename(svaedi=hahitasvaedi,hagkv_flokkun=hagkv_flok)

#F. töflu á bls. 41
# Finna töfluna
#a2<- locate_areas(url2, pages=41)
area2<- list(c(225,  80, 428, 427))

# Merkja dálka
# c41_1<- locate_areas(url2, pages=41)
# c41_2<- locate_areas(url2, pages=41)
# c41_3<- locate_areas(url2, pages=41)
# c41_4<- locate_areas(url2, pages=41)
# c41_5<- locate_areas(url2, pages=41)

sk2<- extract_tables(url2, output = "data.frame",
               pages=41,
               area = area2,
               columns = list(c(77,153,169,231,250,281,305,358,365,418)),
               guess=FALSE
)
sk2<- reduce(sk2, bind_rows)
sk2<- sk2 %>% select(c(2,4,6,8,10))
sk2<- sk2 %>% clean_names() %>% rename(tegund_orku=egund_orkuvinnslu, g_wst_ar=g_wst_ari)

t1<- sk1 %>% mutate(afangi="1-2")  %>% mutate(gwh=as.numeric(g_wst_ar))%>% select(-svaedi, -mw, -g_wst_ar)
t2<- sk2 %>% mutate(afangi="3", hagkv_flokkun = str_replace(hagkv_flokkun, "X", ""), gwh=as.numeric(g_wst_ar)) %>% select(-mw,-g_wst_ar)
t<- bind_rows(t1, t2)

t<- t %>% arrange(hagkv_flokkun, gwh)

# Virkja áfram

df1<- df %>% filter(spa=="Græn framtíð" & ar>2024) %>%
  arrange(ar) %>%
  mutate(n_virkjanir=case_when(
    ar==2026 ~ 320,
    ar==2027 ~ 320+369,
    ar==2029 ~ 320+369+410,
    ar==2031 ~ 320+369+410+738,
    ar==2037 ~ 320+369+410+738+738,
    ar==2042 ~ 320+369+410+738+738+738,
    ar==2047 ~ 320+369+410+738+738+738+1037,
    ar==2054 ~ 320+369+410+738+738+738+1037+1476
  )) %>% fill(n_virkjanir) %>% mutate_all(~replace(., is.na(.), 0))%>%
  mutate(gwh_virkjanir=n_virkjanir+first(gwh))
  
# Loka álveri
straumsvik<- 390*8760/1000

df2<- df1 %>% mutate(l_alver=first(gwh)+straumsvik) 

  
# Mynd 1
ggplot(df1, aes(x=ar)) + 
  geom_line(aes(y = gwh), size=2, orientation = "y") + 
  geom_step(aes(x=ar, y = gwh_virkjanir) ,size=2, alpha=2, direction = "vh") +
  pammtools::geom_stepribbon(aes(ymin=lead(gwh), ymax=lead(gwh_virkjanir)), fill="firebrick4", na.rm=TRUE, alpha=1)+
  geom_ribbon(data=df1, aes(ymin=gwh, ymax=gwh_virkjanir), fill="firebrick4", alpha=1)+
  scale_x_continuous(name="Ár", breaks=seq(min(df1$ar), max(df1$ar),5)) +
  scale_y_continuous(name="GWst", breaks=seq(round(min(df1$gwh), digits=-1), round(max(df1$gwh_virkjanir), digits=-1), 500))+theme(text = element_text(size=rel(4)))

# Mynd 2


ggplot(df2, aes(x=ar)) +
  geom_line(aes(y = gwh), size=2, orientation = "y") + 
  geom_line(aes(x=ar, y = l_alver) ,size=2, alpha=2)+
  geom_ribbon(aes(ymin=gwh, ymax=l_alver, fill = gwh >= l_alver), show.legend = FALSE, alpha=1)+
  scale_x_continuous(name="Ár", breaks=seq(min(df2$ar), max(df2$ar),5)) +
  scale_y_continuous(name="GWst", breaks=seq(round(min(df2$gwh), digits=-1), round(max(df2$gwh), digits=-1), 500))+theme(text = element_text(size=rel(4)))
