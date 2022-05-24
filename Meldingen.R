

# Data taken from
# https://dataplatform.nl/#/data/2346f748-e7e7-4e6d-b630-1009a3c0e9d8
# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/wijk-en-buurtkaart-2022
# https://www.cbs.nl/nl-nl/maatwerk/2021/31/kerncijfers-wijken-en-buurten-2021

# Packages installation
install.packages("rgeoda")
install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")

# Load  packages
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)

# Functions


## Convert all reports to 
reports_to_sf = function(all_reports, category)
  st_transform(
    st_as_sf(x = all_reports[startsWith(all_reports$categorie, category),], 
             coords = c("lon", "lat"), 
             crs = 4326), 
    crs = 3857)



# Kerncijfers
kern = read_excel("./.data/kwb-2021.xls") %>% 
  filter(recs == "Buurt" & gm_naam == "Utrecht")

FILTER_ONLY_POLLUTION = function(c)
  startsWith(c, 'Afval en vervuiling openbare ruimte;') |
  c == 'Anders nl; met betrekking tot afval' |
  c == 'Anders nl; met betrekking tot vervuiling'

# Meldingen
reports = read.csv(file = './.data/reports-utrecht.csv') %>%
  mutate(year = as.integer(format(as.POSIXct(begindatum), format='%Y'))) %>%
  filter(year == 2022) %>% 
  filter(FILTER_POLLUTION(categorie))


reports_sf = reports %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3857)
  
sort(unique(reports$categorie))
sort(table(reports$categorie))


# Buurten
utr_bu_woon_sf = st_read("./.data/WijkBuurtkaart_2022_v0/SHP/CBS_buurten2022.shp") %>%
  filter(GM_NAAM == 'Utrecht')  %>%
  filter(!startsWith(BU_NAAM, 'Bedrijven')) %>% 
  merge(kern[,c('a_inw', 'gwb_code')], by.x='BU_CODE', by.y = 'gwb_code') %>% 
  mutate(a_inw = as.integer(a_inw)) %>%
  st_transform(crs = 3857)


# Joining
#nrow(reports_sf)
#reports_outliers = reports[reports$lon > 5.2,]

summary(st_join(reports_sf, utr_bu_woon_sf))

reports_joined = st_join(reports_sf, utr_bu_woon_sf) %>% 
  st_drop_geometry() %>%
  filter(!is.na(BU_CODE)) %>%
  #filter(startsWith(BU_NAAM, "Tuinwijk") | startsWith(BU_NAAM, "Lombok")) %>%
  group_by(BU_CODE) %>% 
  summarise(n = sum(aantal_stemmen)) %>% 
  merge(
    utr_bu_woon_sf 
    #%>% st_drop_geometry()
    , 
    by='BU_CODE') %>%
  mutate(report_index = 100 * n / a_inw) %>%
  mutate(report_cat = as.factor(ntile(report_index, 4))) %>% 
  arrange(-report_index)

nrow(reports_joined)

# utr_buurten_wo_geometry = utr_buurten
# st_geometry(utr_buurten_wo_geometry)=NULL

#grouped_joined = merge(grouped, utr_buurten_wo_geometry, by='BU_CODE') %>% 
#  arrange(-n) 

FILL_COLORS = c("#bdcf32", "#ede15b", "#ef9b20", "#ea5545")

# Plot Tuinwikn and Lombok
ggplot(
  head(reports_joined, 104), 
  aes(x = report_index, y=reorder(BU_NAAM, -report_index), fill = report_cat)) +
  scale_fill_manual(values = FILL_COLORS, guide="none") +
  geom_bar(stat="identity", width=0.8) +
  theme(axis.text.y = element_text(lineheight = 3.4, size = 6)) +
  coord_fixed(ratio = 1.5) +
  labs(
    title = "Utrecht waste and pollution complains",
    subtitle = "All neighbourhoods excl. industrial areas",
    caption = "(based on 2021 data from Slim Melden)", 
    x = '# of votes per 100 residents', 
    y = NULL
  )
  
ggplot(
  reports_joined, 
  aes(x = report_index, y=reorder(BU_NAAM, -report_index), fill = report_cat)) +
  scale_fill_manual(values = FILL_COLORS, guide="none") +
  geom_bar(stat="identity", width=0.8) +
  theme(axis.text.y = element_text(lineheight = 3.4, size = 6)) +
  coord_fixed(ratio = 1.5) +
  labs(
    title = "Utrecht waste and pollution complains",
    subtitle = "All neighbourhoods excl. industrial areas",
    caption = "(based on 2021 data from Slim Melden)", 
    x = '# of votes per 100 residents', 
    y = NULL
  )


buurt_filter = function(BU_NAAM)
  startsWith(BU_NAAM, "Nieuw Engeland") | 
  startsWith(BU_NAAM, "Lombok") |
  startsWith(BU_NAAM, "Laan van Nieuw-Guinea") |
  startsWith(BU_NAAM, "Oog in Al") |
  startsWith(BU_NAAM, "Halve Maan") |
  startsWith(BU_NAAM, "Welgelegen") | 
  startsWith(BU_NAAM, "Kanaleneiland-Noord") |
  startsWith(BU_NAAM, "Transwijk-Noord")

ggplot() +
  geom_sf(
    data = (reports_joined  %>% st_as_sf %>% filter(buurt_filter(BU_NAAM))),
    size = 0.3,
    color = "white"
  ) +
  geom_sf_text(data = reports_joined  %>% st_as_sf %>% filter(buurt_filter(BU_NAAM)), aes(label = paste(substr(BU_NAAM, 0, 20), '...', "\n", round(report_index, 2))), nudge_y = 100) +
  aes(fill = report_cat) +
  scale_fill_manual(values = FILL_COLORS, guide="none") +
  labs(
    title = "Utrecht waste and pollution complains",
    subtitle = "Selected neighbourhoods",
    caption = "(based on 2021 data from Slim Melden)", 
    x = '# of votes per 100 residents', 
    y = NULL
  )


ggplot() +
  geom_sf(
    data = (
      reports_joined  %>% st_as_sf
    ),
    size = 0.3,
    color = "white"
  ) +
  aes(fill = report_cat) +
  scale_fill_manual(values = FILL_COLORS, guide="none") +
  labs(
    title = "Utrecht waste and pollution complains",
    subtitle = "All neighbourhoods excl. industrial areas",
    caption = "(based on 2021 data from Slim Melden)", 
    x = '# of votes per 100 residents', 
    y = NULL
  )


# Mapping reports to neighborhoods

# reports_trash_geo = reports_trash$geometry
##
# pts = st_jitter(st_sample(utr_buurten, 1000), factor=0.2)
# intersects = st_intersects(reports_trash_geo, utr_buurten)
# pts_in = lengths(intersects) > 0

head(pts_in)

joined = st_join(reports_trash, utr_buurten['BU_NAAM'])
joined_grouped = data.frame(table(joined$BU_NAAM))

require(lattice)
barplot(table(joined$BU_NAAM))
ggplot(joined_grouped, aes(Var1, Freq, fill =Var1)) + geom_col() + coord_flip() +
  scale_fill_brewer(palette="Spectral")

dd = utr_buurten %>% filter(startsWith(BU_NAAM, 'Tuinwijk')) %>% 
           mutate(area = st_area(geometry))

ggplot() +
  geom_sf(
    data = utr_buurten %>% filter(startsWith(BU_NAAM, 'Tuinwijk')),
    size = 0.3,
    color = "black",
    fill = "cyan1"
  ) +
  
  #geom_sf(data = reports_trash_geo[pts_in], size = 0.5, color = "brown") +
  #  geom_sf(data = reports_traffic, size = 0.5, color = "red") +
  ggtitle("Utrecht Wijken") +
  coord_sf()
