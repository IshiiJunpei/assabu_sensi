library("leaflet")
library("htmlwidgets")
library("ggplot2")
library("tidyverse")
library(sf)

# データ読込

df <- read_csv("data/sensi_wgs84_time_fix.csv")

df_sf <- st_read("data/assabu_sensi_WGS84.gpkg") 

# 座標取り出し
coord <- st_coordinates(df_sf) 

# sfクラスをtibbleに変換して座標値をバインド
df <- df_sf %>% 
  as_tibble() %>% 
  bind_cols(coord) %>% 
  dplyr::select(Class, Name, Place, year, month, day, X, Y)

# 戦没年をdate形式にする
df <- df %>% 
  mutate(deathDate = str_c(
    year, month, day , sep = "-"
  )) %>% 
  mutate(deathDate = as.Date(deathDate))

# 戦没年を分類
df <- df %>% 
  mutate(yearClass = case_when(
    deathDate < as.Date("1937-7-7") ~ "日中戦争開戦以前",
    deathDate >= as.Date("1937-7-7") &
      deathDate < as.Date("1941-12-8")~ "太平洋戦争開戦以前",
    deathDate >= as.Date("1941-12-8") ~ "太平洋戦争開戦後"
  ))

# deathDateをnumericに変換
df <- df %>% 
  mutate(deathDate_num = as.numeric(deathDate))

# カラーパレットの定義
pal <- colorNumeric(palette = "viridis",
                    domain = c(as.numeric(as.Date("1934-1-1")),
                               as.numeric(as.Date("1950-12-31")))
                    )

# popup用の文字列
df <- df %>% 
  mutate(popup = paste(Name,
                       Class,
                       Place,
                       paste(year, "年", month, "月", day, "日")
                       )
  )

# 凡例に日付を表示させるスクリプト
myLabelFormat = function(...,dates=FALSE){ 
  if(dates){ 
    function(type = "numeric", cuts){ 
      as.Date(cuts, origin="1970-01-01")
    } 
  }else{
    labelFormat(...)
  }
}
                
# leafletでhtml作成
df_map <- df %>% 
  drop_na(deathDate) %>% 
  leaflet() %>% 
  addTiles() %>% 
#  addProviderTiles("Stamen.Terrain") %>% 
  addMarkers(lng = ~X,
             lat = ~Y,
             popup = ~popup,
             group = "日中戦争開戦以前",
             data = filter(.data = df,
                           yearClass == "日中戦争開戦以前")) %>% 
  addMarkers(lng = ~X,
             lat = ~Y,
             popup = ~popup,
             group = "太平洋戦争開戦以前", 
             data = filter(.data = df,
                           yearClass == "太平洋戦争開戦以前")) %>% 
  addMarkers(lng = ~X,
             lat = ~Y,
             popup = ~popup,
             group = "太平洋戦争開戦後", 
             data = filter(.data=df,
                           yearClass=="太平洋戦争開戦後")) %>% 
  addCircleMarkers(lng = ~X,
             lat = ~Y,
             popup = ~popup,
             color = FALSE, 
             fill	= TRUE,
             fillColor 	= ~pal(deathDate_num),
             fillOpacity = 0.9,
             radius = 6,
             group = "死亡年別戦死者",
             weight = 2) %>% 
  addLayersControl(overlayGroups = c(
    "日中戦争開戦以前","太平洋戦争開戦以前","太平洋戦争開戦後", "死亡年別戦死者"
    )) %>% 
  hideGroup(c(
    "日中戦争開戦以前","太平洋戦争開戦以前","太平洋戦争開戦後"
  )) %>% 
  addLegend(position = 'topright',
            pal = pal,
            values = ~deathDate_num,
            title = "戦没年月日",
            labFormat = myLabelFormat(dates=TRUE))

# htmlに保存
saveWidget(df_map,
           file = "index.html")


#####地図使えるタイルを表示
names(providers)



