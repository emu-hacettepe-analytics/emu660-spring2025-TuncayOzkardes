install.packages("readxl")
install.packages("dplyr")
install.packages("DT")
install.packages("htmltools")
install.packages("sf", dependencies = TRUE)
install.packages("patchwork")
install.packages("stringi")
library(readxl)
library(patchwork)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(sf)
library(leaflet)
library(stringi)
library(gridExtra)
library(scales)

istihdam <- read_excel("Project_Data/Istihdam.xlsx")
issizlik <- read_excel("Project_Data/IssizlikOranlari.xlsx")
universiteler_raw <- read_excel("Project_Data/Universiteler.xlsx")

str(istihdam)
str(issizlik)
str(universiteler)


# Üniversite verisinde ilk satırı at ve gerekli kolonu seç
universiteler <- universiteler_raw[-1, ] %>%
  select(Kurulus_Yili = ...2) %>%
  mutate(Kurulus_Yili = as.numeric(substr(Kurulus_Yili, 7, 10)))

# Üniversitelerde mezun verebileceği yıl = Kuruluş yılı + 4
universiteler <- universiteler %>%
  mutate(Mezun_Verebilecegi_Yil = Kurulus_Yili + 4)

# 1. 1988'den önce mezun verebilecek üniversite sayısını hesapla
ilk_toplam <- universiteler %>%
  filter(Mezun_Verebilecegi_Yil < 1988) %>%
  summarise(toplam = n()) %>%
  pull(toplam)

# 2. 1988 sonrası yıllık mezun verecek üniversite sayılarını bul
uni_sayisi <- universiteler %>%
  filter(Mezun_Verebilecegi_Yil >= 1988, Mezun_Verebilecegi_Yil <= 2024) %>%
  group_by(Mezun_Verebilecegi_Yil) %>%
  summarise(mezun_verebilecek_uni_sayisi = n()) %>%
  rename(Yil = Mezun_Verebilecegi_Yil) %>%
  arrange(Yil)

# 3. Eğer 1988 yılı yoksa, başa manuel ekle
if (!1988 %in% uni_sayisi$Yil) {
  uni_sayisi <- bind_rows(
    tibble(Yil = 1988, mezun_verebilecek_uni_sayisi = 0), # 1988'de hiç yeni mezun yoksa bile sıfır olarak ekle
    uni_sayisi
  ) %>%
    arrange(Yil)
}

# 4. 1988 yılındaki üniversite sayısına ilk_toplamı ekle
uni_sayisi <- uni_sayisi %>%
  mutate(mezun_verebilecek_uni_sayisi = ifelse(Yil == 1988, mezun_verebilecek_uni_sayisi + ilk_toplam, mezun_verebilecek_uni_sayisi))

# 5. Son olarak kümülatif toplamı hesapla
uni_sayisi <- uni_sayisi %>%
  mutate(kumulatif_mezun_verebilecek_uni_sayisi = cumsum(mezun_verebilecek_uni_sayisi))

library(scales)  # eklememiz lazım

ggplot(istihdam, aes(x = Yil, y = Istihdam_Sayisi)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(breaks = seq(0, 40000000, by = 5000000),
                     labels = label_comma()) +  # burada değişiklik yaptık!
  labs(
    title = "Yillara Gore Istihdam Sayisi",
    x = "Yil",
    y = "Istihdam_Sayisi"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey70", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey70", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

library(tidyr)

issizlik_long <- issizlik %>%
  pivot_longer(
    cols = -Yil,
    names_to = "Egitim_Durumu",
    values_to = "Issizlik_Orani"
  )



ggplot(issizlik_long, aes(x = Yil, y = Issizlik_Orani)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 1.5) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(limits = c(0, 30)) +
  facet_wrap(~ Egitim_Durumu) +
  labs(
    title = "Egitim Durumuna Gore Yillara Bagli Issizlik Oranlari",
    x = "Yil",
    y = "Issizlik Orani (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, hjust = 0.5)
  )


ggplot(issizlik_long, aes(x = Yil, y = Issizlik_Orani, color = Egitim_Durumu)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(
    title = "Yillara Gore Egitim Durumuna Bagli Issizlik Oranlari",
    x = "Yil",
    y = "Issizlik Orani (%)",
    color = "Egitim Durumu"  # Legend başlığı
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",  # Legend aşağıda
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

egitim_duzen <- c(
  "Okuma yazma bilmeyen",
  "Okuma yazma bilen fakat bir okul bitirmeyen",
  "Ilkokul",
  "Ortaokul veya dengi meslek okul",
  "Genel lise",
  "Lise dengi meslek okul",
  "Yuksekokul veya fakulte"
)

ggplot(issizlik_long, aes(x = Yil, y = Issizlik_Orani, color = Egitim_Durumu)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(limits = c(0, 30)) +
  scale_color_manual(
    values = RColorBrewer::brewer.pal(7, "Set1"),
    breaks = egitim_duzen,
    labels = egitim_duzen,
    name = "Egitim Durumu"
  ) +
  labs(
    title = "Yillara Gore Egitim Durumuna Bagli Issizlik Oranlari",
    x = "Yil",
    y = "Issizlik Orani (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5)
  )
----------

  ggplot(issizlik_long, aes(x = Yil, y = Issizlik_Orani, color = Egitim_Durumu)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(
    title = "Yillara Gore Egitim Durumuna Bagli Issizlik Oranlari",
    x = "Yil",
    y = "Issizlik Orani (%)",
    color = "Egitim Durumu"  # Legend başlığı
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",  # Legend aşağıda
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5)
  )


g1 <- ggplot(istihdam, aes(x = Yil, y = Istihdam_Sayisi)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(breaks = seq(0, 40000000, by = 5000000),
                     labels = scales::label_comma()) +
  labs(
    title = "Yillara Gore Istihdam Sayisi",
    x = "Yil",
    y = "Istihdam Sayisi"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey70", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey70", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

g2 <- ggplot(uni_sayisi, aes(x = Yil, y = kumulatif_mezun_verebilecek_uni_sayisi)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(breaks = seq(0, 220, by = 20)) +
  labs(
    title = "Yillara Gore Kumulatif Mezun Verebilecek Universite Sayisi",
    x = "Yil",
    y = "Kumulatif Mezun Verebilecek Universite Sayisi"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey70", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey70", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

(g1 / g2) + plot_annotation(title = "Üniversite ve İstihdam Gelişimi (1988–2024)")


# Ortak veri seti oluştur
istihdam_korelasyon <- inner_join(
  istihdam,
  uni_sayisi %>% select(Yil, kumulatif_mezun_verebilecek_uni_sayisi),
  by = "Yil"
)

cor_result <- cor(
  istihdam_korelasyon$Istihdam_Sayisi,
  istihdam_korelasyon$kumulatif_mezun_verebilecek_uni_sayisi,
  method = "pearson",
  use = "complete.obs"
)

print(cor_result)

ggplot(istihdam_korelasyon, aes(x = kumulatif_mezun_verebilecek_uni_sayisi, y = Istihdam_Sayisi)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  scale_y_continuous(labels = scales::label_comma()) + 
  labs(
    title = "Istihdam Sayisi vs Mezun Verebilecek Universite Sayisi",
    x = "Kumulatif Mezun Verebilecek Universite Sayisi",
    y = "Istihdam Sayisi"
  ) +
  theme_minimal()



# 1. Üniversite sayısı verisi
uni_data <- uni_sayisi %>%
  select(Yil, kumulatif_mezun_verebilecek_uni_sayisi)

# 2. Mezun işsizliği (sadece "Yuksekokul veya fakulte")
mezun_issizlik <- issizlik_long %>%
  filter(Egitim_Durumu == "Yuksekokul veya fakulte") %>%
  select(Yil, Mezun_Issizlik_Orani = Issizlik_Orani)

# 3. Birleştir
analiz_df <- inner_join(uni_data, mezun_issizlik, by = "Yil")


uni_data <- uni_sayisi %>% select(Yil, uni_sayisi = kumulatif_mezun_verebilecek_uni_sayisi)

mezun_issizlik <- issizlik_long %>%
  mutate(Egitim_Durumu = stringi::stri_trans_general(Egitim_Durumu, "Latin-ASCII")) %>%
  filter(Egitim_Durumu == "Yuksekokul veya fakulte") %>%
  select(Yil, issizlik_orani = Issizlik_Orani)

analiz_df <- inner_join(uni_data, mezun_issizlik, by = "Yil")


normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

analiz_df <- analiz_df %>%
  mutate(
    uni_norm = normalize(uni_sayisi),
    issizlik_norm = normalize(issizlik_orani)
  )

library(ggplot2)

ggplot(analiz_df, aes(x = Yil)) +
  geom_line(aes(y = uni_norm), color = "blue", linewidth = 1.2) +
  geom_line(aes(y = issizlik_norm), color = "red", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Universite Sayisi ve Mezun Issizlik Orani (Normalize)",
    x = "Yil",
    y = "Normalize Edilmis Deger"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 11)
  ) +
  annotate("text", x = 1990, y = 0.9, label = "Uni Sayisi", color = "blue") +
  annotate("text", x = 1990, y = 0.7, label = "Issizlik Orani", color = "red")

# 1. Üniversite verisini oku ve temizle
universiteler <- read_excel("Project_Data/Universiteler.xlsx", skip = 1)

# Kolon isimlerini düzelt
names(universiteler)[1:5] <- c("Birim_Adi", "Kurulus_Yili", "Tur", "Il", "Bolge")

# Haritayla eşleşmesi için il isimlerini büyüt
uni_iller <- universiteler %>%
  count(Il, name = "Universite_Sayisi") %>%
  mutate(Il = toupper(Il))

# 2. Türkiye il sınırlarını oku
# Not: Harita dosyasını projene koy: "Project_Data/turkiye_il_sinirlari.geojson"
turkiye_sf <- st_read("Project_Data/turkiye_il_sinirlari.geojson", quiet = TRUE) %>%
  mutate(il_adi = toupper(ADI))  # Haritadaki il adını normalize et

# 3. Üniversite verisi ile harita datasını birleştir
harita_data <- turkiye_sf %>%
  left_join(uni_iller, by = c("il_adi" = "Il")) %>%
  mutate(Universite_Sayisi = replace_na(Universite_Sayisi, 0))

# 4. Renk paleti belirle
pal <- colorBin("YlGnBu", domain = harita_data$Universite_Sayisi, bins = 5)

# 5. Interaktif haritayı oluştur
leaflet(harita_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(Universite_Sayisi),
    weight = 1,
    color = "white",
    fillOpacity = 0.8,
    label = ~paste0(il_adi, ": ", Universite_Sayisi, " universite"),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~Universite_Sayisi,
    title = "Universite Sayisi",
    position = "bottomright"
  )
download.file(
  url = "https://raw.githubusercontent.com/alpers/Turkey-Maps-GeoJSON/master/tr-cities.json",
  destfile = "Project_Data/turkiye_il_sinirlari.geojson",
  mode = "wb"
)
# 1. Üniversite verisini oku ve kolon isimlerini düzelt
universiteler <- read_excel("Project_Data/Universiteler.xlsx", skip = 1)
names(universiteler)[1:5] <- c("Birim_Adi", "Kurulus_Yili", "Tur", "Il", "Bolge")

# 2. Türkçe karakterleri ASCII'ye çevir ve büyük harf yap
universiteler <- universiteler %>%
  mutate(Il = toupper(stri_trans_general(Il, "Latin-ASCII")))

# 3. İl bazında üniversite sayılarını hesapla
uni_iller <- universiteler %>%
  count(Il, name = "Universite_Sayisi")

# 4. Harita verisini oku (GeoJSON)
turkiye_sf <- st_read("Project_Data/turkiye_il_sinirlari.geojson", quiet = TRUE)

# 5. Haritadaki il isimlerini de normalize et
turkiye_sf <- turkiye_sf %>%
  mutate(il_adi = toupper(stri_trans_general(name, "Latin-ASCII")))

# 6. Harita ile üniversite sayılarını birleştir
harita_data <- turkiye_sf %>%
  left_join(uni_iller, by = c("il_adi" = "Il")) %>%
  mutate(Universite_Sayisi = replace_na(Universite_Sayisi, 0))

# 7. Harita üzeri etiket koordinatları (her ilin ortası)
harita_centroids <- st_centroid(harita_data)

# 8. Renk paleti tanımla
pal <- colorBin("YlGnBu", domain = harita_data$Universite_Sayisi, bins = 5)

# 9. Haritayı çiz
leaflet(harita_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(Universite_Sayisi),
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste0(il_adi, ": ", Universite_Sayisi, " universite"),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      bringToFront = TRUE
    )
  ) %>%
  addLabelOnlyMarkers(
    data = harita_centroids,
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    label = ~as.character(Universite_Sayisi),
    labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                style = list("font-weight" = "bold", "color" = "black", "font-size" = "11px"))
  ) %>%
  addLegend(
    pal = pal,
    values = ~Universite_Sayisi,
    title = "Universite Sayisi",
    position = "bottomright"
  )
---------------

# --- Grafik 1: Üniversite Sayısı
g1 <- ggplot(uni_sayisi, aes(x = Yil, y = kumulatif_mezun_verebilecek_uni_sayisi)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(breaks = seq(0, 220, by = 20)) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 2006, y = 210, label = "2006: Her ile bir\nuniversite politikası",
           color = "red", size = 3.5, hjust = -0.1, vjust = 1) +
  labs(
    title = "Yıllara Göre Kümülatif Mezun Verebilecek Üniversite Sayısı",
    x = "Yıl", y = "Üniversite Sayısı"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# --- Grafik 2: İstihdam
g2 <- ggplot(istihdam, aes(x = Yil, y = Istihdam_Sayisi)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(breaks = seq(0, 40000000, by = 5000000),
                     labels = label_comma()) +
  labs(
    title = "Yıllara Göre İstihdam Sayısı",
    x = "Yıl", y = "İstihdam"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# --- Grafik 3: İşsizlik
g3 <- ggplot(issizlik_long, aes(x = Yil, y = Issizlik_Orani, color = Egitim_Durumu)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1988, 2024, by = 5)) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(
    title = "Yıllara Göre Eğitim Durumuna Bağlı İşsizlik Oranı",
    x = "Yıl", y = "İşsizlik (%)", color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# --- 3 grafiği alt alta sırala
grid.arrange(g1, g2, g3, ncol = 1)

(g1 / g2 / g3) + 
  plot_annotation(title = "1988–2024 Arası Üniversiteleşme, İstihdam ve İşsizlik Trendleri",
                  theme = theme(plot.title = element_text(size = 14, hjust = 0.5)))
