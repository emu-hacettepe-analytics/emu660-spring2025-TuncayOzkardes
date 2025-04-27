install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

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
