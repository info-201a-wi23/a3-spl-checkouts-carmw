library("dplyr")
library("tidyr")
library("stringr")

spl_df <- read.csv("~/../Desktop/INFO 201/a3-spl-checkouts-carmw/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>%
  filter(MaterialType %in% c("BOOK", "EBOOK", "AUDIOBOOK"))

book_formats_df <- spl_df %>% 
  mutate(WrittenOrAudio = if_else(MaterialType == "AUDIOBOOK", "audio",
                                  "written")) %>% 
  mutate(PhysicalOrDigital = if_else(MaterialType == "BOOK", "physical",
                                     "digital"))
  
physical_or_digital_checkouts <- book_formats_df %>% 
  group_by(CheckoutYear, PhysicalOrDigital) %>%
  summarise(total_checkouts = sum(Checkouts, na.rm = TRUE))

written_or_audio_checkouts <- book_formats_df %>% 
  group_by(CheckoutYear, WrittenOrAudio) %>%
  summarise(total_checkouts = sum(Checkouts, na.rm = TRUE))

time_of_year_checkouts <- spl_df %>% 
  mutate(SummerCheckout = if_else(CheckoutMonth %in% c(6, 7, 8), "summer",
                                  "not summer")) %>% 
  group_by(SummerCheckout, MaterialType) %>%
  summarise(total_checkouts = mean(Checkouts, na.rm = TRUE))
  

p_2019 <- physical_or_digital_checkouts %>% 
  filter(CheckoutYear == 2019) %>% 
  filter(PhysicalOrDigital == "physical") %>% 
  pull(total_checkouts)

d_2019 <- physical_or_digital_checkouts %>% 
  filter(CheckoutYear == 2019) %>% 
  filter(PhysicalOrDigital == "digital") %>% 
  pull(total_checkouts)

p_2020 <- physical_or_digital_checkouts %>% 
  filter(CheckoutYear == 2020) %>% 
  filter(PhysicalOrDigital == "physical") %>% 
  pull(total_checkouts)

d_2020 <- physical_or_digital_checkouts %>% 
  filter(CheckoutYear == 2020) %>% 
  filter(PhysicalOrDigital == "digital") %>% 
  pull(total_checkouts)

physical_digital_diff_pre_covid <- p_2019 - d_2019
  
physical_digital_diff_covid <- p_2020 - d_2020

change_in_physical <- p_2020 - p_2019
  
change_in_digital <- d_2020 - d_2019
  

a_2019 <- written_or_audio_checkouts %>% 
  filter(CheckoutYear == 2019) %>% 
  filter(WrittenOrAudio == "audio") %>% 
  pull(total_checkouts)

w_2019 <- written_or_audio_checkouts %>% 
  filter(CheckoutYear == 2019) %>% 
  filter(WrittenOrAudio == "written") %>% 
  pull(total_checkouts)

a_2020 <- written_or_audio_checkouts %>% 
  filter(CheckoutYear == 2020) %>% 
  filter(WrittenOrAudio == "audio") %>% 
  pull(total_checkouts)

w_2020 <- written_or_audio_checkouts %>% 
  filter(CheckoutYear == 2020) %>% 
  filter(WrittenOrAudio == "written") %>% 
  pull(total_checkouts)

written_audio_diff_pre_covid <- w_2019 - a_2019

written_audio_diff_covid <- w_2020 - a_2020

written_lost_lead <- written_audio_diff_pre_covid - written_audio_diff_covid
