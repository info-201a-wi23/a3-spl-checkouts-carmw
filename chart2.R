library("ggplot2")
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

written_or_audio_checkouts <- book_formats_df %>% 
  group_by(CheckoutYear, WrittenOrAudio) %>%
  summarise(total_checkouts = sum(Checkouts, na.rm = TRUE))

ggplot(written_or_audio_checkouts) +
  geom_line(mapping = aes(x = CheckoutYear, y = total_checkouts, color =
                            WrittenOrAudio)) +
  labs(title = "SPL Book, Ebook, and AudioBook Checkouts 2017-2022",
       x = "Year", y = "Checkouts", color = "Publication Medium") +
  scale_color_discrete(labels = c("Audiobooks", "Written (Books & Ebooks)")) +
  scale_x_continuous(limits = c(2017, 2022))