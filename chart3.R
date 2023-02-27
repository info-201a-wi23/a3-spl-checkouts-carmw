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

time_of_year_checkouts <- spl_df %>% 
  mutate(SummerCheckout = if_else(CheckoutMonth %in% c(6, 7, 8), "summer",
                                  "not summer")) %>% 
  group_by(SummerCheckout, MaterialType) %>%
  summarise(total_checkouts = mean(Checkouts, na.rm = TRUE))


ggplot(time_of_year_checkouts) +
  geom_col(mapping = aes(x = MaterialType, y = total_checkouts, fill =
                           SummerCheckout), position = "dodge") +
  labs(title = "Average SPL Book, Ebook, and AudioBook Checkouts by Time of Year",
       x = "Publication Medium", y = "Checkouts") +
  scale_fill_discrete(name = "Time of Year", labels = c("Rest of Year", "Summer"))