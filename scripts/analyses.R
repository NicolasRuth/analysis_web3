library(tidyverse)
library(wordcloud)
library(ggpubr)
theme_set(theme_bw())

# Load data
df <- read.csv("data/data_w3.csv", sep = ";")
# Load overall data (ratio between articles with and without Web3)
dat_all <- read.csv("data/data_w3_overview.csv", sep = ";")

# 1. Amount of media coverage over the years 2016-2022

df_yearly <- df %>%
  group_by(year) %>%
  summarise(number = n()) %>%
  arrange(year)

df_yearly

plot_yearly <- ggplot(df, aes(as.factor(year)))
plot_yearly + geom_bar(aes(fill = magazine)) +
  labs(fill = "Magazines",
       x = "Years",
       y = "Number of articles")

# alternativ transparent bg

plot_yearly <- ggplot(df, aes(as.factor(year))) +
  geom_bar(aes(fill = magazine)) +
  labs(fill = "Magazines",
       x = "Jahre",
       y = "Anzahl der Artikel") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18))

ggsave("plot_yearly.png", plot = plot_yearly, bg = "transparent", width = 8, height = 4)


## Differences between magazines

df_by_magazine <- df %>%
  group_by(magazine) %>%
  summarise(number = n()) %>%
  arrange(-number)

df_by_magazine <- rbind(df_by_magazine, c("Musik & Bildung", 0))
df_by_magazine <- rbind(df_by_magazine, c("Rondo", 0))
df_by_magazine <- rbind(df_by_magazine, c("Sound & Recording", 0))
df_by_magazine$number <- as.numeric(df_by_magazine$number)

plot_by_magazine <- ggplot(df_by_magazine,
                           aes(x = reorder(magazine, -number), y = number)) +
  geom_bar(stat = "identity", aes(fill = magazine)) +
  coord_flip() +
  labs(x = "Magazines",
       y = "Number of articles") +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none")

plot_by_magazine

# 2. Web3 as main or sub theme

df_mweb3n_vs_sub <- df %>%
  group_by(main_sub) %>%
  summarise(Anzahl = n())

df_mweb3n_vs_sub # 1 = mweb3n, 2 = subtheme

# 3. Specific Web3 topics

df_web3_topics <- df %>%
  select(w3_topic_1, w3_topic_2, w3_topic_3) %>%
  gather(key = "Category", value = "Topic") %>%
  group_by(Topic) %>%
  summarise(Number = n()) %>%
  arrange(-Number)

df_web3_topics

# 4. Overall topics

df_general_topic <- df %>%
  group_by(topic) %>%
  summarise(Number = n()) %>%
  arrange(-Number)

df_general_topic

wordcloud(words = df_general_topic$topic,
          freq = df_general_topic$Number, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

# 5. People and organisations
## People

df_people <- df %>%
  select(starts_with("people")) %>%
  gather(key = "PeopleNum", value = "People") %>%
  group_by(People) %>%
  summarise(Number = n()) %>%
  arrange(-Number)

df_people

## Organisations
df_companies <- df %>%
  select(starts_with("companies")) %>%
  gather(key = "CompaniesNum", value = "Companies") %>%
  group_by(Companies) %>%
  summarise(Number = n()) %>%
  arrange(-Number)

df_companies

# 6. Framing of articles
df_framing <- df %>%
  group_by(framing) %>%
  summarise(Number = n())

mean(df$framing)
sd(df$framing)

df_framing

plot_framing <- ggplot(df_framing,
                       aes(x = framing,
                           y = Number,
                       fill = factor(framing))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Reds") +
  labs(x = "Negative to positive framing",
       y = "Number of articles") +
  theme(legend.position = "none")

# alternativ trans bg

plot_framing <- ggplot(df_framing,
                       aes(x = framing,
                           y = Number,
                           fill = factor(framing))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Oranges") +
  labs(x = "Negatives bis positives Framing",
       y = "Anzahl der Artikel") +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18))

ggsave("plot_framing.png", plot = plot_framing, bg = "transparent", width = 6, height = 4)


plot_framing

xtabs(~df$framing+df$magazine)

group_by(df, magazine) %>%
  summarise(
    mean = mean(framing, na.rm = TRUE),
    sd = sd(framing, na.rm = TRUE),
    number = count(framing, na.rm = TRUE)
  ) %>%
  arrange(-mean)

# 7. TAM
## Usability
df_easeofuse <- df %>%
  group_by(eou_tam) %>%
  summarise(Number = n())

df_easeofuse

plot_easeofuse <- ggplot(df_easeofuse,
                         aes(x = eou_tam, y = Number)) +
  geom_bar(stat = "identity", fill = "darkolivegreen2") +
  ylim(0, 10) +
  labs(x = "Perceived ease of use (TAM)",
       y = "Number of articles")

plot_easeofuse

## Usefulness
df_usability <- df %>%
  group_by(usa_tam) %>%
  summarise(Number = n())

df_usability

plot_usability <- ggplot(df_usability, aes(x = usa_tam,
                                             y = Number)) +
  geom_bar(stat = "identity", fill = "darkolivegreen3") +
  labs(x = "Perceived usability (TAM)",
       y = "Number of articles")

plot_usability

## Enjoyment
df_enjoyment <- df %>%
  group_by(enj_tam) %>%
  summarise(Number = n())

df_enjoyment

plot_enjoyment <- ggplot(df_enjoyment, aes(x = enj_tam,
                                           y = Number)) +
  geom_bar(stat = "identity", fill = "darkolivegreen4") +
  labs(x = "Perceived enjoyment (TAM)",
       y = "Number of articles")

plot_enjoyment

combinedTAM <- ggarrange(plot_easeofuse, plot_usability, plot_enjoyment,
                           ncol = 3, nrow = 1)

combinedTAM

# alternative in trans bg

plot_easeofuse <- ggplot(df_easeofuse,
                         aes(x = eou_tam, y = Number)) +
  geom_bar(stat = "identity", fill = "darkolivegreen2") +
  ylim(0, 10) +
  labs(x = "Wg. Benutzerfreundlichkeit (TAM)",
       y = "Anzahl der Artikel") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18))

plot_usability <- ggplot(df_usability, aes(x = usa_tam,
                                           y = Number)) +
  geom_bar(stat = "identity", fill = "darkolivegreen3") +
  labs(x = "Wg. Nützlichkeit (TAM)",
       y = "Anzahl der Artikel") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18))

plot_enjoyment <- ggplot(df_enjoyment, aes(x = enj_tam,
                                           y = Number)) +
  geom_bar(stat = "identity", fill = "darkolivegreen4") +
  labs(x = "Wg. Unterhaltungswert (TAM)",
       y = "Anzahl der Artikel") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18))

combinedTAM_trans <- ggarrange(plot_easeofuse, plot_usability, plot_enjoyment,
                         ncol = 3, nrow = 1)

ggsave("combinedTAM.png", plot = combinedTAM_trans, bg = "transparent", width = 14, height = 4)
