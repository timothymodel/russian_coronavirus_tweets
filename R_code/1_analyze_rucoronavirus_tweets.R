##### If you do not see below Russian cyrillic,
##### file must be opened with UTF-8 Encoding

##### System Setup (WINDOWS) #####

Sys.setlocale('LC_ALL', 'russian') # Plays nice with cyrillic characters
Sys.setlocale("LC_TIME", "English") # English for time converstion

##### Load Relevant Packages #####

library(jsonlite)
library(data.table)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(quanteda)
library(pals)
library(tidytext)

setwd("C:/Users/model/OneDrive/Research/COVID-19/OSoME Twitter/")

korona_df <- readRDS("./koronasub.RDS")
korona_df <- as.data.frame(korona_df)

##### Clean Data #####

# Remove Non-Russia User Locations #

non_rusloc <- c(
  "Ukrain",
  "Укра",
  "Казах",
  "Kazakh",
  "Кыргиз",
  "Mongol",
  "Монгол",
  "Minsk",
  "Минск",
  "Tashkent",
  "London",
  "Киев",
  "Беларус",
  "Belarus",
  "Uzbek",
  "Узбек",
  "Ulanb",
  "Ulaanb",
  "Уланб",
  "Dubai",
  "Bishkek",
  "Бишкек",
  "Нью-Йорк",
  "New York",
  "Київ",
  "Киев",
  "Kiev",
  "Астана",
  "Astana"
)

korona_df <- korona_df[!grepl(paste(non_rusloc,
                                    collapse = "|"),
                              korona_df$user.location), ]

# Remove Retweets and Common Non-Russian Language Patterns #

non_ruslang <- c("RT @", "ө", "ї", "үү")

korona_df <- korona_df[!grepl(paste(non_ruslang,
                                    collapse = "|"),
                              korona_df$user.location), ]

# Create Time Variable

korona_df$timestamp <- strptime(korona_df$created_at,
                                format = "%a %b %d %H:%M:%S %z %Y")
korona_df$week <- week(korona_df$timestamp)
korona_df$day <- as.Date(korona_df$timestamp)

# Histogram By Day #

annotation_r <- data.frame(
  x = as.Date(c("2020-03-30", "2020-04-30")),
  y = c(30000, 30000),
  label = c("Moscow Lockdown", "Mishustin,Infected")
)

annotation_l <- data.frame(
  x = as.Date(c("2020-01-23", "2020-03-02")),
  y = c(30000, 30000),
  label = c("Wuhan Lockdown", "First Moscow Case")
)

coronavirus_tweets <- ggplot(korona_df, aes(x = day)) +
  geom_histogram(
    color = "black",
    fill = "white",
    size = 1,
    bins = 50
  ) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-23")),
             linetype = 3,
             size = 1) + # Wuhan Lockdown
  geom_vline(xintercept = as.numeric(as.Date("2020-03-02")),
             linetype = 3,
             size = 1) + # 1st Case in Moscow
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")),
             linetype = 3,
             size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-30")),
             linetype = 3,
             size = 1) +
  geom_text(
    data = annotation_r,
    aes(x = x, y = y, label = label),
    size = 4,
    color = "black"
  ) +
  geom_text(
    data = annotation_l,
    aes(x = x, y = y, label = label),
    size = 4,
    color = "black"
  ) +
  ylab("# of Tweets") +
  xlab("Date") +
  ggtitle("Russian 'coronavirus' tweets over time") +
  theme_minimal() +
  theme(text = element_text(size = 20))

ggsave(
  coronavirus_tweets,
  device = "pdf",
  width = 6,
  height = 4,
  path = "./coronavirus_tweets.pdf"
)

# Sample #

set.seed(98765)

korona_sample <- korona_df %>%
  group_by(week) %>% # Group data frame by week
  sample_frac(.2) # Take some % of tweets per week

##### Pre-Process Text #####

korona_corpus <- corpus(korona_sample$text)

korona_corpus <- korona_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("russian")) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_wordstem(language = "russian")

korona_dtm <- korona_corpus %>%
  dfm()

##### Term Categories Time Series #####

# Time Series Comparison of Foreign, National, and Regional Word Categories

terms <- c(
  # foreign terms
  "америк",
  "сша",
  "трамп",
  "вашингтон",
  "кита",
  "вухан",
  "си",
  "европ",
  "меркел",
  "макрон",
  "джонсон",
  "париж",
  
  # national terms
  "путин",
  "рф",
  "россия",
  "русс",
  "конституц",
  "власт",
  
  # regional terms
  "регион",
  "област",
  "губернатор",
  "москв",
  "петербург",
  "питер"
)

korona_dtm_ts <- as.matrix(korona_dtm[, terms])
korona_countsweek <- aggregate(korona_dtm_ts,
                               by = list(week = korona_sample$week),
                               sum)
korona_freq <- korona_countsweek[, terms]
korona_total_weekly <- korona_sample %>%
  group_by(week) %>%
  count()

korona_countsday <- aggregate(korona_dtm_ts,
                              by = list(day = korona_sample$day),
                              sum)
korona_freq <- korona_countsday[, terms]
korona_total_daily <- korona_sample %>%
  group_by(day) %>%
  count()

korona_plottingdata <- cbind(korona_total_daily, korona_freq)
korona_plottingdata$Foreign <-
  (rowSums(korona_plottingdata[3:14])) / korona_plottingdata$n
korona_plottingdata$National <-
  (rowSums(korona_plottingdata[15:20])) / korona_plottingdata$n
korona_plottingdata$Regional <-
  (rowSums(korona_plottingdata[21:26])) / korona_plottingdata$n

korona_tsplot_wordgroups <-
  ggplot(subset(korona_plottingdata, day > "2020-01-14"), aes(x = day)) +
  geom_area(aes(y = Foreign,
                fill = "Foreign",
                color = "Foreign"),
            alpha = .5,
            size = 1) +
  geom_area(
    aes(y = National,
        fill = "National",
        color = "National"),
    alpha = .5,
    size = 1
  ) +
  geom_area(
    aes(y = Regional,
        fill = "Regional",
        color = "Regional"),
    alpha = .5,
    size = 1
  ) +
  ylab("Proportion of daily coronavirus tweets") +
  xlab("Date") +
  ggtitle("Russian 'coronavirus' tweet categories over time") +
  scale_fill_manual(name = "Categories",
                    values = c("#482677FF",
                               "#55C667FF",
                               "#FDE725FF")) +
  scale_color_manual(values = c("#482677FF",
                                "#55C667FF",
                                "#FDE725FF")) +
  guides(colour = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 20))

ggsave(
  korona_tsplot_wordgroups,
  device = "pdf",
  width = 6,
  height = 4,
  path = "./korona_termcategories.pdf"
)

##### Sentiment Time Series #####

ru_dict <-
  readr::read_csv(
    "https://raw.githubusercontent.com/text-machine-lab/sentimental/master/sentimental/word_list/russian.csv"
  )

korona_sentiment <- korona_df %>%
  unnest_tokens(output = "tweet_text",
                input = "text",
                token = "tweets") %>%
  unnest_tokens(output = "word",
                input = "tweet_text",
                token = "words") %>%
  inner_join(ru_dict, by = "word")

vtsiom_putintrust <- as.data.frame(cbind(putintrust, pollday))

korona_sentiment_daily <- korona_sentiment %>%
  group_by(day) %>%
  summarise(sentiment = mean(score))

korona_sentiment_plot <-
  ggplot(subset(korona_sentiment_daily, day > "2020-01-10"),
         aes(x = day, y = sentiment)) +
  geom_line(size = 2,
            color = "black",
            alpha = .2) +
  stat_smooth(
    method = "loess",
    color = "#366092",
    fill = "#366092",
    size = 1
  ) +
  ylab("Sentiment ([+] = positive emotion, [-] = negative emotion)") +
  xlab("Date") +
  ggtitle("Russian 'coronavirus' tweet sentiment over time") +
  theme_minimal() +
  theme(text = element_text(size = 20))

ggsave(
  korona_sentiment_plot,
  device = "pdf",
  width = 6,
  height = 4,
  path = "./korona_sentimentplot.pdf"
)
