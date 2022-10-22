# Loading libraries
library(tidyverse)
library(tidytext)
library(textdata)
library(readxl)
library(stopwords)



# Downloading dialogue data
if (!file.exists('..raw_data/subtitles.xlsx')) {
        url <- 'https://drive.google.com/u/0/uc?id=1hPF9QMA5x7nes3BD2MFiIrp08xY9EJo6&export=download'
        download.file(url,
                      'raw_data/subtitles.xlsx',
                      mode = 'wb')
}



# Reading in data
dat <- 
        readxl::read_excel(path = '../raw_dat/subtitles.xlsx',
                           sheet = 2,
                           col_names = FALSE) %>%
        # Changing column names
        setNames(c('character', 
                   'line', 
                   'season.episode', 
                   'line.number')) %>%
        select(-line.number) %>%
        # Separating season.episode column
        separate(season.episode,
                 into = c('season', 
                          'episode'),
                 sep = '\\.') %>%
        # Retaining only main characters
        filter(character %in% c('Abed', 
                                'Annie', 
                                'Britta', 
                                'Chang', 
                                'Dean', 
                                'Jeff', 
                                'Pierce', 
                                'Shirley', 
                                'Troy')) %>%
        # Changing character names to factors
        # Ordered as they are credited in the intro, with precedence to the
        # characters that were present for all six seasons
        mutate(character = factor(character,
                                   levels = c('Jeff',
                                              'Britta',
                                              'Abed',
                                              'Annie',
                                              'Chang',
                                              'Dean',
                                              'Shirley',
                                              'Troy',
                                              'Pierce')))



# Fixing formatting errors
dat[grep(pattern = '^1$', 
         x = dat$episode), 4] <- '10'
dat[grep(pattern = '^2$', 
         x = dat$episode), 4] <- '20'
dat <-
        dat %>% 
        type_convert(col_types = cols(character = col_character(), 
                                      line = col_character(), 
                                      season = col_double(), 
                                      episode = col_double()))



# Total word counts
word_counts <- 
        dat %>%
        unnest_tokens(word, 
                      line) %>%
        count(character, 
              season, 
              episode)

# Total word counts without stopwords
word_counts_filt <-
        dat %>%
        unnest_tokens(word, 
                      line) %>%
        anti_join(get_stopwords()) %>%
        count(character, 
              season, 
              episode)



# Total line counts
line_counts <-
        dat %>%
        count(character, 
              season, 
              episode) 



# Sentiment data
sentiment_dat <- 
        dat %>%
        unnest_tokens(word, 
                      line) %>%
        anti_join(get_stopwords()) %>%
        inner_join(get_sentiments('afinn')) %>%
        mutate(sentiment = factor(ifelse(value > 0,
                                         'positive',
                                         'negative'),
                                  levels = c('positive',
                                             'negative')))



# Word frequencies
word_freq <- 
        dat %>%
        unnest_tokens(word, 
                      line) %>%
        count(word, character)

# Word frequencies without stopwords
word_freq_filt <- 
        dat %>%
        unnest_tokens(word, 
                      line) %>%
        anti_join(get_stopwords()) %>%
        count(word, character)



# Writing data
rm(dat)
save.image('raw_dat/data.RData')