# Loading in libraries
library(rvest)
library(tidyverse)

# Scraping links to scripts for individual episodes
url <- 'https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=community'

ep_list <- 
        read_html(url) %>%
        html_nodes('.text-dark')

episodes <- c()

for (i in 1:length(ep_list)) {
        if (grepl(pattern = 'episode=s[0-9]{2}e[0-9]{2}', x = ep_list[i])) {
                reg_match <- 
                        regexec(pattern = '(episode=s[0-9]{2}e[0-9]{2})',
                                text = ep_list[i])
                episodes <- c(episodes, 
                              regmatches(ep_list[i], 
                                               reg_match)[[1]][1])
        }
}



# Scraping scripts from episode links
for (i in 1:length(episodes)) {
        url <- paste('https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=community&', 
                     episodes[i], 
                     sep = '')
        dialogue_data <- read_html(url) %>%
                html_nodes('.scrolling-script-container')
        reg_match <- 
                regexec(pattern = '\t\t\t(.+)',
                        text = dialogue)
        dialogue <- 
                regmatches(dialogue, reg_match)[[1]] %>%
                str_split(pattern = '<br>')
        dialogue[1:(grep(pattern = '^$', 
                         x = dialogue)[1] - 1)] %>%
                # Writing scripts to csv files
                write.csv(file = paste('raw_dat/',
                                       gsub(pattern = 'episode=', 
                                            replacement='', 
                                            x = episodes[i]), 
                                       '.csv', 
                                       sep = ''))
}



closeAllConnections()