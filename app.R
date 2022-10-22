## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(textdata)
library(stopwords)

# Loading in data
load('raw_data/data.RData')

# Define UI for app
ui <- dashboardPage(
        
        # Setting skin
        skin = 'black',
        
        # App title
        dashboardHeader(title = span("COMMUNITY", 
                                          style = "font-family: 'Impact'; color: navy; font-size: 36px; font-weight: bold")),
        
        # Sidebar layout 
        dashboardSidebar(
                sidebarMenu(
                        menuItem('About',
                                 tabName = 'about',
                                 icon = icon('question')),
                        menuItem('Summary',
                                 tabName = 'summary', 
                                 icon = icon('comment-dots')),
                        menuItem('Sentiment Analysis',
                                 tabName = 'sent',
                                 icon = icon('theater-masks')),
                        menuItem('Word Frequencies',
                                 tabName = 'freq',
                                 icon = icon('sort-amount-down'))
                ),
                br(),
                
                # Input: Selecting characters
                checkboxGroupInput('characters',
                                   h4('Select characters:'),
                                   choices = list('Abed' = 'Abed',
                                                  'Annie' = 'Annie',
                                                  'Britta' = 'Britta',
                                                  'Chang' = 'Chang',
                                                  'Dean' = 'Dean',
                                                  'Jeff' = 'Jeff',
                                                  'Pierce' = 'Pierce',
                                                  'Shirley' = 'Shirley',
                                                  'Troy' = 'Troy'
                                   ),
                                   selected = c('Abed',
                                                'Annie',
                                                'Britta',
                                                'Chang',
                                                'Dean',
                                                'Jeff',
                                                'Pierce',
                                                'Shirley',
                                                'Troy'))
        ),
        
        # Main panel for displaying outputs
        dashboardBody(
                tabItems(
                        
                        # About section
                        tabItem(tabName = 'about',
                                img(src = 'Community_2009_logo.png',
                                    height = 58,
                                    width = 400),
                                br(),
                                br(),
                                p('Community is a sitcom that ran from 2009 to
                                2014, spanning over 110 episodes spread over six 
                                  seasons.'), 
                                p('In anticipation for the Community film,
                                ordered recently by the NBC streaming service
                                Peacock, I built this simple dashboard to 
                                illustrate some of the text analysis I performed
                                with the dialogue spoken by all nine main
                                  characters in the show'),
                                p('You can navigate through this dashboard and
                                display different subsets of characters in the
                                plots by clicking through the tabs and
                                  checkboxes in the sidebar to the left.'),
                                p('All credits go to the script database',
                                  a('Springfield! Springfield!', 
                                    href = 'https://www.springfieldspringfield.co.uk/'),
                                  'for providing the raw scripts, and',
                                  a('u/running_tiger',
                                    href = 'https://www.reddit.com/user/running-tiger/'),
                                  'on Reddit for the speaker annotations.')
                        ),
                        
                        # Section for displaying word and line summaries
                        tabItem(tabName = 'summary',
                                fluidRow(
                                        column(width = 3,
                                               
                                               # Input: Display words or lines
                                               box(title = 'Data control',
                                                   status = 'danger',
                                                   solidHeader = TRUE,
                                                   width = NULL,
                                                   radioButtons('summary_control',
                                                                'Sort by:',
                                                                choices = list('Words' = 'words',
                                                                               'Lines' = 'lines'))
                                               ),
                                               
                                               # Input: Whether to filter out inconsequential words
                                               box(title = 'Data filtering',
                                                   status = 'success',
                                                   solidHeader = TRUE,
                                                   width = NULL,
                                                   radioButtons('filter_control',
                                                                'Remove stopwords:',
                                                                choices = list('Yes' = 'yes',
                                                                               'No' = 'no'),
                                                                selected = 'no')
                                               )
                                        ),
                                        
                                        # Output: plot1
                                        box(title = uiOutput('title_1'),
                                            status = 'primary',
                                            collapsible = TRUE,
                                            width = 9,
                                            plotOutput('plot1')
                                        )
                                        
                                ),
                                
                                fluidRow(
                                        
                                        # Output: plot2
                                        box(title = uiOutput('title_2'),
                                            status = 'primary',
                                            collapsible = TRUE,
                                            width = 12,
                                            plotOutput('plot2')
                                        )
                                ),
                                fluidRow(
                                        
                                        # Output: plo3
                                        box(title = uiOutput('title_3'),
                                            status = 'primary',
                                            collapsible = TRUE,
                                            width = 12,
                                            plotOutput('plot3')
                                        )
                                )
                        ),
                        
                        # Section for displaying sentiments
                        tabItem(tabName = 'sent',
                                
                                # Output: plot4
                                fluidRow(
                                        box(title = 'Ratio of positive-negative sentiments',
                                            status = 'primary',
                                            collapsible = TRUE,
                                            width = 12,
                                            plotOutput('plot4')
                                        )
                                ),
                                
                                # Output: plot5
                                fluidRow(box(title = 'Sentiment score per season',
                                             status = 'primary',
                                             collapsible = TRUE,
                                             width = 12,
                                             plotOutput('plot5')
                                )
                                ),
                                
                                # Output: plot6
                                fluidRow(
                                        box(title = 'Sentiment score per episode',
                                            status = 'primary',
                                            collapsible = TRUE,
                                            width = 12,
                                            plotOutput('plot6')
                                        )
                                )
                        ),
                        
                        # Section for displaying word frequencies
                        tabItem(tabName = 'freq',
                                
                                # Input: Selecting character
                                fluidRow(
                                        column(width = 3,
                                               box(title = 'Character',
                                                   status = 'danger',
                                                   solidHeader = TRUE,
                                                   width = NULL,
                                                   selectInput('character_freq',
                                                               'Select character:',
                                                               choices = list('Abed' = 'Abed',
                                                                              'Annie' = 'Annie',
                                                                              'Britta' = 'Britta',
                                                                              'Chang' = 'Chang',
                                                                              'Dean' = 'Dean',
                                                                              'Jeff' = 'Jeff',
                                                                              'Pierce' = 'Pierce',
                                                                              'Shirley' = 'Shirley',
                                                                              'Troy' = 'Troy'
                                                               ),
                                                               selected = c('Abed'))
                                               ),
                                               
                                               # Input: Choosing raw word frequencies (tf) or frequencies balanced by importance relative to character (tf-idf)
                                               box(title = 'Algorithm',
                                                   status = 'success',
                                                   solidHeader = TRUE,
                                                   width = NULL,
                                                   radioButtons('freq_algo',
                                                                'Formula for calculating word frequencies:',
                                                                choices = list('Term frequency' = 'tf',
                                                                               'TF-IDF' = 'tf_idf'),
                                                                selected = 'tf_idf')
                                               ),
                                               
                                               # Input: Whether to filter out inconsequential words
                                               box(title = 'Data filtering',
                                                   status = 'info',
                                                   solidHeader = TRUE,
                                                   width = NULL,
                                                   radioButtons('filter_control_freq',
                                                                'Remove stopwords:',
                                                                choices = list('Yes' = 'yes',
                                                                               'No' = 'no'),
                                                                selected = 'yes')
                                               ),
                                               
                                               # Input: How many entries to output
                                               box(title = 'Number of words',
                                                   status = 'warning',
                                                   solidHeader = TRUE,
                                                   width = NULL,
                                                   sliderInput('n_words',
                                                                'Display n entries:',
                                                                min = 0, 
                                                               max = 100, 
                                                               value = 25)
                                               )
                                        ),
                                        
                                        # Output: tab1
                                        box(title = 'Word frequencies',
                                            status = 'primary',
                                            collapsible = TRUE,
                                            width = 3,
                                            tableOutput('table1')
                                        )
                                )
                        )
                )
        )
)

# Define server logic
server <- function(input, output) { 
        
        # Reactive expression to choose data for summary tab
        df_summary <- reactive({
                if(input$summary_control == 'words') {
                        if(input$filter_control == 'yes') {
                                return(word_counts_filt)
                        }
                        else {
                                return(word_counts)
                        }
                }
                else {
                        return(line_counts)
                }
        })
        
        # Reactive expression to choose data for word frequency tab
        df_freq <- reactive({
                switch(input$filter_control_freq,
                       'yes' = 'word_freq_filt',
                       'no' = 'word_freq')
        })
        
        # Generating title for plot1
        output$title_1 <-
                renderUI({
                        paste('Total',
                              input$summary_control, 
                              'spoken')
                })
        
        # Generating plot1
        output$plot1 <- 
                renderPlot({
                        df_summary() %>% 
                                group_by(character, 
                                         season) %>% 
                                summarise(total_words = sum(n)) %>% 
                                mutate(season = as.factor(paste('Season',
                                                                season))) %>%
                                filter(character %in% c(input$characters)) %>%
                                ggplot(aes(x = character, 
                                           y = total_words,
                                           fill = season)) + 
                                geom_bar(position = 'stack',
                                         stat = 'identity') +
                                theme_minimal() +
                                labs(fill = '') +
                                theme(axis.title.x = element_blank(),
                                      axis.title.y = element_blank())
                })
        
        # Generating title for plot2
        output$title_2 <-
                renderUI({
                        paste('Proportion of total', 
                              input$summary_control, 
                              'spoken per season')
                })
        
        # Generating plot2
        output$plot2 <- 
                renderPlot({
                        df_summary() %>% 
                                group_by(character, 
                                         season) %>% 
                                summarise(total_words = sum(n)) %>%
                                group_by(season) %>%
                                mutate(season = as.factor(paste('Season',
                                                                season)),
                                       proportion = total_words / sum(total_words)) %>%
                                filter(character %in% c(input$characters)) %>%
                                ggplot(aes(x = character, 
                                           y = proportion,
                                           fill = character)) + 
                                geom_bar(position = 'stack',
                                         stat = 'identity') +
                                theme_minimal() +
                                facet_grid(~season) +
                                scale_fill_brewer(palette = 'Paired') +
                                theme(legend.position = 'none',
                                      axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      axis.text.x = element_text(angle = 90,
                                                                 vjust = 0.5,
                                                                 hjust = 1))
                })
        
        # Generating title for plot3
        output$title_3 <-
                renderUI({
                        paste('Total',
                              input$summary_control, 
                              'spoken per episode')
                })
        
        # Generating plot3
        output$plot3 <- 
                renderPlot({
                        df_summary() %>%
                                group_by(character, 
                                         season, 
                                         episode) %>%
                                summarise(total_words = sum(n)) %>%
                                mutate(season = as.factor(paste('Season',
                                                                season))) %>%
                                filter(character %in% c(input$characters)) %>%
                                ggplot(aes(x = episode,
                                           y = total_words,
                                           color = character,
                                           group = character)) +
                                geom_point() +
                                geom_line() +
                                theme_minimal() +
                                facet_grid(~season) +
                                scale_color_brewer(palette = 'Paired') +
                                xlab('Episode') +
                                labs(color = '') +
                                theme(axis.title.y = element_blank())
                })
        
        # Generating plot4
        output$plot4 <-
                renderPlot({
                        sentiment_dat %>%
                                count(sentiment, 
                                      character, 
                                      season) %>%
                                group_by(character,
                                         season) %>%
                                mutate(season = as.factor(paste('Season',
                                                                season)),
                                       proportion = n / sum(n)) %>%
                                ggplot(aes(x = character,
                                           y = proportion,
                                           fill = sentiment)) +
                                geom_bar(position = 'stack',
                                         stat = 'identity') +
                                theme_minimal() +
                                facet_grid(~season) +
                                scale_color_brewer(palette = 'Paired') +
                                labs(fill = '') +
                                theme(axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      axis.text.x = element_text(angle = 90,
                                                                 vjust = 0.5,
                                                                 hjust = 1))
                })
        
        # Generating plot5
        output$plot5 <-
                renderPlot({
                        sentiment_dat %>%
                                group_by(character,
                                         season) %>%
                                summarise(value = sum(value)) %>%
                                mutate(season = as.factor(paste('Season',
                                                                season))) %>%
                                filter(character %in% c(input$characters)) %>%
                                ggplot(aes(x = character,
                                           y = value,
                                           fill = character)) +
                                geom_bar(stat = 'identity') +
                                theme_minimal() +
                                facet_grid(~season) +
                                scale_fill_brewer(palette = 'Paired') +
                                labs(color = '') +
                                theme(legend.position = 'none',
                                      axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      axis.text.x = element_text(angle = 90,
                                                                 vjust = 0.5,
                                                                 hjust = 1))
                })
        
        # Generating plot6
        output$plot6 <-
                renderPlot({
                        sentiment_dat %>%
                                group_by(character,
                                         season,
                                         episode) %>%
                                summarise(value = sum(value)) %>%
                                mutate(season = as.factor(paste('Season',
                                                                season))) %>%
                                filter(character %in% c(input$characters)) %>%
                                ggplot(aes(x = episode,
                                           y = value,
                                           color = character,
                                           group = character)) +
                                geom_point() +
                                geom_line() + 
                                theme_minimal() +
                                facet_grid(~season) +
                                scale_color_brewer(palette = 'Paired') +
                                xlab('Episode') +
                                labs(color = '') +
                                theme(axis.title.y = element_blank())
                })
        
        # Generating table1
        output$table1 <-
                renderTable({
                        get(df_freq()) %>%
                                bind_tf_idf(word,
                                            character,
                                            n) %>%
                                filter(character == input$character_freq) %>%
                                slice_max(order_by = !!sym(input$freq_algo),
                                          n = input$n_words) %>%
                                select(word, n, !!sym(input$freq_algo))
                },
                digits = 6)
}

# Creating Shiny app
shinyApp(ui, server)