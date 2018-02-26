con <- dbConnect(
    dbDriver("PostgreSQL"),
    dbname = "postgres",
    host = "localhost",
    port = 5433,
    user = "postgres",
    password = rstudioapi::askForPassword('Database password')
)

# sql_command <-
#     'SELECT * FROM aviation_safety_training limit 20'
#
# query_result <- dbGetQuery(con, sql_command) %>%
#     as_tibble()
# query_result
#

aviation_db <- tbl(con,'aviation_safety_training')

# aviation_df <- aviation_db %>% head(20)

text <- aviation_db %>%
    select(text) %>%
    pull()

docs <- Corpus(VectorSource(text))
docs
inspect(docs[1])

docs %<>%
    tm_map(content_transformer(tolower))
inspect(docs[1])

docs %<>%
    tm_map(removeWords,stopwords('english'))
inspect(docs[1])

docs %<>%
    tm_map(removePunctuation)
inspect(docs[1])

docs %<>%
    tm_map(stripWhitespace)
inspect(docs[1])

# install.packages('SnowballC')
library('SnowballC')

docs %<>%
    tm_map(stemDocument)
inspect(docs[1])

dtm <- TermDocumentMatrix(docs)
#
# inspect(dtm)
# m <- as.matrix(dtm)
# m
# v <- sort(rowSums(m),decreasing = T)
# v
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)
#
# wordcloud2(d[1:300,],color = 'random-dark',fontFamily = 'arial')
#
# findFreqTerms(dtm, 10000)
#
# findAssocs(dtm,findFreqTerms(dtm, 20000),0.3)
#
# findAssocs(dtm,'feet',0.3)
#





tidy_text %<>% mutate(lower = word == tolower(word))
tidy_text %>% filter(lower == F)
tidy_text %>% count(lower)
tidy_text <- training_data %>%
    unnest_tokens(word, text, to_lower = F)
tidy_text %>% filter(lower == F) %>%
    count(word, sort = T) %>%
    filter(n > 7000) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    coord_flip()
tidy_text %>%
    filter(lower == F) %>%
    mutate(word = str_to_lower(word)) %>%
    anti_join(stop_words) %>%
    count(word, sort = T)
