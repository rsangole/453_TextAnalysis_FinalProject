vs <- VectorSource(training_data$text)

docs <- Corpus(vs)
docs
meta(docs, 'doc_id') <- training_data$doc_id
inspect(docs[10])

stopwords_w_spaces <- stopwords('english') %>%
    gsub(pattern = '\'', replacement = ' ')
taxiway_designations <- unlist(c(letters,
                                 map(letters,  ~ paste0(.x, letters))))

docs %<>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords_w_spaces) %>%
    tm_map(removeWords, taxiway_designations) %>%
    tm_map(stripWhitespace)

docs
inspect(docs[10])

dtm <- TermDocumentMatrix(docs)
inspect(dtm)
#
# m <- as.matrix(dtm)
# m
# v <- sort(rowSums(m),decreasing = T)
# v
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)

# wordcloud2(d[1:300,],color = 'random-dark',fontFamily = 'arial')

# findFreqTerms(dtm, 10000)

# findAssocs(dtm,findFreqTerms(dtm, 20000),0.3)
#
# findAssocs(dtm,'feet',0.3)

tidy_test_tfidf = tibble(doc_id = meta(docs)[[1]],
                       txt = map_chr(.x = 1:length(docs),  ~ docs[.x]$content))
tidy_test_tfidf %<>%
    unnest_tokens(word, txt) %>%
    group_by(doc_id) %>%
    count(word) %>%
    ungroup() %>%
    bind_tf_idf(term = word, document = doc_id, n = n)
tidy_test_tfidf

tidy_test_tfidf %>%
    inner_join(training_labels)

unique_category_dsids <-
    training_labels$doc_id[(training_labels %>% select(-doc_id) %>% rowSums()) ==
                               1]

unique_category_dsid_mapping <-
    training_labels %>%
    filter(doc_id %in% unique_category_dsids) %>%
    melt(id.vars = 'doc_id',
         variable.name = 'category',
         value.name = 'value') %>%
    filter(value == 1) %>%
    select(-value)

to_plot <- tidy_test_tfidf %>%
    filter(doc_id %in% unique_category_dsids) %>%
    inner_join(unique_category_dsid_mapping) %>%
    arrange(-tf_idf) %>%
    group_by(category) %>%
    slice(1:4)

to_plot %>%
    ggplot(aes(tf, idf)) +
    geom_point(color = 'red') +
    geom_text_repel(
        aes(label = word, color = category),
        size = 4,
        force = 7,
        min.segment.length = .5,
        max.iter = 100
    ) +
    theme_bw() +
    scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.05))+
    scale_y_continuous(limits = c(3,11), breaks = seq(3,11,1))

tidy_test_tfidf %>%
    filter(doc_id %in% unique_category_dsids) %>%
    inner_join(unique_category_dsid_mapping) %>%
    arrange(-tf_idf) %>%
    group_by(category) %>%
    slice(1:10) %>%
    ggplot(aes(reorder(word,tf_idf),tf_idf)) +
    geom_bar(aes(fill=category),stat = 'identity') +
    coord_flip()+
    facet_wrap(~category,scales = 'free',nrow = 3)
