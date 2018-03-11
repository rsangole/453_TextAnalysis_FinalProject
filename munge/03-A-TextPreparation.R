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

tidy_test_tfidf %<>%
    inner_join(training_labels)

tidy_test_tfidf

to_plot <- tidy_test_tfidf %>%
    select(-n) %>%
    melt(
        id.vars = c('doc_id', 'word', 'tf', 'idf', 'tf_idf'),
        variable.name = 'category',
        value.name = 'value'
    ) %>%
    filter(value) %>%
    arrange(-tf_idf) %>%
    group_by(category) %>%
    slice(1:10)

to_plot %>%
    ggplot(aes(tf, idf)) +
    geom_point(color = 'red') +
    geom_text_repel(
        aes(label = word, color = category),
        size = 3.5,
        force = 1,
        min.segment.length = .5,
        max.iter = 200
    ) +
    theme_bw() +
    scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.05)) +
    scale_y_continuous(limits = c(4, 7), breaks = seq(4, 7, .5))

to_plot %>%
    arrange(-tf_idf) %>%
    group_by(category) %>%
    slice(1:10) %>%
    ggplot(aes(reorder(word, tf_idf), tf_idf)) +
    geom_bar(aes(fill = category), stat = 'identity') +
    coord_flip() +
    facet_wrap( ~ category, scales = 'free', nrow = 3)

str(dtm)


tidy_test_tfidf %>%
    select(-n) %>%
    melt(
        id.vars = c('doc_id', 'word', 'tf', 'idf', 'tf_idf'),
        variable.name = 'category',
        value.name = 'value'
    ) %>%
    filter(value) %>%
    arrange(-tf_idf) %>%
    group_by(category) %>%
    slice(1:30) %>% pull(word) -> top_words
top_words

tbl_df(t(as.matrix(dtm))) %>% select(one_of(top_words)) -> smaller_table
smaller_table

# tsne.mat <- as.matrix(dtm) %>% t
tsne.mat <- as.matrix(smaller_table)
result <- tsne::tsne(tsne.mat,
                     k = 3,
                     max_iter = 500,
                     epoch = 10,
                     )
head(result)

docid_to_category_mapping <-
    training_labels %>% melt('doc_id') %>% filter(value) %>% arrange(doc_id) %>% tbl_df %>% select(-value)

tsne_result <- tbl_df(result) %>%
    bind_cols(docid_to_category_mapping)
tsne_result

ggplot(tsne_result, aes(x = V1, y = V2)) +
    geom_point(aes(color = variable))
ggplot(tsne_result, aes(x = V2, y = V3)) +
    geom_point(aes(color = variable))

smaller_table %<>% bind_cols(docid_to_category_mapping)
smaller_distinct_table <- smaller_table[!duplicated(smaller_table[,1:147]),]
docs_kept <- smaller_distinct_table %>% select(doc_id,variable)
smaller_distinct_table %<>% select(-doc_id,-variable)
rtsne_out <- Rtsne(as.matrix(smaller_distinct_table),dims = 3,theta = .2,verbose = T,max_iter = 2000)
rtsne_out <- tbl_df(rtsne_out$Y) %>% bind_cols(docs_kept)
ggplot(rtsne_out, aes(x = V1, y = V2)) +
    geom_point(aes(color = variable,pch=variable))
ggplot(rtsne_out, aes(x = V1, y = V3)) +
    geom_point(aes(color = variable,pch=variable))
