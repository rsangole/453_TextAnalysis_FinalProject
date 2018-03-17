vs <- VectorSource(training_data_uniques$text)

docs <- Corpus(vs)
docs
meta(docs, 'doc_id') <- training_data_uniques$doc_id
inspect(docs[10])

stopwords_w_spaces <- stopwords('english') %>%
    gsub(pattern = '\'', replacement = ' ')
taxiway_designations <- unlist(c(letters,
                                 map(letters,  ~ paste0(.x, letters))))
remove_fullstop <-
    function(x) {
        gsub(pattern = '\\.',
             replacement = ' ',
             x = x)
    }

docs %<>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(remove_fullstop)) %>%
    tm_map(removePunctuation) %>%
    # tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords_w_spaces) %>%
    tm_map(removeWords, taxiway_designations) %>%
    tm_map(removeWords, airport_iata_codes[1:7000]) %>%
    tm_map(removeWords, airport_iata_codes[7000:7800]) %>%
    tm_map(stripWhitespace)
docs
inspect(docs[10])

tdm <- TermDocumentMatrix(docs)
inspect(tdm)

category_labels <- training_labels_uniques %>% melt(id.vars='doc_id', variable.name= 'category') %>% filter(value) %>% select(-value) %>% as_tibble() %>% arrange(doc_id)

tidy_text <-  tibble(doc_id = meta(docs)[[1]],
                     txt = map_chr(.x = 1:length(docs),  ~ docs[.x]$content)) %>%
    left_join(category_labels) %>%
    unnest_tokens(word, txt)
tidy_text

tidy_text %>% count(category, word, sort = T)

tidy_text_tfidf <- tidy_text %>%
    count(category, word, sort = T) %>%
    bind_tf_idf(term = word, document = category, n = n)
tidy_text_tfidf %>% arrange(-tf_idf)

# tidy_test_tfidf %<>%
#     inner_join(training_labels)
# tidy_test_tfidf
#
# to_plot <- tidy_test_tfidf %>%
#     select(-n) %>%
#     melt(
#         id.vars = c('doc_id', 'word', 'tf', 'idf', 'tf_idf'),
#         variable.name = 'category',
#         value.name = 'value'
#     ) %>%
#     filter(value) %>%
#     arrange(-tf_idf) %>%
#     group_by(category) %>%
#     slice(1:20) %>%
#     ungroup()
#
# to_plot %>%
#     ggplot(aes(tf, idf)) +
#     geom_point(color = 'black') +
#     geom_text_repel(
#         aes(label = word, color = category),
#         size = 3.5,
#         force = 1,
#         min.segment.length = 1.5,
#         max.iter = 200
#     ) +
#     # scale_x_continuous(limits = c(0.025,0.175))+
#     scale_y_continuous(limits = c(2,9))+
#     theme_bw()
#
# to_plot %>%
#     group_by(category) %>%
#     arrange(-tf_idf) %>%
#     slice(1:20) %>%
#     ggplot(aes(reorder(word, tf_idf), tf_idf)) +
#     geom_bar(aes(fill = category), stat = 'identity') +
#     coord_flip() +
#     labs(y='TF IDF',x='Top words per category')+
#     facet_grid(category~.,scales = 'free')
#
# tidy_test_tfidf %>%
#     select(-n) %>%
#     melt(
#         id.vars = c('doc_id', 'word', 'tf', 'idf', 'tf_idf'),
#         variable.name = 'category',
#         value.name = 'value'
#     ) %>%
#     filter(value) %>%
#     arrange(-tf_idf) %>%
#     group_by(category) %>%
#     slice(1:30) %>% pull(word) -> top_words
# top_words
#

# smaller_table <-
#     tbl_df(t(as.matrix(tdm))) %>% select(one_of(top_words))
# smaller_table
#
# docid_to_category_mapping <-
#     training_labels %>%
#     melt('doc_id') %>%
#     filter(value) %>%
#     arrange(doc_id) %>%
#     tbl_df %>%
#     select(-value)
#
# smaller_table %<>% bind_cols(docid_to_category_mapping)
# smaller_distinct_table <-
#     smaller_table[!duplicated(smaller_table[, 1:90]),]
# docs_kept <- smaller_distinct_table %>% select(doc_id, variable)
# smaller_distinct_table %<>% select(-doc_id, -variable)
# rtsne_out <-
#     Rtsne(
#         as.matrix(smaller_distinct_table),
#         dims = 2,
#         theta = .2,
#         verbose = T,
#         max_iter = 2000
#     )
# rtsne_out <- tbl_df(rtsne_out$Y) %>% bind_cols(docs_kept)
# ggplot(rtsne_out, aes(x = V1, y = V2)) +
#     geom_point(aes(color = variable, pch = variable), size = 3) +
#     theme_bw()
# ggplot(rtsne_out, aes(x = V1, y = V3)) +
#     geom_point(aes(color = variable, pch = variable)) +
#     theme_bw()
