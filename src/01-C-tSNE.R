m <-
    quanteda::as.DocumentTermMatrix(x = doc_feature_matrix) %>% as.TermDocumentMatrix() %>% as.matrix
dim(m)
tsne_out <- Rtsne::Rtsne(m, dims = 2)
plot(tsne_out$Y)

dist_m <- dist(m)
tsne_out_dist <- Rtsne::Rtsne(dist_m, dims = 2)
plot(tsne_out_dist$Y)

cache('tsne_out_dist')

tsne_plot <- tbl_df(tsne_out_dist$Y) %>%
    mutate(words = doc_feature_matrix@Dimnames$features)

tidy_text_tfidf %>% filter(tf_idf > 0) %>% group_by(word) %>% top_n(1000, tf_idf) -> top_1e3_tfidf

top_1e3_tfidf %>%
    ungroup() %>%
    left_join(tsne_plot, by = c('word' = 'words')) %>%
    top_n(n = 3e3, wt = tf_idf) %>%
    ggplot(aes(V1, V2, word=word)) +
    geom_point(size = 1, aes(color=log10(tf_idf))) +
    theme_bw() -> p

plotly::ggplotly(p)
# # geom_label(aes(label=words),size=2.5,label.size = 0)+
# geom_text_repel(
#     aes(label = words),
#     show.legend = F,
#     size = 2.5,
#     force = 0.5,
#     min.segment.length = 0.5,
#     max.iter = 100
# )
