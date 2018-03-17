tidy_text_tfidf %>%
    arrange(-tf_idf) %>%
    slice(1:20)

tidy_text_tfidf %>%
    group_by(category) %>%
    top_n(3) %>%
    ggplot(aes(tf, idf)) +
    geom_smooth(se = F,color='lightblue',size=1)+
    geom_point(aes(color = category)) +
    geom_text_repel(
        aes(label = word, color = category),
        show.legend = F,
        size = 3,
        force = 1,
        min.segment.length = 1.5,
        max.iter = 100
    ) +
    # scale_x_continuous(limits = c(0.025,0.175))+
    # scale_y_continuous(limits = c(2,9))+
    labs(title = 'Top 3 words by tf-idf per category', x = 'TF', y = 'IDF') +
    theme_bw()

tidy_text_tfidf %>%
    group_by(category) %>%
    top_n(10) %>%
    ggplot(aes(reorder(word, tf_idf), tf_idf, fill = category)) +
    geom_col(show.legend = F) +
    coord_flip() +
    labs(y = 'TF IDF', x = 'Top words') +
    facet_wrap( ~ category, scales = 'free') -> p

facet_multiple(
    plot = p,
    facets = 'category',
    ncol = 3,
    nrow = 3,
    scales = 'free'
)
