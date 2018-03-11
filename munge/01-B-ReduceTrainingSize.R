if(config$reduce_dataset) {
    set.seed(1)

    # categories - b s f l e are the top 5
    # (colSums(training_labels) + 21519) %>%
    #     tail(-1) %>%
    #     as.data.frame() %>%
    #     rownames_to_column() %>%
    #     mutate(rowname =
    #                reorder(rowname, .)) %>%
    #     ggplot(aes(rowname, .)) +
    #     geom_col() +
    #     coord_flip() +
    #     labs(x = 'dsid category', y = 'count of dsid', caption = 'All Training Data')

    # Drop all other categories
    training_labels %<>%
        select(doc_id, cat_b, cat_f, cat_e, cat_l, cat_s) %>%
        rowwise() %>%
        mutate(in_exactly_one_col = sum(cat_b, cat_f, cat_e, cat_l, cat_s)) %>%
        ungroup() %>%
        filter(in_exactly_one_col == -3) %>%
        select(-in_exactly_one_col) %>%
        mutate(
            cat_b = ifelse(cat_b == -1, F, T),
            cat_f = ifelse(cat_f == -1, F, T),
            cat_e = ifelse(cat_e == -1, F, T),
            cat_l = ifelse(cat_l == -1, F, T),
            cat_s = ifelse(cat_s == -1, F, T)
        )

    training_labels %<>%
        reshape2::melt(id.vars = 'doc_id') %>%
        filter(value) %>%
        group_by(variable) %>%
        sample_n(config$sample_size) %>%
        reshape2::dcast(doc_id ~ variable, fill = F) %>%
        tbl_df

    training_labels %>% colSums()

    training_data <-
        training_data[training_data$doc_id %in% training_labels$doc_id, ]

    attr(training_data, 'state') <- 'reduced'
    attr(training_labels, 'state') <- 'reduced'

    cache('training_data')
    cache('training_labels')
}
