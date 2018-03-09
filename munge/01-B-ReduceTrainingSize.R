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
        mutate(in_atleast_one_col = sum(cat_b, cat_f, cat_e, cat_l, cat_s)) %>%
        ungroup() %>%
        filter(in_atleast_one_col != -5) %>%
        select(-in_atleast_one_col) %>%
        mutate(
            cat_b = ifelse(cat_b == -1, 0, 1),
            cat_f = ifelse(cat_f == -1, 0, 1),
            cat_e = ifelse(cat_e == -1, 0, 1),
            cat_l = ifelse(cat_l == -1, 0, 1),
            cat_s = ifelse(cat_s == -1, 0, 1)
        )

    training_labels %<>%
        reshape2::melt(id.vars = 'doc_id') %>%
        filter(value == 1) %>%
        group_by(variable) %>%
        sample_n(3000) %>%
        reshape2::dcast(doc_id ~ variable, fill = 0) %>%
        tbl_df

    training_labels %>% colSums()

    training_data <-
        training_data[training_data$doc_id %in% training_labels$doc_id, ]

    attr(training_data, 'state') <- 'reduced'
    attr(training_labels, 'state') <- 'reduced'

    cache('training_data')
    cache('training_labels')
}
