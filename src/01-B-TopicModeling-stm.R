doc_feature_matrix <- tidy_text %>%
    count(category, word, sort = T) %>%
    cast_dfm(category, word, n)

doc_feature_matrix

topic_model_tsnesearch <- stm(documents = doc_feature_matrix, K = 0, init.type = 'Spectral')
summary(topic_model_tsnesearch)
plot(topic_model_tsnesearch)

topic_model <- stm(documents = doc_feature_matrix, K = 0, init.type = 'Spectral')
summary(topic_model)
plot(topic_model)


# Which words contribute to each topic?
tidy_beta <- tidy(topic_model)
tidy_beta

tidy_beta %>%
    mutate(topic = as.factor(topic)) %>%
    group_by(topic) %>%
    top_n(10) %>%
    ungroup() %>%
    ggplot(aes(reorder(term, beta), beta, fill = topic)) +
    geom_col(show.legend = F) +
    coord_flip() +
    facet_wrap( ~ topic, scales = 'free') +
    labs(
        x = NULL,
        y = expression(beta),
        title = "Highest word probabilities for each topic",
        subtitle = "Different words are associated with different topics"
    )

# How much of the topic contributes to the word?
tidy_gamma <- tidy(topic_model,
                   matrix = 'gamma',
                   document_names = rownames(doc_feature_matrix))
tidy_gamma %>%
    ggplot(aes(gamma, fill = as.factor(topic))) +
    geom_histogram() +
    facet_wrap( ~ document) +
    labs(title = "Distribution of document probabilities for each topic",
         y = "Number of categories",
         x = expression(gamma))
