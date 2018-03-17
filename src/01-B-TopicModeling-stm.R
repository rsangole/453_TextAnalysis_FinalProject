doc_feature_matrix <- tidy_text %>%
    count(category, word, sort = T) %>%
    cast_dfm(category, word, n)

doc_feature_matrix

topic_model_tsnesearch <-
    stm(documents = doc_feature_matrix,
        K = 0,
        init.type = 'Spectral')
summary(topic_model_tsnesearch)
plot(topic_model_tsnesearch)

topic_model <-
    stm(documents = doc_feature_matrix,
        K = 10,
        init.type = 'Spectral')
summary(topic_model)
plot(topic_model)

cache('topic_model')

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





# BY doc_id
#


processed <- tibble(doc_id = meta(docs)[[1]],
                     txt = map_chr(.x = 1:length(docs),  ~ docs[.x]$content)) %>%
    textProcessor(documents = .$txt,lowercase = F,removestopwords = F,removenumbers = F,removepunctuation = F,stem = F)
out <- prepDocuments(documents = processed$documents,vocab = processed$vocab,meta = processed$meta)
topic_model2 <-
    stm(documents = out$documents,
        vocab = out$vocab,
        K = 10,
        init.type = 'Spectral')
summary(topic_model2)
plot(topic_model2,n = 4)

shorttexts <- map_chr(.x = training_data_uniques$text,.f = ~strtrim(.x[[1]],300))
findThoughts(topic_model2,texts = training_data_uniques$text,topics = 8,n = 2)$docs[[1]] %>% plotQuote(main='Topic 8',width = 80)
findThoughts(topic_model2,texts = shorttexts,topics = 9,n = 2)$docs[[1]] %>% plotQuote(main='Topic 9', width = 80)
findThoughts(topic_model2,texts = shorttexts,topics = 10,n = 2)$docs[[1]] %>%  plotQuote(main='Topic 10', width = 80)
findThoughts(topic_model2,texts = shorttexts,topics = 7,n = 2)$docs[[1]] %>% plotQuote(main='Topic 7', width = 80)
findThoughts(topic_model2,texts = shorttexts,topics = 1,n = 2)$docs[[1]] %>% plotQuote(main='Topic 1',width = 80)

plot(topic_model2, type = 'perspectives',topics = c(8,7))
plot(topic_model2, type = 'perspectives',topics = c(6,1))
plot(topic_model2, type = 'perspectives',topics = c(9,2))

# Which words contribute to each topic?
tidy_beta2 <- tidy(topic_model2)
tidy_beta2

tidy_beta2 %>%
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
tidy_gamma2 <- tidy(topic_model2,
                   matrix = 'gamma')
tidy_gamma2 %>%
    filter(document<10) %>%
    ggplot(aes(gamma, fill = as.factor(topic))) +
    geom_histogram() +
    facet_wrap( ~ document) +
    labs(title = "Distribution of document probabilities for each topic",
         y = "Number of categories",
         x = expression(gamma))

mod.out.corr <- topicCorr(topic_model2)
plot(mod.out.corr)
