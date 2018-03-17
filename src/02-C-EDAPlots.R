training_data %>%
    unnest_tokens(words, text) %>%
    count(doc_id) %>%
    ggplot() +
    geom_histogram(aes(n), binwidth = 20, fill = 'purple') +
    theme_bw() +
    labs(x = '# of words per DSI') +
    geom_vline(aes(xintercept = mean(n)), color = 'black', lty = 2)+
    geom_vline(aes(xintercept = median(n)), color = 'black', lty = 1)
# scale_x_continuous(limits = c(0,20))


wordcloud2(tidy_text %>% count(word, sort = T) %>% top_n(100),
           color = 'random-dark',
           fontFamily = 'arial')


training_data %>%
    unnest_tokens(sent, text, token = 'regex', pattern = '[\\.]')%>%
    count(doc_id) %>%
    ggplot() +
    geom_histogram(aes(n), binwidth = 2, fill = 'purple') +
    theme_bw() +
    labs(x = '# of sentences per DSI') +
    geom_vline(aes(xintercept = mean(n)), color = 'black', lty = 2)+
    geom_vline(aes(xintercept = median(n)), color = 'black', lty = 1)
