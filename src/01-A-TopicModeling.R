dtm <- DocumentTermMatrix(docs)
aviation_lda <- LDA(dtm, k=3, control=list(seed=1211))
aviation_lda
aviation_topics <- tidy(aviation_lda, matrix = "beta")
aviation_topics

# densityplot(~beta,groups=topic,aviation_topics,plot.points=F,xlim = c(0,0.001),auto.key = T)

av_top_terms <- aviation_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

av_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free",nrow = 3) +
    coord_flip()

beta_sprd <- aviation_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > 0.006 | topic2 > 0.006) %>%
    mutate(log_ratio_21 = log2(topic2 / topic1))

ggplot(beta_sprd,aes(reorder(term,log_ratio_21),log_ratio_21))+
    geom_col()+
    coord_flip()

beta_sprd <- aviation_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > 0.006 | topic3 > 0.006) %>%
    mutate(log_ratio_31 = log2(topic3 / topic1))

ggplot(beta_sprd,aes(reorder(term,log_ratio_31),log_ratio_31))+
    geom_col()+
    coord_flip()

# aviation_topics %>%
#     mutate(topic = paste0("topic", topic)) %>%
#     spread(topic, beta) %>%
#     filter(topic1 > 0.01 | topic2 > 0.01 | topic3 > 0.01
#            | topic4 > 0.01 | topic5 > 0.01 | topic6 > 0.01) -> beta_sprd_2
# for (i in 1:6) {
#     result <- list()
#     k=1
#     for (j in 1:6) {
#         if(i==j) next
#         temp <- beta_sprd_2 %>%
#             select(term,
#                    contains(as.character(i)),
#                    contains(as.character(j)))
#         temp['log_ratio'] <- log2(temp[2]/temp[3])
#         temp %<>%
#             mutate(term=reorder(term,log_ratio))
#         p <- ggplot(temp,aes(term,log_ratio))+
#             geom_col(show.legend = F)+
#             coord_flip()+
#             labs(title=paste0('log_ratio between ',
#                               j,' and ', i))
#         # ggsave(filename = paste0('graphs/log_ratio',i,j,'.png'),
#         # device = 'png',width = 5, height = 5)
#         result[[k]] <- p
#         k=k+1
#     }
#     p <- grid.arrange(result[[1]],result[[2]],result[[3]],result[[4]],result[[5]],nrow=1)
#     ggsave(filename = paste0('graphs/log_ratio_',i,'.png'), plot = p,
#            device = 'png',width = 15, height = 4)
# }

cl <- kmeans(dtm,centers = 3,nstart = 10)
kmeans_results <- tibble(orig = docid_to_category_mapping$variable,
                         result = cl$cluster)
kmeans_results %>%
    ggplot(aes(orig,result,color=orig))+
    geom_jitter()