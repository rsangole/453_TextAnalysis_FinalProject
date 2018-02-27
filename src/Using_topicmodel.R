library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
#
# data("AssociatedPress")
# AssociatedPress
#
# ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
# ap_lda
#
#
# ap_topics <- tidy(ap_lda, matrix = "beta")
# ap_topics
#
#
# ap_top_terms <- ap_topics %>%
#     group_by(topic) %>%
#     top_n(10, beta) %>%
#     ungroup() %>%
#     arrange(topic, -beta)
#
# ap_top_terms %>%
#     mutate(term = reorder(term, beta)) %>%
#     ggplot(aes(term, beta, fill = factor(topic))) +
#     geom_col(show.legend = FALSE) +
#     facet_wrap(~ topic, scales = "free") +
#     coord_flip()
#
#
# beta_spread <- ap_topics %>%
#     mutate(topic = paste0("topic", topic)) %>%
#     spread(topic, beta) %>%
#     filter(topic1 > .001 | topic2 > .001) %>%
#     mutate(log_ratio = log2(topic2 / topic1))
#
# beta_spread
#
# ap_documents <- tidy(ap_lda, matrix = "gamma")
# ap_documents
#
# tidy(AssociatedPress) %>%
#     filter(document == 6) %>%
#     arrange(desc(count))

# Aviation Data -----------------------------------------------------------
training_labels %>%
    mutate(SumCols = 22+rowSums(.)) %>%
    histogram(~SumCols,data = .,breaks=0:20,scales = list(x=list(at=0:20)))
# Perhaps 6 groups

dtm <- DocumentTermMatrix(docs)
aviation_lda <- LDA(dtm, k=2, control=list(seed=1234))
aviation_lda

aviation_topics <- tidy(aviation_lda, matrix = "beta")
aviation_topics

densityplot(~beta,groups=topic,aviation_topics,plot.points=F,ylim = c(-1,1000))

aviation_top_terms <- aviation_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

aviation_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

beta_spread <- aviation_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > 0.006 | topic2 > 0.006) %>%
    mutate(log_ratio_21 = log2(topic2 / topic1))

ggplot(beta_spread,aes(reorder(term,log_ratio_21),log_ratio_21))+
    geom_col()+
    coord_flip()

aviation_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > 0.01 | topic2 > 0.01 | topic3 > 0.01
           | topic4 > 0.01 | topic5 > 0.01 | topic6 > 0.01) -> beta_spread_2

for (i in 1:6) {
    result <- list()
    k=1
    for (j in 1:6) {
        if(i==j) next
        temp <- beta_spread_2 %>%
            select(term,
                   contains(as.character(i)),
                   contains(as.character(j)))
        temp['log_ratio'] <- log2(temp[2]/temp[3])
        temp %<>%
            mutate(term=reorder(term,log_ratio))
        p <- ggplot(temp,aes(term,log_ratio))+
            geom_col(show.legend = F)+
            coord_flip()+
            labs(title=paste0('log_ratio between ',
                             j,' and ', i))
        # ggsave(filename = paste0('graphs/log_ratio',i,j,'.png'),
               # device = 'png',width = 5, height = 5)
        result[[k]] <- p
        k=k+1
    }
    p <- grid.arrange(result[[1]],result[[2]],result[[3]],result[[4]],result[[5]],nrow=1)
    ggsave(filename = paste0('graphs/log_ratio_',i,'.png'), plot = p,
           device = 'png',width = 15, height = 4)
}