library('ProjectTemplate')
load.project()


m <- quanteda::as.DocumentTermMatrix(x = doc_feature_matrix) %>% as.TermDocumentMatrix() %>% as.matrix
dim(m)
tsne_out <- Rtsne::Rtsne(m,dims=2)
plot(tsne_out$Y)
dist_m <- dist(m)
tsne_out_dist <- Rtsne::Rtsne(dist_m,dims=2)
plot(tsne_out_dist$Y)

# m <- as.matrix(tdm)
# m
# v <- sort(rowSums(m), decreasing = T)
# v
# d <- data.frame(word = names(v), freq = v)
# head(d, 10)
# wordcloud2(d[1:300, ], color = 'random-dark', fontFamily = 'arial')

# findFreqTerms(tdm, 300)
# findAssocs(tdm,findFreqTerms(tdm, 300),0.3)
# findAssocs(dtm,'feet',0.3)
#

cache('tsne_out_dist')
