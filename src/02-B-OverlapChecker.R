overlap_check_df <- training_labels %>% select_if(is.logical) %>%
    map_df(~.x*training_labels$doc_id)

overlap_check_df

intersect(overlap_check_df$cat_a,overlap_check_df$cat_b) %>% length

names(overlap_check_df)

overlap_df <- matrix(data = runif(n = 22*22),nrow = 22,dimnames = list(names(overlap_check_df),names(overlap_check_df)))

get_overlap <- function(x,y){
    length(intersect(x,y))-1
}

for (i in 1:length(names(overlap_check_df))) {
    for(j in 1:length(names(overlap_check_df))){
        overlap_df[i,j] <- get_overlap(overlap_check_df[[i]], overlap_check_df[[j]])
    }
}

overlap_df
plot(overlap_df)

tbl_df(overlap_df) %>%
    mutate(X = as.factor(names(overlap_check_df))) %>%
    melt(id.vars='X') %>% tbl_df() %>%
    ggplot(aes(x = X, y=variable))+
    geom_tile(aes(fill=value))+
    labs(x='',y='')+
    theme(axis.text.x = element_text(angle = 90))
