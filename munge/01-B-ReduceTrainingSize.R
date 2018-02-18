if(config$reduce_dataset){
    set.seed(1)
    training_rows <- sample(1:nrow(training_data),
                            size = 50,
                            replace = F)
    training_data %<>% slice(training_rows)
    training_labels %<>% slice(training_rows)
    attr(training_data,'state') <- 'reduced_seed_1'
    attr(training_labels,'state') <- 'reduced_seed_1'

    cache('training_data')
    cache('training_labels')
}
