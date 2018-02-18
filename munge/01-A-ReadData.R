training_data <- read_delim(file = 'data/TrainingData.txt',
                            delim = '~',
                            col_names = c('DISD_id','text'))

training_labels <- read_csv(file = 'data/TrainCategoryMatrix.csv',
                            col_names = paste0('Cat',LETTERS[1:22]),
                            col_types = paste0(rep('i',22),collapse = ''))

dim(training_data)
dim(training_labels)

attr(training_data,'state') <- 'raw'
attr(training_labels,'state') <- 'raw'

cache('training_data')
cache('training_labels')