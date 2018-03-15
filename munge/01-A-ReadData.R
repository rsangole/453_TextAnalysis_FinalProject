registerDoMC(cores = 4)

training_data <- read_delim(
    file = 'data/TrainingData.txt',
    delim = '~',
    col_names = c('doc_id', 'text')
)

training_labels <- read_csv(
    file = 'data/TrainCategoryMatrix.csv',
    col_names = paste0('cat_', letters[1:22]),
    col_types = paste0(rep('i', 22), collapse = '')
) %>%
    rownames_to_column('doc_id') %>%
    mutate(doc_id = as.integer(doc_id))

dim(training_data)
dim(training_labels)

attr(training_data, 'state') <- 'raw'
attr(training_labels, 'state') <- 'raw'

cache('training_data')
cache('training_labels')

airport_iata_codes <- readr::read_csv('data/airports.csv',
                                      na = '',
                                      trim_ws = T,
                                      locale = locale(encoding = "UTF-8"))%>%
    select(iata_code) %>%
    na.omit %>%
    mutate(iata_code = str_to_lower(iata_code)) %>%
    distinct() %>%
    pull()
airport_iata_codes %<>% sort() %>% tail(-2)
airport_iata_codes
