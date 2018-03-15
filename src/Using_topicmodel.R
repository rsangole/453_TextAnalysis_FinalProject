library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Aviation Data -----------------------------------------------------------
training_labels %>%
    mutate(SumCols = 22+rowSums(.)) %>%
    histogram(~SumCols,data = .,breaks=0:20,scales = list(x=list(at=0:20)))
# Perhaps 6 groups