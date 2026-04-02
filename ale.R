library(mlr)
library(tidyverse)

data(Ozone, package = 'mlbench')

ozonetib<-as_tibble(Ozone)

names(ozonetib)<-c(
    'Month',
    'Date',
    'Day',
    'Ozone',
    'Press_height',
    'Wind',
    'Humid',
    'Temp_Sand',
    'Temp_Monte',
    'Inv_height',
    'Press_grad',
    'Inv_temp',
    'Visib'
)

map_dbl(ozonetib, ~sum(is.na(.)))

ozonetib

a<-mutate_all(ozonetib,as.numeric)

a

ozoneclean<-a %>% filter(is.na(Ozone) == FALSE)

ozoneclean

ozoneuntidy<-gather(ozoneclean,key = 'Variable',value = 'Value', -Ozone)

ozoneuntidy

"
(
    ggplot(ozoneuntidy,aes(Value,Ozone)) + 
    facet_wrap(~Variable, scale = 'free_x') +
    geom_point() + 
    theme_bw() + 
    geom_smooth() + 
    geom_smooth(method = 'lm', col='red')
)
"

ggsave('ozone.png')

#?imputations

imputemethod<-imputeLearner('regr.rpart')

imputemethod

ozoneimp<-impute(as.data.frame(ozoneclean), classes = list(numeric = imputemethod))

ozoneimp$data

ozonetask<-makeRegrTask(data = ozoneimp$data, target = 'Ozone')

lin<-makeLearner('regr.lm')

listFilterMethods()

filtervals<-generateFilterValuesData(ozonetask, method = 'linear.correlation')

filtervals$data

#plotFilterValues(filtervals) + theme_bw()

ggsave('correlation.png')

filterwrapper<-makeFilterWrapper(
    learner = lin, 
    fw.method = 'linear.correlation'
)

filterwrapper
