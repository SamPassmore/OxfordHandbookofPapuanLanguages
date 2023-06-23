## Decision trees for identifying Papuan Kinship systems
library(rpart)
library(rpart.plot)
library(caret)

# Load the data
kinbank_structures = read.csv('processed_data/sibling_vectors.csv')

# add in Papuan variable
papuan_languages = read.csv('processed_data/papuan_languages.csv')
kinbank_structures$papuan = ifelse(kinbank_structures$X %in% papuan_languages$ID, 1, 0)

# training and test 
train.index = createDataPartition(kinbank_structures$papuan, p = .8, list = FALSE)
train = kinbank_structures[ train.index,]
test  = kinbank_structures[-train.index,]

## Run decision tree
fit = rpart(papuan~., data = train[,-1], method = 'class')
rpart.plot(fit)

predict_unseen = predict(fit, test, type = 'class')
table(test$papuan, predict_unseen)
sum(test$papuan == 1 & predict_unseen == 1)


## ratios

n_lang = 100
n_structures = 7
r = n_lang/n_structures

1000 * r



