library(tm)
library(NLP)
library(e1071)
library(wordcloud)
DATA1=mutate()

bind1=Corpus(VectorSource(DATA1$Message))
bind1=tm_map(bind1,content_transformer(tolower))
bind1=tm_map(bind1,content_transformer(removeNumbers))
bind1=tm_map(bind1,removeWords,stopwords())
bind1=tm_map(bind,content_transformer(removePunctuation))
bind1=tm_map(bind,content_transformer(stripWhitespace))


matrid=DocumentTermMatrix(bind1)

ham=subset(bind,DATA1$Category=="ham")
spam=subset(bind,DATA1$Category=="spam")

step1_train=DATA1[1:4169,]$Category
step1_test=DATA1[4169:5572,]$Category


step2_train=matrid[1:4169,]
step2_test=matrid[4169:5572,]

step3_train=bind1[1:4169]
step3_test=bind1[4169:5572]

f_data=findFreqTerms(matrid,5)

step4_train=step2_train[,f_data]
step4_test=step2_test[,f_data]
y_n =function(x)
{y=ifelse(x>0 ,1,0)
y=factor(x,levels = c(0,1),labels = c("yes","no"))
y}

step5_train=apply(step4_train,2,y_n)
step5_test=apply(step4_test,2,y_n)

p=naiveBayes(step5_train,step1_train)
c=predict(p,step5_test)
k=table(step1_test,c)
