library(tm)
library(RWeka)
library(RColorBrewer)
library(htmltools)

# *****************************************************************
# *Первый шаг - до распечатки наиболее часто встречающихся биграмм*
# *****************************************************************

# Функция загрузки одной статьи и сцепления всего ее текста в одну строку
Load_Article <- function(filename)
{
    text<-paste0(readLines(filename,warn=FALSE,encoding="UTF-8"),collapse="")
    if(substr(text,1,1)=="\UFEFF")
        return(enc2utf8(substring(enc2native(text),9)))
    return(text)
}

Step1<-function(path)
{
    setwd(path)
    
    # Считываем список файлов TXT в директории (1 файл - 1 статья)
    filelist <- list.files(path = ".", pattern = "*.txt", all.files = FALSE,
                           full.names = FALSE, recursive = FALSE,
                           ignore.case = FALSE, include.dirs = FALSE)

    # Загружаем все статьи в массив (1 элемент - 1 статья)
    articles.all <- sapply(filelist,Load_Article,simplify = "array",USE.NAMES=FALSE)

    # Создаем корпус и строим term-document matrix
    articles.corpus<-Corpus(VectorSource(articles.all),readerControl=list(language="ru"))
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    control<-list(stopwords=FALSE,
                  removePunctuation=TRUE,
                  removeNumbers=TRUE,
                  tokenize = BigramTokenizer)
    articles.tdm<-TermDocumentMatrix(articles.corpus,control)

    # Преобразуем полученный объект к обычной матрице
    # (ряды - биграммы, колонки - статьи)
    articles.matrix<-as.matrix(articles.tdm)

    # Находим суммы по рядам матрицы 
    # (общее количество вхождений каждой биграммы во все статьи)
    bigrams.counts<-rowSums(articles.matrix)

    # Получаем сами биграммы
    bigrams.names<-dimnames(articles.matrix)[[1]]

    # Создаем фрейм данных с двумя колонками 
    # (биграммы и число их вхождений во все статьи)
    bigrams.df<-data.frame(Bigram=bigrams.names,Count=bigrams.counts,
                       row.names=NULL,
                       stringsAsFactors=FALSE)

    # Сортируем фрейм, чтобы биграммы с максимальным числом 
    # вхождений оказались первыми
    bigrams.df.sorted<-bigrams.df[order(-bigrams.df[,2]),]

    # Распечатываем 500 биграмм с максимальным числом вхождений
    print (head(bigrams.df.sorted,500))
    
    return(list(filelist=filelist,
                articles_all=articles.all,
                articles_matrix=articles.matrix,
                bigrams_df=bigrams.df))
}

# *******************************************************************
# *Второй шаг - вплоть до вывода HTML-файла с результатами обработки*
# *******************************************************************
    
# Функция возвращает позиции "важных" слов внутри одного предложения
Count_word_matches<-function(words,checked.words)
{
    return(sort(unique(unlist(sapply(checked.words,
                                     grep,
                                     x=unlist(words),
                                     fixed=TRUE,
                                     USE.NAMES=FALSE)))))
}

# Выставить оценку одного предложения по индексам "важных" слов
Score_sentence<-function(sentence.indexes,distance.threshold)
{
    if(length(sentence.indexes)==0)
        return (0)
    
    # Находим кластеры "важных" слов
    clusters<-list()
    cluster<-c(sentence.indexes[1])
    
    i<-2
    while (i<=length(sentence.indexes))
    {
        if(sentence.indexes[i]-sentence.indexes[i-1]<distance.threshold)
            cluster<-c(cluster,sentence.indexes[i])
        else
        {
            clusters<-append(clusters,list(cluster))
            cluster<-c(sentence.indexes[i])
        }
        i=i+1
    }
    
    # Выставляем оценки кластерам. Максимальная оценка будет оценкой для
    # всего предложения
    cluster.score.max<-0
    for(c in clusters)
    {
        important.words.in.cluster<-length(c)
        total.words.in.cluster<-c[length(c)]-c[1]+1
        score<-1.0*important.words.in.cluster^2/total.words.in.cluster
        
        if (score>cluster.score.max)
            cluster.score.max<-score
    }
    return(cluster.score.max)
}

# Функция возвращает индексы "важных" предложений из массива полученных
# на основе списка "важных" слов и алгоритма Луна.
# tops - количество возвращаемых индексов (ТОП [tops] "важных предложений")
# distance.threshold - максимально оцениваемое расстояние между словами
Luhn_summarize<-function(sentences,important.words,tops=5,distance.threshold=5)
{
    # Приводим предложения от списка к вектору    
    sentences<-unlist(sentences)
    
    # Переводим все предложения в нижний регистр
    sentences.low<-tolower(sentences)
    
    # Разбираем все предложения на слова
    sentences.words.list<-sapply(sentences.low,WordTokenizer,USE.NAMES=FALSE)
    
    # Выясняем позиции "важных" слова во всех предложениях
    important.words.indexes<-sapply(sentences.words.list,Count_word_matches,
                                    checked.words=important.words,USE.NAMES=FALSE)
    
    # Получить оценки всех предложений в статье
    sentenses.scores<-sapply(important.words.indexes,
                             Score_sentence,
                             distance.threshold=distance.threshold)
    
    # Сцепляем номера предложений и оценки в матрицу
    sentenses.scores.matrix<-cbind(c(1:length(sentenses.scores)),sentenses.scores)
    
    # Получаем матрицу, где ряды отсортированы по убыванию оценок предложений
    sentenses.scores.matrix.sorted<-sentenses.scores.matrix[order(sentenses.scores.matrix[,2],
                                                                  decreasing=TRUE),]
    
    # Получаем вектор номеров предложений, отсортированный по возрастанию для
    #сохранения порядка предложений
    interesting.sentenses.numbers<-
        sort(sentenses.scores.matrix.sorted[1:min(tops,nrow(sentenses.scores.matrix.sorted)),1])
    
    return(interesting.sentenses.numbers)
}

Step2<-function(data,bigram.interest.indexes)
{
    bigrams.df=data$bigrams_df
    filelist=data$filelist
    articles.all=data$articles_all
    
    bigram.interest.names=bigrams.df[bigram.interest.indexes,1]

    # Отдельные уникальные слова в интересных биграммах
    bigram.interest.words=unique(unlist(strsplit(bigram.interest.names," ")))

    # Убираем стоп-слова
    stop.words=which(bigram.interest.words %in% stopwords("ru"))
    if(length(stop.words))
        bigram.interest.words=bigram.interest.words[-stop.words]

    # Разделяем каждую статью на предложения
    articles.all.sentences=strsplit(articles.all,"[!.?] ")

    interesting.sentenses.numbers=lapply(articles.all.sentences,
                         Luhn_summarize,
                         important.words=bigram.interest.words,
                         tops=5)

    doc.content<-""

    for (i in 1:length(filelist))
    {
        doc.content<-paste0(doc.content,h2(filelist[i]))
        for (j in 1:length(articles.all.sentences[[i]]))
        {
            if (j %in% interesting.sentenses.numbers[[i]])
                doc.content<-paste0(doc.content,
                                    br(
                                        strong(articles.all.sentences[[i]][j]," ")))
            else
                doc.content<-paste0(doc.content,
                                    br(articles.all.sentences[[i]][j]," "))
        }
    }

    doc<-tags$html(
                    tags$head(
                                tags$title("Отчет по статьям")
                             ),
                    tags$body(
                                h1("Отчет по статьям"),
                                HTML(doc.content)
                             )
                  )

    writeLines(as.character(doc),"result.html")
}

#**********************
#* Основная программа *
#**********************
data=Step1("C:/Work/articles_Luhn/scr")

# Пользовательский код - выбор и занесение номеров "интересных" биграмм в переменную
# возможная корректировка текстовых опечаток в биграммах,
# удаленных дефисов, возможное удаление из рассмотрения статей, в которых не встретилось
# ни одной интересной биграммы, вывод диаграмм по биграммам и т. д.

# "Интересные" биграммы, выбираемые пользователем вручную
bigram.interest.indexes<-c(24276,10482,9439,22118,22112,22117,9450,
                           22101,22119,22398,9442,22087,24271,2212)

articles.matrix.hm<-data$articles_matrix[bigram.interest.indexes,]

null.bigrams.docs<-which(colSums(articles.matrix.hm)==0)
if(length(null.bigrams.docs)>0)
{
    articles.matrix.hm<-articles.matrix.hm[,-null.bigrams.docs]
    data$articles_all<-data$articles_all[-null.bigrams.docs]
    data$filelist<-data$filelist[-null.bigrams.docs]
}

Step2(data,bigram.interest.indexes)

rm(list=ls())