library(dplyr)
library(qdap)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(tidyr)

cfda_data = read.csv("Cleaned_Dataset.csv", stringsAsFactors = F)

# Text Mining Analysis on Assistance Listing Objectives

objectives = select(cfda_data, Program.Number, Objectives..050.)

# Most frequent terms

freq_terms(text.var=objectives,top=25,stopwords = Top200Words)
plot(freq_terms(text.var = objectives,top = 25, 
                stopwords = c(Top200Words,'Program','Programs','Provide','Support')))

# Positive and Negative Words
objectives%>%
    group_by(Program.Number)%>%
    unnest_tokens(output = word, input = Objectives..050.)%>%
    inner_join(get_sentiments('bing'))%>%
    group_by(sentiment)%>%
    count()%>%
    ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+guides(fill=F)+
    coord_flip()

# Emotions of objectives

nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',header = F,col.names = c('word','sentiment','num'),sep = '\t'); nrc = nrc[nrc$num!=0,]; nrc$num = NULL

objectives%>%
    group_by(Program.Number)%>%
    unnest_tokens(output = word, input = Objectives..050.)%>%
    inner_join(nrc)%>%
    group_by(sentiment)%>%
    count()%>%
    ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()

# Sentiment of objectives
objectives %>%
    group_by(Program.Number)%>%
    unnest_tokens(output=word,input=Objectives..050.)%>%
    inner_join(get_sentiments('afinn'))%>%
    summarize(Objectives_Sentiment = mean(value))%>%
    ungroup()%>%
    ggplot(aes(x=Objectives_Sentiment,fill=Objectives_Sentiment>0))+
    geom_histogram(binwidth = 0.1)+
    scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
    guides(fill=F)

# Wordcloud of objectives

wordcloudData = 
    objectives %>%
    group_by(Program.Number)%>%
    unnest_tokens(output=word,input=Objectives..050.)%>%
    anti_join(c(stop_words,'Program','Programs','Provide','Support'))%>%
    group_by(word)%>%
    summarize(freq = n())%>%
    arrange(desc(freq))%>%
    ungroup()%>%
    data.frame()

set.seed(1)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

#------------------------------------------------
# Text Mining Analysis on Assistance Listing Applicants and Eligibility

applicants = select(cfda_data, Program.Number, Applicant.Eligibility..081., Beneficiary.Eligibility..082.,
                    Application.Procedures..092., Award.Procedure..093.)
applicants$application_conc <- paste(cfda_data$Applicant.Eligibility..081., cfda_data$Beneficiary.Eligibility..082.,
                                     cfda_data$Application.Procedures..092., cfda_data$Award.Procedure..093., sep="")
applicants = select(applicants, Program.Number, application_conc)

# Most frequent terms

freq_terms(text.var=applicants,top=25,stopwords = Top200Words)
plot(freq_terms(text.var = applicants,top = 25, 
                stopwords = c(Top200Words,'Program','Programs','Application','Applications', 'Applicants',
                              'Review', 'Under', 'Eligible')))

# Positive and Negative Words
applicants%>%
    group_by(Program.Number)%>%
    unnest_tokens(output = word, input = application_conc)%>%
    inner_join(get_sentiments('bing'))%>%
    group_by(sentiment)%>%
    count()%>%
    ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+guides(fill=F)+
    coord_flip()

# Emotions of objectives
applicants%>%
    group_by(Program.Number)%>%
    unnest_tokens(output = word, input = appli)%>%
    inner_join(nrc)%>%
    group_by(sentiment)%>%
    count()%>%
    ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()

# Sentiment of objectives
applicants %>%
    group_by(Program.Number)%>%
    unnest_tokens(output=word,input=application_conc)%>%
    inner_join(get_sentiments('afinn'))%>%
    summarize(Application_Sentiment = mean(value))%>%
    ungroup()%>%
    ggplot(aes(x=Application_Sentiment,fill=Application_Sentiment>0))+
    geom_histogram(binwidth = 0.1)+
    scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
    guides(fill=F)

# Wordcloud of objectives

wordcloudData = 
    applicants %>%
    group_by(Program.Number)%>%
    unnest_tokens(output=word,input=application_conc)%>%
    anti_join(c(stop_words,'Program','Programs','Provide','Support'))%>%
    group_by(word)%>%
    summarize(freq = n())%>%
    arrange(desc(freq))%>%
    ungroup()%>%
    data.frame()

set.seed(1)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))
