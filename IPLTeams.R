library(twitteR)
library(rjson)
library(httr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(plyr)
library(bit64)


setup_twitter_oauth("kdl0GFdhw3QxrYymejuDiA", "mQxilm03S73pHizbwhfjJEdEL5ZoiU1ozrzxa1Cwu5Y", "217330049-i21KBaPP6snH7a7KZeo9WZQaRx1kU3t4xQykMZkZ","Cm6aeFgRF05U4uGYutcUPk6HSUceg4vSBfSEEI")

### Load the Hu & Liu’s opinion lexicon of positive and negative words

pos.words <- scan('HuLiuOpinionLexicon\\positive-words.txt', what='character', comment.char=';')
neg.words <- scan('HuLiuOpinionLexicon\\negative-words.txt', what='character', comment.char=';')

#Jeff Green sentiment function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        sentence = tolower(sentence)
        word.list = str_split(sentence, '\\s+')
        words = unlist(word.list)
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        score = sum(pos.matches) - sum(neg.matches)
        return(score)
    }, pos.words, neg.words, .progress=.progress )
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

csk.tweets <- searchTwitter('#csk', n=1000, lang="en")
kkr.tweets <- searchTwitter('#kkr', n=1000, lang="en")
kxip.tweets <- searchTwitter('#kxip', n=1000, lang="en")
rr.tweets <- searchTwitter('#rr', n=1000, lang="en")
srh.tweets <- searchTwitter('#srh', n=1000, lang="en")
mi.tweets <- searchTwitter('#mi', n=1000, lang="en")
dd.tweets <- searchTwitter('#dd', n=1000, lang="en")
rcb.tweets <- searchTwitter('#rcb', n=1000, lang="en")

# csk.tweets.df <- do.call("rbind", lapply(csk.tweets, as.data.frame))

csk.text = laply(csk.tweets, function(t) t$getText())
kkr.text = laply(kkr.tweets, function(t) t$getText())
kxip.text = laply(kxip.tweets, function(t) t$getText())
rr.text = laply(rr.tweets, function(t) t$getText())
srh.text = laply(srh.tweets, function(t) t$getText())
mi.text = laply(mi.tweets, function(t) t$getText())
dd.text = laply(dd.tweets, function(t) t$getText())
rcb.text = laply(rcb.tweets, function(t) t$getText())

csk.text = gsub("[^[:alnum:]|^[:space:]]", "", csk.text)
kkr.text = gsub("[^[:alnum:]|^[:space:]]", "", kkr.text)
kxip.text = gsub("[^[:alnum:]|^[:space:]]", "", kxip.text)
rr.text = gsub("[^[:alnum:]|^[:space:]]", "", rr.text)
srh.text = gsub("[^[:alnum:]|^[:space:]]", "", srh.text)
mi.text = gsub("[^[:alnum:]|^[:space:]]", "", mi.text)
dd.text = gsub("[^[:alnum:]|^[:space:]]", "", dd.text)
rcb.text = gsub("[^[:alnum:]|^[:space:]]", "", rcb.text)

csk.scores <- score.sentiment(csk.text, pos.words, 
                                   neg.words, .progress='text')
kkr.scores <- score.sentiment(kkr.text, pos.words, 
                                   neg.words, .progress='text')
kxip.scores <- score.sentiment(kxip.text, pos.words, 
                                   neg.words, .progress='text')
rr.scores <- score.sentiment(rr.text, pos.words, 
                                   neg.words, .progress='text')
srh.scores <- score.sentiment(srh.text, pos.words, 
                                   neg.words, .progress='text')
mi.scores <- score.sentiment(mi.text, pos.words, 
                                   neg.words, .progress='text')
dd.scores <- score.sentiment(dd.text, pos.words, 
                                   neg.words, .progress='text')
rcb.scores <- score.sentiment(rcb.text, pos.words, 
                                   neg.words, .progress='text')


csk.scores$team = '1 Chennai SuperKings'
csk.scores$code = 'CSK'
kkr.scores$team = '2 Kolkata KnightRiders'
kkr.scores$code = 'KKR'
kxip.scores$team = '8 Kings XI Punjab'
kxip.scores$code = 'KXIP'
rr.scores$team = '3 Rajasthan Royals'
rr.scores$code = 'RR'
srh.scores$team = '5 SunRisers Hyderabad'
srh.scores$code = 'SRH'
mi.scores$team = '6 Mumbai Indians'
mi.scores$code = 'MI'
dd.scores$team = '7 Delhi DareDevils'
dd.scores$code = 'DD'
rcb.scores$team = '4 Royal Challengers Bangalore'
rcb.scores$code = 'RCB'

ipl.scores = rbind(csk.scores, kkr.scores,  rr.scores, rcb.scores, srh.scores, mi.scores, dd.scores, kxip.scores)

ggplot(data=ipl.scores) +
    geom_bar(mapping=aes(x=score, fill=team), binwidth=1) + 
    facet_grid(team~.) +
    theme_bw() + scale_color_brewer() +
    labs(title="IPL Teams Sentiment")


ggplot(ipl.scores, aes(x=team, y=score, group=team)) +
    geom_boxplot(aes(fill=team)) +
    geom_jitter(color="gray40",
                position=position_jitter(width=0.2), alpha=0.3) +
    ggtitle("IPL Teams Sentiment Scores")

