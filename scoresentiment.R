score.sentiment <- function(sentences, pos.words, neg.words, .progress = "none") {
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words) {
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
    }, pos.words, neg.words, .progress = .progress )
  return(scores)
}