options(stringsAsFactors=FALSE)

trim <- function(words) {gsub("^\\s+|\\s+$", "", words)}

getID <- function(wrd){
  hold <- dict[which(dict$Words==wrd),"ID"]
  return(hold)
}

getWord <- function(ids){
  hold <- dict[which(dict$ID==ids),"Words"]
  return(hold)
}

word_proc <- function(user_in){
  words <- tolower(user_in)
  words <- trim(words)
  
  print(words)
  
  len_input <- length(strsplit(words, " ")[[1]])
  
  print(len_input)
  
  if(len_input >= 3){
  words <- unlist(strsplit(words, split=" ", 
                           fixed=TRUE))[(len_input - 2):(len_input)]}
  else{words <- unlist(strsplit(words, split=" ", 
                                fixed=TRUE))[(len_input - 1):(len_input)]}
  print(words)  
  
  len_input <- length(words)

  for(i in 1:len_input){
    if(words[i]%in%dict$Words){words[i] <- getID(words[i])}
    else{words[i] <- NA}
  }  
    return(words)
}

total_freq <- function(words){
  
  len_input <- length(words)
  
  if(len_input >= 3){
    words <- unlist(strsplit(words, split=" ", 
                             fixed=TRUE))[(len_input - 2):(len_input)]
    print(words)##
    
    bi_grams_guess <- bi_grams[bi_grams$One == words[3], ]
    
    tri_grams_guess <- tri_grams[tri_grams$One==words[2] 
                              & tri_grams$Two==words[3], ]
    
    four_grams_guess <- four_grams[four_grams$One==words[1] 
                               & four_grams$Two==words[2]
                               & four_grams$Three==words[3], ]
    
    final <- list(bi_grams_guess, tri_grams_guess, four_grams_guess, words)
    return(final)    
  }
  
  if(len_input == 2){
    words <- unlist(strsplit(words, split=" ", 
                             fixed=TRUE))[(len_input - 1):(len_input)]
    
    bi_grams_guess <- bi_grams[bi_grams$One == words[2], ]
    
    tri_grams_guess <- tri_grams[tri_grams$One==words[1] &
                                tri_grams$Two==words[2], ]
    
    four_grams_guess <- four_grams
    four_grams_guess[1:nrow(four_grams_guess),] <- NA
    
    final <- list(bi_grams_guess, tri_grams_guess, four_grams_guess, words)
    return(final)
  }
  
  if(len_input == 1){
    words <- unlist(strsplit(words, split=" ", 
                             fixed=TRUE))[(len_input):(len_input)]
    
    bi_grams_guess <- bi_grams[bi_grams$One == words[1], ]
    
    tri_grams_guess <- tri_grams
    tri_grams_guess[1:nrow(tri_grams_guess),] <- NA
    
    four_grams_guess <- four_grams
    four_grams_guess[1:nrow(four_grams_guess),] <- NA
    
    final <- list(bi_grams_guess, tri_grams_guess, four_grams_guess, words)
    return(final)
  }
  
}

gram_mle <- function(full_grams, ngram){
  
  if(ngram == 0){k <- "Words"; hold <- uni_grams}
  if(ngram == 1){k <- "Two"; hold <- full_grams[[ngram]]}
  if(ngram == 2){k <- "Three"; hold <- full_grams[[ngram]]}
  if(ngram == 3){k <- "Four"; hold <- full_grams[[ngram]]}
  
  if(nrow(hold)==0){numms <- rep(NA, ncol(hold)); hold  <- rbind(hold, as.data.table(numms), fill=TRUE) ; hold$Category <- NA; hold$r <- NA; hold$mle <- NA; mle <- hold[,c("Category", "r", "mle"), with = FALSE]; return(mle)}
  if(is.na(hold$Total[1])){hold$Category <- NA; hold$r <- NA; hold$mle <- NA; mle <- hold[1,c("Category", "r", "mle"), with = FALSE]; return(mle)}
  else{
  hold2 <- aggregate(hold$Total, by = list(Category = hold[[k]]), FUN=sum)
  hold_colsum <- sum(hold$Total)
  mle <- cbind(hold2, mle = hold2$x/hold_colsum)
  names(mle)[2] <- "r"
  return(mle)
  }
  }

good_turing <- function(four_mle){
  
  if(is.na(four_mle$Category[1]) ==T){blah <- four_mle[,2:3, with = FALSE]; colnames(blah) <- c("r", "n"); blah[,"Z"] <- NA; blah[, "log_r"] <- NA; blah[, "log_Z"] <- NA; blah[, "gt"] <- NA; blah[, "lgt"] <- NA;  blah[, "gt_sd"] <- NA; blah[, "p"] <- NA; blah[, "r_star"] <- NA; return(blah)}
  else{
  #convert table of frequencies of species to data frame  
  blah <- as.data.table(table(as.data.frame(four_mle$r)))
  print(head(blah))
  #convert factor column to numeric
  blah$V1 <- as.numeric(as.character(blah$V1))
  print(head(blah))
  #name columns, r is frequency, n is frequency of frequency
  colnames(blah) <- c("r", "n")
  blah$r <- as.integer(as.character(blah$r))
  
  #create empty columns to be filled below
  blah[,"Z"] <- NA
  blah[, "log_r"] <- NA
  blah[, "log_Z"] <- NA
  blah[, "gt"] <- NA #good turing estimate
  blah[, "lgt"] <- NA #linear good turing estimate
  blah[, "gt_sd"] <- NA #good turing standard deviation
  blah[, "p"] <- NA
  blah[, "r_star"] <- NA
  
  #order data frame on r 
  blah<- blah[with(blah, order(r)),]
  #total number of species observed
  N <- sum(blah$r * blah$n)
  #estimate of probability of all species not observed
  p <- blah$n[1]/N
  
  
  #fill in Z column, the average of surrounding r values
  for(j in 1:nrow(blah)){
    if(j == 1){ i <- 0; k <- 2; blah$Z[j] <- blah$n[j]/(0.5 * (k - i)); i<-i+1; k<-k+1}
    if(j == nrow(blah)){k <- 2*k - i; blah$Z[j] <- blah$n[j]/(0.5 * (k - i))}
    if(j != 1 & j!= nrow(blah)) {blah$Z[j] <- blah$n[j]/(0.5 * (k - i)); i<-i+1; k<-k+1}
  }
  
  #fillin log(r) and log(Z) columns
  blah$log_r <- log(blah$r)
  blah$log_Z <- log(blah$Z)
  
  #regression model, log(Z) = a + b * log(r)
  hold <- lm(log(Z)~log(r), blah)
  #extract coefficients
  b<- as.numeric(hold$coefficients[2])
  a<- as.numeric(hold$coefficients[1])
  
  #fill in gt
  for(i in 1:nrow(blah)){
    blah$gt[i] <- (blah$r[i]+1)*(blah$n[i+1]/blah$n[i])
  }
  
  #fill in lgt
  for(i in 1:nrow(blah)){
    blah$lgt[i] <- blah$r[i]*(1+1/blah$r[i])^(b+1)
  }
  
  
  #fill in gt_sd
  for(i in 1:nrow(blah)){
    blah$gt_sd[i] <- (1+blah$r[i])^2*(blah$n[i+1]/blah$n[i]^2)*(1+blah$n[i+1]/blah$n[i])
  }
  
  
  #fill in r_star
  holding<-0
  for(i in 1:nrow(blah)){
    if(is.na(blah$gt_sd[i])==FALSE && holding==0 && abs(blah$gt[i]-blah$lgt[i])>1.65*blah$gt_sd[i] ){blah$r_star[i] <- blah$gt[i]}
    if(holding > 0 || is.na(blah$gt_sd[i])==TRUE || abs(blah$gt[i]-blah$lgt[i])<=1.65*blah$gt_sd[i] ) {blah$r_star[i] <- blah$lgt[i]; holding <- holding +  1}
  }
  
  #number of species observed, adjusted values
  n_prime <- sum(blah$r_star*blah$n)
  
  #probability of seeing species
  blah$p <- (1 - p)*(blah$r_star/n_prime) 
  
  return(blah)
  }
}

gt_combine <- function(grm_ml, gt, gram){
  if(is.na(grm_ml$Category[1])){hold <- NULL; return(hold)}
  else{
  hold <- merge(grm_ml, gt, by = "r")
  hold <- hold[ , c(1:3, 8:12)]
  hold <- hold[ , c(2,1, 3:8)]
  colnames(hold)[2:8] <- paste(colnames(hold)[2:8], gram, sep="_");
  return(hold)
  }
}

all_combine <- function(uni, bi, tri, four){
  if(!is.null(bi)){
  hold <- merge(uni, bi, by = "Category", all = T)}
  if(!is.null(tri)){
  hold <- merge(hold, tri, by = "Category", all = T)}
  if(!is.null(four)){
  hold <- merge(hold, four, by = "Category", all = T)}
  return(hold)
}

katz <- function(gtur){
  k <- 1
  j <- 0
  gtur[, "P_katz"] <- NA
  
  if(is.null(gtur$r_star_four) & !is.null(gtur$r_star_tri)){gtur <- gtur[with(gtur, order(r_star_tri, decreasing = TRUE)),]}
  if(is.null(gtur$r_star_tri)& !is.null(gtur$r_star_bi)){gtur <- gtur[with(gtur, order(r_star_bi, decreasing = TRUE)),]}  

  
  if(!is.null(gtur$r_star_four)){
  gtur <- gtur[with(gtur, order(r_star_four, decreasing = TRUE)),]
  for(i in 1:nrow(gtur)){
    if(is.na(gtur$r_four[i]) == F & gtur$r_four[i] > k){gtur$P_katz[i] <- gtur$r_star_four[i]/gtur$r_four[i] * gtur$r_four[i]/(sum(gtur$r_tri[1:nrow(gtur)], na.rm = TRUE) - gtur$r_tri[i]); j <- i}
    else{b <- 1 - sum(gtur$r_star_four[1:(j+1)]/gtur$r_four[1:(j+1)] * gtur$r_four[1:(j+1)]/gtur$r_tri[1:(j+1)]); a <- b / sum(gtur$r_star_tri[1:i]/gtur$r_tri[1:i] * gtur$r_tri[1:i]/gtur$r_bi[1:i]); gtur <- gtur[with(gtur, order(r_star_tri, decreasing = TRUE)),]; i <-  0; k <- 0; break}
  }
  }
  
  if(!is.null(gtur$r_star_tri)){
  for(i in 1:nrow(gtur)){
    if(is.na(gtur$P_katz[i]) == F) {next}
    if(is.na(gtur$r_tri[i]) == F & gtur$r_tri[i] > k){gtur$P_katz[i] <- gtur$r_star_tri[i]/gtur$r_tri[i] * gtur$r_tri[i]/(sum(gtur$r_bi[1:nrow(gtur)], na.rm = TRUE) - gtur$r_bi[i]); j <- i}
    else{b <- 1 - sum(gtur$r_star_tri[1:(j+1)]/gtur$r_tri[1:(j+1)] * gtur$r_tri[1:(j+1)]/gtur$r_bi[1:(j+1)]); a <- b / sum(gtur$r_star_tri[1:i]/gtur$r_tri[i:1] * gtur$r_tri[1:i]/gtur$r_bi[1:i]); gtur <- gtur[with(gtur, order(r_star_bi, decreasing = TRUE)),]; i <- 0; k <- 0; break}
  }
  }
  
  if(!is.null(gtur$r_star_bi)){
  for(i in 1:nrow(gtur)){
    if(is.na(gtur$P_katz[i]) == F) {next}
    if(is.na(gtur$r_bi[i]) == F & gtur$r_bi[i] > k){gtur$P_katz[i] <- gtur$r_star_bi[i]/gtur$r_bi[i] * gtur$r_bi[i]/(sum(gtur$r_uni[1:nrow(gtur)], na.rm = TRUE) - gtur$r_uni[i])}
    else{b <- 1 - sum(gtur$r_star_bi[1:(j+1)]/gtur$r_bi[1:(j+1)] * gtur$r_bi[1:(j+1)]/gtur$r_bi[1:(j+1)]); a <- b / sum(gtur$r_star_bi[1:i]/gtur$r_bi[1:i] * gtur$r_bi[1:i]/gtur$r_uni[1:i]); gtur <- gtur[with(gtur, order(r_star_uni, decreasing = TRUE)),]; break}
  }
  }
  return(gtur)
}

testing <- function(teh_text){

  #rm(hold, uni_hold, bi_hold, tri_hold, four_hold, gt_uni, gt_bi, gt_tri, gt_four, uni_comb, bi_comb, tri_comb, four_comb, final, guess)
  
  used <- word_proc(teh_text)
  
  hold <- total_freq(used)
  
  uni_hold <- gram_mle(uni_grams, 0)
  bi_hold <- gram_mle(hold, 1)
  tri_hold <- gram_mle(hold, 2)
  four_hold <- gram_mle(hold, 3)
  
print(head(uni_hold))
print(head(bi_hold))
print(head(tri_hold))
print(head(four_hold))
    
  gt_uni <- good_turing(uni_hold)
  gt_bi <- good_turing(bi_hold)
  gt_tri <- good_turing(tri_hold)
  gt_four <- good_turing(four_hold)
  
  uni_comb <- gt_combine(uni_hold, gt_uni, "uni")
  bi_comb <- gt_combine(bi_hold, gt_bi, "bi")
  tri_comb <- gt_combine(tri_hold, gt_tri, "tri")
  four_comb <- gt_combine(four_hold, gt_four, "four")
  
  final <- all_combine(uni_comb, bi_comb, tri_comb, four_comb)
  
  guess <- katz(final)
  
  z <- 0
  
  if(length(guess$r_star_four) != sum(is.na(guess$r_star_four))){z <- 1;
    guess <- guess[with(guess, order(r_star_four, P_katz, decreasing = TRUE)),]
  }
  if(z == 0 & length(guess$r_star_tri) != sum(is.na(guess$r_star_tri))){z <-1;
    guess <- guess[with(guess, order(r_star_tri, P_katz, decreasing = TRUE)),]
  }
  if(z == 0 & length(guess$r_star_bi) != sum(is.na(guess$r_star_bi))){
    guess <- guess[with(guess, order(r_star_bi, P_katz, decreasing = TRUE)),]
  }
  
  return(paste(getWord(guess$Category[1]), " | ", getWord(guess$Category[2]), " | ", getWord(guess$Category[3])))
  

}



shinyServer(function(input, output, session){
  
  x <- eventReactive(input$goButton, {input$text})
  
  output$value <- renderText({
   
    ins <- x()
    a_hold <- testing(ins)
    a_hold  
      })
    
output$metTable <- renderTable(metxt)      
    
  })

