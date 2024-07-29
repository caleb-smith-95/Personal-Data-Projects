library(tidyverse)

set.seed(1)
iters <- 10000
panels <- 18
players <- 16

stat_df <- data.frame(rep(NA, iters), rep(1, iters))
colnames(stat_df) <- c("First_Winner", "Counts")


for(i in 1:iters){
  game_mat <- matrix(NA, players, panels) #Create game matrix with each player as a row and each set of panels
                                          #as a column
  guinea_pig <- c(rep(NA, panels))  #This vector of iid bernoulli random variables will contain whether each
                                #panel was guessed correctly on the first try. The person who has to
                                #guess is referred to here as the "Guinea Pig". The same player may be
                                #the Guinea Pig multiple times, or may never be the Guinea Pig at all
  lead <- 1
  
  for(p in 1:panels){  
    guinea_pig[p] <- sample(c(0,1), 1) #0 means the Guinea pig died on this panel, 1 means they survived
    if(guinea_pig[p] == 0){ #If the Guinea Pig dies...
      game_mat[lead, p:panels] <- 0 #The player who was the Guinea Pig receives a 0 for this and all future
                                    #sets of panels
      lead <- lead+1  #The new "Guinea Pig" is the next person in line
      if(lead > players){ #If you don't add this we get errors
        break  #New Line
      }else{
        game_mat[lead:players, p] <- 1
      }
      
    }else{  #If the Guinea Pit lives...
      game_mat[lead:players, p] <- 1 #All the players pass this round, and the same Guinea Pig goes again
    }
  }
  
  win <- game_mat[,panels] == 1
  stat_df$First_Winner[i] <- players - sum(win) + 1
  
  if(stat_df$First_Winner[i] > players){
    stat_df$First_Winner[i] <- NA
  }
}

count(stat_df)
max(stat_df, na.rm = T)
min(stat_df, na.rm = T)

freq <- group_by(stat_df, First_Winner)

freq_plot <- ggplot(freq, aes(x = First_Winner)) + geom_histogram(stat = "count", binwidth = 1) + scale_x_continuous(breaks = c(0:players))
freq_plot

cum_freq <- summarize(freq, totals = sum(Counts))
cum_vect <- c(cum_freq$totals[1], rep(NA, nrow(cum_freq)-1))
for(l in 2:nrow(cum_freq)){
  cum_vect[l] <- cum_vect[l-1] + cum_freq$totals[l]
}

cum_freq$Cumulative <- cum_vect
cum_freq <- mutate(cum_freq, Win_Pct = Cumulative/iters)

ggplot(cum_freq, aes(x = First_Winner, y = Win_Pct)) + geom_point() + scale_x_continuous(breaks = c(0:players)) + xlab("Player")

#Everything above this works
if(is.na(cum_freq[nrow(cum_freq)])){
  ex_survivors <- sum(cum_freq$Cumulative[1:(nrow(cum_freq)-1)])/iters
}else{
  ex_survivors <- sum(cum_freq$Cumulative[1:nrow(cum_freq)])/iters
}

ex_survivors
