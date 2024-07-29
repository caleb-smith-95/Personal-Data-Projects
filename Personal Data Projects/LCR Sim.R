
games <- 20
players <- 10
die <- c("H", "H", "H", "L", "C", "R")

games_mat <- matrix(0, 4, games)
rownames(games_mat) <- c("Rounds", "Max", "Winner", "Finish")


roll <- function(dollars){
  if(dollars >= 3){
    sample(die, 3, replace = T)
  }else if(dollars > 0){
    sample(die, dollars, replace = T)
  }
}

for(g in 1:games){
  board <- c(rep(3, players))
  rounds <- matrix(board, players, 1)
  n <- 1
  while(sum(rounds[,n] > 0) > 1){
    for(p in 1:players){
      throw <- roll(board[p]) #Each player's roll
      if(length(throw) > 0){
        for(t in 1:length(throw)){ #Iterates over every dice thrown in the roll
          if(throw[t] == "C"){
            board[p] <- board[p] - 1
          }
          else if(throw[t] == "R"){
            if(p == players){
              board[players] <- board[players] - 1
              board[1] <- board[1] + 1
            }
            else{
              board[p] <- board[p] - 1
              board[p+1] <- board[p+1] + 1
            }
          }
          else if (throw[t] == "L"){
            if(p == 1){
              board[1] <- board[1] - 1
              board[players] <- board[players] + 1
            }
            else{
              board[p] <- board[p] - 1
              board[p-1] <- board[p-1] + 1
            }
          }
        }
      }
    }
    rounds <- cbind(rounds, board)
    n <- n+1
  }
  games_mat[1,g] <- ncol(rounds)
  games_mat[2,g] <- max(rounds)
  games_mat[4,g] <- sum(rounds[,ncol(rounds)])
  if(games_mat[4,g] > 0){
    for(p2 in 1:players){
      if(rounds[p2,ncol(rounds)] > 0){
        games_mat[3,g] <- p2
      }
    }
  }
  else{
    for(p2 in 1:players){
      if(rounds[p2, n-1] > 0){
        games_mat[3,g] <- p2
        games_mat[4,g] <- rounds[p2, n-1]
      }
    }
  }
}

winners_mat <- matrix(0, players, 1)
for(w in 1:players){
  winners_mat[w] <- sum(games_mat[3,] == w)
}

summary_mat <- matrix(0, 10, 1)
rownames(summary_mat) <- c("Mean Rounds", "Median Rounds", "Mode Rounds", "Max Rounds", "Min Rounds", "Max Max", "Average Max", "Most Wins", "Least Wins", "Max Finish")
summary_mat[1] <- mean(games_mat[1,])
summary_mat[2] <- median(games_mat[1,])
summary_mat[4] <- max(games_mat[1,])
summary_mat[5] <- min(games_mat[1,])
summary_mat[6] <- max(games_mat[2,])
