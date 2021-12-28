# BattleSim <- function(nGroups, #Number of individuals to simulate
#                       nPeople, #Number of individuals in each group
#                       nRounds #Number of rounds of battle
# ){ 

# Dependent packages    
  # install.packages("remotes")
  # remotes::install_github("felixmil/rollr")
  pacman::p_load(rollr)
  
  
# Variables
  Groups <- c("Good", "Bad")
  nPeople <- c(7,7)
  nRounds <- 30
    
# Creating a dataframe to house the data on all of the individual relevant to the battle
  rows <- 1:sum(nPeople)
  cols <- c("Person", "Group", "ATK","DMG", "HPmax", "HP", "AC", "Initiative", "EngagedWith", "LifeStatus", "RoundKilled")
  df <- data.frame(matrix(NA, 
                          nrow = length(rows), 
                          ncol = length(cols), 
                          dimnames = list(rows, cols)))

# Populating that dataframe with data
# No names to start, just numbers; sequential list equivalent to the sum of nPeople
df$Person <- rows
# This specification should allow us to still simulate battles with uneven sides
df$Group <- c(rep(Groups[1], nPeople[1]),
              rep(Groups[2], nPeople[2]))
# Just a consistent Bonus
df$ATK <- 7
# Just a consistent value, we can likely pipe this into the rollR function as a command 
df$DMG <- "1d8"
# Once again, something I would hope to make part of the function later
df$HPmax <- 30
# Once again, something I would hope to make part of the function later
df$HP <- df$HPmax
# Once again, something I would hope to make part of the function later
df$AC <- 12
# We are now simulating initiative in a very barebones, sloppy fashion, basically, just sampling without replacement a number of times equivalent to the number of individuals we have. 
df$Initiative <- sample(rows,
                       size = length(rows),
                       replace = FALSE)
# Will track whether a character is still in battle or not
df$LifeStatus <- "Active"


# Simulating the battle round by round with a for loop
# return(
# During each round of battle...
for (i in 1:nRounds){
  ATK <- NA
  DMG <- NA
# Checking On Battle Progress                
  for (l in 1:length(Groups)){
    if (sum(df$LifeStatus == "Inactive" & df$Group == Groups[l]) == nPeople[l]){
      print(paste0("Battle ended! The ", Groups[l], " have fallen."))
      break
    }
  }
# ... For each initiative position ....
    for (j in sort(unique(df$Initiative))){
# ... For each battle participant ....
      for (k in rows){
# ... skip over participants that don't have priority initiative.
        if (df$Initiative[k] == j){
# ... and check whether they are currently alive. 
          if (df$LifeStatus[k] == "Active"){
# If someone has engaged, randomly choose someone to attack.            
            if (is.na(df$EngagedWith[k])){
              df$EngagedWith[k] <- sample(df$Person[(df$Group != df$Group[k]) & df$LifeStatus == "Active"], 1)
              print(paste0(df$Person[k], " has engaged with ", df$EngagedWith[k], "."))
            }
# If the person they were previously engaged with died, randomly choose someone else to attack.            
            if (!is.na(df$EngagedWith[k]) & df$LifeStatus[df$EngagedWith[k]] == "Inactive"){
              df$EngagedWith[k] <- sample(df$Person[(df$Group != df$Group[k]) & df$LifeStatus == "Active"], 1)
              print(paste0(df$Person[k], " has engaged with ", df$EngagedWith[k], "."))
            }
# If someone is engaged with someone, let them try to attack..            
            if (!is.na(df$EngagedWith[k]) & df$LifeStatus[df$EngagedWith[k]] == "Active"){
              ATK <- roll_dice("1d20")
# ... if they roll a Natural 1, they'll inflict damage on themselves
              if (ATK == 1){
                DMG <- roll_dice(df$DMG[k])
                df$HP[df$Person[k]] <- df$HP[df$Person[k]] - DMG
                print(paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but hurt themselves in the process and took ", DMG, " points of damage."))
# ... and if their HP drops below 0 as a result, they will die.                
                if (df$HP[df$Person[k]] <= 0){
                  df$HP[df$Person[k]] <- 0
                  df$LifeStatus[df$Person[k]] <- "Inactive"
                  df$RoundKilled[df$Person[k]] <- i
                  print(paste0(df$Person[k], " has accidentally committed suicide!"))
                }
              }
# ... if they roll a Natural 20, they'll inflict critical damage
              if (ATK == 20){
                DMG <- (roll_dice(df$DMG[k]) * 2)
                df$HP[df$EngagedWith[k]] <- df$HP[df$EngagedWith[k]] - DMG
                print(paste0(df$Person[k], " has attacked ", df$EngagedWith[k]," and done critical damage, at ", DMG, " points total."))
# ... and if their target's HP drops below 0 as a result, they will die.  
              if (df$HP[df$EngagedWith[k]] <= 0){
                  df$HP[df$EngagedWith[k]] <- 0
                  df$LifeStatus[df$EngagedWith[k]] <- "Inactive"
                  df$RoundKilled[df$EngagedWith[k]] <- i
                  print(paste0(df$EngagedWith[k], " has been annhiliated!"))
                }
              }
# ... and if their attack roll is somewhere in the middle.
              if (ATK > 1 & ATK < 20){
# ... add their attack modifier
                ATK <- ATK + df$ATK[k]
              }
# ... if the attack roll is greater than their target's AC, roll damage.
                if (ATK > df$AC[df$EngagedWith[k]]){
                  DMG <- roll_dice(df$DMG[k])
                  df$HP[df$EngagedWith[k]] <- df$HP[df$EngagedWith[k]] - DMG
                  print(paste0(df$Person[k], " has attacked ", df$EngagedWith[k]," and done ", DMG, " points of damage."))
# ... and if their target's HP drops below 0 as a result, they will die. 
                if (df$HP[df$EngagedWith[k]] <= 0){
                    df$HP[df$EngagedWith[k]] <- 0
                    df$LifeStatus[df$EngagedWith[k]] <- "Inactive"
                    df$RoundKilled[df$EngagedWith[k]] <- i
                    print(paste0(df$EngagedWith[k], " has been subdued!"))
                  }
                }
# ... if the attack roll is less than or equal to their target's AC, the attack misses.
                if (ATK < df$AC[df$EngagedWith[k]]){
                  print(paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but missed."))
                }
                if (ATK == df$AC[df$EngagedWith[k]]){
                  print(paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but ", df$EngagedWith[k], " just barely escaped without damage."))
                }              
# Checking On Battle Progress                
                for (l in 1:length(Groups)){
                  if (sum(df$LifeStatus == "Inactive" & df$Group == Groups[l]) == nPeople[l]){
                    print(paste0("Battle ended! The ", Groups[l], " have fallen."))
                    break
                  }
                }  
              }
            }
          }
        }
      }
    }
# )
# }

