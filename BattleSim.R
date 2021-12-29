# You may or may not need these packages in order to run this function
# install.packages("remotes")
# remotes::install_github("felixmil/rollr")

# Values present represent defaults which can be modified as needed. Only value required is Groups.
BattleSim <- function(Groups, #Vector consisting of the names of the conflicting groups
                      nPeople = c(7,7),  #Vector consisting of the number of people in each group, respectively
                      ATK = c(0,0), #Vector consisting of the attack bonus for each group, respectively
                      DMG = c("1d8", "1d8"), #Vector consisting of the damage die for each group, respectively. Should be specified as a string and any bonus noted added with spaces on both sides of the + sign (i.e., "1d8 + 2")
                      HPmax = c(30, 30), #Vector consisting of the HPmax for each group, respectively. It is assumed each group starts the battle at full health.
                      AC = c(12, 12), #Vector consisting of the AC bonus of each group, respectively.
                      PlayByPlay = FALSE) #Logical Command that will include output of what happens on each turn.  
{
# Dependent packages    
  pacman::p_load(rollr)

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
df$ATK <- c(rep(ATK[1], nPeople[1]),
            rep(ATK[2], nPeople[2]))
# Just a consistent value, we can likely pipe this into the rollR function as a command 
df$DMG <- c(rep(DMG[1], nPeople[1]),
            rep(DMG[2], nPeople[2]))
# Once again, something I would hope to make part of the function later
df$HPmax <- c(rep(HPmax[1], nPeople[1]),
              rep(HPmax[2], nPeople[2]))
# Once again, something I would hope to make part of the function later
df$HP <- df$HPmax
# Once again, something I would hope to make part of the function later
df$AC <- c(rep(AC[1], nPeople[1]),
           rep(AC[2], nPeople[2]))
# We are now simulating initiative in a very barebones, sloppy fashion, basically, just sampling without replacement a number of times equivalent to the number of individuals we have. 
df$Initiative <- sample(rows,
                       size = length(rows),
                       replace = FALSE)
# Will track whether a character is still in battle or not
df$LifeStatus <- "Active"

# Simulating the battle round by round with a for loop
ATK <- NA
DMG <- NA
Summary <- NA
Round <- 0
# During each round of battle (however many it may take)...
  while (sum(df$LifeStatus == "Inactive" & df$Group == Groups[1]) != nPeople[1] &
         sum(df$LifeStatus == "Inactive" & df$Group == Groups[2]) != nPeople[2]){
    Round <- Round + 1
# ... For each initiative position ....
    for (j in sort(unique(df$Initiative))){
# ... For each battle participant ....
      for (k in rows){
# ... skip over participants that don't have priority initiative.
          if (df$Initiative[k] == j){
# ... and check whether they are currently alive. 
            if (df$LifeStatus[k] == "Active"){
# if there opposing party is dead, cease this turn.
              if ((sum(df$LifeStatus == "Inactive" & df$Group == Groups[1]) == nPeople[1] |
                    sum(df$LifeStatus == "Inactive" & df$Group == Groups[2]) == nPeople[2])){
                break}
# if there opposing party is alive, continue this turn.
              if ((sum(df$LifeStatus == "Inactive" & df$Group == Groups[1]) != nPeople[1] |
                 sum(df$LifeStatus == "Inactive" & df$Group == Groups[2]) != nPeople[2])){
# If someone has not engaged, randomly choose someone to attack.            
                if (is.na(df$EngagedWith[k])){
                  df$EngagedWith[k] <- sample(df$Person[(df$Group != df$Group[k]) & df$LifeStatus == "Active"], 1)
                  Action <- paste0(df$Person[k], " has engaged with ", df$EngagedWith[k], ".")
                  Summary <- c(Summary, Action)
                }        
# If the person they were previously engaged with died, randomly choose someone else to attack.            
                if (!is.na(df$EngagedWith[k]) & df$LifeStatus[df$EngagedWith[k]] == "Inactive"){
                  df$EngagedWith[k] <- sample(df$Person[(df$Group != df$Group[k]) & df$LifeStatus == "Active"], 1)
                  Action <- paste0(df$Person[k], " has engaged with ", df$EngagedWith[k], ".")
                  Summary <- c(Summary, Action)
                }
# If someone is engaged with someone, let them try to attack..            
                if (!is.na(df$EngagedWith[k]) & df$LifeStatus[df$EngagedWith[k]] == "Active"){
                  ATK <- roll_dice("1d20")
    # ... if they roll a Natural 1, they'll inflict damage on themselves
                  if (ATK == 1){
                    DMG <- roll_dice(df$DMG[k])
                    df$HP[df$Person[k]] <- df$HP[df$Person[k]] - DMG
                    Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but hurt themselves in the process and took ", DMG, " points of damage.")
                    Summary <- c(Summary, Action)
    # ... and if their HP drops below 0 as a result, they will die.                
                    if (df$HP[df$Person[k]] <= 0){
                      df$HP[df$Person[k]] <- 0
                      df$LifeStatus[df$Person[k]] <- "Inactive"
                      df$RoundKilled[df$Person[k]] <- Round
                      Action <- paste0(df$Person[k], " has accidentally committed suicide!")
                      Summary <- c(Summary, Action)
                    }
                  }
    # ... if they roll a Natural 20, they'll inflict critical damage
                  if (ATK == 20){
                    DMG <- (roll_dice(df$DMG[k]) * 2)
                    df$HP[df$EngagedWith[k]] <- df$HP[df$EngagedWith[k]] - DMG
                    Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k]," and done critical damage, at ", DMG, " points total.")
                    Summary <- c(Summary, Action)
    # ... and if their target's HP drops below 0 as a result, they will die.  
                  if (df$HP[df$EngagedWith[k]] <= 0){
                      df$HP[df$EngagedWith[k]] <- 0
                      df$LifeStatus[df$EngagedWith[k]] <- "Inactive"
                      df$RoundKilled[df$EngagedWith[k]] <- Round
                      Action <- paste0(df$EngagedWith[k], " has been annhiliated!")
                      Summary <- c(Summary, Action)
                    }
                  }
    # ... and if their attack roll is somewhere in the middle...
                  if (ATK > 1 & ATK < 20){
    # ... add their attack modifier
                    ATK <- ATK + df$ATK[k]
    # ... if the attack roll is greater than their target's AC, roll damage.
                    if (ATK > df$AC[df$EngagedWith[k]]){
                      DMG <- roll_dice(df$DMG[k])
                      df$HP[df$EngagedWith[k]] <- df$HP[df$EngagedWith[k]] - DMG
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k]," and done ", DMG, " points of damage.")
                      Summary <- c(Summary, Action)
    # ... and if their target's HP drops below 0 as a result, they will die. 
                    if (df$HP[df$EngagedWith[k]] <= 0){
                        df$HP[df$EngagedWith[k]] <- 0
                        df$LifeStatus[df$EngagedWith[k]] <- "Inactive"
                        df$RoundKilled[df$EngagedWith[k]] <- Round
                        Action <- paste0(df$EngagedWith[k], " has been subdued!")
                        Summary <- c(Summary, Action)
                      }
                    }
    # ... if the attack roll is less than or equal to their target's AC, the attack misses.
                    if (ATK < df$AC[df$EngagedWith[k]]){
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but missed.")
                      Summary <- c(Summary, Action)
                    }
                    if (ATK == df$AC[df$EngagedWith[k]]){
                      Action <- paste0(df$Person[k], " has attacked ", df$EngagedWith[k],", but ", df$EngagedWith[k], " just barely escaped without damage.")
                      Summary <- c(Summary, Action)
                    }              
                  }
                }
              }
            }
          }
        }
      }
    }
# Removing the NA value we needed to start the Summary variable
Summary <- Summary[-1]
# Displaying the Play By Play if it was requested
if (PlayByPlay == TRUE){
  print(Summary)
}
# Displaying the final summary of the battle.
for (i in 1:length(Groups)){
  if (sum(df$LifeStatus == "Inactive" & df$Group == Groups[i]) == nPeople[i]){ 
    (Action <-print(paste("Battle ended! The", Groups[i], "have fallen. After", Round, "rounds,", 
                          sum(df$LifeStatus == "Active" & df$Group == Groups[length(Groups)+1-i]), Groups[length(Groups)+1-i], "remain!", sep= " ")))
    Summary <- c(Summary, Action)
  }
}
# Returning the final summary dataframe. 
return(df)
}

# Example
df <- BattleSim(Groups = c("Good", "Bad"),  #Vector consisting of the names of the conflicting groups
                nPeople = c(7,18),  #Vector consisting of the number of people in each group, respectively
                ATK = c(9,5), #Vector consisting of the attack bonus for each group, respectively
                DMG = c("1d12 + 2", "1d8"), #Vector consisting of the damage die for each group, respectively. Should be specified as a string and any bonus noted added with spaces on both sides of the + sign (i.e., "1d8 + 2")
                HPmax = c(45, 30), #Vector consisting of the HPmax for each group, respectively. It is assumed each group starts the battle at full health.
                AC = c(18, 12), #Vector consisting of the AC bonus of each group, respectively.
                PlayByPlay = FALSE) #Logical Command that will include output of what happens on each turn.
