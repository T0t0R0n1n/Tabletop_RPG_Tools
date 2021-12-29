# Values present represent defaults which can be modified as needed. Only value required is Groups.
FightersSim <- function(Groups, #Vector consisting of the names of the conflicting groups
                        nPeople = c(7,7),  #Vector consisting of the number of people in each group, respectively
                        Uniform = TRUE, #A logical value which specifies whether all members of a given group should have uniform stats (i.e., AC, HP, etc.). If false, stats will be randomly generated within reasonable parameters 
                        ATK = NA, #Vector consisting of the attack bonus for each group, respectively
                        DMG = NA, #Vector consisting of the damage die for each group, respectively. Should be specified as a string and any bonus noted added with spaces on both sides of the + sign (i.e., "1d8 + 2")
                        HPmax = NA, #Vector consisting of the HPmax for each group, respectively. It is assumed each group starts the battle at full health.
                        HP = NA, #Vector consisting of the HP for each group, respectively. If unspecified, HP will take the value of HPmax at the start of the battle. 
                        AC = NA, #Vector consisting of the AC bonus of each group, respectively.
                        DEX = NA) #Vector specifing the dexterity scores (Range: 1-20) of each group, respectively.
{ # Creating a dataframe to house the data on all of the individual relevant to the battle
  rows <- 1:sum(nPeople)
  cols <- c("Person", "Group", "ATK","DMG", "HPmax", "HP", "AC", "DEX", "Initiative", "EngagedWith", "LifeStatus", "RoundKilled")
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
  if (Uniform == TRUE){
    #If attack bonus values are provided...
    if (!is.na(ATK)){
      df$ATK <- c(rep(ATK[1], nPeople[1]),
                  rep(ATK[2], nPeople[2]))
    }
    #If attack bonus values must be simualted...
    if (is.na(ATK)){
      df$ATK <- c(rep(sample(0:9,
                             size = 1,
                             replace = TRUE,
                             prob = seq(10,1,-1)), 
                      nPeople[1]),
                  rep(sample(0:9,
                             size = 1,
                             replace = TRUE,
                             prob = seq(10,1,-1)), 
                      nPeople[2]))
    }
    #If damage specifications are are provided...
    if (!is.na(DMG)){
      df$DMG <- c(rep(DMG[1], nPeople[1]),
                  rep(DMG[2], nPeople[2]))
    }
    #If damage values must be simualted...
    if (is.na(DMG)){
      DMGdice <- c("1d4", "1d4 + 1", "1d4 + 2", "1d4 + 3", "1d4 + 4",
                   "2d4", "2d4 + 1", "2d4 + 2", "2d4 + 3", "2d4 + 4",
                   "1d6", "1d6 + 1", "1d6 + 2", "1d6 + 3", "1d6 + 4",
                   "2d6", "2d6 + 1", "2d6 + 2", "2d6 + 3", "2d6 + 4",
                   "1d8", "1d8 + 1", "1d8 + 2", "1d8 + 3", "1d8 + 4",
                   "2d8", "2d8 + 1", "2d8 + 2", "2d8 + 3", "2d8 + 4",
                   "1d10", "1d10 + 1", "1d10 + 2", "1d10 + 3", "1d10 + 4",
                   "2d10", "2d10 + 1", "2d10 + 2", "2d10 + 3", "2d10 + 4")
      DMGprob <- c(40, 32, 24, 16, 8, 
                   36, 28, 20, 12, 4,
                   39, 31, 23, 15, 7,
                   35, 27, 19, 11, 3,
                   38, 30, 22, 14, 6,
                   34, 26, 18, 10, 2,
                   37, 29, 21, 13, 5,
                   33, 25, 17, 9, 1)
      df$DMG <- c(rep(sample(DMGdice,
                             size = 1,
                             replace = TRUE,
                             prob = DMGprob), 
                      nPeople[1]),
                  rep(sample(DMGdice,
                             size = 1,
                             replace = TRUE,
                             prob = DMGprob), 
                      nPeople[2]))
    }
    #If HPmax values are provided ....
    if (!is.na(HPmax)){
      df$HPmax <- c(rep(HPmax[1], nPeople[1]),
                  rep(HPmax[2], nPeople[2]))
    }
    #If HPmax values must be simualted ...
    if (is.na(HPmax)){
      df$HPmax <- c(rep(sample(seq(20,70,1),
                               size = 1,
                               replace = TRUE,
                               prob = seq(70,20,-1)), 
                        nPeople[1]),
                    rep(sample(seq(20,70,1),
                               size = 1,
                               replace = TRUE,
                               prob = seq(70,20,-1)), 
                        nPeople[2]))
    }
    #Making HP equivalent to HPmax
    df$HP <- df$HPmax
    #If AC is specified ...
    if (!is.na(AC)){
      df$AC <- c(rep(AC[1], nPeople[1]),
                 rep(AC[2], nPeople[2]))
    }
    #If AC needs to be simulated ...
    if (is.na(AC)){
      df$AC <- c(rep(sample(seq(10,21,1),
                            size = 1,
                            replace = TRUE,
                            prob = seq(21,10,-1)), 
                     nPeople[1]),
                 rep(sample(seq(10,21,1),
                            size = 1,
                            replace = TRUE,
                            prob = seq(21,10,-1)), 
                     nPeople[2]))
    }
  }
  if (Uniform == FALSE){
    #If attack bonus values must be simualted...
    if (is.na(ATK)){
      df$ATK <- c(sample(0:9,
                         size = nPeople[1],
                         replace = TRUE,
                         prob = seq(10,1,-1)),
                  sample(0:9,
                         size = nPeople[2],
                         replace = TRUE,
                         prob = seq(10,1,-1)))
    }
    #If damage values must be simualted...
    if (is.na(DMG)){
      DMGdice <- c("1d4", "1d4 + 1", "1d4 + 2", "1d4 + 3", "1d4 + 4",
                   "2d4", "2d4 + 1", "2d4 + 2", "2d4 + 3", "2d4 + 4",
                   "1d6", "1d6 + 1", "1d6 + 2", "1d6 + 3", "1d6 + 4",
                   "2d6", "2d6 + 1", "2d6 + 2", "2d6 + 3", "2d6 + 4",
                   "1d8", "1d8 + 1", "1d8 + 2", "1d8 + 3", "1d8 + 4",
                   "2d8", "2d8 + 1", "2d8 + 2", "2d8 + 3", "2d8 + 4",
                   "1d10", "1d10 + 1", "1d10 + 2", "1d10 + 3", "1d10 + 4",
                   "2d10", "2d10 + 1", "2d10 + 2", "2d10 + 3", "2d10 + 4")
      DMGprob <- c(40, 32, 24, 16, 8, 
                   36, 28, 20, 12, 4,
                   39, 31, 23, 15, 7,
                   35, 27, 19, 11, 3,
                   38, 30, 22, 14, 6,
                   34, 26, 18, 10, 2,
                   37, 29, 21, 13, 5,
                   33, 25, 17, 9, 1)
      df$DMG <- c(sample(DMGdice,
                         size = nPeople[1],
                         replace = TRUE,
                         prob = DMGprob),
                  sample(DMGdice,
                         size = nPeople[2],
                         replace = TRUE,
                         prob = DMGprob))
    }
    #If HPmax values must be simualted ...
    if (is.na(HPmax)){
      df$HPmax <- c(sample(seq(20,70,1),
                               size = nPeople[1],
                               replace = TRUE,
                               prob = seq(70,20,-1)), 
                    sample(seq(20,70,1),
                               size = nPeople[2],
                               replace = TRUE,
                               prob = seq(70,20,-1)))
    }
    #Making HP equivalent to HPmax
    df$HP <- df$HPmax
    #If AC needs to be simulated ...
    if (is.na(AC)){
      df$AC <- c(sample(seq(10,21,1),
                        size = nPeople[1],
                        replace = TRUE,
                        prob = seq(21,10,-1)),
                 sample(seq(10,21,1),
                        size = nPeople[2],
                        replace = TRUE,
                        prob = seq(21,10,-1)))
    }
  }
  # Will track whether a character is still in battle or not
  df$LifeStatus <- "Active"
  # We are now simulating initiative in a very barebones, sloppy fashion, basically, just sampling without replacement a number of times equivalent to the number of individuals we have. 
  df$Initiative <- sample(rows,
                          size = length(rows),
                          replace = FALSE)
  # Returning the final summary dataframe. 
  return(df)
}

# Example
df <- FightersSim(Groups = c("Good", "Bad"),  #Vector consisting of the names of the conflicting groups
                  nPeople = c(7,18),  #Vector consisting of the number of people in each group, respectively
                  Uniform = FALSE)
