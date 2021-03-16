library(dplyr)
library(tidyr)

cbb <- read.csv("C:/Users/Bradley/Documents/STAT 451/March Madness/CBBscores.csv")


points <- rbind(
    data.frame(points=cbb$HomeTeamScore,
               team=cbb$HomeTeam,
               opponent=cbb$VisitingTeam),
    data.frame(points=cbb$VisitingTeamScore,
               team=cbb$VisitingTeam,
               opponent=cbb$HomeTeam))

poisson_model <- glm(points ~  team + opponent, family=poisson(link=log),data=points)
summary(poisson_model)


simulate_match <- function(foot_model, homeTeam, awayTeam,max_points=300){
  home_points_avg <- predict(foot_model,
                            data.frame(team=homeTeam, 
                                       opponent=awayTeam), type="response")
  away_points_avg <- predict(foot_model, 
                            data.frame(team=awayTeam, 
                                       opponent=homeTeam), type="response")
  dpois(0:max_points,home_points_avg) %o% dpois(0:max_points,away_points_avg) 
}

First_Four_Home <- c("NorfolkState", "WichitaState", "SaintMarysCA", "MichiganState")
First_Four_Away <- c("AppalachianState", "Drake", "TexasSouthern", "UCLA")

#FIRST 4----------------------------------------------------------------------------------------------
i <- 1
while (i < 5) {
  probs <- simulate_match(poisson_model, First_Four_Home[i], First_Four_Away[i], max_points=300)
  home_score <- predict(poisson_model, 
          data.frame(team=First_Four_Home[i], 
                     opponent=First_Four_Away[i]), type="response")
  away_score <- predict(poisson_model, 
                        data.frame(team=First_Four_Away[i], 
                                   opponent=First_Four_Home[i]), type="response")
  #home team win
  win_prob_home <- sum(probs[lower.tri(probs)])
  result <- paste(First_Four_Home[i], "has a", win_prob_home * 100,"% chance of winning with a score of",home_score,"(",First_Four_Home[i],
                  ")","to",away_score,"(",First_Four_Away[i],")")
  print(result)
  i <- i + 1
}

Round_of_64_Home <- c("Gonzaga", "Oklahoma", "Creighton", "Virginia", "USC", "Kansas", "Oregon", "Iowa", "Michigan", "LSU", "Colorado", 
                      "FloridaState", "BYU", "Texas", "Connecticut", "Alabama", "Baylor", "NorthCarolina", "Villanova", "Purdue", "TexasTech", 
                      "Arkansas", "Florida", "OhioState", "Illinois", "LoyolaChicago", "Tennessee", "OklahomaState", 
                      "SanDiegoState", "WestVirginia", "Clemson", "Houston")

Round_of_64_Away <- c("NorfolkState", "Missouri", "CalSantaBarbara", "Ohio", "WichitaState", "EasternWashington", "VCU", "GrandCanyon", "MountStMarys", 
                      "St.Bonaventure", "Georgetown", "NCGreensboro", "UCLA", "AbileneChristian", "Maryland", "Iona", "Hartford", "Wisconsin", 
                      "Winthrop","NorthTexas", "UtahState", "Colgate", "VirginiaTech", "OralRoberts", "Drexel", "GeorgiaTech", "OregonState", "Liberty", 
                      "Syracuse", "MoreheadState", "Rutgers", "ClevelandState")


#ROUND OF 64------------------------------------------------------------------------------------------
i <- 1
while (i < 33) {
  probs <- simulate_match(poisson_model, Round_of_64_Home[i], Round_of_64_Away[i], max_points=300)
  home_score <- predict(poisson_model, 
                        data.frame(team=Round_of_64_Home[i], 
                                   opponent=Round_of_64_Away[i]), type="response")
  away_score <- predict(poisson_model, 
                        data.frame(team=Round_of_64_Away[i], 
                                   opponent=Round_of_64_Home[i]), type="response")
  #home team win
  win_prob_home <- sum(probs[lower.tri(probs)])
  result <- paste(Round_of_64_Home[i], "has a", win_prob_home * 100,"% chance of winning with a score of",home_score,"(",Round_of_64_Home[i],
                  ")","to",away_score,"(",Round_of_64_Away[i],")")
  print(result)
  i <- i + 1
}

Round_of_32_Home <- c("Gonzaga", "Creighton", "USC", "Oregon", "Michigan", "Colorado", "BYU", "Maryland", "Baylor", "Villanova", "TexasTech", 
                      "Florida", "Illinois", "Tennessee", "SanDiegoState", "Rutgers")

Round_of_32_Away <- c("Oklahoma", "Virginia", "Kansas", "Iowa", "St.Bonaventure", "FloridaState", "Texas", "Alabama", "Wisconsin", "Purdue",
                      "Colgate", "OhioState", "LoyolaChicago", "OklahomaState", "WestVirginia", "Houston")

#ROUND OF 32-----------------------------------------------------------------------------------------
i <- 1
while (i < 17) {
  probs <- simulate_match(poisson_model, Round_of_32_Home[i], Round_of_32_Away[i], max_points=300)
  home_score <- predict(poisson_model, 
                        data.frame(team=Round_of_32_Home[i], 
                                   opponent=Round_of_32_Away[i]), type="response")
  away_score <- predict(poisson_model, 
                        data.frame(team=Round_of_32_Away[i], 
                                   opponent=Round_of_32_Home[i]), type="response")
  #home team win
  win_prob_home <- sum(probs[lower.tri(probs)])
  result <- paste(Round_of_32_Home[i], "has a", win_prob_home * 100,"% chance of winning with a score of",home_score,"(",Round_of_32_Home[i],
                  ")","to",away_score,"(",Round_of_32_Away[i],")")
  print(result)
  i <- i + 1
}

test <- simulate_match(poisson_model, "Houston", "Rutgers", max_points=300)
sum(test[lower.tri(test)])

Sweet_16_Home <- c("Gonzaga", "USC", "Michigan", "Texas", "Baylor", "Colgate", "Illinois", "WestVirginia")

Sweet_16_Away <- c("Virginia", "Iowa", "FloridaState", "Alabama", "Villanova", "OhioState", "Tennessee", "Houston")

#SWEET 16--------------------------------------------------------------------------------------------
i <- 1
while (i < 9) {
  probs <- simulate_match(poisson_model, Sweet_16_Home[i], Sweet_16_Away[i], max_points=300)
  home_score <- predict(poisson_model, 
                        data.frame(team=Sweet_16_Home[i], 
                                   opponent=Sweet_16_Away[i]), type="response")
  away_score <- predict(poisson_model, 
                        data.frame(team=Sweet_16_Away[i], 
                                   opponent=Sweet_16_Home[i]), type="response")
  #home team win
  win_prob_home <- sum(probs[lower.tri(probs)])
  result <- paste(Sweet_16_Home[i], "has a", win_prob_home * 100,"% chance of winning with a score of",home_score,"(",Sweet_16_Home[i],
                  ")","to",away_score,"(",Sweet_16_Away[i],")")
  print(result)
  i <- i + 1
}

test <- simulate_match(poisson_model, "Houston", "WestVirginia", max_points=300)
sum(test[lower.tri(test)])

Elite_8_Home <- c("Gonzaga", "Michigan", "Baylor", "Illinois")
Elite_8_Away <- c("Iowa", "Alabama", "OhioState", "Houston")

#ELITE 8------------------------------------------------------------------------------------
i <- 1
while (i < 5) {
  probs <- simulate_match(poisson_model, Elite_8_Home[i], Elite_8_Away[i], max_points=300)
  home_score <- predict(poisson_model, 
                        data.frame(team=Elite_8_Home[i], 
                                   opponent=Elite_8_Away[i]), type="response")
  away_score <- predict(poisson_model, 
                        data.frame(team=Elite_8_Away[i], 
                                   opponent=Elite_8_Home[i]), type="response")
  #home team win
  win_prob_home <- sum(probs[lower.tri(probs)])
  result <- paste(Elite_8_Home[i], "has a", win_prob_home * 100,"% chance of winning with a score of",home_score,"(",Elite_8_Home[i],
                  ")","to",away_score,"(",Elite_8_Away[i],")")
  print(result)
  i <- i + 1
}

Final_4_Home <- c("Gonzaga", "Baylor")
Final_4_Away <- c("Michigan", "Houston")

#FINAL 4-------------------------------------------------------------------------------------
i <- 1
while (i < 3) {
  probs <- simulate_match(poisson_model, Final_4_Home[i], Final_4_Away[i], max_points=300)
  home_score <- predict(poisson_model, 
                        data.frame(team=Final_4_Home[i], 
                                   opponent=Final_4_Away[i]), type="response")
  away_score <- predict(poisson_model, 
                        data.frame(team=Final_4_Away[i], 
                                   opponent=Final_4_Home[i]), type="response")
  #home team win
  win_prob_home <- sum(probs[lower.tri(probs)])
  result <- paste(Final_4_Home[i], "has a", win_prob_home * 100,"% chance of winning with a score of",home_score,"(",Final_4_Home[i],
                  ")","to",away_score,"(",Final_4_Away[i],")")
  print(result)
  i <- i + 1
}

#CHAMPIONSHIP GAME - HOUSTON VS. MICHIGAN----------------------------------------------------
probs <- simulate_match(poisson_model, "Houston", "Michigan", max_points=300)
#home team win
win_prob_home <- sum(probs[lower.tri(probs)])
result <- paste("Houston", "has a", win_prob_home * 100,"% chance of winning")
print(result)

Houston_points <- predict(poisson_model, data.frame(team="Houston", 
                                     opponent="Michigan"), type="response")
Houston_points

Michigan_points <- predict(poisson_model, data.frame(team="Michigan", 
                                                    opponent="Houston"), type="response")
Michigan_points

#The model predicts the national champion to be Houston, with a 64-63 victory over Michigan!