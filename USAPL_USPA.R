library(dplyr)
library(lubridate)

url <-"http://www.openpowerlifting.org/static/data/openpowerlifting.csv"
url2 <- "http://www.openpowerlifting.org/static/data/meets.csv"
pl <- read.csv(url)
meets <- read.csv(url2)

openpl <- left_join(pl, meets, by = "MeetID")
openpl$Division[openpl$Federation=="USPA" & openpl$Division == "Open Men"] <- "Open"
openpl$Division[openpl$Federation=="USPA" & openpl$Division == "Open Women"] <- "Open"


pct_calc <- function(fed, division, gender, weight) {
  fed_meets <- openpl %>% filter(Federation == fed)
  fed_meets$Date <- ymd(fed_meets$Date)
  fed_meets <- fed_meets %>% filter(Date >= "2013-01-01")
  
  fed_raw <- fed_meets %>% filter(Equipment == "Raw", Sex == gender, WeightClassKg == weight, Division == division) %>% 
      filter(!is.na(TotalKg) & !is.na(BestSquatKg) & !is.na(BestBenchKg) & !is.na(BestDeadliftKg)) %>% 
      mutate(total_rank = rank(TotalKg)/length(TotalKg), 
             sq_rank = rank(BestSquatKg)/length(BestSquatKg),
             bp_rank = rank(BestBenchKg)/length(BestBenchKg),
             dl_rank = rank(BestDeadliftKg)/length(BestDeadliftKg))
  
  
  fed_raw$total_rank <- round(fed_raw$total_rank/0.05) * 5
  fed_raw$total_rank[fed_raw$total_rank == 100] <- 99
  fed_raw$total_rank[fed_raw$total_rank == 0] <- 5
  fed_raw$total_rank <- as.factor(fed_raw$total_rank)
  
  fed_raw$sq_rank <- round(fed_raw$sq_rank/0.05) * 5
  fed_raw$sq_rank[fed_raw$sq_rank == 100] <- 99
  fed_raw$sq_rank[fed_raw$sq_rank == 0] <- 5
  fed_raw$sq_rank <- as.factor(fed_raw$sq_rank)
  
  fed_raw$bp_rank <- round(fed_raw$bp_rank/0.05) * 5
  fed_raw$bp_rank[fed_raw$bp_rank == 100] <- 99
  fed_raw$bp_rank[fed_raw$bp_rank == 0] <- 5
  fed_raw$bp_rank <- as.factor(fed_raw$bp_rank)
  
  fed_raw$dl_rank <- round(fed_raw$dl_rank/0.05) * 5
  fed_raw$dl_rank[fed_raw$dl_rank == 100] <- 99
  fed_raw$dl_rank[fed_raw$dl_rank == 0] <- 5
  fed_raw$dl_rank <- as.factor(fed_raw$dl_rank)
  
  total <- fed_raw %>% 
    group_by(total_rank) %>% 
    arrange(total_rank) %>% 
    summarise(mean_total = mean(TotalKg)*2.20462)
  
  squat <- fed_raw %>% 
    group_by(sq_rank) %>% 
    arrange(sq_rank) %>% 
    summarise(mean_squat = mean(BestSquatKg)*2.20462)
  
  bench <- fed_raw %>% 
    group_by(bp_rank) %>% 
    arrange(bp_rank) %>% 
    summarise(mean_bench = mean(BestBenchKg)*2.20462)
  
  dl <- fed_raw %>% 
    group_by(dl_rank) %>% 
    arrange(dl_rank) %>% 
    summarise(mean_dl = mean(BestDeadliftKg)*2.20462)
  
  df <- data.frame(Percentile = total$total_rank, 
                   Squat = round(squat$mean_squat), 
                   Bench = round(bench$mean_bench), 
                   Deadlift = round(dl$mean_dl), 
                   Total = round(total$mean_total))  

  
  return(df)
}

pct_calc("USPA", "Open", "M", 60)
pct_calc("USPA", "Open", "M", 67.5)
pct_calc("USPA", "Open", "M", 75)
pct_calc("USPA", "Open", "M", 82.5)
pct_calc("USPA", "Open", "M", 90)
pct_calc("USPA", "Open", "M", 100)
pct_calc("USPA", "Open", "M", 110)
pct_calc("USPA", "Open", "M", 125)
pct_calc("USPA", "Open", "M", 140)
pct_calc("USPA", "Open", "M", "140+")


pct_calc("USAPL", "R-O", "M", 59)
pct_calc("USAPL", "R-O", "M", 66)
pct_calc("USAPL", "R-O", "M", 74)
pct_calc("USAPL", "R-O", "M", 83)
pct_calc("USAPL", "R-O", "M", 93)
pct_calc("USAPL", "R-O", "M", 105)
pct_calc("USAPL", "R-O", "M", 120)
pct_calc("USAPL", "R-O", "M", "120+")

pct_calc("USPA", "Open", "F", 48)
pct_calc("USPA", "Open", "F", 52)
pct_calc("USPA", "Open", "F", 56)
pct_calc("USPA", "Open", "F", 60)
pct_calc("USPA", "Open", "F", 67.5)
pct_calc("USPA", "Open", "F", 75)
pct_calc("USPA", "Open", "F", 82.5)
pct_calc("USPA", "Open", "F", 90)
pct_calc("USPA", "Open", "F", "90+")

pct_calc("USAPL", "R-O", "F", 47)
pct_calc("USAPL", "R-O", "F", 52)
pct_calc("USAPL", "R-O", "F", 57)
pct_calc("USAPL", "R-O", "F", 63)
pct_calc("USAPL", "R-O", "F", 72)
pct_calc("USAPL", "R-O", "F", 84)
pct_calc("USAPL", "R-O", "F", "84+")


