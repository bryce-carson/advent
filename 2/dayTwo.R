library(tidyverse)
library(rlist)
library(doParallel)

### Part One
inputDay2 <- read_csv("~/Downloads/inputDay2.txt", col_types = "c", col_names = "direction")
data <- inputDay2$direction %>%
  map(str_split, pattern = " ") %>%
  list.flatten() %>%
  tibble(direction = map_chr(., 1),
         amount = map_chr(., 2) %>%
           as.integer()) %>%
  select(direction, amount)

group_by(data, direction) %>%
  summarize(value = sum(amount))

## This is horrible and I shouldn't be doing manual calculations like this but oh well. Quick and dirty.
(1910 - 977) * 2165

### Part Two
inputDayTwo <- read_lines("~/Downloads/inputDay2.txt")
aim = 0
position = list(horiontal = 0, depth = 0)
foreach(string = inputDayTwo, .combine = bind_rows) %do% {
  direction <- str_split(string, " ")[[1]][1]
  amount <- str_split(string, " ")[[1]][2] %>% as.numeric()
    if(direction == "down") { aim <<- aim + amount }
    if(direction == "up") { aim <<- aim - amount }
    if(direction == "forward") {
      position$horiontal <<- position$horiontal + amount
      position$depth <<- position$depth + 2*aim
    }
  tibble(direction, amount)
}

