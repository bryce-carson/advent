library(tidyverse)
library(magrittr)
library(assertr)

# Common operation
input <- read_file("/Users/bryce/code/advent/three.text")
data <- tibble(nibblebyte = unlist(str_split(input, "\n"))) %>%
  filter(nibblebyte != "") %>%
  mutate(bits = str_split(nibblebyte, ""))
commonbits <- data %>%
  summarize(commonbit = unlist(
    map(
      .x = 1:12,
      .f = ~ round(sum(as.numeric(map_chr(bits, .x))) /
        length(data$nibblebyte))
    )
  ))

str_binary_to_decimal <- function(binary_string) {
  sum(
    as.numeric(unlist(str_split(binary_string, ""))) *
      2**(seq(str_length(binary_string), by = -1) - 1)
  )
}

## Part One
commonbits %>% summarize(
  gammarate = paste0(case_when(
    commonbits == 0 ~ 0,
    commonbits == 1 ~ 1
  )),
  epsilonrate = paste0(case_when(
    commonbits == 0 ~ 1,
    commonbits == 1 ~ 0
  ))
  ) %>% ## FIXME
  str_glue_data("Gamma rate: {paste0(str_c(gammarate, collapse = ''))},\nEpsilon rate: {paste0(str_c(epsilonrate, collapse = ''))}\nPower consumption: {prod(unlist( map_dbl(.x = c(gammarate, epsilonrate), .f = ~ str_binary_to_decimal(paste0(str_c(.x, collapse = '')))) ))}")

## Part Two
## Frequent bit

find_common_bit <- function(tibble, bit_position) {
  summarize(tibble, commonbit = unlist(
    map(
      .x = bit_position,
      .f = ~ round(sum(as.numeric(map_chr(bits, .x))) /
        length(tibble$nibblebyte))
    )
    )) %>% as.character() %>% return()
}

recurse_filtrate_bits <- function(tibble, bit_position, gas) {
  ## FIXME: oxygen just "doesn't" work somehow.
  if (attr(tibble, "row.names") %>% length() <= 2) {
    if (gas == "O2") {
      print("Filtering air.")
      tibble %>%
        verify(nrow(.) > 0) %>%
        verify(nrow(.) <= 2) %>%
        filter(map(bits, bit_position) == "1") %>%
        print() %>%
        return()
    }
    if (gas == "CO2") {
      print("Reticulating splines.")
      print("Carbon dioxide filtered.")
      print("Is air good?")
      print("We don't know, we're elves!")
      tibble %>%
        verify(nrow(.) > 0) %>%
        verify(nrow(.) <= 2) %>%
        filter(map(bits, bit_position) == "0") %>%
        print() %>%
        return()
    }
  } else {
    if (gas == "O2") {
      result <- tibble %>%
        mutate(bitTruth = map2(.x = bits,
                               .y = find_common_bit(tibble, bit_position),
                               .f = ~ .x == .y)) %>%
        filter(map(bitTruth, bit_position) == TRUE)
    } else {
      result <- tibble %>%
        mutate(bitTruth = map2(.x = bits,
                               .y = find_common_bit(tibble, bit_position),
                               .f = ~ .x == .y)) %>%
        filter(map(bitTruth, bit_position) == FALSE)
    }
    recurse_filtrate_bits(result, bit_position = bit_position + 1, gas)
  }
}

## ## With `... %>% print() %>% return()`
## recurse_filtrate_bits(data, 1, "O2")
## recurse_filtrate_bits(data, 1, "CO2")

## Mapping over `c("O2", "CO2")`
map(
  .x = c("O2", "CO2"),
  .f = ~ recurse_filtrate_bits(tibble = data, bit_position = 1,
                               gas = .x)
)
  ## verify(nrow(.) == 2)
## summarize(lifeSupportRating = prod(map_dbl(nibblebyte, str_binary_to_decimal)))
