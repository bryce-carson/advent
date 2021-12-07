library(tidyverse)
library(magrittr)
library(assertr)

# Common operation
input <- read_file("~/code/advent/three.txt")
sample <- read_file("~/code/advent/three_sample.txt")
eshoe <- read_file("~/code/advent/three_eshoe.txt")

## NOTE: just change between input and sample to use the different data.
data <- tibble(nibblebyte = unlist(str_split(input, "\n"))) %>%
  filter(nibblebyte != "") %>%
  mutate(bits = str_split(nibblebyte, ""))

commonbits <- data %>%
  summarize(commonbit = unlist(
    map(
      .x = seq(str_length(data$nibblebyte)[1]),
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
  ) %>%
  summarize(gamma_rate = str_c(gammarate, collapse = ''),
         epsilon_rate = str_c(epsilonrate, collapse = ''),
         power_consumption = prod(
           map_dbl(.x = c(gamma_rate,
                          epsilon_rate),
                   .f = str_binary_to_decimal)))

## Part Two
## Frequent bit

find_common_bit <- function(tibble, bit_position, gas) {
  result <- summarize(tibble,
    commonbit =
      map_chr(
        .x = bit_position,
        .f = ~ sum(as.numeric(map_chr(bits, .x))) / nrow(tibble)
      )
  ) %>%
    mutate(
      commonbit = case_when(
        and(commonbit == 0.5, gas == "O2") ~ "1",
        and(commonbit == 0.5, gas == "CO2") ~ "0",
        commonbit > 0.5 ~ "1",
        commonbit < 0.5 ~ "0",
      )
    ) %>%
    chuck("commonbit")
  return(result)
}

recurse_filtrate_bits <- function(tibble, bit_position, gas) {
  ## NOTE: piping to `return()`, even in different conditions is problematic and
  ## can cause one of them, during a map over different input parameters, to
  ## return `NULL` despite a value existing.
  if (nrow(tibble) <= 2) {
    ## print(tibble)
    if (gas == "O2") {
      result <- tibble %>%
        verify(nrow(.) > 0) %>%
        verify(nrow(.) <= 2) %>%
        filter(map(bits, bit_position) == "1") %>%
        verify(is.null(.) == FALSE) %>%
        verify(nrow(.) == 1)
      return(result)
    }
    if (gas == "CO2") {
      result <- tibble %>%
        verify(nrow(.) > 0) %>%
        verify(nrow(.) <= 2) %>%
        filter(map(bits, bit_position) == "0") %>%
        verify(is.null(.) == FALSE) %>%
        verify(nrow(.) == 1)
      return(result)
    }
  } else {
    result <- tibble %>%
      mutate(bitTruth = map2(
        .x = bits,
        .y = find_common_bit(tibble, bit_position, gas),
        .f = ~ .x == .y
      ))
    if (gas == "O2") {
      result <- result %>%
        filter(map(bitTruth, bit_position) == TRUE)
    } else {
      result <- result %>%
        filter(map(bitTruth, bit_position) == FALSE)
    }

    recurse_filtrate_bits(result, bit_position + 1, gas)
  }
}

## Mapping over `c("O2", "CO2")`
map_dfr(
  .x = c("O2", "CO2"),
  .f = ~ recurse_filtrate_bits(
    tibble = data, bit_position = 1,
    gas = .x
  )
) %>%
  verify(nrow(.) == 2) %>%
  mutate(lifeSupportRating = prod(map_dbl(nibblebyte, str_binary_to_decimal)))
