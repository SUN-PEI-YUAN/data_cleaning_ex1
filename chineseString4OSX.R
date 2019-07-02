# address <- c(
#   "宜蘭縣數學鎮數學里10鄰數學路１００巷１６之２號",
#   "基隆市太陽區太陽里17鄰太陽三街２２３之２號十九樓",
#   "基隆市白雲區白雲里20鄰白雲三街５９號十樓之１",
#   "新竹市海洋區海洋里13鄰海洋路２９號六樓",
#   "臺北市小明區小名里20鄰小名路２２２號二十樓",
#   "新北市語文區語文里17鄰語文路２２１號二十九樓之５",
#   "宜蘭縣飛機鎮飛機里3鄰飛機路７３號",
#   "新北市紅色區紅色里15鄰紅色路四段１５號之４十七樓"
# )

library(magrittr)
library(plyr)
library(stringr)
library(stringi)

full2halfDigits <- function(x, rawObject = c("ef", "bc")) {
  raw_address <- charToRaw(x)
  loc_maybe_fullwidth_digits <- which(raw_address %in% rawObject)
  split_loc_maybe_fullwidth_digits <- split(
    loc_maybe_fullwidth_digits, 
    cumsum( 
      c(TRUE, diff(loc_maybe_fullwidth_digits)!=1)
    )
  )
  write(x, file = 'errorLogPath.log', append = TRUE)
  # loc_fullwidth_digits <- lapply(split_loc_maybe_fullwidth_digits, function(x) {
  #   if (raw_address[x[1]] == 'ef' & raw_address[x[2]] == 'bc')
  #     x[3] <- x[2] + 1
  #   return(x)
  # })
  # loc_fullwidth_digits <- unlist(loc_fullwidth_digits, use.names = FALSE)
  # replace_element <- loc_fullwidth_digits[!loc_fullwidth_digits %in% loc_maybe_fullwidth_digits]
  # delete_element <- loc_fullwidth_digits[loc_fullwidth_digits %in% loc_maybe_fullwidth_digits]
  # tmp <- as.integer(raw_address[replace_element])
  # raw_address[replace_element] <- as.raw(tmp - 96)
  # raw_address <- rawToChar(raw_address[-delete_element])
  # return(raw_address)
  # [1] "宜蘭縣數學鎮數學里10鄰數學路100巷16之2號"
  # [2] "基隆市太陽區太陽里17鄰太陽三街223之2號十九樓"
  # [3] "基隆市白雲區白雲里20鄰白雲三街59號十樓之1"
  # [4] "新竹市海洋區海洋里13鄰海洋路29號六樓"
  # [5] "臺北市小明區小名里20鄰小名路222號二十樓"
  # [6] "新北市語文區語文里17鄰語文路221號二十九樓之5"
  # [7] "宜蘭縣飛機鎮飛機里3鄰飛機路73號"
  # [8] "新北市紅色區紅色里15鄰紅色路四段15號之4十七樓"
}


chinese2digits <- function(x) {
  vals <- sapply(str_split(x, "")[[1]], function(chi_digit) {
    mapvalues(
      chi_digit,
      c(
        "零", "一", "二",
        "三", "四", "五",
        "六", "七", "八",
        "九", "十", "百",
        "千", "萬", "億"
        ),
      c(0:10, 10 ^ c(2, 3, 4, 8)),
      FALSE
    )
  }) %>% as.integer
  digit_output <- 0
  base_term <- 1
  for (i in rev(seq_along(vals)))
  {
    if (vals[i] >= 10 && i == 1)
    {
      base_term <-
        ifelse(vals[i] > base_term, vals[i], base_term * vals[i])
      digit_output <- digit_output + vals[i]
    } else if (vals[i] >= 10)
    {
      base_term <-
        ifelse(vals[i] > base_term, vals[i], base_term * vals[i])
    } else
    {
      digit_output <- digit_output + base_term * vals[i]
    }
  }
  return(digit_output)
  ## test
  # chinese2digits("一百五十二") # 152
  # chinese2digits("一億零八萬零三百二十三") # 100080323
  # chinese2digits("十九") # 19
}

chinese2Digits <- function(x) {
  x <- sapply(x, function(y) {
    pattern_starts <- "[零一二三四五六七八九十百千萬億]+樓"
    if (!str_detect(y, pattern_starts))
      return(y)
    stairs <- str_extract(y, pattern_starts)
    y <-
      str_replace(y, str_c("(\\d+)(", pattern_starts, ")"), "\\1, \\2")
    y <-
      str_replace(stairs, "樓", "") %>% chinese2digits %>% str_c("樓") %>%
      {
        str_replace(y, stairs, .)
      }
    return(y)
  })
  names(x) <- NULL
  return(x)
# [1] "宜蘭縣數學鎮數學里10鄰數學路100巷16之2號"      "基隆市太陽區太陽里17鄰太陽三街223之2號19樓"
# [3] "基隆市白雲區白雲里20鄰白雲三街59號10樓之1"     "新竹市海洋區海洋里13鄰海洋路29號6樓"
# [5] "臺北市小明區小名里20鄰小名路222號20樓"         "新北市語文區語文里17鄰語文路221號29樓之5"
# [7] "宜蘭縣飛機鎮飛機里3鄰飛機路73號"               "新北市紅色區紅色里15鄰紅色路四段15號之4, 17樓"
}


# example:
  # x <- sapply(address, full2halfDigits, USE.NAMES = FALSE)
  # y <- chinese2Digits(x); x; y;
