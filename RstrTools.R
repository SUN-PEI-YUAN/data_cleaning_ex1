strsplit2 <- function(x, split, type = "remove", perl = FALSE, ...) {
  # strsplit 進化版
  # 
  if (type == "remove") 
  {
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } 
  else if (type == "before") 
  {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x, split = paste0("(?<=.)(?=", split, ")"), perl = TRUE, ...)
  } 
  else if (type == "after") 
  {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x, split = paste0("(?<=", split, ")"), perl = TRUE, ...)
  } 
  else 
  {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

is.GenericNo <- function(x, ...) {
  # 檢測x向量是否為統一編號
  #
  # Args:
  #   x: 字串向量
  #
  # Returns:
  #   logical
  cond <- '[0-9]{8}'
  return(grepl(pattern = cond, x = x, ...))
}

is.goverment <- function(x, ...) {
  # 檢測x向量是否為政府名稱
  #
  # Args:
  #   x: 字串向量
  #
  # Returns:
  #   logical
  gov.name <- c(
    "基隆市政府", "新北市政府", "台北巿政府",
    "桃園市政府", "新竹縣政府", "新竹市政府",
    "苗栗縣政府", "台中市政府", "南投縣政府",
    "雲林縣政府", "嘉義縣政府", "嘉義市政府",
    "台南市政府", "高雄巿政府", "屏東縣政府",
    "宜蘭縣政府", "花蓮縣政府", "台東縣政府",
    "澎湖縣政府", "金門縣政府", "連江縣政府",
    "彰化縣政府",
  )
  return(x %in% gov.name)
}

is.address <- function(x, ...) {
  # 檢測x向量是否為地址
  #
  # Args:
  #   x: 字串向量
  #
  # Returns:
  #   logical
  cond <- '[縣市鄉鎮市區路街村里巷弄號之樓室]'
  grepl(pattern = cond, x = x, ...)
}

is.dateformat <- function(x, ...) {
  # 檢測x向量是否為日期
  #
  # Args:
  #   x: 字串向量
  #
  # Returns:
  #   logical
  cond <- '[0-9]{2,3}/[0-9]{2}/[0-9]{2}'
  return(grepl(pattern = cond, x = x, ...))
}

is.ServicesNo <- function(x, ...) {
  # 檢測x向量是否為營業項目
  #
  # Args:
  #   x: 字串向量
  #
  # Returns:
  #   logical
  cond <- '[A-Z+][A-Z0-9]{6}'
  return(grepl(pattern = cond, x = x, ...))
}

is.chinese <- function(x, ...) {
  # 移除x中含有的中文
  #
  # Args:
  #   x: 字串向量
  #
  # Returns:
  #   logical
  
  cond <- '[\u4e00-\u9fa5]{1,}'
  return(grepl(pattern = cond, x = x, ...))
}

rm.chinese <- function(x, ...) {
  # 移除x中含有的中文
  #
  # Args:
  #   x: 字串向量
  #
  # Returns:
  #   logical
  
  cond <- '[\u4e00-\u9fa5]{1,}'
  return(gsub(pattern = cond, replacement = '', x = x, ...))
}
