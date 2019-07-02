#! /usr/bin/R

# ----------------------------------------------------------------------
# 
# Dependence library
#
lib_list <- c('pdftools', 'jsonlite', 'readxl', 'sqldf', 'xml2')
new.packages <- lib_list[!(lib_list %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(lib_list)
#
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#
# 下載營業項目說明清單
#   參考連結: https://gcis.nat.gov.tw/cod/index.jsp
download.file(url = 'https://gcis.nat.gov.tw/cod/v7_ref_v8.xls', destfile = 'comlevel.xls')
. <- data.frame(readxl::read_excel('comlevel.xls'))
colnames(.) <- c('V7', 'V7_result', 'V8', 'V8_result')
comp.levels <- unique(unlist(.[, c('V7_result', 'V8_result')]))
comp.levels <- comp.levels[!is.na(comp.levels)]
#
# ----------------------------------------------------------------------

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

data.dir <- './pdf'
pdf.list1 <- list.files(data.dir, full.names = TRUE, pattern = '登記清冊.pdf$')
pdf.list2 <- list.files(data.dir, full.names = TRUE, pattern = '項目清冊.pdf$')
creates <- list.files(data.dir, full.names = TRUE, pattern = '.設立.+項目清冊.pdf$')
replaces <- list.files(data.dir, full.names = TRUE, pattern = '.變更.+項目清冊.pdf$')
deletes <- list.files(data.dir, full.names = TRUE, pattern = '.解散.+項目清冊.pdf$')

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

output <- function(fun) {
  # output formater
  #
  # Args:
  #   fun: 執行的程式碼
  #
  # Returns:
  #   NULL
  sink('out.txt')
  print(fun)
  sink()
}

sz <- function(obj, unit = 'MB') {
  # object size formater
  format(object.size(obj), unit = unit)
}

dump_pdf_text <- function(pdfpath, delectHeader = c(-1, -2, -3), errorLogPath='./pdfError.log', ...) {
  # 將pdf檔轉換成文字
  #
  # Args:
  #   pdfpath: pdf檔案路徑
  #   delectHeader: 刪除pdf頭部，預設1:3行
  #   errorLogPath: error.log放置處，預設在R的目前位置(getwd())
  #
  # Returns:
  #   如果pdf輸出有失敗的部分為輸出log檔
  pdftext <- pdftools::pdf_text(pdf = pdfpath)
  pdftext <- strsplit(pdftext, split = '\n')
  pdftext <- lapply(pdftext, function(x) x[delectHeader])
  pdftext <- unlist(pdftext)
  if (identical(pdftext, character(0))) 
  {
    return(NA)
  } 
  else if (length(pdftext) == 1 & length(strsplit(pdftext, ' ')) < 2) 
  {
    return(NA)
  }
  else 
  {
    mainData <- which(
      grepl(pattern = '[0-9]{2,3}/[0-9]{2}/[0-9]{2}', x = pdftext) & grepl(pattern = '[0-9]{8}', x = pdftext)
    ) # 主要資料
    subData <- which(
      !grepl(pattern = '[0-9]{2,3}/[0-9]{2}/[0-9]{2}', x = pdftext) & !grepl(pattern = '[0-9]{8}', x = pdftext)
    ) # 其他資料
    splitSubData <- split(subData, cut(subData, c(mainData, max(subData)))) # 切割其他資料，準備合併到主資料
    dt <- cbind(pdftext[mainData], splitSubData) # 合併
    dt <- apply(dt, 1, function(x) {
      x <- c(x[[1]], pdftext[x[[2]]])
      paste(x, collapse = ' ')
    })
    names(dt) <- NULL
    dt <- as.list(dt)
    
    tryCatch({
      lapply(dt, function(x) {
        x <- unlist(strsplit(x, '\\s'))
        x <- x[x != '']
        # 時間id
        
        which_is_time <- which(is.dateformat(x))
        which_is_address <- max(which(grepl(pattern = '[縣市鄉鎮市區路街村里巷弄號]', x = x[1:which_is_time])))
        
        for (string in which_is_time + 1:length(x))
        {
          if (grepl('^[0-9]*$', x[string]) | is.dateformat(x[string]) | is.na(x[string]) | is.ServicesNo(x[string])) {
            next
          }
          # 檢查後面字串是否正確 若後面字串為地址字串則合併到主要地址字串當中
          if (grepl(pattern = '[0-9]{1,}巷', x = x[string]) | grepl(pattern = '[0-9]{1,}弄', x = x[string]) | grepl(pattern = '[0-9]{1,}號', x = x[string]))
          {
            x[which_is_address] <- paste(x[which_is_address], x[string], collapse = ' ')
            x[string] <- ''
          } 
          else
          {
            x[string] <- ''
          }
        }
        return(x[x != ''])
      })  
    }, error = function(e) {
      ### 輸出 error.log ###
      if (!file.exists(errorLogPath)) 
      {
        errorLogPath <- file(errorLogPath)
      } 
      else 
      {
        errorLogPath <- errorLogPath
      }
      errMsg <- sprintf("[%s] PDF: %s | status: %s", Sys.time(), pdfpath, e)
      write(errorMsg, file = errorLogPath, append = TRUE)
      ### 輸出 error.log ###
    })
  }
}

# run run run XDDDDDD

library(parallel)
cl <- makeCluster(detectCores())
clusterExport(cl, c('is.GenericNo', 'is.goverment', 'is.address', 'is.dateformat', 'is.ServicesNo', 'is.chinese', 'rm.chinese'))

starttime <- Sys.time()
creates.result <- parLapply(cl, creates, dump_pdf_text)
replaces.result <- parLapply(cl, replaces, dump_pdf_text)
deletes.result <- parLapply(cl, deletes, dump_pdf_text)
endtime <-  Sys.time() - starttime