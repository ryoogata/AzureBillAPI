#' 定価表の取得
#'
#' @param SUBSCRIPTION_ID character サブスクリプションID
#' @param ADTOKEN character AD Token
#'
#' @return pricelist data.frame
#'
#' @export
getPriceTable <- function(SUBSCRIPTION_ID, ADTOKEN){

  # 内部関数の定義

  ##  Elastic 価格表の取得
  getMeterRatesList <- function(ROW){
    result <- ROW %>%
      .[!is.na(.)] %>%
      as.list()

    return(result)
  }

  #APIVERSIO <- "2015-06-01-preview"
  APIVERSIO <- "2016-08-31-preview"
  OFFERDURABLEID <- "MS-AZR-0003P"
  CURRENCY <- "JPY"
  LOCALE <- "en-US"
  REGIONINFO <- "JP"
  FILTER <- paste0("OfferDurableId eq '", OFFERDURABLEID, "' and Currency eq '", CURRENCY
                  ,"' and Locale eq '", LOCALE
                  ,"' and RegionInfo eq '", REGIONINFO, "'")

  # Header の作成
  HEADER <- c('Authorization' = paste("Bearer", ADTOKEN, sep = " ")
              ,'Accept' = "application/json")

  # URL の作成
  PRICE_URL <- paste0("https://management.azure.com/subscriptions/", SUBSCRIPTION_ID
                     ,"/providers/Microsoft.Commerce/RateCard?api-version=", APIVERSIO
                     ,"&%24filter=", URLencode(FILTER))

  # データの取得
  h = RCurl::basicTextGatherer()
  txt <- RCurl::getURL(PRICE_URL
                ,.opts = list(httpheader = HEADER, verbose = FALSE)
                ,.encoding = 'UTF-8'
                ,header = TRUE
                ,followLocation = FALSE
                ,headerfunction = h$update
                )

  REDIRECT_URL <- read.dcf(textConnection(paste(h$value(NULL)[-1], collapse=""))) %>%
    magrittr::extract(4)

  pricelist_data <- RCurl::getURL(REDIRECT_URL) %>%
    jsonlite::fromJSON(.)

  # 価格表の作成
  pricelist <- pricelist_data$Meters

  tmp <- apply(pricelist$MeterRates, 1, getMeterRatesList)
  pricelist$RatesList <- tmp

  # 価格表の整形 ----

  # pricelist の MeterTags が list なのを charcter に変換
  mode(pricelist$MeterTags) <- "character"
  pricelist$MeterTags[pricelist$MeterTags == "character(0)"] <- ""

  # MeterRates が data.frame になっているので、列に変換
  MeterRates <- pricelist$MeterRates
  names(MeterRates) <- paste( "ListPrice", names(MeterRates), sep = ".")
  pricelist <- pricelist[,!(colnames(pricelist) %in% "MeterRates")]
  pricelist <- cbind(pricelist, MeterRates)

  # 段階課金のフラグ列: elastic を追加
  ListPriceColumn <- grep("ListPrice", names(pricelist))
  elasticbill <- pricelist[apply(pricelist[,ListPriceColumn][,-1], 1, function(x){!all(is.na(x))}),]
  elasticbill$elastic <- "YES"
  pricelist <- dplyr::left_join(x=pricelist, y=elasticbill[,c("MeterId","elastic")], by = "MeterId")
  pricelist[which(is.na(pricelist$elastic)),]$elastic <- "NO"

  return(pricelist)
}
