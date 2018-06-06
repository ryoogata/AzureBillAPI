#' CSP 価格表の取得
#'
#' @param PC_TOKEN character Azure CSP PartnetCenter Token
#'
#' @return ratecard data.frame
#'
#' @export
getCSPPriceTable <- function(PC_TOKEN){
  # 通貨とリージョンの指定
  CURRENCY <- "JPY"
  REGION <- "JP"

  # UUID の作成
  CORRELATIONID <- uuid::UUIDgenerate()
  REQUESTID <- uuid::UUIDgenerate()

  ## RATE_CARD URL の作成
  RATE_CARD_URL <- paste0("https://api.partnercenter.microsoft.com/v1/ratecards/azure?currency=", CURRENCY
                          ,"&region=", REGION)

  ## RATE_CARD Header の作成
  RATE_CARD_HEADER <- c( 'Authorization' = paste("Bearer", PC_TOKEN, sep = " ")
                         ,'Accept' = "application/json"
                         ,'MS-Contract-Version' = "v1"
                         ,'MS-CorrelationId' = CORRELATIONID
                         ,'MS-RequestId' = REQUESTID
                        )

  ## 価格表の取得
  ratecard_data <- RCurl::getURL(RATE_CARD_URL
                                 ,.opts = list(httpheader = RATE_CARD_HEADER)
                                 ,.encoding = 'UTF-8') %>%
    jsonlite::fromJSON(.)

  RCurl::getURL(RATE_CARD_URL
                ,.opts = list(httpheader = RATE_CARD_HEADER, verbose = FALSE)
                ,.encoding = 'UTF-8')

  ## 価格表の作成
  ratecard <- ratecard_data$meters

  ## ratecard の 列: tags が list なのを charcter に変換
  mode(ratecard$tags) <- "character"
  ratecard$tags[ratecard$tags == "character(0)"] <- ""

  ## 列: rates が data.frame になっているので、列に変換
  rates <- ratecard$rates
  names(rates) <- paste( "CSPListPrice", names(rates), sep = ".")
  ratecard <- ratecard[,!(colnames(ratecard) %in% "rates")]
  ratecard <- cbind(ratecard, rates)

  ## 段階課金のフラグ列: elastic を追加
  CSPListPriceColumn <- grep("CSPListPrice", names(ratecard))
  elasticbill <- ratecard[apply(ratecard[,CSPListPriceColumn][,-1], 1, function(x){!all(is.na(x))}),]
  elasticbill$elastic <- "YES"
  ratecard <- dplyr::left_join(x=ratecard, y=elasticbill[,c("id","elastic")], by = "id")
  ratecard[which(is.na(ratecard$elastic)),]$elastic <- "NO"

  return(ratecard)
}
