#' Azure CSP PartnetCenter Token の取得
#'
#' @param DOMAIN character
#' @param CLIENT_ID character
#' @param CLIENT_SECRET character
#'
#' @return TOKEN character CSP PartnetCenter Token
#'
#' @export
getAzureCSPPartnerCenterToken <- function(DOMAIN, CLIENT_ID, CLIENT_SECRET){

  # URL の作成
  URL <- paste0("https://login.windows.net/", DOMAIN, "/oauth2/token?api-version=1.0")

  # HTTP メッセージボディの作成
  BODY <- paste0("grant_type=client_credentials&resource=https://graph.windows.net&client_id=", CLIENT_ID
                              ,"&client_secret=", CLIENT_SECRET
                              )

  # POST の実行
  RESPONSE <- httr::POST(URL, body = BODY, httr::timeout(30))

  # 取得した Response から token を取得
  TOKEN <- httr::content(RESPONSE)$access_token

  return(TOKEN)
}
