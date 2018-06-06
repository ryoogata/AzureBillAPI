#' AzureAD Token の取得
#'
#' @param TENANT_ID character
#' @param CLIENT_ID character
#' @param CLIENT_SECRET character
#'
#' @return TOKEN character Azure AD Token
#'
#' @export
getAzureADToken <- function(TENANT_ID, CLIENT_ID, CLIENT_SECRET){

  # URL の作成
  URL <- paste0("https://login.microsoftonline.com/" , TENANT_ID , "/oauth2/token?api-version=1.0")

  # HTTP メッセージボディの作成
  BODY <-  paste0("grant_type=client_credentials&client_id=" ,CLIENT_ID
                  ,"&client_secret=" ,CLIENT_SECRET
                  ,"&resource=https%3A%2F%2Fmanagement.azure.com%2F"
                  )

  # POST の実行
  RESPONSE <- httr::POST(URL, body = BODY, httr::timeout(30))

  # 取得した Response から TOKEN を取得
  TOKEN <- httr::content(RESPONSE)$access_token

  return(TOKEN)
}
