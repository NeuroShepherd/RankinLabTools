

#' Title
#'
#' @param api_key
#' @param base_url
#' @param overwrite
#' @param install
#'
#' @return
#' @export
#'
#' @examples
qualtrics_ucsf_api_credentials <- function(api_key, base_url = "ucsf.co1.qualtrics.com", overwrite = FALSE, install = FALSE) {

  qualtRics::qualtrics_api_credentials(api_key, base_url, overwrite, install)

}
