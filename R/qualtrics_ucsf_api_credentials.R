

#' UCSF Qualtrics API Credentialiing
#'
#' @description
#' This function is a wrapper around the `qualtRics::qualtrics_api_credentials()` function where the `base_url` has been set to UCSF's Qualtrics API endpoint, `ucsf.co1.qualtrics.com`.
#' See the UCSF_Qualtrics vignette for details on usage.
#'
#' @param api_key your personal Qualtrics API key formatted in quotes.
#' @param base_url defaults to querying the UCSF Qualtrics instance
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
