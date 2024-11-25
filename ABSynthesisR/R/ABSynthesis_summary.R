#' Combine AB test results and get summary statistics
#'
#' @description ABSynthesis_summary returns summary statistics combining the results of all online controlled experiments
#' @param successes_base Count of responses in the base condition
#' @param base_n Count of vistors exposed to the base condition
#' @param successes_variant Count of responses in the base condition
#' @param variant_n Count of vistors exposed to the base condition
#' @return Summary statistics of a meta-analysis of online controlled experiments
#' @examples ABSynthesis_summary(c(10, 10), c(50, 50), c(12, 12), c(50, 50))
#' @export
ABSynthesis_summary <- function(successes_base, base_n, successes_variant, variant_n)
    {   rapidapi_key <- test_key()
        a <- paste(sprintf("successes_base=%s", successes_base), collapse = "&")
        b <- paste(sprintf("visitors_base=%s", base_n), collapse = "&")
        c <- paste(sprintf("successes_variant=%s", successes_variant), collapse = "&")
        d <- paste(sprintf("visitors_variant=%s", variant_n), collapse = "&")
        path <- paste0(c("https://absynthesis.p.rapidapi.com/summary_results?", paste0(c(a, b, c, d), collapse = "&")) , collapse = "")

        return(jsonlite::fromJSON(rawToChar(httr::GET(path, httr::add_headers(`x-rapidapi-host` = 'absynthesis.p.rapidapi.com/' , `x-rapidapi-key`=rapidapi_key))$content)))
    }
