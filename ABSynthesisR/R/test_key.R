test_key <- function()
{
    if(exists("rapidapi_key") == T){return(rapidapi_key)} else
    {rapidapi_key <- Sys.getenv("rapidapi_key")
        if(rapidapi_key == "")
        {message("No key called rapidapi_key found in the .Renviron file.

        If you don't have a key, you can download one from https://rapidapi.com/mirror-analytics-mirror-analytics-default/api/absynthesis/pricing.

        If you have one downloaded already, please ensure it is stored correctly in your .Renviron file with the name 'rapidapi_key'. Run  Sys.getenv('rapidapi_key') to test this")}else
            return(rapidapi_key)}
}
