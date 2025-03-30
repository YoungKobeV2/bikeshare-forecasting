
# get entry script path
entry_script_path <- paste0(
    Sys.getenv("AML_APP_ROOT"),
    "/",
    Sys.getenv("AZUREML_ENTRY_SCRIPT")
)

# plumb/build api
pr <- plumber::plumb(entry_script_path)

# set Docs = TRUE to get build in UI
pr$setDocs(TRUE)

# api arguments 
api_args <- list(host="0.0.0.0",port=8000)

# start api 
do.call(pr$run,api_args)

