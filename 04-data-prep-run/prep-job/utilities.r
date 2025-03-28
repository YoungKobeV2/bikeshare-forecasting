# Azure ML utility to enable usage of the MLFlow R API for tracking with Azure Machine Learning (Azure ML)

library(mlflow)
library(httr)
library(later)

# Improved Python binary detection
get_python_bin <- function() {
  # Try multiple ways to find Python
  python_paths <- c(
    Sys.which("python"),
    Sys.which("python3"),
    system("which python 2>/dev/null", intern = TRUE),
    system("which python3 2>/dev/null", intern = TRUE),
    "/usr/bin/python",
    "/usr/bin/python3",
    "/usr/local/bin/python",
    "/usr/local/bin/python3"
  )
  
  # Filter out empty results and get the first valid one
  python_paths <- python_paths[nzchar(python_paths)]
  if (length(python_paths) > 0) {
    return(python_paths[1])
  }
  
  stop("Python binary not found. Please install Python or set MLFLOW_PYTHON_BIN environment variable.")
}

# Improved MLflow binary detection
get_mlflow_bin <- function() {
  # Try multiple ways to find MLflow
  mlflow_paths <- c(
    Sys.which("mlflow"),
    system("which mlflow 2>/dev/null", intern = TRUE),
    system2(get_python_bin(), c("-m", "pip", "show", "mlflow"), stdout = TRUE, stderr = FALSE),
    "/usr/local/bin/mlflow"
  )
  
  # Filter out empty results and get the first valid one
  mlflow_paths <- mlflow_paths[nzchar(mlflow_paths)]
  if (length(mlflow_paths) > 0) {
    return(mlflow_paths[1])
  }
  
  stop("MLflow not found. Please run `pip install mlflow` or set MLFLOW_BIN environment variable.")
}

# Set MLFlow related env vars with improved detection
tryCatch({
  if (!nzchar(Sys.getenv("MLFLOW_PYTHON_BIN"))) {
    Sys.setenv(MLFLOW_PYTHON_BIN = get_python_bin())
  }
  if (!nzchar(Sys.getenv("MLFLOW_BIN"))) {
    Sys.setenv(MLFLOW_BIN = get_mlflow_bin())
  }
}, error = function(e) {
  warning(paste("Failed to automatically configure MLflow:", e$message))
})

# MLflow client for Azure ML
new_mlflow_client.mlflow_azureml <- function(tracking_uri) {
  host <- paste("https", tracking_uri$path, sep = "://")
  get_host_creds <- function() {
    mlflow:::new_mlflow_host_creds(
      host = host,
      token = Sys.getenv("MLFLOW_TRACKING_TOKEN"),
      username = Sys.getenv("MLFLOW_TRACKING_USERNAME", NA),
      password = Sys.getenv("MLFLOW_TRACKING_PASSWORD", NA),
      insecure = Sys.getenv("MLFLOW_TRACKING_INSECURE", NA)
    )
  }
  
  cli_env <- function() {
    creds <- get_host_creds()
    res <- list(
      MLFLOW_TRACKING_USERNAME = creds$username,
      MLFLOW_TRACKING_PASSWORD = creds$password,
      MLFLOW_TRACKING_TOKEN = creds$token,
      MLFLOW_TRACKING_INSECURE = creds$insecure
    )
    res[!is.na(res)]
  }
  
  mlflow:::new_mlflow_client_impl(get_host_creds, cli_env, class = "mlflow_azureml_client")
}

# Authentication functions
get_auth_header <- function() {
  headers <- list()
  auth_token <- Sys.getenv("MLFLOW_TRACKING_TOKEN")
  auth_header <- paste("Bearer", auth_token, sep = " ")
  headers$Authorization <- auth_header
  headers
}

get_token <- function(host, exp_id, run_id) {
  req_headers <- do.call(httr::add_headers, get_auth_header())
  token_host <- gsub("mlflow/v1.0", "history/v1.0", host)
  token_host <- gsub("azureml://", "https://", token_host)
  api_url <- paste0(token_host, "/experimentids/", exp_id, "/runs/", run_id, "/token")
  GET(api_url, timeout(getOption("mlflow.rest.timeout", 30)), req_headers)
}

fetch_token_from_aml <- function() {
  message("Refreshing token")
  tracking_uri <- Sys.getenv("MLFLOW_TRACKING_URI")
  exp_id <- Sys.getenv("MLFLOW_EXPERIMENT_ID")
  run_id <- Sys.getenv("MLFLOW_RUN_ID")
  sleep_for <- 1
  time_left <- 30
  
  response <- get_token(tracking_uri, exp_id, run_id)
  while (response$status_code == 429 && time_left > 0) {
    time_left <- time_left - sleep_for
    warning(paste("Request returned with status code 429 (Rate limit exceeded). Retrying after ",
                sleep_for, " seconds. Will continue to retry 429s for up to ", time_left,
                " second.", sep = ""))
    Sys.sleep(sleep_for)
    sleep_for <- min(time_left, sleep_for * 2)
    response <- get_token(tracking_uri, exp_id)
  }

  if (response$status_code != 200) {
    warning(paste("Error fetching token will try again after sometime:", response$status_code))
  } else {
    text <- content(response, "text", encoding = "UTF-8")
    json_resp <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    Sys.setenv(MLFLOW_TRACKING_TOKEN = json_resp$token)
    message("Token refreshed successfully")
  }
}

clean_tracking_uri <- function() {
  tracking_uri <- httr::parse_url(Sys.getenv("MLFLOW_TRACKING_URI"))
  tracking_uri$query <- ""
  Sys.setenv(MLFLOW_TRACKING_URI = httr::build_url(tracking_uri))
}

# Token refresh scheduling
schedule_token_refresh <- function() {
  interval <- as.numeric(Sys.getenv("MLFLOW_TOKEN_REFRESH_INTERVAL_SECONDS", 30))
  fetch_token_from_aml()
  later::later(schedule_token_refresh, delay = interval)
}

# Initialize
clean_tracking_uri()
schedule_token_refresh()

# Verify MLflow is properly configured
tryCatch({
  mlflow:::mlflow_get_active_run_id()
  message("MLflow configured successfully")
}, error = function(e) {
  warning(paste("MLflow configuration issue:", e$message))
  warning("Please ensure Python and MLflow are properly installed and configured")
})