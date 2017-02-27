
# Creates new environment for the package
if (!exists(".indicoio")) {
  .indicoio <- new.env()
}

.onAttach <- function(libname, pkgname) {
  # Shows welcome message
  packageStartupMessage("\n========================================================\nindicoio: A simple R wrapper for the indico set of APIs \nFind more at: http://indico.io\n========================================================\n")
}

.onLoad <- function(libname, pkgname) {
  # Sets package-wide variables
  if (exists(".indicoio")) {
    .indicoio$header <- c("Content-type" = "application/json",
                          "Accept" = "application/json",
                          "client-lib" = "R",
                          "version-number" = "0.10.4")
    .indicoio$remote_api <- "https://apiv2.indico.io/"
    .indicoio$private_cloud <- FALSE
    .indicoio$api_key = FALSE
    .indicoio$cloud = FALSE

    # Paths to search for config files
    loadConfiguration()
  }
}

loadConfiguration <- function() {
  # Load configuration from files and env variables
  globalPath <- path.expand("~/.indicorc")
  localPath <- file.path(getwd(), ".indicorc")
  globalConfig <- readFile(globalPath)
  localConfig <- readFile(localPath)
  loadConfigFile(globalConfig)
  loadConfigFile(localConfig)
  loadEnvironmentVars()
}

loadEnvironmentVars <- function() {
  # Load auth from environment variables
  authDefined <- (Sys.getenv("INDICO_API_KEY") != "")
  if (authDefined) {
    .indicoio$api_key <- Sys.getenv("INDICO_API_KEY")
  }

  # Load subdomain from environment variables
  cloudDefined <- (Sys.getenv("INDICO_CLOUD") != "")
  if (cloudDefined) {
    .indicoio$cloud <- Sys.getenv("INDICO_CLOUD")
  }
}

readFile <- function(filepath) {
  # Returns file content or FALSE if the path does not exist
  if (!file.exists(filepath)) {
    content <- FALSE
  } else {
    connection <- file(filepath)
    content  <- readLines(connection)
    if (length(content) == 0L) {
      content <- FALSE
    }
    close(connection)
  }
  content
}

loadConfigFile <- function(content) {
  # Load from global configuration file
  if (is.character(content)) {
    config <- Parse.INI(content)
    if (validAuthConfig(config)) {
      .indicoio$api_key <- config$auth$api_key
    }

    if (validPrivateCloudConfig(config)) {
      .indicoio$cloud <- config$private_cloud$cloud
    }
  }
}

validAuthConfig <- function(config) {
  # ensure .ini file contains the proper fields
  return (("auth" %in% names(config)) &&
          ("api_key" %in% names(config[['auth']])))
}

validPrivateCloudConfig <- function(config) {
  # ensure .ini file contains the proper fields
  return (("private_cloud" %in% names(config)) &&
          ("cloud" %in% names(config[['private_cloud']])))
}

trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

Parse.INI <- function(Lines)
{
  # Parse .ini style configuration files (.indicorc)

  # change section headers
  Lines <- chartr("[]", "==", Lines)

  connection <- textConnection(Lines)
  d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
  close(connection)

  # location of section breaks
  L <- d$V1 == ""
  d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3],
                           V1 != "")

  value <- sprintf("'%s'", trim(d$V2))
  ToParse <- paste("INI.list$", d$V3, "$",  d$V1, " <- ", value, sep="")

  INI.list <- list()
  eval(parse(text=ToParse))

  return(INI.list)
}
