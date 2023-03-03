# @file Logging.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of PioneerWatchfulWaiting
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.systemInfo <- function() {
  si <- sessionInfo()
  lines <- c()
  lines <- c(lines, "R version:")
  lines <- c(lines, si$R.version$version.string)
  lines <- c(lines, "")
  lines <- c(lines, "Platform:")
  lines <- c(lines, si$R.version$platform)
  lines <- c(lines, "")
  lines <- c(lines, "Locale:")
  lines <- c(lines, si$locale)
  lines <- c(lines, "")
  lines <- c(lines, "Attached base packages:")
  lines <- c(lines, paste("-", si$basePkgs))
  lines <- c(lines, "")
  lines <- c(lines, "Other attached packages:")
  for (pkg in si$otherPkgs) lines <- c(lines,
                                       paste("- ", pkg$Package, " (", pkg$Version, ")", sep = ""))
  return(paste(lines, collapse = "\n"))
}

rootFTPFolder <- function() {
  return("/Task5/")
}

#' @export
uploadDiagnosticsResults <- function(outputFolder, privateKeyFileName, userName) {
  uploadResults(file.path(outputFolder, "diagnostics"), privateKeyFileName, userName, remoteFolder = paste0(rootFTPFolder(), "CohortDiagnostics"))
}

#' @export
uploadStudyResults <- function(outputFolder, privateKeyFileName, userName) {
  uploadResults(outputFolder, privateKeyFileName, userName, remoteFolder = paste0(rootFTPFolder(), "StudyResults"))
}

#' Upload results to OHDSI server
#' 
#' @details 
#' This function uploads the 'Results_<databaseId>.zip' to the OHDSI SFTP server. Before sending, you can inspect the zip file,
#' wich contains (zipped) CSV files. You can send the zip file from a different computer than the one on which is was created.
#' 
#' @param privateKeyFileName   A character string denoting the path to the RSA private key provided by the study coordinator.
#' @param userName             A character string containing the user name provided by the study coordinator.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#'                             
uploadResults <- function(outputFolder, privateKeyFileName, userName, remoteFolder) {
  fileName <- list.files(outputFolder, "^Results_.*.zip$", full.names = TRUE)
  if (length(fileName) == 0) {
    stop("Could not find results file in folder. Did you run (and complete) execute?") 
  }
  if (length(fileName) > 1) {
    stop("Multiple results files found. Don't know which one to upload") 
  }
  OhdsiSharing::sftpUploadFile(privateKeyFileName = privateKeyFileName, 
                               userName = userName,
                               remoteFolder = remoteFolder,
                               fileName = fileName)
  ParallelLogger::logInfo("Finished uploading")
}



# A helper function to give an error if rendered SQL contains parameters
assertNoParameters <- function(renderedSql) {
  remainingParams <- unique(stringr::str_extract_all(renderedSql, "@\\w+")[[1]])
  if (length(remainingParams) > 0) {
    remainingParams <- paste(remainingParams, collapse = ",")
    rlang::abort(paste("SQL contains parameters! ", remainingParams))
  }
  return(renderedSql)
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor)/nrow(data), 1)
    msg <- glue::glue("censoring {sum(toCensor)} values ({percent}%) from {fieldName} because value below minimum")
    ParallelLogger::logInfo(msg)
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}
