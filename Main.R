# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of "PatientLevelPredictionValidationModule
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

# smoking variable
getSmokingCovariateData <- function(connection,
  oracleTempSchema = NULL,
  cdmDatabaseSchema,
  cohortTable = "#cohort_person",
  cohortId = -1,
  cdmVersion = "5",
  rowIdField = "subject_id",
  covariateSettings,
  aggregated = FALSE
  ){
  
  
  if (aggregated)
    stop("Aggregation not supported")
  
  # Some SQL to construct the covariate:
  sql <-  "select @row_id_field, max(smoke_value)*1000+@analysis_id as covariate_id,
1 as covariate_value
from (
select p.@row_id_field, o.OBSERVATION_CONCEPT_ID,
  case 
     when VALUE_AS_STRING = 'Current smoker' then 3
     when VALUE_AS_STRING = 'Previously smoked' then 2 
     when VALUE_AS_STRING = 'Not currently smoking' then 2
     else  1 
  end as smoke_value
  from @cdm_database_schema.observation o inner join @cohort_table p
  on o.person_id = p.subject_id where 
  o.OBSERVATION_CONCEPT_ID = 40766362 and 
  o.OBSERVATION_DATE <= dateadd(day, @end_day, p.cohort_start_date)
  and o.OBSERVATION_DATE >= dateadd(day, @start_day, p.cohort_start_date)
  and o.VALUE_AS_STRING in ('Never smoked','Current smoker','Previously smoked','Not currently smoking')
  ) obs
  group by @row_id_field
  "
  sql <- SqlRender::render(sql,
    cohort_table = cohortTable,
    cohort_id = cohortId,
    row_id_field = rowIdField,
    cdm_database_schema = cdmDatabaseSchema,
    analysis_id = covariateSettings$analysisId,
    start_day = covariateSettings$startDay,
    end_day = covariateSettings$endDay
    )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  
  # Construct covariate reference:
  covariateRef <-  data.frame(
    covariateId = (1:3)*1000+covariateSettings$analysisId,
    covariateName = c('never-smoker', 'previous_smoker' ,'Smoker'),
    analysisId = rep(covariateSettings$analysisId,3),
    conceptId = rep(0,3)
  )
  
  # Construct analysis reference:
  analysisRef <- data.frame(
    analysisId = covariateSettings$analysisId,
    analysisName = "Smoking status",
    domainId = "Smoking",
    startDay = covariateSettings$startDay,
    endDay = covariateSettings$endDay,
    isBinary = "Y",
    missingMeansZero = "Y"
    )
  
  # Construct analysis reference:
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(
    covariates = covariates, 
    covariateRef = covariateRef, 
    analysisRef = analysisRef
    )
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  return(result)
  
}

# Module methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

getSharedResourceByClassName <- function(sharedResources, className) {
  returnVal <- NULL
  for (i in 1:length(sharedResources)) {
    if (className %in% class(sharedResources[[i]])) {
      returnVal <- sharedResources[[i]]
      break
    }
  }
  invisible(returnVal)
}

# this updates the cohort table details in covariates
updateCovariates <- function(plpModel, cohortTable, cohortDatabaseSchema){
  
  covSettings <- plpModel$modelDesign$covariateSettings
  # if a single setting make it into a list to force consistency
  if(inherits(covSettings, 'covariateSettings')){
    covSettings <- list(covSettings)
  }
  
  for(i in 1:length(covSettings)){
    if('cohortTable' %in% names(covSettings[[i]])){
      covSettings[[i]]$cohortTable <- cohortTable
    }
    if('cohortDatabaseSchema' %in% names(covSettings[[i]])){
      covSettings[[i]]$cohortDatabaseSchema <- cohortDatabaseSchema
    }
  }
  
  plpModel$modelDesign$covariateSettings <- covSettings
  
  return(plpModel)
}

createCohortDefinitionSetFromJobContext <- function(sharedResources, settings) {
  cohortDefinitions <- list()
  if (length(sharedResources) <= 0) {
    stop("No shared resources found")
  }
  cohortDefinitionSharedResource <- getSharedResourceByClassName(sharedResources = sharedResources, 
                                                                 class = "CohortDefinitionSharedResources")
  if (is.null(cohortDefinitionSharedResource)) {
    stop("Cohort definition shared resource not found!")
  }
  cohortDefinitions <- cohortDefinitionSharedResource$cohortDefinitions
  if (length(cohortDefinitions) <= 0) {
    stop("No cohort definitions found")
  }
  cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in 1:length(cohortDefinitions)) {
    cohortJson <- cohortDefinitions[[i]]$cohortDefinition
    cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(
      cohortId = as.integer(cohortDefinitions[[i]]$cohortId),
      cohortName = cohortDefinitions[[i]]$cohortName,
      json = cohortJson,
      stringsAsFactors = FALSE
    ))
  }
  return(cohortDefinitionSet)
}

# Module methods -------------------------
execute <- function(jobContext) {
  rlang::inform("Validating inputs")
  inherits(jobContext, 'list')

  if (is.null(jobContext$settings)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }
  
  workFolder <- jobContext$moduleExecutionSettings$workSubFolder
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  
  rlang::inform("Executing PLP Validation")
  moduleInfo <- getModuleInfo()

  # find where cohortDefinitions are as sharedResources is a list
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(
    sharedResources = jobContext$sharedResources,
    settings = jobContext$settings
    )
  
  # check the model locations are valid and apply model
  upperWorkDir <- dirname(workFolder)
  modelTransferFolder <- sort(dir(upperWorkDir, pattern = 'ModelTransferModule'), decreasing = T)[1]
  
  modelSaveLocation <- file.path( upperWorkDir, modelTransferFolder, 'models') # hack to use work folder for model transfer 
  modelLocationList <- file.path(modelSaveLocation, jobContext$settings$modelLocationList)

  databaseNames <- c()
  modelInd <- 0
  for(modelLocation in modelLocationList){   
    modelInd <- modelInd + 1
    plpModel <- tryCatch(
      {PatientLevelPrediction::loadPlpModel(modelLocation)},
      error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    )
    if(!is.null(plpModel)){

      # append model ind to ensure analysis id is unique
      plpModel$trainDetails$analysisId <- paste0(plpModel$trainDetails$analysisId, '_', modelInd)
   
      # update the plpModel cohort covariates to replace the schema and cohortTable
      # cohortDatabaseSchema, cohortTable
      plpModel <- updateCovariates(
        plpModel = plpModel,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, 
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema
        )
      
      # create the database details:
      databaseDetails <- list()
      for(ddind in 1:max(1, length(jobContext$settings$validationComponentsList[[modelInd]]$targetId))){

        tid <- jobContext$settings$validationComponentsList[[modelInd]]$targetId[ddind]
        oid <- jobContext$settings$validationComponentsList[[modelInd]]$outcomeId[ddind]
        
        databaseNames <- c(databaseNames, paste0(jobContext$moduleExecutionSettings$connectionDetailsReference,'_T',tid,'_O',oid ))

          databaseDetails[[ddind]] <- PatientLevelPrediction::createDatabaseDetails(
            connectionDetails = jobContext$moduleExecutionSettings$connectionDetails, 
            cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
            cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
            cdmDatabaseName = paste0(jobContext$moduleExecutionSettings$connectionDetailsReference,'_T',tid,'_O',oid ),
            cdmDatabaseId = jobContext$moduleExecutionSettings$databaseId,
            #tempEmulationSchema =  , is there s temp schema specified anywhere?
            cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, 
            outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema, 
            outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, 
            targetId = tid, # could make this a list
            outcomeIds = oid # could make this a list
          )
        }
      
      PatientLevelPrediction::externalValidateDbPlp(
        plpModel = plpModel, 
        validationDatabaseDetails = databaseDetails, 
        validationRestrictPlpDataSettings = jobContext$settings$restrictPlpDataSettings[[modelInd]], 
        settings = jobContext$settings$validationSettings[[modelInd]], 
        #logSettings = , 
        outputFolder = workFolder
      )

    } else{
      ParallelLogger::logInfo(paste0('Issue loading model at ', modelLocation))
    }
  }
  
  # save results into sqlite
  # move results into database
  for(databaseName in unique(databaseNames)){
    tryCatch({
      PatientLevelPrediction::insertResultsToSqlite(
        resultLocation = file.path(workFolder, databaseName), 
        cohortDefinitions = cohortDefinitionSet,
        #databaseList = PatientLevelPrediction::createDatabaseList(
        #  cdmDatabaseSchemas = validationDatabaseDetail$cdmDatabaseSchema,
        #  cdmDatabaseNames = validationDatabaseDetail$cdmDatabaseName,
        #  databaseRefIds = validationDatabaseDetail$cdmDatabaseId 
        #),
        sqliteLocation = file.path(workFolder,'sqlite')
      )
    })
  }

  # Export the results
  rlang::inform("Export data to csv files")

  sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(workFolder, "sqlite","databaseFile.sqlite")
  )
    
  PatientLevelPrediction::extractDatabaseToCsv(
    connectionDetails = sqliteConnectionDetails, 
    databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
      resultSchema = 'main', # sqlite settings
      tablePrefix = '', # sqlite settings
      targetDialect = 'sqlite', 
      tempEmulationSchema = NULL
    ), 
    csvFolder = file.path(workFolder, 'results'),
    fileAppend = NULL
  )
  
  # Zip the results
  rlang::inform("Zipping csv files")
  DatabaseConnector::createZipFile(
    zipFile = file.path(resultsFolder, 'results.zip'),
    files = file.path(workFolder, 'results')
  )
  
  
}
