createPatientLevelPredictionValidationModuleSpecifications <- function(
  modelLocationList, # a vector of plpModel locations
  validationComponentsList = list(
    list(
      targetId = c(1,1),
      oucomeId = c(2,4)
      #populationSettings = , # add a population setting for a different tar?
      ), 
    list(
          targetId = 3,
          oucomeId = 2
          )
    ),
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), # vector
  validationSettings = PatientLevelPrediction::createValidationSettings(recalibrate = "weakRecalibration")
) {
  
  if(length(modelLocationList) != length(validationComponentsList)){
    stop('modelLocationList and validationComponentsList must be same length')
  }
  
  specifications <- list(
    module = "PatientLevelPredictionValidationModule",
    version = "0.0.3",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = list(
      modelLocationList = modelLocationList,
      validationComponentsList = validationComponentsList,
      restrictPlpDataSettings = restrictPlpDataSettings,
      validationSettings = validationSettings
    )
  )
  class(specifications) <- c("PatientLevelPredictionValidationModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}