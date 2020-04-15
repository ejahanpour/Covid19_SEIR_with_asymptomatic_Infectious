library(R6)

Individual <- R6Class("Individual",
                      public = list(
                          status = "Exposed", 
                          exposure_time = NA,
                          asymptomatic = FALSE,
                          asymptomatic_time = NA,
                          mild = FALSE,
                          mild_to_recover = NA,
                          mild_time = NA,
                          severe = FALSE,
                          severe_to_recover = NA,
                          severe_time = NA,
                          critical = FALSE,
                          critical_to_death = NA,
                          critical_to_recover = NA,
                          cirical_time = NA,
                          initialize = function(exposure_time, asymptomatic = FALSE, asymptomatic_time = NA,
                                                mild = FALSE, mild_to_recover = NA, mild_time = NA,
                                                severe = FALSE, severe_to_recover = NA, severe_time = NA,
                                                critical = FALSE, critical_to_death = NA, critical_to_recover = NA, critical_time = NA
                                                ) {
                            self$exposure_time <- exposure_time
                            self$asymptomatic <- asymptomatic
                            self$asymptomatic_time <- asymptomatic_time
                            self$mild <- mild
                            self$mild_to_recover <- mild_to_recover
                            self$mild_time <- mild_time
                            self$severe <- severe
                            self$severe_to_recover <- severe_to_recover
                            self$severe_time <- severe_time
                            self$critical <- critical
                            self$critical_to_death <- critical_to_death
                            self$critical_to_recover <- critical_to_recover
                            self$cirical_time <- critical_time
                          }, 
                          print = function(exposure_time) {
                            cat("An Individual is exposed with exposure time of: ", self$exposure_time)
                          }
                          ))


