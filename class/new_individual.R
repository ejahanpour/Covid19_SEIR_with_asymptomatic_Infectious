library(R6)

Individual <- R6Class("Individual",
                      public = list(
                        status = "Exposed", 
                        incubation_period = NA,
                        asymptomatic = FALSE,
                        asymptomatic_to_recover = NA,
                        mild = FALSE,
                        mild_to_recover = NA,
                        mild_to_severe = NA,
                        severe = FALSE,
                        severe_to_recover = NA,
                        severe_to_critical = NA,
                        critical = FALSE,
                        critical_to_death = NA,
                        critical_to_recover = NA,
                        initialize = function(incubation_period, asymptomatic = FALSE, asymptomatic_to_recover = NA,
                                              mild = FALSE, mild_to_recover = NA, mild_to_severe = NA,
                                              severe = FALSE, severe_to_recover = NA, severe_to_critical = NA,
                                              critical = FALSE, critical_to_death = NA, critical_to_recover = NA
                        ) {
                          self$incubation_period <- incubation_period
                          self$asymptomatic <- asymptomatic
                          self$asymptomatic_to_recover <- asymptomatic_to_recover
                          self$mild <- mild
                          self$mild_to_recover <- mild_to_recover
                          self$mild_to_severe <- mild_to_severe
                          self$severe <- severe
                          self$severe_to_recover <- severe_to_recover
                          self$severe_to_critical <- severe_to_critical
                          self$critical <- critical
                          self$critical_to_death <- critical_to_death
                          self$critical_to_recover <- critical_to_recover
                        }, 
                        print = function(exposure_time) {
                          cat("An Individual is exposed with exposure time of: ", self$exposure_time)
                        }
                      ))


