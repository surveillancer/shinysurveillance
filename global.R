algorithm_choices <- c("EARSC" = "ears",
                       "Farrington Flexible" = "farringtonflexible",
                       "Negative Binomial CUSUM" = "glrnb")
algorithm_name_dict <- setNames(names(algorithm_choices), algorithm_choices)