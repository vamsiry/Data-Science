# Load packages and data
install.packages("purrr")

packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

data <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\New folder\\train.json")

# unlist every variable except `photos` and `features` and convert to tibble
vars <- setdiff(names(data), c("photos", "features"))
data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)

head(data, n = 1)
###############################################################################
t2$interest_level = as.numeric(t2$interest_level)

t2$building_id = as.numeric(t2$building_id)
t2$manager_id = as.numeric(t2$manager_id)

library(gbm)


gbm1 = gbm(interest_level ~ ., data = t2, distribution = "multinomial", 
           bag.fraction = 0.5, n.trees = 200, interaction.depth =6,
           shrinkage = 0.01, n.minobsinnode = 10)

gbm1$fit
#####################################################################################


