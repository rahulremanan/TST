dependencies <-  c( "Hmisc", 
                    "reshape2",
                    "ggplot2",
                    "GGally",
                    "randomForest",
                    "ggthemes",
                    "gridExtra",
                    "grid",
                    "caret",
                    "plyr",
                    "ggplot2")
missing.dependencies <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
if(length(missing.dependencies)) {
  install.packages(missing.dependencies, repos='http://cran.us.r-project.org',dependencies=TRUE)
}
