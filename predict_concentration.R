##################################################################
# Author: Marc Rigau
# Title: Predict concentration values
# Date: Jan 13, 2021
# Version: 1
##################################################################
library( tidyverse )

##################################################################
####
##

# Set workspace directory
getwd()

# Type in the file name in which the data is to be predicted.
# Note, this file must contain three columns defined as the following:
#
# Column 1: Sample names
# Column 2: Measured fluorescent values
# Column 3: Known concentration values (leave blank otherwise)

file_name <- 'file_name.csv'
file_name

##
####
##################################################################
data = keep.raw.d <- read.csv( file_name, header = T )
data

names(data)[2] <- 'Fluorescence'

names(data)[3] <- 'Concentration'

# Dependent variable Y = Concentration
# Independent variable X = Fluorescence

# => Subset known values in Y
data.model <- subset( data, !is.na( Concentration ) )

# Fluorescence values to be determined as concentration
ggplot( data.model, aes( y = Concentration, x = Fluorescence ) ) +
  geom_point( color = "blue", size = 2 )

# => if exponential data, transform to logs.

##################################################################
# MODEL
##################################################################

# Test alternative models until finding the one that has the closest R^2 to 1

# Linear
model <- lm( Concentration ~ Fluorescence, data.model ); summary( model )
# => is R^2 close to 1?
# => if so, okay. Otherwise, transfer data in exp, log, square, etc., and re-run the model again.

# Before logs:
# Zeros => epsilon = 1e-4
#data[ which( data$Concentration == 0 ), "Concentration" ] = 0.0001

## Dependent variable => log
#data.model$Log.Concentration <- log10( data.model$Concentration )
#model <- lm( Log.Concentration ~ Fluorescence, data.model ); summary( model)

## Independent variable => log
#data.model$Log.Fluorescence <- log( data.model$Fluorescence )
#model <- lm( Concentration ~ Log.Fluorescence, data.model ); summary( model)

## Both variables => log
#model <- lm( Log.Concentration ~ Log.Fluorescence, data.model ); summary( model)


# Plot with best R^2 fit model
ggplot( data.model, aes( y = fitted( model ), x = Fluorescence ) ) + 
  geom_line( size = 1, col = "red" ) +
  geom_point( size = 2 ) +
  labs( title = "Model", 
        x = "Fluorescence", 
        y = "Concentration" ) 

# Check whether the residual of the model is normally distributed. If p.value > 0.05 => ND.
shapiro.test( residuals( model ) ) 

##################################################################
# PREDICTION
##################################################################

# Predict all
prediction <- predict( model, interval = "prediction", newdata = data )

# Insert into the original data frame
data$Predicted <- prediction[ , "fit" ]

# Round predictions down to three decimal positions
data$Predicted <- round( data$Predicted, 3 )
data

# Put into a data frame
predicted <- as.data.frame( cbind(data$Sample, data$Predicted) )
names(predicted) <- c('Sample', 'Concentration')
predicted

# Backup data into a file
write.csv(data, file= file_name, row.names = F )

# View predicted values on a plot
ggplot( data, aes( y = Predicted, x = Fluorescence ) ) +
  geom_line( size = .5, col = "orange" ) +
  geom_point( size = 2, shape=8 ) +
  geom_point( aes( y = Concentration ), col = "blue", size = 2 ) +
  labs( title = "Model: predicted values",
        x = "Fluorescence",
        y = "Concentration" )


