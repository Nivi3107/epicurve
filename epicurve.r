epicurve <- R6::R6Class(
    "epicurve",
    inherit = jmvcore::Analysis,

    public = list(
        initialize = function(options, data, ...) {
            super$initialize(options, data, ...)
            
            # Add a status message for debugging
            self$results$add(jmvcore::Preformatted$new(
                options = options,
                name = "status",
                title = "Status"
            ))
            
            # Add the plot to the results
            self$results$add(jmvcore::Image$new(
                options = options,
                name = "plot",
                title = "Epicurve",
                renderFun = private$.plot,
                requiresData = TRUE
            ))
        },
        .run = function() {
            # This function is executed when the analysis is run.
            
            # Update status message
            self$results$status$setContent("Preparing data...")
            
            # Get the date variable name from the options
            dateVarName <- self$options$dateVar
            
            # Stop if no date variable is selected
            if (is.null(dateVarName)) {
                self$results$status$setContent("Please select a date variable.")
                return()
            }
            
            # Get the data
            data <- self$data
            
            # Convert the date column to Date objects
            try({
                data[[dateVarName]] <- as.Date(data[[dateVarName]])
            }, silent = TRUE)
            
            # Check if the conversion was successful
            if (! "Date" %in% class(data[[dateVarName]])) {
                jmvcore::reject("The selected date variable could not be converted to a date format. Please ensure it is in a standard format (e.g., YYYY-MM-DD).")
                self$results$status$setContent("Error: Invalid date format.")
                return()
            }

            # Store the prepared data in a private field for the plot function to access
            private$.plotData <- data
            self$results$status$setContent("Data ready for plotting.")
        }
    ),
    
    private = list(
        .plotData = NULL, # A private field to hold the data for plotting
        
        .plot = function(image, ...) {
            # This function renders the actual plot.
            
            tryCatch({
                # Get the prepared data
                plotData <- private$.plotData
                dateVarName <- self$options$dateVar
                interval <- self$options$interval
                
                # Stop if there's no data
                if (is.null(plotData) || nrow(plotData) == 0) {
                    return(FALSE)
                }
                
                # Aggregate data based on the selected time interval
                plotData$time_group <- switch(
                    interval,
                    day   = lubridate::floor_date(plotData[[dateVarName]], "day"),
                    week  = lubridate::floor_date(plotData[[dateVarName]], "week"),
                    month = lubridate::floor_date(plotData[[dateVarName]], "month")
                )
                
                # Count cases per time interval
                counts <- as.data.frame(table(plotData$time_group))
                names(counts) <- c("Date", "Cases")
                counts$Date <- as.Date(counts$Date)
                
                # Create the plot using ggplot2
                p <- ggplot2::ggplot(counts, ggplot2::aes(x = Date, y = Cases)) +
                    ggplot2::geom_col(fill = "#0072B2", color = "white", width=0.8) + # Using geom_col for bar chart
                    ggplot2::labs(
                        title = "Epidemic Curve",
                        subtitle = paste("Case counts by", interval),
                        x = paste("Date of Onset (by", interval, ")"),
                        y = "Number of Cases"
                    ) +
                    ggplot2::theme_minimal(base_size = 14) +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5),
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                    )

                # Print the plot to the device
                print(p)
                
                # Return TRUE to indicate the plot was successfully created
                return(TRUE)
            }, error = function(e) {
                # If an error occurs during plotting, show it in the status
                self$results$status$setContent(paste("Plotting Error:", e$message))
                return(FALSE)
            })
        }
    )
)