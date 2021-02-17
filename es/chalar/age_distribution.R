offset_quinary <- seq(from=4, by=5, to=24)
offset_decary <- seq(from=31, by=10, to=71)
offset_elderly <- 82

quinary_band <- c(1:5)
decary_band <- c(1:10)
elderly_band <- c(1:16)

weightings <- function(original, index) {
    subset <- original[index]
    return(subset / sum(subset))
}

reweighted <- function(weightings, aggregate_value) {
    return(weightings * aggregate_value)
}

reweight <- function(offset, band, data_idx){
    function(aggregated_data, full_data, new_data){
        for (i in offset){
            idx = i + band - data_idx
            new_data[idx] <- reweighted(weightings(full_data, idx), aggregated_data[i])
            }
    return(new_data)}
    }

boo <- numeric(91)

f_quinary <- reweight(offset_quinary, quinary_band, 4)
f_decary <- reweight(offset_decary, decary_band, 6)
f_elderly <- reweight(offset_elderly, elderly_band, 7)

reweight_year <- function(data, reweight_year, full_year){
    rw_year <- data$Casualties[data$Year == reweight_year]
    f_year <- data$Casualties[data$Year == full_year]
    rw <- c(1:length(rw_year))
    rw <- f_quinary(rw_year, f_year, rw)
    rw <- f_decary(rw_year, f_year, rw)
    rw <- f_elderly(rw_year, f_year, rw)
    return(rw)
    }

