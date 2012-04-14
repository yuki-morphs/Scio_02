scioFreqs <- function(wave)
{
    graph_num = 3
    op <- par(mfrow=c(graph_num, graph_num))
    duration = length(wave@left)/44100
    cat("duration: ", duration, "\n")

    interval = duration / (graph_num^2 +1)
    cat("interval: ", interval, "\n")

    for (at in seq(from=interval, to=duration-interval,
        by=interval)) 
        spec(wave, at=at, dB="max0")

    par(op)
}
