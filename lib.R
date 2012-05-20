library("seewave")
library("tuneR")

calcCharVecs <- function(wave)
{
    peak_matrix <- fpeaks(meanspec(wave, plot=FALSE), nmax=1, plot=FALSE)
    peak_matrix[,1]
}

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

scioPlotFreq <- function(samples, wn = "hanning", logScale = NULL)
{
    cat("length: ", length(samples), "\n")
    cat("window name: ", wn, "\n")

    W <- ftwindow(length(samples), wn = wn)
    freq <- fft(W*samples)

    data_for_plot <- if (is.null(logScale)) Mod(freq)
       else log10(Mod(freq)) 

    plot(data_for_plot, type='l')
}

scioWaveFreq <- function(wave, from=0, to=length(wave@left)/wave@samp.rate)
{
    f <- wave@samp.rate
    from_frame <- round(f*from)
    to_frame <- round(f*to)

    cat("f: ", f, "\n")
    cat("from_frame: ", from_frame, "\n")
    cat("to_frame: ", to_frame, "\n")

    op <- par(mfrow=c(1, 2))

    plot(wave@left[from_frame:to_frame], type='l')
    spec(wave, from=from, to=to)

    par(op)
}

scioGenSin <- function(freq=1, amp=1, duration=1, f=1)
{
    omega = 2*pi*freq
    sample_num = round(duration*f);

    sample_seq = seq(from=0, to=duration, length.out=sample_num)
    ts(amp*sin(omega*sample_seq), start=0, end=duration, frequency=f)
}
