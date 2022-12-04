#####################################################################
# Shiny App to uncover the mental representation of one's own voice #
# ----------------------------------------------------------------- #
# Written by Ladislas Nalborczyk                                    #
# Contact: ladislas.nalborczyk@gmail.com                            #
# Last updated on December 4, 2022                                  #
#####################################################################

# loading the required packages
library(shinyhelper)
library(shinythemes)
library(base64enc)
library(tidyverse)
library(phonTools)
library(patchwork)
library(seewave)
library(shiny)
library(tuneR)
library(av)

# see also https://books.psychstat.org/rdata/audio-data.html
# importing the audio file
son <- readWave("base_sound_trimmed.wav")

# computing the spectrogram
spectrogramme <- spectro(
    wave = son, f = son@samp.rate, plot = FALSE,
    ovlp = 88, dBref = 2*10e-5, wn = "hanning", wl = 1024
    )

# converting it to a dataframe
frequence <- rep(spectrogramme$freq * 1000, times = ncol(spectrogramme$amp) )
temps <- rep(spectrogramme$time, each = nrow(spectrogramme$amp) )
amplitude <- as.numeric(spectrogramme$amp)

df <- data.frame(
    temps = as.numeric(temps),
    frequence = as.numeric(frequence),
    amplitude = as.numeric(amplitude)
    )

# plotting it
p1 <- data.frame(y = son@left) %>%
    mutate(x = 1:n() /son@samp.rate) %>%
    ggplot(aes(x = x, y = y) ) +
    geom_line() +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    labs(x = "", y = "Amplitude") +
    coord_cartesian(expand = FALSE)

p2 <- df %>%
    ggplot(aes(x = temps, y = frequence, fill = amplitude) ) +
    geom_raster(interpolate = TRUE, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    labs(x = "Temps (s)", y = "Fréquence (Hz)") +
    # scale_fill_brewer(palette = "Spectral") +
    scale_fill_viridis_c(option = "magma") +
    coord_cartesian(expand = FALSE)

# combining both plots
p1 / p2

# importing raw signal
# wave_in <- read_audio_bin(
#     audio = "www/base_sound_trimmed.wav",
#     sample_rate = NULL,
#     start_time = NULL,
#     end_time = NULL
#     )

# computing the fft
# wave_in <- read_audio_fft(
#     audio = "www/base_sound_trimmed.wav",
#     window = hanning(1024),
#     overlap = 0.75,
#     sample_rate = NULL,
#     start_time = NULL,
#     end_time = NULL
#     )

# plotting the spectrogram
# see https://github.com/cran/av/blob/master/R/fft.R
# plot(wave_in, dark = FALSE, legend = FALSE)

# https://cran.r-project.org/web/packages/soundgen/vignettes/acoustic_analysis.html
library(soundgen)

s1 = soundgen(sylLen = 900, temperature = 0.001,
              pitch = list(time = c(0, .3, .8, 1), 
                           value = c(300, 900, 400, 1300)),
              noise = c(-40, -20), 
              subFreq = 100, subDep = 20, jitterDep = 0.5, 
              plot = TRUE, ylim = c(0, 4))

playme(s1)  # to replay w/o re-synthesizing the sound
plot(s1)

spectrogram(
    s1, samplingRate = 16000,
    osc = 'dB',  # plot oscillogram in dB
    heights = c(2, 1),  # spectro/osc height ratio
    noiseReduction = .5,  # subtract the spectrum of noisy parts
    brightness = -1,  # reduce brightness
    contrast = .5,  # increase contrast
    colorTheme = 'heat.colors',  # pick color theme
    cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
    grid = 5,  # lines per kHz; to customize, add manually with graphics::grid()
    ylim = c(0, 5),  # always in kHzmain = 'Spectrogram')
    main = 'Spectrogram'
    )

library(sound)

findWavPlayer()
WavPlayer()
setWavPlayer(command=NULL)

setWavPlayer(command = "afplay")

a <- pitch(s = "base_sound_trimmed.wav", semitones = -2)
pitch(s = "base_sound_trimmed.wav", semitones = -2) %>% play()
play(s = "base_sound_trimmed.wav")
play(pitch(son,-12))

a <- pitch(s = "base_sound_trimmed.wav", semitones = -2)
playsound(sound = a$sound[1, ])

library(tuneR)
tuneR::play(object = a$sound[1, ])

s <- Sine(440,1)
play(s)

# record a sound from R
library(audio)
s <- 1
f <- 44100
class.record <- s*f
record(where = class.record, rate = f, channels = 1)
play(class.record)

# code from https://books.psychstat.org/rdata/audio-data.html
x <- rep(NA_real_, 44100)
# start recording into x
record(where = x, rate = 44100, channels = 1)
# monitor the recording progress
# par(ask=FALSE) # for continuous plotting
# while (is.na(x[length(x)])) plot(x, type='l', ylim=c(-1, 1))
# play the recorded audio
play(x)
s <- as.Sample(sound = x, rate = 44100, bits = 16)
play(s)
# now played backwards
play(reverse(s) )
play(pitch(s, 10) )
play(reverse(pitch(s, -10) ) )
