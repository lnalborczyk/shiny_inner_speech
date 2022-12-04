#####################################################################
# Shiny App to uncover the mental representation of one's own voice #
# ----------------------------------------------------------------- #
# Written by Ladislas Nalborczyk                                    #
# Contact: ladislas.nalborczyk@gmail.com                            #
# Last updated on December 4, 2022                                  #
#####################################################################

# loading the required packages
library(shinyWidgets)
library(shinyhelper)
library(shinythemes)
library(base64enc)
library(tidyverse)
library(phonTools)
library(patchwork)
library(seewave)
library(audio)
library(sound)
library(shiny)
library(tuneR)
library(av)

################################################################################
############################# USER INTERFACE ###################################
################################################################################

url <- "https://twitter.com/intent/tweet?text=Find%20out%20how%20your%20inner%20voice%20sounds&url=https://shiny.rstudio.com/gallery/widget-gallery.html/"

ui <- fluidPage(

    # application title
    titlePanel("How does your voice sound?"),
    tags$h3("Modify the voice until it matches how you hear it in your head"),

    # choosing a theme
    # themeSelector(), # cerulean, spacelab, readable
    
    # setting a theme
    theme = shinytheme("spacelab"),
    
    # uploading an audio file
    sidebarLayout(
        sidebarPanel(
            fluidRow(h3("Step 1: Getting some sound") ),
            tags$br(),
            fileInput(
                inputId = "uploaded_audio",
                label = "Pick an audio file (wav/mp3)",
                multiple = FALSE,
                accept = c(".wav", "audio/*")
                ),
            tags$p(strong("Or record your voice from the mic:") ),
            actionButton(
                inputId = "recorded_audio",
                label = "Record audio",
                icon = icon("microphone"),
                multiple = FALSE
                ),
            # tags$br(),
            tags$hr(),
            fluidRow(h3("Step 2: Manipulating the sound") ),
            tags$br(),
            sliderInput(
                inputId = "pitch",
                label = "Select a pitch increment/decrement (in semitones)",
                value = 0,
                min = -10,
                max = 10,
                step = 1,
                width = "500px"
                ),
            tags$hr(),
            fluidRow(h3("Step 3: Sharing your mental representation") ),
            tags$br(),
            # https://stackoverflow.com/questions/62052804/is-there-a-way-to-add-share-buttons-to-make-plots-shareable-in-shiny
            actionButton(
                inputId = "tweet",
                label = "Share",
                icon = icon("twitter"),
                # Combine text with url variable
                onclick = sprintf("window.open('%s')", url)
                )
            ),
        mainPanel(
            # playing the uploaded audio signal
            actionButton(inputId = "play_audio", label = "Play Audio"),
            plotOutput(outputId = "audio_signal_plot")
            
        )
    ),
    
    # including a footer
    tags$hr(),
    HTML(
        paste(
            "Written by <a href='https://www.barelysignificant.com'>
                    Ladislas Nalborczyk</a>. Last update: December 4, 2022"
            )
        )
)

################################################################################
################################# SERVER #######################################
################################################################################

server <- function (input, output) {
    
    observeEvent(
        
        input$play_audio, {
        
            # input$refresh
            req(input$uploaded_audio)
        
            base64 <- dataURI(file = input$uploaded_audio$datapath, mime = "audio/wav")
        
            insertUI(
                selector = "#play_audio", where = "afterEnd",
                ui = tags$audio(src = base64, type = "audio/wav", autoplay = NA, controls = NA)  
                )
            
            }
        )
    
    output$audio_signal_plot <-
        renderPlot({
            
            input$refresh
            
            # computing the fft
            # wave_in <- read_audio_fft(
            #     audio = input$uploaded_audio$datapath, #"www/base_sound_trimmed.wav",
            #     window = hanning(1024),
            #     overlap = 0.75,
            #     sample_rate = NULL,
            #     start_time = NULL,
            #     end_time = NULL
            #     )
            
            # plotting the spectrogram
            # plot(wave_in)
            
            # importing the audio file
            # audio_file <- readWave("www/base_sound_trimmed.wav")
            audio_file <- readWave(filename = input$uploaded_audio$datapath)
            
            # computing the spectrogram
            spectrogram <- spectro(
                wave = audio_file, f = audio_file@samp.rate, plot = FALSE,
                ovlp = 88, dBref = 2*10e-5, wn = "hanning", wl = 1024
                )
            
            # converting it to a dataframe
            # frequence <- rep(spectrogramme$freq * 1000, times = ncol(spectrogramme$amp) )
            # temps <- rep(spectrogramme$time, each = nrow(spectrogramme$amp) )
            # amplitude <- as.numeric(spectrogramme$amp)
            
            df <- data.frame(
                time = as.numeric(rep(spectrogram$time, each = nrow(spectrogram$amp) ) ),
                freq = rep(spectrogram$freq * 1000, times = ncol(spectrogram$amp) ),
                amp = as.numeric(spectrogram$amp)
                )
            
            # plotting it
            p1 <- data.frame(y = audio_file@left) %>%
                mutate(x = 1:n() / audio_file@samp.rate) %>%
                ggplot(aes(x = x, y = y) ) +
                geom_line() +
                theme_bw(base_size = 12, base_family = "Open Sans") +
                labs(x = "", y = "Amplitude") +
                coord_cartesian(expand = FALSE)
            
            p2 <- df %>%
                ggplot(aes(x = time, y = freq, fill = amp) ) +
                geom_raster(interpolate = TRUE, show.legend = FALSE) +
                theme_bw(base_size = 12, base_family = "Open Sans") +
                labs(x = "Time (s)", y = "Frequence (Hz)") +
                # scale_fill_brewer(palette = "Spectral") +
                scale_fill_viridis_c(option = "magma") +
                coord_cartesian(expand = FALSE)
            
            # combining both plots
            p1 / p2
            
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
