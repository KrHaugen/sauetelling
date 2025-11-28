#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

library(shiny)
library(zoo)
#library(openxlsx)


if (interactive()) {
  
  ui <- fluidPage(
    textInput("saksnr", "Saksnr", "test"),
    verbatimTextOutput('saksnummer'),
    verbatimTextOutput('preview'),
    
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----

      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t",
                                 colon = ':'),
                     selected = ":"),
        downloadButton("downloadData", "Download"),
    
    mainPanel(
        verbatimTextOutput('preview')
    )
    )
    
    
    
  )
  server <- function(input, output) {
      output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$saksnr, ".csv", sep = "")
      },
      content = function(file) {
        write.csv2(x = data, file = paste(input$saksnr, ".csv", sep = ""), row.names = FALSE, fileEncoding = 'UTF-8')
        #        write.xlsx(data, file)
      }
    )  
    
    
      output$preview <- renderPrint({ 
      if (isTruthy(input$file1$datapath)){
      data <- read.csv(input$file1$datapath, fileEncoding = 'UTF-8', sep = input$sep)
      data$utmelding <- sub("[^a-zA-Z]+","\\1", data$Søye.Lam.Helse.Utmelding.Oppvekstkode)
      data$utmelding[data$utmelding == ''] <- '.'
      
      # rader der det har skjedd en forskyvning
      temp <- which(data$utmelding == data$Søye.Lam.Helse.Utmelding.Oppvekstkode)
      
      data_temp <- data.frame(do.call(rbind,strsplit(data$utmelding, split = '\\.')))
      
      # Finne steder der det er flere datoer på samme individ og lage rader for hver dato
      data$Søye.Lam.Helse.Utmelding.Oppvekstkode[c(temp[grep('/', substr(data$Søye.Lam.Helse.Utmelding.Oppvekstkode[temp + 1], 1,3))]+1)]
      data$Søye.Lam.Helse.Utmelding.Oppvekstkode[c(temp[grep('/', substr(data$Søye.Lam.Helse.Utmelding.Oppvekstkode[temp + 1], 1,3))]+1)] <- paste(substr(data$Søye.Lam.Helse.Utmelding.Oppvekstkode[c(temp[grep('/', substr(data$Søye.Lam.Helse.Utmelding.Oppvekstkode[temp + 1], 1,3))]-1)], 1,5), data$Søye.Lam.Helse.Utmelding.Oppvekstkode[c(temp[grep('/', substr(data$Søye.Lam.Helse.Utmelding.Oppvekstkode[temp + 1], 1,3))]+1)])
      
      
      data <- cbind(data, data_temp)
      colnames(data) <- c('søye', 'slett', 'utmelding', 'tapsårsak')
      
      data <- data[data$utmelding != 'Blindtarmskoksidose',]
      data$tapsårsak[data$tapsårsak == data$utmelding] <- ''
      data$slett <- NULL
      
      data_temp <- data.frame(do.call(rbind,strsplit(data$søye, split = "/")))
      data_temp$X1 <- substr(data_temp$X1,7,8)
      data_temp$X3 <- substr(data_temp$X3,1,2)
      data_temp$dato <- paste0(data_temp$X1,'/', data_temp$X2, '/', data_temp$X3)
      data_temp$dato <- gsub("//", '', data_temp$dato)
      
      data_temp$dato[nchar(data_temp$dato) > 8] <- ''
      
      
      data <- cbind(data, data_temp$dato)
      
      data$søye <- sub("\\D*(\\d+).*", "\\1", data$søye)
      data <- data[nchar(data$søye) > 4,]
      data <- data[!is.na(data$søye),]
      
      data$oppvekstmelding <- ''
      data$oppvekstmelding[grep('Kopplam', data$utmelding)] <- 'Kopplam'
      data$oppvekstmelding[grep('Fosterlam', data$utmelding)] <- 'Fosterlam'
      data$oppvekstmelding[grep('Kopplam', data$tapsårsak)] <- 'Kopplam'
      data$oppvekstmelding[grep('Fosterlam', data$tapsårsak)] <- 'Fosterlam'
      data$utmelding[data$utmelding == data$oppvekstmelding] <- ''
      
      data$tapsårsak <- gsub(' Kopplam', '', data$tapsårsak)
      data$tapsårsak <- gsub('Kopplam', '', data$tapsårsak)
      data$tapsårsak <- gsub(' Fosterlam', '', data$tapsårsak)
      data$tapsårsak <- gsub('Fosterlam', '', data$tapsårsak)
      
      data <- data[,c('søye', 'data_temp$dato', 'utmelding','tapsårsak', 'oppvekstmelding')]
      colnames(data) <- c('søye', 'dato', 'utmelding','tapsårsak', 'oppvekstmelding')
      data$lam <- ''
      data$lam[substr(data$søye,1,1) == '5'] <- data$søye[substr(data$søye,1,1) == '5']
      data <- data[,c('søye','lam', 'dato', 'utmelding','tapsårsak', 'oppvekstmelding')]
      data$søye[data$søye == data$lam] <- ''
      
      data <- data[data$søye != 'Søye/Lam Helse Utmelding Oppvekstkode',]
      data <- data[data$søye != 'Side ',]
      data <- data[data$utmelding != 'Sye utmeldt',]
      data <- data[data$søye != 'utmeldt',]
      
      
      temp_gaupe <- which(data$utmelding == 'gaupe')
      temp_ukjent <- which(data$utmelding == 'ukjentart') 
      temp_tatt <- grep('Tatt/skadet av', data$tapsårsak)
      data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_gaupe -1))])] <- gsub('Tatt/skadet av', 'Tatt/skadet av gaupe', data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_gaupe -1))])])
      data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_ukjent -1))])] <- gsub('Tatt/skadet av rovdyr,', 'Tatt/skadet av ukjent rovdyr', data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_ukjent -1))])])
      data$tapsårsak[data$tapsårsak == ' Tatt/skadet av rovdyr, ukjent art'] <- 'Tatt/skadet av ukjent rovdyr'
      data$tapsårsak[data$tapsårsak == ' Tatt/skadet av ukjent rovdyr'] <- 'Tatt/skadet av ukjent rovdyr'
      
      data <- data[data$søye != 'ukjent art',]
      data <- data[data$søye != 'gaupe',]
      data <- data[data$søye != 'klostridiebakterier',]
      
      
      data$oppvekstmelding[which(data$søye == 'Fosterlam')-1] <- paste0(data$oppvekstmelding[which(data$søye == 'Fosterlam')-1], 'Fosterlam')
      data <- data[data$søye != 'Fosterlam',]
      data <- data[data$søye != 'diagnose',]
      data <- data[data$søye != 'Kopplam',]
      data <- data[data$søye != 'mild,moderat,alvorlig',]
      data <- data[data$søye[data$søye != ''] != data$utmelding[data$søye != ''],]}
      else {data = ''}
      print(data)
      })      
      
      

  }
  shinyApp(ui, server)
}
