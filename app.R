Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

library(shiny)
library(zoo)
library(openxlsx)


ui <- fluidPage(
  
  verbatimTextOutput('saksnummer'),
  textInput("saksnr", "Saksnr", ""),
  textAreaInput('rapport', 'Rapport', ''),
  
  # Input: Select a file ----
  #      fileInput("file1", "Choose CSV File",
  #                multiple = FALSE,
  #                accept = c("text/csv",
  #                           "text/comma-separated-values,text/plain",
  #                           ".csv")),
  #      radioButtons("sep", "Separator",
  #                   choices = c(Comma = ",",
  #                               Semicolon = ";",
  #                               Tab = "\t",
  #                               colon = ':'),
  #                   selected = ":"),
  downloadButton("downloadData", "Download"),
  verbatimTextOutput('stats'),
  verbatimTextOutput('preview')
  
)



server <- function(input, output) {

    data <- reactiveVal()
    stats <- reactiveVal()
    observeEvent(input$rapport,{
      if (isTruthy(input$rapport)){
      #        data <- read.csv(input$file1$datapath, fileEncoding = 'UTF-8', sep = input$sep)
      data <- read.csv(text=input$rapport, fileEncoding = 'UTF-8', sep = ';')
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
      temp_ukjent_rovdyr <- which(data$utmelding == 'rovdyrukjent art') 
      temp_tatt <- grep('Tatt/skadet av', data$tapsårsak)
      data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_gaupe -1))])] <- gsub('Tatt/skadet av', 'Tatt/skadet av gaupe', data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_gaupe -1))])])
      data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_ukjent -1))])] <- gsub('Tatt/skadet av rovdyr,', 'Tatt/skadet av ukjent rovdyr', data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_ukjent -1))])])
      data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_ukjent_rovdyr -1))])] <- gsub('Tatt/skadet av', 'Tatt/skadet av ukjent rovdyr', data$tapsårsak[as.numeric(temp_tatt[which((temp_tatt) %in% (temp_ukjent_rovdyr -1))])])
      
      data$tapsårsak[data$tapsårsak == ' Tatt/skadet av rovdyr, ukjent art'] <- 'Tatt/skadet av ukjent rovdyr'
      data$tapsårsak[data$tapsårsak == ' Tatt/skadet av ukjent rovdyr'] <- 'Tatt/skadet av ukjent rovdyr'
      
      data <- data[data$søye != 'ukjent art',]
      data <- data[data$søye != 'gaupe',]
      data <- data[data$søye != 'klostridiebakterier',]
      
      
      kopplam_korrigering <- which(data$lam == '' & data$oppvekstmelding == 'Kopplam')
      
      for (i in 1:length(kopplam_korrigering)){
        if (data$lam[kopplam_korrigering[i]-1] != '') {
          data$oppvekstmelding[kopplam_korrigering[i]-1] <- 'Kopplam'
        }
      }
      
      for (i in 1:length(kopplam_korrigering)){
        if (data$lam[kopplam_korrigering[i]-2] != '' & data$lam[kopplam_korrigering[i]-1] == '') {
          data$oppvekstmelding[kopplam_korrigering[i]-2] <- 'Kopplam'
        }
      }
      
      data$oppvekstmelding[which(data$søye == 'Fosterlam')-1] <- paste0(data$oppvekstmelding[which(data$søye == 'Fosterlam')-1], 'Fosterlam')
      data <- data[data$søye != 'Fosterlam',]
      data <- data[data$søye != 'diagnose',]
      data <- data[data$søye != 'Kopplam',]
      data <- data[data$søye != 'mild,moderat,alvorlig',]
      data <- data[data$søye != 'rovdyr, ukjent art',]
      data <- data[data$søye[data$søye != ''] != data$utmelding[data$søye != ''],]
      
      data(data)
      ## stats
      søyer <- length(unique(data$søye[data$søye != '']))
      data$søye[data$søye == ''] <- NA
      data$søye <- na.locf(data$søye)
      
      # endre sommerbeite utmark til en string check for Tapt sommerbeite - for å kunne fange opp både innmark og utmark
      grep('Tapt sommerbeite', data$utmelding)
      
      nrow(data[data$tapsårsak == ' Tatt/skadet av gaupe' & grep('Tapt sommerbeite', data$utmelding),])
      
      
      gaupe_lam <- sum(grep('Tatt/skadet av gaupe', data$tapsårsak)[grep('Tatt/skadet av gaupe', data$tapsårsak) %in% grep('5', data$lam)] %in% grep('Tapt sommerbeite', data$utmelding))
      jerv_lam <- sum(grep('Tatt/skadet av jerv', data$tapsårsak)[grep('Tatt/skadet av jerv', data$tapsårsak) %in% grep('5', data$lam)] %in% grep('Tapt sommerbeite', data$utmelding))
      ørn_lam <- sum(grep('Tatt/skadet av ørn', data$tapsårsak)[grep('Tatt/skadet av ørn', data$tapsårsak) %in% grep('5', data$lam)] %in% grep('Tapt sommerbeite', data$utmelding))
      rev_lam <- sum(grep('Tatt/skadet av rev', data$tapsårsak)[grep('Tatt/skadet av rev', data$tapsårsak) %in% grep('5', data$lam)] %in% grep('Tapt sommerbeite', data$utmelding))
      ukjent_rovvilt_lam <- sum(grep('Tatt/skadet av ukjent rovdyr', data$tapsårsak)[grep('Tatt/skadet av ukjent rovdyr', data$tapsårsak) %in% grep('5', data$lam)] %in% grep('Tapt sommerbeite', data$utmelding))
      ukjent_lam <- sum(grep('Ukjent årsak', data$tapsårsak)[grep('Ukjent årsak', data$tapsårsak) %in% grep('5', data$lam)] %in% grep('Tapt sommerbeite', data$utmelding))
      
      gaupe_søye <- sum(grep('Tatt/skadet av gaupe', data$tapsårsak)[grep('Tatt/skadet av gaupe', data$tapsårsak) %in% grep('5', data$lam, invert = T)] %in% grep('Tapt sommerbeite', data$utmelding))
      jerv_søye <- sum(grep('Tatt/skadet av jerv', data$tapsårsak)[grep('Tatt/skadet av jerv', data$tapsårsak) %in% grep('5', data$lam, invert = T)] %in% grep('Tapt sommerbeite', data$utmelding))
      ørn_søye <- sum(grep('Tatt/skadet av ørn', data$tapsårsak)[grep('Tatt/skadet av ørn', data$tapsårsak) %in% grep('5', data$lam, invert = T)] %in% grep('Tapt sommerbeite', data$utmelding))
      rev_søye <- sum(grep('Tatt/skadet av rev', data$tapsårsak)[grep('Tatt/skadet av rev', data$tapsårsak) %in% grep('5', data$lam, invert = T)] %in% grep('Tapt sommerbeite', data$utmelding))
      ukjent_rovvilt_søye <- sum(grep('Tatt/skadet av ukjent rovdyr', data$tapsårsak)[grep('Tatt/skadet av ukjent rovdyrk', data$tapsårsak) %in% grep('5', data$lam, invert = T)] %in% grep('Tapt sommerbeite', data$utmelding))
      ukjent_søye <- sum(grep('Ukjent årsak', data$tapsårsak)[grep('Ukjent årsak', data$tapsårsak) %in% grep('5', data$lam, invert = T)] %in% grep('Tapt sommerbeite', data$utmelding))
      
      gaupe_totalt <- gaupe_lam + gaupe_søye
      jerv_totalt <- jerv_lam + jerv_søye
      ørn_totalt <- ørn_lam + ørn_søye
      rev_totalt <- rev_lam + rev_søye
      ukjent_rovvilt_totalt <- ukjent_rovvilt_lam + ukjent_rovvilt_søye
      ukjent_totalt <- ukjent_lam + ukjent_søye
      
      samlet_rovvilt <- gaupe_totalt + jerv_totalt + ørn_totalt + rev_totalt + ukjent_rovvilt_totalt
      
      # må lage basert på unique
      
      length(unique(data$lam[data$lam != '']))
      
      lam <- length(unique(data$lam[data$lam != '']))
      kopplam <- nrow(data[data$oppvekstmelding == 'Kopplam',])
      fosterlam <- nrow(data[data$oppvekstmelding == 'Fosterlam',])
      
      totalt_antall <- lam + søyer
      
      søye_med_lam <- length(unique(data$søye[data$lam != '']))
      søye_uten_lam <- søyer - søye_med_lam
      
      #length(unique(data$søye[nchar(data$søye) > 2]))- length(unique(data$søye[data$lam != ''])) # søye utmeldt og null
      
      beskrivelse <- c('Tapt eller skadet lam av gaupe', 'Tapt eller skadet søye av gaupe', 'Tapt eller skadet totalt av gaupe','',
                       'Tapt eller skadet lam av jerv', 'Tapt eller skadet søye av jerv', 'Tapt eller skadet totalt av jerv','',
                       'Tapt eller skadet lam av ørn', 'Tapt eller skadet søye av ørn','Tapt eller skadet totalt av ørn','',
                       'Tapt eller skadet lam av rev', 'Tapt eller skadet søye av rev', 'Tapt eller skadet totalt av rev','',
                       'Tapt eller skadet lam av ukjent rovvilt', 'Tapt eller skadet søye av ukjent rovvilt','Tapt eller skadet totalt av ukjent rovvilt','',
                       'Totalt tap eller skadet av rovvilt', 
                       'Tap eller skadet lam av ukjent årsak', 'Tap eller skadet søye av ukjent årsak','',
                       'Totalt tapt eller skadd av ukjent årsak', 
                       '','Antall totalt', 'Antall søyer med lam', 'Antall søyer uten lam','Antall lam', 'Antall kopplam', 'Antall fosterlam')
      
      
      antall <- c(gaupe_lam, gaupe_søye, gaupe_totalt,'',
                  jerv_lam, jerv_søye, jerv_totalt,'',
                  ørn_lam, ørn_søye, ørn_totalt,'',
                  rev_lam, rev_søye, rev_totalt,'',
                  ukjent_rovvilt_lam, ukjent_rovvilt_søye, ukjent_rovvilt_totalt,'',
                  samlet_rovvilt, 
                  ukjent_lam, ukjent_søye, ukjent_totalt,'',
                  '',totalt_antall, søye_med_lam, søye_uten_lam,lam, kopplam, fosterlam)
      
      stats <- data.frame(beskrivelse, antall)
      stats(stats) }     
      
    else {data('')}

    })
    
    
    
    
    output$stats <- renderPrint({
      print(stats())
    })
    
    output$preview <- renderPrint({ 

    print(data())
    
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        #  paste(input$saksnr, ".csv", sep = "")
        paste(input$saksnr, ".xlxs", sep = "")
      },
      content = function(file) {
        #          write.csv2(data, file, row.names = FALSE, fileEncoding = 'UTF-8')
    #    wb <- createWorkbook()
    #    writeData(wb, sheet = 1, x = data(), startCol = 'A', startRow = 1)
    #    writeData(wb, sheet = 2, x = stats(), startCol = 'H', startRow = 1)
    #    write.xlxs(wb, file = paste(input$saksnr, ".xlxs", sep = ""))
        write.xlsx(data(), file)
        test <- list(data(), stats())
        write.xlsx(test, file, asTable = T)
      }
    )  
  })      
  
  
}
shinyApp(ui, server)
