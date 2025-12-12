Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

library(shiny)
library(zoo)
library(openxlsx)


ui <- fluidPage(
  
  verbatimTextOutput('saksnummer'),
  textInput("saksnr", "Saksnr", ""),
  textAreaInput('rapport', 'Rapport', ''),
  downloadButton("downloadData", "Download"),
  verbatimTextOutput('stats'),
  verbatimTextOutput('preview')
  
)



server <- function(input, output) {

    data <- reactiveVal()
    stats <- reactiveVal()
    observeEvent(input$rapport,{
      lam_siffer <- substr(Sys.Date(), start = 4, stop = 4)
      
      if (input$saksnr != ''){
        lam_siffer <- substr(input$saksnr, start = 4, stop = 4)
      }
      
      if (isTruthy(input$rapport)){
      #        data <- read.csv(input$file1$datapath, fileEncoding = 'UTF-8', sep = input$sep)
      data <- read.csv(text=input$rapport, fileEncoding = 'UTF-8', sep = ';')
      data<-  data.frame(unlist(strsplit(data$Søye.Lam.Helse.Utmelding.Oppvekstkode, '(?<=.)(?=../../..)', perl = T)))
      data <- data.frame(unlist(regmatches(data$unlist.strsplit.data.Søye.Lam.Helse.Utmelding.Oppvekstkode.., regexpr(' ', data$unlist.strsplit.data.Søye.Lam.Helse.Utmelding.Oppvekstkode..), invert = T)))
      colnames(data) <- c('søye')
      data$dato <- ''
      data$utmelding <- ''
      data$dato[grep('../../..',data$søye)] <- data$søye[grep('../../..',data$søye)]
      data$dato <- gsub(':', '', data$dato)
      data$utmelding[grep('../../..',data$søye)] <- data$søye[grep('../../..',data$søye)+1]
      data$utmelding[grep('Foreb[.]', data$utmelding)] <- '766 - Foreb flercellede parasitter (eks rundorm)' # fjerne . i melding for å splitte på . senere
      data$tapsårsak <- ''
      data$tapsårsak <- gsub('^.+[\\.]', '', data$utmelding)
      data$utmelding <- gsub('\\..*', '', data$utmelding)
      data$tapsårsak[data$tapsårsak == data$utmelding] <- ''
      data$tapsårsak[data$tapsårsak == 'Kopplam' | data$tapsårsak == 'Fosterlam' | data$tapsårsak == ' Kopplam' | data$tapsårsak == ' Fosterlam'] <- ''
      data$oppvekstmelding <- ''
      data$oppvekstmelding[grep('Kopplam', data$søye) -1] <- 'Kopplam'
      data$oppvekstmelding[grep('Fosterlam', data$søye) -1] <- 'Fosterlam'
      
      
      data$lam <- ''
      data$lam[substr(data$søye,1,1) == lam_siffer] <- data$søye[substr(data$søye,1,1) == lam_siffer] # and nchars(data$søye) == 5
      data$lam[grep(' ', data$lam)] <- ''
      
      data <- data[,c('søye','lam', 'dato', 'utmelding','tapsårsak', 'oppvekstmelding')]
      data$søye[data$søye == data$lam] <- ''
      

      
      # korrigering gaupe, ukjent art, rovdyr ukjent art
      rovdyr_ukjentart_korrigering <- which(data$søye == 'rovdyr,' & data$tapsårsak == '')
      ukjentart_korrigering <- which(data$søye == 'ukjentart' & data$tapsårsak == '')
      gaupe_korrigering <- which(data$søye == 'gaupe' & data$tapsårsak == '')
      
      if (length(rovdyr_ukjentart_korrigering) > 0){
        for (i in 1:length(rovdyr_ukjentart_korrigering)){
          if (data$tapsårsak[rovdyr_ukjentart_korrigering[i]-1] == ' Tatt/skadet av') {
            data$tapsårsak[rovdyr_ukjentart_korrigering[i]-1] <- 'Tatt/skadet av ukjent rovdyr'
          }
          
          if (data$tapsårsak[rovdyr_ukjentart_korrigering[i]-2] == ' Tatt/skadet av') {
            data$tapsårsak[rovdyr_ukjentart_korrigering[i]-2] <- 'Tatt/skadet av ukjent rovdyr'
          }
        }
      }
      
      if (length(ukjentart_korrigering) > 0){
        for (i in 1:length(rovdyr_ukjentart_korrigering)){
          if (data$tapsårsak[ukjentart_korrigering[i]-1] == ' Tatt/skadet av') {
            data$tapsårsak[ukjentart_korrigering[i]-1] <- 'Tatt/skadet av ukjent rovdyr'
          }
          
          if (data$tapsårsak[ukjentart_korrigering[i]-2] == ' Tatt/skadet av') {
            data$tapsårsak[ukjentart_korrigering[i]-2] <- 'Tatt/skadet av ukjent rovdyr'
          }
        }
      }
      
      if (length(gaupe_korrigering) > 0){
        for (i in 1:length(gaupe_korrigering)){
          if (data$tapsårsak[gaupe_korrigering[i]-1] == ' Tatt/skadet av') {
            data$tapsårsak[gaupe_korrigering[i]-1] <- 'Tatt/skadet av gaupe'
          }
          
          if (data$tapsårsak[gaupe_korrigering[i]-2] == ' Tatt/skadet av') {
            data$tapsårsak[gaupe_korrigering[i]-2] <- 'Tatt/skadet av gaupe'
          }
        }
      }
      
      
      data$tapsårsak[data$tapsårsak == ' Tatt/skadet av rovdyr, ukjent art'] <- 'Tatt/skadet av ukjent rovdyr'
      data$tapsårsak[data$tapsårsak == ' Tatt/skadet av ukjent rovdyr'] <- 'Tatt/skadet av ukjent rovdyr'
      
      
      
      
      kopplam_korrigering <- which((data$lam == '' & data$dato == '') & data$oppvekstmelding == 'Kopplam')
      fosterlam_korrigering <- which((data$lam == '' & data$dato == '') & data$oppvekstmelding == 'Fosterlam')
      
      if (length(kopplam_korrigering) > 0){
        for (i in 1:length(kopplam_korrigering)){
          if (data$lam[kopplam_korrigering[i]-1] != '' | data$dato[kopplam_korrigering[i]-1] != '') {
            data$oppvekstmelding[kopplam_korrigering[i]-1] <- 'Kopplam'
          }
        }
        
        
        for (i in 1:length(kopplam_korrigering)){
          if ((data$lam[kopplam_korrigering[i]-2] != '' | data$dato[kopplam_korrigering[i]-2] != '' ) & (data$lam[kopplam_korrigering[i]-1] == '' | data$dato[kopplam_korrigering[i]-1] == '')) {
            data$oppvekstmelding[kopplam_korrigering[i]-2] <- 'Kopplam'
          }
        }
        
        for (i in 1:length(kopplam_korrigering)){
          if ((data$lam[kopplam_korrigering[i]-3] != '' | data$dato[kopplam_korrigering[i]-3] != '' ) & (data$lam[kopplam_korrigering[i]-2] == '' | data$dato[kopplam_korrigering[i]-2] == '')) {
            data$oppvekstmelding[kopplam_korrigering[i]-3] <- 'Kopplam'
          }
        }    
      }
      
      if (length(fosterlam_korrigering) > 0){
        for (i in 1:length(fosterlam_korrigering)){
          if (data$lam[fosterlam_korrigering[i]-1] != '' | data$dato[fosterlam_korrigering[i]-1] != '') {
            data$oppvekstmelding[fosterlam_korrigering[i]-1] <- 'Fosterlam'
          }
        }
        
        
        for (i in 1:length(fosterlam_korrigering)){
          if ((data$lam[fosterlam_korrigering[i]-2] != '' | data$dato[fosterlam_korrigering[i]-2] != '' ) & (data$lam[fosterlam_korrigering[i]-1] == '' | data$dato[fosterlam_korrigering[i]-1] == '')) {
            data$oppvekstmelding[fosterlam_korrigering[i]-2] <- 'Fosterlam'
          }
        }
        
        for (i in 1:length(fosterlam_korrigering)){
          if ((data$lam[fosterlam_korrigering[i]-3] != '' | data$dato[fosterlam_korrigering[i]-3] != '' ) & (data$lam[fosterlam_korrigering[i]-2] == '' | data$dato[fosterlam_korrigering[i]-2] == '')) {
            data$oppvekstmelding[fosterlam_korrigering[i]-3] <- 'Fosterlam'
          }
        }    
      }
      

      
      data$søye <- sub("\\D*(\\d+).*", "\\1", data$søye)
      data$søye[nchar(data$søye) < 4] <- ''
      data$søye <- gsub("[^0-9.-]", "", data$søye)
      data$søye <- gsub("[.]", "", data$søye)
      data$søye <- gsub("[-]", "", data$søye)
      
      data <- data[data$søye != '' | data$lam != '' | data$dato != '' | data$utmelding != '',]
      
      # bytte ut dette med alt som ikke er nummer
      data <- data[data$søye != 'ukjent art',]
      data <- data[data$søye != 'gaupe',]
      data <- data[data$søye != 'klostridiebakterier',]
      data <- data[data$søye != 'Fosterlam',]
      data <- data[data$søye != 'diagnose',]
      data <- data[data$søye != 'Kopplam',]
      data <- data[data$søye != 'mild,moderat,alvorlig',]
      data <- data[data$søye != 'rovdyr, ukjent art',]
      data <- data[data$søye != 'Fravendt:',]
      data <- data[data$søye[data$søye != ''] != data$utmelding[data$søye != ''],]
      
      data <- data[data$søye != '' | data$lam != '' | data$dato != '' | data$utmelding != '',]
      
      data$tapsårsak <- gsub(' Kopplam', '', data$tapsårsak)
      data$tapsårsak <- gsub(' Fosterlam', '', data$tapsårsak)
      
      # bytte ut dette med alt som ikke er nummer
      data <- data[data$søye != 'ukjent art',]
      data <- data[data$søye != 'gaupe',]
      data <- data[data$søye != 'klostridiebakterier',]
      data <- data[data$søye != 'Fosterlam',]
      data <- data[data$søye != 'diagnose',]
      data <- data[data$søye != 'Kopplam',]
      data <- data[data$søye != 'mild,moderat,alvorlig',]
      data <- data[data$søye != 'rovdyr, ukjent art',]
      data <- data[data$søye != 'Fravendt:',]
      data <- data[data$søye[data$søye != ''] != data$utmelding[data$søye != ''],]
      
      data(data)
      ## stats
      søyer <- length(unique(data$søye[data$søye != '']))
      data$søye[data$søye == ''] <- NA
      
      data$søye <- na.locf(data$søye)

      
      #for søye:
      data$lam[data$lam == ''] <- NA
      #data$lam <- na.locf(data$lam)
      data$lam <- transform(data, value = ave(lam, søye, FUN = na.locf0))$value
      
      #søye med og uten lam
      
      data$temp <- 0
      data$temp[!is.na(data$lam)] <- 1
      
      temp <- aggregate(data$temp, by = list(data$søye), mean)
      temp$x[temp$x > 0] <- 1
      colnames(temp) <- c('søye', 'mor')
      data <- merge(data, temp, by='søye')
      
      
      data$lam[is.na(data$lam)] <- ''
      
      
      ## finne de med mer enn 3 lam
      
      #temp <- aggregate(data$temp[data$lam %in%unique(data$lam)], by = list(data$søye[data$lam %in%unique(data$lam)]), sum)
      temp <-aggregate(data$temp, by = list(data$søye, data$lam), mean)
      temp <- aggregate(temp$x, by=list(temp$Group.1), sum)
      colnames(temp) <- c('søye', 'antall lam')
      data <- merge(data, temp, by = 'søye')
      
      temp <- aggregate(data$`antall lam`, by = list(data$søye), mean)
      temp <- temp[temp$x >= 3,]
      antall_lam_i_tre_pluss_kull <- sum(temp$x)
      
      # antall lam 
      lam <- length(unique(data$lam[data$lam != '']))
      kopplam <- length(unique(data$lam[data$oppvekstmelding == 'Kopplam']))
      fosterlam <- length(unique(data$lam[data$oppvekstmelding == 'Fosterlam']))
      
      totalt_antall <- lam + søyer
      
      #sum antall kopplam per søye. Beregn antall søsken av kopplam ved antall lam per søe - kopplam per søe
      data$kopplam_dummy <- 0
      data$kopplam_dummy[grep('Kopplam', data$oppvekstmelding)] <- 1
      
      temp <- aggregate(data$kopplam_dummy, by=list(data$søye), sum)
      colnames(temp) <- c('søye', 'antall_kopplam_søye')
      
      data <- merge(data, temp, by=c('søye'))
      
      # Ettåringer - problemer med tellingen
      
      data$ettåring <- 0
      data$ettåring[substr(data$søye,1,1) == as.numeric(lam_siffer) -1] <- 1
      antall_ettåringer <- sum(aggregate(data$ettåring, by=list(data$søye), mean)[2])
      
      data$lam_ettåring <- 0
      data$lam_ettåring[data$temp == 1 & data$ettåring == 1] <- 1
      antall_lam_ettåring <- sum(aggregate(data$lam_ettåring, by=list(data$søye,data$lam), mean)[3])
      
      ## Kaster ut de som er slaktet for å regne ut tap
      data <- data[grep('Slaktet', data$utmelding, invert = T),]
      
      # sykdom
      
      data$sykdom <- 0
      data$sykdom[grep('Hold', data$tapsårsak)] <- 1
      data$sykdom[grep('Sjukdom', data$tapsårsak)] <- 1
      data$sykdom[grep('Mastitt', data$tapsårsak)] <- 1
      data$sykdom[grep('flercellede', data$tapsårsak)] <- 1
      
      sykdom_søye <- sum(data$sykdom[data$lam == '' & data$mor == 1])
      sykdom_søye_u_lam <- sum(data$sykdom[grep('0', data$mor)])
      sykdom_lam <- sum(data$sykdom[grep(lam_siffer, data$lam)])
      
      # ulykke
      
      data$ulykke <- 0
      data$ulykke[grep('Ulykke', data$tapsårsak)] <- 1
      data$ulykke[grep('tråkk', data$tapsårsak)] <- 1
      
      ulykke_søye <- sum(data$ulykke[data$lam == '' & data$mor == 1])
      ulykke_søye_u_lam <- sum(data$ulykke[grep('0', data$mor)])
      ulykke_lam <- sum(data$ulykke[grep(lam_siffer, data$lam)])
      
      # dupliserer for å regne ut tap på sommerbeite
      data_sommer <- data[grep('sommer', data$utmelding),]
      
      
      # sykdom sommer
      
      sykdom_søye_sommer <- sum(data_sommer$sykdom[data_sommer$lam == '' & data_sommer$mor == 1])
      sykdom_søye_u_lam_sommer <- sum(data_sommer$sykdom[grep('0', data_sommer$mor)])
      sykdom_lam_sommer <- sum(data_sommer$sykdom[grep(lam_siffer, data_sommer$lam)])
      
      # ulykke
      
      ulykke_søye_sommer <- sum(data_sommer$ulykke[data_sommer$lam == '' & data_sommer$mor == 1])
      ulykke_søye_u_lam_sommer <- sum(data_sommer$ulykke[grep('0', data_sommer$mor)])
      ulykke_lam_sommer <- sum(data_sommer$ulykke[grep(lam_siffer, data_sommer$lam)])
      
      # regne ut tap
      data$gaupe <- 0
      data$gaupe[grep('Tatt/skadet av gaupe', data$tapsårsak)] <- 1
      data$jerv <- 0
      data$jerv[grep('Tatt/skadet av jerv', data$tapsårsak)] <- 1
      data$bjørn <- 0
      data$bjørn[grep('Tatt/skadet av bjørn', data$tapsårsak)] <- 1
      data$ulv <- 0
      data$ulv[grep('Tatt/skadet av ulv', data$tapsårsak)] <- 1
      data$ørn <- 0
      data$ørn[grep('Tatt/skadet av ørn', data$tapsårsak)] <- 1
      data$rev <- 0
      data$rev[grep('Tatt/skadet av rev', data$tapsårsak)] <- 1
      data$ukjent_rovvilt <- 0
      data$ukjent_rovvilt[grep('Tatt/skadet av ukjent rovdyr', data$tapsårsak)] <- 1
      data$ukjent <- 0
      data$ukjent[grep('Ukjent årsak', data$tapsårsak)] <- 1
      
      
      gaupe_lam <- length(grep('Tatt/skadet av gaupe', data$tapsårsak)[grep('Tatt/skadet av gaupe', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      jerv_lam <- length(grep('Tatt/skadet av jerv', data$tapsårsak)[grep('Tatt/skadet av jerv', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      bjørn_lam <- length(grep('Tatt/skadet av bjørn', data$tapsårsak)[grep('Tatt/skadet av bjørn', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      ulv_lam <- length(grep('Tatt/skadet av ulv', data$tapsårsak)[grep('Tatt/skadet av ulv', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      ørn_lam <- length(grep('Tatt/skadet av ørn', data$tapsårsak)[grep('Tatt/skadet av ørn', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      rev_lam <- length(grep('Tatt/skadet av rev', data$tapsårsak)[grep('Tatt/skadet av rev', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      ukjent_rovvilt_lam <- length(grep('Tatt/skadet av ukjent rovdyr', data$tapsårsak)[grep('Tatt/skadet av ukjent rovdyr', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      ukjent_lam <- length(grep('Ukjent årsak', data$tapsårsak)[grep('Ukjent årsak', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      
      gaupe_lam_sommer <- length(grep('Tatt/skadet av gaupe', data_sommer$tapsårsak)[grep('Tatt/skadet av gaupe', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam)])
      jerv_lam_sommer <- length(grep('Tatt/skadet av jerv', data_sommer$tapsårsak)[grep('Tatt/skadet av jerv', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam)])
      bjørn_lam_sommer <- length(grep('Tatt/skadet av bjørn', data_sommer$tapsårsak)[grep('Tatt/skadet av bjørn', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam)])
      ulv_lam_sommer <- length(grep('Tatt/skadet av ulv', data_sommer$tapsårsak)[grep('Tatt/skadet av ulv', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam)])
      ørn_lam_sommer <- length(grep('Tatt/skadet av ørn', data_sommer$tapsårsak)[grep('Tatt/skadet av ørn', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam)])
      rev_lam_sommer <- length(grep('Tatt/skadet av rev', data$tapsårsak)[grep('Tatt/skadet av rev', data$tapsårsak) %in% grep(lam_siffer, data$lam)])
      ukjent_rovvilt_lam_sommer <- length(grep('Tatt/skadet av ukjent rovdyr', data_sommer$tapsårsak)[grep('Tatt/skadet av ukjent rovdyr', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam)])
      ukjent_lam_sommer <- length(grep('Ukjent årsak', data_sommer$tapsårsak)[grep('Ukjent årsak', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam)])
      
      
      gaupe_søye <- sum(grep('Tatt/skadet av gaupe', data$tapsårsak)[grep('Tatt/skadet av gaupe', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('1', data$mor))
      jerv_søye <- sum(grep('Tatt/skadet av jerv', data$tapsårsak)[grep('Tatt/skadet av jerv', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('1', data$mor))
      bjørn_søye <- sum(grep('Tatt/skadet av bjørn', data$tapsårsak)[grep('Tatt/skadet av bjørn', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('1', data$mor))
      ulv_søye <- sum(grep('Tatt/skadet av ulv', data$tapsårsak)[grep('Tatt/skadet av ulv', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('1', data$mor))
      ørn_søye <- sum(grep('Tatt/skadet av ørn', data$tapsårsak)[grep('Tatt/skadet av ørn', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('1', data$mor))
      rev_søye <- sum(grep('Tatt/skadet av rev', data$tapsårsak)[grep('Tatt/skadet av rev', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('1', data$mor))
      ukjent_rovvilt_søye <- sum(grep('Tatt/skadet av ukjent rovdyr', data$tapsårsak)[grep('Tatt/skadet av ukjent rovdyrk', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('1', data$mor))
      ukjent_søye <- sum(grep('Ukjent årsak', data$tapsårsak)[grep('Ukjent årsak', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('1', data$mor))
      
      gaupe_søye_sommer <- sum(grep('Tatt/skadet av gaupe', data_sommer$tapsårsak)[grep('Tatt/skadet av gaupe', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('1', data_sommer$mor))
      jerv_søye_sommer <- sum(grep('Tatt/skadet av jerv', data_sommer$tapsårsak)[grep('Tatt/skadet av jerv', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('1', data_sommer$mor))
      bjørn_søye_sommer <- sum(grep('Tatt/skadet av bjørn', data_sommer$tapsårsak)[grep('Tatt/skadet av bjørn', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('1', data_sommer$mor))
      ulv_søye_sommer <- sum(grep('Tatt/skadet av ulv', data_sommer$tapsårsak)[grep('Tatt/skadet av ulv', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('1', data_sommer$mor))
      ørn_søye_sommer <- sum(grep('Tatt/skadet av ørn', data_sommer$tapsårsak)[grep('Tatt/skadet av ørn', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('1', data_sommer$mor))
      rev_søye_sommer <- sum(grep('Tatt/skadet av rev', data_sommer$tapsårsak)[grep('Tatt/skadet av rev', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('1', data_sommer$mor))
      ukjent_rovvilt_søye_sommer <- sum(grep('Tatt/skadet av ukjent rovdyr', data_sommer$tapsårsak)[grep('Tatt/skadet av ukjent rovdyrk', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('1', data_sommer$mor))
      ukjent_søye_sommer <- sum(grep('Ukjent årsak', data_sommer$tapsårsak)[grep('Ukjent årsak', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('1', data_sommer$mor))
      
      
      gaupe_søye_u_lam <- sum(grep('Tatt/skadet av gaupe', data$tapsårsak)[grep('Tatt/skadet av gaupe', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('0', data$mor))
      jerv_søye_u_lam <- sum(grep('Tatt/skadet av jerv', data$tapsårsak)[grep('Tatt/skadet av jerv', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('0', data$mor))
      bjørn_søye_u_lam <- sum(grep('Tatt/skadet av bjørn', data$tapsårsak)[grep('Tatt/skadet av bjørn', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('0', data$mor))
      ulv_søye_u_lam <- sum(grep('Tatt/skadet av ulv', data$tapsårsak)[grep('Tatt/skadet av ulv', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('0', data$mor))
      ørn_søye_u_lam <- sum(grep('Tatt/skadet av ørn', data$tapsårsak)[grep('Tatt/skadet av ørn', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('0', data$mor))
      rev_søye_u_lam <- sum(grep('Tatt/skadet av rev', data$tapsårsak)[grep('Tatt/skadet av rev', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('0', data$mor))
      ukjent_rovvilt_søye_u_lam <- sum(grep('Tatt/skadet av ukjent rovdyr', data$tapsårsak)[grep('Tatt/skadet av ukjent rovdyrk', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('0', data$mor))
      ukjent_søye_u_lam <- sum(grep('Ukjent årsak', data$tapsårsak)[grep('Ukjent årsak', data$tapsårsak) %in% grep(lam_siffer, data$lam, invert = T)] %in% grep('0', data$mor))
      
      gaupe_søye_u_lam_sommer <- sum(grep('Tatt/skadet av gaupe', data_sommer$tapsårsak)[grep('Tatt/skadet av gaupe', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('0', data_sommer$mor))
      jerv_søye_u_lam_sommer <- sum(grep('Tatt/skadet av jerv', data_sommer$tapsårsak)[grep('Tatt/skadet av jerv', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('0', data_sommer$mor))
      bjørn_søye_u_lam_sommer <- sum(grep('Tatt/skadet av bjørn', data_sommer$tapsårsak)[grep('Tatt/skadet av bjørn', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('0', data_sommer$mor))
      ulv_søye_u_lam_sommer <- sum(grep('Tatt/skadet av ulv', data_sommer$tapsårsak)[grep('Tatt/skadet av ulv', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('0', data_sommer$mor))
      ørn_søye_u_lam_sommer <- sum(grep('Tatt/skadet av ørn', data_sommer$tapsårsak)[grep('Tatt/skadet av ørn', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('0', data_sommer$mor))
      rev_søye_u_lam_sommer <- sum(grep('Tatt/skadet av rev', data_sommer$tapsårsak)[grep('Tatt/skadet av rev', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('0', data_sommer$mor))
      ukjent_rovvilt_søye_u_lam_sommer <- sum(grep('Tatt/skadet av ukjent rovdyr', data_sommer$tapsårsak)[grep('Tatt/skadet av ukjent rovdyrk', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('0', data_sommer$mor))
      ukjent_søye_u_lam_sommer <- sum(grep('Ukjent årsak', data_sommer$tapsårsak)[grep('Ukjent årsak', data_sommer$tapsårsak) %in% grep(lam_siffer, data_sommer$lam, invert = T)] %in% grep('0', data_sommer$mor))
      
      
      gaupe_totalt <- gaupe_lam + gaupe_søye + gaupe_søye_u_lam
      jerv_totalt <- jerv_lam + jerv_søye + jerv_søye_u_lam
      bjørn_totalt <- bjørn_lam + bjørn_søye + bjørn_søye_u_lam
      ulv_totalt <- ulv_lam + ulv_søye + ulv_søye_u_lam
      ørn_totalt <- ørn_lam + ørn_søye + ørn_søye_u_lam
      rev_totalt <- rev_lam + rev_søye + rev_søye_u_lam
      ukjent_rovvilt_totalt <- ukjent_rovvilt_lam + ukjent_rovvilt_søye + ukjent_søye_u_lam
      ukjent_totalt <- ukjent_lam + ukjent_søye + ukjent_søye_u_lam
      
      samlet_rovvilt <- gaupe_totalt + jerv_totalt + bjørn_totalt + ulv_totalt + ørn_totalt + rev_totalt + ukjent_rovvilt_totalt
      
      fredet_rovvilt_lam <- gaupe_lam + jerv_lam + bjørn_lam + ulv_lam + ørn_lam
      fredet_rovvilt_søye <- gaupe_søye + jerv_søye + bjørn_søye + ulv_søye + ørn_søye
      fredet_rovvilt_søye_u_lam <- gaupe_søye_u_lam + jerv_søye_u_lam + bjørn_søye_u_lam + ulv_søye_u_lam + ørn_søye_u_lam
      
      fredet_rovvilt_lam_sommer <- gaupe_lam_sommer + jerv_lam_sommer + bjørn_lam_sommer + ulv_lam_sommer + ørn_lam_sommer
      fredet_rovvilt_søye_sommer <- gaupe_søye_sommer + jerv_søye_sommer + bjørn_søye_sommer + ulv_søye_sommer + ørn_søye_sommer
      fredet_rovvilt_søye_u_lam_sommer <- gaupe_søye_u_lam_sommer + jerv_søye_u_lam_sommer + bjørn_søye_u_lam_sommer + ulv_søye_u_lam_sommer + ørn_søye_u_lam_sommer
      
      data$fredet_rovvilt <- data$gaupe + data$jerv + data$bjørn + data$ulv + data$ørn
      
      # antall søyer med og uten lam
      søye_med_lam <- sum(aggregate(data$mor, by=list(data$søye), mean)[2])
      søye_uten_lam <- nrow(aggregate(data$mor, by=list(data$søye), mean)[2]) - søye_med_lam
      
      
      # annen kjent årsak
      
      ## Tap annet
      annet_lam <- rev_lam + ukjent_lam + ukjent_rovvilt_lam + sykdom_lam +ulykke_lam
      annet_søye <- rev_søye + ukjent_søye + ukjent_rovvilt_søye + sykdom_søye + ulykke_søye
      annet_søye_u_lam <- rev_søye_u_lam + ukjent_søye_u_lam + ukjent_rovvilt_søye_u_lam + sykdom_søye_u_lam + ulykke_søye_u_lam
      
      annet_lam_sommer <- rev_lam_sommer + ukjent_lam_sommer + ukjent_rovvilt_lam_sommer + sykdom_lam_sommer +ulykke_lam_sommer
      annet_søye_sommer <- rev_søye_sommer + ukjent_søye_sommer + ukjent_rovvilt_søye_sommer + sykdom_søye_sommer + ulykke_søye_sommer
      annet_søye_u_lam_sommer <- rev_søye_u_lam_sommer + ukjent_søye_u_lam_sommer + ukjent_rovvilt_søye_u_lam_sommer + sykdom_søye_u_lam_sommer + ulykke_søye_u_lam_sommer
      
      data$annet <- data$rev + data$ukjent + data$ukjent_rovvilt + data$sykdom + data$ulykke
      
      data$tapt <- data$fredet_rovvilt + data$annet
      
      data$tapt[grep('Tapt sommerbeite', data$utmelding)] <- 1
      data$tapt[grep('Tapt høstbeite', data$utmelding)] <- 1
      data$tapt[grep('Tapt vårbeite', data$utmelding)] <- 1
      
      data$kopplam_tapt<- 0
      data$kopplam_tapt[data$tapt == 1 & data$kopplam_dummy == 1] <- 1
      
      antall_kopplam_tapt <- length(unique(data$lam[data$oppvekstmelding == 'Kopplam' & data$tapt == 1]))
      
      temp <- aggregate(data$kopplam_tapt, by=list(data$søye), sum)
      colnames(temp) <- c('søye', 'kopplam_tapt_per_søye')
      data <- merge(data, temp, by=c('søye'))
      
      
      data$lam_tapt <- 0
      data$lam_tapt[data$temp == 1 & data$tapt == 1] <- 1
      
      temp <- aggregate(data$lam_tapt, by=list(data$søye), sum)
      colnames(temp) <- c('søye', 'lam_tapt_per_søye')
      data <- merge(data, temp, by=c('søye'))
      
      data$søsken_kopplam_tapt <- 0
      data$søsken_kopplam_tapt <- data$lam_tapt_per_søye - data$kopplam_tapt_per_søye
      data$søsken_kopplam_tapt[data$antall_kopplam_søye < 1] <- 0
      
      antall_søsken_tapt <- sum(aggregate(data$søsken_kopplam_tapt, by=list(data$søye), sum)[2])
      
      data$ettåring_tapt <- 0
      data$ettåring_tapt[data$tapt == 1 & data$ettåring == 1 & data$lam == ''] <- 1
      
      antall_ettåring_tapt <- sum(aggregate(data$ettåring_tapt, by=list(data$søye), sum)[2])
      
      data$lam_ettåring_tapt <- 0
      data$lam_ettåring_tapt[data$tapt == 1 & data$lam_ettåring == 1] <- 1
      
      temp <- aggregate(data$lam_ettåring_tapt, by=list(data$søye, data$lam), sum)
      temp <- aggregate(temp$x, by=list(temp$Group.1), sum)
      colnames(temp) <- c('søye', 'antall_lam_tapt_ettåring_søye')
      data <- merge(data, temp, by=c('søye'))
      
      antall_lam_tapt_ettåring <- sum(aggregate(data$antall_lam_tapt_ettåring_søye, by=list(data$søye), mean)[2])
      
      antall_lam_i_tre_pluss_kull_tapt <- sum(data$tapt[data$`antall lam` >= 3])
      
      beskrivelse <- c('Tapt morsøye - gaupe', 'Tapt søye u/lam - gaupe','Tapt lam - gaupe','',
                       'Tapt morsøye - jerv', 'Tapt søye u/lam - jerv','Tapt lam - jerv', '',
                       'Tapt morsøye - bjørn', 'Tapt søye u/lam - bjørn','Tapt lam - bjørn', '',
                       'Tapt morsøye - ulv', 'Tapt søye u/lam - ulv','Tapt lam - ulv', '',
                       'Tapt morsøye - ørn', 'Tapt søye u/lam - ørn','Tapt lam - ørn', '',
                       'Tapt morsøye - fredet rovvilt', 'Tapt søye u/lam - fredet rovvilt','Tapt lam - fredet rovvilt', '',
                       'Tapt morsøye - annet', 'Tapt søye u/lam - annet','Tapt lam - annet', '',
                       'Tapt morsøye - rev', 'Tapt søye u/lam - rev','Tapt lam - rev', '',
                       'Tapt morsøye - sykdom', 'Tapt søye u/lam - sykdom','Tapt lam - sykdom', '',
                       'Tapt morsøye - ulykke', 'Tapt søye u/lam - ulykke','Tapt lam - ulykke', '',
                       'Tapt morsøye - ukjent rovvilt', 'Tapt søye u/lam - ukjent rovvilt','Tapt lam - ukjent rovvilt', '',
                       'Tap morsøye - ukjent årsak','Tapt søye u/lam - ukjent årsak', 'Tap lam - ukjent årsak', '',
                       '','Antall sau totalt', 'Antall søyer med lam', 'Antall søyer uten lam','Antall lam', 'Antall kopplam', 'Antall fosterlam', '',
                       'antall lam i 3+ kull', 'antall lam i 3+ kull tapt', '',
                       'Antall kopplam tapt', 'Antall søsken av kopplam tapt', '',
                       'Antall ettåringer', 'Antall ettåringer tapt', 'Antall lam ettåringer', 'Antall lam av ettåringer tapt')
      
      
      antall <- c(gaupe_søye, gaupe_søye_u_lam,gaupe_lam, '',
                  jerv_søye, jerv_søye_u_lam, jerv_lam,'',
                  bjørn_søye, bjørn_søye_u_lam, bjørn_lam,'',
                  ulv_søye, ulv_søye_u_lam, ulv_lam,'',
                  ørn_søye, ørn_søye_u_lam, ørn_lam, '',
                  fredet_rovvilt_søye, fredet_rovvilt_søye_u_lam, fredet_rovvilt_lam, '',
                  annet_søye, annet_søye_u_lam, annet_lam, '',
                  rev_søye, rev_søye_u_lam, rev_lam, '',
                  sykdom_søye, sykdom_søye_u_lam, sykdom_lam, '',
                  ulykke_søye, ulykke_søye_u_lam, ulykke_lam, '',
                  ukjent_rovvilt_søye, ukjent_rovvilt_søye_u_lam, ukjent_rovvilt_lam, '',
                  ukjent_søye, ukjent_søye_u_lam, ukjent_lam, '',
                  '',totalt_antall, søye_med_lam, søye_uten_lam,lam, kopplam, fosterlam, '',
                  antall_lam_i_tre_pluss_kull, antall_lam_i_tre_pluss_kull_tapt, '',
                  antall_kopplam_tapt, antall_søsken_tapt, '',
                  antall_ettåringer, antall_ettåring_tapt, antall_lam_ettåring, antall_lam_tapt_ettåring)
      
      sommer <- c(gaupe_søye_sommer, gaupe_søye_u_lam_sommer,gaupe_lam_sommer, '',
                  jerv_søye_sommer, jerv_søye_u_lam_sommer, jerv_lam_sommer,'',
                  bjørn_søye_sommer, bjørn_søye_u_lam_sommer, bjørn_lam_sommer,'',
                  ulv_søye_sommer, ulv_søye_u_lam_sommer, ulv_lam_sommer,'',
                  ørn_søye_sommer, ørn_søye_u_lam_sommer, ørn_lam_sommer, '',
                  fredet_rovvilt_søye_sommer, fredet_rovvilt_søye_u_lam_sommer, fredet_rovvilt_lam_sommer, '',
                  annet_søye_sommer, annet_søye_u_lam_sommer, annet_lam_sommer, '',
                  rev_søye_sommer, rev_søye_u_lam_sommer, rev_lam_sommer, '',
                  sykdom_søye_sommer, sykdom_søye_u_lam_sommer, sykdom_lam_sommer, '',
                  ulykke_søye_sommer, ulykke_søye_u_lam_sommer, ulykke_lam_sommer, '',
                  ukjent_rovvilt_søye_sommer, ukjent_rovvilt_søye_u_lam_sommer, ukjent_rovvilt_lam_sommer, '',
                  ukjent_søye_sommer, ukjent_søye_u_lam_sommer, ukjent_lam_sommer, '',
                  '','', '', '','', '', '', '',
                  '', '', '',
                  '', '', '',
                  '', '','', '')
      
      
      stats <- data.frame(beskrivelse, antall, sommer)
      stats(stats) }     
      
    else {data('')}

    })
    
    
    
    
    output$stats <- renderPrint({
      print(stats())
    }, width = '200')
    
    output$preview <- renderPrint({ 

    print(data())
    
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$saksnr, ".xlxs", sep = "")
      },
      content = function(file) {
        write.xlsx(data(), file)
        test <- list(data(), stats())
        write.xlsx(test, file, asTable = T)
      }
    )  
  })      
  
  
}
shinyApp(ui, server)
