pacman::p_load(colourpicker,crosstalk,plotly,aws.s3,readxl,leaflet.extras,reactable,reactablefmtr,shinyjs,leaflegend,leaflet,googleway,tippy,shinyWidgets,htmltools,janitor,fresh,tidyverse,shiny,DT,shinythemes,bslib,waiter,stringr,paletteer,waiter,highcharter,readr,bs4Dash,shinydashboard,shinydashboardPlus,reshape2)


`%!in%` <- Negate(`%in%`) 


#Sys.setenv()

# p_moni despachos do dia 
# ordens de serviço com filtro de ativos são os despachos do momento

# ACESSO
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAZXAS4GOKVFKH4TUQ",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" ="sa-east-1",
           TZ="America/Sao_Paulo")



relacoes =  readxl::read_xlsx("Relação_precos_materiais.xlsx") %>%
  clean_names() 

csvDownloadButton <- function(id, filename = "data.csv", label = "Download em CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

info <- \(texto){
  tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = texto,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    class = "pull-right"
  )
}


server <- function(input, output, session) {
  waiter_hide()
  
  # ACESSO
  Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAZXAS4GOKVFKH4TUQ",
             "AWS_SECRET_ACCESS_KEY" = "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",
             "AWS_DEFAULT_REGION" ="sa-east-1",
             TZ="America/Sao_Paulo")
  shinyjs::runjs(
    "function reload_page() {
  window.location.reload();
  setTimeout(reload_page, 3000000);
 }
 setTimeout(reload_page, 3000000);
 ")
  # CARDS
  hc_theme_sparkline_vb <- function(...) {
    
    theme <- list(
      chart = list(
        backgroundColor = NULL,
        margins = c(0, 0, 0, 0),
        spacingTop = 0,
        spacingRight = 0,
        spacingBottom = 0,
        spacingLeft = 0,
        plotBorderWidth = 0,
        borderWidth = 0,
        style = list(overflow = "visible")
      ),
      xAxis = list(
        visible = FALSE, 
        endOnTick = FALSE, 
        startOnTick = FALSE
      ),
      yAxis = list(
        visible = FALSE,
        endOnTick = FALSE, 
        startOnTick = FALSE
        
      ),
      tooltip = list(
        botderWidth = 0,
        useHTML  =TRUE,
        table=T,
        #backgroundColor = "#ffffff",
        headerFormat = "<b>{point.key}</b><br>" ,
        pointFormat = '<b>Total </b>: <b> {point.y} ({point.p}%)</b></td>',
        style = list(textOutline = "0px white",
                     fontSize = "15px",
                     fontWeight= 'bold')
      ),
      plotOptions = list(
        series = list(
          marker = list(enabled = FALSE),
          lineWidth = 2,
          shadow = FALSE,
          fillOpacity = 0.25,
          color = "#ffffff",
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.00, "#FFFFFF00"),
              list(0.50, "#FFFFFF7F"),
              list(1.00, "#FFFFFFFF")
            )
          )
        )
      ),
      credits = list(
        enabled = FALSE,
        text = ""
      )
    )
    
    theme <- structure(theme, class = "hc_theme")
    
    if (length(list(...)) > 0) {
      theme <- hc_theme_merge(
        theme,
        hc_theme(...)
      )
    }
    
    theme
  }  
  
  # DADOS ----
  atendimentos <- reactive({
    
    #invalidateLater(30000, session=session)
    
    s3read_using(FUN = readxl::read_xlsx,
                 object = "atendimentos.xlsx",
                 bucket = "automacao-conecta"
    ) %>% 
      clean_names() %>% 
      mutate(data_atendimento = as.Date(data_atendimento,"%d/%m/%Y"))%>% 
      rename(lat=latitude_total_ponto,lon=longitude_total_ponto,equipe = desc_equipe)  %>% 
      mutate(semana_marco = week(data_atendimento)-week(as.Date("2023-02-25")),
             mes = month(data_atendimento),
             mes = case_when(
               mes == 3 ~ "Março",
               mes == 4 ~ "Abril",
               mes == 5 ~ "Maio",
               mes == 6 ~ "Junho",
               mes == 7 ~ "Julho",
               mes == 8 ~ "Agosto"
             ),
             mes = factor(mes,levels = c("Março","Abril","Maio","Junho","Julho","Agosto")),
             lat = as.numeric(lat),
             lon = as.numeric(lon)) %>% 
      filter(atendimento %!in% c("MOD: RETRABALHO","MOD: Atendido")) %>% 
      replace_na(list(motivo = "Encontrado normal",tipo_de_ocorrencia = "Não informado")) %>% 
      mutate(hora = hms(hora_inicio)) %>%
      mutate(data_hora = case_when(
        hora <= hms("06:00:00") ~ data_atendimento-1,
        TRUE ~ data_atendimento
      ),
      dia_semana = wday(data_hora,label = T),
      dia_semana = case_when(
        dia_semana %in% c("dom","Sun") ~ "Dom",
        dia_semana %in% c("seg","Mon") ~ "Seg",
        dia_semana %in% c("ter","Tue") ~ "Ter",
        dia_semana %in% c("qua","Wed") ~ "Qua",
        dia_semana %in% c("qui","Thu") ~ "Qui",
        dia_semana %in% c("sex","Fri") ~ "Sex",
        dia_semana %in% c("sab","Sat") ~ "Sab"
        
      ),
      semana = week(data_hora) - week(floor_date(data_hora,"month")) +1)
  })
  solicitacoes <- reactive({
    #invalidateLater(30000, session=session)
    
    s3read_using(FUN = readxl::read_xlsx,
                 object = "solicitacoes.xlsx",
                 bucket = "automacao-conecta"
    ) %>% 
      clean_names() %>% 
      mutate(data_reclamacao = as.Date(data_reclamacao,"%d/%m/%Y"),
             semana_marco = week(data_reclamacao)-week(as.Date("2023-02-25")),
             mes = month(data_reclamacao),
             mes = case_when(
               mes == 3 ~ "Março",
               mes == 4 ~ "Abril",
               mes == 5 ~ "Maio",
               mes == 6 ~ "Junho",
               mes == 7 ~ "Julho",
               mes == 8 ~ "Agosto"
             ),
             mes = factor(mes,levels = c("Março","Abril","Maio","Junho","Julho","Agosto")),
             dia_semana = wday(data_reclamacao,label = T),
             dia_semana = case_when(
               dia_semana %in% c("dom","Sun") ~ "Dom",
               dia_semana %in% c("seg","Mon") ~ "Seg",
               dia_semana %in% c("ter","Tue") ~ "Ter",
               dia_semana %in% c("qua","Wed") ~ "Qua",
               dia_semana %in% c("qui","Thu") ~ "Qui",
               dia_semana %in% c("sex","Fri") ~ "Sex",
               dia_semana %in% c("sab","Sat") ~ "Sab"
               
             ),
             semana = week(data_reclamacao) - week(floor_date(data_reclamacao,"month")) +1) 
  })
  p_moni <- reactive({
    #invalidateLater(30000, session=session)
    
    s3read_using(FUN = readxl::read_xlsx,
                 object = "painel_monitoramento.xlsx",
                 bucket = "automacao-conecta"
    ) %>% 
      clean_names() %>% 
      mutate(
        #recebida =  as.POSIXct(strptime(recebida,"%d/%m/%Y %H:%M")),
        data_limite =as.POSIXct(strptime(data_limite_para_atendimento,"%d/%m/%Y %H:%M")),
        dif = as.numeric(round(difftime(data_limite, as.POSIXct(Sys.time(),"GMT"),units = "hours"),0)),
        data_reclamacao = as.Date(data_reclamacao,"%d/%m/%Y"),
        data_limite_atendimento = as.Date(data_limite_atendimento,"%d/%m/%Y"),
        dias_prazo = as.numeric(data_limite_atendimento - Sys.Date()),
        atrasado = ifelse(dias_prazo < 0, "Atrasada","No Prazo")
      ) %>% 
      rename(lat=latitude_total,lon=longitude_total,protocolo=numero_protocolo) %>% 
      mutate(
        cor_atraso = case_when(
          dias_prazo >= 0 ~ "darkgreen",
          TRUE ~ "red"
        ))
  })
  p_oc <- reactive({
    #invalidateLater(30000, session=session)
    
    s3read_using(FUN = readxl::read_xlsx,
                 object = "painel_ocorrencias.xlsx",
                 bucket = "automacao-conecta"
    ) %>% 
      clean_names() %>% 
      mutate(
        data_limite_para_atendimento = limite_atendimento,
        #recebida =  as.POSIXct(strptime(recebida,"%d/%m/%Y %H:%M")),
        data_limite =as.POSIXct(strptime(limite_atendimento,"%d/%m/%Y %H:%M")),
        dif = as.numeric(round(difftime(data_limite, as.POSIXct(Sys.time(),"GMT"),units = "hours"),0)),
        data_reclamacao = as.Date(data_reclamacao,"%d/%m/%Y"),
        data_limite_atendimento = as.Date(data_limite_atendimento,"%d/%m/%Y"),
        dias_prazo = as.numeric(data_limite_atendimento - Sys.Date()),
        atrasado = ifelse(dias_prazo < 0, "Atrasada","No Prazo")) %>% 
      rename(lat=latitude_total,lon=longitude_total)  %>% 
      mutate(
        cor_atraso = case_when(
          dias_prazo >= 0 ~ "darkgreen",
          TRUE ~ "red"
        ))
  })
  prazo <- reactive({
    
    #invalidateLater(30000, session=session)
    
    s3read_using(FUN = readxl::read_xlsx,
                 object = "sgi_atendimento_atendimentos_prazo.xlsx",
                 bucket = "automacao-conecta"
    ) %>% 
      clean_names() %>% 
      #select(-x1) %>% 
      rename(prazo_hora = x4,atendimento_hora = x6) %>% 
      mutate(prazo = as.Date(prazo,"%d/%m/%Y"),
             data_atendimento = as.Date(data_atendimento,"%d/%m/%Y"),
             mes = month(data_atendimento),
             mes = case_when(
               mes == 3 ~ "Março",
               mes == 4 ~ "Abril",
               mes == 5 ~ "Maio",
               mes == 6 ~ "Junho",
               mes == 7 ~ "Julho",
               mes == 8 ~ "Agosto"
             ),
             mes = factor(mes,levels = c("Março","Abril","Maio","Junho","Julho","Agosto")),
             hora = hms(atendimento_hora),
             data_hora = case_when(
               hora <= hms("06:00:00") ~ data_atendimento-1,
               TRUE ~ data_atendimento
             ),
             dia_semana = wday(data_hora,label = T),
             dia_semana = case_when(
               dia_semana %in% c("dom","Sun") ~ "Dom",
               dia_semana %in% c("seg","Mon") ~ "Seg",
               dia_semana %in% c("ter","Tue") ~ "Ter",
               dia_semana %in% c("qua","Wed") ~ "Qua",
               dia_semana %in% c("qui","Thu") ~ "Qui",
               dia_semana %in% c("sex","Fri") ~ "Sex",
               dia_semana %in% c("sab","Sat") ~ "Sab"
               
             ),
             atendimento = as.character(atendimento)
      )  %>%  mutate(atendimento = as.character(atendimento)) %>% 
      left_join(atendimentos() %>% select(no_atendimento,equipe), by = c("atendimento" = "no_atendimento")) 
    
  })
  materiais_aplicados <- reactive({
    #invalidateLater(30000, session=session)
    
    s3read_using(FUN = readxl::read_xlsx,
                 object = "materiais_aplicados.xlsx",
                 bucket = "automacao-conecta"
    ) %>% 
      clean_names() %>% 
      filter(codigo %in% unique(relacoes$codigo)) %>% 
      mutate(data = as.Date(data,"%d/%m/%Y"),
             hora = hms(hora_inicial),
             data_hora = case_when(
               hora <= hms("06:00:00") ~ data-1,
               TRUE ~ data
             ))  
  }) 
  dados_materias <- reactive({
    
    materiais_aplicados() %>% 
      left_join(relacoes %>% select(-descricao),by = c("codigo")) %>% 
      mutate(quantidade = as.numeric(quantidade),
             gasto_total = abs(quantidade*valor_unitario)) %>% 
      rename("n_atendimento"="atendimento")
  })
  ocorrencias_atrasadas <- reactive({
    
    p_moni() %>% 
      filter(atrasado == "Atrasada") %>% 
      select(protocolo,data_reclamacao,data_limite_atendimento,dias_prazo,atrasado,tipo_de_ocorrencia,bairro,lat,lon,endereco) %>% 
      rbind(
        p_oc() %>% 
          filter(atrasado == "Atrasada") %>% 
          select(protocolo,data_reclamacao,data_limite_atendimento,dias_prazo,atrasado,tipo_de_ocorrencia,bairro,lat,lon,endereco) 
      )  
  }) 
  atendimentos_avulsos <- reactive({
    prazo() %>% 
      filter(data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() },
             status == "Avulso") %>%
      select(no_atendimento = atendimento) %>% 
      left_join(atendimentos() %>% 
                  filter(data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }) %>% 
                  select(no_atendimento,equipe,lat,lon,nome_bairro,endereco),
                by = c("no_atendimento")) %>% 
      select("Nº Atendimento" = no_atendimento,"Equipe" = equipe,lat,lon,"Bairro" = nome_bairro, "Endereço" = endereco) 
    
  }) 
  at_fora_escopo <- reactive({
    p_oc() %>%
      mutate( data_limite_para_atendimento = limite_atendimento,
              #recebida =  as.POSIXct(strptime(recebida,"%d/%m/%Y %H:%M")),
              data_limite =as.POSIXct(strptime(limite_atendimento,"%d/%m/%Y %H:%M")),
              dif = as.numeric(round(difftime(data_limite, as.POSIXct(Sys.time(),"GMT"),units = "hours"),0))) %>% 
      filter(tipo_de_ocorrencia %in%  c("Troca de Potência",
                                        "Necessário Poda de Árvore",
                                        "Instalar braço/luminária"
      )) %>% 
      select(Protocolo = protocolo,
             "Tipo de Ocorrência"=tipo_de_ocorrencia,
             Atrasado = atrasado,Prazo = dias_prazo,
             "Data da Reclamação" = data_reclamacao,
             "Data Limite" =data_limite_atendimento,
             "Endereço" = endereco,
             dif,data_limite) %>% 
      rbind(
        p_moni() %>% 
          mutate( data_limite =as.POSIXct(strptime(data_limite_para_atendimento,"%d/%m/%Y %H:%M")),
                  dif = as.numeric(round(difftime(data_limite, as.POSIXct(Sys.time(),"GMT"),units = "hours"),0))) %>% 
          filter(tipo_de_ocorrencia %in%  c("Troca de Potência",
                                            "Necessário Poda de Árvore",
                                            "Instalar braço/luminária")) %>% 
          select(Protocolo = protocolo,
                 "Tipo de Ocorrência"=tipo_de_ocorrencia,
                 Atrasado = atrasado,
                 Prazo = dias_prazo,
                 "Data da Reclamação" = data_reclamacao,
                 "Data Limite" =data_limite_atendimento,
                 "Endereço" = endereco,
                 dif,data_limite))
  }) 
  os <- reactive({
    s3read_using(FUN = readxl::read_xlsx,
                 object = "ordens_servico.xlsx",
                 bucket = "automacao-conecta"
    ) %>% 
      clean_names() %>% 
      mutate(data= as.POSIXct(strptime(data,"%d/%m/%Y %H:%M"))) 
  })
  ocorrencias_aturorizar <- reactive({
    s3read_using(FUN = readxl::read_xlsx,
                 object = "ocorrencias_autorizar.xlsx",
                 bucket = "automacao-conecta"
    ) %>% 
      clean_names() %>% 
      mutate(data_limite_de_atendimento_original = as.Date(data_limite_de_atendimento_original,"%d/%m/%Y"))
  })
  #----
  
  # CARDS ----
  
  output$c_total_equipes <- renderbs4ValueBox({
    
    bs4ValueBox(color = "primary",
                value = tags$p(style = "font-size: 35px;font-weight: lighter;text-align: center;",
                               "Equipes",HTML("<br>"),
                               atendimentos() %>%
                                 filter(
                                   data_hora == if(input$filtro == Sys.Date()){
                                     if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
                                   } else {
                                     input$filtro
                                   }
                                   #data_hora ==if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){print(Sys.Date()-1)} else{print(Sys.Date())}
                                 ) %>%
                                 select(equipe) %>% 
                                 distinct() %>% 
                                 nrow()),
                gradient = T,
                footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;",
                         # HTML(paste0('<span style="display: grid;font-size:15px;">',
                         #             atendimentos() %>%
                         #               filter(data_hora == Sys.Date()) %>%
                         #               select(equipe) %>% 
                         #               distinct() %>% pull() ,'</span>')),
                         info(texto = paste("Total de Equipes em campo em",input$filtro)),
                         pickerInput(inputId = "equip",
                                     label = "Lista das Equipes",
                                     choices = atendimentos() %>%
                                       filter(
                                         data_hora == if(input$filtro == Sys.Date()){
                                           if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
                                         } else {
                                           input$filtro
                                         }
                                         #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
                                       ) %>%
                                       select(equipe) %>% 
                                       distinct(),
                                     multiple = T,
                                     selected = atendimentos() %>%
                                       filter(
                                         data_hora == if(input$filtro == Sys.Date()){
                                           if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
                                         } else {
                                           input$filtro
                                         }
                                         #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
                                       ) %>%
                                       select(equipe) %>% 
                                       distinct() %>% pull(equipe),
                                     options = list(`actions-box` = T,
                                                    `selected-text-format`= "count",
                                                    `count-selected-text` = "{0}/{1} Equipes"
                                     ),
                                     width = "100%"
                         )
                         
                  )
                ,icon = icon('users',lib = "font-awesome")
    )
    
  })
  
  
  # ATENDIMENTOS 
  at_dia = reactive({
    atendimentos() %>%
      filter(
        #equipe %in% input$equip,
        data_hora == if(input$filtro == Sys.Date()){
          if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
        } else {
          input$filtro
        }
        #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
      ) %>% 
      nrow()
  })
  
  atend_prazo = reactive({
    prazo() %>% 
      filter(
        data_hora == if(input$filtro == Sys.Date()){
          if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
        } else {
          input$filtro
        }
      ) %>% 
      
      #equipe %in% input$equip,
      #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }) %>% 
      group_by(status) %>% 
      summarise(n = n()) %>% 
      filter(status %in% c("Avulso","Fora do prazo","No Prazo"))%>%
      ungroup() %>%
      mutate(p = n/sum(n),
             p = round(100*p,0))
  }) 
  atend_tipo <- reactive({
    atendimentos() %>% 
      filter(
        data_hora == if(input$filtro == Sys.Date()){
          if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
        } else {
          input$filtro
        }
        
        #equipe %in% input$equip,
        #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
      ) %>% 
      group_by(atendimento) %>% 
      summarise(n = n()) 
  })
  atend_og <- reactive({
    prazo() %>% 
      filter(
        data_hora == if(input$filtro == Sys.Date()){
          if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
        } else {
          input$filtro
        }
        
        #equipe %in% input$equip,
        #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
      ) %>%
      mutate(origem_da_ocorrencia = case_when(
        origem_da_ocorrencia == "Sem origem definida" ~ "Avulso",
        TRUE ~ origem_da_ocorrencia)) %>% 
      summarise(n=n(),.by = origem_da_ocorrencia)
  })
  atend_equipe <- reactive({
    atendimentos() %>% 
      filter(
        data_hora == if(input$filtro == Sys.Date()){
          if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
        } else {
          input$filtro
        }
        
        #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
      ) %>% 
      group_by(equipe,atendimento) %>% 
      summarise(n = n()) %>% 
      mutate(atendimento = factor(atendimento,levels = c("Impossibilidade","Encontrado normal","Atendido","Fora do escopo do contrato","Atendimento relacionado"))) %>% 
      complete(atendimento)  
  })
  prazo_equipe <- reactive({
    prazo() %>% 
      filter(
        data_hora == if(input$filtro == Sys.Date()){
          if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
        } else {
          input$filtro
        }
        
        #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
      ) %>% 
      group_by(equipe,status) %>% 
      summarise(n = n()) %>% 
      filter(status %in% c("Avulso","Fora do prazo","No Prazo"))%>%
      mutate(status = factor(status,levels = c("Avulso","No Prazo","Fora do prazo"))) %>% 
      complete(status)  
  })
  media_equipe = reactive({
    atendimentos() %>% 
      filter( 
        data_hora == if(input$filtro == Sys.Date()){
          if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
        } else {
          input$filtro
        }
        
        #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
      ) %>% 
      group_by(equipe) %>% 
      summarise(n = n()) %>% 
      summarise(media = mean(n)) %>% pull(media)
  }) 
  
  
  output$c_at <- renderbs4ValueBox({
    
    
    bs4ValueBox(color = "primary",
                
                value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;"
                               
                               #HTML("<h1 style='font-size: 70px;display:inline;'>" ,at_dia(),"</h1>"),
                               #HTML("<h3 style='display:inline;font-weight: lighter;'> Atendimentos</h3>")
                               #info(texto = paste("Total de Solicatações recebidas em",Sys.Date()))
                               
                               
                               
                               
                               
                ),
                gradient = T,footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;",
                         #info(texto = paste("Total de Atendimentos realizados em",Sys.Date())),
                         
                         fluidRow(
                           column(4,
                                  highchart() %>%  
                                    hc_yAxis(title = list(text = " ",
                                                          style = list(fontSize = '13px',
                                                                       fontWeight= 'bold')),
                                             labels = list(style = list(fontSize = '11px',
                                                                        fontWeight= 'bold')),
                                             gridLineWidth =  0) %>% 
                                    hc_xAxis(title = list(text = " "),
                                             labels = list(rotation = -0,
                                                           style = list(fontSize = '17px',
                                                                        color = "black",
                                                                        fontWeight= 'bold')),
                                             categories = list(
                                               format(input$filtro,"%d/%m/%Y"))
                                    ) |>
                                    hc_add_dependency("plugins/grouped-categories.js") %>%
                                    hc_add_series(data = atend_tipo() %>% 
                                                    filter(atendimento == "Atendido") %>%
                                                    pull(n),
                                                  type="bar",
                                                  name = "Efetivos",
                                                  color = list(
                                                    linearGradient= c( list(x1=0), list(x2=0), list(y1=0), list(y2=1)),
                                                    stops= list(
                                                      c(0, '#26a5bf'),
                                                      c(0.5,"#00586e"),
                                                      c(1, "#064354")
                                                    )
                                                  )) %>% 
                                    hc_add_series(data = atend_tipo() %>% 
                                                    filter(atendimento == "Impossibilidade") %>%
                                                    pull(n),
                                                  type="bar",
                                                  name = "Impossibilitados",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#bb0606'),
                                                      c(0.5,"#750f0f"),
                                                      c(1, "#3d1010")
                                                    )
                                                  ))  %>% 
                                    hc_add_series(data = atend_tipo() %>% filter(atendimento == "Encontrado normal") %>%  pull(n),
                                                  type="bar",
                                                  name = "Encontrado Normal",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#dda33b'),
                                                      c(0.5,"#be892a"),
                                                      c(1, "#bf7b01")
                                                    )
                                                  ))  %>% 
                                    hc_add_series(data = atend_tipo() %>% filter(atendimento == "Atendimento relacionado") %>%  pull(n),
                                                  type="bar",
                                                  name = "Relacionado",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#34b030'),
                                                      c(0.5,"#098305"),
                                                      c(1, "#043d02")
                                                    )
                                                  )) %>% 
                                    hc_add_series(data = atend_tipo() %>% filter(atendimento == "Fora do escopo do contrato") %>%  pull(n),
                                                  type="bar",
                                                  name = "Fora do Escopo",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#e06208'),
                                                      c(0.5,"#9d4506"),
                                                      c(1, "#6c3005")
                                                    )
                                                  )) %>% 
                                    hc_plotOptions(bar = list(
                                      dataLabels = list(
                                        enabled = TRUE,
                                        color = 'white',
                                        format = '{point.y} {series.name}',
                                        #shape = NULL,
                                        style = list(textOutline = NULL,
                                                     fontSize = '12px'))),
                                      series = list(
                                        #pointWidth=25,
                                        
                                        pointPadding= 0,
                                        groupPadding =0,
                                        lineWidth = 1,
                                        borderWidth = 1,
                                        borderColor = "white",
                                        dataLabels = list(enabled = TRUE,
                                                          color = "white")
                                      )
                                    ) %>% 
                                    hc_legend(enabled = F,layout= 'horizontal',itemStyle = list(color = "white", fontSize = '11px')) %>% 
                                    hc_tooltip(table = T,useHTML  =TRUE,
                                               headerFormat= "<span style='font-size:13px;'><b> Tipo de Atendimento </b></span><br>",
                                               pointFormat= "<span <span style='color:{series.color};font-size:15px'><b> {series.name}</b></span>: <b> {point.y} </b> </span><br>",
                                               #xDateFormat =  '%Y-%m-%d',
                                               
                                               style = list(fontSize = '14px')
                                               #backgroundColor = "#ffffff",
                                               #borderColor= '#000000'
                                    ) %>% 
                                    hc_title(text = "Status dos Atendimentos",style = list(color = "white",fontSize = '22px')) %>% 
                                    hc_size(height = 220) %>%   
                                    hc_add_theme(hc_theme_sparkline_vb()) %>% 
                                    hc_credits(enabled = FALSE)
                           ),
                           
                           column(4,
                                  highchart() %>%  
                                    hc_yAxis( title = list(text = " ",
                                                           style = list(fontSize = '13px',
                                                                        fontWeight= 'bold')),
                                              labels = list(style = list(fontSize = '11px',
                                                                         fontWeight= 'bold')),
                                              gridLineWidth =  0) %>% 
                                    hc_xAxis(title = list(text = " "),
                                             labels = list(rotation = -0,
                                                           style = list(fontSize = '17px',
                                                                        color = "black",
                                                                        fontWeight= 'bold')),
                                             categories = list(
                                               format(input$filtro,"%d/%m/%Y"))
                                    ) |>
                                    hc_add_dependency("plugins/grouped-categories.js") %>%
                                    hc_add_series(data = atend_prazo() %>%
                                                    filter(status == "No Prazo") %>%
                                                    pull(n),
                                                  type="column",
                                                  name = "No Prazo",
                                                  color = list(
                                                    linearGradient= c( list(x1=0), list(x2=0), list(y1=0), list(y2=1)),
                                                    stops= list(
                                                      c(0, '#26a5bf'),
                                                      c(0.5,"#00586e"),
                                                      c(1, "#064354")
                                                    )
                                                  )) %>% 
                                    hc_add_series(data = atend_prazo() %>% 
                                                    filter(status == "Avulso") %>%
                                                    pull(n),
                                                  type="column",
                                                  name = "Avulsos",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#dda33b'),
                                                      c(0.5,"#be892a"),
                                                      c(1, "#bf7b01")
                                                    )
                                                  )) %>% 
                                    hc_add_series(data = atend_prazo() %>%
                                                    filter(status == "Fora do prazo") %>%
                                                    pull(n),
                                                  type="column",
                                                  name = "Fora do Prazo",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#bb0606'),
                                                      c(0.5,"#750f0f"),
                                                      c(1, "#3d1010")
                                                    )
                                                  ))  %>% 
                                    hc_plotOptions(column = list(
                                      dataLabels = list(
                                        enabled = TRUE,
                                        color = 'white',
                                        format = '{point.y} {series.name}',
                                        #shape = NULL,
                                        style = list(textOutline = NULL,
                                                     fontSize = '12px'))),
                                      series = list(
                                        groupPadding= 0,
                                        pointPadding= 0,
                                        lineWidth = 1,
                                        borderWidth = 1,
                                        borderColor = "white",
                                        dataLabels = list(enabled = TRUE,
                                                          color = "white")
                                      )
                                    ) %>% 
                                    hc_legend(enabled=F,layout= 'horizontal',itemStyle = list(color = "white", fontSize = '11px')) %>% 
                                    hc_tooltip(table = T,useHTML  =TRUE,
                                               headerFormat= "<span style='font-size:13px;'><b> Prazo dos Atendimentos </b></span><br>",
                                               pointFormat= "<span <span style='color:{series.color};font-size:15px'><b> {series.name}</b></span>: <b> {point.y} </b> </span><br>",
                                               #xDateFormat =  '%Y-%m-%d',
                                               
                                               style = list(fontSize = '14px')
                                               #backgroundColor = "#ffffff",
                                               #borderColor= '#000000'
                                    ) %>% 
                                    hc_title(text = "Prazo dos Atendimentos",style = list(color = "white",fontSize = '22px')) %>% 
                                    hc_size(height = 220) %>%   
                                    hc_add_theme(hc_theme_sparkline_vb()) %>% 
                                    hc_credits(enabled = FALSE)
                           ),
                           column(4,
                                  highchart() %>%  
                                    hc_yAxis( title = list(text = " ",
                                                           style = list(fontSize = '13px',
                                                                        fontWeight= 'bold')),
                                              labels = list(style = list(fontSize = '11px',
                                                                         fontWeight= 'bold')),
                                              gridLineWidth =  0) %>% 
                                    hc_xAxis(title = list(text = " "),
                                             labels = list(rotation = -0,
                                                           style = list(fontSize = '17px',
                                                                        color = "black",
                                                                        fontWeight= 'bold')),
                                             categories = list(
                                               format(input$filtro,"%d/%m/%Y"))
                                    ) |>
                                    hc_add_dependency("plugins/grouped-categories.js") %>%
                                    hc_add_series(data = atend_og() %>%
                                                    filter(origem_da_ocorrencia  == "Call Center") %>%
                                                    pull(n),
                                                  type="bar",
                                                  name = "Call Center",
                                                  color = list(
                                                    linearGradient= c( list(x1=0), list(x2=0), list(y1=0), list(y2=1)),
                                                    stops= list(
                                                      c(0, '#26a5bf'),
                                                      c(0.5,"#00586e"),
                                                      c(1, "#064354")
                                                    )
                                                  )) %>% 
                                    hc_add_series(data = atend_og() %>% 
                                                    filter(origem_da_ocorrencia == "Avulso") %>%
                                                    pull(n),
                                                  type="bar",
                                                  name = "Avulsos",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#dda33b'),
                                                      c(0.5,"#be892a"),
                                                      c(1, "#bf7b01")
                                                    )
                                                  )) %>% 
                                    hc_add_series(data = atend_og() %>%
                                                    filter(origem_da_ocorrencia == "Ronda própria") %>%
                                                    pull(n),
                                                  type="bar",
                                                  name = "Ronda",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#34b030'),
                                                      c(0.5,"#098305"),
                                                      c(1, "#043d02")
                                                    )
                                                  ))  %>% 
                                    hc_add_series(data = atend_og() %>%
                                                    filter(origem_da_ocorrencia == "Prefeitura") %>%
                                                    pull(n),
                                                  type="bar",
                                                  name = "Prefeitura",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#7c3202'),
                                                      c(0.5,"#6a2a02"),
                                                      c(1, "#3e1901")
                                                    )
                                                  )) %>% 
                                    hc_add_series(data = atend_og() %>%
                                                    filter(origem_da_ocorrencia == "App") %>%
                                                    pull(n),
                                                  type="bar",
                                                  name = "App",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#02a890'),
                                                      c(0.5,"#016e5e"),
                                                      c(1, "#01463c")
                                                    )
                                                  )) %>% 
                                    hc_add_series(data = atend_og() %>%
                                                    filter(origem_da_ocorrencia == "Internet") %>%
                                                    pull(n),
                                                  type="bar",
                                                  name = "Internet",
                                                  color = list(
                                                    linearGradient= c( list(x1=0),
                                                                       list(x2=0),
                                                                       list(y1=0),
                                                                       list(y2=1)),
                                                    stops= list(
                                                      c(0, '#03a148'),
                                                      c(0.5,"#037535"),
                                                      c(1, "#01461f")
                                                    )
                                                  )) %>% 
                                    hc_plotOptions(bar = list(
                                      dataLabels = list(
                                        enabled = TRUE,
                                        color = 'white',
                                        format = '{point.y} {series.name}',
                                        #shape = NULL,
                                        style = list(textOutline = NULL,
                                                     fontSize = '12px'))),
                                      series = list(
                                        groupPadding= 0,
                                        pointPadding= 0,
                                        lineWidth = 1,
                                        borderWidth = 1,
                                        borderColor = "white",
                                        dataLabels = list(enabled = TRUE,
                                                          color = "white")
                                      )
                                    ) %>% 
                                    hc_legend(enabled=F,layout= 'horizontal',itemStyle = list(color = "white", fontSize = '11px')) %>% 
                                    hc_tooltip(table = T,useHTML  =TRUE,
                                               headerFormat= "<span style='font-size:13px;'><b> Prazo dos Atendimentos </b></span><br>",
                                               pointFormat= "<span <span style='color:{series.color};font-size:15px'><b> {series.name}</b></span>: <b> {point.y} </b> </span><br>",
                                               #xDateFormat =  '%Y-%m-%d',
                                               
                                               style = list(fontSize = '14px')
                                               #backgroundColor = "#ffffff",
                                               #borderColor= '#000000'
                                    ) %>% 
                                    hc_title(text = "Origem dos Atendimentos",style = list(color = "white",fontSize = '22px')) %>% 
                                    hc_size(height = 220) %>%   
                                    hc_add_theme(hc_theme_sparkline_vb()) %>% 
                                    hc_credits(enabled = FALSE)
                           )
                           
                         )
                         
                         
                         
                  )
                
                
    )
    
  })
  output$c_at_valor <- renderbs4ValueBox({
    
    
    bs4ValueBox(color = "primary",
                
                value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;",
                               
                               HTML("<h1 style='font-size: 55px;display:inline;'>" ,at_dia(),"</h1>"),
                               HTML("<h3 style='font-size: 30px;display:inline;font-weight: lighter;'> Atendimentos</h3>")
                               
                ),
                gradient = T,footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  
                  
                  tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;",
                  )
                
                ,icon = icon('ok',lib = "glyphicon")
                
    )
    
  })
  
  output$c_at_equipe <- renderbs4ValueBox({
    bs4ValueBox(color = "primary",
                
                value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;"
                               
                ),
                gradient = T,footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;",
                         highchart() %>%  
                           hc_yAxis(title = list(text = " ",
                                                 style = list(fontSize = '13px',
                                                              fontWeight= 'bold')),
                                    labels = list(style = list(fontSize = '11px',
                                                               fontWeight= 'bold')),
                                    gridLineWidth =  0,
                                    plotLines = list(
                                      list(
                                        value = media_equipe(),
                                        label = list(text = "Média",
                                                     align = "right",
                                                     
                                                     fontColor = "white",
                                                     rotation = 0),
                                        color = "white",
                                        dashStyle = "dash",
                                        width = 1)),
                                    stackLabels= list(
                                      enabled= TRUE,
                                      style = list(
                                        textOutline= 'none',
                                        color = "white",
                                        fontSize = "14px"
                                      )
                                      
                                    )
                           ) %>% 
                           hc_xAxis(title = list(text = " "),
                                    labels = list(rotation = -0,
                                                  style = list(fontSize = '13px',
                                                               color = "white",
                                                               fontWeight= 'bold')),
                                    categories =  if(length(unique(atend_equipe()$equipe)) == 1)
                                      list(unique(atend_equipe()$equipe)) else
                                        unique(atend_equipe()$equipe),
                                    labels = list(style = list(fontSize = "13px"))
                           ) |>
                           hc_add_dependency("plugins/grouped-categories.js") %>%
                           hc_add_series(data = atend_equipe() %>% 
                                           filter(atendimento == "Fora do escopo do contrato") %>%
                                           pull(n),
                                         type="bar",
                                         name = "Fora do Escopo",
                                         color = list(
                                           linearGradient= c( list(x1=0),
                                                              list(x2=0),
                                                              list(y1=0),
                                                              list(y2=1)),
                                           stops= list(
                                             c(0, '#e06208'),
                                             c(0.5,"#9d4506"),
                                             c(1, "#6c3005")
                                           )
                                         )
                           ) %>%
                           hc_add_series(data = atend_equipe() %>% 
                                           filter(atendimento == "Atendimento relacionado") %>%
                                           pull(n),
                                         type="bar",
                                         name = "Relacionado",
                                         color = list(
                                           linearGradient= c( list(x1=0),
                                                              list(x2=0),
                                                              list(y1=0),
                                                              list(y2=1)),
                                           stops= list(
                                             c(0, '#34b030'),
                                             c(0.5,"#098305"),
                                             c(1, "#043d02")
                                           )
                                         )
                           ) %>%
                           hc_add_series(data = atend_equipe() %>% 
                                           filter(atendimento == "Encontrado normal") %>%
                                           pull(n),
                                         type="bar",
                                         name = "Encontrado Normal",
                                         color = list(
                                           linearGradient= c( list(x1=0),
                                                              list(x2=0),
                                                              list(y1=0),
                                                              list(y2=1)),
                                           stops= list(
                                             c(0, '#d4ca0b'),
                                             c(0.5,"#b29b00"),
                                             c(1, "#736404")
                                           )
                                         )) %>%
                           hc_add_series(data = atend_equipe() %>% 
                                           filter(atendimento == "Impossibilidade") %>%
                                           pull(n),
                                         type="bar",
                                         name = "Impossibilitados",
                                         color = list(
                                           linearGradient= c( list(x1=0),
                                                              list(x2=0),
                                                              list(y1=0),
                                                              list(y2=1)),
                                           stops= list(
                                             c(0, '#bb0606'),
                                             c(0.5,"#750f0f"),
                                             c(1, "#3d1010")
                                           )
                                         )) %>%
                           hc_add_series(data = atend_equipe() %>% 
                                           filter(atendimento == "Atendido") %>%
                                           pull(n),
                                         type="bar",
                                         name = "Efetivos",
                                         color = list(
                                           linearGradient= c( list(x1=0),
                                                              list(x2=0),
                                                              list(y1=0),
                                                              list(y2=1)),
                                           stops= list(
                                             c(0, '#26a5bf'),
                                             c(0.5,"#00586e"),
                                             c(1, "#064354")
                                           )
                                         ))%>% 
                           hc_legend(enabled = T,layout= 'horizontal',itemStyle = list(color = "white", fontSize = '11px')) %>% 
                           hc_plotOptions(
                             bar = list(stacking= "normal",
                                        dataLabels = list(
                                          enabled = TRUE,
                                          color = 'white',
                                          format = '{point.y} ',
                                          shadow= F,
                                          shape = NULL,
                                          style = list(textOutline= 'none',
                                                       fontSize = '13px'))
                             ),
                             series = list(
                               dataSorting= list(
                                 enabled= T,
                                 sortKey= 'n'
                               ),
                               pointPadding= 0,
                               groupPadding =0.2,
                               lineWidth = 1,
                               shadow= F,
                               borderWidth = 1,
                               borderColor = "white",
                               dataLabels = list(enabled = TRUE,
                                                 textOutline= 'none',
                                                 color = "white"))
                           ) %>% 
                           hc_title( text = "Atendimentos por Equipe",
                                     style = list(color = "white",
                                                  useHTML = TRUE,
                                                  fontSize = '18px',
                                                  fontWeight= 'bold')) %>% 
                           hc_tooltip(table = T,
                                      useHTML  =TRUE,
                                      headerFormat= "<span style='font-size:17px;'><b>{point.key} </b></span><br>",
                                      pointFormat= "<span <span style='color:{series.color};font-size:15px'><b> {series.name}</b></span>: <b> {point.y}  </b> </span><br>",
                                      #xDateFormat =  '%Y-%m-%d',
                                      
                                      style = list(fontSize = '14px')
                                      #backgroundColor = "#ffffff",
                                      #borderColor= '#000000'
                           )# %>% 
                         #hc_size(height = 650) 
                  )
    )
  })
  
  output$c_avulso_equipe <- renderbs4ValueBox({
    bs4ValueBox(color = "primary",
                
                value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;"
                               
                ),
                gradient = T,footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;",
                         highchart() %>%  
                           hc_yAxis(title = list(text = " ",
                                                 style = list(fontSize = '13px',
                                                              fontWeight= 'bold')),
                                    labels = list(style = list(fontSize = '11px',
                                                               fontWeight= 'bold')),
                                    gridLineWidth =  0
                                    
                           ) %>% 
                           hc_xAxis(title = list(text = " "),
                                    labels = list(rotation = -0,
                                                  style = list(fontSize = '13px',
                                                               color = "white",
                                                               fontWeight= 'bold')),
                                    categories =  if(length(unique(atend_equipe()$equipe)) == 1)
                                      list(unique(atend_equipe()$equipe)) else
                                        unique(atend_equipe()$equipe),
                                    labels = list(style = list(fontSize = "13px"))
                           ) |>
                           hc_add_dependency("plugins/grouped-categories.js") %>%
                           hc_add_series(data = atend_equipe() %>% 
                                           filter(atendimento == "Atendido") %>%
                                           pull(n),
                                         type="bar",
                                         name = "Atendimentos Efetivos",
                                         color = list(
                                           linearGradient= c( list(x1=0),
                                                              list(x2=0),
                                                              list(y1=0),
                                                              list(y2=1)),
                                           stops= list(
                                             c(0, '#26a5bf'),
                                             c(0.5,"#00586e"),
                                             c(1, "#064354")
                                           )
                                         ))%>% 
                           hc_add_series(data = prazo_equipe() %>% 
                                           drop_na(equipe) %>% 
                                           filter(status == "Avulso") %>%
                                           pull(n),
                                         type="bar",
                                         name = "Atendimentos Avulsos",
                                         color = list(
                                           linearGradient= c( list(x1=0),
                                                              list(x2=0),
                                                              list(y1=0),
                                                              list(y2=1)),
                                           stops= list(
                                             c(0, '#dda33b'),
                                             c(0.5,"#be892a"),
                                             c(1, "#bf7b01")
                                           )
                                         )) %>% 
                           hc_legend(enabled = T,layout= 'horizontal',itemStyle = list(color = "white", fontSize = '11px')) %>% 
                           hc_plotOptions(
                             bar = list(
                               #stacking= "normal",
                               dataLabels = list(
                                 enabled = TRUE,
                                 color = 'white',
                                 format = '{point.y} ',
                                 shadow= F,
                                 shape = NULL,
                                 style = list(textOutline= 'none',
                                              fontSize = '13px'))
                             ),
                             series = list(
                               dataSorting= list(
                                 enabled= T,
                                 sortKey= 'n'
                               ),
                               lineWidth = 1,
                               pointPadding= 0,
                               groupPadding =0.2,
                               shadow= F,
                               borderWidth = 1,
                               borderColor = "white",
                               dataLabels = list(enabled = TRUE,
                                                 textOutline= 'none',
                                                 color = "white"))
                           ) %>% 
                           hc_title( text = "Atendimentos por Equipe",
                                     style = list(color = "white",
                                                  useHTML = TRUE,
                                                  fontSize = '18px',
                                                  fontWeight= 'bold')) %>% 
                           hc_tooltip(table = T,
                                      useHTML  =TRUE,
                                      headerFormat= "<span style='font-size:17px;'><b>{point.key} </b></span><br>",
                                      pointFormat= "<span <span style='color:{series.color};font-size:15px'><b> {series.name}</b></span>: <b> {point.y}  </b> </span><br>",
                                      #xDateFormat =  '%Y-%m-%d',
                                      
                                      style = list(fontSize = '14px')
                                      #backgroundColor = "#ffffff",
                                      #borderColor= '#000000'
                           )# %>% 
                         #hc_size(height = 650) 
                  )
    )
  })
  
  
  # SOLICITACOES
  solic_dia = reactive({
    solicitacoes() %>%
      filter(
        origem_ocorrencia != "Ronda própria",
        data_reclamacao ==  input$filtro 
        
        #data_reclamacao ==Sys.Date()
      ) %>% 
      nrow()
  })
  output$c_solic <- renderbs4ValueBox({
    
    
    
    bs4ValueBox(color = "primary",
                value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;", 
                               HTML("<h1 style='font-size: 55px;display:inline;'>",solic_dia(),"</h1>"),
                               HTML("<h3 style='font-size: 30px;display:inline;font-weight: lighter;'> Solicitações </h3>")
                               #info(texto = paste("Total de Solicatações recebidas em",Sys.Date()))
                ),
                gradient = T,footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;"
                         #info(texto = paste("Total de Solicatações recebidas em",Sys.Date()))
                  )
                , icon = icon('earphone',lib = "glyphicon")
                
    )
    
  })
  
  # OCORRENCIAS ATRASADAS
  output$c_oc_atrasadas <- renderbs4ValueBox({
    
    valor = reactive(nrow(ocorrencias_atrasadas()))
    
    progresso = reactive(round(100*(valor()/(sum(nrow(p_moni()),nrow(p_oc())))),1))
    
    bs4ValueBox(color = "primary",
                value = tags$p(style = "font-size: 40px;font-weight: lighter;text-align: center;",
                               "Ocorrências",
                               
                               HTML("<h1 style='font-size: 60px;display:inline;text-align:left;'>",
                                    nrow(p_moni())+nrow(p_oc()),"</h1>"),
                               HTML("<h3 style='display:inline;font-weight: lighter;'> Pendentes</h3>"),
                               HTML("<br>"),
                               HTML("<h1 style='font-size: 60px;display:inline;'>",
                                    valor(),"</h1>"),
                               HTML("<h3 style='display:inline;font-weight: lighter;'> Atrasadas</h3>"),
                               HTML("<h3 style='display:inline;font-weight: lighter;'>",paste0("(",progresso(),"%)"),"</h3>")
                               
                               
                ),
                gradient = T,
                footer = tagList(
                  bs4ProgressBar(
                    value = progresso(),
                    size = "xxs",status = "danger",animated = T,striped = T
                  )),
                subtitle = tags$p(style = "font-size: 25px;font-weight: lighter;text-align: center;"
                                  
                ),
                icon = icon('time',lib = "glyphicon")
    )
    
  })
  
  # CARD TABELA
  output$c_tab <- renderUI({
    
    # prefeitura
    sol_n <- reactive({
      solicitacoes() %>% 
        filter(origem_ocorrencia == "Prefeitura",
               status %!in%  c("Atendida","Cancelada","Em Elaboração")) %>% nrow()
    }) 
    sol_atrasada <- reactive({
      solicitacoes() %>%
        filter(origem_ocorrencia == "Prefeitura",
               status %!in%  c("Atendida","Cancelada","Em Elaboração"),
               str_detect(tempo_restante,"Vencido")) %>% nrow()
    }) 
    sol_prot <- reactive({
      solicitacoes() %>% 
        filter(origem_ocorrencia == "Prefeitura",
               status %!in%  c("Atendida","Cancelada","Em Elaboração")) %>% 
        select(protocolo) %>% 
        mutate(Tipo = "Prefeitura")
    })
    
    # vistoria
    vis_n <- reactive({
      p_oc() %>% 
        filter(str_detect(tipo_de_ocorrencia,"Vistoria")) %>% nrow()  
    })
    vis_atrasada <- reactive({
      p_oc() %>% 
        filter(str_detect(tipo_de_ocorrencia,"Vistoria"),
               atrasado == "Atrasada") %>% nrow()  
    }) 
    vis_prot <- reactive({
      p_oc() %>% 
        filter(str_detect(tipo_de_ocorrencia,"Vistoria")) %>%
        select(protocolo) %>% 
        mutate(Tipo = "Vistoria")
    })
    
    # escopo
    escopo_n <- reactive({
      at_fora_escopo() %>% nrow()
    }) 
    escopo_atrasada <- reactive({
      at_fora_escopo() %>% filter(Atrasado == "Atrasada") %>% nrow()
    }) 
    escopo_prot <- reactive({
      at_fora_escopo() %>%
        select(protocolo = Protocolo) %>% 
        mutate(Tipo = "Fora do Escopo")
    })
    
    # autorizar 
    autorizar_n <- reactive({
      ocorrencias_aturorizar() %>% nrow()
    }) 
    autorizar_atrasada <- reactive({
      ocorrencias_aturorizar() %>% filter(data_limite_de_atendimento_original < Sys.Date()) %>% nrow()
    }) 
    autorizar_prot <- reactive({
      ocorrencias_aturorizar() %>%
        select(protocolo) %>% 
        mutate(Tipo = "A Autorizar")
    })
    
    
    
    
    mini_data_oc <- reactive({
      data.frame(
        Tipo = c("Prefeitura","Vistoria","Fora do Escopo","A Autorizar"),
        Ocorrencias = c(sol_n(),vis_n(),escopo_n(),autorizar_n()),
        Atrasadas = c(sol_atrasada(),vis_atrasada(),escopo_atrasada(),autorizar_atrasada())
      ) 
    }) 
    
    dados_prot <- reactive({rbind(sol_prot(),vis_prot(),escopo_prot(),autorizar_prot())})
    
    
    bs4ValueBox(color = "primary",width = 12,
                value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;", 
                               mini_data_oc() %>% 
                                 reactable(
                                   theme =  reactableTheme(
                                     highlightColor = "rgb(24 164 255 / 10%)",
                                     #borderColor = "hsl(0, 0%, 93%)",
                                     headerStyle = list(borderColor = "hsl(0, 0%, 90%)")
                                   ),
                                   #width = 400,
                                   #theme = nytimes(),
                                   highlight = T,
                                   style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                                                backgroundColor ="rgb(255 255 255 / 0%)"),
                                   defaultColDef = colDef(align = "left",
                                                          style = list(#fontWeight = "bold",
                                                            color = "white")
                                   ), 
                                   details = function(index){
                                     
                                     dados_intra <- dados_prot()[dados_prot()$Tipo == mini_data_oc()$Tipo[index],]
                                     
                                     htmltools::div(style = "padding: 1rem",
                                                    reactable(dados_intra,
                                                              style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                                                                           backgroundColor ="rgb(255 255 255 / 0%)"),
                                                              highlight = T
                                                              
                                                    )
                                     )
                                   },
                                   bordered = F,
                                   compact = F,
                                   columns = list(
                                     Tipo = colDef(
                                       align = "left",
                                       name = "Pendência",
                                       minWidth = 100,
                                       style = list(fontSize = "1.1rem"),
                                       headerStyle = list(fontSize = "1.1rem",
                                                          #fontWeight = "bold",
                                                          color = "white")
                                     ),
                                     Atrasadas = colDef(
                                       align = "center",
                                       name = "Atrasada",
                                       minWidth = 80,
                                       style = list(fontSize = "1.1rem"),
                                       headerStyle = list(fontSize = "1.1rem",
                                                          #fontWeight = "bold",
                                                          color = "white")
                                     ),
                                     Ocorrencias = colDef(
                                       align = "center",
                                       name = "Total",
                                       minWidth = 80,
                                       style = list(fontSize = "1.1rem"),
                                       headerStyle = list(fontSize = "1.1rem",
                                                          #fontWeight = "bold",
                                                          color = "white")
                                     )
                                   )
                                 )
                               
                ),
                gradient = T,
                footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;")
                #info(texto = paste("Total de Ocorrências Abertas do Tipo Vistorias em",Sys.Date())))
                
                
    )
    
  })
  #----
  
  
  # TABELAS ----
  os_status_data <- reactive({
    os() %>% 
      filter(
        data >= if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   paste(Sys.Date(), "00:00:00") } else{  paste(Sys.Date(), "06:00:00") },
        status %in% c("Pendente", "Parcialmente Executada","Executada")
        
      ) %>% 
      #filter(status %in% c("Pendente", "Parcialmente Executada")) %>% 
      mutate(prazo = as.numeric(prazo)) %>% 
      filter(prazo > 0) %>% 
      select(id_ordem_servico,equipe,status,tarefas_finalizadas,avanco) %>% 
      mutate(avanco = as.numeric(str_remove(avanco,"%"))/100,
             status_col = case_when(
               status == "Parcialmente Executada" ~ "darkgreen",
               status == "Executada" ~ "#00586e",
               TRUE ~ "red")
      ) %>% left_join(p_moni() %>%
                        select(id_ordem_servico,atrasado) %>%
                        tabyl(id_ordem_servico,atrasado),
                      by = "id_ordem_servico")
  })
  atendimentos_realizados <- reactive({
    atendimentos() %>% 
      filter(
        data_hora == if(input$filtro == Sys.Date()){
          if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
        } else {
          input$filtro
        }
        #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
      ) %>% 
      select(n_atendimento = no_atendimento,protocolo,equipe,atendimento,tipo_de_ocorrencia,motivo,endereco,lat,lon,data_atendimento,hora_inicio,hora_conclusao) %>% 
      left_join(solicitacoes() %>% 
                  select(protocolo,origem_ocorrencia,tempo_restante,data_reclamacao)) %>% 
      mutate(cor_atendimento = case_when(
        atendimento == "Atendido" ~ "#00586e",
        atendimento == "Impossibilidade" ~ "#bb0606",
        atendimento == "Encontrado normal" ~ "#dda33b",
        atendimento == "Atendimento relacionado" ~ "#34b030",
        atendimento == "Fora do escopo do contrato"~  "#e06208" 
      )
      )%>% 
      left_join(dados_materias() %>%
                  filter(
                    data_hora == if(input$filtro == Sys.Date()){
                      if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
                    } else {
                      input$filtro
                    }
                    #data_hora == if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
                  ) %>% 
                  group_by(n_atendimento) %>% summarise(gasto = sum(gasto_total)),by = "n_atendimento")  %>% 
      replace_na(replace = list(gasto = 0,
                                origem_ocorrencia = "Avulso",
                                protocolo = "Avulso",
                                tempo_restante = "Avulso/Sem Prazo")) 
    
    
    
  })
  oc_pendentes <- reactive({
    p_oc() %>% 
      select(protocolo,tipo_de_ocorrencia,data_reclamacao,data_limite_para_atendimento,data_limite,dif,lat,lon,endereco) %>% 
      rbind(
        p_moni() %>% 
          select(protocolo,tipo_de_ocorrencia,data_reclamacao,data_limite_para_atendimento,data_limite,dif,lat,lon,endereco)
      ) %>% 
      left_join(solicitacoes()
                %>% select(protocolo,origem_ocorrencia,status,tempo_restante)
                ,by = "protocolo") %>% 
      mutate(
        #vencimento = as.numeric(round(difftime(data_limite, as.POSIXct(Sys.time(),"GMT"),units = "days"),0)),
        cor_vencimento = case_when(
          dif <= 0  ~  "darkred",
          dif > 0 & dif <= 24 ~  "red",
          dif > 24 & dif <= 48 ~  "orange",
          dif > 48 ~  "green",
        )
      ) %>% 
      relocate(dif, .after = last_col())
  })
  
  
  output$status_os <- renderUI({
    
    os_status_data() %>% 
      reactable(
        pagination = F,
        highlight = T,
        theme = reactableTheme(
          highlightColor = "rgb(24 164 255 / 10%)",
          #borderColor = "hsl(0, 0%, 93%)",
          headerStyle = list(borderColor = "hsl(0, 0%, 90%)")
        ),
        #theme = reactableTheme(highlightColor = "rgba(197,222,225,0.9305847338935574)"),
        #theme = pff(header_font_size = 15),
        striped = TRUE,
        searchable = TRUE,
        wrap = FALSE,
        resizable =TRUE,
        bordered = F,
        compact = T,
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                     backgroundColor ="rgb(255 255 255 / 0%)"),
        defaultColDef = colDef(align = "left",
                               style = list(fontWeight = "bold",
                                            color = "black")
        ),
        defaultSorted = 'avanco',
        defaultSortOrder = 'desc',
        columns = list(
          avanco = colDef(name = "Progresso",
                          headerStyle = list(fontSize = "1rem",
                                             fontWeight = "bold",
                                             color = "black"),
                          minWidth = 200,
                          format = colFormat(suffix = "dias"),
                          #cell = color_tiles(.,box_shadow = TRUE,colors = c("#ffe4cc","#750f0f", "#bb0606")),
                          cell = data_bars(.,text_size = 12,
                                           box_shadow = TRUE,
                                           max_value = 1, 
                                           fill_color = c( "#bb0606","#750f0f"),
                                           fill_gradient = TRUE,
                                           number_fmt = scales::percent)
                          #,colors = c("#ffe4cc","#750f0f", "#bb0606")
                          #style = color_scales(dados_tabela,colors = c("#ffe4cc","#750f0f", "#bb0606"))
          ),
          status = colDef(name = "Status",
                          minWidth = 155,
                          headerStyle = list(fontSize = "1rem",
                                             fontWeight = "bold",
                                             color = "black"),
                          cell = pill_buttons(.,
                                              color_ref = "status_col",
                                              opacity = 0.7)
          ),
          status_col = colDef(show = FALSE),
          id_ordem_servico = colDef(name = "Ordem Serviço",
                                    headerStyle = list(fontSize = "1rem",
                                                       fontWeight = "bold",
                                                       color = "black"),
                                    minWidth = 120,
                                    align = "center"),
          tarefas_finalizadas = colDef(name = "Tarefas",
                                       style = list(fontSize = "1.2rem",
                                                    fontWeight = "bold",
                                                    color = "black"),
                                       headerStyle = list(fontSize = "1rem",
                                                          fontWeight = "bold",
                                                          color = "black"),
                                       minWidth = 140,
                                       align = "center"),
          equipe = colDef(name = "Equipe",
                          headerStyle = list(fontSize = "1rem",
                                             fontWeight = "bold",
                                             color = "black")),
          Atrasada = colDef(name = "Atrasadas",
                            style = list(fontSize = "1.2rem",
                                         fontWeight = "bold",
                                         color = "black"),
                            headerStyle = list(fontSize = "1rem",
                                               fontWeight = "bold",
                                               color = "black"),
                            align = "center",
                            maxWidth = 95
                            
          ),
          "No Prazo" = colDef(show = F,name = "No Prazo",style = list(fontSize = "1.2rem",
                                                                      fontWeight = "bold",
                                                                      color = "black"),
                              headerStyle = list(fontSize = "1rem",
                                                 fontWeight = "bold",
                                                 color = "black"),
                              align = "center",maxWidth = 95
          )
        ),
        details = function(index){
          dados_os <- p_moni()[p_moni()$id_ordem_servico ==  os_status_data()$id_ordem_servico[index], ]   %>% 
            mutate(
              cor_atraso = case_when(
                dias_prazo >= 0 ~ "darkgreen",
                TRUE ~ "red"
              ))
          
          htmltools::div(style = "padding: 1rem",
                         reactable(dados_os %>% 
                                     select(equipe,id_ordem_servico,protocolo,atrasado,dias_prazo,data_reclamacao,data_limite_atendimento,tipo_de_ocorrencia,bairro,endereco) ,
                                   defaultSorted = 'dias_prazo',
                                   resizable =TRUE,
                                   searchable = TRUE,
                                   columns = list(
                                     dias_prazo = colDef(
                                       headerStyle = list(fontSize = ".85rem",
                                                          fontWeight = "bold",
                                                          color = "black"),
                                       cell = pill_buttons(dados_os,
                                                           color_ref = "cor_atraso",
                                                           opacity = 0.7)
                                     )
                                   )
                                   ,outlined = TRUE))
        }
      ) 
    
    
    
  })
  
  output$p_map_tab <- renderUI({
    
    
    mydrawPolylineOptions <- function (allowIntersection = TRUE, 
                                       drawError = list(color = "#b00b00", timeout = 2500), 
                                       guidelineDistance = 20, metric = TRUE, feet = FALSE, zIndexOffset = 2000, 
                                       shapeOptions = drawShapeOptions(fill = FALSE), repeatMode = FALSE) {
      leaflet::filterNULL(list(allowIntersection = allowIntersection, 
                               drawError = drawError, guidelineDistance = guidelineDistance, 
                               metric = metric, feet = feet, zIndexOffset = zIndexOffset,
                               shapeOptions = shapeOptions,  repeatMode = repeatMode)) }
    
    funcao_cor <- \(){
      c("red", "darkred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "pink", "cadetblue", "white", "gray", "lightgray", "black")
      #paste0("#", paste0(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = ""))
    }
    
    cor_equipe <- reactive({
      set.seed(9)
      sample(funcao_cor(),length(unique(p_moni()$equipe)),replace = F)
    })
    
    cor_equipe_at <- reactive({
      set.seed(21)
      sample(funcao_cor(),length(unique(atendimentos_realizados()$equipe)),replace = F)
    })
    
    
    data_cor_equipe <- reactive({
      data.frame(equipe = unique(p_moni()$equipe), cor_equipes = cor_equipe())
    })
    data_cor_equipe_at <- reactive({
      data.frame(equipe = unique(atendimentos_realizados()$equipe), cor_equipes = cor_equipe_at())
    })
    
    
    data_tabela_mapa <- reactive({
      p_moni() %>% 
        mutate(
          #vencimento = as.numeric(round(difftime(data_limite, as.POSIXct(Sys.time(),"GMT"),units = "days"),0)),
          cor_vencimento = case_when(
            dif <= 0  ~  "darkred",
            dif > 0 & dif <= 24 ~  "red",
            dif > 24 & dif <= 48 ~  "orange",
            dif > 48 ~  "green",
          )
        ) %>% 
        left_join(data_cor_equipe(),by = "equipe")
    })
    data_tabela_mapa_at <- reactive({
      atendimentos_realizados() %>% 
        left_join(data_cor_equipe_at(),by = "equipe")
    })
    
    
    icons <- reactive({
      awesomeIcons(icon = "flag",
                   iconColor = "black",
                   library = "ion",
                   markerColor =data_tabela_mapa()$cor_equipes,
                   text = ~paste0(data_tabela_mapa()$dif,"h")
      )
    })
    icons_at <- reactive({
      awesomeIcons(icon = "flag",
                   iconColor = "black",
                   library = "ion",
                   markerColor =data_tabela_mapa_at()$cor_equipes
                   #text = ~paste0(data_tabela_mapa()$dif,"h")
      )
    })
    icons_oc <- reactive({
      awesomeIcons(icon = "flag",
                   text = ~paste0(oc_pendentes()$dif,"h"),
                   iconColor = "black",
                   library = "ion",
                   markerColor = oc_pendentes()$cor_vencimento
      )
    })
    
    
    key = "AIzaSyDRrtWaFn6oOqdtBO2Balg0J8NGPAlqb2w"
    
    set_key(key = key)
    
    protocolos_share <- reactive({
      SharedData$new(data_tabela_mapa()
                     ,group = "protocolos")
    })
    protocolos_share_at <- reactive({
      SharedData$new(data_tabela_mapa_at()
                     ,group = "protocolos")
    })
    protocolos_share_oc <- reactive({
      SharedData$new(oc_pendentes()
                     ,group = "protocolos")
    })
    
    data_share <- reactive({
      data_tabela_mapa() %>% 
        select(protocolo,id_ordem_servico,equipe,tipo_de_ocorrencia,endereco,data_limite_para_atendimento,dif,cor_atraso,bairro,cor_equipes,cor_vencimento)  %>% 
        
        SharedData$new(group = "protocolos")
    })
    data_share_at <- reactive({
      data_tabela_mapa_at() %>% 
        #select(protocolo,id_ordem_servico,equipe,tipo_de_ocorrencia,endereco,data_reclamacao,data_limite_atendimento,dias_prazo,cor_atraso,bairro,cor_equipes)  %>% 
        
        SharedData$new(group = "protocolos")
    })
    data_share_oc <- reactive({
      oc_pendentes() %>% 
        SharedData$new(group = "protocolos")
    })
    
    mapa <-
      leaflet(
        protocolos_share()
      ) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng =-47.063240 ,lat = -22.907104,zoom = 11) %>% 
      addGeoJSON(jsonlite::fromJSON("municipio.geojson"),opacity = 0.05) %>% 
      addAwesomeMarkers(icon = icons(),popup = ~paste(
        "<b> Protocolo: </b>",protocolo,"<br>",
        "<b> Equipe: </b>",equipe,"<br>",
        "<b> Endereço: </b>",endereco,"<br>",
        "<b> Bairro: </b>",bairro,"<br>",
        "<b> Tipo de Ocorrência: </b>",tipo_de_ocorrencia,"<br>",
        "<b> Reclamação: </b>",data_reclamacao,"<br>",
        "<b> Limite Atendimento: </b>",data_limite,"<br>",
        "<b> Prazo: </b>",dif," horas","<br>"
      )) %>% 
      addLegend("bottomright",
                colors = data_cor_equipe()$cor_equipes,
                labels =data_cor_equipe()$equipe,
                title = "Equipes") %>% 
      addDrawToolbar(
        polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
        editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
      )  
    
    mapa_at <-
      leaflet(
        protocolos_share_at()
      ) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng =-47.063240 ,lat = -22.907104,zoom = 11) %>% 
      addGeoJSON(jsonlite::fromJSON("municipio.geojson"),opacity = 0.05) %>% 
      addAwesomeMarkers(icon = icons_at(),popup = ~paste("<b> Protocolo: </b>",protocolo,"<br>",
                                                         "<b> Equipe: </b>",equipe,"<br>",
                                                         "<b> Endereço: </b>",endereco,"<br>",
                                                         #"<b> Bairro: </b>",bairro,"<br>",
                                                         "<b> Tipo de Ocorrência: </b>",tipo_de_ocorrencia,"<br>",
                                                         "<b> Motivo: </b>",motivo,"<br>",
                                                         "<b> Origem Ocorrência: </b>",origem_ocorrencia,"<br>",
                                                         "<b> Reclamação: </b>",data_reclamacao,"<br>",
                                                         "<b> Atendimento: </b>",data_atendimento,"<br>",
                                                         "<b> Hora Inicio Atendimento: </b>",hora_inicio,"<br>",
                                                         "<b> Hora Conclusão Atendimento: </b>",hora_conclusao,"<br>",
                                                         "<b> Atendimento Quanto ao Prazo: </b>",tempo_restante,"<br>"
                                                         
                                                         
      )) %>% 
      addLegend("bottomright",
                colors = data_cor_equipe_at()$cor_equipes,
                labels =data_cor_equipe_at()$equipe,
                title = "Equipes") %>% 
      addDrawToolbar(
        polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
        editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
      ) 
    
    mapa_oc <-
      leaflet(
        protocolos_share_oc()
      ) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng =-47.063240 ,lat = -22.907104,zoom = 11) %>% 
      addGeoJSON(jsonlite::fromJSON("municipio.geojson"),opacity = 0.05) %>% 
      addAwesomeMarkers(icon = icons_oc(),popup = ~paste("<b> Protocolo: </b>",protocolo,"<br>",
                                                         "<b> Endereço: </b>",endereco,"<br>",
                                                         "<b> Tipo de Ocorrência: </b>",tipo_de_ocorrencia,"<br>",
                                                         "<b> Origem Ocorrência: </b>",origem_ocorrencia,"<br>",
                                                         "<b> Reclamação: </b>",data_reclamacao,"<br>",
                                                         "<b> Data Limite: </b>",data_limite,"<br>",
                                                         "<b> Status: </b>",status,"<br>",
                                                         "<b> Vencimento: </b>",dif," Horas","<br>"
                                                         
                                                         
                                                         
      )) %>% 
      addLegend("bottomright",
                #labFormat = labelFormat(aling = "left"),
                colors = c("darkred","red","orange","green"),
                labels = c("Vencido","Vence em 24h","Vence em 48h","Vence após 48h"),
                title = "Prazos para Vencimento") %>% 
      addDrawToolbar(
        polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
        editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
      ) 
    
    
    tbl <- data_share() %>% 
      reactable(
        pagination = F,
        height = 500,
        highlight = T,
        theme = reactableTheme( rowSelectedStyle = list(backgroundColor = "#eee",
                                                        boxShadow = "inset 2px 0 0 0 #ffa62d"),
                                highlightColor = "rgb(24 164 255 / 10%)",
                                headerStyle = list(fontWeight = "bold",
                                                   borderColor = "hsl(0, 0%, 90%)",
                                                   background = "rgb(173 194 208 / 98%)")
        ),
        #theme = nytimes(),
        #theme = reactableTheme(highlightColor = "rgba(197,222,225,0.9305847338935574)"),
        #theme = pff(header_font_size = 15),
        striped = TRUE,
        searchable = TRUE,
        wrap = FALSE,
        resizable =TRUE,
        bordered = F,
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                     backgroundColor ="rgb(255 255 255 / 0%)"),
        defaultColDef = colDef(align = "left",
                               style = list(fontWeight = "bold",
                                            color = "black")
        ),
        selection = "multiple",
        onClick = "select",
        rowStyle = list(cursor = "pointer"),
        minRows = 10,
        defaultSorted = 'dif',
        columns = list(
          #dias_prazo = colDef(name = "Prazo (dias)",
          #                    minWidth = 140,
          #  cell =  color_tiles(p_moni(),
          #                      bold_text = T,
          #                      box_shadow = T,
          #                      number_fmt = scales::label_number(accuracy = 1),
          #                      colors = RColorBrewer::brewer.pal(7, 'RdBu'))
          #    
          #),
          cor_atraso = colDef(show = FALSE),
          endereco = colDef(name = "Endereço",
                            show = F,
                            minWidth = 200,
                            resizable = T),
          tipo_de_ocorrencia = colDef(name = "Tipo de Ocorrências",
                                      resizable = T,
                                      minWidth = 100),
          equipe = colDef(name = "Equipe",minWidth = 100,
                          cell = color_tiles(data_tabela_mapa(),
                                             color_ref = "cor_equipes"
                          )
          ),
          id_ordem_servico = colDef(name = "OS",maxWidth = 80),
          protocolo = colDef(name = "Protocolo",maxWidth = 110),
          data_limite_para_atendimento = colDef(name = "Data Limite",
                                                minWidth = 120,
                                                resizable = T),
          dif = colDef(name = "Vencimento (Horas)",
                       align = "center",
                       format = colFormat(suffix = " Horas"),
                       cell =  color_tiles(data_tabela_mapa(),
                                           bold_text = T,
                                           box_shadow = T,
                                           color_ref  = "cor_vencimento",
                                           number_fmt = scales::number_format(suffix = " Horas")
                                           #colors = RColorBrewer::brewer.pal(5, 'RdBu')
                       )
          ),
          bairro = colDef(show = F),
          cor_equipes = colDef(show = F),
          cor_vencimento = colDef(show = F)
        )
      )
    tbl_at <- data_share_at() %>% 
      reactable(
        pagination = T,
        #height = 500,
        highlight = T,
        theme = reactableTheme( rowSelectedStyle = list(backgroundColor = "#eee",
                                                        boxShadow = "inset 2px 0 0 0 #ffa62d"),
                                highlightColor = "rgb(24 164 255 / 10%)",
                                headerStyle = list(fontWeight = "bold",
                                                   borderColor = "hsl(0, 0%, 90%)",
                                                   background = "rgb(173 194 208 / 98%)")
        ),
        #theme = reactableTheme(highlightColor = "rgba(197,222,225,0.9305847338935574)"),
        #theme = pff(header_font_size = 15),
        striped = TRUE,
        defaultSorted = 'gasto',
        defaultSortOrder = "desc",
        searchable = TRUE,
        wrap = FALSE,
        resizable =TRUE,
        bordered = F,
        compact = T,
        selection = "multiple",
        onClick = "select",
        rowStyle = list(cursor = "pointer"),
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                     backgroundColor ="rgb(255 255 255 / 0%)"),
        defaultColDef = colDef(align = "left",
                               style = list(fontWeight = "bold",
                                            color = "black")
        ),
        columns = list(
          n_atendimento = colDef(name = "Nº Atend",
                                 maxWidth = 100,
                                 footer = "Total",
                                 footerStyle = list(fontSize = "1rem",
                                                    fontWeight = "bold",
                                                    color = "black"),
                                 headerStyle = list(fontSize = "1rem",
                                                    fontWeight = "bold",
                                                    color = "black")),
          protocolo = colDef(name = "Protocolo",
                             maxWidth = 100,
                             footerStyle = list(fontSize = "1rem",
                                                fontWeight = "bold",
                                                color = "black"),
                             headerStyle = list(fontSize = "1rem",
                                                fontWeight = "bold",
                                                color = "black")),
          equipe = colDef(name = "Equipe",
                          minWidth = 120,
                          headerStyle = list(fontSize = "1rem",
                                             fontWeight = "bold",
                                             color = "black"),
                          cell = color_tiles(data_tabela_mapa_at(),
                                             color_ref = "cor_equipes"
                          )
          ),
          atendimento = colDef(name = "Status",
                               headerStyle = list(fontSize = "1rem",
                                                  fontWeight = "bold",
                                                  color = "black"),
                               cell = pill_buttons(atendimentos_realizados(),
                                                   color_ref = "cor_atendimento",
                                                   box_shadow = T,
                                                   opacity = 0.7)),
          origem_ocorrencia = colDef(name = "Origem",
                                     headerStyle = list(fontSize = "1rem",
                                                        fontWeight = "bold",
                                                        color = "black")),
          tempo_restante = colDef(name = "Prazo",
                                  headerStyle = list(fontSize = "1rem",
                                                     fontWeight = "bold",
                                                     color = "black")),
          tipo_de_ocorrencia = colDef(name = "Tipo Ocorrência",
                                      headerStyle = list(fontSize = "1rem",
                                                         fontWeight = "bold",
                                                         color = "black")),
          motivo =colDef(name = "Motivo",
                         headerStyle = list(fontSize = "1rem",
                                            fontWeight = "bold",
                                            color = "black")),
          endereco = colDef(name = "Endereço",
                            headerStyle = list(fontSize = "1rem",
                                               fontWeight = "bold",
                                               color = "black")),
          lat = colDef(show = F),
          lon = colDef(show = F),
          hora_inicio = colDef(show = F),
          data_atendimento = colDef(show = F),
          hora_conclusao = colDef(name = "Hora Atendimento",
                                  headerStyle = list(fontSize = "1rem",
                                                     fontWeight = "bold",
                                                     color = "black")),
          cor_atendimento= colDef(show = F),
          data_reclamacao = colDef(name = "Reclamação",
                                   headerStyle = list(fontSize = "1rem",
                                                      fontWeight = "bold",
                                                      color = "black")),
          gasto = colDef(name = "Gasto",align = "center",
                         headerStyle = list(fontSize = "1rem",
                                            fontWeight = "bold",
                                            color = "black"),
                         cell = icon_assign(atendimentos_realizados(),
                                            icon = "dollar-sign",
                                            align_icons = "right",
                                            fill_color = "red",
                                            empty_color = "white",
                                            buckets =3,
                                            show_values = "left",
                                            number_fmt =  scales::number_format(prefix = "R$ "),
                                            empty_opacity = 0
                         ),
                         footer = \(values){ sprintf("R$%.2f", sum(values)) },
                         footerStyle = list(fontSize = "1rem",
                                            fontWeight = "bold",
                                            color = "black")
          ),
          cor_equipes = colDef(show = F)
        ),
        details = function(index){
          dados_at <- dados_materias()[dados_materias()$n_atendimento ==  atendimentos_realizados()$n_atendimento[index], ] 
          
          htmltools::div(style = "padding: 1rem",
                         reactable(dados_at %>% 
                                     select(n_atendimento,protocolo,codigo,descricao,equipe,quantidade,valor_unitario),
                                   resizable =TRUE,
                                   searchable = TRUE
                                   ,outlined = TRUE))
        }
      )
    
    tbl_oc <- data_share_oc() %>% 
      reactable(
        pagination = F,
        height = 500,
        highlight = T,
        theme = reactableTheme( rowSelectedStyle = list(backgroundColor = "#eee",
                                                        boxShadow = "inset 2px 0 0 0 #ffa62d"),
                                highlightColor = "rgb(24 164 255 / 10%)",
                                headerStyle = list(fontWeight = "bold",
                                                   borderColor = "hsl(0, 0%, 90%)",
                                                   background = "rgb(173 194 208 / 98%)")
        ),
        #theme = reactableTheme(highlightColor = "rgba(197,222,225,0.9305847338935574)"),
        #theme = pff(header_font_size = 15),
        striped = TRUE,
        defaultSorted = 'dif',
        #defaultSortOrder = "desc",
        searchable = TRUE,
        wrap = FALSE,
        resizable =TRUE,
        bordered = F,
        compact = T,
        selection = "multiple",
        onClick = "select",
        rowStyle = list(cursor = "pointer"),
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                     backgroundColor ="rgb(255 255 255 / 0%)"),
        defaultColDef = colDef(align = "left",
                               style = list(fontWeight = "bold",
                                            color = "black")
        ),
        columns = list(
          protocolo = colDef(name = "Protocolo",
                             maxWidth = 100,
                             footerStyle = list(fontSize = "1rem",
                                                fontWeight = "bold",
                                                color = "black"),
                             headerStyle = list(fontSize = "1rem",
                                                fontWeight = "bold",
                                                color = "black")),
          origem_ocorrencia = colDef(name = "Origem",
                                     headerStyle = list(fontSize = "1rem",
                                                        fontWeight = "bold",
                                                        color = "black")),
          tempo_restante = colDef(name = "Prazo",show = F,
                                  headerStyle = list(fontSize = "1rem",
                                                     fontWeight = "bold",
                                                     color = "black")),
          tipo_de_ocorrencia = colDef(name = "Tipo Ocorrência",
                                      headerStyle = list(fontSize = "1rem",
                                                         fontWeight = "bold",
                                                         color = "black")),
          endereco = colDef(name = "Endereço",
                            headerStyle = list(fontSize = "1rem",
                                               fontWeight = "bold",
                                               color = "black")),
          status = colDef(name = "Status",
                          headerStyle = list(fontSize = "1rem",
                                             fontWeight = "bold",
                                             color = "black")),
          lat = colDef(show = F),
          lon = colDef(show = F),
          data_reclamacao = colDef(name = "Reclamação",
                                   headerStyle = list(fontSize = "1rem",
                                                      fontWeight = "bold",
                                                      color = "black")),
          data_limite_para_atendimento = colDef(name = "Data Limite",
                                                minWidth = 180,
                                                resizable = T),
          dif = colDef(name = "Vencimento (Horas)",
                       style = list(fontSize = ".9rem"),
                       headerStyle = list(fontSize = "1rem",
                                          #fontWeight = "bold",
                                          color = "black"),
                       format = colFormat(suffix = " Horas"),
                       cell =  color_tiles(oc_pendentes(),
                                           bold_text = T,
                                           box_shadow = T,
                                           color_ref  = "cor_vencimento",
                                           number_fmt = scales::number_format(suffix = " Horas")
                                           #colors = RColorBrewer::brewer.pal(5, 'RdBu')
                       )),
          data_limite = colDef(show = F),
          lat = colDef(show = F),
          lon = colDef(show = F),
          cor_vencimento = colDef(show = F),
          tempo_restante = colDef(show = F)
        )
      )
    
    
    
    if(input$c_view == "Atendimentos Pendentes"){
      
      htmltools::browsable(
        htmltools::tagList(mapa, tbl)
      )
    } else if(input$c_view == "Atendimentos Realizados"){
      htmltools::browsable(
        htmltools::tagList(mapa_at, tbl_at)
      )
    } else{
      htmltools::browsable(
        htmltools::tagList(mapa_oc, tbl_oc)
      )
    }
    
  })
  
  output$c_gasto <- renderUI({
    
    
    
    
    if(input$c_view_equipes == "Gasto Total"){
      
      bs4ValueBox(color = "primary",width = 12,
                  value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;", 
                                 atendimentos_realizados() %>% 
                                   group_by(equipe) %>% 
                                   summarise(gasto = sum(gasto)) %>% 
                                   left_join(
                                     atendimentos_realizados() %>%
                                       group_by(equipe) %>% 
                                       summarise(Atendimentos = n()),
                                     by = "equipe"
                                   ) %>% 
                                   arrange(desc(gasto)) %>% 
                                   reactable(
                                     theme =  reactableTheme(
                                       highlightColor = "rgb(24 164 255 / 10%)",
                                       #borderColor = "hsl(0, 0%, 93%)",
                                       headerStyle = list(borderColor = "hsl(0, 0%, 90%)")
                                     ),
                                     #width = 400,
                                     #theme = nytimes(),
                                     highlight = T,
                                     style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                                                  backgroundColor ="rgb(255 255 255 / 0%)"),
                                     defaultColDef = colDef(align = "left",
                                                            style = list(#fontWeight = "bold",
                                                              color = "white")
                                     ),
                                     bordered = F,
                                     compact = F,
                                     columns = list(
                                       equipe = colDef(
                                         align = "center",
                                         name = "Equipe",
                                         minWidth = 120,
                                         style = list(fontSize = "1rem"),
                                         headerStyle = list(fontSize = "1rem",
                                                            #fontWeight = "bold",
                                                            color = "white"),
                                         footer = "Total",
                                         footerStyle = list(fontSize = "1rem",
                                                            fontWeight = "bold",
                                                            color = "white")
                                       ),
                                       Atendimentos = colDef(
                                         align = "center",
                                         name = "Atendimentos",
                                         minWidth = 110,
                                         style = list(fontSize = "1rem"),
                                         headerStyle = list(fontSize = ".9rem",
                                                            #fontWeight = "bold",
                                                            color = "white"),
                                         footer = \(values){ sprintf("%.0f", sum(values)) },
                                         footerStyle = list(fontSize = "1rem",
                                                            fontWeight = "bold",
                                                            color = "white")
                                       ),
                                       gasto = colDef(name = "Gasto",align = "center",
                                                      headerStyle = list(fontSize = "1rem",
                                                                         #fontWeight = "bold",
                                                                         color = "white"),
                                                      cell = icon_assign(atendimentos_realizados()%>% 
                                                                           group_by(equipe) %>% 
                                                                           summarise(gasto = sum(gasto)) %>% 
                                                                           left_join(
                                                                             atendimentos_realizados() %>%
                                                                               group_by(equipe) %>% 
                                                                               summarise(Atendimentos = n()),
                                                                             by = "equipe"
                                                                           ) %>% 
                                                                           arrange(desc(gasto)),
                                                                         icon = "dollar-sign",
                                                                         icon_size = 13,
                                                                         align_icons = "right",
                                                                         fill_color = "green",
                                                                         empty_color = "white",
                                                                         buckets = 3,
                                                                         show_values = "left",
                                                                         number_fmt =  scales::number_format(prefix = "R$ "),
                                                                         empty_opacity = 0
                                                      ),
                                                      footer = \(values){ sprintf("R$%.2f", sum(values)) },
                                                      footerStyle = list(fontSize = "1rem",
                                                                         fontWeight = "bold",
                                                                         color = "white")
                                       )
                                     )
                                   )
                  ),
                  gradient = T,
                  footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                  subtitle = 
                    tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;")
      )
      
    } else {
      
      bs4ValueBox(color = "primary",width = 12,
                  value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;", 
                                 atendimentos() %>% 
                                   filter(data_hora == if(input$filtro == Sys.Date()){
                                     if(format(Sys.time(), "%Y-%m-%d %X") < paste(Sys.Date(), "06:00:00")){   Sys.Date()-1 } else{   Sys.Date() }
                                   } else {
                                     input$filtro
                                   }) %>%
                                   group_by(equipe) %>% 
                                   summarise(dist = geosphere::distHaversine(cbind(lat,lon),
                                                                             cbind(lag(lat),
                                                                                   lag(lon)))) %>% 
                                   drop_na(dist) %>%
                                   group_by(equipe) %>% 
                                   reframe(media = round(mean(dist),0)) %>% 
                                   arrange(desc(media)) %>% 
                                   reactable(
                                     theme =  reactableTheme(
                                       highlightColor = "rgb(24 164 255 / 10%)",
                                       #borderColor = "hsl(0, 0%, 93%)",
                                       headerStyle = list(borderColor = "hsl(0, 0%, 90%)")
                                     ),
                                     #width = 400,
                                     #theme = nytimes(),
                                     highlight = T,
                                     style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                                                  backgroundColor ="rgb(255 255 255 / 0%)"),
                                     defaultColDef = colDef(align = "left",
                                                            style = list(#fontWeight = "bold",
                                                              color = "white")
                                     ),
                                     bordered = F,
                                     compact = F,
                                     columns = list(
                                       equipe = colDef(
                                         align = "center",
                                         name = "Equipe",
                                         minWidth = 120,
                                         style = list(fontSize = "1rem"),
                                         headerStyle = list(fontSize = "1rem",
                                                            #fontWeight = "bold",
                                                            color = "white")
                                       ),
                                       media = colDef(
                                         align = "center",
                                         name = "Distância Méd",
                                         minWidth = 110,
                                         style = list(fontSize = "1rem"),
                                         headerStyle = list(fontSize = ".9rem",
                                                            #fontWeight = "bold",
                                                            color = "white"),
                                         format = colFormat(suffix = " Metros")
                                       )
                                     )
                                   )
                  ),
                  gradient = T,
                  footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                  subtitle = 
                    tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;")
      )
      
    }
    
    
    
    
  })
  
  output$c_oc_tipo <- renderUI({
    
    oc_tipo <- reactive({
      oc_pendentes() %>% 
        group_by_("tipo_de_ocorrencia"= input$c_tipo_oc) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) 
    })
    
    bs4ValueBox(color = "primary",width = 12,
                value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;", 
                               oc_tipo() %>% 
                                 reactable(
                                   theme =  reactableTheme(
                                     highlightColor = "rgb(24 164 255 / 10%)",
                                     #borderColor = "hsl(0, 0%, 93%)",
                                     headerStyle = list(borderColor = "hsl(0, 0%, 90%)")
                                   ),
                                   #width = 400,
                                   #theme = nytimes(),
                                   highlight = T,
                                   defaultPageSize = 5,
                                   style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                                                backgroundColor ="rgb(255 255 255 / 0%)"),
                                   defaultColDef = colDef(align = "left",
                                                          style = list(#fontWeight = "bold",
                                                            color = "white")
                                   ),
                                   bordered = F,
                                   compact = T,
                                   columns = list(
                                     tipo_de_ocorrencia  = colDef(
                                       align = "left",
                                       name = "Ocorrência",
                                       minWidth = 120,
                                       style = list(fontSize = ".9rem"),
                                       headerStyle = list(fontSize = "1rem",
                                                          #fontWeight = "bold",
                                                          color = "white")
                                     ),
                                     n = colDef(
                                       align = "center",
                                       name = "Quantidade",
                                       minWidth = 110,
                                       style = list(fontSize = "0.9rem"),
                                       headerStyle = list(fontSize = ".9rem",
                                                          #fontWeight = "bold",
                                                          color = "white")
                                     )
                                   ), 
                                   details = function(index){
                                     
                                     dados_intra <- if(input$c_tipo_oc == "tipo_de_ocorrencia"){
                                       oc_pendentes()[oc_pendentes()$tipo_de_ocorrencia == oc_tipo()$tipo_de_ocorrencia[index],]
                                     } else {
                                       oc_pendentes()[oc_pendentes()$origem_ocorrencia == oc_tipo()$tipo_de_ocorrencia[index],]
                                       
                                     }
                                     
                                     
                                     
                                     
                                     htmltools::div(style = "padding: 1rem",
                                                    reactable(
                                                      if(input$c_tipo_oc == "tipo_de_ocorrencia"){
                                                        dados_intra %>% 
                                                          select(protocolo,origem_ocorrencia,dif) 
                                                      } else {
                                                        dados_intra %>% 
                                                          select(protocolo,"origem_ocorrencia"=tipo_de_ocorrencia,dif) 
                                                      }
                                                      
                                                      ,
                                                      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                                                                   backgroundColor ="rgb(255 255 255 / 0%)"),
                                                      highlight = T,
                                                      compact = T,
                                                      defaultSorted = "dif",
                                                      defaultSortOrder = "asc",
                                                      columns = list(
                                                        # data_limite_para_atendimento = colDef(name = "Vencimento",
                                                        #                                       style = list(fontSize = ".9rem"),
                                                        #                                       headerStyle = list(fontSize = "1rem",
                                                        #                                                          #fontWeight = "bold",
                                                        #                                                          color = "white")),
                                                        origem_ocorrencia = colDef(name = "Origem",
                                                                                   style = list(fontSize = ".9rem"),
                                                                                   headerStyle = list(fontSize = "1rem",
                                                                                                      #fontWeight = "bold",
                                                                                                      color = "white")),
                                                        dif = colDef(name = "Prazo",
                                                                     style = list(fontSize = ".9rem"),
                                                                     headerStyle = list(fontSize = "1rem",
                                                                                        #fontWeight = "bold",
                                                                                        color = "white"),
                                                                     format = colFormat(suffix = " Horas"),
                                                                     cell =  color_tiles(dados_intra,
                                                                                         bold_text = T,
                                                                                         box_shadow = T,
                                                                                         color_ref  = "cor_vencimento",
                                                                                         number_fmt = scales::number_format(suffix = " Horas")
                                                                                         #colors = RColorBrewer::brewer.pal(5, 'RdBu')
                                                                     ))
                                                      )
                                                      
                                                    )
                                     )
                                   }
                                 )),
                gradient = T,
                footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;")
    )
  })
  
  
  acumulado <- reactive({
    atendimentos() %>% 
      filter(data_hora >= floor_date(input$filtro,"month"),
             data_hora<= input$filtro,
             if(input$c_tipo_at %in% c("Atendido","Impossibilidade","Encontrado normal")){
               atendimento == input$c_tipo_at
             } else if(input$c_tipo_at == "Atendimentos"){
               atendimento %in% atendimento
             } else {
               is.na(protocolo)
             }
      ) %>% 
      group_by(equipe) %>% 
      summarise(n=n()) %>% 
      arrange(desc(n))
  })
  media_equipe_acum <- reactive({
    acumulado() %>% pull(n) %>% mean() 
  })
  output$c_at_equipe_acum <- renderbs4ValueBox({
    
    bs4ValueBox(color = "primary",
                value = tags$p(style = "font-size: 30px;font-weight: lighter;text-align: center;"
                ),
                gradient = T,footer = tags$p(style = "heigth:0px;margin-bottom: 0rem;"),
                subtitle = 
                  tags$p(style = "font-size: 20px;font-weight: lighter;text-align: center;",
                         
                         hchart(acumulado(),"bar",
                                hcaes(x=equipe,y=n),
                                color = list(
                                  linearGradient= c( list(x1=0), list(x2=0), list(y1=0), list(y2=1)),
                                  stops= list(
                                    c(0, '#26a5bf'),
                                    c(0.5,"#00586e"),
                                    c(1, "#064354")
                                  )
                                )
                         )%>% 
                           hc_add_dependency("plugins/grouped-categories.js") %>%
                           hc_yAxis(title = list(text = " ",
                                                 style = list(fontSize = '13px',
                                                              fontWeight= 'bold')),
                                    labels = list(style = list(fontSize = '11px',
                                                               fontWeight= 'bold')),
                                    gridLineWidth =  0,
                                    plotLines = list(
                                      list(
                                        value = media_equipe_acum(),
                                        label = list(text = "",
                                                     align = "right",
                                                     
                                                     fontColor = "white",
                                                     rotation = 0),
                                        color = "white",
                                        dashStyle = "dash",
                                        width = 1)),
                                    stackLabels= list(
                                      enabled= TRUE,
                                      style = list(
                                        textOutline= 'none',
                                        color = "white",
                                        fontSize = "14px"
                                      )
                                      
                                    )
                           ) %>% 
                           hc_xAxis(title = list(text = " "),
                                    labels = list(rotation = -0,
                                                  style = list(fontSize = '13px',
                                                               color = "white",
                                                               fontWeight= 'bold')),
                                    categories =  if(length(unique(acumulado()$equipe)) == 1)
                                      list(unique(acumulado()$equipe)) else
                                        unique(acumulado()$equipe),
                                    labels = list(style = list(fontSize = "13px"))
                           ) %>% 
                           hc_plotOptions(bar = list(
                             dataLabels = list(
                               enabled = TRUE,
                               color = 'white',
                               format = '{point.y}',
                               #shape = NULL,
                               style = list(textOutline = NULL,
                                            fontSize = '12px'))),
                             series = list(
                               #pointWidth=25,
                               
                               #pointPadding= 0,
                               #groupPadding =0,
                               lineWidth = 1,
                               borderWidth = 1,
                               borderColor = "white",
                               dataLabels = list(enabled = TRUE,
                                                 color = "white")
                             )
                           ) %>% 
                           hc_legend(enabled = F,layout= 'horizontal',itemStyle = list(color = "white", fontSize = '11px')) %>% 
                           hc_tooltip(table = T,useHTML  =TRUE,
                                      headerFormat= "<span style='font-size:13px;'><b> Atendimentos Acumulados </b></span><br>",
                                      pointFormat= "<span <span style='color:{series.color};font-size:15px'><b> Atendimentos</b></span>: <b> {point.y} </b> </span><br>",
                                      #xDateFormat =  '%Y-%m-%d',
                                      
                                      style = list(fontSize = '14px')
                                      #backgroundColor = "#ffffff",
                                      #borderColor= '#000000'
                           ) %>% 
                           hc_title(text = paste0("Atendimentos Acumulados por Equipe - ",
                                                  format(floor_date(input$filtro,"month"),"%d/%m"),
                                                  " até ",
                                                  format(input$filtro,"%d/%m")),
                                    style = list(color = "white",fontSize = '22px')) 
                         #hc_size(height = 220) %>%   
                         #hc_add_theme(hc_theme_sparkline_vb()) %>% 
                         #hc_credits(enabled = FALSE)
                         
                  )
    )
    
  })
}

shinyApp(ui, server)