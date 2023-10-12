pacman::p_load(colourpicker,crosstalk,plotly,aws.s3,readxl,leaflet.extras,reactable,reactablefmtr,shinyjs,leaflegend,leaflet,googleway,tippy,shinyWidgets,htmltools,janitor,fresh,tidyverse,shiny,DT,shinythemes,bslib,waiter,stringr,paletteer,waiter,highcharter,readr,bs4Dash,shinydashboard,shinydashboardPlus,reshape2)



ui <- bs4DashPage(
  
  #dark = NULL,
  header = bs4DashNavbar(
    
    title = dashboardBrand(title = "", opacity = 1,color = "primary",
                           href = "https://conectacampinas.net/",
                           image = "https://conectacampinas.net/wp-content/uploads/2023/03/logo-conecta-campinas2.svg"
    ),
    lefttUi = tags$li(class = "navbar-collapse collapse dropdown",
                      tags$ul(class = "nav navbar-nav sidebar-menu",
                              bs4SidebarMenuItem(paste0("Conecta Campinas - Diário De Operação (Atualizado a cada hora, com incio as 6h (turno Manhã) e encerrameno as 6h do dia segunte (final turno Noite)"),
                                                 tabName="pag1"),
                              
                              style="font-size: 1em;"
                      )
    ),
    sidebarIcon = tags$ul(class = "navbar-nav",style="width: 0px;font-size:0px;")
  ),
  
  sidebar = bs4DashSidebar(disable = T
  ),
  
  body = bs4DashBody(shinyjs::useShinyjs(),
                     
                     
                     # CONFIGS ----
                     useWaiter(), 
                     waiterShowOnLoad(html =spin_6(),color = "rgba(13,32,43,0.87)"),
                     waiterOnBusy(html = spin_6(),color = "rgba(13,32,43,0.87)"),
                     useHostess(),
                     
                     use_theme(create_theme(bs4dash_color(
                       #blue = "#042365",
                       #blue = "#198ab2",
                       red = "#8f2121",
                       #blue = "#133729"
                       
                       # esse verde usava ############
                       #blue = "#284825",
                       #####################
                       blue = "#578b53"
                       #"#284025"
                       #"#1b5415" 
                       #"#0E360A"
                     ),
                     bs4dash_vars(
                       "card-title-font-size"= "1.2rem",
                       "progress-bar-border-radius"="2px",
                       "body-bg"="rgba(2,58,79,0.71)"
                     ))),
                     # CONFIGS ----
                     
                     
                     tabItems(
                       tabItem(tabName = "pag1",
                               
                               fluidRow(
                                 # CSS ----
                                 tags$style(HTML("
                                                 
                            .awesome-marker i {
                                color: #000;
                            }
                                                 
                          .info.legend.leaflet-control i {
                               float: left;
                                    }
                                                 
                            .leaflet-top {z-index:999!important;}  
                          
                                .content-wrapper>.content {
                                  padding: 0px !important;
                                }

                                                 
                                 body {
                                 text-align: center
                                 }             
                                                 
                              .tab-content {
                                  background: #235776;
                                  background-image: linear-gradient(to top,#cccbcf26,#000000);
                              }
                                                 
                                                   
                              .form-control {
                                  text-align: center;
                              }
                                
                              .card-primary:not(.card-outline)>.card-header{
                                  background-image:linear-gradient(to left,#000000ab,rgb(13 110 147 / 71%));
                              }
                                
                              .bg-primary {
                                  background-image: linear-gradient(to right,#030f02,#307aa5);
                              }
                                
                             #c_solic .bg-gradient-primary {
                                  background: linear-gradient(#8c8422, #000000) repeat-x !important;
                             }
                            #c_vistoria .bg-gradient-primary {
                                  background: linear-gradient(#983737, #000000) repeat-x !important;
                            }
                            #c_tab .bg-gradient-primary {
                                  background: linear-gradient(#983737, #1c0303) repeat-x !important;
                            }
                            #c_oc_tipo .bg-gradient-primary {
                                  background: linear-gradient(#983737, #1c0303) repeat-x !important;
                            }
                            #c_gasto .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                            }
                            #c_prefeitura .bg-gradient-primary {
                                  background: linear-gradient(#983737, #000000) repeat-x !important;
                              }
                             
                             .card-body {
                                background-image: linear-gradient(to bottom,#cccbcf26,#b9c7cf);
                             }
                            
                            
                             #c_at_imp .bg-gradient-primary {
                                  background: linear-gradient(#983737, #000000) repeat-x !important;
                              }
                             #c_at .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                             }
                          
                          #c_at_equipe_acum .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                          }
                            #c_at_equipe .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                            }
                           #c_avulso_equipe .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                             }
                              #c_at_valor .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                             }
                             #c_graf .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                             }
                             #c_at_t .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                             }
                             #c_total_equipes .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_at .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_at_equipe .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                           #c_at_equipe_acum .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                           #c_avulso_equipe .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_at_valor .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_at_t .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_solic .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                              #c_vistoria .small-box p{
                                 margin-bottom: 0.5rem;
                              }
                              #c_tab .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_prefeitura .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_at_prazo .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_at_fprazo .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_at_avulso .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                             #c_graf .small-box p{
                                 margin-bottom: 0.5rem;
                             }
                              
                              
                            #c_at_avulso .bg-gradient-primary {
                                  background: linear-gradient(#bf7b01, #000000) repeat-x !important;
                            }
                            #c_at_prazo .bg-gradient-primary {
                                  background: linear-gradient(#1d77ab, #000000) repeat-x !important;
                              }
                             
                            #c_at_fprazo .bg-gradient-primary {
                                  background: linear-gradient(#983737, #000000) repeat-x !important;
                            }
                            #c_at_fprazo .bg-gradient-primary {
                                  background: linear-gradient(#983737, #000000) repeat-x !important;
                            }
                            #c_oc_atrasadas .bg-gradient-primary {
                                  background: linear-gradient(#983737, #000000) repeat-x !important;
                            }
                            #c_total_equipes .bg-gradient-primary {
                                  background: linear-gradient(#054f02, #000000) repeat-x !important;
                            }
                             
                          .brand-link {
                              display: block;
                              font-size: 1.25rem;
                              line-height: 1.5;
                              padding: 0.8125rem 3.5rem;
                              transition: width .3s ease-in-out;
                              white-space: nowrap;
                          }
                          .img-circle {
                              border-radius: 0%;
                          }
                          .elevation-3 {
                            box-shadow: 0 0px 0px rgb(0 0 0 / 19%), 0 0px 0px rgb(0 0 0 / 23%) !important;
                          }
                          .dropdown-item.active, .dropdown-item:active {
                          background-color: #dee2e6;
                          }
                          .btn:not(:disabled):not(.disabled):active, .btn:not(:disabled):not(.disabled).active {
                          background-color: #b5b5b596;
                          }
                          .bootstrap-select .dropdown-menu li.active small {
                          color: #000!important;
                          }
                          .text-muted {
                          color: #000 !important;
                          }
                          
                          .highcharts-data-table table {
                            font-family: Verdana, sans-serif;
                            border-collapse: collapse;
                            border: 1px solid #ebebeb;
                            margin: 10px auto;
                            text-align: center;
                            width: 100%;
                            max-width: 500px;
                          }
                          
                          .highcharts-data-table caption {
                            padding: 1em 0;
                            font-size: .2em;
                            color: #555;
                          }
                          
                          .highcharts-data-table th {
                            font-weight: 600;
                            padding: 0.5em;
                          }
                          
                          .highcharts-data-table td,
                          .highcharts-data-table th,
                          .highcharts-data-table caption {
                            padding: 0.5em;
                          }
                          
                          .highcharts-data-table thead tr,
                          .highcharts-data-table tr:nth-child(even) {
                            background: #f8f8f8;
                          }
                          
                          .highcharts-data-table tr:hover {
                            background: #f1f7ff;
                          }
                          "
                                                 # CSS ----
                                 )),
                                 # BODY ----
                                 column(width = 12,  align="center",
                                        
                                        dateInput(inputId = "filtro",
                                                  label = h6(HTML("<i class='glyphicon glyphicon-calendar'></i>  Selecione uma Data para gerar o RDO"),style= 'color: white;'),
                                                  min = "2023-03-01",
                                                  max = Sys.Date(),
                                                  language = "pt-BR",
                                                  value = Sys.Date(),
                                                  format = "dd/mm/yyyy"
                                                  
                                        ),tags$hr()
                                 ),
                                 
                                 column(width = 9,
                                        
                                        fluidRow(
                                          bs4ValueBoxOutput(outputId = "c_total_equipes",width =2),
                                          column(4,
                                                 bs4ValueBoxOutput(outputId = "c_at_valor",width = 12),
                                                 bs4ValueBoxOutput(outputId = "c_solic",width = 12)
                                                 
                                          ),
                                          bs4ValueBoxOutput(outputId = "c_at",width = 6),
                                          
                                          
                                          column(12,
                                                 bs4Card(id = "at_so2",width = 12,elevation = 4,maximizable = T,status = "primary",
                                                         title = "Tabela/Mapa dos Despachos a Serem Realizados, Atendimentos Realizados e Ocorrências Pendentes",
                                                         solidHeader = T,collapsed = F,headerBorder = F,
                                                         radioGroupButtons(
                                                           inputId = "c_view",
                                                           label = "Selecione uma Opção Visualização", 
                                                           choices = c("Atendimentos Pendentes",
                                                                       "Atendimentos Realizados",
                                                                       "Ocorrências Pendentes"),
                                                           justified = T,
                                                           selected = "Atendimentos Pendentes",
                                                           width = "100%",
                                                           checkIcon = list(yes = icon("ok",lib = "glyphicon"))
                                                         ),
                                                         uiOutput("p_map_tab")
                                                 )
                                                 
                                          ),
                                          column(12,
                                                 bs4Card(id = "at_so",width = 12,elevation = 4,maximizable = T,status = "primary",
                                                         title = "Status dos Despachos",
                                                         solidHeader = T,collapsed = F,headerBorder = F,
                                                         #csvDownloadButton("status_os", filename = "status_os.csv"),
                                                         uiOutput("status_os")
                                                 )
                                                 
                                          ),
                                          column(12,
                                                 radioGroupButtons(
                                                   inputId = "c_tipo_at",
                                                   label = " ", 
                                                   choices = c("Atendimentos",
                                                               "Efetivo"="Atendido",
                                                               "Avulsos",
                                                               "Impossibilidade",
                                                               "Encontrado normal"),
                                                   justified = T,
                                                   selected = "Atendimentos",
                                                   width = "100%",
                                                   checkIcon = list(yes = icon("ok",lib = "glyphicon"))
                                                 ),
                                                 bs4ValueBoxOutput(outputId = "c_at_equipe_acum",width = 12),
                                          )
                                        )
                                 ),
                                 column(3,
                                        bs4ValueBoxOutput(outputId = "c_oc_atrasadas",width =12),
                                        uiOutput("c_tab"),
                                        radioGroupButtons(
                                          inputId = "c_tipo_oc",
                                          label = " ", 
                                          choices = c("Tipo"="tipo_de_ocorrencia",
                                                      "Origem"="origem_ocorrencia"),
                                          justified = T,
                                          selected = "tipo_de_ocorrencia",
                                          width = "100%",
                                          checkIcon = list(yes = icon("ok",lib = "glyphicon"))
                                        ),
                                        uiOutput("c_oc_tipo"),
                                        radioGroupButtons(
                                          inputId = "c_view_equipes",
                                          label = " ", 
                                          choices = c("Gasto Total",
                                                      "Distância Méd"),
                                          justified = T,
                                          selected = "Gasto Total",
                                          width = "100%",
                                          checkIcon = list(yes = icon("ok",lib = "glyphicon"))
                                        ),
                                        uiOutput("c_gasto"),
                                        bs4ValueBoxOutput(outputId = "c_at_equipe",width = 12),
                                        bs4ValueBoxOutput(outputId = "c_avulso_equipe",width = 12)
                                 )
                                 
                                 
                                 # ----
                               ))
                     )
  ),
  
  footer =  dashboardFooter()
)