# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB diagnostic cascade, treatment success rate, and number
# of labs for a group of countries and for a country using JSON data retrieved 
# from the WHO global tuberculosis database.
# Takuya Yamanaka, May 2022
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.0"
library("shiny")
library("glue")
library("rlang")
library("shiny")
library("sourcetools")
library("dplyr")
library("ggplot2")
library("readxl")
library(shinythemes)
library("cowplot")
library(jsonlite)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- 
  navbarPage(
    "TB Diagnostic Cascade",
    tabPanel(
      "Global, Groups of countries",
      # --------------------- Global, WHO regions, HBCs, GF etc ---------------------#
      fluidPage(theme = shinytheme("sandstone"),
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 downloadButton('dl_fig_reg', 'Download (figures)')
                 ),      
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 downloadButton('dl_dat_reg', 'Download (data)')
          )),      
        
        br(),
        
        fluidRow(tags$div(id = "page_header",
                          HTML("Select a group of countries"),
                          uiOutput(outputId = "group"))
        ),
        
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 plotOutput("group_plot1")
          ),
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 plotOutput("group_plot2")
          )),
        
        br(),
        
        
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 plotOutput("group_plot3")
          ),
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 plotOutput("group_plot4")
          )),
        
        br(),
        
        fluidRow(tags$div(style = "padding: 20px; font-size: 80%;",
                          "*: the percentage in Fig.1 is the proportion of new and relapse cases and cases with unknown previous TB treatment history to the estimated TB incidence",
                          br(),
                          "**: the percentage in Fig.1 is the proportion of laboratory confirmed pulmonary TB to all the notified pulmonary TB",
                          br(),
                          "***: the percentage in Fig.1 is the proportion of new and relapse cases notified and tested using a WHO-recommended rapid diagnostic (for example Xpert MTB/RIF) at the time of TB diagnosis to all the notified pulmonary TB"
        )),
        
        fluidRow(tags$div(id = "metadata2",
                          style = "padding: 20px; font-style: italic; font-size: 80%;",
                          
                          # Add app version number and links to GTB and Github
                          HTML(paste0(app_version,
                                      ", Source code on <a href='https://github.com/tyamanaka312/tb_dxcascade' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
        )) 
        
        
      )
    ),
    
    tabPanel(
      "Single country",
      # --------------------- each country ---------------------#
      fluidPage(
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 downloadButton('dl_fig_con', 'Download (figures)')
          ),      
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 downloadButton('dl_dat_con', 'Download (data)')
          )),      
        
        br(),
        
        fluidRow(tags$div(id = "page_header2",
                          HTML("Select a country"),
                          uiOutput(outputId = "country"))
        ),
        
      fluidRow(
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               plotOutput("country_plot1")
        ),
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               plotOutput("country_plot2")
        )),
      
      br(),
      
      fluidRow(
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               plotOutput("country_plot3")
        ),
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               plotOutput("country_plot4")
        )),
      
      br(),
      
      fluidRow(tags$div(style = "padding: 20px; font-size: 80%;",
                        "*: the percentage in Fig.1 is the proportion of new and relapse cases and cases with unknown previous TB treatment history to the estimated TB incidence",
                        br(),
                        "**: the percentage in Fig.1 is the proportion of laboratory confirmed pulmonary TB to all the notified pulmonary TB",
                        br(),
                        "***: the percentage in Fig.1 is the proportion of new and relapse cases notified and tested using a WHO-recommended rapid diagnostic (for example Xpert MTB/RIF) at the time of TB diagnosis to all the notified pulmonary TB"
      )),
      
      fluidRow(tags$div(id = "metadata3",
                        style = "padding: 20px; font-style: italic; font-size: 80%;",
                        
                        # Add app version number and links to GTB and Github
                        HTML(paste0(app_version,
                                    ", Source code on <a href='https://github.com/tyamanaka312/tb_dxcascade' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
      ))
      ))
  )



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Back end server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {
  
  
  json_url <- "https://extranet.who.int/tme/generateJSON.asp"
  
  # Get the latest list of countries with provisional data to use in country dropdown
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  group_list_json <- reactive({
    
    url <- paste0(json_url, "?ds=groups")
    
    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
    # group_list <- json$groups %>%
    #   select(group_code, group_description) %>%
    #   arrange(group_description)
  })
  
  # Build the select country control
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$group <- renderUI({
    
    already_selected <- input$group_code
    
    # Create a named list for selectInput
    group_list <- group_list_json()$groups %>%
      select(group_code, group_description) %>%
      arrange(group_description)
    
    group_list <- setNames(group_list[,"group_code"], group_list[,"group_description"])
    
    selectInput(inputId = "group_code",
                label = "",
                choices = group_list,
                # next line needed to avoid losing the selected country when the language is changed
                selected = already_selected,
                selectize = FALSE,
                width = "380px")
  })
  
  # Get the data as a JSON file for the chosen group
  # - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  gdata <- reactive({
    url <- paste0(json_url, "?ds=dxcascade_group&group_code=", input$group_code)
    
    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
  })
  
  
  # base reactive data frame
  df_group <- reactive({
    gdata <- gdata()
    df_est <- gdata$incidence 
    
    df_not <- gdata$notifications  %>%
      rowwise() %>%
      mutate(conf_rrmdr   = sum(c(conf_rrmdr,conf_rr_nfqr,conf_rr_fqr),na.rm = TRUE),
             all_labconf  = sum(c(new_labconf,ret_rel_labconf),na.rm = TRUE),
             all_pulmo    = sum(c(new_labconf,ret_rel_labconf,new_clindx,ret_rel_clindx),na.rm = TRUE),
             # rdst_all     = sum(c(rdst_new,rdst_ret,rdst_unk),na.rm = TRUE),
             conf_rrmdr_tx= sum(c(unconf_rrmdr_tx,conf_rrmdr_tx,unconf_rr_nfqr_tx,conf_rr_nfqr_tx,conf_rr_fqr_tx),na.rm = TRUE))
    
    df_lab <- gdata$labs %>%
      rowwise() %>%
      mutate(wrd=sum(c(xpert,m_wrd),na.rm = TRUE))
    
    df_out <- gdata$outcomes %>%
      subset(year>2014) 
    
    df_out %>%
      full_join(df_lab,by=c('year')) -> df
    df_not %>%
      right_join(df,by=c('year')) -> df
    df_est %>%
      right_join(df,by=c('year')) -> df
    
    df_group <- df %>%
      mutate(pct_dummy=e_inc_num/e_inc_num,
             not_rate=c_newinc/e_inc_num,
             labconf_rate=all_labconf/all_pulmo,
             rapid_rate=newinc_rdx/c_newinc,
             mdr_tx_rate=conf_rrmdr_tx/conf_rrmdr,
             lab_ratio=wrd/smear)
    
  })
  
  
  # fig 1 reactive data frame
  df_group1 <- reactive({
    df_group1 <- df_group() 
    df_group1 <- df_group1 %>%
      select(year,e_inc_num,c_newinc,all_labconf,newinc_rdx) %>%
      tidyr::gather(., key = variable, value = value, e_inc_num:newinc_rdx)
    
    df_group1b <- df_group() 
    df_group1b <- df_group1b %>%
      select(year,pct_dummy,not_rate,labconf_rate,rapid_rate) %>%
      tidyr::gather(., key = var_pct, value = pct, pct_dummy:rapid_rate)
    
    df_group1b %>%
      ungroup() %>%
      select(-year) %>% cbind.data.frame(df_group1,.) -> df_group1
    
  })
  
  
  # fig 1 
  group_p1 <- reactive({
    df <- df_group1()
    
    df <- df %>%
      mutate(variable=factor(variable,labels=c("Lab-confirmed**","Notified*","Estimated incidence","Rapid diagnosis at time of diagnosis***")),
             variable=factor(variable,levels=c("Estimated incidence","Notified*","Lab-confirmed**","Rapid diagnosis at time of diagnosis***"))) %>%
      mutate(textcol=ifelse(var_pct=="rapid_rate","B","W"))
    
    p1 <- df %>%
      ggplot(aes(x = as.factor(year), y = value, fill=variable, group=variable)) +
      geom_bar(position="dodge", stat = "identity", width=0.9, alpha=.79) +
      geom_text(aes(y=max(value)/10, label = paste0(round(pct*100,0),"%"), group=variable,color= textcol), 
                position = position_dodge(width = 0.9), size = 3, 
                vjust=2) +
      scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      # scale_y_continuous(labels = scales::comma) +
      # scale_y_continuous(breaks=c(0, 2500000, 5000000, 7500000, 10000000), labels = c("0", "2.5M", "5M", "7.5M", "10M")) +
      labs(title="Fig.1 Diagnostic cascade for all forms of TB in 2015-2020") +
      xlab("Year") +
      ylab(NULL) +
      scale_fill_manual(name = NULL, values=c("darkblue","dodgerblue3","green3","goldenrod3")) +
      scale_color_manual(values = c('W' = 'white', 'B' = 'black'), guide = "none")+
      theme_light() +
      theme(legend.position = "top")
    
    p1 <- p1 +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold")
      ) +
      theme(strip.placement = "outside",strip.background = element_blank(),
            panel.border =  element_rect(colour = "grey", fill=NA, size=1),
            panel.spacing.x = unit(3,"line")) +
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.title = element_text(size=11), # ,face="bold", 
            axis.text.x =  element_text(size = 10), #,angle = 90, vjust = 0.6, hjust=0.6
            axis.text.y =  element_text(size = 10), #, angle = 90, hjust =0.4)
            strip.text.x = element_text(size = 10))
    
    p1
  })
  
  # fig 1 output
  output$group_plot1 <- renderPlot({
    group_p1()
  })
  
  # fig 2 reactive data frame
  df_group2 <- reactive({
    df_group2 <- df_group() 
    df_group2 <- df_group2 %>%
      select(year,conf_rrmdr,conf_rrmdr_tx) %>%
      tidyr::gather(., key = variable, value = value, conf_rrmdr:conf_rrmdr_tx)
    
    df_group2b <- df_group() 
    df_group2b <- df_group2b %>%
      select(year,pct_dummy,mdr_tx_rate) %>%
      tidyr::gather(., key = var_pct, value = pct, pct_dummy:mdr_tx_rate)
    
    df_group2b %>%
      ungroup() %>%
      select(-year) %>% cbind.data.frame(df_group2,.) -> df_group2
    
  })
  
  # fig 2
  group_p2 <- reactive({
    df <- df_group2()
    
    df <- df %>%
      mutate(variable=factor(variable,labels=c("Detected MDR-TB","MDR-TB placed on treatment")))%>%
      mutate(textcol=ifelse(var_pct=="rapid_rate","B","W"))
    
    p2 <- df %>%
      ggplot(aes(x = as.factor(year), y = value, fill=variable, group=variable)) +
      geom_bar(position="dodge", stat = "identity", width=0.8, alpha=.79) +
      geom_text(aes(y=max(value)/10, label = paste0(round(pct*100,0),"%"), group=variable, color=textcol), 
                position = position_dodge(width = 0.8), size = 3, color= "white",
                vjust=2) +
      scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      # scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000), labels = c("0", "50K", "100K", "150K", "200K")) +
      labs(title="Fig.2 Diagnostic cascade for MDR-TB in 2015-2020") +
      xlab("Year") +
      ylab(NULL) +
      scale_fill_manual(name = NULL, values=c("dodgerblue3","green3")) +
      scale_color_manual(values = c('W' = 'white', 'B' = 'black'), guide = "none")+
      theme_light() +
      theme(legend.position = "top")
    
    p2 <- p2 +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold")
      ) +
      theme(strip.placement = "outside",strip.background = element_blank(),
            panel.border =  element_rect(colour = "grey", fill=NA, size=1),
            panel.spacing.x = unit(3,"line")) +
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.title = element_text(size=11), # ,face="bold", 
            axis.text.x =  element_text(size = 10), #,angle = 90, vjust = 0.6, hjust=0.6
            axis.text.y =  element_text(size = 10), #, angle = 90, hjust =0.4)
            strip.text.x = element_text(size = 10))
    
    p2
  })
  
  # fig 2 output
  output$group_plot2 <- renderPlot({
    group_p2()
  })
  
  # fig 3 reactive data frame
  df_group3 <- reactive({
    df_group3 <- df_group() 
    df_group3 <- df_group3 %>%
      select(year,c_new_tsr,c_mdr_tsr) %>%
      tidyr::gather(., key = variable, value = value, c_new_tsr:c_mdr_tsr)
  })
  
  # fig 3
  group_p3 <- reactive({
    df <- df_group3()
    
    df <- df %>%
      mutate(variable=factor(variable,labels=c("MDR-TB","TB")),
             variable=factor(variable,levels=c("TB","MDR-TB")))
    
    p3 <- df %>%
      ggplot(aes(x = as.factor(year), y = value, group=variable,colour=variable)) +
      geom_line(size=1) +
      geom_point(size=3) +
      geom_text(aes(y=value, label = paste0(value,"%"), group=variable), 
                position = position_dodge(width = 0.8), size = 3, color="black",
                vjust=-2) + 
      ylim(40,100) +
      labs(title="Fig.3 Treatment success rate in 2015-2020") +
      xlab("Year") +
      ylab("%") +
      scale_colour_manual(name = NULL, values=c("dodgerblue3","green3")) +
      theme_light() +
      theme(legend.position = "top")
    
    p3 <- p3 +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold")
      ) +
      theme(strip.placement = "outside",strip.background = element_blank(),
            panel.border =  element_rect(colour = "grey", fill=NA, size=1),
            panel.spacing.x = unit(3,"line")) +
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.title = element_text(size=11), # ,face="bold", 
            axis.text.x =  element_text(size = 10), #,angle = 90, vjust = 0.6, hjust=0.6
            axis.text.y =  element_text(size = 10), #, angle = 90, hjust =0.4)
            strip.text.x = element_text(size = 10))
    
    p3
  })
  
  # fig 3 output
  output$group_plot3 <- renderPlot({
    group_p3()
  })
  
  # fig 4 reactive data frame
  df_group4 <- reactive({
    df_group4 <- df_group() 
    df_group4 <- df_group4 %>%
      select(year,smear,wrd) %>%
      tidyr::gather(., key = variable, value = value, smear:wrd)
    
    df_group4b <- df_group() 
    df_group4b <- df_group4b %>%
      select(year,pct_dummy,lab_ratio) %>%
      tidyr::gather(., key = var_pct, value = pct, pct_dummy:lab_ratio)
    
    df_group4b %>%
      ungroup() %>%
      select(-year) %>% cbind.data.frame(df_group4,.) -> df_group4
    
    df_group4 <- df_group4 %>%
      mutate(pct=ifelse(var_pct=="pct_dummy",NA,pct))
  })
  
  # fig 4
  group_p4 <- reactive({
    df <- df_group4()
    
    df <- df %>%
      mutate(variable=factor(variable,labels=c("Microscopy labs","Labs with Xpert")))
    
    p4 <- df %>%
      ggplot(aes(x = as.factor(year), y = value, group=variable,fill=variable)) +
      geom_bar(position="dodge", stat = "identity", width=0.8, alpha=.79) +
      geom_line(aes(y=pct*100*(max(value)/40), group=variable),color="deeppink2",size=1) + 
      scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      scale_y_continuous(sec.axis = sec_axis(~./(max(df$value)/40), name = "Xpert/microscopy lab ratio (%)")) +
      labs(title="Fig.4 Number of diagnostic labs") +
      xlab("Year") +
      ylab("") +
      scale_fill_manual(name = NULL, values=c("dodgerblue3","goldenrod2")) +
      theme_light() +
      theme(legend.position = "top")
    
    p4 <- p4 +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold")
      ) +
      theme(strip.placement = "outside",strip.background = element_blank(),
            panel.border =  element_rect(colour = "grey", fill=NA, size=1),
            panel.spacing.x = unit(3,"line")) +
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.title = element_text(size=11), # ,face="bold", 
            axis.text.x =  element_text(size = 10), #,angle = 90, vjust = 0.6, hjust=0.6
            axis.text.y =  element_text(size = 10), #, angle = 90, hjust =0.4)
            strip.text.x = element_text(size = 10))
    
    p4
  })
  
  # fig 4 output
  output$group_plot4 <- renderPlot({
    group_p4()
  })
  
  # multiplot for download
  p_group <- reactive({
    aligned_plots <- align_plots(group_p1(),group_p2(),group_p3(),group_p4(),align="hv", axis="tblr") 
    # aligned_plots <- align_plots(p1,p2,p3,p4,p5, align="hv", axis="tblr") 
    p <- ggdraw() +
      draw_plot(group_p1(), x=0.02,y=0.50, width=0.45, height=0.45) +
      draw_plot(group_p2(), x=0.50,y=0.50, width=0.45, height=0.45) +
      draw_plot(group_p3(), x=0.02,y=0.02, width=0.45, height=0.45) +
      draw_plot(group_p4(), x=0.50,y=0.02, width=0.45, height=0.45) #+
      # draw_plot(group_p5(), x=0.02,y=0.02, width=0.45, height=0.3) 
    #  draw_plot(legend_2, x=0.5, y=0, hjust=0.5, width=1, height=0.1)
    p
    
  })
  
  output$dl_fig_reg <- downloadHandler(
    filename = function() { 
      paste0("tb_dxcascade_group_",input$group_code,"_",Sys.Date(),".pdf") },
    content = function(file) {
      ggsave(file, plot = p_group(), device = "pdf",width=22,height=22, title="TB diagnostic cascade 2015-2020")
    }
  )
  
  output$dl_dat_reg <- downloadHandler(
    filename = function() { 
      paste0("tb_dxcascade_group_",input$group_code,"_",Sys.Date(),".csv") },
    content = function(file) {
      write.csv(df_group(), file, row.names = FALSE)
    })  
  
# --------------------- Country ---------------------#
  
  # Get the latest list of countries with provisional data to use in country dropdown
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  country_list_json <- reactive({
    
    url <- paste0(json_url, "?ds=countries")
    
    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
    # group_list <- json$groups %>%
    #   select(group_code, group_description) %>%
    #   arrange(group_description)
  })
  
  # Build the select country control
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$country <- renderUI({
    
    already_selected <- input$iso2
    
    # Create a named list for selectInput
    country_list <- country_list_json()$countries %>%
      select(iso2, country) %>%
      arrange(country)
    
    country_list <- setNames(country_list[,"iso2"], country_list[,"country"])
    
    selectInput(inputId = "iso2",
                label = "",
                choices = country_list,
                # next line needed to avoid losing the selected country when the language is changed
                selected = already_selected,
                selectize = FALSE,
                width = "380px")
  })
  
  # Get the data as a JSON file for the chosen group
  # - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  cdata <- reactive({
    url <- paste0(json_url, "?ds=dxcascade_country&iso2=", input$iso2)
    
    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
  })
  
  
  # base reactive data frame
  df_country <- reactive({
    cdata <- cdata()
    df_est <- cdata$incidence 
    
    df_not <- cdata$notifications  %>%
      rowwise() %>%
      mutate(conf_rrmdr   = sum(c(conf_rrmdr,conf_rr_nfqr,conf_rr_fqr),na.rm = TRUE),
             all_labconf  = sum(c(new_labconf,ret_rel_labconf),na.rm = TRUE),
             all_pulmo    = sum(c(new_labconf,ret_rel_labconf,new_clindx,ret_rel_clindx),na.rm = TRUE),
             # rdst_all     = sum(c(rdst_new,rdst_ret,rdst_unk),na.rm = TRUE),
             conf_rrmdr_tx= sum(c(unconf_rrmdr_tx,conf_rrmdr_tx,unconf_rr_nfqr_tx,conf_rr_nfqr_tx,conf_rr_fqr_tx),na.rm = TRUE))
    
    df_lab <- cdata$labs %>%
      rowwise() %>%
      mutate(wrd=sum(c(xpert,m_wrd),na.rm = TRUE))
    
    df_out <- cdata$outcomes %>%
      subset(year>2014) 
    
    df_out %>%
      full_join(df_lab,by=c('year')) -> df
    df_not %>%
      right_join(df,by=c('year')) -> df
    df_est %>%
      right_join(df,by=c('year')) -> df
    
    df_country <- df %>%
      mutate(pct_dummy=e_inc_num/e_inc_num,
             not_rate=c_newinc/e_inc_num,
             labconf_rate=all_labconf/all_pulmo,
             rapid_rate=newinc_rdx/c_newinc,
             mdr_tx_rate=conf_rrmdr_tx/conf_rrmdr,
             lab_ratio=wrd/smear)
    
  })
  
  # fig 1 reactive data frame
  df_country1 <- reactive({
    df_country1 <- df_country() 
    df_country1 <- df_country1 %>%
      select(year,e_inc_num,c_newinc,all_labconf,newinc_rdx) %>%
      tidyr::gather(., key = variable, value = value, e_inc_num:newinc_rdx)
    
    df_country1b <- df_country() 
    df_country1b <- df_country1b %>%
      select(year,pct_dummy,not_rate,labconf_rate,rapid_rate) %>%
      tidyr::gather(., key = var_pct, value = pct, pct_dummy:rapid_rate)
    
    df_country1b %>%
      ungroup() %>%
      select(-year) %>% cbind.data.frame(df_country1,.) -> df_country1
    
  })
  
  # fig 1 
  country_p1 <- reactive({
    df <- df_country1()
    
    df <- df %>%
      mutate(variable=factor(variable,labels=c("Lab-confirmed**","Notified*","Estimated incidence","Rapid diagnosis at time of diagnosis***")),
             variable=factor(variable,levels=c("Estimated incidence","Notified*","Lab-confirmed**","Rapid diagnosis at time of diagnosis***"))) %>%
      mutate(textcol=ifelse(var_pct=="rapid_rate","B","W"))
    
    p1 <- df %>%
      ggplot(aes(x = as.factor(year), y = value, fill=variable, group=variable)) +
      geom_bar(position="dodge", stat = "identity", width=0.9, alpha=.79) +
      geom_text(aes(y=max(ifelse(is.na(value),0,value))/10, label = paste0(round(ifelse(is.na(pct),0,pct)*100,0),"%"), group=variable,color= textcol), 
                position = position_dodge(width = 0.9), size = 3, 
                vjust=2) +
      scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      # scale_y_continuous(labels = scales::comma) +
      # scale_y_continuous(breaks=c(0, 2500000, 5000000, 7500000, 10000000), labels = c("0", "2.5M", "5M", "7.5M", "10M")) +
      labs(title="Fig.1 Diagnostic cascade for all forms of TB in 2015-2020") +
      xlab("Year") +
      ylab(NULL) +
      scale_fill_manual(name = NULL, values=c("darkblue","dodgerblue3","green3","goldenrod3")) +
      scale_color_manual(values = c('W' = 'white', 'B' = 'black'), guide = "none")+
      theme_light() +
      theme(legend.position = "top")
    
    p1 <- p1 +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold")
      ) +
      theme(strip.placement = "outside",strip.background = element_blank(),
            panel.border =  element_rect(colour = "grey", fill=NA, size=1),
            panel.spacing.x = unit(3,"line")) +
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.title = element_text(size=11), # ,face="bold", 
            axis.text.x =  element_text(size = 10), #,angle = 90, vjust = 0.6, hjust=0.6
            axis.text.y =  element_text(size = 10), #, angle = 90, hjust =0.4)
            strip.text.x = element_text(size = 10))
    
    p1
  })
  
  # fig 1 output
  output$country_plot1 <- renderPlot({
    country_p1()
  })
  
  # fig 2 reactive data frame
  df_country2 <- reactive({
    df_country2 <- df_country() 
    df_country2 <- df_country2 %>%
      select(year,conf_rrmdr,conf_rrmdr_tx) %>%
      tidyr::gather(., key = variable, value = value, conf_rrmdr:conf_rrmdr_tx)
    
    df_country2b <- df_country() 
    df_country2b <- df_country2b %>%
      select(year,pct_dummy,mdr_tx_rate) %>%
      tidyr::gather(., key = var_pct, value = pct, pct_dummy:mdr_tx_rate)
    
    df_country2b %>%
      ungroup() %>%
      select(-year) %>% cbind.data.frame(df_country2,.) -> df_country2
    
  })
  
  # fig 2
  country_p2 <- reactive({
    df <- df_country2()
    
    df <- df %>%
      mutate(variable=factor(variable,labels=c("Detected MDR-TB","MDR-TB placed on treatment")))%>%
      mutate(textcol=ifelse(var_pct=="rapid_rate","B","W"))
    
    p2 <- df %>%
      ggplot(aes(x = as.factor(year), y = value, fill=variable, group=variable)) +
      geom_bar(position="dodge", stat = "identity", width=0.8, alpha=.79) +
      geom_text(aes(y=max(value)/10, label = paste0(round(pct*100,0),"%"), group=variable, color=textcol), 
                position = position_dodge(width = 0.8), size = 3, color= "white",
                vjust=2) +
      scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      # scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000), labels = c("0", "50K", "100K", "150K", "200K")) +
      labs(title="Fig.2 Diagnostic cascade for MDR-TB in 2015-2020") +
      xlab("Year") +
      ylab(NULL) +
      scale_fill_manual(name = NULL, values=c("dodgerblue3","green3")) +
      scale_color_manual(values = c('W' = 'white', 'B' = 'black'), guide = "none")+
      theme_light() +
      theme(legend.position = "top")
    
    p2 <- p2 +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold")
      ) +
      theme(strip.placement = "outside",strip.background = element_blank(),
            panel.border =  element_rect(colour = "grey", fill=NA, size=1),
            panel.spacing.x = unit(3,"line")) +
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.title = element_text(size=11), # ,face="bold", 
            axis.text.x =  element_text(size = 10), #,angle = 90, vjust = 0.6, hjust=0.6
            axis.text.y =  element_text(size = 10), #, angle = 90, hjust =0.4)
            strip.text.x = element_text(size = 10))
    
    p2
  })
  
  # fig 2 output
  output$country_plot2 <- renderPlot({
    country_p2()
  })
  
  # fig 3 reactive data frame
  df_country3 <- reactive({
    df_country3 <- df_country() 
    df_country3 <- df_country3 %>%
      select(year,c_new_tsr,c_mdr_tsr) %>%
      tidyr::gather(., key = variable, value = value, c_new_tsr:c_mdr_tsr)
  })
  
  # fig 3
  country_p3 <- reactive({
    df <- df_country3()
    
    df <- df %>%
      mutate(variable=factor(variable,labels=c("MDR-TB","TB")),
             variable=factor(variable,levels=c("TB","MDR-TB")))
    
    p3 <- df %>%
      ggplot(aes(x = as.factor(year), y = value, group=variable,colour=variable)) +
      geom_line(size=1) +
      geom_point(size=3) +
      geom_text(aes(y=value, label = paste0(value,"%"), group=variable), 
                position = position_dodge(width = 0.8), size = 3, color="black",
                vjust=-2) + 
      ylim(40,100) +
      labs(title="Fig.3 Treatment success rate in 2015-2020") +
      xlab("Year") +
      ylab("%") +
      scale_colour_manual(name = NULL, values=c("dodgerblue3","green3")) +
      theme_light() +
      theme(legend.position = "top")
    
    p3 <- p3 +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold")
      ) +
      theme(strip.placement = "outside",strip.background = element_blank(),
            panel.border =  element_rect(colour = "grey", fill=NA, size=1),
            panel.spacing.x = unit(3,"line")) +
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.title = element_text(size=11), # ,face="bold", 
            axis.text.x =  element_text(size = 10), #,angle = 90, vjust = 0.6, hjust=0.6
            axis.text.y =  element_text(size = 10), #, angle = 90, hjust =0.4)
            strip.text.x = element_text(size = 10))
    
    p3
  })
  
  # fig 3 output
  output$country_plot3 <- renderPlot({
    country_p3()
  })
  
  # fig 4 reactive data frame
  df_country4 <- reactive({
    df_country4 <- df_country() 
    df_country4 <- df_country4 %>%
      select(year,smear,wrd) %>%
      tidyr::gather(., key = variable, value = value, smear:wrd)
    
    df_country4b <- df_country() 
    df_country4b <- df_country4b %>%
      select(year,pct_dummy,lab_ratio) %>%
      tidyr::gather(., key = var_pct, value = pct, pct_dummy:lab_ratio)
    
    df_country4b %>%
      ungroup() %>%
      select(-year) %>% cbind.data.frame(df_country4,.) -> df_country4
    
    df_country4 <- df_country4 %>%
      mutate(pct=ifelse(var_pct=="pct_dummy",NA,pct))
  })
  
  # fig 4
  country_p4 <- reactive({
    df <- df_country4()
    
    df <- df %>%
      mutate(variable=factor(variable,labels=c("Microscopy labs","Labs with Xpert")))
    
    p4 <- df %>%
      ggplot(aes(x = as.factor(year), y = value, group=variable,fill=variable)) +
      geom_bar(position="dodge", stat = "identity", width=0.8, alpha=.79) +
      geom_line(aes(y=pct*100*(max(value)/40), group=variable),color="deeppink2",size=1) + 
      scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      scale_y_continuous(sec.axis = sec_axis(~./(max(df$value)/40), name = "Xpert/microscopy lab ratio (%)")) +
      labs(title="Fig.4 Number of diagnostic labs") +
      xlab("Year") +
      ylab("") +
      scale_fill_manual(name = NULL, values=c("dodgerblue3","goldenrod2")) +
      theme_light() +
      theme(legend.position = "top")
    
    p4 <- p4 +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold")
      ) +
      theme(strip.placement = "outside",strip.background = element_blank(),
            panel.border =  element_rect(colour = "grey", fill=NA, size=1),
            panel.spacing.x = unit(3,"line")) +
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.title = element_text(size=11), # ,face="bold", 
            axis.text.x =  element_text(size = 10), #,angle = 90, vjust = 0.6, hjust=0.6
            axis.text.y =  element_text(size = 10), #, angle = 90, hjust =0.4)
            strip.text.x = element_text(size = 10))
    
    p4
  })
  
  # fig 4 output
  output$country_plot4 <- renderPlot({
    country_p4()
  })
  
  # multiplot for download
  p_country <- reactive({
    aligned_plots <- align_plots(country_p1(),country_p2(),country_p3(),country_p4(), align="hv", axis="tblr") 
    # aligned_plots <- align_plots(p1,p2,p3,p4,p5, align="hv", axis="tblr") 
    p <- ggdraw() +
      draw_plot(country_p1(), x=0.02,y=0.50, width=0.45, height=0.45) +
      draw_plot(country_p2(), x=0.50,y=0.50, width=0.45, height=0.45) +
      draw_plot(country_p3(), x=0.02,y=0.02, width=0.45, height=0.45) +
      draw_plot(country_p4(), x=0.50,y=0.02, width=0.45, height=0.45) 
    p
    
  })
  
  output$dl_fig_con <- downloadHandler(
    filename = function() { 
      paste0("tb_dxcascade_country_",input$iso2,"_",Sys.Date(),".pdf") },
    content = function(file) {
      ggsave(file, plot = p_country(), device = "pdf",width=22,height=22, title="TB diagnostic cascade 2015-2020")
    }
  )
  
  output$dl_dat_con <- downloadHandler(
    filename = function() { 
      paste0("tb_dxcascade_country_",input$iso2,"_",Sys.Date(),".csv") },
    content = function(file) {
      write.csv(df_country(), file, row.names = FALSE)
    })  
  

}

# Run the application
shinyApp(ui = ui, server = server)

  
