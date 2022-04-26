#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw
shinyServer(function(input, output) {
  
  # FUNCION SELECCION DE MONEDA ----
  pais_func <- reactive({
    if (input$zona_selec == "A"){
      pais_selec <- "Zona A"
    }
    if (input$zona_selec == "B"){
      pais_selec <- "Zona B"
    }
    if (input$zona_selec == "C"){
      pais_selec <- "Zona C"
    }
    if (input$zona_selec == "D"){
      pais_selec <- "Zona D"
    }
    if (input$zona_selec == "E"){
      pais_selec <- "Zona E"
    }
    
    return(pais_selec)
  })
  
  # FUNCION CALCULO DE OUTLIERS----
  outliers_estad_func <- reactive({
    #outliers estadisticos
    outliers_estad <- data.frame()
    for (i in 45:58) {
      outliers <- boxplot.stats(filter(data_facturacion, zona == input$zona_selec, nivel == as.numeric(paste0(i)))$facturacion_anual)$out
      if (length(outliers)>0){
        for (j in 1:length(outliers)) {
          outliers_estad <- rbind(outliers_estad, filter(data_facturacion, nivel == as.numeric(paste0(i)), facturacion_anual == outliers[j]))
        }
      }
    }
    outliers_estad <- outliers_estad[ , -c(12, 13, 14)] #quito las columnas de logaritmos
    
    return(outliers_estad)
  })
  
  # FUNCION LIMITES Y CALCULO CANTIDADES DE FRANQUICIAS SEGUN CUANTILES Y FUERA DE LIMITES: LIMITES ACTUALES----
  lista_orig_func <- reactive({
    req(input$carga_orig)
    archivo_orig <- input$carga_orig
    if (is.null(archivo_orig)) { return(NULL) } 
    data_original <- read_excel(archivo_orig$datapath)
    
    colnames(data_original) <- c("nivel", "piso", "q1_q2", "mediana_teo", "q3_q4", "tope")
    
    #Limites teoricos (limites teoricos de tas segun nivel)
    data_original <- data_original %>% filter(nivel >= 1, nivel <= 14)
    
    limites_teoricos <- data_original %>%
      dplyr::select(nivel, piso, mediana_teo, tope)
    
    #valores fuera de escala teorica
    #fuera_escala <- data.frame()
    bajo_piso_orig <- data.frame()
    cuantil1_orig <- data.frame()
    cuantil2_orig <- data.frame()
    cuantil3_orig <- data.frame()
    cuantil4_orig <- data.frame()
    sobre_tope_orig <- data.frame()
    for (i in data_original[[1, "nivel"]]:data_original[[nrow(data_original), "nivel"]]) {
      bp_orig <- filter(data_escalas, zona == input$zona_selec, nivel == i,
                        facturacion_anual < data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "piso"]])
      q1_orig <- filter(data_escalas, zona == input$zona_selec, nivel == i,
                        facturacion_anual > data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "piso"]],
                        facturacion_anual < data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "q1_q2"]])
      q2_orig <- filter(data_escalas, zona == input$zona_selec, nivel == i,
                        facturacion_anual > data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "q1_q2"]],
                        facturacion_anual < data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "mediana_teo"]])
      q3_orig <- filter(data_escalas, zona == input$zona_selec, nivel == i,
                        facturacion_anual > data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "mediana_teo"]],
                        facturacion_anual < data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "q3_q4"]])
      q4_orig <- filter(data_escalas, zona == input$zona_selec, nivel == i,
                        facturacion_anual > data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "q3_q4"]],
                        facturacion_anual < data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "tope"]])
      st_orig <- filter(data_escalas, zona == input$zona_selec, nivel == i,
                        facturacion_anual > data_original[[(i+1-limites_teoricos[[1, "nivel"]]), "tope"]])
      #fuera_escala <- rbind(fuera_escala, bajo_piso, sobre_tope)
      bajo_piso_orig <- rbind(bajo_piso_orig, bp_orig)
      cuantil1_orig <- rbind(cuantil1_orig, q1_orig)
      cuantil2_orig <- rbind(cuantil2_orig, q2_orig)
      cuantil3_orig <- rbind(cuantil3_orig, q3_orig)
      cuantil4_orig <- rbind(cuantil4_orig, q4_orig)
      sobre_tope_orig <- rbind(sobre_tope_orig, st_orig)
    }
    # cant fuera de escala teo segun nivel
    #cant_fuera_orig <- fuera_escala %>% group_by(nivel) %>% summarize(cantidad_fuera_orig=n())
    cant_bajo_piso_orig <- bajo_piso_orig %>% group_by(nivel) %>% summarize(`Bajo piso`=n())
    cant_cuantil1_orig <- cuantil1_orig %>% group_by(nivel) %>% summarize(`Q1 teorico`=n())
    cant_cuantil2_orig <- cuantil2_orig %>% group_by(nivel) %>% summarize(`Q2 teorico`=n())
    cant_cuantil3_orig <- cuantil3_orig %>% group_by(nivel) %>% summarize(`Q3 teorico`=n())
    cant_cuantil4_orig <- cuantil4_orig %>% group_by(nivel) %>% summarize(`Q4 teorico`=n())
    cant_sobre_tope_orig <- sobre_tope_orig %>% group_by(nivel) %>% summarize(`Sobre tope`=n())
    
    nivel_df <- data.frame("nivel" = nivel_vector)
    
    #armo una tabla de contingencia nivel vs cuantil (Q)
    tabla_contingencia_orig <- left_join(nivel_df, cant_bajo_piso_orig) %>% left_join(cant_cuantil1_orig) %>%
      left_join(cant_cuantil2_orig) %>% left_join(cant_cuantil3_orig) %>% left_join(cant_cuantil4_orig) %>% left_join(cant_sobre_tope_orig)
    row.names(tabla_contingencia_orig) <- tabla_contingencia_orig$nivel
    tabla_contingencia_orig$nivel <- NULL
    
    #calculo distancia (%) al piso y al tope de la nivel (de la original) para los bajo piso y los sobre tope
    bajo_piso_orig <- cbind(bajo_piso_orig, data.frame("porcen_vs_piso_nivel" = 1:nrow(bajo_piso_orig), "porcen_vs_tope_nivel" = 1:nrow(bajo_piso_orig)))
    sobre_tope_orig <- cbind(sobre_tope_orig, data.frame("porcen_vs_piso_nivel" = 1:nrow(sobre_tope_orig), "porcen_vs_tope_nivel" = 1:nrow(sobre_tope_orig)))
    
    limites_teoricos2 <- tibble::column_to_rownames(limites_teoricos, var = "nivel")
    
    for (k in 1:nrow(bajo_piso_orig)) {
      bajo_piso_orig[[k , "porcen_vs_piso_nivel"]] <- round(((limites_teoricos2[[bajo_piso_orig[[k , "nivel"]], "piso"]]/bajo_piso_orig[[k , "facturacion_anual"]])-1)*(-1), 4)
      bajo_piso_orig[[k , "porcen_vs_tope_nivel"]] <- round(((limites_teoricos2[[bajo_piso_orig[[k , "nivel"]], "tope"]]/bajo_piso_orig[[k , "facturacion_anual"]])-1)*(-1), 4)
    }
    for (k in 1:nrow(sobre_tope_orig)) {
      sobre_tope_orig[[k , "porcen_vs_piso_nivel"]] <- round(((sobre_tope_orig[[k , "facturacion_anual"]]/limites_teoricos2[[sobre_tope_orig[[k , "nivel"]], "piso"]])-1), 4)
      sobre_tope_orig[[k , "porcen_vs_tope_nivel"]] <- round(((sobre_tope_orig[[k , "facturacion_anual"]]/limites_teoricos2[[sobre_tope_orig[[k , "nivel"]], "tope"]])-1), 4)
    }
    
    lista_original <- list("data_original" = data_original, 
                           "limites_teoricos" = limites_teoricos, 
                           "bajo_piso_orig" = bajo_piso_orig, 
                           "cuantil1_orig" = cuantil1_orig, 
                           "cuantil2_orig" = cuantil2_orig,
                           "cuantil3_orig" = cuantil3_orig,
                           "cuantil4_orig" = cuantil4_orig,
                           "sobre_tope_orig" = sobre_tope_orig,
                           "tabla_contingencia_orig" = tabla_contingencia_orig)
    
    return(lista_original)
    
  })
  
  # FUNCION LIMITES Y CALCULO CANTIDADES DE FRANQUICIAS SEGUN CUANTILES Y FUERA DE LIMITES: LIMITES PROPUESTOS----
  lista_prop_func <- reactive({
    req(input$carga_prop)
    archivo_prop <- input$carga_prop
    if (is.null(archivo_prop)) {return(NULL)} 
    data_propuesta <- read_excel(archivo_prop$datapath)
    
    #Nuevas escalas teoricas PROPUESTAS (límites teo de tas segun categoria)
    data_propuesta <- data_propuesta %>% filter(nivel >= 1, nivel <= 14)# %>%
    #  rename(piso_prop = `1er Q piso`, q1_q2_prop = `1er Q/  2do Q`, mediana_teo_prop = `Mediana (2Q/3Q)`, q3_q4_prop = `3er Q / 4to Q`, tope_prop = `4to Q tope`)
    
    colnames(data_propuesta) <- c("nivel", "piso_prop", "q1_q2_prop", "mediana_teo_prop", "q3_q4_prop", "tope_prop")
    
    limites_teo_prop <- data_propuesta %>%
      dplyr::select(nivel, piso_prop, mediana_teo_prop, tope_prop)
    
    #valores fuera de limites teoricos PROPUESTOS
    #fuera_escala_prop <- data.frame()
    bajo_piso_prop <- data.frame()
    cuantil1_prop <- data.frame()
    cuantil2_prop <- data.frame()
    cuantil3_prop <- data.frame()
    cuantil4_prop <- data.frame()
    sobre_tope_prop <- data.frame()
    for (i in data_propuesta[[1, "nivel"]]:data_propuesta[[nrow(data_propuesta), "nivel"]]) {
      bp_prop <- filter(data_escalas, pais == input$zona_selec, nivel == i,
                        facturacion_anual < data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "piso_prop"]])
      q1_prop <- filter(data_escalas, pais == input$zona_selec, nivel == i,
                        facturacion_anual > data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "piso_prop"]],
                        facturacion_anual < data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "q1_q2_prop"]])
      q2_prop <- filter(data_escalas, pais == input$zona_selec, nivel == i,
                        facturacion_anual > data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "q1_q2_prop"]],
                        facturacion_anual < data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "mediana_teo_prop"]])
      q3_prop <- filter(data_escalas, pais == input$zona_selec, nivel == i,
                        facturacion_anual > data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "mediana_teo_prop"]],
                        facturacion_anual < data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "q3_q4_prop"]])
      q4_prop <- filter(data_escalas, pais == input$zona_selec, nivel == i,
                        facturacion_anual > data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "q3_q4_prop"]],
                        facturacion_anual < data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "tope_prop"]])
      st_prop <- filter(data_escalas, pais == input$zona_selec, nivel == i,
                        facturacion_anual > data_propuesta[[(i+1-limites_teo_prop[[1, "nivel"]]), "tope_prop"]])
      #fuera_escala <- rbind(fuera_escala, bajo_piso, sobre_tope)
      bajo_piso_prop <- rbind(bajo_piso_prop, bp_prop)
      cuantil1_prop <- rbind(cuantil1_prop, q1_prop)
      cuantil2_prop <- rbind(cuantil2_prop, q2_prop)
      cuantil3_prop <- rbind(cuantil3_prop, q3_prop)
      cuantil4_prop <- rbind(cuantil4_prop, q4_prop)
      sobre_tope_prop <- rbind(sobre_tope_prop, st_prop)
    }
    # cant fuera de escala teo segun nivel
    #cant_fuera_orig <- fuera_escala %>% group_by(nivel) %>% summarize(cantidad_fuera_orig=n())
    cant_bajo_piso_prop <- bajo_piso_prop %>% group_by(nivel) %>% summarize(`Bajo piso`=n())
    cant_cuantil1_prop <- cuantil1_prop %>% group_by(nivel) %>% summarize(`Q1 teorico`=n())
    cant_cuantil2_prop <- cuantil2_prop %>% group_by(nivel) %>% summarize(`Q2 teorico`=n())
    cant_cuantil3_prop <- cuantil3_prop %>% group_by(nivel) %>% summarize(`Q3 teorico`=n())
    cant_cuantil4_prop <- cuantil4_prop %>% group_by(nivel) %>% summarize(`Q4 teorico`=n())
    cant_sobre_tope_prop <- sobre_tope_prop %>% group_by(nivel) %>% summarize(`Sobre tope`=n())
    
    nivel_df <- data.frame("nivel" = nivel_vector)
    
    #armo una tabla de contingencia nivel vs cuantil
    tabla_contingencia_prop <- left_join(nivel_df, cant_bajo_piso_prop) %>% left_join(cant_cuantil1_prop) %>%
      left_join(cant_cuantil2_prop) %>% left_join(cant_cuantil3_prop) %>% left_join(cant_cuantil4_prop) %>% left_join(cant_sobre_tope_prop)
    row.names(tabla_contingencia_prop) <- tabla_contingencia_prop$nivel
    tabla_contingencia_prop$nivel <- NULL
    
    #calculo distancia (%) al piso y al tope de la nivel (de la original) para los bajo piso y los sobre tope
    bajo_piso_prop <- cbind(bajo_piso_prop, data.frame("porcen_vs_piso_nivel" = 1:nrow(bajo_piso_prop), "porcen_vs_tope_nivel" = 1:nrow(bajo_piso_prop)))
    sobre_tope_prop <- cbind(sobre_tope_prop, data.frame("porcen_vs_piso_nivel" = 1:nrow(sobre_tope_prop), "porcen_vs_tope_nivel" = 1:nrow(sobre_tope_prop)))
    
    limites_teo_prop2 <- tibble::column_to_rownames(limites_teo_prop, var = "nivel")
    
    for (k in 1:nrow(bajo_piso_prop)) {
      bajo_piso_prop[[k , "porcen_vs_piso_nivel"]] <- round(((limites_teo_prop2[[bajo_piso_prop[[k , "nivel"]], "piso_prop"]]/bajo_piso_prop[[k , "facturacion_anual"]])-1)*(-1), 4)
      bajo_piso_prop[[k , "porcen_vs_tope_nivel"]] <- round(((limites_teo_prop2[[bajo_piso_prop[[k , "nivel"]], "tope_prop"]]/bajo_piso_prop[[k , "facturacion_anual"]])-1)*(-1), 4)
    }
    for (k in 1:nrow(sobre_tope_prop)) {
      sobre_tope_prop[[k , "porcen_vs_piso_nivel"]] <- round(((sobre_tope_prop[[k , "facturacion_anual"]]/limites_teo_prop2[[sobre_tope_prop[[k , "nivel"]], "piso_prop"]])-1), 4)
      sobre_tope_prop[[k , "porcen_vs_tope_nivel"]] <- round(((sobre_tope_prop[[k , "facturacion_anual"]]/limites_teo_prop2[[sobre_tope_prop[[k , "nivel"]], "tope_prop"]])-1), 4)
    }
    
    lista_propuesta <- list("data_propuesta" = data_propuesta,
                            "limites_teo_prop" = limites_teo_prop,
                            "bajo_piso_prop" = bajo_piso_prop,
                            "cuantil1_prop" = cuantil1_prop, 
                            "cuantil2_prop" = cuantil2_prop,
                            "cuantil3_prop" = cuantil3_prop,
                            "cuantil4_prop" = cuantil4_prop,
                            "sobre_tope_prop" = sobre_tope_prop,
                            "tabla_contingencia_prop" = tabla_contingencia_prop)
    
    return(lista_propuesta)
  })
  
  # CALCULOS PARA LA TABLA INICIAL: PROGRESIONES, RELACIONES, TRANSF DEL MERCADO (SIN incluir transf a regresionado)----
  transfo_inicial_func1 <-reactive({
    
    #comparacion y progresiones de los limites actuales vs propuestos
    req(input$carga_orig)
    archivo_orig <- input$carga_orig
    if (is.null(archivo_orig)) { return(NULL) } 
    data_original <- read_excel(archivo_orig$datapath)
    req(input$carga_prop)
    archivo_prop <- input$carga_prop
    if (is.null(archivo_prop)) {return(NULL)} 
    data_propuesta <- read_excel(archivo_prop$datapath)
    
    nivel_inicial <- 45
    nivel_final <- 58
    
    data_original <- dplyr::filter(data_original, nivel >= nivel_inicial, nivel <= nivel_final)
    data_propuesta <- dplyr::filter(data_propuesta, nivel >= nivel_inicial, nivel <= nivel_final)
    
    data_original <- dplyr::rename(data_original, "mediana_actual" = "Mediana (2Q/3Q)")
    data_propuesta <- dplyr::rename(data_propuesta, "mediana_propuesta" = "Mediana (2Q/3Q)")
    
    progresion_actual <- c(NA)
    for (l in 1:(length(data_original$mediana_actual)-1)) {
      progresion_actual <- c(progresion_actual, (data_original$mediana_actual[l+1]/data_original$mediana_actual[l])-1)
    }
    progresion_prop <- c(NA)
    for (l in 1:(length(data_propuesta$mediana_propuesta)-1)) {
      progresion_prop <- c(progresion_prop, (data_propuesta$mediana_propuesta[l+1]/data_propuesta$mediana_propuesta[l])-1)
    }
    
    tabla_comparacion_inicial <- data.frame("nivel" = nivel_inicial:nivel_final, "mediana_actual" = data_original$mediana_actual, "progresion_actual" = progresion_actual,
                                            "mediana_propuesta" = data_propuesta$mediana_propuesta, "progresion_propuesta" = progresion_prop)
    
    tabla_comparacion_inicial <- mutate(tabla_comparacion_inicial, porcent_ajuste_prop_vs_actual = (mediana_propuesta/mediana_actual)-1)
    
    #los datos de mercado crudo (raw), los transformo a nivel de la empresa (tranf. midpoint raw)
    req(input$carga_mercado)
    archivo_mercado <- input$carga_mercado
    if (!is.null(archivo_mercado)) {
      data_mercado <- read_excel(archivo_mercado$datapath)
      
      data_mercado <- data_mercado %>% select(clase, mediana_mercado) %>% rename(nivel = PositionClass, mediana_mercado_crudo = TotalCashTarget_Median_OW)
      
      data_mercado$nivel <- as.numeric(data_mercado$nivel)
      data_mercado$mediana_mercado_crudo <- as.numeric(data_mercado$mediana_mercado_crudo)
      data_mercado <- dplyr::filter(data_mercado, nivel >= nivel_inicial, nivel <= nivel_final)
      
      #afecto los datos de mercado crudo segun los porcentajes de aumento de input de cada grupo
      data_mercado <- data_mercado %>%
        mutate(midpoint_raw = ifelse(nivel <= 51, mediana_mercado_crudo*(1+input$porc_g1/100), 
                                     ifelse(nivel <= 54, mediana_mercado_crudo*(1+input$porc_g2/100),
                                            ifelse(nivel <= 58, mediana_mercado_crudo*(1+input$porc_g3/100),
                                                   mediana_mercado_crudo*(1+input$porc_g4/100)))))
      
      #transformo a nivel de la empresa
      data_mercado <- data_mercado %>%
        mutate(atc_midpoint_raw = ifelse(nivel <= 48, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c1/100)+(1/(1+(input$coef_d_m4a4/100)))*(input$coef_d_m4a4/100)*(input$coef_b/100)))*(1+(input$env/100)),
                                         ifelse(nivel <= 51, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c1/100)+(1/(1+(input$coef_d_5a7/100)))*(input$coef_d_5a7/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                ifelse(nivel == 52, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c2/100)+(1/(1+(input$coef_d_8/100)))*(input$coef_d_8/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                       ifelse(nivel == 53, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c2/100)+(1/(1+(input$coef_d_9/100)))*(input$coef_d_9/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                              ifelse(nivel == 54, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c2/100)+(1/(1+(input$coef_d_10/100)))*(input$coef_d_10/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                     ifelse(nivel == 55, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c3/100)+(1/(1+(input$coef_d_11/100)))*(input$coef_d_11/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                            ifelse(nivel == 56, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c3/100)+(1/(1+(input$coef_d_12/100)))*(input$coef_d_12/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                   ifelse(nivel == 57, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c3/100)+(1/(1+(input$coef_d_13/100)))*(input$coef_d_13/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                          ifelse(nivel == 58, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c3/100)+(1/(1+(input$coef_d_14/100)))*(input$coef_d_14/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                                 ifelse(nivel == 59, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c4/100)+(1/(1+(input$coef_d_15/100)))*(input$coef_d_15/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                                        ifelse(nivel == 60, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c4/100)+(1/(1+(input$coef_d_16/100)))*(input$coef_d_16/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                                               ifelse(nivel == 61, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c4/100)+(1/(1+(input$coef_d_17/100)))*(input$coef_d_17/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                                                      ifelse(nivel == 62, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c4/100)+(1/(1+(input$coef_d_18/100)))*(input$coef_d_18/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                                                             ifelse(nivel == 63, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c4/100)+(1/(1+(input$coef_d_19/100)))*(input$coef_d_19/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                                                                    ifelse(nivel == 64, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c4/100)+(1/(1+(input$coef_d_20/100)))*(input$coef_d_20/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                                                                           ifelse(nivel == 65, (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c4/100)+(1/(1+(input$coef_d_21/100)))*(input$coef_d_21/100)*(input$coef_b/100)))*(1+(input$env/100)), 
                                                                                                                                                  (midpoint_raw/(1+(input$coef_a/100)+(input$coef_c4/100)+(1/(1+(input$coef_d_22/100)))*(input$coef_d_22/100)*(input$coef_b/100)))*(1+(input$aging/100)))))))))))))))))))
      
      
      tabla_comparacion_inicial <- left_join(tabla_comparacion_inicial, data_mercado)
      
      #calculo la variación de la propuesta respecto a la mediana del mercado crudo
      tabla_comparacion_inicial <- tabla_comparacion_inicial %>%
        mutate(porp_vs_atc_midpoint_raw = (mediana_propuesta/atc_midpoint_raw)-1)
      
      # selecciono todas las columnas menos mediana_mercado_crudo que ya no me sirve
      tabla_comparacion_inicial <- tabla_comparacion_inicial %>% select(nivel, mediana_actual, progresion_actual, mediana_propuesta, progresion_propuesta, porcent_ajuste_prop_vs_actual, midpoint_raw, transf_midpoint_raw, porp_vs_transf_midpoint_raw)
      
      
      return(tabla_comparacion_inicial)
    }
    
  })
  
  # CALCULOS PARA TRANSFORMACION A MERCADO REGRESIONADO (INCLUYENDO CORTES)----
  transfo_inicial_func2 <- reactive({
    tabla_comparacion_inicial <- transfo_inicial_func1()
    
    for (k in 1:nrow(tabla_comparacion_inicial)) {
      tabla_comparacion_inicial[[ k, "nivel"]] <- (nivel_inicial + k - 1)
    }
    
    corte_1 <- input$rango_corte_1
    corte_2 <- input$rango_corte_2
    corte_3 <- input$rango_corte_3
    rango_corte_1 <- corte_1[1]:corte_1[2]
    #filas_corte_1 <- 1:length(rango_corte_1)
    #filas_corte_2 <- (length(filas_corte_1)+1):(length(filas_corte_1)+length(rango_corte_2))
    #filas_corte_3 <- ((length(filas_corte_1)+length(rango_corte_2))+1):((length(filas_corte_1)+length(rango_corte_2))+length(rango_corte_3))
    
    # Caso sin poner corte 2 ni 3, solo 1
    if ((is.null(corte_2)) & (is.null(corte_3))){
      if (input$modelo_selec == "Modelo Exponencial"){
        modelo1 <- lm(log(transf_midpoint_raw) ~ nivel, data = tabla_comparacion_inicial)
        tabla_comparacion_inicial <- cbind(tabla_comparacion_inicial, data.frame("mercado_regresionado" = exp(modelo1$fitted.values)))
        resumen1 <- summary(modelo1)
        R2_1 <- resumen1$r.squared
        F_obs_1 <- resumen1$fstatistic["value"]
        gl_1 <- resumen1$fstatistic["dendf"]
        valorp_global_1 <- 1-pf(F_obs_1, df1 = length(modelo1$coefficients)-1, df2 = gl_1)
      }
      
      if (input$modelo_selec ==  "Modelo Cuadrático"){
        modelo1 <- lm(transf_midpoint_raw ~ nivel + I(categoria^2), data = tabla_comparacion_inicial)
        tabla_comparacion_inicial <- cbind(tabla_comparacion_inicial, data.frame("mercado_regresionado" = modelo1$fitted.values))
        resumen1 <- summary(modelo1)
        R2_1 <- resumen1$r.squared
        F_obs_1 <- resumen1$fstatistic["value"]
        gl_1 <- resumen1$fstatistic["dendf"]
        valorp_global_1 <- 1-pf(F_obs_1, df1 = length(modelo1$coefficients)-1, df2 = gl_1)
      }
      
      #calculo la variación de la propuesta respecto a la mediana del mercado regresionado
      tabla_comparacion_inicial <- tabla_comparacion_inicial %>%
        mutate(porp_vs_atc_midpoint_regressed = (mediana_propuesta/mercado_regresionado)-1)
      
      return(list("tabla_comparacion_inicial" = tabla_comparacion_inicial,
                  "Cortes" = paste0("corte 1"),
                  "R2" = R2_1,
                  "valorp_global" = valorp_global_1))
    }
    
    # Caso poniendo cortes 1 y 2, sin 3
    if ((!is.null(corte_2)) & is.null(corte_3)){
      rango_corte_2 <- corte_2[1]:corte_2[2]
      if (TRUE %in% (rango_corte_1 %in% rango_corte_2)){
        #hay superposicion de los cortes si se cumple esto
        tabla_comparacion_inicial <- cbind(tabla_comparacion_inicial, data.frame("mercado_regresionado" = rep(0, times = nrow(tabla_comparacion_inicial))))
        return(list("tabla_comparacion_inicial" = tabla_comparacion_inicial,
                    "mensaje_error" = "HAY SUPERPOSICIÓN EN LOS CORTES"))
      }
      else {
        if (input$modelo_selec == "Modelo Exponencial"){
          #corte 1
          tabla_comparacion_inicial_corte_1 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_1)
          modelo1 <- lm(log(transf_midpoint_raw) ~ nivel, data = tabla_comparacion_inicial_corte_1)
          resumen1 <- summary(modelo1)
          R2_1 <- resumen1$r.squared
          F_obs_1 <- resumen1$fstatistic["value"]
          gl_1 <- resumen1$fstatistic["dendf"]
          valorp_global_1 <- 1-pf(F_obs_1, df1 = length(modelo1$coefficients)-1, df2 = gl_1)
          tabla_comparacion_inicial_corte_1 <- cbind(tabla_comparacion_inicial_corte_1, data.frame("mercado_regresionado" = exp(modelo1$fitted.values)))
          
          #corte 2
          tabla_comparacion_inicial_corte_2 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_2)
          modelo2 <- lm(log(transf_midpoint_raw) ~ nivel, data = tabla_comparacion_inicial_corte_2)
          resumen2 <- summary(modelo2)
          R2_2 <- resumen2$r.squared
          F_obs_2 <- resumen2$fstatistic["value"]
          gl_2 <- resumen2$fstatistic["dendf"]
          valorp_global_2 <- 1-pf(F_obs_2, df1 = length(modelo2$coefficients)-1, df2 = gl_2)
          tabla_comparacion_inicial_corte_2 <- cbind(tabla_comparacion_inicial_corte_2, data.frame("mercado_regresionado" = exp(modelo2$fitted.values)))
          
          #union de tablas
          tabla_comparacion_inicial <- rbind(tabla_comparacion_inicial_corte_1, tabla_comparacion_inicial_corte_2)
        }
        if (input$modelo_selec ==  "Modelo Cuadrático"){
          #corte 1
          tabla_comparacion_inicial_corte_1 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_1)
          modelo1 <- lm(transf_midpoint_raw ~ nivel + I(nivel^2), data = tabla_comparacion_inicial_corte_1)
          resumen1 <- summary(modelo1)
          R2_1 <- resumen1$r.squared
          F_obs_1 <- resumen1$fstatistic["value"]
          gl_1 <- resumen1$fstatistic["dendf"]
          valorp_global_1 <- 1-pf(F_obs_1, df1 = length(modelo1$coefficients)-1, df2 = gl_1)
          tabla_comparacion_inicial_corte_1 <- cbind(tabla_comparacion_inicial_corte_1, data.frame("mercado_regresionado" = modelo1$fitted.values))
          
          #corte 2
          tabla_comparacion_inicial_corte_2 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_2)
          modelo2 <- lm(transf_midpoint_raw ~ nivel + I(nivel^2), data = tabla_comparacion_inicial_corte_2)
          resumen2 <- summary(modelo2)
          R2_2 <- resumen2$r.squared
          F_obs_2 <- resumen2$fstatistic["value"]
          gl_2 <- resumen2$fstatistic["dendf"]
          valorp_global_2 <- 1-pf(F_obs_2, df1 = length(modelo2$coefficients)-1, df2 = gl_2)
          tabla_comparacion_inicial_corte_2 <- cbind(tabla_comparacion_inicial_corte_2, data.frame("mercado_regresionado" = modelo2$fitted.values))
          
          #union de tablas
          tabla_comparacion_inicial <- rbind(tabla_comparacion_inicial_corte_1, tabla_comparacion_inicial_corte_2)
        }
        
        #calculo la variación de la propuesta respecto a la mediana del mercado regresionado
        tabla_comparacion_inicial <- tabla_comparacion_inicial %>%
          mutate(porp_vs_transf_midpoint_regressed = (mediana_propuesta/mercado_regresionado)-1)
        
        return(list("tabla_comparacion_inicial" = tabla_comparacion_inicial,
                    "Cortes" = c(paste0("corte 1"), paste0("corte 2")),
                    "R2" = c(R2_1, R2_2),
                    "valorp_global" = c(valorp_global_1, valorp_global_2)))
      }
    }
    
    # Caso poniendo cortes 1, 2 y 3
    if ((!is.null(corte_2)) & (!is.null(corte_3))){
      rango_corte_2 <- corte_2[1]:corte_2[2]
      rango_corte_3 <- corte_3[1]:corte_3[2]
      if (TRUE %in% ((rango_corte_1 %in% rango_corte_2) | (rango_corte_1 %in% rango_corte_3) | (rango_corte_2 %in% rango_corte_3))){
        #hay superposicion de los cortes si se cumple esto
        tabla_comparacion_inicial <- cbind(tabla_comparacion_inicial, data.frame("mercado_regresionado" = rep(0, times = nrow(tabla_comparacion_inicial))))
        return(list("tabla_comparacion_inicial" = tabla_comparacion_inicial,
                    "mensaje_error" = "HAY SUPERPOSICIÓN EN LOS CORTES"))
      }
      else {
        if (input$modelo_selec == "Modelo Exponencial"){
          #corte 1
          tabla_comparacion_inicial_corte_1 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_1)
          modelo1 <- lm(log(transf_midpoint_raw) ~ nivel, data = tabla_comparacion_inicial_corte_1)
          resumen1 <- summary(modelo1)
          R2_1 <- resumen1$r.squared
          F_obs_1 <- resumen1$fstatistic["value"]
          gl_1 <- resumen1$fstatistic["dendf"]
          valorp_global_1 <- 1-pf(F_obs_1, df1 = length(modelo1$coefficients)-1, df2 = gl_1)
          tabla_comparacion_inicial_corte_1 <- cbind(tabla_comparacion_inicial_corte_1, data.frame("mercado_regresionado" = exp(modelo1$fitted.values)))
          
          #corte 2
          tabla_comparacion_inicial_corte_2 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_2)
          modelo2 <- lm(log(transf_midpoint_raw) ~ nivel, data = tabla_comparacion_inicial_corte_2)
          resumen2 <- summary(modelo2)
          R2_2 <- resumen2$r.squared
          F_obs_2 <- resumen2$fstatistic["value"]
          gl_2 <- resumen2$fstatistic["dendf"]
          valorp_global_2 <- 1-pf(F_obs_2, df1 = length(modelo2$coefficients)-1, df2 = gl_2)
          tabla_comparacion_inicial_corte_2 <- cbind(tabla_comparacion_inicial_corte_2, data.frame("mercado_regresionado" = exp(modelo2$fitted.values)))
          
          #corte 3
          tabla_comparacion_inicial_corte_3 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_3)
          modelo3 <- lm(log(transf_midpoint_raw) ~ nivel, data = tabla_comparacion_inicial_corte_3)
          resumen3 <- summary(modelo3)
          R2_3 <- resumen3$r.squared
          F_obs_3 <- resumen3$fstatistic["value"]
          gl_3 <- resumen3$fstatistic["dendf"]
          valorp_global_3 <- 1-pf(F_obs_3, df1 = length(modelo3$coefficients)-1, df2 = gl_3)
          tabla_comparacion_inicial_corte_3 <- cbind(tabla_comparacion_inicial_corte_3, data.frame("mercado_regresionado" = exp(modelo3$fitted.values)))
          
          #union de tablas
          tabla_comparacion_inicial <- rbind(tabla_comparacion_inicial_corte_1, tabla_comparacion_inicial_corte_2, tabla_comparacion_inicial_corte_3)
        }
        if (input$modelo_selec ==  "Modelo Cuadrático"){
          #corte 1
          tabla_comparacion_inicial_corte_1 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_1)
          modelo1 <- lm(transf_midpoint_raw ~ nivel + I(nivel^2), data = tabla_comparacion_inicial_corte_1)
          resumen1 <- summary(modelo1)
          R2_1 <- resumen1$r.squared
          F_obs_1 <- resumen1$fstatistic["value"]
          gl_1 <- resumen1$fstatistic["dendf"]
          valorp_global_1 <- 1-pf(F_obs_1, df1 = length(modelo1$coefficients)-1, df2 = gl_1)
          tabla_comparacion_inicial_corte_1 <- cbind(tabla_comparacion_inicial_corte_1, data.frame("mercado_regresionado" = modelo1$fitted.values))
          
          #corte 2
          tabla_comparacion_inicial_corte_2 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_2)
          modelo2 <- lm(transf_midpoint_raw ~ nivel + I(nivel^2), data = tabla_comparacion_inicial_corte_2)
          resumen2 <- summary(modelo2)
          R2_2 <- resumen2$r.squared
          F_obs_2 <- resumen2$fstatistic["value"]
          gl_2 <- resumen2$fstatistic["dendf"]
          valorp_global_2 <- 1-pf(F_obs_2, df1 = length(modelo2$coefficients)-1, df2 = gl_2)
          tabla_comparacion_inicial_corte_2 <- cbind(tabla_comparacion_inicial_corte_2, data.frame("mercado_regresionado" = modelo2$fitted.values))
          
          #corte 3
          tabla_comparacion_inicial_corte_3 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_3)
          modelo3 <- lm(transf_midpoint_raw ~ nivel + I(nivel^2), data = tabla_comparacion_inicial_corte_3)
          resumen3 <- summary(modelo3)
          R2_3 <- resumen3$r.squared
          F_obs_3 <- resumen3$fstatistic["value"]
          gl_3 <- resumen3$fstatistic["dendf"]
          valorp_global_3 <- 1-pf(F_obs_3, df1 = length(modelo3$coefficients)-1, df2 = gl_3)
          tabla_comparacion_inicial_corte_3 <- cbind(tabla_comparacion_inicial_corte_3, data.frame("mercado_regresionado" = modelo3$fitted.values))
          
          
          #union de tablas
          tabla_comparacion_inicial <- rbind(tabla_comparacion_inicial_corte_1, tabla_comparacion_inicial_corte_2, tabla_comparacion_inicial_corte_3)
        }
        
        #calculo la variación de la propuesta respecto a la mediana del mercado regresionado
        tabla_comparacion_inicial <- tabla_comparacion_inicial %>%
          mutate(porp_vs_atc_midpoint_regressed = (mediana_propuesta/mercado_regresionado)-1)
        
        return(list("tabla_comparacion_inicial" = tabla_comparacion_inicial,
                    "Cortes" = c(paste0("corte 1"), paste0("corte 2"), paste0("corte 3")),
                    "R2" = c(R2_1, R2_2, R2_3),
                    "valorp_global" = c(valorp_global_1, valorp_global_2, valorp_global_3)))
        
      }
    }
    
    
  })
  # VALIDACION DE REGRESION ----
  output$validacion_reg <- renderReactable({
    corte_1 <- input$rango_corte_1
    corte_2 <- input$rango_corte_2
    corte_3 <- input$rango_corte_3
    rango_corte_1 <- corte_1[1]:corte_1[2]
    validacion <- data.frame("Cortes" = transfo_inicial_func2()$Cortes,
                             "R2" = transfo_inicial_func2()$R2,
                             "Valor_p_global" = transfo_inicial_func2()$valorp_global)
    if ((!is.null(corte_2)) & is.null(corte_3)){
      rango_corte_2 <- corte_2[1]:corte_2[2]
      if (TRUE %in% (rango_corte_1 %in% rango_corte_2)){
        validacion <- data.frame("ERROR" = transfo_inicial_func2()$mensaje_error)
      }
      else {
        validacion <- data.frame("Cortes" = transfo_inicial_func2()$Cortes,
                                 "R2" = transfo_inicial_func2()$R2,
                                 "Valor_p_global" = transfo_inicial_func2()$valorp_global)
      }
    }
    if ((!is.null(corte_2)) & (!is.null(corte_3))){
      rango_corte_2 <- corte_2[1]:corte_2[2]
      rango_corte_3 <- corte_3[1]:corte_3[2]
      if (TRUE %in% ((rango_corte_1 %in% rango_corte_2) | (rango_corte_1 %in% rango_corte_3) | (rango_corte_2 %in% rango_corte_3))){
        validacion <- data.frame("ERROR" = transfo_inicial_func2()$mensaje_error)
      }
      else {
        validacion <- data.frame("Cortes" = transfo_inicial_func2()$Cortes,
                                 "R2" = transfo_inicial_func2()$R2,
                                 "Valor_p_global" = transfo_inicial_func2()$valorp_global)
      }
    }
    
    row.names(validacion) <- NULL
    
    reactable(validacion, columns = list(
      Cortes = colDef(name = "Cortes",
                      width = 60),
      R2 = colDef(name = "R^2", format = colFormat(separators = TRUE, digits = 5),
                  width = 120),
      Valor_p_global = colDef(name = "Valor p (global)", format = colFormat(separators = TRUE, digits = 10),
                              width = 120)
    ))
  })
  
  # GRAFICO PARA CORTES DE REGRESION----
  output$grafico_reg <- renderPlotly({
    corte_1 <- input$rango_corte_1
    corte_2 <- input$rango_corte_2
    corte_3 <- input$rango_corte_3
    #rango_corte_1 <- corte_1[1]:corte_1[2]
    tabla_comparacion_inicial <- transfo_inicial_func2()$tabla_comparacion_inicial
    
    # solo cortes 1 y 2
    #if ((!is.null(corte_2)) & is.null(corte_3)){
    #  rango_corte_2 <- corte_2[1]:corte_2[2]
    #  tabla_comparacion_inicial_corte_1 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_1)
    #  tabla_comparacion_inicial_corte_2 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_2)
    #  
    #}
    
    # cortes 1, 2 y 3
    #if ((!is.null(corte_2)) & (!is.null(corte_3))){
    #  rango_corte_2 <- corte_2[1]:corte_2[2]
    #  rango_corte_3 <- corte_3[1]:corte_3[2]
    #  tabla_comparacion_inicial_corte_1 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_1)
    #  tabla_comparacion_inicial_corte_2 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_2)
    #  tabla_comparacion_inicial_corte_3 <- dplyr::filter(tabla_comparacion_inicial, nivel %in% rango_corte_3)
      
    #}
    # solo corte 1
    
    # armo un unico grafico para todos los casos
    tabla_comparacion_inicial$nivel <- as.factor(tabla_comparacion_inicial$nivel)
    graf_reg <- plot_ly(tabla_comparacion_inicial, x = ~nivel, type = 'scatter',
                        y = ~transf_midpoint_raw, name = "Transf. Midpoint Raw", mode = 'lines+markers') %>%
      add_trace(y = ~mercado_regresionado, name = 'Transf. Midpoint Regressed', mode = 'lines+markers') %>%
      config(displaylogo = FALSE, modeBarButtonsToAdd = list('drawline', 
                                                             'drawopenpath', 
                                                             'drawclosedpath', 
                                                             'drawcircle', 
                                                             'drawrect', 
                                                             'eraseshape')) %>%
      layout(xaxis = list(title = 'Nivel', gridcolor = '#e0ddd7'),
             yaxis = list(title = 'Tranf. Midpoint Raw/Regressed'))
    
    graf_reg
    
    
  })
  
  # TABLA DE COMPARACION INICIAL----
  output$tabla_inicial <- renderReactable({
    
    tabla_comparacion_inicial <- transfo_inicial_func2()$tabla_comparacion_inicial
    
    tabla_comparacion_inicial$nivel <- as.factor(tabla_comparacion_inicial$nivel)
    
    row.names(tabla_comparacion_inicial) <- NULL
    
    reactable(tabla_comparacion_inicial, columns = list(
      nivel = colDef(name = "Nivel",
                   width = 40),
      mediana_actual = colDef(name = "Mediana Actual", format = colFormat(prefix = "$", separators = TRUE, digits = 1),
                              minWidth = 98,
                              footer = function(values) {
                                sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(tabla_comparacion_inicial$mediana_actual))
                              }),
      progresion_actual = colDef(name = "Progresión Actual", format = colFormat(percent = TRUE, digits = 2),
                                 minWidth = 98,
                                 cell = data_bars(tabla_comparacion_inicial,
                                                  number_fmt = scales::label_percent(accuracy = 0.01, decimal.mark = ","),
                                                  text_position = "above",
                                                  fill_color = "indianred1",
                                                  fill_opacity = 0.6,
                                                  text_color = "black")),
      mediana_propuesta = colDef(name = "Mediana Propuesta", format = colFormat(prefix = "$", separators = TRUE, digits = 1),
                                 minWidth = 98,
                                 footer = function(values) {
                                   sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(tabla_comparacion_inicial$mediana_propuesta))
                                 }),
      progresion_propuesta = colDef(name = "Progresión Propuesta", format = colFormat(percent = TRUE, digits = 2),
                                    minWidth = 98,
                                    cell = data_bars(tabla_comparacion_inicial,
                                                     number_fmt = scales::label_percent(accuracy = 0.01, decimal.mark = ","),
                                                     text_position = "above",
                                                     fill_color = "olivedrab3",
                                                     fill_opacity = 0.6,
                                                     text_color = "black")),
      porcent_ajuste_prop_vs_actual = colDef(name = "% Ajuste vs actual", format = colFormat(percent = TRUE, digits = 2),
                                             minWidth = 98,
                                             cell = data_bars(tabla_comparacion_inicial,
                                                              number_fmt = scales::label_percent(accuracy = 0.01, decimal.mark = ","),
                                                              text_position = "inside-end",
                                                              fill_color = unique(c(ifelse(TRUE %in% (as.vector(tabla_comparacion_inicial$porcent_ajuste_prop_vs_actual) < 0), "indianred1", "olivedrab3"), "olivedrab3")),
                                                              fill_opacity = 0.6,
                                                              text_color = "black",
                                                              round_edges = T)),
      midpoint_raw = colDef(name = "Midpoint Raw (con % aumento mediana)", format = colFormat(prefix = "$", separators = TRUE, digits = 1),
                            minWidth = 98,
                            footer = function(values) {
                              sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(tabla_comparacion_inicial$midpoint_raw))
                            }),
      transf_midpoint_raw = colDef(name = "Tranf. Midpoint Raw (mercado a nivel empresa)", format = colFormat(prefix = "$", separators = TRUE, digits = 1),
                                minWidth = 98,
                                footer = function(values) {
                                  sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(tabla_comparacion_inicial$transf_midpoint_raw))
                                }),
      porp_vs_transf_midpoint_raw = colDef(name = "Propuesta VS Tranf. Midpoint Raw", format = colFormat(percent = TRUE, digits = 2),
                                        minWidth = 113,
                                        cell = data_bars(tabla_comparacion_inicial,
                                                         number_fmt = scales::label_percent(accuracy = 0.01, decimal.mark = ","),
                                                         text_position = "outside-end",
                                                         fill_color = unique(c(ifelse(TRUE %in% (as.vector(tabla_comparacion_inicial$porp_vs_transf_midpoint_raw) < 0), "lightblue", "orange"), "orange")),
                                                         fill_opacity = 0.6,
                                                         text_color = "black",
                                                         box_shadow = T)),
      mercado_regresionado = colDef(name = "Tranf. Midpoint Regressed", format = colFormat(prefix = "$", separators = TRUE, digits = 1),
                                    minWidth = 98,
                                    footer = function(values) {
                                      sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(tabla_comparacion_inicial$mercado_regresionado))
                                    }),
      porp_vs_transf_midpoint_regressed = colDef(name = "Propuesta VS Tranf. Midpoint Regressed", format = colFormat(percent = TRUE, digits = 2),
                                              minWidth = 113,
                                              cell = data_bars(tabla_comparacion_inicial,
                                                               number_fmt = scales::label_percent(accuracy = 0.01, decimal.mark = ","),
                                                               text_position = "outside-end",
                                                               fill_color = unique(c(ifelse(TRUE %in% (as.vector(tabla_comparacion_inicial$porp_vs_transf_midpoint_raw) < 0), "lightblue", "orange"), "orange")),
                                                               fill_opacity = 0.6,
                                                               text_color = "black",
                                                               box_shadow = T))
    ),
    defaultPageSize = 15, minRows = 15,
    selection = "multiple",
    borderless = TRUE,
    onClick = "select",
    theme = reactableTheme(
      rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d"),
      borderColor = "#e0ddd7"
    ),
    columnGroups = list(
      colGroup(name = "Actual", columns = c("mediana_actual", "progresion_actual")),
      colGroup(name = "Propuesta", columns = c("mediana_propuesta", "progresion_propuesta", "porcent_ajuste_prop_vs_actual")),
      colGroup(name = "Mercado Crudo", columns = c("midpoint_raw", "transf_midpoint_raw", "porp_vs_transf_midpoint_raw")),
      colGroup(name = "Mercado Regresionado", columns = c("mercado_regresionado", "porp_vs_transf_midpoint_regressed"))
    ),
    bordered = TRUE, striped = TRUE, highlight = TRUE
    )
    
  })
  
  # BOXPLOTS----
  output$distPlot <- renderPlotly({
    
    data_facturacion$grupo <- as.factor(data_facturacion$grupo)
    
    lim_teo_auxiliar <- lista_orig_func()$limites_teoricos
    
    lim_teo_auxiliar$nivel <- as.factor(lim_teo_auxiliar$nivel)
    
    tam_muestra_CAT <- data_facturacion %>% filter(zona == input$zona_selec) %>% group_by(nivel) %>% summarize(num=n())
    data_boxplots <- data_facturacion %>% filter(zona == input$zona_selec, nivel %in% input$nivel) %>% left_join(tam_muestra_nivel) %>% mutate(myaxis = paste0(nivel, "\n", "n=", num))
    outliers_arg_boxp <- outliers_estad_func() %>% filter(nivel %in% input$nivel) %>% left_join(tam_muestra_nivel) %>% mutate(myaxis = paste0(nivel, "\n", "n=", num))
    limites_teoricos_boxp <- lim_teo_auxiliar %>% filter(nivel %in% input$nivel) %>% left_join(tam_muestra_nivel) %>% mutate(myaxis = paste0(nivel, "\n", "n=", num))
    
    colores_de_cajas <- c(RColorBrewer::brewer.pal(9, "Blues")[4], RColorBrewer::brewer.pal(9, "Blues")[6], RColorBrewer::brewer.pal(9, "Blues")[8])
    
    
    
    #if (nrow(lista_prop_func()$limites_teo_prop) != 0){ #(NO hago esto al final)
      
    lim_teo_prop_auxiliar <- lista_prop_func()$limites_teo_prop
    lim_teo_prop_auxiliar$nivel <- as.factor(lim_teo_prop_auxiliar$nivel)
    limites_teo_prop_boxp <- lim_teo_prop_auxiliar %>% filter(nivel %in% input$nivel) %>% left_join(tam_muestra_nivel) %>% mutate(myaxis = paste0(nivel, "\n", "n=", num))
    
    if ("Boxplot mostrando estadisticas y outliers" == input$boxplot_selec){
      boxplot_arg <- ggplotly(ggplot() +
                                geom_boxplot(data_boxplots,
                                             mapping = aes(y=facturacion_anual, x=myaxis, fill=grupo),
                                             outlier.size = 0.1) +#, outlier.colour = "yellow", outlier.fill = "yellow") + #plotly NO DEJA HACER ESTO
                                geom_point(data = outliers_arg_boxp, mapping = aes(y=facturacion_anual,
                                                                                   x = myaxis,
                                                                                   text= paste0("ID: ", ID),
                                                                                   color = desemp,
                                                                                   shape = rend)) + #porbar shape = desemp
                                ggtitle(paste0("Facturación anual según nivel en ", pais_func())) +
                                labs(x="Nivel (y cantidad de franquicias)", y=paste0("Facturación Anual [$", input$zona_selec, "]")) +
                                scale_fill_brewer(palette="Blues")) %>% layout(showlegend = F)
    }
    
    if ("Boxplot mostrando puntos y tam de muestra" == input$boxplot_selec){
      boxplot_arg <- ggplotly(data_boxplots %>%
                                ggplot( aes(x=myaxis, y=facturacion_anual, fill=grupo)) +
                                geom_jitter(alpha = 0.2) +
                                geom_boxplot() +
                                ggtitle(paste0("Facturación anual según nivel en ", pais_func())) +
                                xlab("Nivel (y cantidad de franquicias)") +
                                ylab(paste0("Facturación Anual [$", input$zona_selec, "]")) +
                                scale_fill_brewer(palette="Blues")) %>% layout(showlegend = F)
    }
    
    if ("Boxplot comparando límites teóricos con estadisticas" == input$boxplot_selec){
      limites_teoricos_boxp$nivel <- as.factor(limites_teoricos_boxp$nivel)
      limites_teo_prop_boxp$nivel <- as.factor(limites_teo_prop_boxp$nivel)
      boxplot_arg <- plot_ly(x = ~myaxis) %>%
        add_boxplot(data = data_boxplots, y = ~facturacion_anual, color = ~grupo, colors = colores_de_cajas, alpha = 0.5, strokes = "Black") %>%
        layout(title = paste0("Facturación Anual según nivel en ", pais_func()),
               yaxis = list(zeroline = FALSE, title = paste0("Facturación Anual [$", input$zona_selec, "]")),
               xaxis = list(zeroline = FALSE, title = 'nivel (y cantidad de franquicias)'),
               legend = list(title= "Grupos:"),
               annotations = 
                 list(x = 1, y = -0.1, text = "Para visualizar claramente outliers seleccionar el gráfico correspondiente", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=-10.5,
                      font=list(size=8, color="black"))) %>%
        add_markers(data = limites_teoricos_boxp, y = ~tope, name = 'Tope Actual', marker = list(color = "red", symbol = "triangle-up-dot", size = 10)) %>%
        add_markers(data = limites_teoricos_boxp, y = ~mediana_teo, name = 'Mediana Teórica Actual', marker = list(color = "red", symbol = "hexagram-dot", size = 10)) %>%
        add_markers(data = limites_teoricos_boxp, y = ~piso, name = 'Piso Actual', marker = list(color = "red", symbol = "triangle-down-dot", size = 10)) %>%
        add_markers(data = limites_teo_prop_boxp, y = ~tope_prop, name = 'Tope Propuesta', marker = list(color = "green", symbol = "triangle-up-dot", size = 10)) %>%
        add_markers(data = limites_teo_prop_boxp, y = ~mediana_teo_prop, name = 'Mediana Teórica Propuesta', marker = list(color = "green", symbol = "hexagram-dot", size = 10)) %>%
        add_markers(data = limites_teo_prop_boxp, y = ~piso_prop, name = 'Piso Propuesta', marker = list(color = "green", symbol = "triangle-down-dot", size = 10))
      
      
    }
    boxplot_arg# %>% layout(showlegend = F)
      
    #}
    
  })
  
  # TABLA OUTLIERS ----
  output$tabla_outliers <- DT::renderDataTable({
    datatable(arrange(select(outliers_estad_func(), ID, rend, desemp, posicion, nivel, antig_nivel, facturacion_anual, aum_fact), desc(nivel)),
              class = 'cell-border stripe order-column compact',
              rownames = F,
              colnames = c("ID Franquicia" = "ID", "Rendimiento" = "rend", "Desempeño" = "desemp", "Antigüedad dentro del Nivel" = "antig_nivel", "Facturación Anual" = "facturacion_anual", "Aumento de Facturación Anual" = "aum_fact"),
              caption = "Franquicias con Facturación Anual estadísticamente atípica según su Nivel asignado",
              filter = 'top',
              options = list(pageLength = 10, autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-right', targets = c(1, 2, 3, 4))))) %>%
      formatCurrency(columns = "Facturación Anual", currency = "$ ", mark = ".", dec.mark = ",", digits = 1) %>%
      formatCurrency(columns = "Antigüedad dentro del Nivel", currency = "", mark = ".", dec.mark = ",") %>%
      formatPercentage(columns = "Aumento Facturación Anual", mark = ".", dec.mark = ",", digits = 2)
  })
  
  # HISTOGRAMAS----
  output$histPlot <- renderPlotly({
    
    lim_teo_auxiliar <- lista_orig_func()$limites_teoricos
    
    #if (is.null(lista_prop_func())){ #(NO hago esto al final)
    #  
    #}
    
    #if (nrow(lista_prop_func()$limites_teo_prop) != 0){
    nivel_hist <- input$nivel_hist
    `Piso` <- as.numeric(filter(lim_teo_auxiliar, nivel == nivel_hist)[1 , "piso"])
    piso_hist <- `Piso`
    `Tope` <- as.numeric(filter(lim_teo_auxiliar, nivel == nivel_hist)[1 , "tope"])
    tope_hist <- `Tope`
    `Mediana Teorica` <- as.numeric(filter(lim_teo_auxiliar, nivel == nivel_hist)[1 , "mediana_teo"])
    mediana_teo_hist <- `Mediana Teorica`
    `Mediana Estadistica` <- median(filter(data_facturacion, pais == input$zona_selec, nivel == nivel_hist)$facturacion_anual)
    mediana_est_hist <- `Mediana Estadistica`
    `Piso PROPUESTA` <- as.numeric(filter(lista_prop_func()$limites_teo_prop, nivel == nivel_hist)[1 , "piso_prop"])
    piso_hist_prop <- `Piso PROPUESTA`
    `Tope PROPUESTA` <- as.numeric(filter(lista_prop_func()$limites_teo_prop, nivel == nivel_hist)[1 , "tope_prop"])
    tope_hist_prop <- `Tope PROPUESTA`
    `Mediana Teorica PROPUESTA` <- as.numeric(filter(lista_prop_func()$limites_teo_prop, nivel == nivel_hist)[1 , "mediana_teo_prop"])
    mediana_teo_hist_prop <- `Mediana Teorica PROPUESTA`
    ancho_interv_hist <- round(tope_hist-piso_hist)/10
    colores_hist <- c("Piso" = "red", "Tope" = "red", "Mediana Teorica" = "red", "Mediana Estadistica" = "blue",
                      "Piso PROPUESTA" = "green", "Tope PROPUESTA" = "green", "Mediana Teorica PROPUESTA" = "green")
    histograma_tas_nivel <- ggplotly(ggplot(filter(data_facturacion, pais == input$zona_selec, nivel == nivel_hist), aes(x=facturacion_anual, fill=nivel)) + 
                                     geom_histogram(binwidth = ancho_interv_hist, position = 'identity', fill = "blue", alpha = 0.35) +
                                     scale_color_manual(labels = c("Piso", "Tope", "Mediana teorica", "Mediana estadistica", "Piso PROPUESTA", "Tope PROPUESTA", "Mediana Teorica PROPUESTA"), values = colores_hist) +
                                     geom_vline(aes(xintercept = `Piso`, color = "Piso"), alpha = 0.8, linetype = "dotted", show.legend = T) +
                                     geom_vline(aes(xintercept = `Tope`, color = "Tope"), alpha = 0.8, linetype = "dotted", show.legend = T) +
                                     geom_vline(aes(xintercept = `Mediana Teorica`, color = "Mediana Teorica"), alpha = 0.8, show.legend = T) +
                                     geom_vline(aes(xintercept = `Mediana Estadistica`, color = "Mediana Estadistica"), alpha = 1, show.legend = T) +
                                     geom_vline(aes(xintercept = `Piso PROPUESTA`, color = "Piso PROPUESTA"), alpha = 0.8, linetype = "dotted", show.legend = T) +
                                     geom_vline(aes(xintercept = `Tope PROPUESTA`, color = "Tope PROPUESTA"), alpha = 0.8, linetype = "dotted", show.legend = T) +
                                     geom_vline(aes(xintercept = `Mediana Teorica PROPUESTA`, color = "Mediana Teorica PROPUESTA"), alpha = 0.8, show.legend = T) +
                                     xlab(paste0("Facturación Anual [$ ", input$zona_selec, "]")) +
                                     ylab("Cantidad de franquicias por intervalo") +
                                     labs(title = paste0("Histograma de la Facturación Anual para el Nivel ", cat_hist), subtitle = paste0("[$ ", input$zona_selec, "]"), color = NULL) +
                                     theme(legend.position = "bottom") + #no funciona con plotly
                                     scale_x_continuous(labels = format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE))) %>%
      layout(title = list(text = paste0(paste0("Histograma de la Facturación Anual para el Nivel ", cat_hist),
                                        '<br>',
                                        '<sup>',
                                        paste0(pais_func()),
                                        '</sup>')))
    
    histograma_tas_nivel
    #}
  })
  
  # MULTIHISTOGRAMA----
  output$multihistPlot <- renderPlotly({
    
    lim_teo_auxiliar <- lista_orig_func()$limites_teoricos
    
    multihist_no_sup <- ggplotly(ggplot(filter(data_facturacion, zona == input$zona_selec), aes(x=facturacion_anual, color=nivel, fill=nivel)) + 
                                   geom_histogram(alpha=0.6, binwidth = round(mean(lim_teo_auxiliar$tope)-mean(lim_teo_auxiliar$piso))/10) +
                                   scale_fill_viridis(discrete=TRUE) +
                                   scale_color_viridis(discrete=TRUE) +
                                   theme_ipsum() +
                                   theme(
                                     legend.position="none",
                                     panel.spacing = unit(0.1, "lines"),
                                     strip.text.x = element_text(size = 8)
                                   ) +
                                   xlab(NULL) +
                                   ylab("Cantidad de personas") +
                                   scale_x_continuous(labels = NULL) +
                                   facet_wrap(~nivel,nrow = 1, strip.position = c("bottom")), height = 280) %>% #no esta funcionando la posicion bottom con plotly
      layout(title = paste0(pais_func()))
    
    
    multihist_no_sup
  })
  
  # TABLAS DE CONTINGENCIA/HEATMAPS----
  output$tabla_contingencia_orig_salida <- renderReactable({
    tabla_contin_orig <- lista_orig_func()$tabla_contingencia_orig
    
    #temppal <- c('#f7f5f5', '#f5cbcb', '#f2a5a5', '#f57f7f', '#f75c5c', '#f73636', '#fa0a0a')
    reactable(tabla_contin_orig, 
              defaultColDef = colDef(style = color_scales(tabla_contin_orig, span = TRUE, colors = RColorBrewer::brewer.pal(9,"Reds")),
              minWidth = 50),
              defaultPageSize = nrow(tabla_contin_orig), minRows = nrow(tabla_contin_orig))
        
    #datatable(lista_orig_func()$tabla_contingencia_orig,
    #          class = 'cell-border stripe order-column compact',
    #          options = list(pageLength = 15, autoWidth = TRUE))
  })
  output$tabla_contingencia_prop_salida <- renderReactable({
    tabla_contin_prop <- lista_prop_func()$tabla_contingencia_prop
    tabla_contin_orig <- lista_orig_func()$tabla_contingencia_orig
    
    #temppal <- c('#f7f5f5', '#f5cbcb', '#f2a5a5', '#f57f7f', '#f75c5c', '#f73636', '#fa0a0a')
    reactable(tabla_contin_prop,
              defaultColDef = colDef(style = color_scales(tabla_contin_prop, span = TRUE, colors = RColorBrewer::brewer.pal(9,"Reds")),
              minWidth = 50),
              defaultPageSize = nrow(tabla_contin_prop), minRows = nrow(tabla_contin_prop))
  })
  
  # GRAFICO DE BARRAS CON POSICIONAMIENTO----
  output$barras_totales_Q <- renderPlot({
    tabla_contin_orig <- lista_orig_func()$tabla_contingencia_orig
    tabla_contin_prop <- lista_prop_func()$tabla_contingencia_prop
    
    
    nivel_num <- as.numeric(input$nivel_barras)[1]:as.numeric(input$cats_barras)[2]
    tabla_contin_orig <- mutate(tabla_contin_orig, nivel = rownames(tabla_contin_orig))
    tabla_contin_prop <- mutate(tabla_contin_prop, nivel = rownames(tabla_contin_prop))
    tabla_contin_orig <- filter(tabla_contin_orig, nivel %in% nivel_num)
    tabla_contin_prop <- filter(tabla_contin_prop, nivel %in% nivel_num)
    tabla_contin_orig$nivel <- NULL
    tabla_contin_prop$nivel <- NULL
    
    colnames(tabla_contin_orig) <- c("Bajo_piso", "Q1", "Q2", "Q3", "Q4", "Sobre_tope")
    colnames(tabla_contin_prop) <- c("Bajo_piso", "Q1", "Q2", "Q3", "Q4", "Sobre_tope")
    tabla_contin_orig[is.na(tabla_contin_orig)] <- 0
    tabla_contin_prop[is.na(tabla_contin_prop)] <- 0
    totales_orig <- dplyr::summarise(tabla_contin_orig,
                                     `Bajo piso` = sum(Bajo_piso), `Q1` = sum(Q1), `Q2` = sum(Q2), `Q3` = sum(Q3), `Q4` = sum(Q4), `Sobre tope` = sum(Sobre_tope))
    totales_prop <- dplyr::summarise(tabla_contin_prop,
                                     `Bajo piso` = sum(Bajo_piso), `Q1` = sum(Q1), `Q2` = sum(Q2), `Q3` = sum(Q3), `Q4` = sum(Q4), `Sobre tope` = sum(Sobre_tope))
    tabla_totales <- rbind(totales_orig, totales_prop)
    rownames(tabla_totales) <- c("limites_actuales", "limites_propuestos")
    tabla_totales <- t(tabla_totales)
    tabla_totales2 <- data.frame("cuantil" = row.names(tabla_totales), "limites_actuales" = as.vector(tabla_totales[,1]), "limites_propuestos" = as.vector(tabla_totales[,2]))
    tabla_totales3 <- data.frame("cuantil" = c(tabla_totales2[ , "cuantil"], tabla_totales2[ , "cuantil"]),
                                 "totales" = c(tabla_totales2[ , "limites_actuales"], tabla_totales2[ , "limites_propuestos"]),
                                 "condicion" = c(rep("Limites Actuales", nrow(tabla_totales2)), rep("Limites Propuestos", nrow(tabla_totales2))))
    tabla_totales4 <- as.data.frame(tabla_totales3)
    
    barrasplot <- ggplot(tabla_totales4, aes(fill=condicion, y=totales, x=cuantil, label = totales, ymax = (max(totales)+40))) + 
      geom_bar(position="dodge", stat="identity") +
      theme_ipsum() +
      xlab("") +
      ylab("") +
      ggtitle("Posicionamiento total de franquicias según cuantil") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "top", legend.title = element_blank(), legend.text = element_text(size=15)) +
      scale_fill_manual(values=c("#bf2e2e", "#61ba34")) +
      geom_text(size = 5, stat="identity", position = position_dodge(width = 0.9), vjust=-0.25) #position = position_dodge(width = 0.9)
    
    barrasplot
  })
  
  # TABLAS FRANQUICIAS FUERA DE LOS LIMITES----
  output$tabla_bajo_lim_teo_orig <- DT::renderDataTable({
    datatable(arrange(select(lista_orig_func()$bajo_piso_orig, ID, rend, desemp, posicion, nivel, Q, antig_nivel, facturacion_anual, porcen_vs_piso_nivel, porcen_vs_tope_nivel), desc(nivel)),
              class = 'cell-border stripe order-column compact',
              rownames = F,
              colnames = c("ID Franquicia" = "ID", "Rendimiento" = "rend", "Desempeño" = "desemp", "Antigüedad dentro del nivel" = "antig_nivel", "Facturación Anual" = "facturacion_anual", "% vs Piso nivel" = "porcen_vs_piso_nivel", "% vs Tope nivel" = "porcen_vs_tope_nivel"),
              filter = 'top',
              options = list(pageLength = 5, autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-right', targets = c(1, 2, 3, 4, 5, 6, 7, 8, 9))))) %>%
      formatCurrency(columns = "Facturación Anual", currency = "$ ", mark = ".", dec.mark = ",", digits = 1) %>%
      formatPercentage(columns = c("% vs Piso nivel", "% vs Tope nivel"), digits = 2, mark = ".", dec.mark = ",") %>%
      formatCurrency(columns = "Antigüedad dentro del Nivel", currency = "", mark = ".", dec.mark = ",")
  })
  output$tabla_sobre_lim_teo_orig <- DT::renderDataTable({
    datatable(arrange(select(lista_orig_func()$sobre_tope_orig, ID, rend, desemp, posicion, nivel, Q, antig_nivel, facturacion_anual, porcen_vs_piso_nivel, porcen_vs_tope_nivel), desc(nivel)),
              class = 'cell-border stripe order-column compact',
              rownames = F,
              colnames = c("ID Franquicia" = "ID", "Rendimiento" = "rend", "Desempeño" = "desemp", "Antigüedad dentro del Nivel" = "antig_nivel", "Facturación Anual" = "facturacion_anual", "% vs Piso nivel" = "porcen_vs_piso_nivel", "% vs Tope nivel" = "porcen_vs_tope_nivel"),
              filter = 'top',
              options = list(pageLength = 5, autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-right', targets = c(1, 2, 3, 4, 5, 6, 7, 8, 9))))) %>%
      formatCurrency(columns = "Facturación Anual", currency = "$ ", mark = ".", dec.mark = ",", digits = 1) %>%
      formatPercentage(columns = c("% vs Piso nivel", "% vs Tope nivel"), digits = 2, mark = ".", dec.mark = ",") %>%
      formatCurrency(columns = "Antigüedad dentro del Nivel", currency = "", mark = ".", dec.mark = ",")
  })
  output$tabla_bajo_lim_teo_prop <- DT::renderDataTable({
    datatable(arrange(select(lista_prop_func()$bajo_piso_prop, ID, rend, desemp, posicion, nivel, Q, antig_nivel, facturacion_anual, porcen_vs_piso_nivel, porcen_vs_tope_nivel), desc(nivel)),
              class = 'cell-border stripe order-column compact',
              rownames = F,
              colnames = c("ID Facturación" = "ID", "Rendimiento" = "rend", "Desempeño" = "desemp", "Antigüedad dentro del Nivel" = "antig_nivel", "Facturación Anual" = "facturacion_anual", "% vs Piso nivel" = "porcen_vs_piso_nivel", "% vs Tope nivel" = "porcen_vs_tope_nivel"),
              filter = 'top',
              options = list(pageLength = 5, autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-right', targets = c(1, 2, 3, 4, 5, 6, 7, 8, 9))))) %>%
      formatCurrency(columns = "Facturación Anual", currency = "$ ", mark = ".", dec.mark = ",", digits = 1) %>%
      formatPercentage(columns = c("% vs Piso nivel", "% vs Tope nivel"), digits = 2, mark = ".", dec.mark = ",") %>%
      formatCurrency(columns = "Antigüedad dentro del Nivel", currency = "", mark = ".", dec.mark = ",")
  })
  output$tabla_sobre_lim_teo_prop <- DT::renderDataTable({
    datatable(arrange(select(lista_prop_func()$sobre_tope_prop, ID, rendo, desemp, posicion, nivel, Q, antig_nivel, facturacion_anual, porcen_vs_piso_nivel, porcen_vs_tope_nivel), desc(nivel)),
              class = 'cell-border stripe order-column compact',
              rownames = F,
              colnames = c("ID Franquicia" = "ID", "Rendimiento" = "rend", "Desempeño" = "desemp", "Antigüedad dentro del Nivel" = "antig_nivel", "Facturación Anual" = "facturacion_anual", "% vs Piso nivel" = "porcen_vs_piso_nivel", "% vs Tope nivel" = "porcen_vs_tope_nivel"),
              filter = 'top',
              options = list(pageLength = 5, autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-right', targets = c(1, 2, 3, 4, 5, 6, 7, 8, 9))))) %>%
      formatCurrency(columns = "Facturación Anual", currency = "$ ", mark = ".", dec.mark = ",", digits = 1) %>%
      formatPercentage(columns = c("% vs Piso nivel", "% vs Tope nivel"), digits = 2, mark = ".", dec.mark = ",") %>%
      formatCurrency(columns = "Antigüedad dentro del Nivel", currency = "", mark = ".", dec.mark = ",")
  })
  
})
