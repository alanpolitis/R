

server=function(input,output){
  
  #GRAFICOS
  
  #TABLAS
  
  output$tablacomu=renderTable({
    df_COVID %>% 
      filter(SEMANA==max(df_COVID$SEMANA)) %>%
      filter(COMUNA==input$barrio) %>% 
      select(BARRIO,CASOS) %>% 
      arrange(desc(CASOS))
    
  })
  
  #GRAFICO DE DONA CON DISTRIBUCION DE CASOS ABSOLUTOS X COMUNA  
  
  output$casos1=renderPlotly({
    df_COVID %>% 
      filter(SEMANA==max(df_COVID$SEMANA))%>% 
      select(COMUNA,CASOS) %>% 
      group_by(COMUNA) %>% 
      summarise(CASOS=sum(CASOS)) %>%
      plot_ly(
        values=~CASOS,
        labels =~COMUNA,
        text=~paste(CASOS, 'casos'),
        textinfo='label+percent',
        insidetextfont=list(color='white'),
        hoverinfo='text',
        type='pie',
        hole=0.5,
        showlegend=FALSE
      ) %>% 
      layout(title=list(text='Casos totales al 31-07-20',
                        font=list(color='black',size=16))
      )
    
  })
  
  #GRAFICO DE BARRAS CON COMPARACION DE CASOS ABSOLUTOS Y CASOS CADA 100.000 HABITANTES  
  
  output$casos100=renderPlotly({
    plot_ly() %>% 
      add_trace(data=CASOS100,
                x=~COMUNA,
                y=~CASOS,
                name='Casos absolutos',
                mode='bar') %>%
      add_trace(data=CASOS100,
                x=~COMUNA,
                name='Casos cada 100K hab',
                y=~casos100,
                mode='bar') %>% 
      layout(title='Comparación de casos absolutos y casos cada 100.000 habitantes',
             xaxis=list(title=''))
    
  })
  
  #TIPO DE CONTAGIO 
  
  output$tipoconta=renderPlot({
    ggplot(data=df_CABA %>% 
             filter(provincia=='CABA',
                    tipo_contagio != 'NA',
                    comuna==input$comuna0) %>%
             mutate(mes=substring(fecha_apertura_snvs,6,7)) %>%
             count(mes,tipo_contagio) %>% 
             mutate(cantidad_casos=n),
           aes(x=tipo_contagio,y=cantidad_casos, fill=tipo_contagio))+
      geom_col(color='black',show.legend = FALSE)+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5,size = 20),
            axis.text = element_text(size = 14))+
      facet_grid(mes~.)+
      coord_flip()+
      labs(title='Tipos de contagio por comuna por mes',
           x='',
           y='')
  })
  
  #GRAFICO DE EVOLUCIÓN DE CASOS DIARIOS, TOTAL CABA Y OTRO DE PUNTOS X COMUNA
  
  output$casos2=renderPlotly({
    plot_ly() %>%
      add_trace(data=df_CABA %>%
                  filter(provincia=='CABA',
                         clasificacion=='confirmado',
                         fecha_apertura_snvs<as.Date('2020-07-30')) %>%
                  count(fecha_apertura_snvs),
                x=~fecha_apertura_snvs,
                y=~n,
                xaxis='',
                mode=~'bar',
                name='Contagios') %>%
      add_trace(data=df_CABA %>%
                  filter(provincia=='CABA',
                         clasificacion=='confirmado',
                         fecha_alta_medica<as.Date('2020-07-30')) %>%
                  count(fecha_alta_medica),
                x=~fecha_alta_medica,
                y=~n,
                xaxis='',
                mode=~'bar',
                name='Altas') %>%
      add_trace(data=df_CABA %>%
                  filter(provincia=='CABA',
                         clasificacion=='confirmado',
                         fecha_fallecimiento<as.Date('2020-07-30')) %>%
                  count(fecha_fallecimiento),
                x=~fecha_fallecimiento,
                y=~n,
                xaxis='',
                mode=~'bar',
                name='Fallecidos')%>%
      layout(title='Contagios, altas y fallecimientos diarios totales en la Ciudad de Buenos Aires',
             xaxis=list(title='Fecha'),
             yaxis=list(title='Cantidad de contagios, altas y fallecimientos'))
  })
  
  output$casos2.1=renderPlotly({
    plot_ly(data=df_CABA %>% 
              filter(provincia=='CABA',
                     clasificacion=='confirmado',
                     comuna!='COMUNA NA') %>% 
              count(fecha_apertura_snvs,comuna) %>% 
              mutate(comuna=as.factor(comuna)),
            x=~fecha_apertura_snvs,
            y=~n,
            color=~comuna,
            text=~comuna,
            type = 'scatter',
            xaxis='') %>% 
      layout(title='Casos diarios de COVID por comuna',
             yaxis=list(title='Cantidad de contagios'),
             xaxis=list(title='Fecha'))
    
  })
  
  #PLOTLY INTERACTIVO DE CASOS SEMANALES ACUMULADOS X BARRIO Y COMUNA.
  
  output$casos3=renderPlotly({
    plot_ly(data=df_COVID %>% 
              count(SEMANA,COMUNA,BARRIO,CASOS) %>% 
              filter(COMUNA %in% c(input$comuna)), 
            x=~CASOS,
            y=~COMUNA,
            color=~BARRIO,
            frame=~SEMANA,
            text=~BARRIO,
            hoverinfo='text',
            type='bar',
            mode='markers') %>% 
      layout(yaxis=list(title=''))
  })
  
  #GRAFICO DE PROPORCION DE FALLECIDOS Y NO FALLECIDOS POR COMUNA
  
  output$comunafalle=renderPlotly({
    ggplotly(ggplot(data= df_CABA %>% 
                      filter(provincia == 'CABA',
                             sexo %in% c('F','M'),
                             comuna == input$comufalle)%>% 
                      count(provincia,sexo,edad,fallecido) %>% 
                      mutate(rango=case_when(edad %in% c(0:15) ~ '0 a 15 años',
                                             edad %in% c(16:30) ~ '16 a 30 años',
                                             edad %in% c(31:45) ~ '31 a 45 años',
                                             edad %in% c(46:50) ~ '46 a 50 años',
                                             edad %in% c(51:65) ~ '51 a 65 años',
                                             edad %in% c(66:80) ~ '66 a 80 años',
                                             TRUE ~ 'más de 80 años'),
                             cantidad=n),
                    aes(x=sexo,y=cantidad,fill=rango,sep=edad))+
               geom_col(position='fill')+
               facet_grid(fallecido~.)+
               coord_flip()+
               theme_bw()+
               labs(title='Relación entre edad y fallecimiento por comuna',
                    y='',
                    x='',
                    fill='Rango etario'))
  })
  
  #TEXTO
  
  output$texto=renderText({
    
    if (input$rank=='Demografía'){
      print('Considera un promedio de los puntajes de la densidad poblacional (relación entre población y superficie) y de la proporción de la población de riesgo (mayor de 60 años) de cada comuna.  Mientras más se acerca a rojo la comuna, mayor es la densidad poblacional y la población resulta más envejecida.')
    }
    else if (input$rank=='Salud'){
      print('Considera un promedio de los puntajes de metros cuadrados de espacios verdes por habitante y la cantidad de personas con prepaga por comuna. Mientras más se acerca a rojo la comuna, menor es la cantidad de espacios verdes por habitante y mayor es la dependencia del sistema de salud público.')
      
    }
    else if (input$rank=='Empleo'){
      print('Considera un promedio de los puntajes de la proporción de trabajadores esenciales e impedidos de hacer teletrabajo y la proporción de trabajadores informales de cada comuna. Mientras más se acerca a rojo la comuna, mayor es la proporción de estos trabajadores.')
    }
    else if (input$rank=='Cercanía'){
      print('Considera los puntajes que refieren a la cercanía media de las viviendas a comercios esenciales de cada comuna. Mientras más se acerca a rojo la comuna, mayor es la distancia media a estos comercios.')
      
    }
    else if (input$rank=='Vivienda'){
      print('Considera un promedio de los puntajes de la proporción de viviendas unipersonales, precarias y que presentan hacinamiento crítico. Mientras más se acerca a rojo la comuna, menor es la cantidad de viviendas unipersonales y mayor es el hacinamiento y la precariedad.')
    }
    else {}
    
    
  })
  
  #TABLA
  
  output$tabla_rank=renderTable({
    
    if (input$rank=='Demografía'){
      st_geometry(puntajes) = NULL
      
      puntajes %>%  select(COMUNAS,Demografía) %>% top_n(6,Demografía) %>% arrange(desc(Demografía)) %>% select(COMUNAS)
    }
    else if (input$rank=='Salud'){
      st_geometry(puntajes) = NULL
      
      puntajes %>%  select(COMUNAS,Salud) %>% top_n(6,Salud) %>% arrange(desc(Salud)) %>% select(COMUNAS)
      
    }
    else if (input$rank=='Empleo'){
      st_geometry(puntajes) = NULL
      
      puntajes %>%  select(COMUNAS,Empleo) %>% top_n(6,Empleo) %>% arrange(desc(Empleo)) %>% select(COMUNAS)
    }
    else if (input$rank=='Cercanía'){
      st_geometry(puntajes) = NULL
      
      puntajes %>%  select(COMUNAS,Cercanía) %>% top_n(6,Cercanía) %>% arrange(desc(Cercanía)) %>% select(COMUNAS)
    }
    else if (input$rank=='Vivienda'){
      st_geometry(puntajes) = NULL
      
      puntajes %>%  select(COMUNAS,Vivienda) %>% top_n(6,Vivienda) %>% arrange(desc(Vivienda)) %>% select(COMUNAS)
    }
    else {}
    
    
  })
  
  
  
  #MAPAS
  
  output$mapas=renderLeaflet({
    if (input$rank=='Demografía'){
      
      leaflet(puntajes) %>% addProviderTiles("CartoDB") %>% 
        addPolygons(color = "#444444", 
                    weight = 1, smoothFactor = 0.5, opacity = 1, fillOpacity = 0.5, fillColor = ~palnumericD(puntajes$Demografía), 
                    group = "Demografía", highlightOptions = highlightOptions(color = "white", 
                                                                              weight = 2, bringToFront = TRUE), label = ~puntajes$COMUNAS, labelOptions = labelOptions(direction = "auto"), 
                    popup = popup) %>% 
        addLegend(position = "topright", pal = palnumericD, values = ~puntajes$Demografía, 
                  title = "Puntaje")
    }
    
    else if (input$rank=='Salud'){
      
      leaflet(puntajes) %>% addProviderTiles("CartoDB") %>% 
        addPolygons(color = "#444444", 
                    weight = 1, smoothFactor = 0.5, opacity = 1, fillOpacity = 0.5, fillColor = ~palnumericS(puntajes$Salud), 
                    group = "Salud", highlightOptions = highlightOptions(color = "white", 
                                                                         weight = 2, bringToFront = TRUE), label = ~puntajes$COMUNAS, labelOptions = labelOptions(direction = "auto"), 
                    popup = popup) %>% 
        addLegend(position = "topright", pal = palnumericS, values = ~puntajes$Salud, 
                  title = "Puntaje")
    }
    
    else if (input$rank=='Empleo'){
      
      leaflet(puntajes) %>% addProviderTiles("CartoDB") %>% 
        addPolygons(data = puntajes, color = "#444444", weight = 1, smoothFactor = 0.5, 
                    opacity = 1, fillOpacity = 0.5, fillColor = ~palnumericE(puntajes$Empleo), 
                    group = "Empleo", highlightOptions = highlightOptions(color = "white", 
                                                                          weight = 2, bringToFront = TRUE), label = ~puntajes$COMUNAS, labelOptions = labelOptions(direction = "auto"), 
                    popup = popup) %>% 
        addLegend(position = "topright", pal = palnumericE, values = ~puntajes$Empleo, 
                  title = "Puntaje")
    }
    
    else if (input$rank=='Cercanía'){
      
      leaflet(puntajes) %>% addProviderTiles("CartoDB") %>% 
        addPolygons(data = puntajes, color = "#444444", weight = 1, smoothFactor = 0.5, 
                    opacity = 1, fillOpacity = 0.5, fillColor = ~palnumericE(puntajes$Cercanía), group = "Cercanía", 
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                    label = ~puntajes$COMUNAS, labelOptions = labelOptions(direction = "auto"), 
                    popup = popup) %>% 
        addLegend(position = "topright", pal = palnumericC, values = ~puntajes$Cercanía, 
                  title = "Puntaje")
    }
    
    else if (input$rank=='Vivienda'){
      
      leaflet(puntajes) %>% addProviderTiles("CartoDB") %>% 
        addPolygons(data = puntajes, color = "#444444", weight = 1, smoothFactor = 0.5, 
                    opacity = 1, fillOpacity = 0.5, fillColor = ~palnumericE(puntajes$Vivienda), group = "Vivienda", 
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                    label = ~puntajes$COMUNAS, labelOptions = labelOptions(direction = "auto"), 
                    popup = popup) %>% 
        addLegend(position = "topright", pal = palnumericV, values = ~puntajes$Vivienda, 
                  title = "Puntaje")
    }
    
    
    
    
  })
  
  
  
  #TABLA PUNTAJES
  
  output$puntajes=renderDataTable({
    
    st_geometry(puntajes)=NULL
    
    puntajes %>%  select(COMUNAS,Total,Empleo,Vivienda,Cercanía,Salud,Demografía)
  })
  
  #CONCLUSIONES
  
  
  output$mapacon1=renderLeaflet({
    leaflet(CASOS100) %>% addProviderTiles("CartoDB") %>% 
      addPolygons(data = CASOS100, color = "#444444", weight = 1, smoothFactor = 0.5, 
                  opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric100(CASOS100$casos100), group = "Casos", 
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                  label = ~CASOS100$casos100, labelOptions = labelOptions(direction = "auto"), 
                  popup = popup) %>% 
      addLegend(position = "topright", pal = palnumeric100, values = ~CASOS100$casos100, 
                title = "Casos c/100 mil hab.")
    
  })
  
  output$mapacon2=renderLeaflet({
    leaflet(puntajes) %>% addProviderTiles("CartoDB") %>% 
      addPolygons(data = puntajes, color = "#444444", weight = 1, smoothFactor = 0.5, 
                  opacity = 1, fillOpacity = 0.5, fillColor = ~palnumericTotal(puntajes$Total), group = "Casos", 
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                  label = ~puntajes$Total, labelOptions = labelOptions(direction = "auto"), 
                  popup = popup) %>% 
      addLegend(position = "topright", pal = palnumericTotal, values = ~puntajes$Total, 
                title = "Puntaje total")
    
  })
  
  
  output$tablacon2=renderTable({
    st_geometry(puntajes)=NULL
    
    puntajes %>%
      top_n(5,Total) %>%
      arrange(desc(Total)) %>%
      select(COMUNAS,BARRIOS,Total)
    
  })
  
  output$tablacon1=renderTable({
    st_geometry(CASOS100)=NULL
    
    CASOS100 %>%
      top_n(6,casos100) %>%
      arrange(desc(casos100)) %>%
      mutate(COMUNAS=COMUNA) %>% 
      select(COMUNAS,BARRIOS,casos100)
  })
  
  
}