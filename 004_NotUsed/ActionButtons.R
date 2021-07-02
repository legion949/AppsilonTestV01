# Ships Names and Id
output$buttons <- renderUI({ 
  
  div(
    br(),br(),br(),
    actionButton(inputId = "b1", label = "Best Example 1 -  Vessel"),
    actionButton(inputId = "b2", label = "Best Example 2 - Air Craft"),
    actionButton(inputId = "b3", label = "Best Example 3 - LigthHouse"),
    br(),
    br(),
    actionButton(inputId = "b4", label = "Bad Example 1 -  Wrong Ship Name"),
    
  )
})

######



###############################################
# Example Selection
{
  ###
  
  
  
  
  # Selected Example
  my_example <- reactiveVal(1)
  
  # Example 1 - Best Example 1 - Vessel
  observeEvent(input$b1, {
    my_example(1)
    
  })
  
  
  # Example 2 - Best Example 2 - Air Craft
  observeEvent(input$b2, {
    my_example(2)
    
  })
  
  
  
  # Example 3 - Best Example 3: LightHouse
  observeEvent(input$b3, {
    my_example(3)
    
  })
  
  # Example 4 - Bad Example 1
  observeEvent(input$b4, {
    my_example(4)
    
  })
  
  
  # Example 5 - Random Example
  observeEvent(input$b5, {
    my_example(5)
    
  })
  
  #######################################################################
  observe(,{
    
    
    
    
    
    
    
    # Can also set the label and select items
    updateSelectInput(session, "my_ship_type_fusion",
                      label = 'Ship Type',
                      choices = internal_options()[[1]],
                      selected = example_details[my_example(),1]
    )
    
    updateRadioButtons(session, "orden01",
                       label = "Order Type",
                       choices = internal_options()[[2]],
                       selected = example_details[my_example(),2]
                       
                       
                       
                       
                       
                       # Can also set the label and select items
                       updateSelectInput(session, "inVar2",
                                         label = h4(input$orden01),
                                         choices = lvl_ship_name,
                                         selected = lvl_ship_name[names(lvl_ship_name) == example_details[my_example(),3]]
                       )
                       
                       
                       
  })
    
    
    ###    
}


###############################################  