#rotations/shiny/rotresist1/server.r
#andy south 9/10/17
#initially copied from resistance/resistmopb2


library(shiny)
#library(devtools)
#install_github('AndySouth/rotations')
library(rotations)


shinyServer(function(input, output) {
    
  
  output$plotA <- renderPlot({
    
    #add dependency on the button
    if ( input$aButtonRunA >= 0 ) 
    {
      #isolate reactivity of other objects
      isolate({
        
        #cat("running resistSimple with these inputs:", input$P_1, input$P_2*input$P_1, input$h.RS1_00, input$h.RS2_00,"\n")
        
        run_rot(n_insecticides = input$n_A, 
                rotation_interval = 50, 
                max_generations = 200,
                eff = input$effectiveness_A1,
                dom = input$dominance_A1,
                rr = input$advantage_A1,
                cost = input$cost_A1)
        
        # runcurtis_f2( max_gen = 500,
        #               P_1 = input$frequency_A1, 
        #               P_2 = input$frequency_A2,
        #               #P_2 = input$P_2*input$P_1, #if doing 2 as a proportion of 1, i need to put a limit of 1 on this
        #               h.RS1_A0 = input$dominance_A1, 
        #               h.RS2_0B = input$dominance_A2,
        #               exposure = input$exposure_A,
        #               phi.SS1_A0 = input$effectiveness_A1,
        #               phi.SS2_0B = input$effectiveness_A2,
        #               rr_restoration_ins1 = input$advantage_A1,
        #               rr_restoration_ins2 = input$advantage_A2,
        #               cex.axis = 1,
        #               addCombinedStrategy = FALSE )

                
        #a hack to output the inputs
        # cat("A:\n")
        # cat("runcurtis_f2( max_gen=500, ", 
        #               "P_1 =",input$frequency_A1,",", 
        #               "P_2 =",input$frequency_A2,",", 
        #               "h.RS1_A0 =",input$dominance_A1,",", 
        #               "h.RS2_0B =",input$dominance_A2,",",
        #               "exposure =",input$exposure_A,",",
        #               "phi.SS1_A0 =",input$effectiveness_A1,",",
        #               "phi.SS2_0B =",input$effectiveness_A2,",",
        #               "rr_restoration_ins1 =",input$advantage_A1,",",
        #               "rr_restoration_ins2 =",input$advantage_A2,",",
        #               "addCombinedStrategy = FALSE,", 
        #               "strategyLabels = c('seq','','adapt','mix2')",
        #               ")\n" )
        
               
      }) #end isolate  
    } #end button
  })

  output$plotB <- renderPlot({
    
    #add dependency on the button
    if ( input$aButtonRunB >= 1 ) 
    {
      #isolate reactivity of other objects
      isolate({
        
        run_rot(n_insecticides = input$n_B,
                rotation_interval = 50,
                max_generations = 200,
                eff = input$effectiveness_B1,
                dom = input$dominance_B1,
                rr = input$advantage_B1,
                cost = input$cost_B1)
        
        
        # runcurtis_f2( max_gen = 500,
        #               P_1 = input$frequency_B1, 
        #               P_2 = input$frequency_B2,
        #               h.RS1_A0 = input$dominance_B1, 
        #               h.RS2_0B = input$dominance_B2,
        #               exposure = input$exposure_B,
        #               phi.SS1_A0 = input$effectiveness_B1,
        #               phi.SS2_0B = input$effectiveness_B2,
        #               rr_restoration_ins1 = input$advantage_B1,
        #               rr_restoration_ins2 = input$advantage_B2,
        #               cex.axis = 1,
        #               addCombinedStrategy = FALSE )
        
        #a hack to output the inputs
        # cat("B:\n")
        # cat("run_rot( max_gen=500, ", 
        #     "P_1 =",input$frequency_B1,",", 
        #     "P_2 =",input$frequency_B2,",", 
        #     "h.RS1_A0 =",input$dominance_B1,",", 
        #     "h.RS2_0B =",input$dominance_B2,",",
        #     "exposure =",input$exposure_B,",",
        #     "phi.SS1_A0 =",input$effectiveness_B1,",",
        #     "phi.SS2_0B =",input$effectiveness_B2,",",
        #     "rr_restoration_ins1 =",input$advantage_B1,",",
        #     "rr_restoration_ins2 =",input$advantage_B2,",",
        #     "addCombinedStrategy = FALSE,", 
        #     "strategyLabels = c('seq','','adapt','mix2')",
        #     ")\n" )
        
      }) #end isolate  
    } #end button
  })  
  
    
})