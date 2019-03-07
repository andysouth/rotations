#rotations/shiny/rotresist1/server.r
#andy south 9/10/17
#initially copied from resistance/resistmopb2


library(shiny)
#before deploying to shinyapps need to install rotations from github
#library(devtools)
#install_github('ian-hastings/rotations')
library(rotations)


shinyServer(function(input, output) {
    
  #default resolution is 72, increasing it makes plot elements crisper but bigger
  output$plotA <- renderPlot(res = 82, {
    
    #add dependency on the button, make this 1 if don't want it to appear first time
    if ( input$aButtonRunA >= 0 ) 
    {
      #isolate reactivity of other objects
      isolate({
        
        #cat("running resistSimple with these inputs:", input$P_1, input$P_2*input$P_1, input$h.RS1_00, input$h.RS2_00,"\n")
        
        run_rot(n_insecticides =    input$n_A, 
                max_gen =   input$max_gen,
                start_freqs =       input$frequency_A,
                rot_interval = input$rot_interval_A, 
                eff =               input$effectiveness_A,
                dom_sel =           input$dom_sel_A,
                dom_cos =           input$dom_cos_A,
                rr =                input$advantage_A,
                expo_hi =           input$exposure_A,
                coverage =          input$coverage_A,
                migration =         input$migration_A, 
                cost =              input$cost_A,
                logy =              input$logy, #inefficient that it reruns when changing to log
                no_r_below_start =  input$no_r_below_start,
                min_rwr_interval =  input$min_rwr_interval,
                threshold =         input$threshold,
                mort_or_freq =      input$mort_or_freq
                )
                
        # a hack to output the inputs, so it can be run from the console
        cat("A:\n")
        cat("run_rot( n_insecticides =",input$n_A,",", 
                      "max_gen =",input$max_gen,",", 
                      "start_freqs =",input$frequency_A,",", 
                      "rot_interval =",input$rot_interval_A,",",
                      "eff =",input$effectiveness_A,",",
                      "dom_sel =",input$dom_sel_A,",",
                      "dom_cos =",input$dom_cos_A,",",
                      "rr =",input$advantage_A,",",
                      "expo_hi =",input$exposure_A,",",
                      "coverage =",input$coverage_A,",",
                      "migration =",input$migration_A,",",
                      "cost =",input$cost_A,",",
                      "logy =",input$logy,",",
                      "no_r_below_start =",input$no_r_below_start,",",
                      "min_rwr_interval =",input$min_rwr_interval,",",
                      "threshold =",input$threshold,",",
                      "mort_or_freq ='",input$mort_or_freq,"'",
                      ")\n" )
        
               
      }) #end isolate  
    } #end button
  })

  output$plotB <- renderPlot(res = 82,{
    
    #add dependency on the button, make this 1 if don't want it to appear first time
    if ( input$aButtonRunB >= 0 ) 
    {
      #isolate reactivity of other objects
      isolate({
        
        run_rot(n_insecticides =    input$n_B, 
                max_gen =   input$max_gen,
                start_freqs =       input$frequency_B,
                rot_interval = input$rot_interval_B, 
                eff =               input$effectiveness_B,
                dom_sel =           input$dom_sel_B,
                dom_cos =           input$dom_cos_B,
                rr =                input$advantage_B,
                expo_hi =           input$exposure_B,
                coverage =          input$coverage_B,
                migration =         input$migration_B, 
                cost =              input$cost_B,
                logy =              input$logy, #inefficient that it reruns when changing to log
                no_r_below_start =  input$no_r_below_start,
                min_rwr_interval =  input$min_rwr_interval,
                threshold =         input$threshold,
                mort_or_freq =      input$mort_or_freq )
        
        
        # a hack to output the inputs, so it can be run from the console
        cat("B:\n")
        cat("run_rot( n_insecticides =",input$n_B,",", 
            "max_gen =",input$max_gen,",", 
            "start_freqs =",input$frequency_B,",", 
            "rot_interval =",input$rot_interval_B,",",
            "eff =",input$effectiveness_B,",",
            "dom_sel =",input$dom_sel_B,",",
            "dom_cos =",input$dom_cos_B,",",
            "rr =",input$advantage_B,",",
            "expo_hi =",input$exposure_B,",",
            "coverage =",input$coverage_B,",",
            "migration =",input$migration_B,",",
            "cost =",input$cost_B,",",
            "logy =",input$logy,",",
            "no_r_below_start =",input$no_r_below_start,",",
            "min_rwr_interval =",input$min_rwr_interval,",",
            "threshold =",input$threshold,",",
            "mort_or_freq ='",input$mort_or_freq,"'",
            ")\n" )
        
      }) #end isolate  
    } #end button
  })  
  
    
})