### Create diagram for the CPS fishery ###

pacman::p_load(
  DiagrammeR,     # for flow diagrams
  networkD3,      # For alluvial/Sankey diagrams
  tidyverse)      # data management and visualization

DiagrammeR::grViz("               # All instructions are within a large character string
digraph surveillance_diagram {    # 'digraph' means 'directional graph', then the graph name 
  
  # graph statement
  #################
  graph [layout = dot,
         rankdir = LR,            # layout top-to-bottom
         fontsize = 12]
  

  # nodes (circles)
  #################
  node [shape = circle,           # shape = circle
       fixedsize = false]                      
  
  r1 [label = 'Availability'] 
  c1 [label = 'Fuel prices'] 
  c2 [label = 'Wages'] 
  c3 [label = 'Distance\ntravelled'] 
  p1 [label = 'Demand']
  p2 [label = 'Supply']
  p3 [label = 'International\ncatches']
  Prices [label = 'Prices']
  Prices [label = 'Prices']
  Cost [label = 'Expected\nCost',
            fontcolor = red]
  Revenue [label = 'Expected\nRevenue',
            fontcolor = darkgreen]
  l1 [label = 'Closures'] 
  Landings [style = filled, 
            label = 'Landings of\nspecies S at port J',
            fontcolor = black,
            fillcolor = PowderBlue]

  Participation [style = filled, 
            label = 'Participation in \nfishery S at port J',
            fontcolor = black,
            fillcolor = PowderBlue]

  # edges
  #######
  p1 -> Prices [label = '+',
                          fontcolor = darkgreen,
                          color = darkgreen]
  p2 -> Prices [label = '-',
                          fontcolor = red,
                          color = red]
                          
  Participation -> Landings [label = '+',
                          fontcolor = darkgreen,
                          color = darkgreen]

  Revenue -> Participation [label = '-',
                          fontcolor = darkgreen,
                          color = darkgreen]
                          
  Landings -> p2 [label = 'Local supply (+)',
                          fontcolor = darkgreen,
                          color = darkgreen]
                      
  p3 -> p2 [label = '(+)',
                          fontcolor = darkgreen,
                          color = darkgreen]
                          
                          
  Landings -> r1 [label = '-',
                          fontcolor = red,
                          color = red]
                          
  r1 -> l1 [label = '-',
                          fontcolor = red,
                          color = red]
                          
  r1 -> c3 [label = '?']
                          

  # grouped edge
  {r1 Prices} -> Revenue [label = '+',
                                      fontcolor = darkgreen,
                                      color = darkgreen]
                                      
  {l1 Cost} -> Participation [label = '-',
                                      fontcolor = red,
                                      color = red]
                                      
  {c1 c2 c3} -> Cost [label = '+',
                          fontcolor = darkgreen,
                          color = darkgreen]
                          
  {c1 c2} -> c3 [label = '-',
                          fontcolor = red,
                          color = red]
                                      
}
")

