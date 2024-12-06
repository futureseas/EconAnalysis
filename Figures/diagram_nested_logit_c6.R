

gc()
rm(list = ls())
library(data.tree)


vessel2 <- Node$new("Vessel")
  part <- vessel2$AddChild("Participate\nin CPS")
    omck <- part$AddChild("Mackerel")
      laa <- omck$AddChild("Columbia River (OR)")
    nanc <- part$AddChild("Anchovy")
      clo <- nanc$AddChild("Columbia\nRiver (OR)")
      cwa <- nanc$AddChild("Coastal WA")
      clw <- nanc$AddChild("Columbia\nRiver (WA)")
    psdn <- part$AddChild("Sardine")
      clo <- psdn$AddChild("Columbia\nRiver (OR)")
      cwa <- psdn$AddChild("Coastal WA")
      clw <- psdn$AddChild("Columbia\nRiver (WA)")
      cba <- psdn$AddChild("Coos Bay")
  part_dcrb <- vessel2$AddChild("Participate\nin Crab")
  dcrb <- part_dcrb$AddChild("Dungeness\nCrab")
    cwa <- dcrb$AddChild("Coastal WA")
  part_slmn <- vessel2$AddChild("Participate\nin Salmon")
   sock <- part_slmn$AddChild("Sockeye")
   nps <- sock$AddChild("North\nPuget Sound")
nopart <- vessel2$AddChild("No\nparticipation")
  
SetGraphStyle(vessel2, rankdir = "LR")
SetEdgeStyle(vessel2, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(vessel2, style = "filled,rounded", shape = "box", fontcolor = "black",fillcolor = "lightgrey", 
             fontname = "helvetica", tooltip = GetDefaultTooltip)
SetNodeStyle(vessel2$`No\nparticipation`, fillcolor = "firebrick1")
SetNodeStyle(vessel2$`Participate\nin CPS`, fillcolor = "ivory1")
SetNodeStyle(vessel2$`Participate\nin Salmon` , fillcolor = "powderblue")
SetNodeStyle(vessel2$`Participate\nin Crab` , fillcolor = "powderblue")
plot(vessel2)

