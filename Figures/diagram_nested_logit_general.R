

gc()
rm(list = ls())
library(data.tree)

vessel2 <- Node$new("Vessel")
part <- vessel2$AddChild("Participate")
msqd <- part$AddChild("Species 1")
laa <- msqd$AddChild("Port 1")
mna <- msqd$AddChild("Port 2")
sba <- msqd$AddChild("Port 4")
cmck <- part$AddChild("Species 2")
laa <- cmck$AddChild("Port 2")
sba <- cmck$AddChild("Port 3")
nanc <- part$AddChild("Species 3")
laa <- nanc$AddChild("Port 1")
mna <- nanc$AddChild("Port 2")
sfa <- nanc$AddChild("Port 4")
nopart <- vessel2$AddChild("No\nparticipation")
nospec <- nopart$AddChild("None")
noport <- nospec$AddChild("None")

SetGraphStyle(vessel2, rankdir = "TB")
SetEdgeStyle(vessel2, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(vessel2, style = "filled,rounded", shape = "box", fontcolor = "black",fillcolor = "lightgrey", 
             fontname = "helvetica", tooltip = GetDefaultTooltip)
SetNodeStyle(vessel2$`No\nparticipation`, fillcolor = "firebrick1")
SetNodeStyle(vessel2$Participate, fillcolor = "powderblue")
SetNodeStyle(vessel2$Participate$`Species 1`, fillcolor = "ivory1")
SetNodeStyle(vessel2$Participate$`Species 2`, fillcolor = "ivory1")
SetNodeStyle(vessel2$Participate$`Species 3`, fillcolor = "ivory1")
plot(vessel2)

