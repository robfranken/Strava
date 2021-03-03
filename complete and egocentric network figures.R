#### Figures for networks ####
rm (list = ls( ))
setwd("C:\\Users\\u244147\\Documents\\dissertatie\\Strava data\\clubs") 
getwd()


library(igraph)


####################################################

# First, we make a directed 'club' network, called club

club <- make_graph(edges=c("A", "B", "B", "A",
                           "A", "C", "C", "A",
                           "B", "C", "C", "B",
                           "A", "D", "D", "A", "B", "D", "D", "B", "C", "D", "D", "C",
                           "E", "F", "F", "E",
                           "E", "G", "G", "E", "G", "F",
                           "H", "I", "I", "H",
                           "A", "E"),
                   isolates=c("J", "K", "L", "T", "Q", "R"),
                   directed=T)
plot(club)           

vertex_attr(club)$color <- c("#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA", "#BEBADA","#BEBADA", "#8DD3C7","#BEBADA", "#FFFFB3", "#8DD3C7")
vertex_attr(club)
  
  
plot(simplify(club),
     
     # === vertex
     vertex.frame.color = "black",                 # Node border color
     vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     vertex.size=16,                               # Size of the node (default is 15)
     vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
     
     # === vertex label
     vertex.label=NA,                              # Character vector used to label the nodes
     vertex.label.family="Times",                  # Font family of the label (e.g.“Times”, “Helvetica”)
     vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=1,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="black",                           # Edge color
     edge.width=2,                                 # Edge width, defaults to 1
     edge.arrow.size=.7,                          # Arrow size, defaults to 1
     edge.arrow.width=1,                           # Arrow width, defaults to 1
     edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
     edge.curved=0   ,                             # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
)

title(main = "Complete network of Kudo-ties in fictional Strava club (N=13)")


####################################################

####################################################

# Now we make the ego-centered (2.5 degree) network

ego <- make_graph(edges=c("A", "B", "A", "C", "A", "D", "A", "E",
                          "B", "F", "B", "G", "B", "X", 
                          "C", "H", "C", "I", "C", "Z",
                          "D", "K", "D", "M", "D", "Q",
                          "E", "P", "E", "T", "E", "N",
                          "G", "F", "Z", "I", "X", "G", "P", "T", "N", "P", "T", "N",
                          "Q", "M", "K", "M",
                          "I", "K", "B", "N"), 
                  directed = F)
ego
plot(simplify(ego))


vertex_attr(ego)$shape <- c("square", "circle", "circle", "circle", "circle", "circle", "circle", "circle", "circle", "circle", "circle", "circle","circle", "circle","circle", "circle", "circle")
vertex_attr(ego)$color <- c("#BEBADA", "#8DD3C7", "#8DD3C7", "#8DD3C7", "#8DD3C7",
                            "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3", "#FFFFB3")

plot(ego, 
     
     # === vertex
     vertex.frame.color = "black",                 # Node border color
     vertex.shape=vertex_attr(ego)$shape,                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     vertex.size=16,                               # Size of the node (default is 15)
     vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
     
     # === vertex label
     vertex.label=NA,                              # Character vector used to label the nodes
     vertex.label.family="Times",                  # Font family of the label (e.g.“Times”, “Helvetica”)
     vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=1,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="black",                           # Edge color
     edge.width=2,                                 # Edge width, defaults to 1
     edge.arrow.size=1,                            # Arrow size, defaults to 1
     edge.arrow.width=1,                           # Arrow width, defaults to 1
     edge.lty=c("solid","solid", "solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid", "solid","solid","solid","solid","solid","solid","solid","solid","solid","solid",  "dotted", "dotted"),                       
     edge.curved=0   ,                             # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
)


