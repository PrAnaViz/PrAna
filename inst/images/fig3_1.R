DiagrammeR::grViz ("digraph {

graph [layout = dot, rankdir = LR, compound = true, style = invis]

node [shape = rectangle, 
      style = filled, 
      fixedsize = true,
      width = 2.8,
      height = 1,
      fontsize = 20,
      fillcolor = Linen]

data1 [label = 'NHS \n Prescription \n Dataset', shape = folder, fillcolor = Beige,  width = 2]
data2 [label = 'dm+d \n files', shape = folder, fillcolor = Beige,  width = 2 ]
process [label =  'Data \n Preparation', fillcolor = LightSteelBlue]
conversion [label = 'Data \n Conversion', fillcolor = LightSteelBlue]
visualise [label= 'Visualise & \n Analyse', fillcolor = LightSteelBlue]
tab1 [label = 'Targeted \n Approach', shape = tab, fillcolor = LavenderBlush ]
tab2 [label = 'Non-targeted \n Approach', shape = tab, fillcolor = LavenderBlush ]

# edge definitions with the node IDs
subgraph cluster1 {
{data1 data2}  -> process -> conversion -> visualise -> {tab1 tab2} 
}

}")
