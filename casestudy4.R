
drawD3 = function(csv_path){
  library(ggplot2)
  library(scatterD3) 
  library(d3heatmap) 
  library(ggrepel) 
  library(htmlwidgets) 
  library(data.table) 
  
  dt <- fread(csv_path, header=TRUE)
  dt <- dt[team!='TOTAL']
  
  # plot sum of squares as a function of cluster count in order # to find the "elbow". Optimal cluster count was found to be 7
  
  cl <- kmeans(dt[, list(x, y)], centers=7)
  
  positions <- c('Shooting Guard', 'Dynamic C/PF',
                 'Slow C/PF', 'Dynamic SF/PF',
                 'Slow SF/PF', 'Point Guard', 'Stretch C/PF')
  
  
  dt[, 'cluster' := cl$cluster]
  dt[, 'position' := positions[dt$cluster]]
  
  tooltips <- paste("<strong>", dt$player,"</strong><br /><strong>", dt$team, "</strong><br />")
  
  p <- scatterD3(x = dt$x,
                 y = dt$y,
                 lab = dt$player,
                 col_var=dt$team,
                 symbol_var=dt$position,
                 point_opacity = 0.7,
                 tooltip_text = tooltips,
                 col_lab = "Team",
                 symbol_lab = "Position Group",
                 width=1000,
                 height=850)
  return(p)

}

d3= drawD3('C:/Users/Shaowei/shiyishi.csv')

save2html= function(picture,name){
  saveWidget(picture, paste(name,'.html',''))
}


drawD3AdSave = function(csv_path,name){
  d3= drawD3(csv_path)
  save2html(d3,name)
  
}

drawD3AdSave('C:/Users/Shaowei/Desktop/DS501/casestudy4/2014_casestudy4.csv','2014_casestudy4')
