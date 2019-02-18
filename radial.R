# radial distribution function

# shape: OGR object containing city data
# blgPos: position of building aroung which the radial distribution should be computed
# avgSize: average size of buildings (by radius)
radialDist <- function(shape, avgBlgSize, citySize, blgPos)
{
  N <- 0   # number of bulidings inside designated area
  g <- 0   # radial dist function
  n <- nrow(shape@data)  # total number of buildings
  m <- citySize/(avgBlgSize/20)   # number of subdivisions of r into dr sized bins (dr is 5% of avgBlgSize)
  Aground <- (10*avgBlgSize)^2
  dist <- rep(m, 0)
  blgCoords <- matrix(nrow = 1000, ncol = 2, data = NA)    # overestimate the max number of buildings in Aground to 300
  
  # store the coordinates of the buildings located in the desired area (Aground) to save one round of looping
  for(i in 1:n)
  {
    coords = shape@polygons[[i]]@Polygons[[1]]@labpt
    
    if(coords[1] < blgPos[1] + 5*avgBlgSize & coords[1] > blgPos[1] - 5*avgBlgSize)    # if buiding is within x-bound
    {
      if(coords[2] < blgPos[2] + 5*avgBlgSize & coords[2] > blgPos[2] - 5*avgBlgSize)    # y-bound
      {
        N <- N + 1
        blgCoords[N,] <- coords
      }
    }
  }
  
  for(k in 1:m)   # each k is a binned value for r
  {
    r <- k*0.5*citySize/m
    dr <- avgBlgSize/20
    print(k)
    
    for(i in 1:N)   # loop through all buildings inside Aground
    {
      countBlgs <- 0
      
      for(j in 1:n)   # for each one, loop through all other buildings to compute radial distance (could be sped up with a data structure)
      {
        coords = shape@polygons[[j]]@Polygons[[1]]@labpt    # outer building coordinates
        d <- distance(blgCoords[i,1], blgCoords[i,2], coords[1], coords[2])    # distance between outer building and reference building
        
        if(d < r + dr & d > r)
        {
          countBlgs <- countBlgs + 1
        }
      }
      
      g <- g + countBlgs
      
      # if(i%%10 == 0)
      # {
      #   print("Part 2")
      #   print(i*100/N)
      #   print("%")
      # }
    }
    
    dist[k] <- (1/N)*(Aground/N)*(1/(2*pi*r*dr))*g
  }
  
  return(dist)
}


##########################################################
############# EXAMPLE

# shape   # beirut ogr object

# first compute average building size. obtain city size from gis measurement (or double loop all buildings)
avgBlgSize <- 66 # mean(beiTable[,"radius"])
citySize <- 6500   # 6.5 km between farthest two buildings in this dataset

dist <- radialDist(shape, avgBlgSize, citySize, c(-337590, -28928))




