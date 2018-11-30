library(datasets)
iris.df =data(iris)
data("iris")
iris.df=iris[1:100 , ]

total_cost = 0

lr = 0.0000001

theta = c(0,0,0,0,0)

diff = c(0,0,0,0,0)

J = vector(mode = "numeric" , length = 300 )

iris.df$y = 0

iris.df$y1[iris.df$Species =="setosa"] = 1

sigmoid = function(z)
{
  sig = 1/( 1 + exp(-z) )
  
  return(sig)  
}

for (j in 1:300) {
  
  
  for (i in 1:100) {
    
    #Hypothesis 
    hypo = sigmoid ( 1*theta[1] + iris.df$Sepal.Length[i]*theta[2] + iris.df$Sepal.Width[i]*theta[3] + iris.df$Petal.Length[i]*theta[4] + iris.df$Petal.Width[i]*theta[5] )
    
    #Backward Propagation For First Layer
    
    diff[1] = diff[1] + hypo*(1-hypo) *(hypo-y[i])  #For Baised Node
    
    diff[2] = diff[2] + iris.df$Sepal.Length[i] * hypo*(1-hypo) * (hypo-y[i])  
    
    diff[3] = diff[3] + iris.df$Sepal.Width[i] * hypo*(1-hypo) * (hypo-y[i])  
    
    diff[4] = diff[4] + iris.df$Petal.Length[i] * hypo*(1-hypo) * (hypo-y[i]) 
    
    diff[5] = diff[5] + iris.df$Petal.Width[i] * hypo*(1-hypo) *(hypo-y[i]) 
    
    
    cost = ( iris.df$y[i] * log(hypo) + (1-y[i]) * log(1-hypo) )/nrow(iris.df)
    
    total_cost = total_cost + cost
    
  }
  
  ############# Weights Updation
  
  J[j] = total_cost
  
  theta[1] = theta[1] - (lr/nrow(iris.df)) * diff[1]
  
  theta[2] = theta[2] - (lr/nrow(iris.df)) * diff[2]
  
  theta[3] = theta[3] - (lr/nrow(iris.df)) * diff[3]
  
  theta[4] = theta[4] - (lr/nrow(iris.df)) * diff[4]
  
  theta[5] = theta[5] - (lr/nrow(iris.df)) * diff[5]
  
}


plot(J)

