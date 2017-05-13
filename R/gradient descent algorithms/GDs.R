library(rgl)

### Loss Function
lossFunction=function(y, betas, x){
  #y_hat = log(betas[1]*x + betas[2])
  y_hat = sin(betas[1]*x + betas[2])
  errors = sum(abs((y - y_hat)))
  errors
}

## Function  - One var
n = 1000

betas = c(3,1)

x = sort(runif(n, 0, 10))
#y = betas[2]*sin(betas[1]*x) + cos(betas[2]*x)
y = sin(betas[1]*x + betas[2])

plot(x,y)


beta_1 = vector('double')
beta_2 = vector('double')
z = vector('double')

start=0.03125
end=5
step = 0.03125
for(i in seq(start, end, step)){
  for(j in seq(start, end, step)){
    beta_1 = c(beta_1, i)
    beta_2 = c(beta_2, j)
    z = c(z, lossFunction(y, c(i,j), x))
  }
}

nColors = 128
colindex = as.integer(cut(z,breaks=nColors))

plot3d(beta_1, beta_2, z,col=tim.colors(nColors)[colindex])
