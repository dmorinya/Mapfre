library(doParallel)

nCores     <- detectCores()
registerDoParallel(nCores)

##
# Over-reporting modeling: Simulation study 
##

# Function for computing probabilities
# transition probabilities P(X(n)|X(n-1)):
# data: time series of counts (data[1]:x(n-1) and data[2]:x(n))
# p: vector of values for alpha and lambda (p[1]:alpha and p[2]:lambda)
tp = function ( data , p ) {
  kk = 0 : min ( data [ 1 ] ,
                 data [ 2 ] )
  sum ( dbinom ( kk , 
                 data [ 1 ] , 
                 p [ 1 ] ) * 
          dpois ( data [ 2 ] - kk , 
                  p [ 2 ] ) )
  }

# pn function computes the probabilities needed for the emission probabilities:
# y: value of y
# x: value of x
# p: vector of probabilities (p[4]:phi(1) and p[5]:phi(2))
pn = function ( y , x , p ) {
  
  pn = NULL
  
  if ( y == 0 ) { 
    pn [ 1 ] = ( 1 - p [ 4 ] - p [ 5 ] ) ^ x
  }
  
  if ( y == 1 ) {
    pn [ 1 ] = ( 1 - p [ 4 ] - p [ 5 ] ) ^ x
    pn [ 2 ] = x * p [ 4 ] * ( 1 - p [ 4 ] - p [ 5 ] ) ^ ( x - 1 )
    }
  if ( y >= 2 ) {
    pn [ 1 ] = ( 1 - p [ 4 ] - p [ 5 ] ) ^ x
    pn [ 2 ] = x * p [ 4 ] * ( 1 - p [ 4 ] - p [ 5 ] ) ^ ( x - 1 ) 
    
    for ( j in 3 : ( y + 1 ) ) { 
      pn [ j ] = ( 1 / ( ( j - 1 ) * ( 1 - p [ 4 ] - p [ 5 ] ) ) ) * 
        ( ( p [ 4 ] * ( x - ( j - 2 ) ) * 
              pn [ j - 1 ] ) + 
            ( p [ 5 ] * ( 2 * x - ( j - 3 ) ) * pn [ j - 2 ] ) )
    }
  }
  
  return ( pn )
  
  }

# ep: computes the emission probabilities of the model
# data: data frame with time series of counts (data[1]:x(n-1), data[2]:x(n) and data[3]:y(n))
# p: probabilities vector (p[3]:omega)
ep = function ( data , p ) {
  e.aux = NULL
  if ( data [ 2 ] > data [ 3 ] ) {
    e.aux = p [ 3 ] * pn ( y = data [ 3 ] , 
                           x = data [ 2 ] , 
                           p = p ) [ data [ 3 ] + 1 ] 
  }
  if ( data [ 2 ] == data [ 3 ] ) { 
    e.aux = ( ( 1 - p [ 3 ] ) + 
                p [ 3 ] * pn ( y = data [ 3 ] , 
                               x = data [ 2 ] , 
                               p = p ) [ data [ 3 ] + 1 ] )
    }
  if ( data [ 2 ] < data [ 3 ] & data [ 2 ] >= data [ 3 ] / 2 ) {
    e.aux = p [ 3 ] * pn ( y = data [ 3 ] , 
                           x = data [ 2 ] ,
                           p = p ) [ data [ 3 ] + 1 ] 
    }
  if ( data [ 2 ] < data [ 3 ] / 2 ) {
    e.aux = 0 
    }
  
  e.aux
  }

# gamma(n)
# data: data frame with time series of counts (data[4]:, data[5]: and data[6]:)
gp = function ( data ) {
  exp ( log ( data [ 4 ] ) + 
          log ( data [ 5 ] ) + 
          log ( data [ 6 ] ) ) 
  }

# LF
# p: probabilities vector (p[1]: alpha, p[2]: lambda, p[3]:omega, p[4]:phi(1), p[5]:phi(2))
LF = function ( p , x.aux , y , y.len , x.len ) {
  
  # Step gamma(x(1),y(1))
  data.aux.n1 = NULL
  data.aux.n1 = expand.grid ( x.a = x.aux , 
                              pt.n1 = NA , 
                              pe.n1 = NA , 
                              gamma.1 = NA )
  names ( data.aux.n1 ) = c ( "x(1)" , 
                              "P(x(1))" , 
                              "P(y(1)|x(1))" , 
                              "gamma(1)" )
  
  # P(x(1))
  data.aux.n1 [ , "P(x(1))" ] = dpois ( data.aux.n1 [ , "x(1)" ] , 
                                        p [ 2 ] / ( 1 - p [ 1 ] ) )
  
  # P(y(1)|x(1)) probabilities
  if ( sum ( data.aux.n1 [ , "x(1)" ] >  y [ 1 ] ) != 0 ) {
  data.aux.n1 [ data.aux.n1 [ , "x(1)" ] >  y [ 1 ] , "P(y(1)|x(1))" ] = 
    apply ( cbind ( NA , 
                    data.aux.n1 [ data.aux.n1 [ , "x(1)" ] > y [ 1 ] , "x(1)" ] , y [ 1 ] ) ,
            1 , 
            ep , 
            p = p )
  }
  if ( sum ( data.aux.n1 [ , "x(1)" ] == y [ 1 ] ) != 0 ) {
  data.aux.n1 [ data.aux.n1 [ , "x(1)" ] == y [ 1 ] , "P(y(1)|x(1))" ] = 
    apply ( cbind ( NA , 
                    data.aux.n1 [ data.aux.n1 [ , "x(1)" ] == y [ 1 ] , "x(1)" ] , y [ 1 ] ) , 
            1 , 
            ep , 
            p = p )
  }
  if ( sum ( data.aux.n1 [ , "x(1)" ] <  y [ 1 ] & data.aux.n1 [ , "x(1)" ] >= ( y [ 1 ] ) / 2 ) != 0 ) {
  data.aux.n1 [ data.aux.n1 [ , "x(1)" ] <  y [ 1 ] & data.aux.n1 [ , "x(1)" ] >= ( y [ 1 ] ) / 2 , "P(y(1)|x(1))" ] = 
    apply ( cbind ( NA , 
                    data.aux.n1 [ data.aux.n1 [ , "x(1)" ] < y [ 1 ] & data.aux.n1 [ , "x(1)" ] >= ( y [ 1 ] / 2 ) ,"x(1)" ] , y [ 1 ] ) , 
            1 , 
            ep , 
            p = p ) 
  }
  if ( sum ( data.aux.n1 [ , "x(1)" ] < ( y [ 1 ] / 2 ) ) != 0 ) {
  data.aux.n1 [ data.aux.n1 [ , "x(1)" ] < ( y [ 1 ] / 2 ) , "P(y(1)|x(1))" ] = 0
  }
  
  # gamma(1)
  data.aux.n1 [ , "gamma(1)" ] = exp ( log ( data.aux.n1 [ , "P(x(1))" ] ) + 
                                         log ( data.aux.n1 [ , "P(y(1)|x(1))" ] ) )
  p.forward = NULL
  p.forward = data.aux.n1 [ , "gamma(1)" ]
  
  # Steps gamma(x(n),y(n))
  data.aux = NULL
  data.aux = expand.grid ( x.a = x.aux , 
                           x.b = x.aux )
  data.aux = cbind ( data.aux , 
                     yi = NA , 
                     p.tran = apply ( data.aux , 
                                      1 , 
                                      tp , 
                                      p = p ) , 
                     p.emis = NA , 
                     p.forward = NA )
  names ( data.aux ) = c ( "x(n-1)" , 
                           "x(n)" , 
                           "y(n)" , 
                           "P(x(n)|x(n-1))" , 
                           "P(y(n)|x(n))" , 
                           "gamma(n-1)" )
  
  for ( k in 2 : y.len )
    {
    data.aux [ , "y(n)" ] = y [ k ]
    data.aux [ , "P(y(n)|x(n))" ] = apply ( data.aux , 
                                            1 , 
                                            ep , 
                                            p = p )
    data.aux [ , "gamma(n-1)" ] = rep ( p.forward , 
                                        times = x.len )
    
    forward.aux = data.frame ( data.aux [ , "x(n)" ] , 
                               apply ( data.aux , 
                                       1 , 
                                       gp ) )
    names ( forward.aux ) = c ( "x(n)" , 
                                "gamma(n)" )
    p.forward = tapply ( forward.aux [ , "gamma(n)" ] , 
                         forward.aux [ , "x(n)" ] , 
                         sum )
    }
  
  return ( - log ( sum ( p.forward ) ) )
  }

expSimuML = function ( n , p , th = 3 , conf = 0.95 ) {
 
  # Xn process 
  xn = function ( n , p ) {
    x = NULL
    x [ 1 ] = rpois ( 1 , 
                      p [ 2 ] / ( 1 - p [ 1 ] ) )
    for ( i in 2 : n ) { 
      x [ i ] = rbinom ( 1 , 
                         x [ i - 1 ] ,
                         p [ 1 ] ) + 
        rpois ( 1 , p [ 2 ] )
    }
    return ( x )
  }
  
  # Yn process 
  yn = function ( n , x , p ) {
    y = NULL
    i = rbinom ( n , 
                 1 , 
                 p [ 3 ] )
    for ( j in 1 : n ) {
      if ( i [ j ] == 1 ) {
        y [ j ] = sum ( sample ( 0 : 2 , 
                                 x [ j ] , 
                                 p = c ( 1 - p [ 4 ] - p [ 5 ] ,
                                         p [ 4 ] , 
                                         p [ 5 ] ) ,
                                 replace = TRUE ) )
        } else {
          y [ j ] = x [ j ]
        }
      }
    return ( y )
  }
  
  # Expected ACF
  ACF = function ( p , k ) {
    p [ 1 ] ^ k * 
      ( 1 - p [ 3 ] * ( 1 -p [ 4 ] - 2 * p [ 5 ] ) ) ^ 2 / ( ( 1 - p [ 3 ] * ( 1 - p [ 4 ] - 4 * p [ 5 ] ) ) + 
                                                               ( p [ 2 ] / ( 1 - p [ 1 ] ) ) * p [ 3 ] * ( 1 - p [ 3 ] ) * ( 1 - p [ 4 ] - 2 * p [ 5 ] ) ^ 2 ) 
    }
 
  # Selection of Y process 
  x = xn ( n = n , p = p )
  y = yn ( n = n , x = x , p = p )
  acfE = apply ( cbind ( 1 : 5 ) , 
                 1 , 
                 function ( x ) ACF ( p = p , k = x ) )
  acfO = abs ( as.numeric ( acf ( y , 
                                  plot = FALSE ) $acf [ 2 : 6 ] ) ) 
  c1 = abs ( acfE - acfO ) <= 0.01  
  c2 = abs ( mean ( y ) - mean ( x ) * ( 1 - p [ 3 ] * ( 1 - p [ 4 ] -  2 * p [ 5 ] ) ) ) <= 0.5
  c3 = var ( y ) - ( mean ( x ) * ( 1 - p [ 3 ] * ( 1 - p [ 4 ] - 4 * p [ 5 ] ) ) + 
                       mean ( x ) ^ 2 * p [ 3 ] * ( 1 - p [ 3 ] ) * ( 1 - p [ 4 ] - 2 * p [ 5 ] ) ^ 2 ) <= 0.5 
  
  control = function ( y , c1 , c2 , c3 ) { 
    while ( length ( c ( c1 , c2 , c3 ) ) - sum ( c ( c1 , c2 , c3 ) ) != 0 ) {
      x = xn ( n = n , p = p )
      y = yn ( n = n , x = x , p = p )
      acfE = apply ( cbind ( 1 : 5 ) , 
                     1 , 
                     function ( x ) ACF ( p = p , k = x ) )
      acfO = abs ( as.numeric ( acf ( y , 
                                    plot = FALSE ) $acf [ 2 : 6 ] ) ) 
      c1 = abs ( acfE - acfO ) <= 0.01  
      c2 = abs ( mean ( y ) - mean ( x ) * ( 1 - p [ 3 ] * ( 1 - p [ 4 ] -  2 * p [ 5 ] ) ) ) <= 0.5
      c3 = abs ( var ( y ) - ( mean ( x ) * ( 1 - p [ 3 ] * ( 1 - p [ 4 ] - 4 * p [ 5 ] ) ) + 
                               mean ( x ) ^ 2 * p [ 3 ] * ( 1 - p [ 3 ] ) * ( 1 - p [ 4 ] - 2 * p [ 5 ] ) ^ 2 ) ) <= 0.5 
      
      } 
    y 
    }

  y = control ( y , c1 , c2 , c3 )  
  t.max = th * max ( y )
  x.aux = 0 : t.max
  y.len = length ( y ) 
  x.len = length ( x.aux )
  resNLM = tryCatch ( nlm ( LF , 
                            p = p , 
                            hessian = TRUE , 
                            print.level = 2 , 
                            y = y , 
                            x.aux = x.aux , 
                            y.len = y.len , 
                            x.len = x.len ) ,
                      error = function ( error_message ) { 
                        return ( NA )
                        } )
  if ( sum ( is.na ( resNLM ) ) != 0 ) {
  cr1 = NA 
  cr2 = NA 
  } else {
  cr1 = sum ( is.nan ( resNLM$estimate - qnorm ( conf ) * sqrt ( diag ( solve ( resNLM$hessian ) ) ) ) ) 
  cr2 = sum ( is.nan ( resNLM$estimate + qnorm ( conf ) * sqrt ( diag ( solve ( resNLM$hessian ) ) ) ) ) }
  
  while ( cr1 != 0 | cr2 != 0 | is.na ( cr1 ) | is.na ( cr2 ) ) {
    x = xn ( n = n , p = p )
    y = yn ( n = n , x = x , p = p )
    acfE = apply ( cbind ( 1 : 5 ) , 
                   1 , 
                   function ( x ) ACF ( p = p , k = x ) )
    acfO = abs ( as.numeric ( acf ( y , 
                                    plot = FALSE ) $acf [ 2 : 6 ] ) ) 
    c1 = abs ( acfE - acfO ) <= 0.01  #0.05
    c2 = abs ( mean ( y ) - mean ( x ) * ( 1 - p [ 3 ] * ( 1 - p [ 4 ] -  2 * p [ 5 ] ) ) ) <= 0.5
    c3 = var ( y ) - ( mean ( x ) * ( 1 - p [ 3 ] * ( 1 - p [ 4 ] - 4 * p [ 5 ] ) ) + 
                         mean ( x ) ^ 2 * p [ 3 ] * ( 1 - p [ 3 ] ) * ( 1 - p [ 4 ] - 2 * p [ 5 ] ) ^ 2 ) <= 0.5 
    y = control ( y , c1 , c2 , c3 )
    t.max = th * max ( y )
    x.aux = 0 : t.max
    y.len = length ( y ) 
    x.len = length ( x.aux )
    resNLM = tryCatch ( nlm ( LF , 
                              p = p , 
                              hessian = TRUE , 
                              print.level = 2 , 
                              y = y , 
                              x.aux = x.aux , 
                              y.len = y.len , 
                              x.len = x.len ) ,
                        error = function ( error_message ) { 
                          return ( NA )
                          } )
    if ( sum ( is.na ( resNLM ) ) != 0 ) {
      cr1 = NA 
      cr2 = NA 
    } else {
      cr1 = sum ( is.nan ( resNLM$estimate - qnorm ( conf ) * sqrt ( diag ( solve ( resNLM$hessian ) ) ) ) ) 
      cr2 = sum ( is.nan ( resNLM$estimate + qnorm ( conf ) * sqrt ( diag ( solve ( resNLM$hessian ) ) ) ) ) }
  }
  
  return ( list ( n = n , 
                  p = p , 
                  y = y , 
                  resNLM = resNLM ) ) 
  }

# Overreporting (4 scenarios)
# Case 1: w=0.7 and moderate overreporting (phi1=0.2; phi2=0.7) 
result <- foreach(k=1:20, .combine=rbind) %dopar% {
    sink("log.txt", append=TRUE)
    cat(paste("Starting iteration ", k,"\n"))
    try(expSimuML( n = 200 ,
             p = c ( 0.5 , 3 , 0.7 , 0.2 , 0.7 ) ,
             th = 3 ,
             conf = 0.95 ))
}
save(list="result", file="Results/Sim/case1_OVER.RData")

