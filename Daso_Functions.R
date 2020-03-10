# Funciones para cálculo de Indicadores Dasométricos


# Cuadratic Mean Diameter in cm (Dg) (1)
Dg.1 <- function(x){
  
 dg <- sqrt(sum(x^2, na.rm=T)/sum(!is.na(x)))
  
 return(dg)
}


# Where BA is stand basal area, n is the number of trees,
# and k is a constant based on measurement units
# for BA in ft2 and Dg in inches, k=0.005454
# for BA in m2 and Dg in cm, k=0.0000785.


# Cuadratic Mean Diameter in cm (Dg) (2)

# BA: Basal Area

Dg.2 <- function(x, BA, k=0.0000785){
  
  dg <- sqrt(BA/(k*sum(!is.na(x))))
  
  return(dg)
}


# Mean Diameter of Dominants Trees in cm (D0)

# Cuadratic Mean Diameter in cm (Dg) (2)
S <- function(x){
  
  dom <- 
  dg <- sqrt(ba/(k*sum(!is.na(x))))
  
  return(dg)
}




# Trees per hectare (N)

# a: Plot area

N <- function(x,a){
  
  n <- length(x)
  
  na <- (n*10000)/a
  
  return(na)
}



# Dominant height in m


Hd <- function(x){
  
  q3 <- quantile(x, probs = 0.75, na.rm = T)
  
  sel <- x[x>=q3 & !is.na(x)]
  
  hd <- mean(sel, na.rm = T)
  
  return(hd)
}




# Dominant height trees selection

# Con el método de agrupación por cuantiles


PS_sel <- function(x){
  
  # q <- quantile(x, probs = c(0,0.25,0.5,0.75,1), na.rm = T)
  q <- unique(quantile(x, probs = c(0,0.5,0.75,1), na.rm = T))
  
  PS <- discretize(x, "fixed", breaks = q, ordered = T,
                    # labels=c("Suprimido", "Intermedio", "Codominante", "Dominante")
                   labels=c("Suprimido", "Codominante", "Dominante")
                   )
  
  return(PS)
}


# Con el método de agrupación por clústers (K-means)

PS_sel_2 <- function(x){
  
  q <- quantile(x, probs = c(0,0.25,0.5,0.75,1), na.rm = T)
  
  PS <- discretize(x, "fixed", breaks = q, ordered = T,
                   labels=c("Suprimido", "Intermedio", "Codominante", "Dominante"))
  
  return(PS)
}



# Count "Suprimidos"

Sup <- function(x){
  
  sup <- sum(!is.na(x[x=="Suprimido"]))
  
  return(sup)
}


# Count "Intermedios"

Int <- function(x){
  
  int <- sum(!is.na(x[x=="Intermedio"]))
  
  return(int)
}



# Count "Codominantes"

Cod <- function(x){
  
  cod <- sum(!is.na(x[x=="Codominante"]))
  
  return(cod)
}




# Count "Dominantes"

Dom <- function(x){
  
  dom <- sum(!is.na(x[x=="Dominante"]))
  
  return(dom)
}




# Codominant height in m


Hcd <- function(x){
  
  q2 <- quantile(x, probs = 0.5, na.rm = T)
  q3 <- quantile(x, probs = 0.75, na.rm = T)
  
  h <- x[x>=q2 & x<=q3 & !is.na(x)]
  
  hcd <- mean(h, na.rm = T)
  
  return(hcd)
}




# Codominant height in m


Hcd_dom <- function(x){
  
  q2 <- quantile(x, probs = 0.5, na.rm = T)
  
  h <- x[x>=q2 & !is.na(x)]
  
  hcd <- mean(h, na.rm = T)
  
  return(hcd)
}



# Dominant diameter in cm


Dd <- function(x){
  
  q3 <- quantile(x, probs = 0.75, na.rm = T)
  
  d <- x[x>=q3 & !is.na(x)]
  
  dd <- mean(d, na.rm = T)
  
  return(dd)
}



# Codominant diameter in cm


Dcd <- function(x){
  
  q2 <- quantile(x, probs = 0.5, na.rm = T)
  q3 <- quantile(x, probs = 0.75, na.rm = T)
  
  d <- x[x>=q2 & x<=q3 & !is.na(x)]
  
  dcd <- mean(d, na.rm = T)
  
  return(dcd)
}



# Basal Area in m² per hectare (G)

# a: Plot area

G <- function(x, a){
  
  g <- sum(((pi/40000)*(x^2)), na.rm = T)
  
  G <- (g*10000)/a
  
  return(G)
}




# Mean Annual Increment (MAI) - Diameter

# x: DataFrame object

MAId <- function(x, t){
  
  mai <- x/t
  
  maid <- mean(mai, na.rm = T)
  
  return(maid)
}





# Mean Annual Increment (MAI) - Height

# x: DataFrame object

MAIh <- function(x, t){
  
  mai <- x/t
  
  maih <- mean(mai, na.rm = T)
  
  return(maih)
}





# Mode function
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
