
rutgers.optimal.cat.blocks.random = function(n.fact=2,n.cat=0, in.frml=NA,size,blocks=1,
                                             r.starts=4,iter=1000,random.blocks=FALSE) {
  # Function to generate d-optimal designs for two-level factor systems
  my.factors = lapply(1:n.fact, function(x) c(-1,1)) # create a list of factors
  names(my.factors) <- letters[1:n.fact] # names factors letters
  #
  # Augment to handle multiple blocks
  n = size*blocks
  random = ifelse(random.blocks & blocks>1,TRUE,FALSE)
  #
  # add categorical factors
  if (n.cat>0) {
    my.cat.factor = sprintf("L%02d",1:n.cat) # create a list of factors
    my.factors = append(my.factors,list(my.cat.factor) )
    names(my.factors)[n.fact+1] <- letters[n.fact+1]
  }
  #
  # add block factor
  if (blocks>0) {
    my.block.factor = sprintf("Block%02d",1:blocks) # create a list of factors
    my.factors = append(my.factors,list(my.block.factor) )
    names(my.factors)[length(my.factors)] <- "Blocks"
  }
  
  #
  my.grid = expand.grid(my.factors) # create a grid of all unique combinations
  
  # create the base model matrix
  this.frml = ifelse(is.na(in.frml), 
                     sprintf("~(%s)^%s", paste(letters[1:(length(my.factors)-1)], collapse = "+"), length(my.factors)), 
                     in.frml)
  if (blocks>1 & !random) {
    this.frml = sprintf("%s+Blocks",this.frml)
  }
  #
  options(contrasts = c("contr.XuWu", "XuWu.poly"))
  #
  Xbase = model.matrix(as.formula(this.frml), my.grid)
  #
  Vinv=diag(1)
  if (random) {
    xdat = cbind(data.frame(y=rnorm(n=NROW(my.grid))),my.grid)
    m.random = lme4::lmer(sprintf("y%s+(1|Blocks)",this.frml),data=xdat)
    Xbase = lme4::getME(m.random,"X") # this is probably not necessary
    V = diag(blocks*size)
    for (k2 in 1:blocks) {
      this.rc = (1:size)+(k2-1)*size
      V[this.rc,this.rc] = V[this.rc,this.rc] + 1
    }
    Vinv = solve(V)
  }
  # 
  
  # generate D-optimal Design
  detFinal = NA
  all.runs = 1:NROW(Xbase)
  final.runs = NA
  
  for (k in 1:r.starts) { # 20 random starts
    temp.runs = sample(all.runs, n, replace=TRUE)
    #
    if (blocks>1) {
      temp.runs = numeric(0)
      for (bi in 1:blocks) {
        bi1 = (NROW(Xbase)/blocks)*(bi-1) + 1
        bi2 = bi1 + (NROW(Xbase)/blocks)-1
        temp.runs = c(temp.runs,sample(all.runs[bi1:bi2],size,replace=T))
      }
    }
    #
    if (is.na(detFinal)) {
      final.runs <- temp.runs
      Xfinal = Xbase[final.runs,]
      if (!random) {
        
        detFinal = det(t(Xfinal) %*% Xfinal)
      } else {
        detFinal = det(t(Xfinal)%*% Vinv %*% Xfinal)
        
      }
    }
    
    for (i in 1:iter) { # 2000 iterations per start
      for (j in 1:n) {
        temp.runs[j] <- sample(all.runs, 1)
        #
        if (blocks>1) {
          bi1 = (NROW(Xbase)/blocks)*(ceiling(j/size)-1) + 1
          bi2 = bi1 + (NROW(Xbase)/blocks)-1
          temp.runs[j]  = sample(all.runs[bi1:bi2],1)
        }
        Xchallenger = Xbase[temp.runs,]
        if (!random) {
          detChallenger = det(t(Xchallenger) %*% Xchallenger)
          
        } else {
          detChallenger = det(t(Xchallenger)%*% Vinv %*% Xchallenger)
          
        }
        
        if (detChallenger > detFinal) {
          final.runs <- temp.runs
          Xfinal = Xbase[final.runs,]
          detFinal = det(t(Xfinal) %*% Xfinal)
          #
          if (random) {
            detFinal = det(t(Xfinal)%*% Vinv %*% Xfinal)
            
          }
          #
          
        }
      }
    }
  }
  # calculate power
  options(contrasts=c("contr.sum", "contr.poly"))
  Xpower = model.matrix(as.formula(this.frml), my.grid)[final.runs,]
  #
  if (random) {
    
    xdat = cbind(data.frame(y=rnorm(n=NROW(my.grid))),my.grid)
    m.random = lme4::lmer(sprintf("y%s+(1|Blocks)",this.frml),data=xdat)
    Xbase = lme4::getME(m.random,"X")
    Xpower = Xbase[final.runs,]
  }
  Beta = matrix(rep(c(-1,1), NCOL(Xpower))[1:NCOL(Xpower)], nrow=NCOL(Xpower), ncol=1)
  xpxinv = solve(t(Xpower) %*% Xpower)
  df = NROW(Xpower) - NCOL(Xpower)
  if (random) {
    xpxinv = solve(t(Xpower)%*% Vinv %*% Xpower)
    df = NROW(Xpower) - NCOL(Xpower)
    df = numeric(NCOL(xpxinv)) + df
    df[1] = blocks-1
  }
  
  
  
  Fcrit = qf(.95, df1=1, df2=df)
  coef.power = tibble(coef=colnames(Xpower), Beta=Beta, 
                      power=pf(Fcrit, df1=1, df2=df, ncp=1/diag(xpxinv), lower.tail = FALSE))
  
  # Calculate A-efficiency
  calculateAefficiency <- function(X) {
    if (ncol(X) == 0) return(NA)
    eigenvalues <- eigen(t(X) %*% X)$values
    a_eff <- length(eigenvalues) / sum(1 / eigenvalues)
    return(a_eff)
  }
  
  # Calculate G-efficiency
  calculateGefficiency <- function(X) {
    if (ncol(X) == 0) return(NA)
    diag_XtX_inv <- diag(solve(t(X) %*% X))
    g_eff <- 1 / max(diag_XtX_inv)
    return(g_eff)
  }
  
  Deff = detFinal^(1/NCOL(Xfinal))/n
  Aeff <- calculateAefficiency(Xfinal)
  Geff <- calculateGefficiency(Xfinal)
  
  res <- list(
    Deff = Deff,
    Aeff = Aeff,
    Geff = Geff,
    power = coef.power,
    X = my.grid[final.runs,],
    n = n,
    blocks = blocks,
    size = size
  )
  return(res)
}

#
# example 1 - Random Blocks
# rutgers.optimal.cat.blocks.random(n.fact=2,n.cat=3,
#                                   in.frml="~(a+b+c)^2",size=8,blocks=2,
#                                   r.starts=10,iter=2000,random.blocks=T)
# # example 2 - fixed blocks
# rutgers.optimal.cat.blocks.random(n.fact=2,n.cat=3, 
#                                   in.frml="~(a+b+c)^2",size=8,blocks=2,
#                                   r.starts=10,iter=2000,random.blocks=F)