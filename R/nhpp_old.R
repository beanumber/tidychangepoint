#' Bloque de log posterior NHPP
#'
#' @param vec_d_i vector de días en los que hubo revases entre el los puntos de
#'   cambio tau1 y tau2
#' @param tau1 valor del primer punto de cambio
#' @param tau2 valor del segundo punto de cambio
#' @param theta vector de parámetros de verosimilitud del NHPP
#' @param vec_dist_a_priori description
#' @param mat_phi description
#' @export
Bloq_LogPost_NHPP <- function(vec_d_i, tau1, tau2, theta, vec_dist_a_priori, mat_phi) {
  Bloq_LogVero_NHPP(vec_d_i, tau1, tau2, theta) +
    Bloq_LogPrio_NHPP(vec_dist_a_priori, theta, mat_phi)
}

#' @rdname Bloq_LogPost_NHPP
#' @export
Bloq_LogPrio_NHPP <- function(vec_dist_a_priori, theta, mat_phi) {
  if (length(vec_dist_a_priori) == 2) {
    if (all(vec_dist_a_priori == c("Gamma", "Gamma"))) {
      return((mat_phi[1, 2] - 1) * log(theta[1]) - mat_phi[1, 1] * theta[1] + # Exp 74 pag 21
               (mat_phi[2, 2] - 1) * log(theta[2]) - mat_phi[2, 1] * theta[2]) # Exp 75 pag 21
    }
  } else if (length(vec_dist_a_priori) == 3) {
    if (all(vec_dist_a_priori == c("Gamma", "Gamma", "Gamma"))) {
      return((mat_phi[1, 2] - 1) * log(theta[1]) - mat_phi[1, 1] * theta[1] +
               (mat_phi[2, 2] - 1) * log(theta[2]) - mat_phi[2, 1] * theta[2] +
               (mat_phi[3, 2] - 1) * log(theta[3]) - mat_phi[3, 1] * theta[3]) # Antes de la exp 76 pag 22
    }
  } else {
    print("No se tiene registrada esa vec_dist_a_priori; Bloq_LogPrio_NHPP")
  }
}

#' @rdname Bloq_LogPost_NHPP
#' @export
Bloq_LogVero_NHPP <- function(vec_d_i, tau1, tau2, theta, nhpp_dist = "W") {
  if (nhpp_dist == "W") {
    return((tau1^theta[1] - tau2^theta[1]) / theta[2]^theta[1] +
             length(vec_d_i) * (log(theta[1]) - theta[1] * log(theta[2])) +
             (theta[1] - 1) * sum(log(vec_d_i)))
  } # Expresion pg 15 entre las expresiones 18 y 19
  if (nhpp_dist == "EW") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sigma <- theta[3]
    sumd <- sum(vec_d_i)
    sumlogd <- sum(log(vec_d_i))
    sumlog1menosed <- sum(log(1 - exp(-(vec_d_i / sigma)^alpha))) # exp 44 pg 18
    sumlog1menosed2 <- sum(log(1 - (1 - exp(-(vec_d_i / sigma)^alpha))^beta)) # exp 46 pg 18
    return(-sigma^(-alpha) * sumd^alpha # exp 42 pg 18
           + (-1 + beta) * sumlog1menosed - sumlog1menosed2 + # exp 44 pg 18
             (-1 + alpha) * sumlogd + difN * log(alpha * beta) - log(1 - (1 - exp(-(tau1 / sigma)^alpha))^beta) + # exp 43_1+exp 42_1-exp 30 pg 16
             log(1 - (1 - exp(-(tau2 / sigma)^alpha))^beta) - (-1 + alpha) * difN * log(sigma)) # exp 46-exp 43_2
  }
  if (nhpp_dist == "GO") {
    return(theta[1] * (exp(-theta[2] * tau2) - exp(-theta[2] * tau1)) + length(vec_d_i) * (log(theta[1]) + log(theta[2])) - theta[2] * sum(vec_d_i)) # exp 54 pg 19
  }
  if (nhpp_dist == "GGO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sigma <- theta[3]
    # sumd <- sum(vec_d_i)
    sumlogd <- sum(log(vec_d_i))
    sumdasigma <- sum(vec_d_i^sigma)
    return(alpha * (exp(-beta * tau2^sigma) - exp(-beta * tau1^sigma)) + (-1 + sigma) * sumlogd + difN * (log(alpha) + log(beta) + log(sigma)) - beta * sumdasigma) # exp 63 pg 20
  }
  if (nhpp_dist == "MO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sumlogalphamasd <- sum(log(alpha + vec_d_i))
    return(difN * log(beta) + beta * (log(alpha + tau1) - log(alpha + tau2)) - sumlogalphamasd) # exp 71 pg 21
  }
}

#' @rdname Bloq_LogPost_NHPP
#' @export
D_Bloq_LogPost_NHPP <- function(vec_d_i, tau1, tau2, theta, vec_dist_a_priori, mat_phi) {
  D_Bloq_LogVero_NHPP(vec_d_i, tau1, tau2, theta) +
    D_Bloq_LogPrio_NHPP(vec_dist_a_priori, theta, mat_phi)
}


#' @rdname Bloq_LogPost_NHPP
#' @param vec_dist_a_priori vector que determina cual es la distribución a
#'   priori de los parametros; por ahora se tiene programado
#'   vec_dist_a_priori=c("Gamma","Gamma") y
#'   vec_dist_a_priori=c("Gamma","Gamma","Gamma")
#' @param theta vector de parámetros de verosimilitud
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori; cada renglón tiene todos los parametros de una
#'   distribución
#'
#' @export
D_Bloq_LogPrio_NHPP <- function(vec_dist_a_priori, theta, mat_phi) {
  if (length(vec_dist_a_priori) == 2) {
    if (all(vec_dist_a_priori == c("Gamma", "Gamma"))) {
      # Parcial con respecto a alfa
      p1 <- (-1 - theta[1] * mat_phi[1, 1] + mat_phi[1, 2]) / theta[1]
      # Parcial con respecto a beta
      p2 <- (-1 - theta[2] * mat_phi[2, 1] + mat_phi[2, 2]) / theta[2]
      return(c(p1, p2))
    }
  } else if (length(vec_dist_a_priori) == 3) {
    if (all(vec_dist_a_priori == c("Gamma", "Gamma", "Gamma"))) {
      # Parcial con respecto a alfa
      p1 <- (-1 - theta[1] * mat_phi[1, 1] + mat_phi[1, 2]) / theta[1]
      # Parcial con respecto a beta
      p2 <- (-1 - theta[2] * mat_phi[2, 1] + mat_phi[2, 2]) / theta[2]
      # Parcial con respecto a beta
      p3 <- (-1 - theta[3] * mat_phi[3, 1] + mat_phi[3, 2]) / theta[3]
      return(c(p1, p2, p3))
    }
  } else {
    print("No se tiene registrada esa vec_dist_a_priori; D_Bloq_LogPrio_NHPP")
  }
}


#' @rdname Bloq_LogPost_NHPP
#' @param nhpp_dist nombre de tasa de NHPP
#' @export
D_Bloq_LogVero_NHPP <- function(vec_d_i, tau1, tau2, theta, nhpp_dist = "W") {
  if (nhpp_dist == "W") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sumlogd <- sum(log(vec_d_i))
    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial de alpha
      p1 <- difN / alpha + sumlogd - difN * log(beta) + beta^(-alpha) * tau2^alpha * (log(beta) - log(tau2))
      # Parcial de beta
      p2 <- alpha * beta^(-1 - alpha) * (-difN * beta^alpha + tau2^alpha)
    } else { # para los otros regímenes (o bloques)
      # Parcial de alpha
      p1 <- difN / alpha + sumlogd + beta^(-alpha) * (-(difN * beta^alpha + tau1^alpha - tau2^alpha) * log(beta) + tau1^alpha * log(tau1) - tau2^alpha * log(tau2))
      # Parcial de beta
      p2 <- -alpha * beta^(-1 - alpha) * (difN * beta^alpha + tau1^alpha - tau2^alpha)
    }
  }
  
  
  
  if (nhpp_dist == "EW") {
    return("Me niego a hacer esta, es muy larga; D_Bloq_LogVero_NHPP")
  }
  if (nhpp_dist == "GO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sumd <- sum(vec_d_i)
    
    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial con respecto de alpha
      p1 <- difN / alpha - exp(-beta * tau1) + exp(-beta * tau2)
      # Parcial con respecto de beta
      p2 <- difN / beta - sumd - alpha * exp(-beta * tau2) * tau2
    } else {
      # Parcial con respecto de alpha
      p1 <- difN / alpha - exp(-beta * tau1) + exp(-beta * tau2)
      # Parcial con respecto de beta
      p2 <- difN / beta - sumd + alpha * exp(-beta * tau1) * tau1 - alpha * exp(-beta * tau2) * tau2
    }
  }
  if (nhpp_dist == "GGO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sigma <- theta[3]
    # sumd <- sum(vec_d_i)
    sumlogd <- sum(log(vec_d_i))
    sumdasigma <- sum(vec_d_i^sigma)
    D_sumdasigma <- sum(log(vec_d_i) * vec_d_i^sigma)
    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial con respecto de alpha
      p1 <- -1 + difN / alpha + exp(-beta * tau2^sigma)
      # Parcial con respecto de beta
      p2 <- difN / beta - alpha * exp(-beta * tau2^sigma) * tau2^sigma - sumdasigma
      # Parcial con respecto de sigma
      p3 <- difN / sigma + sumlogd - alpha * beta * exp(-beta * tau2^sigma) * tau2^sigma * log(tau2) - beta * D_sumdasigma
    } else {
      # Parcial con respecto de alpha
      p1 <- difN / alpha - exp(-beta * tau1^sigma) + exp(-beta * tau2^sigma)
      # Parcial con respecto de beta
      p2 <- difN / beta + alpha * exp(-beta * tau1^sigma) * tau1^sigma - alpha * exp(-beta * tau2^sigma) * tau2^sigma - sumdasigma
      # Parcial con respecto de sigma
      p3 <- difN / sigma + sumlogd + alpha * beta * exp(-beta * tau1^sigma) * tau1^sigma * log(tau1) - alpha * beta * exp(-beta * tau2^sigma) * tau2^sigma * log(tau2) - beta * D_sumdasigma
    }
  }
  if (nhpp_dist == "MO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    D_sumlogalphamasd <- sum(1 / (alpha + vec_d_i))
    # OBS: los resultadso tau1==0 y tau1!=0 son iguales, se podría borrar el if
    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial con respecto de alpha
      p1 <- (beta * tau2) / (alpha^2 + alpha * tau2) - D_sumlogalphamasd
      # Parcial con respecto de beta
      p2 <- difN / beta + log(alpha) - log(alpha + tau2)
    } else {
      # Parcial con respecto de alpha
      p1 <- beta * (1 / (alpha + tau1) - 1 / (alpha + tau2)) - D_sumlogalphamasd
      # Parcial con respecto de beta
      p2 <- difN / beta + log(alpha + tau1) - log(alpha + tau2)
    }
  }
  if (nhpp_dist %in% c("W", "GO", "MO")) {
    return(c(p1, p2))
  } else {
    return(c(p1, p2, p3))
  }
}


#' @rdname Bloq_LogPost_NHPP
#' @param vec_d_i vector de días de un régimen
#' @param tau_left valor del primer punto de cambio
#' @param tau_right valor del segundo punto de cambio
#' @param initial_val_optim initial values for optimization
#' @param vec_dist_a_priori nombres de distribuciones a priori
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori
#' @param mat_low_upp matriz con lugares donde buscar; cada renglon es para un
#'   parámetro del NHPP
#' @param ... arguments passed to [stats::optim()]
#' @return regresa un el resultado de optim
#' @export
#' @examples
#' fit_nhpp_region(exceedances(lista_AG), 0, 575)
#' fit_nhpp_region(exceedances(lista_AG), 0, 575, initial_val_optim = c(1, 10))
#' 
#'
fit_nhpp_region <- function(t, tau_left, tau_right, 
                            initial_val_optim = c(0.1, 0.5), 
                            mat_low_upp = matrix(c(c(1e-4, 1e-8), c(1e+1, 1e+5)), nrow = 2), 
                            vec_dist_a_priori = c("Gamma", "Gamma"), 
                            mat_phi = matrix(c(1, 3, 2, 1.2), ncol = 2), ...) {
  # Definimos las funciones que vamos a utilizar para encontrar el mínimo
  my_fn <- function(theta) {
    -Bloq_LogPost_NHPP(t, tau1 = tau_left, tau2 = tau_right, theta, vec_dist_a_priori, mat_phi)
  }
  my_gn <- function(theta) {
    -D_Bloq_LogPost_NHPP(t, tau1 = tau_left, tau2 = tau_right, theta, vec_dist_a_priori, mat_phi)
  }
  # Calculamos el mínimo
  (val_optimos <- stats::optim(
    initial_val_optim,
    fn = my_fn, 
    gr = my_gn,
    lower = mat_low_upp[, 1], 
    upper = mat_low_upp[, 2],
    method = "L-BFGS-B",
    ... = ...
  ))
  return(val_optimos)
}
