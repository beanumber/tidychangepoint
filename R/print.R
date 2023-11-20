#' print_pow_tau
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'
print_pow_tau <- function(i_tau, i_sig_alp) {
  paste0("pow(tau[", i_tau, "]/sigma[", i_sig_alp, "],alpha[", i_sig_alp, "])")
}

#' print_pow_d
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'

print_pow_d <- function(i_d, i_sig_alp) {
  paste0("pow(d[", i_d, "]/sigma[", i_sig_alp, "],alpha[", i_sig_alp, "])")
}

print_step <- function(i_d, i_tau, extra_menos = F, Do_step2 = F) {
  if (Do_step2) {
    if (!extra_menos) texto <- paste0("step2(d[", i_d, "]-tau[", i_tau, "]-0.5)")
    if (extra_menos) texto <- paste0("step2(-(d[", i_d, "]-tau[", i_tau, "]-0.5))")
  } else {
    if (!extra_menos) texto <- paste0("step(d[", i_d, "]-tau[", i_tau, "]-0.5)")
    if (extra_menos) texto <- paste0("step(-(d[", i_d, "]-tau[", i_tau, "]-0.5))")
  }
  return(texto)
}

