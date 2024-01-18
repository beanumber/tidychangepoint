library(BayesianMDLGA)
theta <- cpt_best_params(lista_AG)
sigma <- theta$beta
alpha <- theta$alpha
tau <- cpt_best(lista_AG)
d <- lista_AG$exceedances

funcion_media_acumulada <- function (i) {
    step(-(d[i]-tau[1]-0.5))*pow(d[i]/sigma[1],alpha[1])+
+ 
    step(d[i]-tau[1]-0.5)*step(-(d[i]-tau[2]-0.5))*(pow(d[i]/sigma[2],alpha[2])+pow(tau[1]/sigma[1],alpha[1])-pow(tau[1]/sigma[2],alpha[2]))+
+ 
    step(d[i]-tau[2]-0.5)*step(-(d[i]-tau[3]-0.5))*(pow(d[i]/sigma[3],alpha[3])+pow(tau[2]/sigma[2],alpha[2])+pow(tau[1]/sigma[1],alpha[1])-pow(tau[1]/sigma[2],alpha[2])-pow(tau[2]/sigma[3],alpha[3]))+
+ 
    step(d[i]-tau[3]-0.5)*step(-(d[i]-tau[4]-0.5))*(pow(d[i]/sigma[4],alpha[4])+pow(tau[3]/sigma[3],alpha[3])+pow(tau[2]/sigma[2],alpha[2])+pow(tau[1]/sigma[1],alpha[1])-pow(tau[1]/sigma[2],alpha[2])-pow(tau[2]/sigma[3],alpha[3])-pow(tau[3]/sigma[4],alpha[4]))+
+ 
    step(d[i]-tau[4]-0.5)*(pow(d[i]/sigma[5],alpha[5])+pow(tau[4]/sigma[4],alpha[4])+pow(tau[3]/sigma[3],alpha[3])+pow(tau[2]/sigma[2],alpha[2])+pow(tau[1]/sigma[1],alpha[1])-pow(tau[1]/sigma[2],alpha[2])-pow(tau[2]/sigma[3],alpha[3])-pow(tau[3]/sigma[4],alpha[4])-pow(tau[4]/sigma[5],alpha[5]))+
0}

m_carlos <- funcion_media_acumulada(1:360)

usethis::use_data(m_carlos, overwrite = TRUE)
