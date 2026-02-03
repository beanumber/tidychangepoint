# Generate a list of candidate changepoints using a genetic algorithm

Generate a list of candidate changepoints using a genetic algorithm

## Usage

``` r
evolve_gbmdl(x, mat_cp, these_bmdls)

junta_1_puntos_cambio(padres, mat_cp)

junta_k_puntos_cambio(mat_padres, mat_cp)

mata_1_tau_volado(cp, prob_volado = 0.5)

mata_k_tau_volado(mat_cp)

muta_1_cp_BMDL(
  cp,
  x,
  probs_nuevos_muta0N = c(0.8, 0.1, 0.1),
  dist_extremos = 10,
  min_num_cpts = 1,
  mutation_possibilities = c(-1, 0, 1),
  mutation_probs = c(0.3, 0.4, 0.3),
  max_num_cp = 20
)

muta_k_cp_BMDL(mat_cp, x)

sim_1_cp_BMDL(x, max_num_cp = 20, prob_inicial = 0.06)

sim_k_cp_BMDL(x, generation_size = 50, max_num_cp = 20)

probs_vec_MDL(vec_MDL, probs_rank0_MDL1 = 0)

selec_k_pares_de_padres(vec_probs)

chromo2tau(chromo)

mat_cp_2_list(mat_cp)
```

## Arguments

- x:

  A time series object

- mat_cp:

  A matrix of potential changepoints

- these_bmdls:

  vector of
  [`BMDL()`](https://beanumber.github.io/tidychangepoint/reference/BMDL.md)
  scores

- padres:

  vector de longitud dos con índice de papa e índice de mama

- mat_padres:

  matriz de kx2 la cual contiene en sus renglones las parejas de padres

- cp:

  vector cromosoma que se va a poner a prueba

- prob_volado:

  probabilidad de quitar un tiempo de cambio existente utilizado por
  mata_k_tau_volado para quitar elementos de más. Se recomienda dejar el
  valor de 0.5 ya que así al juntar los pc del padre y madre se
  eliminará la mitad de estos

- probs_nuevos_muta0N:

  probabilidades de mutar 0,1,2,...,l hasta cierto numero l; eg si vale
  c(.5,.2,.2,.1) se tiene una probabilidad 0.5 de mutar 0 (de no mutar),
  probabilidad 0.2 de mutar 1,, probabilidad 0.2 de mutar 2, y,
  probabilidad 0.1 de mutar 3.

- dist_extremos:

  distancia entre el primer los puntos de cambio v_0 y v_1 al igual que
  entre v_m y v\_{m+1}; distancia minima que debe de haber de un punto
  de cambio y los valores 1 y T, donde T es la longitud total de la
  serie

- min_num_cpts:

  es la cota inferior del número de puntos de cambio que puede tener un
  cromosoma

- mutation_possibilities:

  vector con mutaciones posibles; eg si mutaciones=c(-1,0,1) entonces un
  punto de cambio puede ser movido una unidad a la izquierda, puede
  quedarse igual, o moverse una unidad a la derecha

- mutation_probs:

  probabilidades de mutación. Las longitudes de este vector y mutaciones
  tienen que ser iguales; eg si mutaciones=c(-1,0,1) y probs_muta =
  c(.2, .6, .2) entonces se tiene una probabilidad .2 de que el punto de
  cambio se desplace a la izquierda, probabilidad .6 de quedar igual, y
  probabilidad . 2 de ser movido a la derecha

- max_num_cp:

  el máximo número de rebases. Este parámetro se ocupa en particular
  para que todos los cromosomas quepan en una matriz.

- prob_inicial:

  probabilidad de que en la primera generación un punto cualquiera sea
  punto de cambio. Se recomienda =.5 ya que con esto se distribuyen
  relativamente uniformes los puntos de cambio

- generation_size:

  tamaño de las generaciones

- vec_MDL:

  vector con valores MDL

  OBSERVACIÓN: Esto regresa numeros negativos, los cuales mientras más
  negativo mejor, ya que dará que es un mejor vector de tiempos de
  cambio. Es decir, un MDL de -6000 es mejor que -4000

- probs_rank0_MDL1:

  para medir obtener la probabilidad de los padres se pueden tomar o las
  probabilidades con respecto a los rangos (como en el artículo) o se
  pueden tomar las probabilidades con respecto a el MDL. La diferencia
  radica en que si se toma con respecto al MDL se tendrá que un
  cromosoma con un gran MDL este tendrá una gran ventaja de ocurrir, en
  cambio cuando solo se tiene rank esta ventaja gran ventaja se reduce

- vec_probs:

  vector de probabilidades de selección de cada uno de los cromosomas

- chromo:

  Chromosome, from a row of the matrix `mat_cp`

## Value

regresa una matriz de las mismas dimensiones que mat_cp, pero con los
nuevos cromosomas

el mismo cromosoma sin algunos de sus puntos de cambio

regresa una matriz a la cual se le quitaron a sus cromosomas algunos
puntos de cambio

regresa un vector mutado

regreas una mat_cp mutada

regresa una matriz de `k` por `max_num_cp+3`, la cual en cada renglón
tiene una simulación de un vector de tiempos de cambio

regresa un vector de probabilidades

## Details

regresa un vector de tamaño `max_num_cp+3` donde la primera entrada es
m, la segunda \\v_0=1, ...., v\_{m+1}=N,0,...,0\\

por ejemplo: `c(4,1,3,8,11,15,20,0,0,0,0)` para `m=4`,
\\max\\num\\cp=8\\, \\N=20\\. Se tienen `m` puntos de cambio, los cuales
\\\tau_0=1\\ y \\\tau\_{m+1}= N+1\\, pero en nuestro caso tenemos que
los vectores `cp` tienen \\c(m,\tau_0=1,\tau_1,...,\tau\_{m-1},\tau_m=
N,0,0,0)\\ por lo cual se nosotros:

- empieza con el número de puntos de cambio;

- la segunda entrada es un uno;

- la tercera entrada es el primer punto de cambio;

- las siguientes son otros puntos de cambio;

- la siguiente entrada después de punto de cambio tiene el valor `N`; y

- los siguientes son númores cero hasta llenarlo para que sea de tamaño
  `max_num_cp`

## Examples

``` r
mat_cp <- sim_k_cp_BMDL(DataCPSim)
bmdls <- mat_cp |> 
  mat_cp_2_list() |> 
  evaluate_cpts(.data = as.ts(DataCPSim), model_fn = fit_nhpp) |> 
  dplyr::pull(BMDL)
evolve_gbmdl(exceedances(DataCPSim), mat_cp, bmdls)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#>  [1,]    6    1  556  706  885  911 1084 1086 1096     0     0     0     0
#>  [2,]    7    1  605  609  727  901  914  941 1003  1096     0     0     0
#>  [3,]    9    1  613  780  838  840  883  941 1016  1045  1057  1096     0
#>  [4,]    6    1  809  837  839  940 1044 1070 1096     0     0     0     0
#>  [5,]    9    1  581  656  824  830  842  891  920   943   999  1096     0
#>  [6,]    7    1  561  791  824  843  951 1025 1063  1096     0     0     0
#>  [7,]   10    1  698  798  846  866  954  998 1019  1029  1069  1086  1096
#>  [8,]    9    1  553  654  826  863  884  894  923   968  1084  1096     0
#>  [9,]   10    1  609  681  953  971  978 1014 1042  1068  1072  1078  1096
#> [10,]    8    1  557  672  733  741  791  891  931  1005  1096     0     0
#> [11,]    9    1  642  645  755  886  978  981 1005  1045  1070  1096     0
#> [12,]    7    1  557  619  642  984 1015 1016 1043  1096     0     0     0
#> [13,]   13    1  717  831  853  878  920  931  944   978  1043  1058  1071
#> [14,]    8    1  668  864  886  900 1007 1064 1071  1086  1096     0     0
#> [15,]    9    1  615  646  825  912  939  964 1014  1029  1069  1096     0
#> [16,]   12    1  706  763  791  823  843  935  944   963   983  1003  1029
#> [17,]    8    1  634  852  875  967  993 1015 1077  1086  1096     0     0
#> [18,]    7    1  605  791  898  970  977  980 1040  1096     0     0     0
#> [19,]   11    1  613  659  668  711  903  950  951   957   985  1003  1024
#> [20,]   14    1  613  621  645  655  672  681  717   790   817   971  1015
#> [21,]    6    1  924  954  992  995 1048 1053 1096     0     0     0     0
#> [22,]    8    1  575  706  832  866  896  944 1029  1072  1096     0     0
#> [23,]    9    1  548  605  794  848  867  897 1044  1054  1058  1096     0
#> [24,]    9    1  609  633  886  897  919  954  979  1073  1086  1096     0
#> [25,]   11    1  598  671  697  798  866  895  903   907   915   919  1001
#> [26,]    9    1  656  688  733  863  935  947  960  1011  1026  1096     0
#> [27,]   11    1  561  623  716  768  794  831  836   844   861   933   951
#> [28,]   11    1  705  716  817  838  862  883  889   982   996  1060  1073
#> [29,]    9    1  415  746  784  884  903 1024 1035  1044  1083  1096     0
#> [30,]    7    1  561  920  964 1001 1046 1064 1075  1096     0     0     0
#> [31,]   11    1  553  575  581  654  753  841  853   893   954   968   978
#> [32,]    8    1  581  604  605  697  901  938 1025  1072  1096     0     0
#> [33,]    8    1  605  794  889  894  964  970 1051  1086  1096     0     0
#> [34,]    6    1  791  856  861  877  904 1048 1096     0     0     0     0
#> [35,]   10    1  647  672  738  879  885  897  914   954  1048  1071  1096
#> [36,]   10    1  590  646  886  915  935  974 1000  1008  1049  1051  1096
#> [37,]   11    1  548  848  859  986  996 1017 1018  1025  1028  1057  1085
#> [38,]    7    1  681  716  792  836  951 1001 1084  1096     0     0     0
#> [39,]    7    1  621  792  911  964 1000 1010 1068  1096     0     0     0
#> [40,]   12    1  415  615  716  837  867  884  932   959  1044  1058  1063
#> [41,]   14    1  596  694  738  837  838  843  895   915   948   955  1030
#> [42,]    8    1  681  883  905  977  987 1023 1033  1037  1096     0     0
#> [43,]    7    1  415  557  911  970 1012 1054 1079  1096     0     0     0
#> [44,]    6    1  557  763  863 1011 1043 1086 1096     0     0     0     0
#> [45,]    9    1  657  741  780  830  875  935 1013  1049  1077  1096     0
#> [46,]    6    1  605  719  727  891 1085 1086 1096     0     0     0     0
#> [47,]    9    1  556  646  681  748  904  920  987  1025  1032  1096     0
#> [48,]    3    1  867  868  915 1096    0    0    0     0     0     0     0
#> [49,]    6    1  415  763  823  826  886 1013 1096     0     0     0     0
#> [50,]   11    1  590  755  836  885  971  980  993  1011  1022  1028  1086
#>       [,14] [,15] [,16] [,17] [,18] [,19] [,20]
#>  [1,]     0     0     0     0     0     0     0
#>  [2,]     0     0     0     0     0     0     0
#>  [3,]     0     0     0     0     0     0     0
#>  [4,]     0     0     0     0     0     0     0
#>  [5,]     0     0     0     0     0     0     0
#>  [6,]     0     0     0     0     0     0     0
#>  [7,]     0     0     0     0     0     0     0
#>  [8,]     0     0     0     0     0     0     0
#>  [9,]     0     0     0     0     0     0     0
#> [10,]     0     0     0     0     0     0     0
#> [11,]     0     0     0     0     0     0     0
#> [12,]     0     0     0     0     0     0     0
#> [13,]  1073  1086  1096     0     0     0     0
#> [14,]     0     0     0     0     0     0     0
#> [15,]     0     0     0     0     0     0     0
#> [16,]  1058  1096     0     0     0     0     0
#> [17,]     0     0     0     0     0     0     0
#> [18,]     0     0     0     0     0     0     0
#> [19,]  1096     0     0     0     0     0     0
#> [20,]  1016  1041  1086  1096     0     0     0
#> [21,]     0     0     0     0     0     0     0
#> [22,]     0     0     0     0     0     0     0
#> [23,]     0     0     0     0     0     0     0
#> [24,]     0     0     0     0     0     0     0
#> [25,]  1096     0     0     0     0     0     0
#> [26,]     0     0     0     0     0     0     0
#> [27,]  1096     0     0     0     0     0     0
#> [28,]  1096     0     0     0     0     0     0
#> [29,]     0     0     0     0     0     0     0
#> [30,]     0     0     0     0     0     0     0
#> [31,]  1096     0     0     0     0     0     0
#> [32,]     0     0     0     0     0     0     0
#> [33,]     0     0     0     0     0     0     0
#> [34,]     0     0     0     0     0     0     0
#> [35,]     0     0     0     0     0     0     0
#> [36,]     0     0     0     0     0     0     0
#> [37,]  1096     0     0     0     0     0     0
#> [38,]     0     0     0     0     0     0     0
#> [39,]     0     0     0     0     0     0     0
#> [40,]  1073  1096     0     0     0     0     0
#> [41,]  1055  1069  1086  1096     0     0     0
#> [42,]     0     0     0     0     0     0     0
#> [43,]     0     0     0     0     0     0     0
#> [44,]     0     0     0     0     0     0     0
#> [45,]     0     0     0     0     0     0     0
#> [46,]     0     0     0     0     0     0     0
#> [47,]     0     0     0     0     0     0     0
#> [48,]     0     0     0     0     0     0     0
#> [49,]     0     0     0     0     0     0     0
#> [50,]  1096     0     0     0     0     0     0
sim_1_cp_BMDL(exceedances(DataCPSim))
#>  [1]   17    1  589  615  717  817  823  830  861  883  884  903  916  944  975
#> [16]  999 1054 1061 1067 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_1))
#>  [1]   17    1   40  260  329  393  403  515  526  647  806  857  883  925  955
#> [16]  957  962 1010 1030 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_2))
#>  [1]   17    1  152  406  545  737  745  772  783  830  910  937  966  981 1024
#> [16] 1074 1076 1077 1089 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_3))
#>  [1]   17    1  419  590  612  783  808  839  840  857  876  896  914  925  936
#> [16]  944  979  990 1025 1096
sim_1_cp_BMDL(exceedances(bogota_pm))
#>  [1]   17    1   81   94  138  213  249  361  362  393  583  632  655  789  790
#> [16] 1010 1050 1085 1087 1096

sim_k_cp_BMDL(DataCPSim)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#>  [1,]   17    1  619  765  828  830  861  879  882   888   889   919   941
#>  [2,]   17    1  226  633  671  749  755  794  825   830   862   933   937
#>  [3,]   17    1  354  713  780  790  871  877  882   889   946   957   961
#>  [4,]   17    1  308  656  792  798  823  853  857   861   881   885   947
#>  [5,]   17    1  604  677  694  729  753  771  858   891   926   939   995
#>  [6,]   17    1  556  716  842  843  866  885  908   934   940   972  1011
#>  [7,]   17    1  589  659  677  698  713  749  860   887   893   929   930
#>  [8,]   17    1  571  604  619  649  677  831  854   861   868   893  1028
#>  [9,]   17    1  562  589  621  647  728  738  824   825   875   885   915
#> [10,]   17    1   60  354  557  561  656  698  704   822   830   856   947
#> [11,]   17    1  271  571  647  678  741  838  844   845   853   882   905
#> [12,]   17    1  221  645  656  677  688  717  817   861   886   968   971
#> [13,]   17    1  571  633  655  719  727  823  836   839   860   866   891
#> [14,]   17    1  488  605  615  618  619  698  717   848   883   947   951
#> [15,]   17    1  598  649  687  728  813  817  861   878   883   904   908
#> [16,]   17    1  548  642  698  711  784  785  798   851   853   907   931
#> [17,]   17    1  634  677  722  830  849  857  861   875   915   926   934
#> [18,]   17    1  548  589  613  698  722  753  858   883   931   967   974
#> [19,]   17    1  308  566  575  704  729  792  837   880   889   899   945
#> [20,]   12    1  623  919  939  946  975  976  978  1040  1046  1083  1091
#> [21,]   14    1  561  562  706  753  824  843  844   849   903   928   939
#> [22,]   17    1   51  671  713  716  741  831  884   900   923   966   967
#> [23,]   17    1  226  619  628  657  672  688  755   790   798   831   865
#> [24,]   16    1  590  727  822  830  898  910  951   970   984   989  1022
#> [25,]   17    1  596  629  678  823  848  868  887   893   939  1019  1022
#> [26,]   16    1  571  581  728  817  853  887  911   924   926   929   978
#> [27,]   17    1  633  655  803  860  864  877  882   928   931   940   944
#> [28,]   17    1  557  613  803  840  860  882  883   963   976   979   990
#> [29,]   17    1  566  655  688  749  774  830  833   875   916   933   968
#> [30,]   17    1  233  722  822  845  854  866  881   895   905   912   935
#> [31,]   17    1   60  557  613  668  677  705  722   900   901   972   983
#> [32,]   12    1  621  628  738  774  837  861  875   918   941   966   973
#> [33,]   17    1  615  656  727  844  852  853  912   918   937   953   967
#> [34,]   17    1  415  694  719  725  741  830  845   851   867   877   896
#> [35,]   17    1  557  619  657  719  729  746  777   809   932   945   981
#> [36,]   17    1  571  708  733  830  848  882  888   895   903   969   984
#> [37,]   17    1  221  308  596  634  647  677  727   755   771   790   813
#> [38,]   17    1  562  668  681  716  722  833  849   863   877   890   911
#> [39,]   17    1  548  656  668  837  851  859  867   868   875   888   896
#> [40,]   17    1  233  753  761  765  846  865  866   935   939   945   968
#> [41,]   17    1  221  645  657  681  719  780  817   884   900   934   976
#> [42,]   17    1  571  575  741  885  903  911  933   959   964   967  1002
#> [43,]   17    1  581  596  642  680  729  748  809   836   854   861   894
#> [44,]   17    1  589  598  623  642  663  729  761   790   930  1010  1012
#> [45,]   17    1  596  681  729  828  842  852  854   859   866   924   945
#> [46,]   17    1  596  677  822  861  863  877  905   919   963   964   967
#> [47,]   17    1  226  271  415  618  755  785  831   882   883   924   948
#> [48,]   17    1   20  654  681  687  711  748  763   771   823   892   928
#> [49,]   17    1  575  663  681  837  841  854  859   867   891   932   964
#> [50,]   17    1  571  615  642  706  719  729  780   836   838   852   859
#>       [,14] [,15] [,16] [,17] [,18] [,19] [,20]
#>  [1,]  1033  1052  1053  1057  1064  1070  1096
#>  [2,]   979   995  1012  1018  1040  1054  1096
#>  [3,]   971  1014  1020  1045  1046  1072  1096
#>  [4,]   966   968   972  1004  1061  1087  1096
#>  [5,]  1032  1037  1057  1077  1089  1091  1096
#>  [6,]  1019  1021  1036  1048  1085  1091  1096
#>  [7,]   980   985  1009  1022  1044  1071  1096
#>  [8,]  1030  1041  1045  1071  1074  1078  1096
#>  [9,]   930  1004  1024  1036  1087  1092  1096
#> [10,]   960  1045  1046  1063  1069  1085  1096
#> [11,]   915   947   951   997  1031  1064  1096
#> [12,]   985  1017  1019  1022  1081  1088  1096
#> [13,]   933   937   943   961  1080  1085  1096
#> [14,]   954   966   969  1041  1073  1074  1096
#> [15,]   957   983  1019  1025  1034  1036  1096
#> [16,]   976  1011  1014  1019  1022  1074  1096
#> [17,]   940   946   983  1001  1004  1088  1096
#> [18,]   979   981   993  1061  1086  1089  1096
#> [19,]   966   991  1005  1035  1052  1088  1096
#> [20,]  1092  1096     0     0     0     0     0
#> [21,]   992  1023  1066  1096     0     0     0
#> [22,]   980   991  1008  1010  1022  1037  1096
#> [23,]   924   950  1027  1029  1036  1079  1096
#> [24,]  1025  1030  1052  1077  1092  1096     0
#> [25,]  1032  1042  1067  1085  1090  1091  1096
#> [26,]  1000  1004  1005  1024  1077  1096     0
#> [27,]   949   969   990  1017  1036  1076  1096
#> [28,]  1005  1009  1027  1033  1048  1092  1096
#> [29,]   983  1001  1026  1027  1062  1079  1096
#> [30,]   959   962   990  1059  1065  1090  1096
#> [31,]  1003  1013  1016  1029  1078  1081  1096
#> [32,]  1045  1096     0     0     0     0     0
#> [33,]   986  1000  1014  1063  1069  1094  1096
#> [34,]   992  1017  1066  1082  1089  1093  1096
#> [35,]   985   986  1006  1053  1068  1084  1096
#> [36,]  1010  1019  1041  1070  1071  1082  1096
#> [37,]   833   871   905   931  1011  1086  1096
#> [38,]   923   933   936   988  1015  1033  1096
#> [39,]  1012  1013  1029  1079  1090  1091  1096
#> [40,]   984   995  1031  1038  1060  1061  1096
#> [41,]   988   995  1020  1028  1030  1075  1096
#> [42,]  1023  1026  1027  1028  1070  1095  1096
#> [43,]   932   954   964   998  1008  1048  1096
#> [44,]  1022  1026  1056  1072  1089  1094  1096
#> [45,]   974   982   998  1042  1061  1074  1096
#> [46,]   989   990  1008  1050  1054  1056  1096
#> [47,]   981   984  1000  1013  1043  1079  1096
#> [48,]   940   941   993  1042  1078  1090  1096
#> [49,]   988   997  1001  1037  1069  1073  1096
#> [50,]   949   957   970  1029  1035  1065  1096

chromo <- c(4, 1, 557, 877 , 905, 986, 1096, 0, 0, 0)
chromo2tau(chromo)
#> [1] 557 877 905 986
```
