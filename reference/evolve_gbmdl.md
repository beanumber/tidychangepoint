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
#>  [1,]   11    1  613  668  790  865  887  940  969   995   998  1037  1086
#>  [2,]    8    1  634  719  733  875  880  911  954   982  1096     0     0
#>  [3,]   10    1  727  741  838  841  998 1025 1034  1062  1070  1082  1096
#>  [4,]    8    1  613  657  878  920  940  961 1060  1086  1096     0     0
#>  [5,]   12    1  621  623  677  753  826  839  919   966   970  1026  1036
#>  [6,]    9    1  615  761  810  828  853  888  899  1003  1016  1096     0
#>  [7,]   10    1  583  717  867  939  950  960  963  1003  1063  1074  1096
#>  [8,]   10    1  488  562  589  717  890  951  964   967  1038  1065  1096
#>  [9,]    7    1  704  753  840  875  883 1001 1060  1096     0     0     0
#> [10,]    6    1  628  771  791  839  901 1005 1096     0     0     0     0
#> [11,]   13    1  633  646  668  746  780  790  853   854   897   970   999
#> [12,]    8    1  561  794  868  912  966  978 1066  1084  1096     0     0
#> [13,]    7    1  727  749  892  940 1006 1026 1072  1096     0     0     0
#> [14,]    6    1  633  768  864  904 1068 1086 1096     0     0     0     0
#> [15,]    8    1  488  605  613  727  910  979  981  1050  1096     0     0
#> [16,]    5    1  905  949  978 1001 1069 1096    0     0     0     0     0
#> [17,]    8    1  604  784  798  867  893  911  964   998  1096     0     0
#> [18,]    9    1  488  633  678  704  865  911  968   977  1002  1096     0
#> [19,]    9    1  642  645  681  817  833  845  932  1051  1086  1096     0
#> [20,]   11    1  556  609  698  748  810  883  959   977   995  1000  1068
#> [21,]    6    1  765  822  853  894  905 1086 1096     0     0     0     0
#> [22,]    7    1  834  844  964  966 1047 1063 1086  1096     0     0     0
#> [23,]    7    1  621  656  727  973 1065 1073 1086  1096     0     0     0
#> [24,]    6    1  846  912  972  995 1037 1086 1096     0     0     0     0
#> [25,]    5    1  628  822  895  964 1086 1096    0     0     0     0     0
#> [26,]    9    1  708  771  892  915  941  980  989  1027  1078  1096     0
#> [27,]    7    1  706  725  826  853  959 1020 1076  1096     0     0     0
#> [28,]    7    1  488  711  834  837  907  930 1086  1096     0     0     0
#> [29,]   11    1  415  548  706  729  749  763  988  1004  1006  1027  1038
#> [30,]   10    1  613  634  657  688  896  903  947  1003  1068  1086  1096
#> [31,]   11    1  663  668  725  741  833  838  839   890   908  1027  1086
#> [32,]   12    1  615  634  771  809  893  905  939   953   986   999  1019
#> [33,]    4    1  753  846 1027 1038 1096    0    0     0     0     0     0
#> [34,]    8    1  621  824  825  826  880  964  967  1025  1096     0     0
#> [35,]    9    1  719  733  790  823  853  878  923   980  1086  1096     0
#> [36,]    9    1  415  548  619  628  935  963 1012  1035  1085  1096     0
#> [37,]    8    1  605  609  655  738  859  937 1064  1074  1096     0     0
#> [38,]    7    1  621  678  706  839  845  910 1038  1096     0     0     0
#> [39,]    9    1  613  681  809  828  882  918  938   957   978  1096     0
#> [40,]    8    1  548  618  688  824  838  944 1010  1035  1096     0     0
#> [41,]    9    1  415  729  790  828  840  879 1016  1033  1051  1096     0
#> [42,]    4    1  841  846  921 1019 1096    0    0     0     0     0     0
#> [43,]    8    1  711  790  859  866  889  899  968  1029  1096     0     0
#> [44,]   11    1  415  561  771  774  858  878  888   892  1037  1077  1086
#> [45,]   12    1  415  823  825  839  846  868  951   963  1026  1032  1064
#> [46,]    8    1  711  832  880  972  978  982 1021  1040  1096     0     0
#> [47,]    5    1  619  623  960 1002 1035 1096    0     0     0     0     0
#> [48,]   10    1  415  645  763  879  964  968  977  1038  1074  1086  1096
#> [49,]   12    1  415  609  618  642  657  840  865   896   903   908   934
#> [50,]    9    1  645  852  868  964 1002 1039 1064  1070  1073  1096     0
#>       [,14] [,15] [,16] [,17] [,18] [,19] [,20]
#>  [1,]  1096     0     0     0     0     0     0
#>  [2,]     0     0     0     0     0     0     0
#>  [3,]     0     0     0     0     0     0     0
#>  [4,]     0     0     0     0     0     0     0
#>  [5,]  1039  1096     0     0     0     0     0
#>  [6,]     0     0     0     0     0     0     0
#>  [7,]     0     0     0     0     0     0     0
#>  [8,]     0     0     0     0     0     0     0
#>  [9,]     0     0     0     0     0     0     0
#> [10,]     0     0     0     0     0     0     0
#> [11,]  1033  1086  1096     0     0     0     0
#> [12,]     0     0     0     0     0     0     0
#> [13,]     0     0     0     0     0     0     0
#> [14,]     0     0     0     0     0     0     0
#> [15,]     0     0     0     0     0     0     0
#> [16,]     0     0     0     0     0     0     0
#> [17,]     0     0     0     0     0     0     0
#> [18,]     0     0     0     0     0     0     0
#> [19,]     0     0     0     0     0     0     0
#> [20,]  1096     0     0     0     0     0     0
#> [21,]     0     0     0     0     0     0     0
#> [22,]     0     0     0     0     0     0     0
#> [23,]     0     0     0     0     0     0     0
#> [24,]     0     0     0     0     0     0     0
#> [25,]     0     0     0     0     0     0     0
#> [26,]     0     0     0     0     0     0     0
#> [27,]     0     0     0     0     0     0     0
#> [28,]     0     0     0     0     0     0     0
#> [29,]  1096     0     0     0     0     0     0
#> [30,]     0     0     0     0     0     0     0
#> [31,]  1096     0     0     0     0     0     0
#> [32,]  1057  1096     0     0     0     0     0
#> [33,]     0     0     0     0     0     0     0
#> [34,]     0     0     0     0     0     0     0
#> [35,]     0     0     0     0     0     0     0
#> [36,]     0     0     0     0     0     0     0
#> [37,]     0     0     0     0     0     0     0
#> [38,]     0     0     0     0     0     0     0
#> [39,]     0     0     0     0     0     0     0
#> [40,]     0     0     0     0     0     0     0
#> [41,]     0     0     0     0     0     0     0
#> [42,]     0     0     0     0     0     0     0
#> [43,]     0     0     0     0     0     0     0
#> [44,]  1096     0     0     0     0     0     0
#> [45,]  1066  1096     0     0     0     0     0
#> [46,]     0     0     0     0     0     0     0
#> [47,]     0     0     0     0     0     0     0
#> [48,]     0     0     0     0     0     0     0
#> [49,]  1065  1096     0     0     0     0     0
#> [50,]     0     0     0     0     0     0     0
sim_1_cp_BMDL(exceedances(DataCPSim))
#>  [1]   17    1  308  634  672  755  785  833  841  852  854  861  862  868  904
#> [16]  914  959 1004 1029 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_1))
#>  [1]   17    1   52  198  363  493  832  838  877  879  897  945  946  948  988
#> [16] 1006 1012 1051 1071 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_2))
#>  [1]   17    1  370  379  454  498  521  720  751  806  843  875  944  961  999
#> [16] 1004 1011 1072 1087 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_3))
#>  [1]   17    1  419  526  635  676  755  880  947  950  956  960  965  998 1002
#> [16] 1005 1048 1073 1089 1096
sim_1_cp_BMDL(exceedances(bogota_pm))
#>  [1]   17    1   72  111  115  278  292  360  414  427  462  535  538  583  678
#> [16]  760  901 1030 1076 1096

sim_k_cp_BMDL(DataCPSim)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#>  [1,]   17    1  271  561  583  590  647  706  727   728   893   914   953
#>  [2,]   17    1   60  221  226  561  615  864  867   878   934   936   974
#>  [3,]   17    1  618  621  628  647  708  790  881   886   898   907   957
#>  [4,]   16    1  415  628  649  738  741  856  903   910   915   947   951
#>  [5,]   15    1  271  618  647  755  810  837  871   876   886   973  1011
#>  [6,]   17    1  271  663  748  749  790  809  817   838   840   849   889
#>  [7,]   17    1  663  668  678  853  875  936  947   963   979   985  1000
#>  [8,]   17    1  583  618  642  666  680  687  729   763   822   837   872
#>  [9,]   16    1  226  548  590  680  774  854  900   911   953   955  1029
#> [10,]   17    1  810  834  863  885  897  924  959   979   992  1010  1021
#> [11,]   17    1  415  581  678  733  833  853  885   904   914   932  1010
#> [12,]   17    1  618  746  836  860  878  883  887   895   897   903   915
#> [13,]   17    1  621  705  727  746  810  848  857   933   951   981   984
#> [14,]   17    1  557  672  765  791  865  911  930   941   947   984   999
#> [15,]   16    1  621  677  727  790  813  840  854   887   932   934   964
#> [16,]   17    1  557  581  605  655  777  851  864   867   875   876   914
#> [17,]   17    1   60  677  680  694  729  790  825   891   893   895   943
#> [18,]   17    1  581  697  705  738  741  763  833   839   895   943   947
#> [19,]   17    1  271  596  697  763  803  864  867   868   884   896   915
#> [20,]   17    1   51  221  354  613  717  746  780   828   834   899   923
#> [21,]   17    1  556  687  713  755  765  828  830   867   884   898   900
#> [22,]   17    1  562  657  687  713  722  729  777   864   882   931   935
#> [23,]   17    1  655  733  771  792  832  839  864   895   905   936   939
#> [24,]   17    1  656  668  681  838  887  889  894   967   972   991   995
#> [25,]   17    1   51  761  845  882  890  923  924   936   937   945   973
#> [26,]   17    1  581  706  791  792  842  857  894   900   924   943   964
#> [27,]   17    1  354  609  704  785  848  852  856   898   959   971  1013
#> [28,]   17    1  654  681  711  717  728  729  828   852   867   936   953
#> [29,]   17    1   20  415  581  646  671  698  733   792   837   856   886
#> [30,]   17    1  647  749  840  880  905  935  938   943   955   973   996
#> [31,]   13    1  604  609  645  719  780  839  865   953   993  1020  1046
#> [32,]   17    1  271  681  791  832  836  837  851   883   891   898   941
#> [33,]   17    1  557  645  705  713  741  748  784   792   853   883   912
#> [34,]   14    1  553  575  605  655  717  777  853   888   891   971   996
#> [35,]   17    1  677  748  749  771  843  859  866   891   911   926   969
#> [36,]   17    1  656  704  861  870  883  890  945   978   991   997  1004
#> [37,]   17    1  688  706  834  852  859  871  876   901   916   936   968
#> [38,]   17    1   60  271  596  672  711  836  838   842   868   896   915
#> [39,]   17    1  308  680  687  694  713  838  861   879   898   934   944
#> [40,]   13    1  609  633  646  657  672  862  884   951   954  1003  1029
#> [41,]   17    1  488  596  598  657  671  681  863   901   903   904   939
#> [42,]   16    1  271  645  646  780  823  895  920   954   957   959   976
#> [43,]   17    1  308  615  647  680  688  717  722   755   765   798   838
#> [44,]   17    1  634  680  694  841  842  863  883   928   930   961   964
#> [45,]   17    1  226  575  655  771  810  823  828   836   841   843   848
#> [46,]   17    1   60  614  680  753  780  837  891   894   904   928   961
#> [47,]   11    1  226  354  798  853  858  894  929   933   955  1006  1093
#> [48,]   17    1  615  646  659  694  711  738  824   912   918   935   940
#> [49,]   16    1  583  605  621  717  813  930  970   979  1008  1015  1016
#> [50,]   17    1   60  634  672  704  717  763  813   851   883   907   989
#>       [,14] [,15] [,16] [,17] [,18] [,19] [,20]
#>  [1,]  1023  1030  1040  1044  1085  1091  1096
#>  [2,]   979  1010  1015  1024  1060  1079  1096
#>  [3,]  1003  1012  1026  1054  1064  1079  1096
#>  [4,]  1019  1035  1059  1066  1086  1096     0
#>  [5,]  1020  1040  1051  1066  1096     0     0
#>  [6,]   893   946   986   988  1073  1083  1096
#>  [7,]  1005  1022  1062  1065  1081  1088  1096
#>  [8,]   889   898   914   955   986  1012  1096
#>  [9,]  1035  1063  1077  1091  1095  1096     0
#> [10,]  1023  1056  1064  1075  1086  1093  1096
#> [11,]  1023  1030  1036  1037  1050  1082  1096
#> [12,]   932   939   959  1036  1046  1052  1096
#> [13,]  1009  1013  1040  1059  1069  1078  1096
#> [14,]  1004  1009  1047  1055  1058  1083  1096
#> [15,]   966  1007  1054  1056  1093  1096     0
#> [16,]   918   940   953   978  1020  1095  1096
#> [17,]  1005  1006  1024  1026  1090  1094  1096
#> [18,]   954  1012  1029  1054  1068  1082  1096
#> [19,]   921  1012  1039  1050  1070  1094  1096
#> [20,]   932   970   986  1035  1074  1087  1096
#> [21,]  1010  1043  1049  1058  1064  1086  1096
#> [22,]   936  1033  1036  1048  1049  1057  1096
#> [23,]   944   974   980   992  1020  1049  1096
#> [24,]  1009  1025  1030  1042  1051  1081  1096
#> [25,]  1012  1020  1043  1048  1075  1080  1096
#> [26,]  1003  1017  1030  1044  1054  1058  1096
#> [27,]  1017  1025  1033  1041  1066  1085  1096
#> [28,]   961  1008  1013  1021  1033  1070  1096
#> [29,]   893   915  1007  1049  1064  1072  1096
#> [30,]  1027  1030  1031  1040  1043  1090  1096
#> [31,]  1058  1068  1096     0     0     0     0
#> [32,]   957  1002  1016  1024  1044  1060  1096
#> [33,]   950   985  1035  1050  1086  1090  1096
#> [34,]  1018  1039  1062  1096     0     0     0
#> [35,]   986  1033  1039  1074  1079  1087  1096
#> [36,]  1012  1057  1069  1076  1083  1084  1096
#> [37,]   969   997  1001  1022  1050  1069  1096
#> [38,]   935   978   993  1013  1053  1079  1096
#> [39,]   951   961   973   978  1027  1029  1096
#> [40,]  1090  1094  1096     0     0     0     0
#> [41,]  1004  1020  1047  1048  1068  1078  1096
#> [42,]   987  1032  1058  1072  1076  1096     0
#> [43,]   867   945   985  1016  1082  1094  1096
#> [44,]  1024  1025  1038  1068  1070  1091  1096
#> [45,]   892   894   953   968  1000  1019  1096
#> [46,]   967   971   998  1019  1022  1089  1096
#> [47,]  1096     0     0     0     0     0     0
#> [48,]   951   973   978   988  1001  1030  1096
#> [49,]  1042  1068  1069  1073  1090  1096     0
#> [50,]   998  1036  1048  1060  1081  1090  1096

chromo <- c(4, 1, 557, 877 , 905, 986, 1096, 0, 0, 0)
chromo2tau(chromo)
#> [1] 557 877 905 986
```
