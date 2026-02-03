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
#>  [1,]    5    1  589  663  836  947 1019 1096    0     0     0     0     0
#>  [2,]    5    1  774  953 1013 1020 1050 1096    0     0     0     0     0
#>  [3,]    8    1  659  666  672  996 1004 1008 1049  1085  1096     0     0
#>  [4,]   11    1  663  666  746  784  794  883  946  1023  1036  1064  1081
#>  [5,]    9    1  657  843  888  890  963  991  995  1037  1052  1096     0
#>  [6,]   12    1  488  656  859  903  905  907  918   928   932   955  1046
#>  [7,]    9    1  613  884  954  992 1021 1055 1065  1083  1086  1096     0
#>  [8,]    8    1  604  654  824  852  919  951 1006  1086  1096     0     0
#>  [9,]    8    1  583  629  654  746  893  915  951  1030  1096     0     0
#> [10,]    6    1  688  729  755  941 1006 1026 1096     0     0     0     0
#> [11,]    7    1  561  659  951  984 1007 1046 1059  1096     0     0     0
#> [12,]    9    1  415  680  688  857  861  895  899   930   932  1096     0
#> [13,]   12    1  613  810  914  923  926  994 1020  1037  1038  1050  1056
#> [14,]    9    1  415  562  780  839  840  864 1016  1059  1075  1096     0
#> [15,]    6    1  883  924  984 1008 1051 1079 1096     0     0     0     0
#> [16,]    8    1  613  646  659  864  951 1028 1045  1059  1096     0     0
#> [17,]    8    1  688  725  733  777  839  911 1035  1036  1096     0     0
#> [18,]   11    1  581  687  794  852  894  912  929   934  1024  1025  1086
#> [19,]    9    1  415  681  741  867  891  936  951   974  1015  1096     0
#> [20,]   10    1  557  581  596  609  780  825  914   940  1030  1081  1096
#> [21,]   10    1  415  598  798  837  859  895  896   994  1080  1086  1096
#> [22,]    7    1  656  848  872  892  951  967 1011  1096     0     0     0
#> [23,]   10    1  583  619  655  837  840  951 1007  1041  1069  1074  1096
#> [24,]   10    1  657  668  813  940 1005 1009 1019  1038  1082  1086  1096
#> [25,]    7    1  415  590  727  803  858  894 1085  1096     0     0     0
#> [26,]    8    1  629  649  898  907  914 1002 1064  1080  1096     0     0
#> [27,]   14    1  615  646  680  717  755  765  784   839   947   953   984
#> [28,]    8    1  680  681  889  897 1024 1040 1069  1085  1096     0     0
#> [29,]   10    1  589  663  666  761  854  931  941   986  1044  1081  1096
#> [30,]    8    1  666  677  727  822  853  923 1030  1040  1096     0     0
#> [31,]    9    1  596  646  738  838  867  892 1036  1039  1064  1096     0
#> [32,]    9    1  621  877  899  970 1006 1009 1010  1028  1054  1096     0
#> [33,]   14    1  623  706  748  755  849  910  936   973   981  1010  1013
#> [34,]    6    1  663  666  823  858  943 1038 1096     0     0     0     0
#> [35,]    9    1  415  852  871  887  894  933 1005  1050  1074  1096     0
#> [36,]    8    1  832  885  897  899  914  932  940   978  1096     0     0
#> [37,]    9    1  672  765  791  870  895  914 1008  1045  1059  1096     0
#> [38,]   13    1  609  810  824  834  851  865  891   919   953   977  1012
#> [39,]    9    1  415  598  671  903 1003 1021 1047  1073  1077  1096     0
#> [40,]   11    1  415  768  823  842  846  853  930   934   954   967  1001
#> [41,]   10    1  589  677  728  872  893  966 1023  1030  1055  1081  1096
#> [42,]    9    1  556  583  716  738  897  935 1010  1050  1063  1096     0
#> [43,]    6    1  596  659  681  863 1021 1086 1096     0     0     0     0
#> [44,]    9    1  681  728  872  888  899  912  957  1013  1057  1096     0
#> [45,]   12    1  557  583  590  619  727  728  791   840   893   940  1004
#> [46,]    6    1  556  780  861  951 1029 1086 1096     0     0     0     0
#> [47,]    7    1  415  596  809  837  951 1072 1085  1096     0     0     0
#> [48,]    9    1  646  708  881  907  911 1053 1065  1080  1086  1096     0
#> [49,]    8    1  672  716  803  861  867  985 1015  1030  1096     0     0
#> [50,]   13    1  415  614  618  659  761  919  921   924   945   989  1000
#>       [,14] [,15] [,16] [,17] [,18] [,19] [,20]
#>  [1,]     0     0     0     0     0     0     0
#>  [2,]     0     0     0     0     0     0     0
#>  [3,]     0     0     0     0     0     0     0
#>  [4,]  1096     0     0     0     0     0     0
#>  [5,]     0     0     0     0     0     0     0
#>  [6,]  1086  1096     0     0     0     0     0
#>  [7,]     0     0     0     0     0     0     0
#>  [8,]     0     0     0     0     0     0     0
#>  [9,]     0     0     0     0     0     0     0
#> [10,]     0     0     0     0     0     0     0
#> [11,]     0     0     0     0     0     0     0
#> [12,]     0     0     0     0     0     0     0
#> [13,]  1068  1096     0     0     0     0     0
#> [14,]     0     0     0     0     0     0     0
#> [15,]     0     0     0     0     0     0     0
#> [16,]     0     0     0     0     0     0     0
#> [17,]     0     0     0     0     0     0     0
#> [18,]  1096     0     0     0     0     0     0
#> [19,]     0     0     0     0     0     0     0
#> [20,]     0     0     0     0     0     0     0
#> [21,]     0     0     0     0     0     0     0
#> [22,]     0     0     0     0     0     0     0
#> [23,]     0     0     0     0     0     0     0
#> [24,]     0     0     0     0     0     0     0
#> [25,]     0     0     0     0     0     0     0
#> [26,]     0     0     0     0     0     0     0
#> [27,]  1017  1019  1082  1096     0     0     0
#> [28,]     0     0     0     0     0     0     0
#> [29,]     0     0     0     0     0     0     0
#> [30,]     0     0     0     0     0     0     0
#> [31,]     0     0     0     0     0     0     0
#> [32,]     0     0     0     0     0     0     0
#> [33,]  1040  1053  1055  1096     0     0     0
#> [34,]     0     0     0     0     0     0     0
#> [35,]     0     0     0     0     0     0     0
#> [36,]     0     0     0     0     0     0     0
#> [37,]     0     0     0     0     0     0     0
#> [38,]  1020  1081  1096     0     0     0     0
#> [39,]     0     0     0     0     0     0     0
#> [40,]  1096     0     0     0     0     0     0
#> [41,]     0     0     0     0     0     0     0
#> [42,]     0     0     0     0     0     0     0
#> [43,]     0     0     0     0     0     0     0
#> [44,]     0     0     0     0     0     0     0
#> [45,]  1057  1096     0     0     0     0     0
#> [46,]     0     0     0     0     0     0     0
#> [47,]     0     0     0     0     0     0     0
#> [48,]     0     0     0     0     0     0     0
#> [49,]     0     0     0     0     0     0     0
#> [50,]  1006  1051  1096     0     0     0     0
sim_1_cp_BMDL(exceedances(DataCPSim))
#>  [1]   17    1  271  548  583  629  728  761  763  780  823  853  870  959  979
#> [16] 1055 1063 1071 1077 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_1))
#>  [1]   17    1  294  515  707  754  875  903  918  937  958  965  966 1000 1001
#> [16] 1005 1038 1064 1068 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_2))
#>  [1]   17    1  618  639  662  726  753  760  780  842  857  911  938  982  986
#> [16] 1053 1068 1071 1072 1096
sim_1_cp_BMDL(exceedances(rlnorm_ts_3))
#>  [1]   17    1  558  626  752  778  782  786  841  852  873  886  943 1001 1045
#> [16] 1061 1071 1079 1085 1096
sim_1_cp_BMDL(exceedances(bogota_pm))
#>  [1]   17    1   25   38  193  330  343  393  659  664  697  724  816  907  992
#> [16] 1044 1073 1080 1083 1096

sim_k_cp_BMDL(DataCPSim)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#>  [1,]   17    1  566  605  654  672  746  813  825   844   881   891   931
#>  [2,]   17    1  233  634  678  711  719  791  837   899   964   975  1000
#>  [3,]   17    1  308  680  727  733  785  831  887   928   981   990   997
#>  [4,]   17    1  553  619  817  868  896  961  964   968   973   975   992
#>  [5,]   17    1  589  615  717  725  823  830  846   861   883   884   903
#>  [6,]   17    1   51  488  680  763  768  844  871   876   881   904   933
#>  [7,]   16    1  562  575  615  633  790  813  845   846   881   916   918
#>  [8,]   17    1  663  680  705  728  729  826  850   863   864   870   936
#>  [9,]   13    1  605  717  725  774  860  865  924   935   953  1024  1031
#> [10,]   17    1  765  830  843  861  879  882  888   889   912   919   950
#> [11,]   17    1  226  619  633  671  749  755  794   825   830   862   933
#> [12,]   17    1  354  780  871  877  889  937  946   957   961   971  1014
#> [13,]   17    1  656  713  790  792  798  857  861   881   882   885   943
#> [14,]   17    1  308  677  694  729  823  853  858   891   926   939   947
#> [15,]   17    1  556  604  716  748  753  771  842   866   908   939   940
#> [16,]   17    1  556  677  713  749  843  885  887   893   929   934   972
#> [17,]   17    1  589  604  619  649  659  677  698   854   861   893   930
#> [18,]   17    1  562  571  589  728  798  824  825   831   868   875   885
#> [19,]   15    1   60  354  621  647  698  738  822   824   830   856   915
#> [20,]   17    1  557  561  571  647  678  704  741   882   915   947   951
#> [21,]   10    1  271  645  688  838  845  853  861   886   905  1022  1096
#> [22,]   17    1  221  571  633  656  677  717  719   817   842   891   968
#> [23,]   17    1  655  727  823  832  836  839  860   866   883   933   937
#> [24,]   17    1  488  615  618  619  649  687  698   717   813   848   861
#> [25,]   15    1  598  642  728  785  883  904  907   908   957   983  1019
#> [26,]   17    1  548  677  698  711  784  798  849   851   853   857   915
#> [27,]   16    1  589  634  722  774  830  861  875   926   934   946   974
#> [28,]   17    1  548  566  613  698  722  753  883   901   931   945   966
#> [29,]   17    1  575  614  623  704  729  792  837   880   889   899   946
#> [30,]   17    1  561  562  753  844  849  919  928   939   940   975   976
#> [31,]   17    1   51  671  706  708  713  716  741   843   884   900   903
#> [32,]   17    1  226  619  628  688  790  798  833   865   923   924  1008
#> [33,]   17    1  590  657  672  727  822  830  831   910   950   951   970
#> [34,]   17    1  596  629  678  823  848  868  887   893   898   901  1019
#> [35,]   17    1  571  581  728  803  817  853  911   924   926   929   939
#> [36,]   17    1  655  803  864  877  882  887  928   931   940   949   969
#> [37,]   17    1  633  803  840  860  882  883  944   963   976   990  1005
#> [38,]   17    1  557  566  655  688  749  774  885   916   933   968   979
#> [39,]   17    1  822  830  833  845  854  866  875   881   895   905   912
#> [40,]   17    1   60  233  613  668  677  705  722   733   901   962   972
#> [41,]   17    1  557  621  628  738  774  837  840   852   861   875   889
#> [42,]   17    1  615  656  727  741  845  852  853   880   912   918   937
#> [43,]   17    1  415  694  719  725  830  851  867   877   896   932   986
#> [44,]   17    1  557  619  719  729  733  746  777   809   848   945   981
#> [45,]   17    1  571  596  708  813  830  840  882   888   895   903   969
#> [46,]   17    1  221  308  634  647  677  716  727   755   771   790   833
#> [47,]   17    1  562  668  681  722  833  849  863   877   890   923   933
#> [48,]   17    1  548  656  668  837  851  867  868   875   888   896   968
#> [49,]   17    1  233  753  761  765  846  865  866   935   939   945   984
#> [50,]   17    1  221  633  645  657  681  719  780   817   884   900   934
#>       [,14] [,15] [,16] [,17] [,18] [,19] [,20]
#>  [1,]   954  1018  1030  1076  1078  1082  1096
#>  [2,]  1009  1014  1039  1073  1086  1089  1096
#>  [3,]  1001  1005  1018  1020  1049  1085  1096
#>  [4,]  1014  1061  1062  1066  1067  1077  1096
#>  [5,]   916   944  1054  1059  1061  1066  1096
#>  [6,]   967   975   998  1034  1038  1062  1096
#>  [7,]   931   940   947   994  1029  1096     0
#>  [8,]   940   954   983  1081  1091  1095  1096
#>  [9,]  1064  1077  1096     0     0     0     0
#> [10,]  1033  1052  1053  1057  1064  1070  1096
#> [11,]   941   979   995  1012  1040  1054  1096
#> [12,]  1018  1020  1045  1046  1072  1078  1096
#> [13,]   966   968   972  1004  1061  1083  1096
#> [14,]   995  1032  1037  1057  1077  1085  1096
#> [15,]  1011  1019  1021  1036  1048  1086  1096
#> [16,]   980   985  1009  1022  1044  1071  1096
#> [17,]  1028  1030  1041  1045  1071  1078  1096
#> [18,]   930  1004  1024  1036  1074  1089  1096
#> [19,]   947   960  1069  1085  1096     0     0
#> [20,]   997  1031  1045  1046  1063  1064  1096
#> [21,]     0     0     0     0     0     0     0
#> [22,]   971   985  1017  1019  1081  1086  1096
#> [23,]   943   951   961  1073  1074  1080  1096
#> [24,]   878   947   954   966   969  1041  1096
#> [25,]  1025  1034  1036  1094  1096     0     0
#> [26,]   931   976   986  1011  1022  1074  1096
#> [27,]   983   993  1001  1004  1093  1096     0
#> [28,]   967   979   981  1061  1086  1093  1096
#> [29,]   991  1005  1035  1046  1052  1088  1096
#> [30,]   978   992  1023  1040  1066  1083  1096
#> [31,]   939   966   967   980   991  1010  1096
#> [32,]  1022  1027  1029  1036  1037  1079  1096
#> [33,]   984   989  1022  1030  1052  1077  1096
#> [34,]  1025  1032  1067  1082  1087  1088  1096
#> [35,]   978  1000  1004  1022  1024  1077  1096
#> [36,]   990  1005  1017  1036  1076  1082  1096
#> [37,]  1009  1027  1033  1048  1080  1089  1096
#> [38,]   983  1001  1026  1062  1079  1087  1096
#> [39,]   935   959   990  1027  1059  1065  1096
#> [40,]   983  1003  1013  1016  1029  1078  1096
#> [41,]   918   941   966   967   973  1045  1096
#> [42,]   953   986  1000  1014  1063  1069  1096
#> [43,]   987   992  1017  1066  1085  1092  1096
#> [44,]   985  1006  1053  1068  1071  1084  1096
#> [45,]   984  1010  1019  1041  1070  1082  1096
#> [46,]   871   905   931   949  1011  1088  1096
#> [47,]   936   988  1013  1015  1029  1033  1096
#> [48,]  1012  1022  1031  1079  1093  1094  1096
#> [49,]   995  1020  1023  1038  1060  1061  1096
#> [50,]   976   988   995  1028  1030  1075  1096

chromo <- c(4, 1, 557, 877 , 905, 986, 1096, 0, 0, 0)
chromo2tau(chromo)
#> [1] 557 877 905 986
```
