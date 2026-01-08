# Extract changepoints

Retrieve the indices of the changepoints identified by an algorithm or
model.

## Usage

``` r
changepoints(x, ...)

# Default S3 method
changepoints(x, ...)

# S3 method for class 'mod_cpt'
changepoints(x, ...)

# S3 method for class 'seg_basket'
changepoints(x, ...)

# S3 method for class 'seg_cpt'
changepoints(x, ...)

# S3 method for class 'tidycpt'
changepoints(x, use_labels = FALSE, ...)

# S3 method for class 'ga'
changepoints(x, ...)

# S3 method for class 'cpt'
changepoints(x, ...)

# S3 method for class 'cptga'
changepoints(x, ...)

# S3 method for class 'segmented'
changepoints(x, ...)

# S3 method for class 'breakpointsfull'
changepoints(x, ...)

# S3 method for class 'wbs'
changepoints(x, ...)
```

## Arguments

- x:

  A
  [tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md),
  `segmenter`, or
  [mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md)
  object

- ...:

  arguments passed to methods

- use_labels:

  return the time labels for the changepoints instead of the indices.

## Value

a numeric vector of changepoint indices, or, if `use_labels` is `TRUE`,
a `character` of time labels.

## Details

[tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
objects, as well as their `segmenter` and `model` components, implement
`changepoints()` methods.

Note that this function is not to be confused with
[`wbs::changepoints()`](https://rdrr.io/pkg/wbs/man/changepoints.html),
which returns different information.

For the `default` method, `changepoints()` will attempt to return the
`cpt_true` attribute, which is set by
[`test_set()`](https://beanumber.github.io/tidychangepoint/reference/test_set.md).

## See also

[`wbs::changepoints()`](https://rdrr.io/pkg/wbs/man/changepoints.html)

Other tidycpt-generics:
[`as.model()`](https://beanumber.github.io/tidychangepoint/reference/as.model.md),
[`as.segmenter()`](https://beanumber.github.io/tidychangepoint/reference/as.segmenter.md),
[`diagnose()`](https://beanumber.github.io/tidychangepoint/reference/diagnose.md),
[`fitness()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md)

## Examples

``` r
cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
changepoints(cpts$segmenter)
#>    x3    x6   x12   x18   x19   x22   x23   x24   x25   x26   x27   x28   x30 
#>     3     6    12    18    19    22    23    24    25    26    27    28    30 
#>   x31   x37   x38   x39   x41   x46   x47   x48   x50   x53   x56   x57   x58 
#>    31    37    38    39    41    46    47    48    50    53    56    57    58 
#>   x66   x71   x72   x75   x76   x77   x81   x83   x84   x86   x90   x91   x92 
#>    66    71    72    75    76    77    81    83    84    86    90    91    92 
#>   x93   x97  x101  x103  x105  x107  x108  x111  x112  x114  x117  x121  x123 
#>    93    97   101   103   105   107   108   111   112   114   117   121   123 
#>  x124  x132  x133  x135  x137  x140  x141  x144  x145  x153  x157  x160  x162 
#>   124   132   133   135   137   140   141   144   145   153   157   160   162 
#>  x163  x164  x165  x166  x169  x170  x171  x173  x177  x181  x182  x184  x186 
#>   163   164   165   166   169   170   171   173   177   181   182   184   186 
#>  x187  x189  x190  x194  x195  x196  x198  x199  x201  x204  x206  x209  x212 
#>   187   189   190   194   195   196   198   199   201   204   206   209   212 
#>  x213  x215  x218  x224  x229  x230  x235  x239  x241  x242  x246  x252  x253 
#>   213   215   218   224   229   230   235   239   241   242   246   252   253 
#>  x254  x258  x259  x261  x262  x264  x265  x267  x268  x269  x270  x271  x274 
#>   254   258   259   261   262   264   265   267   268   269   270   271   274 
#>  x277  x278  x279  x280  x281  x282  x284  x286  x287  x288  x289  x290  x292 
#>   277   278   279   280   281   282   284   286   287   288   289   290   292 
#>  x293  x294  x299  x301  x304  x311  x313  x314  x315  x317  x318  x320  x325 
#>   293   294   299   301   304   311   313   314   315   317   318   320   325 
#>  x326  x327  x328  x329  x330  x332  x334  x335  x337  x338  x346  x347  x349 
#>   326   327   328   329   330   332   334   335   337   338   346   347   349 
#>  x350  x353  x355  x356  x361  x363  x365  x368  x369  x370  x371  x375  x376 
#>   350   353   355   356   361   363   365   368   369   370   371   375   376 
#>  x381  x382  x384  x385  x387  x388  x389  x390  x393  x394  x396  x397  x398 
#>   381   382   384   385   387   388   389   390   393   394   396   397   398 
#>  x400  x401  x404  x405  x407  x408  x409  x410  x413  x415  x418  x419  x422 
#>   400   401   404   405   407   408   409   410   413   415   418   419   422 
#>  x424  x425  x426  x430  x431  x432  x433  x434  x435  x436  x440  x441  x443 
#>   424   425   426   430   431   432   433   434   435   436   440   441   443 
#>  x444  x445  x446  x447  x449  x450  x451  x452  x453  x454  x457  x458  x460 
#>   444   445   446   447   449   450   451   452   453   454   457   458   460 
#>  x462  x467  x468  x472  x474  x475  x480  x482  x486  x487  x488  x489  x490 
#>   462   467   468   472   474   475   480   482   486   487   488   489   490 
#>  x491  x492  x493  x495  x496  x499  x501  x503  x506  x507  x508  x510  x511 
#>   491   492   493   495   496   499   501   503   506   507   508   510   511 
#>  x513  x514  x520  x525  x526  x529  x531  x532  x534  x535  x537  x539  x540 
#>   513   514   520   525   526   529   531   532   534   535   537   539   540 
#>  x541  x542  x543  x549  x551  x553  x555  x557  x559  x560  x561  x562  x570 
#>   541   542   543   549   551   553   555   557   559   560   561   562   570 
#>  x571  x574  x576  x577  x578  x579  x580  x581  x582  x586  x587  x588  x589 
#>   571   574   576   577   578   579   580   581   582   586   587   588   589 
#>  x591  x592  x593  x594  x595  x598  x604  x608  x610  x611  x616  x617  x621 
#>   591   592   593   594   595   598   604   608   610   611   616   617   621 
#>  x622  x623  x624  x626  x627  x630  x633  x634  x636  x639  x640  x641  x643 
#>   622   623   624   626   627   630   633   634   636   639   640   641   643 
#>  x646  x649  x652  x655  x658  x659  x660  x662  x665  x666  x667  x669  x671 
#>   646   649   652   655   658   659   660   662   665   666   667   669   671 
#>  x672  x673  x674  x675  x676  x678  x680  x682  x686  x687  x688  x690  x691 
#>   672   673   674   675   676   678   680   682   686   687   688   690   691 
#>  x692  x693  x695  x696  x698  x700  x703  x706  x708  x709  x715  x716  x723 
#>   692   693   695   696   698   700   703   706   708   709   715   716   723 
#>  x724  x726  x727  x728  x729  x730  x731  x732  x733  x735  x737  x739  x740 
#>   724   726   727   728   729   730   731   732   733   735   737   739   740 
#>  x741  x742  x744  x746  x747  x748  x749  x753  x754  x755  x756  x757  x758 
#>   741   742   744   746   747   748   749   753   754   755   756   757   758 
#>  x762  x767  x768  x773  x778  x780  x782  x785  x786  x794  x797  x798  x804 
#>   762   767   768   773   778   780   782   785   786   794   797   798   804 
#>  x805  x808  x813  x815  x818  x821  x823  x825  x826  x827  x828  x829  x832 
#>   805   808   813   815   818   821   823   825   826   827   828   829   832 
#>  x833  x834  x835  x836  x837  x838  x839  x840  x842  x843  x846  x847  x849 
#>   833   834   835   836   837   838   839   840   842   843   846   847   849 
#>  x852  x853  x854  x857  x858  x862  x863  x865  x868  x871  x874  x875  x880 
#>   852   853   854   857   858   862   863   865   868   871   874   875   880 
#>  x881  x884  x885  x889  x891  x898  x899  x900  x901  x906  x911  x916  x918 
#>   881   884   885   889   891   898   899   900   901   906   911   916   918 
#>  x919  x920  x921  x923  x924  x929  x931  x933  x934  x936  x938  x941  x942 
#>   919   920   921   923   924   929   931   933   934   936   938   941   942 
#>  x945  x946  x947  x948  x950  x954  x957  x958  x959  x961  x962  x964  x966 
#>   945   946   947   948   950   954   957   958   959   961   962   964   966 
#>  x967  x968  x969  x970  x972  x973  x974  x979  x980  x981  x983  x984  x985 
#>   967   968   969   970   972   973   974   979   980   981   983   984   985 
#>  x989  x990  x992  x994  x995  x996  x997  x999 x1000 x1003 x1004 x1007 x1008 
#>   989   990   992   994   995   996   997   999  1000  1003  1004  1007  1008 
#> x1010 x1012 x1013 x1017 x1018 x1019 x1021 x1022 x1026 x1029 x1030 x1034 x1036 
#>  1010  1012  1013  1017  1018  1019  1021  1022  1026  1029  1030  1034  1036 
#> x1037 x1039 x1040 x1041 x1042 x1043 x1044 x1049 x1056 x1057 x1060 x1061 x1062 
#>  1037  1039  1040  1041  1042  1043  1044  1049  1056  1057  1060  1061  1062 
#> x1063 x1064 x1069 x1071 x1076 x1084 x1086 x1087 x1088 x1089 x1092 x1094 
#>  1063  1064  1069  1071  1076  1084  1086  1087  1088  1089  1092  1094 

# \donttest{
# Segment a times series using a genetic algorithm
cpts <- segment(DataCPSim, method = "cptga")
changepoints(cpts$segmenter)
#> [1] 554 821 973
# }
cpts <- segment(DataCPSim, method = "segmented")
changepoints(cpts$segmenter)
#> [1] 776

cpts <- segment(DataCPSim, method = "strucchange")
changepoints(cpts$segmenter)
#> [1] 547 767 932

cpts <- segment(DataCPSim, method = "wbs")
changepoints(cpts$segmenter)
#>  [1]  547  809  810  822  823  939  952  953  972  976  980  982  997  999 1031
#> [16] 1032 1040 1041 1046 1063 1064 1065 1066 1086
```
