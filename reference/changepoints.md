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
#>    x2    x3    x9   x10   x11   x14   x16   x18   x22   x27   x28   x29   x30 
#>     2     3     9    10    11    14    16    18    22    27    28    29    30 
#>   x31   x32   x36   x37   x38   x39   x40   x42   x43   x44   x45   x47   x51 
#>    31    32    36    37    38    39    40    42    43    44    45    47    51 
#>   x53   x55   x60   x61   x62   x63   x64   x67   x69   x71   x72   x74   x75 
#>    53    55    60    61    62    63    64    67    69    71    72    74    75 
#>   x76   x77   x81   x83   x86   x87   x90   x91   x92   x93   x94   x95   x97 
#>    76    77    81    83    86    87    90    91    92    93    94    95    97 
#>   x98   x99  x100  x101  x105  x106  x107  x110  x114  x115  x116  x118  x126 
#>    98    99   100   101   105   106   107   110   114   115   116   118   126 
#>  x127  x128  x129  x130  x131  x132  x134  x135  x137  x139  x141  x142  x145 
#>   127   128   129   130   131   132   134   135   137   139   141   142   145 
#>  x146  x147  x150  x152  x154  x157  x158  x160  x161  x162  x165  x166  x167 
#>   146   147   150   152   154   157   158   160   161   162   165   166   167 
#>  x168  x172  x173  x175  x176  x177  x180  x182  x183  x189  x190  x191  x192 
#>   168   172   173   175   176   177   180   182   183   189   190   191   192 
#>  x199  x200  x202  x203  x204  x205  x206  x210  x213  x214  x217  x219  x222 
#>   199   200   202   203   204   205   206   210   213   214   217   219   222 
#>  x224  x225  x230  x232  x237  x239  x241  x243  x249  x254  x255  x259  x260 
#>   224   225   230   232   237   239   241   243   249   254   255   259   260 
#>  x261  x262  x265  x266  x267  x269  x270  x271  x272  x275  x277  x278  x279 
#>   261   262   265   266   267   269   270   271   272   275   277   278   279 
#>  x280  x281  x282  x283  x292  x295  x297  x298  x299  x301  x302  x303  x305 
#>   280   281   282   283   292   295   297   298   299   301   302   303   305 
#>  x307  x311  x312  x313  x314  x315  x316  x318  x319  x321  x328  x329  x333 
#>   307   311   312   313   314   315   316   318   319   321   328   329   333 
#>  x335  x336  x337  x338  x340  x342  x345  x347  x348  x353  x356  x358  x359 
#>   335   336   337   338   340   342   345   347   348   353   356   358   359 
#>  x361  x365  x366  x367  x374  x375  x379  x380  x381  x382  x383  x387  x388 
#>   361   365   366   367   374   375   379   380   381   382   383   387   388 
#>  x391  x394  x397  x399  x400  x406  x412  x414  x416  x420  x421  x423  x424 
#>   391   394   397   399   400   406   412   414   416   420   421   423   424 
#>  x428  x430  x431  x432  x434  x436  x437  x439  x442  x443  x445  x447  x448 
#>   428   430   431   432   434   436   437   439   442   443   445   447   448 
#>  x451  x452  x454  x458  x459  x461  x464  x465  x466  x469  x476  x478  x480 
#>   451   452   454   458   459   461   464   465   466   469   476   478   480 
#>  x484  x485  x487  x489  x493  x494  x496  x497  x498  x500  x501  x504  x510 
#>   484   485   487   489   493   494   496   497   498   500   501   504   510 
#>  x512  x513  x514  x516  x524  x526  x527  x528  x531  x532  x533  x534  x535 
#>   512   513   514   516   524   526   527   528   531   532   533   534   535 
#>  x537  x538  x539  x540  x541  x543  x544  x546  x548  x549  x554  x555  x556 
#>   537   538   539   540   541   543   544   546   548   549   554   555   556 
#>  x558  x559  x563  x564  x565  x567  x571  x572  x573  x580  x582  x586  x587 
#>   558   559   563   564   565   567   571   572   573   580   582   586   587 
#>  x588  x592  x593  x595  x596  x597  x598  x599  x602  x603  x604  x605  x606 
#>   588   592   593   595   596   597   598   599   602   603   604   605   606 
#>  x610  x611  x615  x621  x622  x623  x626  x630  x631  x642  x644  x649  x655 
#>   610   611   615   621   622   623   626   630   631   642   644   649   655 
#>  x656  x657  x659  x660  x661  x663  x666  x667  x672  x673  x676  x677  x679 
#>   656   657   659   660   661   663   666   667   672   673   676   677   679 
#>  x680  x681  x683  x684  x685  x687  x692  x693  x694  x697  x699  x700  x701 
#>   680   681   683   684   685   687   692   693   694   697   699   700   701 
#>  x702  x703  x709  x710  x711  x713  x717  x719  x721  x722  x726  x727  x728 
#>   702   703   709   710   711   713   717   719   721   722   726   727   728 
#>  x731  x732  x733  x734  x738  x739  x741  x742  x743  x746  x748  x749  x750 
#>   731   732   733   734   738   739   741   742   743   746   748   749   750 
#>  x754  x759  x761  x762  x764  x766  x767  x770  x774  x778  x779  x780  x782 
#>   754   759   761   762   764   766   767   770   774   778   779   780   782 
#>  x783  x784  x785  x787  x789  x792  x793  x794  x801  x802  x807  x810  x814 
#>   783   784   785   787   789   792   793   794   801   802   807   810   814 
#>  x816  x819  x824  x827  x829  x832  x834  x838  x839  x841  x844  x846  x848 
#>   816   819   824   827   829   832   834   838   839   841   844   846   848 
#>  x853  x855  x856  x857  x859  x860  x861  x869  x871  x873  x874  x875  x876 
#>   853   855   856   857   859   860   861   869   871   873   874   875   876 
#>  x877  x878  x879  x883  x884  x887  x889  x890  x898  x899  x900  x903  x905 
#>   877   878   879   883   884   887   889   890   898   899   900   903   905 
#>  x908  x909  x911  x913  x914  x917  x918  x919  x924  x928  x929  x936  x938 
#>   908   909   911   913   914   917   918   919   924   928   929   936   938 
#>  x939  x940  x942  x944  x945  x947  x949  x953  x954  x956  x957  x959  x960 
#>   939   940   942   944   945   947   949   953   954   956   957   959   960 
#>  x966  x970  x972  x973  x974  x975  x976  x977  x981  x985  x986  x987  x988 
#>   966   970   972   973   974   975   976   977   981   985   986   987   988 
#>  x989  x990  x991  x992  x995  x996 x1002 x1006 x1007 x1009 x1010 x1011 x1012 
#>   989   990   991   992   995   996  1002  1006  1007  1009  1010  1011  1012 
#> x1014 x1015 x1016 x1017 x1018 x1020 x1022 x1023 x1025 x1027 x1029 x1032 x1035 
#>  1014  1015  1016  1017  1018  1020  1022  1023  1025  1027  1029  1032  1035 
#> x1041 x1042 x1046 x1048 x1049 x1052 x1054 x1055 x1056 x1057 x1059 x1061 x1063 
#>  1041  1042  1046  1048  1049  1052  1054  1055  1056  1057  1059  1061  1063 
#> x1064 x1066 x1067 x1068 x1069 x1070 x1071 x1073 x1074 x1076 x1077 x1080 x1081 
#>  1064  1066  1067  1068  1069  1070  1071  1073  1074  1076  1077  1080  1081 
#> x1082 x1086 x1088 x1090 x1092 x1094 x1095 
#>  1082  1086  1088  1090  1092  1094  1095 

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
