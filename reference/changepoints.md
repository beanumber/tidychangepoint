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

# S3 method for class 'stepmented'
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
#>    x5    x7    x8    x9   x11   x22   x24   x33   x34   x37   x38   x39   x40 
#>     5     7     8     9    11    22    24    33    34    37    38    39    40 
#>   x41   x42   x44   x45   x52   x53   x54   x55   x57   x59   x60   x61   x64 
#>    41    42    44    45    52    53    54    55    57    59    60    61    64 
#>   x66   x67   x72   x73   x76   x77   x78   x79   x81   x83   x84   x85   x86 
#>    66    67    72    73    76    77    78    79    81    83    84    85    86 
#>   x88   x92   x95   x96   x97   x98   x99  x100  x104  x106  x108  x109  x114 
#>    88    92    95    96    97    98    99   100   104   106   108   109   114 
#>  x116  x117  x119  x122  x123  x124  x130  x132  x136  x137  x141  x142  x143 
#>   116   117   119   122   123   124   130   132   136   137   141   142   143 
#>  x144  x145  x146  x148  x149  x152  x153  x154  x158  x163  x167  x168  x169 
#>   144   145   146   148   149   152   153   154   158   163   167   168   169 
#>  x170  x174  x177  x181  x182  x184  x185  x186  x190  x191  x192  x193  x195 
#>   170   174   177   181   182   184   185   186   190   191   192   193   195 
#>  x196  x197  x198  x200  x201  x202  x203  x205  x207  x208  x210  x213  x214 
#>   196   197   198   200   201   202   203   205   207   208   210   213   214 
#>  x215  x217  x219  x223  x224  x225  x227  x231  x233  x234  x235  x236  x237 
#>   215   217   219   223   224   225   227   231   233   234   235   236   237 
#>  x238  x240  x242  x245  x252  x254  x256  x259  x261  x262  x265  x269  x272 
#>   238   240   242   245   252   254   256   259   261   262   265   269   272 
#>  x274  x275  x277  x278  x279  x285  x286  x287  x289  x290  x291  x294  x295 
#>   274   275   277   278   279   285   286   287   289   290   291   294   295 
#>  x297  x302  x303  x305  x310  x311  x313  x317  x318  x321  x322  x323  x327 
#>   297   302   303   305   310   311   313   317   318   321   322   323   327 
#>  x328  x329  x330  x332  x334  x337  x340  x341  x343  x344  x345  x346  x350 
#>   328   329   330   332   334   337   340   341   343   344   345   346   350 
#>  x353  x358  x359  x362  x364  x365  x366  x368  x369  x371  x375  x376  x377 
#>   353   358   359   362   364   365   366   368   369   371   375   376   377 
#>  x378  x384  x386  x389  x390  x392  x394  x395  x397  x398  x402  x403  x406 
#>   378   384   386   389   390   392   394   395   397   398   402   403   406 
#>  x408  x410  x413  x416  x417  x418  x420  x421  x422  x423  x425  x429  x431 
#>   408   410   413   416   417   418   420   421   422   423   425   429   431 
#>  x434  x436  x438  x440  x442  x443  x446  x453  x457  x461  x462  x463  x464 
#>   434   436   438   440   442   443   446   453   457   461   462   463   464 
#>  x466  x468  x469  x472  x478  x479  x481  x482  x485  x486  x488  x494  x498 
#>   466   468   469   472   478   479   481   482   485   486   488   494   498 
#>  x499  x500  x501  x502  x503  x506  x508  x509  x511  x512  x513  x514  x518 
#>   499   500   501   502   503   506   508   509   511   512   513   514   518 
#>  x519  x523  x524  x526  x527  x532  x533  x534  x535  x536  x537  x539  x541 
#>   519   523   524   526   527   532   533   534   535   536   537   539   541 
#>  x542  x543  x544  x545  x548  x552  x554  x559  x566  x570  x572  x573  x574 
#>   542   543   544   545   548   552   554   559   566   570   572   573   574 
#>  x579  x580  x581  x582  x583  x590  x594  x595  x597  x598  x601  x603  x609 
#>   579   580   581   582   583   590   594   595   597   598   601   603   609 
#>  x610  x611  x613  x614  x617  x618  x620  x621  x623  x626  x627  x631  x632 
#>   610   611   613   614   617   618   620   621   623   626   627   631   632 
#>  x635  x636  x637  x638  x639  x640  x641  x642  x643  x644  x645  x647  x648 
#>   635   636   637   638   639   640   641   642   643   644   645   647   648 
#>  x651  x654  x656  x658  x660  x661  x662  x664  x665  x666  x667  x669  x677 
#>   651   654   656   658   660   661   662   664   665   666   667   669   677 
#>  x679  x681  x682  x683  x684  x685  x687  x690  x691  x697  x699  x704  x705 
#>   679   681   682   683   684   685   687   690   691   697   699   704   705 
#>  x706  x707  x709  x710  x714  x721  x725  x726  x727  x730  x733  x739  x740 
#>   706   707   709   710   714   721   725   726   727   730   733   739   740 
#>  x743  x747  x748  x749  x750  x754  x756  x760  x761  x765  x769  x773  x777 
#>   743   747   748   749   750   754   756   760   761   765   769   773   777 
#>  x779  x780  x782  x784  x787  x788  x791  x793  x794  x796  x805  x806  x807 
#>   779   780   782   784   787   788   791   793   794   796   805   806   807 
#>  x808  x809  x810  x811  x813  x817  x818  x822  x823  x828  x830  x831  x832 
#>   808   809   810   811   813   817   818   822   823   828   830   831   832 
#>  x833  x835  x836  x837  x841  x844  x845  x848  x849  x850  x854  x859  x861 
#>   833   835   836   837   841   844   845   848   849   850   854   859   861 
#>  x864  x865  x872  x873  x876  x877  x880  x881  x882  x884  x885  x888  x891 
#>   864   865   872   873   876   877   880   881   882   884   885   888   891 
#>  x892  x893  x897  x898  x901  x904  x905  x907  x910  x911  x912  x913  x918 
#>   892   893   897   898   901   904   905   907   910   911   912   913   918 
#>  x920  x922  x924  x925  x927  x929  x930  x931  x933  x934  x937  x938  x939 
#>   920   922   924   925   927   929   930   931   933   934   937   938   939 
#>  x946  x951  x953  x954  x955  x956  x957  x958  x960  x961  x962  x965  x968 
#>   946   951   953   954   955   956   957   958   960   961   962   965   968 
#>  x971  x972  x976  x977  x981  x985  x997 x1002 x1006 x1009 x1011 x1012 x1014 
#>   971   972   976   977   981   985   997  1002  1006  1009  1011  1012  1014 
#> x1015 x1016 x1017 x1018 x1019 x1024 x1027 x1028 x1029 x1030 x1031 x1032 x1036 
#>  1015  1016  1017  1018  1019  1024  1027  1028  1029  1030  1031  1032  1036 
#> x1037 x1040 x1045 x1048 x1049 x1052 x1053 x1056 x1058 x1059 x1063 x1066 x1067 
#>  1037  1040  1045  1048  1049  1052  1053  1056  1058  1059  1063  1066  1067 
#> x1068 x1071 x1072 x1073 x1074 x1078 x1079 x1080 x1081 x1087 
#>  1068  1071  1072  1073  1074  1078  1079  1080  1081  1087 

# \donttest{
# Segment a times series using a genetic algorithm
cpts <- segment(DataCPSim, method = "cptga")
changepoints(cpts$segmenter)
#> [1] 543 823 940 973
# }
cpts <- segment(DataCPSim, method = "selgmented")
#> No. of breakpoints: 2 .. 3 .. 4 .. 5 .. 6 .. 7 .. 8 .. 9 .. 10 .. 
#> 
#> BIC to detect no. of breakpoints:
#>        0        1        2        3        4        5        6        6 
#> 10727.65 10225.17 10207.73 10221.65 10214.40 10225.80 10231.34 10204.84 
#>        7        8        9 
#> 10219.03 10231.46 10245.46 
#> 
#> No. of selected breakpoints: 4  (2 breakpoint(s) removed due to small slope diff)
changepoints(cpts$segmenter)
#> [1]  566  760  927 1064

# \donttest{
cpts <- segment(DataCPSim, method = "stelpmented")
#> No. of breakpoints: 2 .. 3 .. 4 .. 5 .. 6 .. 7 .. 8 .. 9 .. 10 .. 
#> 
#> BIC to detect no. of breakpoints:
#>        0        1        2        3        4        5        5        5 
#> 8385.437 7430.881 7135.682 6976.159 6987.047 7000.547 7000.547 7000.547 
#>        5        5        5 
#> 7000.547 7000.547 7000.547 
#> 
#> No. of selected breakpoints: 3  
changepoints(cpts$segmenter)
#> [1] 551 822 973
# }
cpts <- segment(DataCPSim, method = "strucchange")
changepoints(cpts$segmenter)
#> [1] 547 767 932

cpts <- segment(DataCPSim, method = "wbs")
changepoints(cpts$segmenter)
#>  [1]  547  822  972  997  999 1031 1033 1040 1041 1063 1066
```
