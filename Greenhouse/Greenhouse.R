# Mitchell Pudil
# STAT 330 
# Greenhouse

nasatemp<-read.table(header=TRUE,text="
Year   Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec    J-D D-N    DJF  MAM  JJA  SON  Year
1969   -10  -14    1   18   19    5   -2    3    9   13   13   28      7   3    -12   13    2   12  1969
1970     9   22    8    9   -4   -2   -2   -9   11    6    2  -12      3   7     20    4   -4    6  1970
1971    -2  -20  -17   -8   -5  -19  -11   -3   -4   -6   -5   -9     -9  -9    -11  -10  -11   -5  1971
1972   -24  -16    2    0   -4    6    1   17    2    9    4   19      1  -1    -16    0    8    5  1972
1973    29   31   27   26   25   17   10    3   10   14    6   -6     16  18     26   26   10   10  1973
1974   -15  -29   -6  -12   -2   -6   -5   11  -12   -8   -7   -9     -8  -8    -16   -7    0   -9  1974
1975     7    6   13    5   16    0   -1  -19   -3   -9  -16  -17     -2  -1      1   11   -7   -9  1975
1976    -2   -6  -21   -8  -24  -16  -10  -14   -9  -26   -4   10    -11 -13     -8  -18  -13  -13  1976
1977    19   22   24   27   30   24   20   16    0    2   17    2     17  18     17   27   20    6  1977
1978     6   14   21   15    6   -2    3  -18    5    0   15   10      6   6      7   14   -6    7  1978
1979    12  -10   18   13    5   13    2   13   25   24   28   47     16  13      4   12   10   26  1979
1980    30   43   30   33   36   17   28   23   19   18   29   19     27  29     40   33   23   22  1980
1981    55   41   49   31   24   31   34   32   16   14   24   43     33  31     38   35   32   18  1981
1982    10   16   -1    9   16    5   13    7   14   13   15   42     13  13     23    8    8   14  1982
1983    52   40   42   31   36   20   17   33   39   16   32   17     31  33     45   36   23   29  1983
1984    30   17   29    8   33    5   17   16   21   16    6   -5     16  18     21   23   13   14  1984
1985    22   -6   17   11   17   18   -1   15   15   12    9   15     12  10      4   15   11   12  1985
1986    29   38   28   24   24   11   11   12    0   13   11   16     18  18     27   25   11    8  1986
1987    36   45   16   22   25   37   45   28   39   32   25   47     33  30     32   21   36   32  1987
1988    57   43   51   45   43   43   34   47   41   39   12   33     41  42     49   46   41   31  1988
1989    15   35   37   33   16   14   33   37   37   32   20   38     29  29     27   29   28   30  1989
1990    40   41   76   55   46   38   43   30   29   41   45   40     44  44     40   59   37   38  1990
1991    40   49   34   52   37   52   47   39   48   30   31   32     41  42     43   41   46   36  1991
1992    44   41   47   23   32   25   12    7    0    8    2   21     22  23     39   34   15    4  1992
1993    37   38   36   27   27   25   28   13   10   22    6   16     24  24     32   30   22   13  1993
1994    27    2   27   40   28   43   31   21   29   41   46   36     31  29     15   32   31   39  1994
1995    50   78   45   47   26   41   47   46   33   47   45   28     44  45     55   39   45   42  1995
1996    25   48   32   36   27   25   36   48   25   19   41   40     33  32     34   32   36   28  1996
1997    31   38   53   36   36   54   35   41   53   62   64   59     47  45     36   42   43   60  1997
1998    60   90   63   63   69   77   68   66   41   44   49   56     62  62     69   65   70   45  1998
1999    48   65   34   33   31   37   37   31   41   39   38   43     40  41     56   33   35   39  1999
2000    24   56   58   57   36   41   39   42   41   28   32   29     40  41     41   50   41   34  2000
2001    42   44   56   51   57   54   60   48   54   51   70   56     54  51     38   55   54   58  2001
2002    75   75   90   57   63   55   61   53   63   55   58   44     62  63     69   70   56   59  2002
2003    72   55   57   54   61   48   54   65   65   74   53   73     61  58     57   57   56   64  2003
2004    58   73   64   63   40   43   24   44   50   63   71   48     53  56     68   56   37   61  2004
2005    70   55   71   68   63   66   64   61   75   76   72   65     67  66     58   67   64   74  2005
2006    56   68   62   50   47   64   52   70   62   67   69   73     62  61     63   53   62   66  2006
2007    94   70   69   74   66   58   60   57   60   57   55   46     64  66     79   69   58   57  2007
2008    23   34   73   51   47   45   59   43   63   63   65   53     52  51     34   57   49   64  2008
2009    61   50   52   59   65   65   70   66   68   64   76   66     63  62     55   59   67   69  2009
2010    73   80   92   85   73   62   59   63   59   69   78   46     70  71     73   83   62   68  2010
2011    49   51   62   62   50   56   71   71   54   63   55   53     58  57     48   58   66   57  2011
2012    45   47   56   67   74   62   54   60   72   75   74   52     61  61     48   66   59   73  2012
2013    66   55   66   52   57   65   57   65   76   67   78   65     64  63     58   58   63   74  2013
2014    73   52   76   77   85   66   56   80   88   81   66   78     73  72     63   79   67   78  2014
2015    81   87   90   74   75   79   71   79   81  107  102  110     86  84     82   80   76   97  2015
2016   115  134  130  107   90   78   82   99   87   89   90   82     99 101    119  109   86   88  2016
2017    96  111  113   93   88   70   81   86   74   88   87   89     90  89     96   98   79   83  2017
 ")
# code to reshape to each row is a month
nasatemp<-nasatemp[,1:13]
dim(nasatemp)
newtemp<-data.frame(temp=matrix(t(nasatemp[2:13]),ncol=1))
newtemp$month<-rep(1:12,49)
newtemp$year<-rep(1969:2017,each=12)


# Monthly Atmospheric CO2 Concentration, expressed as micromol per mole and abbreviated as 
# ppm (1969–present):

maunaloa1<-read.table(header=TRUE,text="
                   where year month methane
                      MLO 1969  8   322.51
                      MLO 1969  9   321.36
                      MLO 1969 10   320.74
                      MLO 1969 11   321.98
                      MLO 1969 12   323.78
                      MLO 1970  1   325.13
                      MLO 1970  2   325.54
                      MLO 1970  3   325.68
                      MLO 1970  4   326.58
                      MLO 1970  5   327.68
                      MLO 1970  6   327.30
                      MLO 1970  7   326.13
                      MLO 1970  8   324.91
                      MLO 1970  9   322.98
                      MLO 1970 10   322.64
                      MLO 1970 11   324.03
                      MLO 1970 12   325.54
                      MLO 1976  7   332.30
                      MLO 1976  8   331.04
                      MLO 1976  9   329.67
                      MLO 1976 10   329.33
                      MLO 1976 11   330.46
                      MLO 1976 12   331.97
                      MLO 1977  1   332.77
                      MLO 1977  2   333.10
                      MLO 1977  3   334.08
                      MLO 1977  4   335.78
                      MLO 1977  5   336.49
                      MLO 1977  6   335.96
                      MLO 1977  7   334.64
                      MLO 1977  8   332.31
                      MLO 1977  9   330.66
                      MLO 1977 10   330.55
                      MLO 1977 11   331.72
                      MLO 1977 12   333.74
                      MLO 1978  1   335.17
                      MLO 1978  2   335.78
                      MLO 1978  3   336.69
                      MLO 1978  4   337.76
                      MLO 1978  5   338.01
                      MLO 1978  6   337.57
                      MLO 1978  7   336.17
                      MLO 1978  8   333.89
                      MLO 1978  9   332.19
                      MLO 1978 10   332.20
                      MLO 1978 11   333.24
                      MLO 1978 12   334.64
                      MLO 1979  1   336.12
                      MLO 1979  2   337.20
                      MLO 1979  3   338.50
                      MLO 1979  4   339.00
                      MLO 1979  5   338.93
                      MLO 1979  6   338.97
                      MLO 1979  7   337.78
                      MLO 1979  8   335.57
                      MLO 1979  9   333.95
                      MLO 1979 10   334.41
                      MLO 1979 11   335.66
                      MLO 1979 12   336.81
                      MLO 1980  1   338.87
                      MLO 1980  2   340.17
                      MLO 1980  3   341.33
                      MLO 1980  4   342.82
                      MLO 1980  5   342.90
                      MLO 1980  6   341.74
                      MLO 1980  7   339.16
                      MLO 1980  8   337.04
                      MLO 1980  9   336.60
                      MLO 1980 10   336.60
                      MLO 1980 11   337.55
                      MLO 1980 12   339.18
                      MLO 1981  1   340.41
                      MLO 1981  2   341.39
                      MLO 1981  3   342.33
                      MLO 1981  4   343.35
                      MLO 1981  5   343.56
                      MLO 1981  6   342.34
                      MLO 1981  7   340.40
                      MLO 1981  8   338.67
                      MLO 1981  9   337.37
                      MLO 1981 10   337.53
                      MLO 1981 11   338.88
                      MLO 1981 12   340.21
                      MLO 1982  1   341.10
                      MLO 1982  2   341.99
                      MLO 1982  3   343.03
                      MLO 1982  4   343.82
                      MLO 1982  5   343.76
                      MLO 1982  6   342.87
                      MLO 1982  7   341.47
                      MLO 1982  8   339.26
                      MLO 1982  9   337.33
                      MLO 1982 10   337.64
                      MLO 1982 11   339.03
                      MLO 1982 12   340.66
                      MLO 1983  1   341.96
                      MLO 1983  2   342.37
                      MLO 1983  3   343.11
                      MLO 1983  4   345.12
                      MLO 1983  5   346.09
                      MLO 1983  6   345.30
                      MLO 1983  7   343.53
                      MLO 1983  8   341.20
                      MLO 1983  9   339.78
                      MLO 1983 10   339.98
                      MLO 1983 11   341.14
                      MLO 1983 12   342.73
                      MLO 1984  1   343.94
                      MLO 1984  2   344.59
                      MLO 1984  3   345.52
                      MLO 1984  4   346.81
                      MLO 1984  5   347.38
                      MLO 1984  6   346.62
                      MLO 1984  7   345.10
                      MLO 1984  8   342.86
                      MLO 1984  9   341.30
                      MLO 1984 10   341.70
                      MLO 1984 11   343.00
                      MLO 1984 12   343.92
                      MLO 1985  1   344.60
                      MLO 1985  2   345.65
                      MLO 1985  3   346.86
                      MLO 1985  4   347.80
                      MLO 1985  5   348.57
                      MLO 1985  6   348.19
                      MLO 1985  7   346.22
                      MLO 1985  8   343.94
                      MLO 1985  9   342.83
                      MLO 1985 10   342.87
                      MLO 1985 11   344.05
                      MLO 1985 12   345.31
                      MLO 1986  1   346.10
                      MLO 1986  2   346.61
                      MLO 1986  3   347.69
                      MLO 1986  4   349.29
                      MLO 1986  5   349.94
                      MLO 1986  6   349.06
                      MLO 1986  7   347.06
                      MLO 1986  8   345.26
                      MLO 1986  9   344.05
                      MLO 1986 10   343.81
                      MLO 1986 11   345.37
                      MLO 1986 12   346.84
                      MLO 1987  1   347.51
                      MLO 1987  2   348.25
                      MLO 1987  3   349.26
                      MLO 1987  4   350.75
                      MLO 1987  5   351.77
                      MLO 1987  6   351.40
                      MLO 1987  7   349.62
                      MLO 1987  8   347.48
                      MLO 1987  9   346.45
                      MLO 1987 10   346.81
                      MLO 1987 11   348.11
                      MLO 1987 12   349.42
                      MLO 1988  1   350.28
                      MLO 1988  2   351.35
                      MLO 1988  3   352.52
                      MLO 1988  4   353.72
                      MLO 1988  5   354.35
                      MLO 1988  6   353.83
                      MLO 1988  7   352.34
                      MLO 1988  8   350.12
                      MLO 1988  9   348.78
                      MLO 1988 10   349.12
                      MLO 1988 11   350.14
                      MLO 1988 12   351.61
                      MLO 1989  1   352.74
                      MLO 1989  2   353.14
                      MLO 1989  3   353.96
                      MLO 1989  4   355.54
                      MLO 1989  5   356.16
                      MLO 1989  6   355.45
                      MLO 1989  7   354.00
                      MLO 1989  8   351.37
                      MLO 1989  9   349.51
                      MLO 1989 10   349.90
                      MLO 1989 11   351.53
                      MLO 1989 12   352.88
                      MLO 1990  1   354.02
                      MLO 1990  2   355.11
                      MLO 1990  3   355.90
                      MLO 1990  4   356.71
                      MLO 1990  5   357.17
                      MLO 1990  6   356.32
                      MLO 1990  7   354.53
                      MLO 1990  8   352.44
                      MLO 1990  9   351.21
                      MLO 1990 10   351.52
                      MLO 1990 11   353.10
                      MLO 1990 12   354.66
                      MLO 1991  1   355.24
                      MLO 1991  2   356.09
                      MLO 1991  3   357.54
                      MLO 1991  4   358.68
                      MLO 1991  5   359.34
                      MLO 1991  6   358.28
                      MLO 1991  7   355.86
                      MLO 1991  8   353.82
                      MLO 1991  9   352.42
                      MLO 1991 10   352.53
                      MLO 1991 11   354.00
                      MLO 1991 12   355.44
                      MLO 1992  1   356.64
                      MLO 1992  2   357.29
                      MLO 1992  3   358.12
                      MLO 1992  4   359.17
                      MLO 1992  5   359.94
                      MLO 1992  6   359.48
                      MLO 1992  7   357.17
                      MLO 1992  8   355.02
                      MLO 1992  9   353.69
                      MLO 1992 10   353.56
                      MLO 1992 11   354.63
                      MLO 1992 12   355.97
                      MLO 1993  1   356.81
                      MLO 1993  2   357.39
                      MLO 1993  3   358.25
                      MLO 1993  4   359.56
                      MLO 1993  5   360.47
                      MLO 1993  6   359.69
                      MLO 1993  7   357.74
                      MLO 1993  8   355.54
                      MLO 1993  9   353.84
                      MLO 1993 10   354.20
                      MLO 1993 11   355.41
                      MLO 1993 12   356.90
                      MLO 1994  1   358.28
                      MLO 1994  2   358.86
                      MLO 1994  3   359.69
                      MLO 1994  4   361.04
                      MLO 1994  5   361.73
                      MLO 1994  6   360.83
                      MLO 1994  7   358.93
                      MLO 1994  8   356.94
                      MLO 1994  9   355.33
                      MLO 1994 10   355.55
                      MLO 1994 11   357.49
                      MLO 1994 12   359.57
                      MLO 1995  1   360.24
                      MLO 1995  2   360.61
                      MLO 1995  3   361.82
                      MLO 1995  4   362.93
                      MLO 1995  5   363.63
                      MLO 1995  6   363.28
                      MLO 1995  7   361.36
                      MLO 1995  8   359.17
                      MLO 1995  9   358.04
                      MLO 1995 10   358.09
                      MLO 1995 11   359.44
                      MLO 1995 12   361.02
                      MLO 1996  1   362.10
                      MLO 1996  2   363.20
                      MLO 1996  3   364.18
                      MLO 1996  4   364.81
                      MLO 1996  5   365.35
                      MLO 1996  6   365.13
                      MLO 1996  7   363.51
                      MLO 1996  8   361.13
                      MLO 1996  9   359.30
                      MLO 1996 10   359.38
                      MLO 1996 11   360.92
                      MLO 1996 12   362.02
                      MLO 1997  1   363.07
                      MLO 1997  2   363.89
                      MLO 1997  3   364.91
                      MLO 1997  4   366.56
                      MLO 1997  5   366.66
                      MLO 1997  6   365.49
                      MLO 1997  7   364.09
                      MLO 1997  8   362.15
                      MLO 1997  9   360.50
                      MLO 1997 10   360.71
                      MLO 1997 11   362.52
                      MLO 1997 12   364.16
                      MLO 1998  1   365.26
                      MLO 1998  2   366.04
                      MLO 1998  3   367.12
                      MLO 1998  4   368.65
                      MLO 1998  5   369.39
                      MLO 1998  6   368.98
                      MLO 1998  7   367.74
                      MLO 1998  8   365.74
                      MLO 1998  9   364.13
                      MLO 1998 10   364.14
                      MLO 1998 11   365.60
                      MLO 1998 12   367.52
                      MLO 1999  1   368.36
                      MLO 1999  2   368.63
                      MLO 1999  3   369.76
                      MLO 1999  4   370.89
                      MLO 1999  5   370.89
                      MLO 1999  6   370.42
                      MLO 1999  7   369.14
                      MLO 1999  8   366.95
                      MLO 1999  9   365.12
                      MLO 1999 10   365.43
                      MLO 1999 11   366.82
                      MLO 1999 12   368.36
                      MLO 2000  1   369.62
                      MLO 2000  2   370.13
                      MLO 2000  3   370.85
                      MLO 2000  4   371.81
                      MLO 2000  5   371.90
                      MLO 2000  6   371.38
                      MLO 2000  7   370.07
                      MLO 2000  8   368.11
                      MLO 2000  9   366.89
                      MLO 2000 10   367.14
                      MLO 2000 11   368.37
                      MLO 2000 12   369.61
                      MLO 2001  1   370.73
                      MLO 2001  2   371.81
                      MLO 2001  3   372.84
                      MLO 2001  4   373.69
                      MLO 2001  5   374.18
                      MLO 2001  6   373.36
                      MLO 2001  7   371.35
                      MLO 2001  8   369.41
                      MLO 2001  9   368.18
                      MLO 2001 10   368.45
                      MLO 2001 11   370.13
                      MLO 2001 12   371.43
                      MLO 2002  1   372.18
                      MLO 2002  2   372.94
                      MLO 2002  3   373.92
                      MLO 2002  4   374.80
                      MLO 2002  5   375.51
                      MLO 2002  6   375.55
                      MLO 2002  7   373.86
                      MLO 2002  8   371.19
                      MLO 2002  9   369.72
                      MLO 2002 10   370.45
                      MLO 2002 11   372.40
                      MLO 2002 12   373.65
                      MLO 2003  1   374.63
                      MLO 2003  2   375.77
                      MLO 2003  3   376.81
                      MLO 2003  4   378.08
                      MLO 2003  5   378.88
                      MLO 2003  6   378.26
                      MLO 2003  7   376.52
                      MLO 2003  8   374.53
                      MLO 2003  9   372.95
                      MLO 2003 10   373.13
                      MLO 2003 11   374.84
                      MLO 2003 12   376.03
                      MLO 2004  1   376.85
                      MLO 2004  2   377.81
                      MLO 2004  3   379.46
                      MLO 2004  4   380.74
                      MLO 2004  5   380.78
                      MLO 2004  6   379.62
                      MLO 2004  7   377.37
                      MLO 2004  8   375.56
                      MLO 2004  9   374.23
                      MLO 2004 10   374.54
                      MLO 2004 11   376.24
                      MLO 2004 12   377.61
                      MLO 2005  1   378.58
                      MLO 2005  2   380.19
                      MLO 2005  3   381.75
                      MLO 2005  4   382.15
                      MLO 2005  5   382.49
                      MLO 2005  6   382.25
                      MLO 2005  7   380.80
                      MLO 2005  8   378.62
                      MLO 2005  9   376.79
                      MLO 2005 10   377.05
                      MLO 2005 11   378.65
                      MLO 2005 12   380.10
                      MLO 2006  1   381.22
                      MLO 2006  2   381.92
                      MLO 2006  3   382.84
                      MLO 2006  4   384.73
                      MLO 2006  5   385.42
                      MLO 2006  6   384.06
                      MLO 2006  7   382.30
                      MLO 2006  8   380.06
                      MLO 2006  9   378.71
                      MLO 2006 10   379.11
                      MLO 2006 11   380.39
                      MLO 2006 12   381.80
                      MLO 2007  1   383.26
                      MLO 2007  2   384.42
                      MLO 2007  3   385.40
                      MLO 2007  4   386.49
                      MLO 2007  5   386.89
                      MLO 2007  6   386.31
                      MLO 2007  7   384.54
                      MLO 2007  8   381.65
                      MLO 2007  9   380.31
                      MLO 2007 10   381.16
                      MLO 2007 11   382.63
                      MLO 2007 12   384.06
                      MLO 2008  1   385.53
                      MLO 2008  2   385.71
                      MLO 2008  3   385.57
                      MLO 2008  4   387.05
                      MLO 2008  5   388.70
                      MLO 2008  6   388.46
                      MLO 2008  7   386.66
                      MLO 2008  8   384.24
                      MLO 2008  9   382.77
                      MLO 2008 10   382.91
                      MLO 2008 11   384.12
                      MLO 2008 12   385.66
                      MLO 2009  1   386.97
                      MLO 2009  2   387.71
                      MLO 2009  3   388.41
                      MLO 2009  4   389.34
                      MLO 2009  5   390.11
                      MLO 2009  6   389.52
                      MLO 2009  7   387.71
                      MLO 2009  8   385.84
                      MLO 2009  9   384.44
                      MLO 2009 10   384.60
                      MLO 2009 11   386.00
                      MLO 2009 12   387.51
                      MLO 2010  1   388.59
                      MLO 2010  2   389.72
                      MLO 2010  3   391.07
                      MLO 2010  4   392.60
                      MLO 2010  5   393.37
                      MLO 2010  6   392.25
                      MLO 2010  7   390.09
                      MLO 2010  8   388.00
                      MLO 2010  9   386.55
                      MLO 2010 10   387.21
                      MLO 2010 11   388.57
                      MLO 2010 12   390.06
                      MLO 2011  1   391.56
                      MLO 2011  2   392.14
                      MLO 2011  3   392.64
                      MLO 2011  4   393.69
                      MLO 2011  5   394.26
                      MLO 2011  6   393.80
                      MLO 2011  7   392.35
                      MLO 2011  8   389.96
                      MLO 2011  9   388.61
                      MLO 2011 10   389.16
                      MLO 2011 11   390.47
                      MLO 2011 12   392.17
                      MLO 2012  1   393.00
                      MLO 2012  2   393.25
                      MLO 2012  3   394.66
                      MLO 2012  4   396.56
                      MLO 2012  5   396.98
                      MLO 2012  6   395.86
                      MLO 2012  7   394.26
                      MLO 2012  8   392.19
                      MLO 2012  9   390.84
                      MLO 2012 10   391.11
                      MLO 2012 11   393.06
                      MLO 2012 12   394.64
                      MLO 2013  1   395.66
                      MLO 2013  2   396.80
                      MLO 2013  3   397.75
                      MLO 2013  4   398.70
                      MLO 2013  5   399.61
                      MLO 2013  6   399.15
                      MLO 2013  7   397.19
                      MLO 2013  8   395.01
                      MLO 2013  9   393.73
                      MLO 2013 10   393.87
                      MLO 2013 11   395.43
                      MLO 2013 12   397.03
                      MLO 2014  1   397.91
                      MLO 2014  2   398.33
                      MLO 2014  3   399.75
                      MLO 2014  4   401.64
                      MLO 2014  5   402.24
                      MLO 2014  6   401.39
                      MLO 2014  7   398.96
                      MLO 2014  8   396.52
                      MLO 2014  9   395.35
                      MLO 2014 10   395.67
                      MLO 2014 11   397.50
                      MLO 2014 12   399.26
                      MLO 2015  1   400.05
                      MLO 2015  2   400.38
                      MLO 2015  3   401.58
                      MLO 2015  4   403.68
                      MLO 2015  5   404.16
                      MLO 2015  6   402.92
                      MLO 2015  7   400.93
                      MLO 2015  8   398.90
                      MLO 2015  9   397.65
                      MLO 2015 10   398.23
                      MLO 2015 11   399.95
                      MLO 2015 12   401.53
                      MLO 2016  1   402.72
                      MLO 2016  2   403.62
                      MLO 2016  3   405.23
                      MLO 2016  4   407.35
                      MLO 2016  5   407.87
                      MLO 2016  6   406.57
                      MLO 2016  7   404.37
                      MLO 2016  8   401.69
                      MLO 2016  9   400.23
                      MLO 2016 10   401.47
                      MLO 2016 11   403.53
                      MLO 2016 12   404.66

")


#Monthly Atmospheric CH4 Concentration, expressed as nanomoles per mole and
#abbreviated as ppb (1983–present)


maunaloa2<-read.table(header=TRUE,text="
where year month methane
MLO 1983  5  1639.23
MLO 1983  6  1633.53
MLO 1983  7  1633.19
MLO 1983  8  1631.38
MLO 1983  9  1648.43
MLO 1983 10  1663.75
MLO 1983 11  1658.22
MLO 1983 12  1654.33
MLO 1984  1  1658.97
MLO 1984  2  1656.47
MLO 1984  3  1655.76
MLO 1984  4  1657.71
MLO 1984  5  1649.33
MLO 1984  6  1633.93
MLO 1984  7  1629.71
MLO 1984  8  1643.76
MLO 1984  9  1663.31
MLO 1984 10  1673.72
MLO 1984 11  1676.09
MLO 1984 12  1671.23
MLO 1985  1  1662.44
MLO 1985  2  1665.22
MLO 1985  3  1677.36
MLO 1985  4  1674.26
MLO 1985  5  1665.95
MLO 1985  6  1658.63
MLO 1985  7  1653.49
MLO 1985  8  1653.73
MLO 1985  9  1667.63
MLO 1985 10  1680.79
MLO 1985 11  1679.95
MLO 1985 12  1677.30
MLO 1986  1  1675.09
MLO 1986  2  1666.10
MLO 1986  3  1672.14
MLO 1986  4  1687.78
MLO 1986  5  1685.33
MLO 1986  6  1684.31
MLO 1986  7  1682.00
MLO 1986  8  1669.87
MLO 1986  9  1679.75
MLO 1986 10  1690.42
MLO 1986 11  1691.76
MLO 1986 12  1697.38
MLO 1987  1  1692.00
MLO 1987  2  1689.67
MLO 1987  3  1694.41
MLO 1987  4  1695.58
MLO 1987  5  1694.19
MLO 1987  6  1687.17
MLO 1987  7  1680.59
MLO 1987  8  1678.21
MLO 1987  9  1681.06
MLO 1987 10  1700.84
MLO 1987 11  1715.96
MLO 1987 12  1707.57
MLO 1988  1  1697.68
MLO 1988  2  1700.56
MLO 1988  3  1705.48
MLO 1988  4  1707.58
MLO 1988  5  1705.13
MLO 1988  6  1698.45
MLO 1988  7  1687.25
MLO 1988  8  1691.20
MLO 1988  9  1700.78
MLO 1988 10  1708.26
MLO 1988 11  1719.26
MLO 1988 12  1725.77
MLO 1989  1  1720.86
MLO 1989  2  1712.63
MLO 1989  3  1715.06
MLO 1989  4  1723.47
MLO 1989  5  1724.35
MLO 1989  6  1713.17
MLO 1989  7  1699.86
MLO 1989  8  1706.65
MLO 1989  9  1720.84
MLO 1989 10  1721.99
MLO 1989 11  1726.19
MLO 1989 12  1730.72
MLO 1990  1  1727.94
MLO 1990  2  1728.47
MLO 1990  3  1733.87
MLO 1990  4  1737.55
MLO 1990  5  1734.57
MLO 1990  6  1722.31
MLO 1990  7  1712.70
MLO 1990  8  1718.02
MLO 1990  9  1733.91
MLO 1990 10  1736.92
MLO 1990 11  1741.48
MLO 1990 12  1749.67
MLO 1991  1  1734.88
MLO 1991  2  1731.02
MLO 1991  3  1746.57
MLO 1991  4  1749.61
MLO 1991  5  1747.34
MLO 1991  6  1740.02
MLO 1991  7  1730.94
MLO 1991  8  1722.04
MLO 1991  9  1734.65
MLO 1991 10  1751.77
MLO 1991 11  1750.32
MLO 1991 12  1752.46
MLO 1992  1  1755.62
MLO 1992  2  1750.06
MLO 1992  3  1752.44
MLO 1992  4  1753.30
MLO 1992  5  1744.58
MLO 1992  6  1743.34
MLO 1992  7  1736.29
MLO 1992  8  1725.76
MLO 1992  9  1729.88
MLO 1992 10  1740.78
MLO 1992 11  1755.35
MLO 1992 12  1758.41
MLO 1993  1  1747.88
MLO 1993  2  1745.69
MLO 1993  3  1748.00
MLO 1993  4  1746.77
MLO 1993  5  1749.61
MLO 1993  6  1745.59
MLO 1993  7  1731.93
MLO 1993  8  1732.02
MLO 1993  9  1744.57
MLO 1993 10  1760.24
MLO 1993 11  1768.05
MLO 1993 12  1763.59
MLO 1994  1  1759.89
MLO 1994  2  1752.11
MLO 1994  3  1754.68
MLO 1994  4  1763.82
MLO 1994  5  1755.15
MLO 1994  6  1746.67
MLO 1994  7  1737.43
MLO 1994  8  1734.00
MLO 1994  9  1757.24
MLO 1994 10  1778.62
MLO 1994 11  1781.15
MLO 1994 12  1779.38
MLO 1995  1  1764.55
MLO 1995  2  1757.10
MLO 1995  3  1764.86
MLO 1995  4  1762.96
MLO 1995  5  1762.21
MLO 1995  6  1759.23
MLO 1995  7  1749.37
MLO 1995  8  1751.18
MLO 1995  9  1761.52
MLO 1995 10  1768.91
MLO 1995 11  1768.32
MLO 1995 12  1773.02
MLO 1996  1  1771.38
MLO 1996  2  1770.46
MLO 1996  3  1769.92
MLO 1996  4  1759.12
MLO 1996  5  1755.04
MLO 1996  6  1753.80
MLO 1996  7  1747.19
MLO 1996  8  1745.61
MLO 1996  9  1767.21
MLO 1996 10  1774.69
MLO 1996 11  1769.96
MLO 1996 12  1773.12
MLO 1997  1  1769.96
MLO 1997  2  1768.67
MLO 1997  3  1775.66
MLO 1997  4  1780.66
MLO 1997  5  1776.62
MLO 1997  6  1768.26
MLO 1997  7  1751.54
MLO 1997  8  1750.35
MLO 1997  9  1775.74
MLO 1997 10  1784.89
MLO 1997 11  1784.07
MLO 1997 12  1783.30
MLO 1998  1  1772.27
MLO 1998  2  1766.13
MLO 1998  3  1774.66
MLO 1998  4  1780.43
MLO 1998  5  1781.20
MLO 1998  6  1779.11
MLO 1998  7  1762.53
MLO 1998  8  1757.23
MLO 1998  9  1771.19
MLO 1998 10  1787.42
MLO 1998 11  1798.38
MLO 1998 12  1803.08
MLO 1999  1  1796.37
MLO 1999  2  1784.76
MLO 1999  3  1788.19
MLO 1999  4  1788.42
MLO 1999  5  1773.25
MLO 1999  6  1771.20
MLO 1999  7  1767.00
MLO 1999  8  1768.66
MLO 1999  9  1785.91
MLO 1999 10  1794.49
MLO 1999 11  1796.52
MLO 1999 12  1801.93
MLO 2000  1  1802.08
MLO 2000  2  1793.84
MLO 2000  3  1791.95
MLO 2000  4  1786.89
MLO 2000  5  1773.88
MLO 2000  6  1772.35
MLO 2000  7  1769.46
MLO 2000  8  1762.08
MLO 2000  9  1773.90
MLO 2000 10  1796.96
MLO 2000 11  1798.68
MLO 2000 12  1790.80
MLO 2001  1  1789.20
MLO 2001  2  1793.33
MLO 2001  3  1801.05
MLO 2001  4  1797.36
MLO 2001  5  1783.16
MLO 2001  6  1773.35
MLO 2001  7  1769.25
MLO 2001  8  1764.68
MLO 2001  9  1779.92
MLO 2001 10  1790.89
MLO 2001 11  1793.03
MLO 2001 12  1796.09
MLO 2002  1  1787.42
MLO 2002  2  1782.86
MLO 2002  3  1786.15
MLO 2002  4  1781.95
MLO 2002  5  1778.33
MLO 2002  6  1774.58
MLO 2002  7  1766.41
MLO 2002  8  1766.10
MLO 2002  9  1780.44
MLO 2002 10  1795.45
MLO 2002 11  1801.15
MLO 2002 12  1792.65
MLO 2003  1  1785.94
MLO 2003  2  1791.76
MLO 2003  3  1796.92
MLO 2003  4  1794.97
MLO 2003  5  1786.89
MLO 2003  6  1790.58
MLO 2003  7  1785.33
MLO 2003  8  1773.06
MLO 2003  9  1788.25
MLO 2003 10  1806.05
MLO 2003 11  1807.61
MLO 2003 12  1794.57
MLO 2004  1  1789.81
MLO 2004  2  1797.91
MLO 2004  3  1806.94
MLO 2004  4  1805.52
MLO 2004  5  1787.75
MLO 2004  6  1779.82
MLO 2004  7  1774.59
MLO 2004  8  1769.80
MLO 2004  9  1778.21
MLO 2004 10  1790.72
MLO 2004 11  1800.49
MLO 2004 12  1796.73
MLO 2005  1  1788.48
MLO 2005  2  1796.08
MLO 2005  3  1802.82
MLO 2005  4  1791.30
MLO 2005  5  1780.86
MLO 2005  6  1780.05
MLO 2005  7  1771.32
MLO 2005  8  1766.46
MLO 2005  9  1787.27
MLO 2005 10  1806.12
MLO 2005 11  1803.11
MLO 2005 12  1798.37
MLO 2006  1  1794.46
MLO 2006  2  1785.84
MLO 2006  3  1786.21
MLO 2006  4  1800.64
MLO 2006  5  1798.33
MLO 2006  6  1779.65
MLO 2006  7  1765.36
MLO 2006  8  1762.14
MLO 2006  9  1775.52
MLO 2006 10  1788.46
MLO 2006 11  1791.44
MLO 2006 12  1794.62
MLO 2007  1  1799.19
MLO 2007  2  1802.60
MLO 2007  3  1802.63
MLO 2007  4  1801.65
MLO 2007  5  1795.23
MLO 2007  6  1781.34
MLO 2007  7  1771.38
MLO 2007  8  1778.95
MLO 2007  9  1793.78
MLO 2007 10  1801.88
MLO 2007 11  1803.32
MLO 2007 12  1805.15
MLO 2008  1  1809.46
MLO 2008  2  1802.99
MLO 2008  3  1792.38
MLO 2008  4  1792.12
MLO 2008  5  1796.01
MLO 2008  6  1791.32
MLO 2008  7  1782.40
MLO 2008  8  1779.41
MLO 2008  9  1794.58
MLO 2008 10  1813.66
MLO 2008 11  1811.90
MLO 2008 12  1812.40
MLO 2009  1  1816.16
MLO 2009  2  1814.73
MLO 2009  3  1815.28
MLO 2009  4  1807.06
MLO 2009  5  1802.72
MLO 2009  6  1806.24
MLO 2009  7  1794.25
MLO 2009  8  1788.09
MLO 2009  9  1801.87
MLO 2009 10  1811.50
MLO 2009 11  1814.68
MLO 2009 12  1815.55
MLO 2010  1  1810.92
MLO 2010  2  1817.87
MLO 2010  3  1821.24
MLO 2010  4  1818.01
MLO 2010  5  1816.50
MLO 2010  6  1803.08
MLO 2010  7  1793.50
MLO 2010  8  1800.61
MLO 2010  9  1810.57
MLO 2010 10  1826.33
MLO 2010 11  1830.53
MLO 2010 12  1821.20
MLO 2011  1  1823.70
MLO 2011  2  1819.68
MLO 2011  3  1815.48
MLO 2011  4  1816.01
MLO 2011  5  1811.52
MLO 2011  6  1810.25
MLO 2011  7  1803.09
MLO 2011  8  1799.91
MLO 2011  9  1813.09
MLO 2011 10  1823.80
MLO 2011 11  1827.44
MLO 2011 12  1831.82
MLO 2012  1  1827.56
MLO 2012  2  1819.24
MLO 2012  3  1824.07
MLO 2012  4  1834.90
MLO 2012  5  1829.55
MLO 2012  6  1814.01
MLO 2012  7  1803.91
MLO 2012  8  1803.99
MLO 2012  9  1815.80
MLO 2012 10  1828.91
MLO 2012 11  1836.46
MLO 2012 12  1836.27
MLO 2013  1  1833.35
MLO 2013  2  1836.92
MLO 2013  3  1838.80
MLO 2013  4  1830.36
MLO 2013  5  1825.20
MLO 2013  6  1814.80
MLO 2013  7  1804.80
MLO 2013  8  1813.64
MLO 2013  9  1838.16
MLO 2013 10  1844.51
MLO 2013 11  1840.42
MLO 2013 12  1848.10
MLO 2014  1  1840.72
MLO 2014  2  1831.02
MLO 2014  3  1847.06
MLO 2014  4  1855.57
MLO 2014  5  1844.18
MLO 2014  6  1831.50
MLO 2014  7  1818.96
MLO 2014  8  1824.29
MLO 2014  9  1841.04
MLO 2014 10  1844.03
MLO 2014 11  1854.77
MLO 2014 12  1863.90
MLO 2015  1  1853.42
MLO 2015  2  1848.67
MLO 2015  3  1858.38
MLO 2015  4  1866.30
MLO 2015  5  1862.18
MLO 2015  6  1844.58
MLO 2015  7  1831.99
MLO 2015  8  1827.55
MLO 2015  9  1840.39
MLO 2015 10  1860.36
MLO 2015 11  1866.15
MLO 2015 12  1863.49
MLO 2016  1  1858.25
MLO 2016  2  1858.33
MLO 2016  3  1864.58
MLO 2016  4  1873.36
MLO 2016  5  1870.71
MLO 2016  6  1855.71
MLO 2016  7  1835.76
MLO 2016  8  1840.68
MLO 2016  9  1863.21
MLO 2016 10  1876.06
MLO 2016 11  1874.51
MLO 2016 12  1864.68
")


# merge datasets and clean up
climate<-merge(merge(newtemp,maunaloa1,by=c("year","month"),all.x=TRUE),maunaloa2,by=c("year","month"),all.x=TRUE)
climate<-subset(climate,!is.na(where.x) & !is.na(where.y) )
climate$co2 <- climate$methane.x
climate$methane <- climate$methane.y
climate<-climate[,c("year","month","temp","co2","methane")]


#Correlation betweeen temperature and CO2
cor(climate$temp, climate$co2)
plot(climate$co2, climate$temp, col="steel blue", main="Correlation of CO2 and Temperature", xlab="CO2", ylab="Temperature")


#Correlation between termpature and methane
cor(climate$temp, climate$methane)
plot(climate$methane, climate$temp, col="green", main="Correlation of Methane and Temperature", xlab="Methane", ylab="Temperature")


#Response variable: Earth's monthly temperature
#Explanatory Variables: Levels of CO2 (ppm) and methane (ppb)

#Model for temp and the assumptions for inference:
#temp=β0 +β1co2+β2methane+ε, ε∼N(0,σ2) 

# if we reject, then test H0: no CO2 effect and test H0: no methane effect

#Fit the model. Would be interesting to interact co2 with methane
#temp=β0 +β1co2+β2methane+ε, ε∼N(0,σ2) and report the parameter estimates and standard errors.

out.climate <- lm(temp~co2+methane, data=climate)
summary(out.climate)
#For a one ppm increase in CO2, we observe an expected global increase (0.01 degrees celcius)
# of 0.741 holding methane constant

#For a one ppb increasei n methane, we observe and expected global increase (0.01 degrees 
# celcius) of 0.094, holding CO2 levels constant. 


#Partial Effect Plot
library(car)
avPlots(out.climate)
#slope if we adjust for methane/co2 (response here is temperature after controlling for methane/temp)
# bigger than average/smaller than average temperature


#Inference: 
# H0: no greenhouse gas effect (i.e. β1=0 and β2=0)
# ANOVA approach: compare two models (how different are they)
# - one assumes H0 is true: lm(temp ~ +1) which is an intercept-only model
# - the other assumes HA is true (HA is out.climate)
# If fit is true, then H0 is reasonable. If fit is different, then HA is true. 


# Test H0: no greenhouse gas effect
# Fit the model assuming H0 is true (reduced model)

nogreenhousegas.climate <- lm(temp ~ +1, data=climate)
anova(nogreenhousegas.climate, out.climate) #df is difference in number of parameters we are using

#Follow up: which gas?

summary(out.climate)  #gives p values for each beta

#Since we reject H0 and conclude there is a significant greenhouse gas effect, look at individual
#regression coefficients. 
#t-test from summary

#95% CI

confint(out.climate)
#They started measuring methane in 1983. 
#Because methane is a trace element, it is harder to measure (higher measurement error)


#Train data: all data, year less than or equal to 2015
# Test data: year=2016

train.climate <- subset(climate, year<=2015)
test.climate <- subset(climate, year==2016)

#Fit model on train
out.train <- lm(temp ~ co2 + methane, data=train.climate)

#Use train model to predict on test
prediction <- predict(out.train, newdata=test.climate)


#compare actual to prediction
cbind(test.climate$temp, predict(out.train, newdata=test.climate))



#Publication-quality graph which includes 95% confidence interval


plot(prediction, test.climate$temp)
abline(0,1,col="blue")


#We tend to underpredict. We should therefore include more greenhouse gas. 
# There might also be an interaction effect.  
























