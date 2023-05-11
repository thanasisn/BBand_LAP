---
title:         "Inspect raw CM-21 data **SIG** "
author:        "Natsis Athanasios"
institute:     "AUTH"
affiliation:   "Laboratory of Atmospheric Physics"
documentclass: article
classoption:   a4paper,oneside
fontsize:      10pt
geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"

link-citations:  yes
colorlinks:      yes

header-includes:
- \usepackage{caption}
- \usepackage{placeins}
- \captionsetup{font=small}

output:
  bookdown::pdf_document2:
    number_sections:  no
    fig_caption:      no
    keep_tex:         no
    latex_engine:     xelatex
    toc:              yes
    toc_depth:        4
    fig_width:        8
    fig_height:       5
  html_document:
    toc:        true
    fig_width:  7.5
    fig_height: 5

date: "2023-05-11"

params:
  CLEAN: TRUE

---

 **SIG**

**Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**

**Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**







## Intro

Produce yearly plots for **CM-21**.

Shows only **raw data** aspects.

It can use flags to show 'CLEAN'/'DIRTY' data.

For 'CLEAN' data, it removes from view:

- Bad recordings ranges `cm21_bad_data_flag`
- Physical recording limits `cm21_signal_lower_limit()` and `cm21_signal_upper_limit()`

Mark outliers for signal and SD with:

**mean(variable) -/+ 4 * sd(variable)**

This is just a report it doesn't alter the data.




\FloatBarrier

\newpage


## Year: 1993 


**Days without any CM-21 data:**

1993-01-01 1993-01-02 1993-01-03 1993-01-04 1993-01-05 1993-01-06 1993-01-07 1993-01-08 1993-01-09 1993-01-10 1993-01-11 1993-01-12 1993-01-13 1993-01-14 1993-01-15 1993-01-16 1993-01-17 1993-01-18 1993-01-19 1993-01-20 1993-01-21 1993-01-22 1993-01-23 1993-01-24 1993-01-25 1993-01-26 1993-01-27 1993-01-28 1993-01-29 1993-01-30 1993-01-31 1993-02-01 1993-02-02 1993-02-03 1993-02-04 1993-02-05 1993-02-06 1993-02-07 1993-02-08 1993-02-09 1993-02-10 1993-02-11 1993-02-12 1993-02-13 1993-02-14 1993-02-15 1993-02-16 1993-02-17 1993-02-18 1993-02-19 1993-02-20 1993-02-21 1993-02-22 1993-02-23 1993-02-24 1993-02-25 1993-02-26 1993-02-27 1993-02-28 1993-03-01 1993-03-02 1993-03-03 1993-03-04 1993-03-05 1993-03-06 1993-03-07 1993-03-08 1993-03-09 1993-03-10 1993-03-11 1993-03-12 1993-03-13 1993-03-14 1993-03-15 1993-03-16 1993-03-17 1993-03-18 1993-03-19 1993-03-20 1993-03-21 1993-03-22 1993-03-23 1993-03-24 1993-03-25 1993-03-26 1993-03-27 1993-03-28 1993-03-29 1993-03-30 1993-03-31 1993-04-01 1993-04-02 1993-04-03 1993-04-04 1993-04-05 1993-04-06 1993-04-07 1993-04-08 1993-04-09 1993-04-10 1993-04-11 1993-04-15 1993-04-16 1993-04-17 1993-04-18 1993-04-19 1993-04-20 1993-04-21 1993-04-22 1993-04-23 1993-04-24 1993-04-25 1993-04-26 1993-04-27 1993-04-28 1993-04-29 1993-05-19 1993-08-22 1993-08-29 1993-08-30  



### Proposed outliers limits 


\footnotesize


--------------------------------------
                an         low     upe
------------------ ----------- -------
          CM21_sig     -0.4608   4.329

       CM21_sig_sd   -0.001128    1.11

  CM21_sig_wo_dark    -0.02018   4.343
--------------------------------------



\normalsize


**Days with outliers:**



1993-06-08

1993-06-08




**Days hitting physical limit:**



1993-06-08

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :1993-01-01 00:00:30    Min.  : 17.19    Min.  :-1.73    Min.  :0.00

  1st Qu.:1993-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.01   1st Qu.:0.00

  Median :1993-07-02 12:00:00   Median : 89.30   Median : 0.03   Median :0.00

    Mean :1993-07-02 12:00:00     Mean : 89.66     Mean : 0.47     Mean :0.01

  3rd Qu.:1993-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.82   3rd Qu.:0.00

   Max.  :1993-12-31 23:59:30    Max.  :162.81    Max.  : 2.89    Max.  :1.85

                           NA               NA    NA's :193052   NA's :193081
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-1} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-2} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-3} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-4} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-5} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-6} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-7} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-8} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-9} \end{center}





\FloatBarrier

\newpage


## Year: 1994 


**Days without any CM-21 data:**

1994-01-26 1994-01-27 1994-01-28 1994-01-29 1994-08-08  



### Proposed outliers limits 


\footnotesize


---------------------------------------
                an          low     upe
------------------ ------------ -------
          CM21_sig       -0.783    4.42

       CM21_sig_sd   -0.0003913   1.107

  CM21_sig_wo_dark     -0.01946   4.432
---------------------------------------



\normalsize


**Days with outliers:**



1994-01-25






**Days hitting physical limit:**



1994-01-25

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :1994-01-01 00:00:30    Min.  : 17.19    Min.  :-3.62    Min.  :0.00

  1st Qu.:1994-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.01   1st Qu.:0.00

  Median :1994-07-02 12:00:00   Median : 89.30   Median : 0.02   Median :0.00

    Mean :1994-07-02 12:00:00     Mean : 89.66     Mean : 0.43     Mean :0.01

  3rd Qu.:1994-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.73   3rd Qu.:0.00

   Max.  :1994-12-31 23:59:30    Max.  :162.81    Max.  : 3.01    Max.  :0.93

                           NA               NA     NA's :35057    NA's :35057
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-10} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-11} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-12} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-13} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-14} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-15} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-16} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-17} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-18} \end{center}





\FloatBarrier

\newpage


## Year: 1995 


**Days without any CM-21 data:**

1995-04-06 1995-04-07 1995-05-07 1995-08-01 1995-10-17 1995-10-18 1995-10-19 1995-10-20  



### Proposed outliers limits 


\footnotesize


-------------------------------------
                an        low     upe
------------------ ---------- -------
          CM21_sig    -0.5483   4.858

       CM21_sig_sd   -0.01987   3.248

  CM21_sig_wo_dark    -0.2688   4.746
-------------------------------------



\normalsize


**Days with outliers:**



1995-09-24 1995-10-15 1995-11-08 1995-12-26

1995-12-13




**Days hitting physical limit:**



1995-10-15 1995-11-08 1995-12-26

\footnotesize


------------------------------------------------------------------------------
                         Date              SZA        CM21_sig     CM21_sig_sd
----------------------------- ---------------- --------------- ---------------
   Min.  :1995-01-01 00:00:30    Min.  : 17.19    Min.  :-1.61    Min.  : 0.00

  1st Qu.:1995-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.01   1st Qu.: 0.00

  Median :1995-07-02 12:00:00   Median : 89.30   Median : 0.00   Median : 0.00

    Mean :1995-07-02 12:00:00     Mean : 89.66     Mean : 0.37     Mean : 0.02

  3rd Qu.:1995-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.58   3rd Qu.: 0.01

   Max.  :1995-12-31 23:59:30    Max.  :162.81    Max.  : 2.90    Max.  :13.42

                           NA               NA     NA's :36573     NA's :36564
------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-19} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-20} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-21} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-22} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-23} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-24} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-25} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-26} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-27} \end{center}




### Year: 1995  exceptions 



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-28} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-29} \end{center}





\FloatBarrier

\newpage


## Year: 1996 


**Days without any CM-21 data:**

1996-08-05 1996-11-11 1996-11-12 1996-11-13 1996-11-14  



### Proposed outliers limits 


\footnotesize


---------------------------------------
                an         low      upe
------------------ ----------- --------
          CM21_sig     -0.2015    2.354

       CM21_sig_sd   -0.001608   0.5367

  CM21_sig_wo_dark    -0.01657    2.338
---------------------------------------



\normalsize


**Days with outliers:**



1996-01-20 1996-02-01






**Days hitting physical limit:**



1996-01-20

\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :1996-01-01 00:00:30    Min.  : 17.19    Min.  :-0.609    Min.  :0.000

  1st Qu.:1996-04-01 12:00:15   1st Qu.: 61.90   1st Qu.:-0.003   1st Qu.:0.000

  Median :1996-07-02 00:00:00   Median : 89.33   Median : 0.000   Median :0.000

    Mean :1996-07-02 00:00:00     Mean : 89.71     Mean : 0.187     Mean :0.004

  3rd Qu.:1996-10-01 11:59:45   3rd Qu.:117.60   3rd Qu.: 0.268   3rd Qu.:0.002

   Max.  :1996-12-31 23:59:30    Max.  :162.81    Max.  : 1.571    Max.  :0.444

                           NA               NA      NA's :18976     NA's :18976
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-30} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-31} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-32} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-33} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-34} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-35} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-36} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-37} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-38} \end{center}




### Year: 1996  exceptions 



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-39} \end{center}





\FloatBarrier

\newpage


## Year: 1997 


**Days without any CM-21 data:**

1997-01-26 1997-01-27 1997-07-01 1997-07-02 1997-07-03 1997-07-04 1997-07-05  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig     -0.01363     2.32

       CM21_sig_sd   -6.771e-05   0.6385

  CM21_sig_wo_dark    -0.008342    2.326
----------------------------------------



\normalsize


**Days with outliers:**



1997-07-29 1997-09-10 1997-09-11






**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :1997-01-01 00:00:30    Min.  : 17.19    Min.  :-0.015    Min.  :0.000

  1st Qu.:1997-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.004   1st Qu.:0.000

  Median :1997-07-02 12:00:00   Median : 89.29   Median : 0.001   Median :0.000

    Mean :1997-07-02 12:00:00     Mean : 89.66     Mean : 0.206     Mean :0.004

  3rd Qu.:1997-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.330   3rd Qu.:0.001

   Max.  :1997-12-31 23:59:30    Max.  :162.81    Max.  : 1.477    Max.  :0.456

                           NA               NA      NA's :21156     NA's :21156
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-40} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-41} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-42} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-43} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-44} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-45} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-46} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-47} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-48} \end{center}





\FloatBarrier

\newpage


## Year: 1998 


**Days without any CM-21 data:**

1998-03-01 1998-03-02 1998-06-05 1998-06-06 1998-06-07 1998-07-01 1998-07-02 1998-07-03 1998-07-04 1998-07-05 1998-07-06 1998-07-07 1998-07-08 1998-08-23  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig     -0.01355    2.233

       CM21_sig_sd   -8.038e-05   0.5858

  CM21_sig_wo_dark    -0.009322    2.239
----------------------------------------



\normalsize


**Days with outliers:**



1998-06-03 1998-06-13






**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :1998-01-01 00:00:30    Min.  : 17.19    Min.  :-0.025    Min.  :0.000

  1st Qu.:1998-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.003   1st Qu.:0.000

  Median :1998-07-02 12:00:00   Median : 89.29   Median : 0.001   Median :0.000

    Mean :1998-07-02 12:00:00     Mean : 89.66     Mean : 0.203     Mean :0.003

  3rd Qu.:1998-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.325   3rd Qu.:0.001

   Max.  :1998-12-31 23:59:30    Max.  :162.81    Max.  : 1.572    Max.  :0.491

                           NA               NA      NA's :29580     NA's :29580
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-49} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-50} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-51} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-52} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-53} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-54} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-55} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-56} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-57} \end{center}





\FloatBarrier

\newpage


## Year: 1999 


**Days without any CM-21 data:**

1999-10-31  



### Proposed outliers limits 


\footnotesize


---------------------------------------
                an          low     upe
------------------ ------------ -------
          CM21_sig     -0.01745   2.185

       CM21_sig_sd   -8.733e-05   0.585

  CM21_sig_wo_dark     -0.01465    2.19
---------------------------------------



\normalsize


**Days with outliers:**



1999-06-09 1999-06-12 1999-06-22 1999-07-17 1999-07-30






**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :1999-01-01 00:00:30    Min.  : 17.19    Min.  :-0.037    Min.  :0.000

  1st Qu.:1999-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.004   1st Qu.:0.000

  Median :1999-07-02 12:00:00   Median : 89.30   Median : 0.002   Median :0.000

    Mean :1999-07-02 12:00:00     Mean : 89.66     Mean : 0.212     Mean :0.003

  3rd Qu.:1999-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.355   3rd Qu.:0.001

   Max.  :1999-12-31 23:59:30    Max.  :162.81    Max.  : 1.615    Max.  :0.543

                           NA               NA       NA's :9082      NA's :9082
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-58} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-59} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-60} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-61} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-62} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-63} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-64} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-65} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-66} \end{center}





\FloatBarrier

\newpage


## Year: 2000 


**Days without any CM-21 data:**

2000-01-01 2000-01-02 2000-02-23 2000-02-24 2000-02-25 2000-06-04 2000-07-25 2000-09-24  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig     -0.09678    2.116

       CM21_sig_sd   -3.664e-05   0.5836

  CM21_sig_wo_dark     -0.09264    2.123
----------------------------------------



\normalsize


**Days with outliers:**










**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2000-01-01 00:00:30    Min.  : 17.19    Min.  :-0.019    Min.  :0.000

  1st Qu.:2000-04-01 12:00:15   1st Qu.: 61.90   1st Qu.:-0.004   1st Qu.:0.000

  Median :2000-07-02 00:00:00   Median : 89.33   Median : 0.002   Median :0.000

    Mean :2000-07-02 00:00:00     Mean : 89.71     Mean : 0.222     Mean :0.003

  3rd Qu.:2000-10-01 11:59:45   3rd Qu.:117.60   3rd Qu.: 0.385   3rd Qu.:0.001

   Max.  :2000-12-31 23:59:30    Max.  :162.81    Max.  : 1.466    Max.  :0.465

                           NA               NA      NA's :22591     NA's :22591
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-67} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-68} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-69} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-70} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-71} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-72} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-73} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-74} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-75} \end{center}





\FloatBarrier

\newpage


## Year: 2001 


**Days without any CM-21 data:**

2001-03-25 2001-07-29 2001-08-04 2001-08-05 2001-08-06 2001-08-25 2001-08-28 2001-08-29 2001-10-10 2001-10-11 2001-10-12 2001-10-13 2001-10-14 2001-10-21 2001-12-25  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig      -0.3308    2.249

       CM21_sig_sd   -0.0003906   0.5859

  CM21_sig_wo_dark      -0.3274    2.255
----------------------------------------



\normalsize


**Days with outliers:**







2001-10-22


**Days hitting physical limit:**





\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2001-01-01 00:00:30    Min.  : 17.19    Min.  :-0.02    Min.  :0.00

  1st Qu.:2001-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.00   1st Qu.:0.00

  Median :2001-07-02 12:00:00   Median : 89.30   Median : 0.00   Median :0.00

    Mean :2001-07-02 12:00:00     Mean : 89.66     Mean : 0.21     Mean :0.00

  3rd Qu.:2001-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.34   3rd Qu.:0.00

   Max.  :2001-12-31 23:59:30    Max.  :162.81    Max.  : 1.67    Max.  :0.46

                           NA               NA     NA's :44983    NA's :44982
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-76} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-77} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-78} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-79} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-80} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-81} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-82} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-83} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-84} \end{center}





\FloatBarrier

\newpage


## Year: 2002 


**Days without any CM-21 data:**

2002-11-14 2002-11-15 2002-11-16 2002-11-17 2002-12-26  



### Proposed outliers limits 


\footnotesize


---------------------------------------
                an          low     upe
------------------ ------------ -------
          CM21_sig     -0.01692   2.216

       CM21_sig_sd   -3.074e-05   0.593

  CM21_sig_wo_dark     -0.01337   2.221
---------------------------------------



\normalsize


**Days with outliers:**



2002-07-10 2002-08-04 2002-08-07 2002-08-20






**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2002-01-01 00:00:30    Min.  : 17.19    Min.  :-0.031    Min.  :0.000

  1st Qu.:2002-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.003   1st Qu.:0.000

  Median :2002-07-02 12:00:00   Median : 89.29   Median : 0.003   Median :0.000

    Mean :2002-07-02 12:00:00     Mean : 89.66     Mean : 0.207     Mean :0.003

  3rd Qu.:2002-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.338   3rd Qu.:0.001

   Max.  :2002-12-31 23:59:30    Max.  :162.81    Max.  : 1.516    Max.  :0.484

                           NA               NA      NA's :22521     NA's :22521
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-85} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-86} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-87} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-88} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-89} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-90} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-91} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-92} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-93} \end{center}





\FloatBarrier

\newpage


## Year: 2003 


**Days without any CM-21 data:**

2003-08-16 2003-08-17 2003-09-27 2003-09-28  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig     -0.01794    2.266

       CM21_sig_sd   -4.466e-05   0.6006

  CM21_sig_wo_dark     -0.01168    2.272
----------------------------------------



\normalsize


**Days with outliers:**



2003-02-28 2003-03-28 2003-08-09 2003-10-08 2003-11-11






**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2003-01-01 00:00:30    Min.  : 17.19    Min.  :-0.035    Min.  :0.000

  1st Qu.:2003-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.004   1st Qu.:0.000

  Median :2003-07-02 12:00:00   Median : 89.30   Median : 0.002   Median :0.000

    Mean :2003-07-02 12:00:00     Mean : 89.66     Mean : 0.213     Mean :0.003

  3rd Qu.:2003-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.349   3rd Qu.:0.001

   Max.  :2003-12-31 23:59:30    Max.  :162.81    Max.  : 1.518    Max.  :0.453

                           NA               NA      NA's :21787     NA's :21787
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-94} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-95} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-96} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-97} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-98} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-99} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-100} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-101} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-102} \end{center}





\FloatBarrier

\newpage


## Year: 2004 


**Days without any CM-21 data:**

2004-05-13 2004-06-30 2004-07-01 2004-07-04 2004-09-25 2004-09-26 2004-09-27 2004-11-22  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig       -2.068    3.262

       CM21_sig_sd   -3.077e-05   0.5285

  CM21_sig_wo_dark    -0.007605    2.288
----------------------------------------



\normalsize


**Days with outliers:**










**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2004-01-01 00:00:30    Min.  : 17.19    Min.  :-0.012    Min.  :0.000

  1st Qu.:2004-04-01 12:00:15   1st Qu.: 61.90   1st Qu.:-0.002   1st Qu.:0.000

  Median :2004-07-02 00:00:00   Median : 89.33   Median : 0.003   Median :0.000

    Mean :2004-07-02 00:00:00     Mean : 89.71     Mean : 0.243     Mean :0.002

  3rd Qu.:2004-10-01 11:59:45   3rd Qu.:117.60   3rd Qu.: 0.185   3rd Qu.:0.001

   Max.  :2004-12-31 23:59:30    Max.  :162.81    Max.  : 2.830    Max.  :0.525

                           NA               NA      NA's :24076     NA's :24076
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-103} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-104} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-105} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-106} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-107} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-108} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-109} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-110} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-111} \end{center}




### Year: 2004  exceptions 


#### BEWARE!
There is an unexpected +2.5V offset in the recording signal for
            2004-07-03 00:00 until 2004-07-22 00:00.
            We changed the allowed physical signal limits to compensate.
            Have to check dark calculation and the final output for problems.


\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-112} \end{center}



\FloatBarrier

\newpage


## Year: 2005 


**Days without any CM-21 data:**

2005-04-05 2005-04-06 2005-04-07 2005-07-07 2005-08-02 2005-08-03 2005-11-15 2005-11-16 2005-11-17 2005-11-18 2005-11-19 2005-11-20 2005-11-21 2005-11-22 2005-11-23 2005-11-24 2005-11-25 2005-11-26 2005-11-27 2005-11-28 2005-11-29 2005-11-30 2005-12-01 2005-12-02 2005-12-03 2005-12-04 2005-12-05 2005-12-06 2005-12-07 2005-12-08 2005-12-13  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig       -1.721   0.5533

       CM21_sig_sd   -1.584e-05   0.7401

  CM21_sig_wo_dark    -0.003688   0.5534
----------------------------------------



\normalsize


**Days with outliers:**



2005-04-02 2005-04-12

2005-04-02 2005-04-12




**Days hitting physical limit:**



2005-04-02 2005-04-12

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2005-01-01 00:00:30    Min.  : 17.19    Min.  :-7.31    Min.  :0.00

  1st Qu.:2005-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.00   1st Qu.:0.00

  Median :2005-07-02 12:00:00   Median : 89.29   Median : 0.00   Median :0.00

    Mean :2005-07-02 12:00:00     Mean : 89.66     Mean : 0.05     Mean :0.00

  3rd Qu.:2005-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.09   3rd Qu.:0.00

   Max.  :2005-12-31 23:59:30    Max.  :162.81    Max.  : 0.37    Max.  :2.82

                           NA               NA     NA's :54536    NA's :54536
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-113} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-114} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-115} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-116} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-117} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-118} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-119} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-120} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-121} \end{center}




### Year: 2005  exceptions 



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-122} \end{center}



\FloatBarrier

\newpage


## Year: 2006 


**Days without any CM-21 data:**

2006-02-11 2006-02-12 2006-04-29 2006-04-30 2006-05-27 2006-05-28 2006-06-08 2006-06-13 2006-06-14 2006-06-15 2006-08-24 2006-10-07 2006-10-08 2006-10-28 2006-11-12 2006-12-08 2006-12-09 2006-12-10 2006-12-11 2006-12-12 2006-12-26 2006-12-31  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig      -0.7355   0.5477

       CM21_sig_sd   -4.371e-05    0.428

  CM21_sig_wo_dark    -0.004128   0.5509
----------------------------------------



\normalsize


**Days with outliers:**



2006-05-26

2006-05-26




**Days hitting physical limit:**



2006-05-26

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2006-01-01 00:00:30    Min.  : 17.19    Min.  :-3.35    Min.  :0.00

  1st Qu.:2006-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.00   1st Qu.:0.00

  Median :2006-07-02 12:00:00   Median : 89.29   Median : 0.00   Median :0.00

    Mean :2006-07-02 12:00:00     Mean : 89.66     Mean : 0.05     Mean :0.00

  3rd Qu.:2006-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.09   3rd Qu.:0.00

   Max.  :2006-12-31 23:59:30    Max.  :162.81    Max.  : 0.38    Max.  :1.75

                           NA               NA     NA's :53318    NA's :53318
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-123} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-124} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-125} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-126} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-127} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-128} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-129} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-130} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-131} \end{center}





\FloatBarrier

\newpage


## Year: 2007 


**Days without any CM-21 data:**

2007-01-01 2007-01-02 2007-01-03 2007-01-04 2007-01-05 2007-01-06 2007-01-07 2007-01-13 2007-01-14 2007-01-21 2007-01-23 2007-02-18 2007-02-19 2007-02-22 2007-02-25 2007-02-26 2007-02-27 2007-02-28 2007-03-08 2007-05-26 2007-05-27 2007-06-08 2007-06-09 2007-06-10 2007-09-16 2007-09-17 2007-12-02  



### Proposed outliers limits 


\footnotesize


---------------------------------------
                an         low      upe
------------------ ----------- --------
          CM21_sig      -0.401   0.8213

       CM21_sig_sd   -2.02e-05   0.5943

  CM21_sig_wo_dark   -0.002629   0.5794
---------------------------------------



\normalsize


**Days with outliers:**

2007-03-09

2007-03-22

2007-03-22




**Days hitting physical limit:**

2007-03-09

2007-03-22

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2007-01-01 00:00:30    Min.  : 17.19    Min.  :-1.81    Min.  :0.00

  1st Qu.:2007-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.00   1st Qu.:0.00

  Median :2007-07-02 12:00:00   Median : 89.30   Median : 0.00   Median :0.00

    Mean :2007-07-02 12:00:00     Mean : 89.66     Mean : 0.05     Mean :0.00

  3rd Qu.:2007-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.09   3rd Qu.:0.00

   Max.  :2007-12-31 23:59:30    Max.  :162.81    Max.  : 2.50    Max.  :2.55

                           NA               NA     NA's :59197    NA's :59197
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-132} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-133} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-134} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-135} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-136} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-137} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-138} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-139} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-140} \end{center}





\FloatBarrier

\newpage


## Year: 2008 


**Days without any CM-21 data:**

2008-03-06 2008-03-07 2008-03-08 2008-03-09 2008-03-10 2008-08-16 2008-08-17  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig    -0.005651   0.5396

       CM21_sig_sd   -1.034e-05    0.146

  CM21_sig_wo_dark    -0.002661   0.5421
----------------------------------------



\normalsize


**Days with outliers:**



2008-07-01






**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2008-01-01 00:00:30    Min.  : 17.19    Min.  :-0.007    Min.  :0.000

  1st Qu.:2008-04-01 12:00:15   1st Qu.: 61.90   1st Qu.:-0.002   1st Qu.:0.000

  Median :2008-07-02 00:00:00   Median : 89.33   Median :-0.001   Median :0.000

    Mean :2008-07-02 00:00:00     Mean : 89.71     Mean : 0.053     Mean :0.001

  3rd Qu.:2008-10-01 11:59:45   3rd Qu.:117.60   3rd Qu.: 0.089   3rd Qu.:0.000

   Max.  :2008-12-31 23:59:30    Max.  :162.81    Max.  : 0.394    Max.  :0.119

                           NA               NA      NA's :14094     NA's :14094
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-141} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-142} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-143} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-144} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-145} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-146} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-147} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-148} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-149} \end{center}





\FloatBarrier

\newpage


## Year: 2009 


**Days without any CM-21 data:**

2009-06-19 2009-06-20 2009-06-21 2009-07-04 2009-07-05 2009-07-06 2009-07-09 2009-08-09 2009-08-22 2009-08-23 2009-08-25 2009-08-26 2009-09-26 2009-09-27  



### Proposed outliers limits 


\footnotesize


---------------------------------------
                an         low      upe
------------------ ----------- --------
          CM21_sig     -0.5701   0.5529

       CM21_sig_sd   -3.74e-05   0.6176

  CM21_sig_wo_dark    -0.05547   0.5542
---------------------------------------



\normalsize


**Days with outliers:**



2009-07-10

2009-07-10




**Days hitting physical limit:**



2009-07-10

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2009-01-01 00:00:30    Min.  : 17.19    Min.  :-2.61    Min.  :0.00

  1st Qu.:2009-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.00   1st Qu.:0.00

  Median :2009-07-02 12:00:00   Median : 89.29   Median : 0.00   Median :0.00

    Mean :2009-07-02 12:00:00     Mean : 89.66     Mean : 0.05     Mean :0.00

  3rd Qu.:2009-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.08   3rd Qu.:0.00

   Max.  :2009-12-31 23:59:30    Max.  :162.81    Max.  : 0.39    Max.  :2.69

                           NA               NA     NA's :45248    NA's :45248
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-150} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-151} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-152} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-153} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-154} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-155} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-156} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-157} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-158} \end{center}





\FloatBarrier

\newpage


## Year: 2010 


**Days without any CM-21 data:**

2010-01-14 2010-01-30 2010-01-31 2010-02-01 2010-02-02 2010-02-03 2010-02-07 2010-02-12 2010-02-13 2010-02-14 2010-02-15 2010-02-16 2010-03-10 2010-03-11 2010-04-03 2010-04-04 2010-04-05 2010-05-11 2010-05-12 2010-05-13 2010-07-10 2010-07-11 2010-07-16 2010-07-17 2010-07-18 2010-07-24 2010-07-25 2010-07-26 2010-07-27 2010-07-28 2010-07-29 2010-07-30 2010-07-31 2010-08-01 2010-08-03 2010-08-06 2010-08-07 2010-08-08 2010-08-09 2010-08-10 2010-08-11 2010-08-12 2010-08-13 2010-08-14 2010-08-15 2010-08-16 2010-08-17 2010-11-16 2010-11-17 2010-12-03 2010-12-04  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig       -1.208   0.5816

       CM21_sig_sd   -3.137e-05   0.7106

  CM21_sig_wo_dark      -0.0324   0.6273
----------------------------------------



\normalsize


**Days with outliers:**



2010-01-15 2010-07-19

2010-01-15 2010-07-19




**Days hitting physical limit:**



2010-01-15 2010-07-19

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2010-01-01 00:00:30    Min.  : 17.19    Min.  :-4.68     Min.  :0.0

  1st Qu.:2010-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.00    1st Qu.:0.0

  Median :2010-07-02 12:00:00   Median : 89.29   Median : 0.00    Median :0.0

    Mean :2010-07-02 12:00:00     Mean : 89.66     Mean : 0.05      Mean :0.0

  3rd Qu.:2010-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.08    3rd Qu.:0.0

   Max.  :2010-12-31 23:59:30    Max.  :162.81    Max.  : 0.40     Max.  :2.1

                           NA               NA    NA's :103331   NA's :103331
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-159} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-160} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-161} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-162} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-163} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-164} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-165} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-166} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-167} \end{center}





\FloatBarrier

\newpage


## Year: 2011 


**Days without any CM-21 data:**

2011-03-10 2011-10-09 2011-11-20 2011-12-06 2011-12-07 2011-12-08 2011-12-09 2011-12-10 2011-12-11 2011-12-12 2011-12-13 2011-12-14 2011-12-15 2011-12-16 2011-12-17 2011-12-18 2011-12-19 2011-12-20 2011-12-21 2011-12-22 2011-12-23 2011-12-24 2011-12-25 2011-12-26 2011-12-27 2011-12-28 2011-12-29 2011-12-30 2011-12-31  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig       -1.743   0.5627

       CM21_sig_sd   -0.0001124   0.9286

  CM21_sig_wo_dark    -0.006127   0.5669
----------------------------------------



\normalsize


**Days with outliers:**



2011-10-20 2011-10-22 2011-10-31 2011-11-07 2011-12-01

2011-10-20 2011-10-22 2011-10-31 2011-11-07 2011-12-01




**Days hitting physical limit:**



2011-04-18 2011-10-20 2011-10-22 2011-10-31 2011-11-07 2011-12-01

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2011-01-01 00:00:30    Min.  : 17.19    Min.  :-3.54    Min.  :0.00

  1st Qu.:2011-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.00   1st Qu.:0.00

  Median :2011-07-02 12:00:00   Median : 89.30   Median : 0.00   Median :0.00

    Mean :2011-07-02 12:00:00     Mean : 89.66     Mean : 0.05     Mean :0.00

  3rd Qu.:2011-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.09   3rd Qu.:0.00

   Max.  :2011-12-31 23:59:30    Max.  :162.81    Max.  : 0.39    Max.  :1.84

                           NA               NA     NA's :49118    NA's :49118
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-168} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-169} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-170} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-171} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-172} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-173} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-174} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-175} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-176} \end{center}





\FloatBarrier

\newpage


## Year: 2012 


**Days without any CM-21 data:**

2012-01-01 2012-01-02 2012-01-03 2012-01-04 2012-01-05 2012-01-06 2012-01-07 2012-01-08 2012-01-09 2012-01-10 2012-01-11 2012-01-12 2012-01-13 2012-01-14 2012-01-15 2012-01-16 2012-01-17 2012-01-18 2012-01-19 2012-01-20 2012-01-21 2012-01-22 2012-01-23 2012-01-24 2012-01-25 2012-01-26 2012-01-27 2012-01-28 2012-01-29 2012-01-30  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig      -0.6364   0.8233

       CM21_sig_sd   -7.295e-05   0.7035

  CM21_sig_wo_dark      -0.6336   0.8263
----------------------------------------



\normalsize


**Days with outliers:**

2012-09-20

2012-09-20 2012-09-21

2012-09-20 2012-09-21




**Days hitting physical limit:**

2012-09-20

2012-09-20 2012-09-21

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2012-01-01 00:00:30    Min.  : 17.19    Min.  :-2.09    Min.  :0.00

  1st Qu.:2012-04-01 12:00:15   1st Qu.: 61.90   1st Qu.: 0.00   1st Qu.:0.00

  Median :2012-07-02 00:00:00   Median : 89.33   Median : 0.00   Median :0.00

    Mean :2012-07-02 00:00:00     Mean : 89.71     Mean : 0.06     Mean :0.00

  3rd Qu.:2012-10-01 11:59:45   3rd Qu.:117.60   3rd Qu.: 0.10   3rd Qu.:0.00

   Max.  :2012-12-31 23:59:30    Max.  :162.81    Max.  : 2.50    Max.  :2.44

                           NA               NA     NA's :45468    NA's :45468
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-177} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-178} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-179} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-180} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-181} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-182} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-183} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-184} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-185} \end{center}





\FloatBarrier

\newpage


## Year: 2013 


**Days without any CM-21 data:**

2013-01-11 2013-03-21 2013-08-29 2013-10-03 2013-10-04 2013-10-05 2013-10-06 2013-10-07 2013-12-11 2013-12-12  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig      -0.4023   0.5571

       CM21_sig_sd   -6.282e-05   0.3543

  CM21_sig_wo_dark      -0.3993   0.5631
----------------------------------------



\normalsize


**Days with outliers:**



2013-12-10

2013-12-10




**Days hitting physical limit:**



2013-12-10

\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2013-01-01 00:00:30    Min.  : 17.19    Min.  :-1.857    Min.  :0.000

  1st Qu.:2013-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.003   1st Qu.:0.000

  Median :2013-07-02 12:00:00   Median : 89.30   Median :-0.001   Median :0.000

    Mean :2013-07-02 12:00:00     Mean : 89.66     Mean : 0.053     Mean :0.001

  3rd Qu.:2013-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.088   3rd Qu.:0.000

   Max.  :2013-12-31 23:59:30    Max.  :162.81    Max.  : 0.387    Max.  :1.392

                           NA               NA      NA's :18574     NA's :18574
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-186} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-187} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-188} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-189} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-190} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-191} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-192} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-193} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-194} \end{center}





\FloatBarrier

\newpage


## Year: 2014 


**Days without any CM-21 data:**

2014-02-06 2014-02-07 2014-02-08 2014-02-09 2014-03-24 2014-03-25 2014-03-26 2014-03-27 2014-03-28 2014-03-29 2014-03-30 2014-07-16 2014-09-29 2014-09-30 2014-10-01  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig      -0.0474   0.5852

       CM21_sig_sd   -0.0009632   0.2291

  CM21_sig_wo_dark     -0.04627   0.5848
----------------------------------------



\normalsize


**Days with outliers:**



2014-01-14

2014-01-14




**Days hitting physical limit:**



2014-01-14

\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2014-01-01 00:00:30    Min.  : 17.19    Min.  :-0.212    Min.  :0.000

  1st Qu.:2014-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.000   1st Qu.:0.001

  Median :2014-07-02 12:00:00   Median : 89.29   Median : 0.001   Median :0.002

    Mean :2014-07-02 12:00:00     Mean : 89.66     Mean : 0.053     Mean :0.002

  3rd Qu.:2014-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.082   3rd Qu.:0.002

   Max.  :2014-12-31 23:59:30    Max.  :162.81    Max.  : 0.412    Max.  :0.699

                           NA               NA      NA's :25519     NA's :25519
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-195} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-196} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-197} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-198} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-199} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-200} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-201} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-202} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-203} \end{center}





\FloatBarrier

\newpage


## Year: 2015 


**Days without any CM-21 data:**

2015-01-22 2015-05-10 2015-05-22 2015-05-23 2015-05-24 2015-05-25 2015-05-26 2015-05-27 2015-05-28 2015-05-29 2015-05-30 2015-05-31 2015-06-01 2015-06-02 2015-06-03 2015-06-04 2015-06-05 2015-06-06 2015-06-07 2015-06-08 2015-06-09  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig       -1.444   0.5417

       CM21_sig_sd   -0.0006123    0.554

  CM21_sig_wo_dark    -0.002761   0.5665
----------------------------------------



\normalsize


**Days with outliers:**



2015-02-18 2015-02-19 2015-04-17 2015-04-18 2015-04-20 2015-04-21 2015-04-22

2015-02-18 2015-02-19 2015-04-17 2015-04-18 2015-04-20 2015-04-21 2015-04-22




**Days hitting physical limit:**



2015-02-18 2015-02-19 2015-04-17 2015-04-18 2015-04-20 2015-04-21 2015-04-22

\footnotesize


-----------------------------------------------------------------------------
                         Date              SZA        CM21_sig    CM21_sig_sd
----------------------------- ---------------- --------------- --------------
   Min.  :2015-01-01 00:00:30    Min.  : 17.19    Min.  :-2.50    Min.  :0.00

  1st Qu.:2015-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.00   1st Qu.:0.00

  Median :2015-07-02 12:00:00   Median : 89.30   Median : 0.00   Median :0.00

    Mean :2015-07-02 12:00:00     Mean : 89.66     Mean : 0.04     Mean :0.00

  3rd Qu.:2015-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.08   3rd Qu.:0.00

   Max.  :2015-12-31 23:59:30    Max.  :162.81    Max.  : 0.40    Max.  :1.16

                           NA               NA     NA's :32046    NA's :32046
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-204} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-205} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-206} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-207} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-208} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-209} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-210} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-211} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-212} \end{center}




### Year: 2015  exceptions 



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-213} \end{center}



\FloatBarrier

\newpage


## Year: 2016 


**Days without any CM-21 data:**

2016-01-23 2016-01-24  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig    -0.003033   0.8844

       CM21_sig_sd   -0.0003983   0.1624

  CM21_sig_wo_dark    -0.002496   0.5608
----------------------------------------



\normalsize


**Days with outliers:**

2016-04-21

2016-08-11 2016-09-10






**Days hitting physical limit:**

2016-04-21



\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2016-01-01 00:00:30    Min.  : 17.20    Min.  :-0.004    Min.  :0.000

  1st Qu.:2016-04-01 12:00:15   1st Qu.: 61.91   1st Qu.: 0.000   1st Qu.:0.001

  Median :2016-07-02 00:00:00   Median : 89.34   Median : 0.001   Median :0.001

    Mean :2016-07-02 00:00:00     Mean : 89.71     Mean : 0.056     Mean :0.002

  3rd Qu.:2016-10-01 11:59:45   3rd Qu.:117.60   3rd Qu.: 0.094   3rd Qu.:0.001

   Max.  :2016-12-31 23:59:30    Max.  :162.81    Max.  : 2.889    Max.  :0.122

                           NA               NA       NA's :6169      NA's :6169
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-214} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-215} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-216} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-217} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-218} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-219} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-220} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-221} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-222} \end{center}





\FloatBarrier

\newpage


## Year: 2017 


**Days without any CM-21 data:**

2017-08-31 2017-11-09  



### Proposed outliers limits 


\footnotesize


---------------------------------------
                an         low      upe
------------------ ----------- --------
          CM21_sig    -0.00311   0.5552

       CM21_sig_sd   9.269e-05   0.1811

  CM21_sig_wo_dark   -0.001988    0.553
---------------------------------------



\normalsize


**Days with outliers:**





2017-10-30 2017-11-10

2017-05-23 2017-09-13


**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2017-01-01 00:00:30    Min.  : 17.20    Min.  :-0.003    Min.  :0.000

  1st Qu.:2017-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.000   1st Qu.:0.001

  Median :2017-07-02 12:00:00   Median : 89.31   Median : 0.002   Median :0.001

    Mean :2017-07-02 12:00:00     Mean : 89.66     Mean : 0.057     Mean :0.002

  3rd Qu.:2017-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.098   3rd Qu.:0.001

   Max.  :2017-12-31 23:59:30    Max.  :162.81    Max.  : 0.474    Max.  :0.288

                           NA               NA       NA's :6195      NA's :6195
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-223} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-224} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-225} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-226} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-227} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-228} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-229} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-230} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-231} \end{center}





\FloatBarrier

\newpage


## Year: 2018 


**Days without any CM-21 data:**

2018-11-27 2018-11-28 2018-11-29 2018-11-30 2018-12-01 2018-12-02 2018-12-03 2018-12-04 2018-12-05  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig    -0.003725   0.5626

       CM21_sig_sd   -0.0003339    0.207

  CM21_sig_wo_dark    -0.009534   0.5583
----------------------------------------



\normalsize


**Days with outliers:**



2018-07-23

2018-12-07




**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2018-01-01 00:00:30    Min.  : 17.19    Min.  :-0.006    Min.  :0.000

  1st Qu.:2018-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.000   1st Qu.:0.001

  Median :2018-07-02 12:00:00   Median : 89.31   Median : 0.002   Median :0.001

    Mean :2018-07-02 12:00:00     Mean : 89.66     Mean : 0.055     Mean :0.002

  3rd Qu.:2018-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.090   3rd Qu.:0.002

   Max.  :2018-12-31 23:59:30    Max.  :162.81    Max.  : 0.491    Max.  :0.519

                           NA               NA      NA's :14578     NA's :14578
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-232} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-233} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-234} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-235} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-236} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-237} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-238} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-239} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-240} \end{center}





\FloatBarrier

\newpage


## Year: 2019 


**Days without any CM-21 data:**

2019-01-25 2019-01-26 2019-01-27 2019-04-25 2019-04-26 2019-04-27 2019-04-28 2019-04-29 2019-04-30 2019-05-01 2019-05-02 2019-05-03 2019-07-08 2019-08-22  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig    -0.003768   0.5877

       CM21_sig_sd   -0.0003182   0.1638

  CM21_sig_wo_dark    -0.003189   0.5523
----------------------------------------



\normalsize


**Days with outliers:**

2019-05-13

2019-06-25 2019-06-28 2019-07-10






**Days hitting physical limit:**

2019-05-13



\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2019-01-01 00:00:30    Min.  : 17.19    Min.  :-0.005    Min.  :0.000

  1st Qu.:2019-04-02 06:00:15   1st Qu.: 61.84   1st Qu.: 0.000   1st Qu.:0.001

  Median :2019-07-02 12:00:00   Median : 89.31   Median : 0.001   Median :0.001

    Mean :2019-07-02 12:00:00     Mean : 89.66     Mean : 0.056     Mean :0.002

  3rd Qu.:2019-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 0.095   3rd Qu.:0.002

   Max.  :2019-12-31 23:59:30    Max.  :162.81    Max.  : 0.961    Max.  :0.119

                           NA               NA      NA's :24315     NA's :24315
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-241} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-242} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-243} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-244} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-245} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-246} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-247} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-248} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-249} \end{center}





\FloatBarrier

\newpage


## Year: 2020 


**Days without any CM-21 data:**

2020-03-26 2020-04-16 2020-04-17 2020-04-18 2020-04-19 2020-04-20 2020-04-21 2020-04-22 2020-08-22 2020-08-23 2020-08-29 2020-08-30 2020-10-24 2020-10-25 2020-10-26 2020-10-27 2020-10-28  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig    -0.004287   0.5684

       CM21_sig_sd   -0.0004328   0.1729

  CM21_sig_wo_dark    -0.003488   0.5691
----------------------------------------



\normalsize


**Days with outliers:**



2020-06-10






**Days hitting physical limit:**





\footnotesize


-----------------------------------------------------------------------------
                         Date               SZA       CM21_sig    CM21_sig_sd
----------------------------- ----------------- -------------- --------------
   Min.  :2020-01-01 00:00:30     Min.  : 6.512    Min.  :0.00    Min.  :0.00

  1st Qu.:2020-04-01 12:00:15   1st Qu.: 61.907   1st Qu.:0.00   1st Qu.:0.00

  Median :2020-07-02 00:00:00   Median : 89.342   Median :0.00   Median :0.00

    Mean :2020-07-02 00:00:00     Mean : 89.710     Mean :0.06     Mean :0.00

  3rd Qu.:2020-10-01 11:59:45   3rd Qu.:117.598   3rd Qu.:0.10   3rd Qu.:0.00

   Max.  :2020-12-31 23:59:30    Max.  :162.809    Max.  :0.39    Max.  :0.13

                           NA                NA    NA's :32651    NA's :32651
-----------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-250} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-251} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-252} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-253} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-254} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-255} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-256} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-257} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-258} \end{center}





\FloatBarrier

\newpage


## Year: 2021 


**Days without any CM-21 data:**

  



### Proposed outliers limits 


\footnotesize


----------------------------------------
                an          low      upe
------------------ ------------ --------
          CM21_sig    -0.005723   0.5504

       CM21_sig_sd   -9.384e-05   0.1605

  CM21_sig_wo_dark    -0.004362   0.5517
----------------------------------------



\normalsize


**Days with outliers:**



2021-08-18






**Days hitting physical limit:**





\footnotesize


---------------------------------------------------------------------------------
                         Date              SZA          CM21_sig      CM21_sig_sd
----------------------------- ---------------- ----------------- ----------------
   Min.  :2021-01-01 00:00:30    Min.  : 17.19    Min.  :-0.0070    Min.  :0.0005

  1st Qu.:2021-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.0004   1st Qu.:0.0028

  Median :2021-07-02 12:00:00   Median : 89.31   Median : 0.0014   Median :0.0033

    Mean :2021-07-02 12:00:00     Mean : 89.66     Mean : 0.0551     Mean :0.0040

  3rd Qu.:2021-10-01 17:59:45   3rd Qu.:117.53   3rd Qu.: 0.0910   3rd Qu.:0.0039

   Max.  :2021-12-31 23:59:30    Max.  :162.80    Max.  : 0.3727    Max.  :0.1258

                           NA               NA        NA's :2669       NA's :2669
---------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-259} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-260} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-261} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-262} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-263} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-264} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-265} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-266} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-267} \end{center}





\FloatBarrier

\newpage


## Year: 2022 


**Days without any CM-21 data:**

2022-03-22  



### Proposed outliers limits 


\footnotesize


--------------------------------------
                an         low     upe
------------------ ----------- -------
          CM21_sig     -0.6463   8.256

       CM21_sig_sd   -0.000678   1.558

  CM21_sig_wo_dark    -0.01707   8.285
--------------------------------------



\normalsize


**Days with outliers:**



2022-02-28 2022-08-25

2022-04-03 2022-04-19 2022-05-07 2022-05-19




**Days hitting physical limit:**

2022-12-22

2022-02-28 2022-08-25

\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2022-01-01 00:00:30    Min.  : 17.19    Min.  :-2.479    Min.  :0.000

  1st Qu.:2022-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.001   1st Qu.:0.002

  Median :2022-07-02 12:00:00   Median : 89.31   Median : 0.002   Median :0.003

    Mean :2022-07-02 12:00:00     Mean : 89.66     Mean : 0.309     Mean :0.007

  3rd Qu.:2022-10-01 17:59:45   3rd Qu.:117.53   3rd Qu.: 0.140   3rd Qu.:0.003

   Max.  :2022-12-31 23:59:30    Max.  :162.81    Max.  : 4.998    Max.  :1.739

                           NA               NA       NA's :5102      NA's :5102
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-268} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-269} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-270} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-271} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-272} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-273} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-274} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-275} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-276} \end{center}





\FloatBarrier

\newpage


## Year: 2023 


**Days without any CM-21 data:**

2023-03-14 2023-03-16 2023-05-10 2023-05-11  



### Proposed outliers limits 


\footnotesize


-------------------------------------
                an       low      upe
------------------ --------- --------
          CM21_sig   -0.1704   0.5477

       CM21_sig_sd    -1.035    1.128

  CM21_sig_wo_dark   -0.1706   0.5402
-------------------------------------



\normalsize


**Days with outliers:**



2023-04-07

2023-03-14




**Days hitting physical limit:**



2023-04-07

\footnotesize


-------------------------------------------------------------------------------
                         Date              SZA         CM21_sig     CM21_sig_sd
----------------------------- ---------------- ---------------- ---------------
   Min.  :2023-01-01 00:00:30    Min.  : 23.02    Min.  :-0.466    Min.  :0.000

  1st Qu.:2023-02-02 12:00:15   1st Qu.: 63.87   1st Qu.: 0.000   1st Qu.:0.002

  Median :2023-03-07 00:00:00   Median : 93.03   Median : 0.001   Median :0.002

    Mean :2023-03-07 00:00:00     Mean : 93.54     Mean : 0.047     Mean :0.003

  3rd Qu.:2023-04-08 11:59:45   3rd Qu.:123.06   3rd Qu.: 0.078   3rd Qu.:0.003

   Max.  :2023-05-10 23:59:30    Max.  :162.33    Max.  : 0.419    Max.  :3.000

                           NA               NA       NA's :6524      NA's :6523
-------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-277} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-278} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-279} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-280} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-281} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-282} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-283} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-284} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/REPORTS/Inspect_CM21_sig_DIRTY_files/figure-latex/unnamed-chunk-3-285} \end{center}

**END**


```
2023-05-11 03:12:53.8 athan@sagan ~/BBand_LAP/inspect_db/Inspect_CM21_sig.R 8.858543 mins
```

