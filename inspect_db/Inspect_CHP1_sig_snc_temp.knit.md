---
title:         "Inspect raw CHP1 data **SIG** "
author:        "Natsis Athanasios"
institute:     "AUTH"
affiliation:   "Laboratory of Atmospheric Physics"
abstract:      "Inspect raw data from CHP1."
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
    fig_width:        8
    fig_height:       5
  html_document:
    toc:        true
    fig_width:  7.5
    fig_height: 5

date: "2023-03-25"

params:
  CLEAN: TRUE

---

 **SIG**

**Source code: [github.com/thanasisn/BBand_LAP](https://github.com/thanasisn/BBand_LAP)**

**Data display: [thanasisn.netlify.app/3-data_display/](https://thanasisn.netlify.app/3-data_display/)**







## Intro

Produce yearly plots for **CHP-1**.

Shows only **raw data** aspects.

It can use flags to show 'CLEAN'/'DIRTY' data.

For 'CLEAN' data, it removes from view:

- Bad recordings ranges `chp1_bad_data`
- Tracker async cases `Async_tracker`
- Physical recording limits `chp1_signal_lower_limit()` and `chp1_signal_upper_limit()`

Mark outliers for signal and SD with:

**mean(variable) -/+ 4 * sd(variable)**

This is just a report it doesn't alter the data.




\FloatBarrier

\newpage


## Year: 2016 


**Days without any CHP-1 data:**

2016-01-01 2016-01-02 2016-01-03 2016-01-04 2016-01-05 2016-01-06 2016-01-07 2016-01-08 2016-01-09 2016-01-10 2016-01-11 2016-01-12 2016-01-13 2016-01-14 2016-01-15 2016-01-16 2016-01-17 2016-01-18 2016-01-19 2016-01-20 2016-01-21 2016-01-22 2016-01-23 2016-01-24 2016-01-25 2016-01-26 2016-01-27 2016-01-28 2016-01-29 2016-01-30 2016-01-31 2016-02-01 2016-02-02 2016-02-03 2016-02-04 2016-02-05 2016-02-06 2016-02-07 2016-02-08 2016-02-09 2016-02-10 2016-02-11 2016-02-12 2016-02-13 2016-02-14 2016-02-15 2016-02-16 2016-02-17 2016-02-18 2016-02-19 2016-02-20 2016-02-21 2016-02-22 2016-02-23 2016-02-24 2016-02-25 2016-02-26 2016-02-27 2016-02-28 2016-02-29 2016-03-01 2016-03-02 2016-03-03 2016-03-04 2016-03-05 2016-03-06 2016-03-07 2016-03-08 2016-03-09 2016-03-10 2016-03-11 2016-03-12 2016-03-13 2016-03-14 2016-03-15 2016-03-16 2016-03-17 2016-03-18 2016-03-19 2016-03-20 2016-03-21 2016-03-22 2016-03-23 2016-03-24 2016-03-25 2016-03-26 2016-03-27 2016-03-28 2016-03-29 2016-03-30 2016-03-31 2016-04-09 2016-04-10 2016-04-14 2016-07-12 2016-07-13 2016-07-14 2016-07-15 2016-07-16 2016-07-17 2016-07-18  



### Proposed outliers limits 


\footnotesize


---------------------------------
           an         low     upe
------------- ----------- -------
     CHP1_sig      -6.354   14.22

  CHP1_sig_sd   -0.000143   3.264
---------------------------------



\normalsize


**Days with outliers:**

2016-04-21



2016-05-31 2016-08-10




**Days hitting physical limit:**

2016-04-21

2016-04-01 2016-04-08 2016-04-11 2016-04-13 2016-05-25 2016-05-27 2016-05-28 2016-05-29 2016-05-30 2016-05-31 2016-05-16 2016-06-09 2016-06-15 2016-08-10 2016-07-07 2016-07-08 2016-07-09 2016-07-10 2016-07-11 2016-07-19 2016-07-22 2016-07-23 2016-07-24 2016-07-25 2016-07-26 2016-07-27 2016-07-28

\footnotesize


---------------------------------------------------------------------------------------------
                         Date              SZA        CHP1_sig    CHP1_sig_sd   Async_tracker
----------------------------- ---------------- --------------- -------------- ---------------
   Min.  :2016-01-01 00:00:30    Min.  : 17.20    Min.  :-5.00    Min.  :0.00   Mode :logical

  1st Qu.:2016-04-01 12:00:15   1st Qu.: 61.91   1st Qu.: 0.00   1st Qu.:0.00    FALSE:362151

  Median :2016-07-02 00:00:00   Median : 89.34   Median : 0.00   Median :0.00      TRUE :6489

    Mean :2016-07-02 00:00:00     Mean : 89.71     Mean : 0.50     Mean :0.02    NA's :158400

  3rd Qu.:2016-10-01 11:59:45   3rd Qu.:117.60   3rd Qu.: 1.70   3rd Qu.:0.00              NA

   Max.  :2016-12-31 23:59:30    Max.  :162.81    Max.  :37.86    Max.  :3.85              NA

                           NA               NA    NA's :149577   NA's :149577              NA
---------------------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-1} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-2} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-3} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-4} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-5} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-6} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-7} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-8} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-9} \end{center}





\FloatBarrier

\newpage


## Year: 2017 


**Days without any CHP-1 data:**

2017-08-31 2017-11-09  



### Proposed outliers limits 


\footnotesize


---------------------------------
           an         low     upe
------------- ----------- -------
     CHP1_sig   -0.006987    7.27

  CHP1_sig_sd   9.408e-05   2.671
---------------------------------



\normalsize


**Days with outliers:**



2017-12-25 2017-12-17 2017-05-17 2017-05-27 2017-08-15 2017-07-26



2017-05-30 2017-09-13


**Days hitting physical limit:**





\footnotesize


-----------------------------------------------------------------------------------------------
                         Date              SZA         CHP1_sig     CHP1_sig_sd   Async_tracker
----------------------------- ---------------- ---------------- --------------- ---------------
   Min.  :2017-01-01 00:00:30    Min.  : 17.20    Min.  :-0.010    Min.  :0.000   Mode :logical

  1st Qu.:2017-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.001   1st Qu.:0.000    FALSE:511243

  Median :2017-07-02 12:00:00   Median : 89.31   Median : 0.000   Median :0.001      TRUE :7157

    Mean :2017-07-02 12:00:00     Mean : 89.66     Mean : 0.861     Mean :0.018      NA's :7200

  3rd Qu.:2017-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 2.045   3rd Qu.:0.003              NA

   Max.  :2017-12-31 23:59:30    Max.  :162.81    Max.  : 4.043    Max.  :1.962              NA

                           NA               NA       NA's :6238      NA's :6238              NA
-----------------------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-10} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-11} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-12} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-13} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-14} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-15} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-16} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-17} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-18} \end{center}





\FloatBarrier

\newpage


## Year: 2018 


**Days without any CHP-1 data:**

2018-11-28 2018-11-29 2018-11-30 2018-12-01 2018-12-02 2018-12-03 2018-12-04 2018-12-05  



### Proposed outliers limits 


\footnotesize


---------------------------------
           an         low     upe
------------- ----------- -------
     CHP1_sig     -0.5099   7.168

  CHP1_sig_sd   -0.000174   2.738
---------------------------------



\normalsize


**Days with outliers:**



2018-05-18






**Days hitting physical limit:**



2018-05-18

\footnotesize


-----------------------------------------------------------------------------------------------
                         Date              SZA         CHP1_sig     CHP1_sig_sd   Async_tracker
----------------------------- ---------------- ---------------- --------------- ---------------
   Min.  :2018-01-01 00:00:30    Min.  : 17.19    Min.  :-2.368    Min.  :0.000   Mode :logical

  1st Qu.:2018-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.001   1st Qu.:0.001    FALSE:502457

  Median :2018-07-02 12:00:00   Median : 89.31   Median : 0.000   Median :0.001      TRUE :7303

    Mean :2018-07-02 12:00:00     Mean : 89.66     Mean : 0.754     Mean :0.020     NA's :15840

  3rd Qu.:2018-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 1.451   3rd Qu.:0.003              NA

   Max.  :2018-12-31 23:59:30    Max.  :162.81    Max.  : 3.956    Max.  :1.974              NA

                           NA               NA      NA's :13138     NA's :13138              NA
-----------------------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-19} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-20} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-21} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-22} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-23} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-24} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-25} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-26} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-27} \end{center}





\FloatBarrier

\newpage


## Year: 2019 


**Days without any CHP-1 data:**

2019-01-25 2019-01-26 2019-01-27 2019-04-25 2019-04-26 2019-04-27 2019-04-28 2019-04-29 2019-04-30 2019-05-01 2019-05-02 2019-05-03 2019-07-03 2019-07-08 2019-08-22  



### Proposed outliers limits 


\footnotesize


----------------------------------
           an          low     upe
------------- ------------ -------
     CHP1_sig    -0.008862   7.219

  CHP1_sig_sd   -9.318e-05    2.69
----------------------------------



\normalsize


**Days with outliers:**



2019-07-20 2019-06-25 2019-06-26 2019-06-10






**Days hitting physical limit:**





\footnotesize


-----------------------------------------------------------------------------------------------
                         Date              SZA         CHP1_sig     CHP1_sig_sd   Async_tracker
----------------------------- ---------------- ---------------- --------------- ---------------
   Min.  :2019-01-01 00:00:30    Min.  : 17.19    Min.  :-0.014    Min.  :0.000   Mode :logical

  1st Qu.:2019-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.001   1st Qu.:0.001    FALSE:492069

  Median :2019-07-02 12:00:00   Median : 89.31   Median : 0.000   Median :0.001      TRUE :6171

    Mean :2019-07-02 12:00:00     Mean : 89.66     Mean : 0.784     Mean :0.019     NA's :27360

  3rd Qu.:2019-10-01 17:59:45   3rd Qu.:117.56   3rd Qu.: 1.652   3rd Qu.:0.003              NA

   Max.  :2019-12-31 23:59:30    Max.  :162.81    Max.  : 4.021    Max.  :1.840              NA

                           NA               NA      NA's :27575     NA's :27575              NA
-----------------------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-28} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-29} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-30} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-31} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-32} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-33} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-34} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-35} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-36} \end{center}





\FloatBarrier

\newpage


## Year: 2020 


**Days without any CHP-1 data:**

2020-03-26 2020-04-16 2020-04-17 2020-04-18 2020-04-19 2020-04-20 2020-04-21 2020-04-22 2020-08-22 2020-08-23 2020-08-29 2020-08-30 2020-10-24 2020-10-25 2020-10-26 2020-10-27 2020-10-28  



### Proposed outliers limits 


\footnotesize


---------------------------------
           an         low     upe
------------- ----------- -------
     CHP1_sig   -0.008523   7.198

  CHP1_sig_sd   -0.000125   2.765
---------------------------------



\normalsize


**Days with outliers:**



2020-05-26 2020-05-27 2020-06-23 2020-07-05






**Days hitting physical limit:**





\footnotesize


----------------------------------------------------------------------------------------------
                         Date               SZA        CHP1_sig    CHP1_sig_sd   Async_tracker
----------------------------- ----------------- --------------- -------------- ---------------
   Min.  :2020-01-01 00:00:30     Min.  : 6.512    Min.  :-0.01    Min.  :0.00   Mode :logical

  1st Qu.:2020-04-01 12:00:15   1st Qu.: 61.907   1st Qu.: 0.00   1st Qu.:0.00    FALSE:485657

  Median :2020-07-02 00:00:00   Median : 89.342   Median : 0.00   Median :0.00      TRUE :5383

    Mean :2020-07-02 00:00:00     Mean : 89.710     Mean : 0.81     Mean :0.02     NA's :36000

  3rd Qu.:2020-10-01 11:59:45   3rd Qu.:117.598   3rd Qu.: 1.80   3rd Qu.:0.00              NA

   Max.  :2020-12-31 23:59:30    Max.  :162.809    Max.  : 3.98    Max.  :1.80              NA

                           NA                NA     NA's :32651    NA's :32651              NA
----------------------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-37} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-38} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-39} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-40} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-41} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-42} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-43} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-44} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-45} \end{center}





\FloatBarrier

\newpage


## Year: 2021 


**Days without any CHP-1 data:**

  



### Proposed outliers limits 


\footnotesize


---------------------------------
           an         low     upe
------------- ----------- -------
     CHP1_sig   -0.008672   7.185

  CHP1_sig_sd   3.808e-05   2.646
---------------------------------



\normalsize


**Days with outliers:**



2021-06-09 2021-06-10 2021-06-12 2021-07-20 2021-07-24 2021-08-18 2021-08-21






**Days hitting physical limit:**





\footnotesize


-------------------------------------------------------------------------------------------------
                         Date              SZA          CHP1_sig      CHP1_sig_sd   Async_tracker
----------------------------- ---------------- ----------------- ---------------- ---------------
   Min.  :2021-01-01 00:00:30    Min.  : 17.19    Min.  :-0.0118    Min.  :0.0004   Mode :logical

  1st Qu.:2021-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.0006   1st Qu.:0.0014    FALSE:515346

  Median :2021-07-02 12:00:00   Median : 89.31   Median : 0.0003   Median :0.0018      TRUE :5934

    Mean :2021-07-02 12:00:00     Mean : 89.66     Mean : 0.7868     Mean :0.0206      NA's :4320

  3rd Qu.:2021-10-01 17:59:45   3rd Qu.:117.53   3rd Qu.: 1.6548   3rd Qu.:0.0036              NA

   Max.  :2021-12-31 23:59:30    Max.  :162.80    Max.  : 3.9242    Max.  :1.8004              NA

                           NA               NA        NA's :2669       NA's :2669              NA
-------------------------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-46} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-47} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-48} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-49} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-50} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-51} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-52} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-53} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-54} \end{center}





\FloatBarrier

\newpage


## Year: 2022 


**Days without any CHP-1 data:**

2022-03-22  



### Proposed outliers limits 


\footnotesize


---------------------------------
           an         low     upe
------------- ----------- -------
     CHP1_sig      -1.064   7.443

  CHP1_sig_sd   7.239e-05   2.773
---------------------------------



\normalsize


**Days with outliers:**

2022-12-22

2022-02-21



2022-12-22 2022-02-21


**Days hitting physical limit:**

2022-12-22

2022-02-21

\footnotesize


-----------------------------------------------------------------------------------------------
                         Date              SZA         CHP1_sig     CHP1_sig_sd   Async_tracker
----------------------------- ---------------- ---------------- --------------- ---------------
   Min.  :2022-01-01 00:00:30    Min.  : 17.19    Min.  :-5.000    Min.  :0.000   Mode :logical

  1st Qu.:2022-04-02 06:00:15   1st Qu.: 61.84   1st Qu.:-0.001   1st Qu.:0.001    FALSE:518063

  Median :2022-07-02 12:00:00   Median : 89.31   Median : 0.000   Median :0.002      TRUE :6097

    Mean :2022-07-02 12:00:00     Mean : 89.66     Mean : 0.833     Mean :0.018      NA's :1440

  3rd Qu.:2022-10-01 17:59:45   3rd Qu.:117.53   3rd Qu.: 1.902   3rd Qu.:0.003              NA

   Max.  :2022-12-31 23:59:30    Max.  :162.81    Max.  :11.163    Max.  :2.127              NA

                           NA               NA       NA's :5009      NA's :5009              NA
-----------------------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-55} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-56} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-57} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-58} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-59} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-60} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-61} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-62} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-63} \end{center}





\FloatBarrier

\newpage


## Year: 2023 


**Days without any CHP-1 data:**

2023-03-16 2023-03-25  



### Proposed outliers limits 


\footnotesize


--------------------------------
           an       low      upe
------------- --------- --------
     CHP1_sig   -464153   629947

  CHP1_sig_sd   -0.4286    2.458
--------------------------------



\normalsize


**Days with outliers:**

2023-03-14 2023-03-15








**Days hitting physical limit:**

2023-03-14 2023-03-15



\footnotesize


-------------------------------------------------------------------------------------------------
                         Date              SZA           CHP1_sig     CHP1_sig_sd   Async_tracker
----------------------------- ---------------- ------------------ --------------- ---------------
   Min.  :2023-01-01 00:00:30    Min.  : 38.83        Min.  : 0.0    Min.  :0.000   Mode :logical

  1st Qu.:2023-01-22 00:00:15   1st Qu.: 68.60       1st Qu.: 0.0   1st Qu.:0.001    FALSE:119797

  Median :2023-02-12 00:00:00   Median : 98.33       Median : 0.0   Median :0.001      TRUE :1163

    Mean :2023-02-12 00:00:00     Mean :100.02        Mean : 17.7     Mean :0.016              NA

  3rd Qu.:2023-03-04 23:59:45   3rd Qu.:131.14       3rd Qu.: 0.4   3rd Qu.:0.003              NA

   Max.  :2023-03-25 23:59:30    Max.  :162.33   Max.  :1080604.0    Max.  :1.751              NA

                           NA               NA         NA's :7250      NA's :7250              NA
-------------------------------------------------------------------------------------------------



\normalsize



\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-64} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-65} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-66} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-67} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-68} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-69} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-70} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-71} \end{center}





\begin{center}\includegraphics[width=1\linewidth]{/home/athan/BBand_LAP/REPORTS/Inspect_CHP1_sig_snc_temp_CLEAN_files/figure-latex/unnamed-chunk-3-72} \end{center}

**END**


```
2023-03-25 07:22:13.0 athan@tyler ~/BBand_LAP/Inspect_CHP1_sig_snc_temp.R 9.697148 mins
```

