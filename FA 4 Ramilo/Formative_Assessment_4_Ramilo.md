Formative Assessment 4
================
Ramilo, Zion John Yousef T.
2024-03-02

1.  A geospatial analysis system has four sensors supplying images. The
    percentage of images supplied by each sensor and the percentage of
    images relevant to a query are shown in the following table.

``` r
head(geospatial_analysis_system)
```

    ##   Sensors P_Images_Supplied P_Relevant_Images
    ## 1       1              0.15              0.50
    ## 2       2              0.20              0.60
    ## 3       3              0.25              0.80
    ## 4       4              0.40              0.85

What is the overall percentage of relevant images?

``` r
p_of_relevant_images = c()
for(i in 1:4){
  intersection <- (geospatial_analysis_system$P_Images_Supplied[i])*(geospatial_analysis_system$P_Relevant_Images[i])
  p_of_relevant_images <- c(p_of_relevant_images,intersection)
}
summed_p_of_relevant_images<-sum(p_of_relevant_images)
print(paste("P(Relevant Images): ",summed_p_of_relevant_images*100,"%"))
```

    ## [1] "P(Relevant Images):  73.5 %"

2.  A fair coin is tossed twice. Let E1 be the event that both tosses
    have the same outcome, that is, E1 = (HH, TT). Let E2 be the event
    that the first toss is a head, that is, E2 = (HH, HT). Let E3 be the
    event that the second toss is a head, that is, E3 = (TH, HH). Show
    that E1, E2, and E3 are pairwise independent but not mutually
    independent.

``` r
head(coinToss_of2)
```

    ##   coin  H  T
    ## 1    H HH TH
    ## 2    T HT TT

Pair Wise Independence E1 = (HH, TT) E2 = (HH, HT) E3 = (TH, HH)

$$
\begin{aligned}
\text{Show that P}(E_1 \cap E_2) = \text{P}(E_1)\times\text{P}(E_2)\\\text{Similarly, }\text{P}(E_2 \cap E_3) = \text{P}(E_2)\times\text{P}(E_3), \text{and},\\
\text{P}(E_1 \cap E_3) = \text{P}(E_1)\times\text{P}(E_3)
\end{aligned}
$$ 

Since E1 = (HH, TT) then it is equivalent to 1/2 similarly for E2 and
E3 wherein individually they have a probability of 1/2. P(E1
intersection E2) is equivalent to 1/4 for both events happening, P(E2
intersection E3) is equivalent to 1/4, P(E1 intersect E3) which is
equivalent to 1/4. 

$$
\begin{aligned}
\text{P}(E_2)=\text{P}(E_2)=\text{P}(E_3) = 1/2, \text{and},\\
\text{P}(E_1 \cap E_2) = \text{P}(E_2 \cap E_3) = \text{P}(E_1 \cap E_3) = 1/4\\
\end{aligned}
$$

Through this computation we can confirm that they are equivalent
rendering E1,E2,and E3 are pairwise independent.

$$
\begin{aligned}
\text{P}(E_1 \cap E_2) = \text{P}(E_1)\times\text{P}(E_2) = 1/4\\
\text{P}(E_2 \cap E_3) = \text{P}(E_2)\times\text{P}(E_3) = 1/4\\
\text{P}(E_1 \cap E_3) = \text{P}(E_1)\times\text{P}(E_3) = 1/4
\end{aligned}
$$

To prove that they are not mutually independent we must show that the
intersection of all events shall not be equivalent to multiplication of
all the individual events. 

$$
\begin{aligned}
\text{P}(E_1 \cap E_2 \cap E_3) \ne \text{P}(E_1)\times\text{P}(E_2)\times\text{P}(E_3)
\end{aligned}
$$

For all events to happen simultaneously we must have an out come of
HH which in this case is equivalent to a probability of 1/4 and if we
multiply them with each other we get 1/8 since individually they are all
equal to 1/2.

$$
\begin{aligned}
\text{P}(E_1 \cap E_2 \cap E_3) = 1/4\\ \text{P}(E_1)\times\text{P}(E_2)\times\text{P}(E_3) = 1/8
\end{aligned}
$$

Since they are not equivalent we can say that they are not mutually
independent.
