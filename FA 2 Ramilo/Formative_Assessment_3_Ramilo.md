Formative Assessment 3
================
Zion John Yousef T. Ramilo
2024-02-12

## Binary Communication Channel Problem

A binary communication channel carries data as one of two sets of
signals denoted by 0 and 1. Owing to noise, a transmitted 0 is sometimes
received as a 1, and a transmitted 1 is sometimes received as a 0. For a
given channel, it can be assumed that a transmitted 0 is correctly
received with probability 0.95, and a transmitted 1 is correctly
received with probability 0.75. Also, 70% of all messages are
transmitted as a 0. If a signal is sent, determine the probability
that: - a 1 was received; - a 1 was transmitted given than a 1 was
received.

$$
P(\text{received}_0 | \text{transmitted}_0) = 0.95
$$ $$
P(\text{received}_1 | \text{transmitted}_0) = 0.05
$$ $$
P(\text{received}_1 | \text{transmitted}_1) = 0.75
$$ $$
P(\text{received}_0 | \text{transmitted}_1) = 0.25
$$ $$
P(\text{transmitted}_0) = 0.7
$$ $$
P(\text{transmitted}_1) = 0.3
$$

a.) Find P(R1)

$$
P(\text{received}_1) = P(\text{received}_1\,\cap\,\text{transmitted}_1)\,\cup\,P(\text{received}_1\,\cap\,\text{transmitted}_1)
$$ $$
\text{Since they are mutually exclusive we can say that: }
$$ $$
P(\text{received}_1) = P(\text{received}_1\quad\cap\quad\text{transmitted}_1)\quad+\quad P(\text{received}_1\quad\cap\quad\text{transmitted}_1)
$$ $$
\text{Since}\quad P(\text{received}_1\quad\cap\quad\text{transmitted}_1) = P(\text{transmitted}_1) * P(\text{received}_1\quad|\quad\text{transmitted}_1)\quad\text{then}
$$ $$ 
P(\text{received}_1) = P(\text{transmitted}_1) * P(\text{received}_1\quad|\quad\text{transmitted}_1) + P(\text{transmitted}_0) * P(\text{received}_1\quad|\quad\text{transmitted}_0)
$$ $$
\text{Which is equivalent to: }
$$ $$
P(\text{received}_1) = (0.3\quad*\quad0.75)\quad+\quad(0.7\quad*\quad0.05) = 0.26
$$

``` r
received_1 <- (0.3*0.75)+(0.7*0.05)
print(paste("P(R_1) = ",received_1))
```

    ## [1] "P(R_1) =  0.26"

b.) Find P(T1 \| R1)

$$
P(\text{transmitted}_1\quad|\quad\text{received}_1) = \frac{P(\text{transmitted}_1) * P(\text{received}_1\quad|\quad\text{transmitted}_1)}{P(\text{received}_1)}
$$ $$
\text{Which is equivalent to: }
$$ $$
P(\text{transmitted}_1\quad|\quad\text{received}_1) = \frac{0.3 * 0.75}{0.26} \approx 0.87
$$

``` r
transmitted_1_received_1 <- (0.3*0.75)/0.26
print(paste("P(T_1 | R_1) = ",transmitted_1_received_1))
```

    ## [1] "P(T_1 | R_1) =  0.865384615384615"

## Jane, Amy, and Ava

There are three employees working at an IT company: Jane, Amy, and Ava,
doing 10%, 30%, and 60% of the programming, respectively. 8% of Jane’s
work, 5% of Amy’s work, and just 1% of Ava‘s work is in error. What is
the overall percentage of error? If a program is found with an error,
who is the most likely person to have written it?

$$
P(\text{Jane}) = 0.1
$$ $$
P(\text{Amy}) = 0.3
$$ $$
P(\text{Ava}) = 0.6
$$ $$
P(\text{Error} | \text{Jane}) = 0.08
$$ $$
P(\text{Error} | \text{Amy}) = 0.05
$$ $$
P(\text{Error} | \text{Ava}) = 0.01
$$

a.) Find total Error or P(Error)

$$
P(\text{Error}) = P(\text{Error}\cap\text{Jane})\cup P(\text{Error}\cap\text{Amy})\cup P(\text{Error}\cap\text{Ava})
$$ $$
P(\text{Error}) = P(\text{Jane})P(\text{Error}|\text{Jane}) + P(\text{Amy})P(\text{Error}|\text{Amy}) + P(\text{Ava})P(\text{Error}|\text{Ava})
$$ $$
P(\text{Error}) = (0.1*0.08)+(0.3*0.05)+(0.6*0.01) = 0.029 = 2.9\%
$$

``` r
error <- (0.1*0.08)+(0.3*0.05)+(0.6*0.01)
print(paste("Overall error is",error))
```

    ## [1] "Overall error is 0.029"

b.) Find the Maximum P(employee \| Error) $$
P(\text{Jane}|\text{Error}) = \frac{P(\text{Jane})*P(\text{Error}|\text{Jane})}{P(\text{Error})} = \frac{0.1*0.08}{0.029} \approx 0.2759 \approx 27.59\%
$$

$$
P(\text{Amy}|\text{Error}) = \frac{P(\text{Amy})*P(\text{Error}|\text{Amy})}{P(\text{Error})} = \frac{0.3*0.05}{0.029} \approx 0.5172 \approx 51.72\%
$$ $$
P(\text{Ava}|\text{Error}) = \frac{P(\text{Ava})*P(\text{Error}|\text{Ava})}{P(\text{Error})} = \frac{0.6*0.01}{0.029} \approx 0.2069 \approx 20.69\%
$$

``` r
jane_error <- (0.1*0.08) / 0.029
amy_error <- (0.3*0.05) / 0.029
ava_error <- (0.6*0.01) / 0.029

print(paste("Given an Error is caused by Jane:",jane_error))
```

    ## [1] "Given an Error is caused by Jane: 0.275862068965517"

``` r
print(paste("Given an Error is caused by Amy:",amy_error))
```

    ## [1] "Given an Error is caused by Amy: 0.517241379310345"

``` r
print(paste("Given an Error is caused by Ava:",ava_error))
```

    ## [1] "Given an Error is caused by Ava: 0.206896551724138"

Given That Amy has the highest probability out of the 3 it is highly
probable that the error was caused by Amy.
