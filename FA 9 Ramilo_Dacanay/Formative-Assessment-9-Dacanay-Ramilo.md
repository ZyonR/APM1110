Formative Assessment 9
================
Ramilo, Zion John Yousef and Dacanay, Jordan
2024-05-02

A random variable X is said to have the gamma distribution, or to be
gamma distributed if the density function is:

$$
f(x) = \frac{x^{\alpha-1} e^{-\frac{x}{\beta}}}{\Gamma(\alpha)\beta^\alpha}
$$ 

Note: The Gamma Distribution is used for continuous random variables
hence, we are to use the continuous definition of the expected value
which is defined as follows:

$$
E[x] = \int_0^\infty\ x\cdot f(x) dx
$$

Given this definition let us substitute $f(x)$ onto the integral;
which now equates the expected to the following equation:

$$
E[x] = \int_0^\infty\ x\cdot \frac{x^{\alpha-1} e^{-\frac{x}{\beta}}}{\Gamma(\alpha)\beta^\alpha} dx
$$ Since we are just integrating with respect to X we are able to remove
the constants namely: $\frac{1}{\Gamma(\alpha)}$, in addition, let us
simplify the x variable and combine it with our $\beta$. Constituting to
an equation of: $$
E[x] = \frac{1}{\Gamma(\alpha)}\int_0^\infty\ (\frac{x}{\beta})^{\alpha} \cdot e^{-\frac{x}{\beta}} dx
$$ as we can observe we have $\frac{x}{\beta}$ within the exponential
and the expression next to it enabling a substitution technique; which
shows the following expression: $$
u = \frac{x}{\beta}
$$ $$
du = \frac{1}{\beta} dx
$$ $$
\beta \cdot du = dx
$$ $$ 
u = \frac{(\infty)}{\beta} = \infty
$$ $$ 
u = \frac{(0)}{\beta} = 0
$$ Therefore; $$
E[x] = \frac{\beta}{\Gamma(\alpha)}\int_0^\infty\ u^{\alpha} \cdot e^{-u} du
$$ But we want to show that: $$
E[x] = \alpha \cdot \beta
$$ We must find a way to eliminate ${\Gamma(\alpha)}$ and in order to do
that we must remember the properties of the Gamma Function to which is
defined as the following: $$
\Gamma(\alpha) = \int_0^\infty\ t^{\alpha-1} \cdot e^{-t} dt
$$ and the following property: $$
\Gamma(\alpha+1) = \alpha \cdot \Gamma(\alpha)
$$ Given this property and definition let us now show $\alpha -1$ by
substituting it; $\alpha = \nu-1$ then: $$
E[x] = \frac{\beta}{\Gamma(\alpha)}\int_0^\infty\ u^{\nu-1} \cdot e^{-u} du
$$

$$
E[x] = \frac{\beta}{\Gamma(\alpha)} \cdot \Gamma(\nu)
$$ $$
E[x] = \frac{\beta}{\Gamma(\alpha)} \cdot \Gamma(\alpha + 1)
$$ $$
E[x] = \frac{\beta}{\Gamma(\alpha)} \cdot \alpha \cdot \Gamma(\alpha)
$$ $$
E[x] = \alpha \cdot \beta
$$ $$
\mu = \alpha \cdot \beta
$$ Hence, we have proved that $\mu = \alpha \cdot \beta$.

Q.E.D.

Now let us prove that $\sigma^{2} = \alpha \cdot \beta^{2}$, where we
are able to find the variance through the following expression: $$
Var[x] = E[x^{2}] - E[x]^{2}
$$ We know that E\[x\] is equivalent to $\alpha \cdot \beta$ to which in
this case substituting this value we are to obtain: $$
Var[x] = E[x^{2}] - \alpha^{2}\beta^{2}
$$ We only now need the expression for $E[x^{2}]$. Let us start defining
the expected value as the following: $$
E[x^{2}] = \int_0^\infty\ x^{2}\cdot f(x) dx
$$ To which we can substitute the Probability Density Function
constituting to the following equation: $$
E[x^{2}] = \int_0^\infty\ x^{2}\cdot \frac{x^{\alpha-1} e^{-\frac{x}{\beta}}}{\Gamma(\alpha)\beta^\alpha} dx
$$ Simplifying we get:  
$$
E[x^{2}] = \frac{1}{\Gamma(\alpha)}\int_0^\infty\ x\cdot(\frac{x}{\beta})^{\alpha} \cdot e^{-\frac{x}{\beta}} dx
$$ Applying U-Substitution: $$ 
u = \frac{x}{\beta}
$$ $$
du = \frac{1}{\beta} dx
$$ $$ 
\beta \cdot du = dx
$$ $$
u = \frac{(\infty)}{\beta} = \infty
$$ $$
u = \frac{(0)}{\beta} = 0
$$ $$
u = \frac{x}{\beta} = \beta\cdot u = x
$$ Where we obtain the following:
$$E[x^{2}] = \frac{\beta}{\Gamma(\alpha)}\int_0^\infty\ \beta\cdot u\cdot u^{\alpha} \cdot e^{-u} dx$$
$$
E[x^{2}] = \frac{\beta^{2}}{\Gamma(\alpha)}\int_0^\infty\ u^{\alpha+1} \cdot e^{-u} dx
$$ Extending the property and definition of the Gamma function we have
the following expression where $\alpha + 1 = \nu-1$ to which rewriting
obtains the equation of $\nu = \alpha + 2$. $$
E[x^{2}] = \frac{\beta^{2}}{\Gamma(\alpha)}\int_0^\infty\ u^{\nu-1} \cdot e^{-u} dx
$$ $$
E[x^{2}] = \frac{\beta^{2}}{\Gamma(\alpha)} \Gamma(\nu)
$$

$$
E[x^{2}] = \frac{\beta^{2}}{\Gamma(\alpha)} \Gamma(\alpha + 2)
$$ Where we can extend the property: $$
\Gamma(\alpha + 1) = \alpha\Gamma(\alpha)
$$ $$
\Gamma(\alpha + 2) = (\alpha+1)\Gamma(\alpha+1) = \alpha(\alpha+1)\Gamma(\alpha)
$$ Substituting we get: $$
E[x^{2}] = \beta^{2}\cdot\alpha\cdot(\alpha+1)
$$ $$
E[x^{2}] = \alpha^{2}\cdot\beta^{2}+\alpha\cdot\beta^{2}
$$ Hence; $$
Var[x] = E[x^{2}] - E[x]^{2}
$$

$$
Var[x] = \alpha^{2}\cdot\beta^{2}+\alpha\cdot\beta^{2} - \alpha^{2}\cdot\beta^{2}
$$ $$
Var[x] = \alpha\cdot\beta^{2}
$$

Q.E.D.

Prove that the mean and variance of a binomially distributed random
variable are, respectively $\mu = np$ and $\sigma^{2} = npq$. Given this
distribution applies to discrete distributions we are to define our
expected value according to the following expression: $$
E[x] = \sum_{x=0}^n x\cdot P(X=x)
$$ Where the binomial distribution has the probability mass function of:
$$
P(X=x) = _nC_x \cdot p^{x}\cdot q^{n-x} = \frac{n!}{x!\cdot (n-x)!}\cdot p^{x}\cdot q^{n-x}
$$ Substituting we get: $$
E[x] = \sum_{x=0}^n x\cdot \frac{n!}{x!\cdot (n-x)!}\cdot p^{x}\cdot q^{n-x}
$$ We can cancel 1 x from the x! wherein we get (x-1)! and since we have
decreased the subset of n we are to decrease n and $p^{x}$ as well by 1
in order to equalize the expression; to which we get: $$
E[x] = \sum_{x=0}^n \frac{n\cdot(n-1)!}{(x-1)!\cdot (n-x)!}\cdot p\cdot p^{x-1}\cdot q^{n-x}
$$ Since n and p are just constants we can move them in front of the
summation giving us: $$
E[x] = np \cdot \sum_{x=0}^n \frac{(n-1)!}{(x-1)!\cdot (n-x)!}\cdot p^{x-1}\cdot q^{n-x}
$$ Upon checking if the values for (n-x)! still checks out the modified
version of the expression we are to obtain: $$
[(n-1)-(x-1)]! = (n-1-x+1)! = (n-x)!
$$ Let us substitute (n-1) = y and (x-1) = z giving us: $$
E[x] = np \cdot \sum_{x=0}^n \frac{y!}{z!\cdot (y-z)!}\cdot p^{z}\cdot q^{y-z}
$$ to which this expression expresses the Probability Mass Function if
the Binomial Distribution and one of the Axioms of Probability states
that the sum of all probabilities equates to 1; we can say that
$\sum_{x=0}^n \frac{y!}{z! \cdot (y-z)!}\cdot p^{z}\cdot q^{y-z} = 1$
\Hence; $$
E[x] = np \cdot 1
$$ $$
\mu = np
$$

Q.E.D.

To obtain the Variance of the binomial distribution we are to use the
following expression: $$
Var[x] = E[x^{2}] - E[x]^{2}
$$ Where we know that $E[x] = np$ and substituting we get: $$
Var[x] = E[x^{2}] - n^{2}p^{2}
$$ To find $E[x^{2}]$ we are to apply the following expression: $$
E[x^{2}] = \sum_{x=0}^n x^{2} P(X=x)
$$ $$
E[x^{2}] = \sum_{x=0}^n x^{2}\cdot \frac{n!}{x!\cdot (n-x)!}\cdot p^{x}\cdot q^{n-x}
$$ To work around this summation we are to apply the following equality
$x^{2} = x(x-1)+x$ where we can work around the factorial. $$
E[x^{2}] = \sum_{x=0}^n x(x-1)\cdot \frac{n!}{x(x-1)(x-2)!\cdot (n-x)!}\cdot p^{x}\cdot q^{n-x} + \sum_{x=0}^n x\cdot \frac{n!}{x!\cdot (n-x)!}\cdot p^{x}\cdot q^{n-x}
$$ We know that the last summation equates to the expected value giving
us: $$
E[x^{2}] = \sum_{x=0}^n x(x-1)\cdot \frac{n!}{x(x-1)(x-2)!\cdot (n-x)!}\cdot p^{x}\cdot q^{n-x} + np
$$ Simplifying the expression: $$
E[x^{2}] = \sum_{x=0}^n \frac{n!}{(x-2)!\cdot (n-x)!}\cdot p^{x}\cdot q^{n-x} + np
$$ Since the subset has decreased by 2 let us equalize this through the
n which reflects the population and remove $p^{2}$ to our summation, and
moving them onto the front we get: $$
E[x^{2}] = n(n-1)p^{2}\sum_{x=0}^n \frac{(n-2)!}{(x-2)!\cdot (n-x)!}\cdot p^{x-2}\cdot q^{n-x} + np
$$ We know that
$\sum_{x=0}^n \frac{(n-2)!}{(x-2)!\cdot (n-x)!}\cdot p^{x-2}\cdot q^{n-x} = 1$
from the axioms of Probability, therefore we get: $$
E[x^{2}] = n(n-1)p^{2} + np
$$ Hence; $$
Var[x] = n(n-1)p^{2} + np - n^{2}p^{2}
$$

$$
Var[x] = n^{2}p^{2}-np^{2} + np - n^{2}p^{2}
$$ $$Var[x] = np-np^{2}$$ $$Var[x] = np(1-p)$$ $$\sigma^{2} = npq$$

Q.E.D.
