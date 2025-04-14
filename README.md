# te.test
 A fast implementation in R of the T<sub>e</sub> Test, as described in the paper "[Te Test: A New Non-asymptotic T-test for Behrens-Fisher Problems](https://arxiv.org/abs/2210.16473)".

# Install 
```
install.packages("https://github.com/tascofield/te-test/releases/download/1.0/te.test_1.0.tar.gz") 
library("te.test")
```

# help(te.test) 

<body class="vsc-initialized"><div class="container"><main>



<h2>Te T-test</h2>

<h3>Description</h3>

<p><code>te.test</code> is a fast implementation of the T<sub>e</sub> Test, as described in the paper "Te Test: A New Non-asymptotic T-test for Behrens-Fisher Problems". Can be used as a drop-in replacement for <code>stats::t.test</code>, where appropriate.
</p>


<h3>Usage</h3>

<pre><code class="language-R">te.test(x,...)

##Default S3 method:
te.test(x,y,
        alternative=c("two.sided", "less", "greater"),
        mu = 0, conf.level = 0.95,...)

## S3 method for class 'formula'
te.test(formula, data, subset, na.action = na.pass, ...)
</code></pre>


<h3>Arguments</h3>

<table>
<tbody><tr><td><code id="x">x</code></td>
<td>
<p>a (non-empty) numeric vector of data values.</p>
</td></tr>
<tr><td><code id="y">y</code></td>
<td>
<p>a (non-empty) numeric vector of data values.</p>
</td></tr>
<tr><td><code id="alternative">alternative</code></td>
<td>
<p>a character string specifying the alternative hypothesis, must be one of <code>"two.sided"</code> (default), <code>"greater"</code> or <code>"less"</code>. You can specify just the initial letter.</p>
</td></tr>
<tr><td><code id="mu">mu</code></td>
<td>
<p>a number indicating the true value of the difference in means (i.e. <code>mean(x) - mean(y)</code>).</p>
</td></tr>
<tr><td><code id="conf.level">conf.level</code></td>
<td>
<p>confidence level of the interval.</p>
</td></tr>
<tr><td><code id="formula">formula</code></td>
<td>
<p>a formula of the form <code>lhs ~ rhs</code> where <code>lhs</code> is a numeric variable giving the data values and <code>rhs</code> is a factor with two levels giving the corresponding groups.</p>
</td></tr>
<tr><td><code id="data">data</code></td>
<td>
<p>an optional matrix or data frame (or similar: see <code>model.frame</code>) containing the variables in the formula <code>formula</code>. By default the variables are taken from <code>environment(formula)</code>.</p>
</td></tr>
<tr><td><code id="subset">subset</code></td>
<td>
<p>an optional vector specifying a subset of observations to be used.</p>
</td></tr>
<tr><td><code id="na.action">na.action</code></td>
<td>
<p>a function which indicates what should happen when the data contain <code>NA</code>s.</p>
</td></tr>
</tbody></table>


<h3>Details</h3>

<p><code>te.test</code> should be used only in situations where Welch's T-test would otherwise be appropriate (unequal variances, unequal sample sizes). Its benefit is that it is an <a href="https://en.wikipedia.org/wiki/Exact_test">exact test</a>.
</p>
<p>If the sample sizes of the data are equal, this test has been shown to be equivalent to the paired t-test (though the confidence intervals won't be the same, since they will still have different <code>stderr</code>s).
Therefore, it will also fail in the same circumstances: when the sample sizes are equal, and the pairwise differences between corresponding data are constant (e.g. x=c(1,2,7),y=c(11,12,17)), it will generate an error.
It may also do this in other circumstances (even when sample sizes are not equal) but the likelihood of this should be vanishingly small.
</p>
<p>Unlike the paper, this uses the matrix:
</p>
<image src="https://github.com/user-attachments/assets/440c23f0-73a2-4232-8cba-6fa514063c5b">

</p>
<p>Which has the same required properties as the one in the paper, but is easier to calculate with. <image src="https://github.com/user-attachments/assets/626ab06a-8936-4ebf-ad69-aeff82e3393f"> is defined similarly.
</p>


<h3>Value</h3>

<p>A list with class "htest" containing the following components (just like <code>t.test</code>):
</p>

<ul>
<li> <p><code>statistic</code>: the value of the t-statistic.
</p>
</li>
<li> <p><code>parameter</code>: the degrees of freedom for the t-statistic.
</p>
</li>
<li> <p><code>p.value</code>: the p-value for the test.
</p>
</li>
<li> <p><code>conf.int</code>: a confidence interval for the mean appropriate to the specified alternative hypothesis.
</p>
</li>
<li> <p><code>estimate</code>: the estimated difference in means.
</p>
</li>
<li> <p><code>null.value</code>: the specified hypothesized value of the mean difference.
</p>
</li>
<li> <p><code>stderr</code>: the standard error of the mean difference, always <img src="https://github.com/user-attachments/assets/5242d914-8874-4f2f-95f4-54f5bf632908">
</p>
</li>
<li> <p><code>alternative</code>: a character string describing the alternative hypothesis.
</p>
</li>
<li> <p><code>method</code>: a character string indicating what type of t-test was performed, always <code>"Te Two Sample t-test"</code>
</p>
</li>
<li> <p><code>data.name</code>: a character string giving the names of the data.
</p>
</li></ul>

<h3>Examples</h3>

```

te.test(1:10,7:20)        # P = .0000001456466 (would be 0.00001855282 with t.test)
te.test(1:10,c(7:20,200)) # P = .00009977306   (would be 0.1245135     with t.test)

## Traditional interface
with(mtcars, te.test(mpg[am == 0], mpg[am == 1]))

## Formula interface
te.test(mpg ~ am, data = mtcars)
```

<hr><div style="text-align: center;">[Package <em>te.test</em> version 1.0 Index]</div></main>

</div>
</body>
