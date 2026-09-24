For one vote $v$ and one bloc $b$，

the number of legislator who cast different votes are
$n^{yes}_{vb}$、$n^{no}_{vb}$、$n^{abs}_{vb}$，

the number of legialator presenting is
$n^{present}_{vb} = n^{yes}_{vb} + n^{no}_{vb} + n^{abs}_{vb}$，

the number of legislator who actually votes yes or no is $n^{cast}_{vb} = n^{yes}_{vb} + n^{no}_{vb}$

### 1. Percentage of agreement within a bloc: `agree_within_bloc`

Among the legislator in a bloc who presents, the share of legislator who votes with the majority in the bloc, in which majority is measured by modal.

$$\text{agree\_within\_bloc}_{vb}=\frac{\max\left(n^{yes}_{vb},\,n^{no}_{vb},\,n^{abs}_{vb}\right)}{n^{present}_{vb}}\;\in\;\left[\tfrac13,\,1\right]$$


Another measurement choice that only counts yes and no and excluding absention：

$$\text{agree\_within\_bloc\_yn}_{vb}=\frac{\max\left(n^{yes}_{vb},\,n^{no}_{vb}\right)}{n^{cast}_{vb}}\;\in\;\left[\tfrac12,\,1\right]$$


### 2. Percentage of agreement within a coaltion: `agree_within_coalition`

Combining all legislators with `status == "coalition"` on a specific date into a coalition group $C_v$，and the formula is the same with indicator one but examining the cohesion in governing coalition。

$$\text{agree\_within\_coalition}_{v}=\frac{\max\left(n^{yes}_{vC},\,n^{no}_{vC},\,n^{abs}_{vC}\right)}{n^{present}_{vC}}\;\in\;\left[\tfrac13,\,1\right]$$


### 3. Additional variable: `follows_coalition`

Whether a specific blocs' majority stand aligns with the majority stand of the coaltion：

$$\text{follows\_coalition}_{vb}=\left[\,\text{bloc\_majority}_{vb}=\text{coalition\_majority}_{v}\,\right]$$

When either side is a draw, the majority variables `bloc_majority,` `coalition_majority` is NA, so `follows_coalition` is also NA.
