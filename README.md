
<!-- README.md is generated from README.Rmd. Please edit that file -->
TextMiningCrisis
================

------------------------------------------------------------------------

------------------------------------------------------------------------

last update: 30/01/2020

Description
===========

Package containing a set of functions to perform a supervised text mining using a lexicon of economic crisis to observe the profile and intensity of economic crisis in a text document.

Author
======

-   Manuel Betin
-   Umberto Collodel

current version:
================

1.3.0

see NEWS.md for more details on new features

\# usage

key functions are

-   pdf\_from\_url()
-   aggregate\_corpus()
-   tf()
-   idf()
-   run\_tf()
-   run\_tf\_by\_chunk()
-   run\_tf\_update()

depreciate functions:

-   plot\_cos\_sim() =&gt; cosim\_fig()
-   key\_word\_crisis() =&gt; lexicon()
-   typology\_categories() =&gt; lexicon\_typology()
-   find\_associated\_keywords() =&gt; lexicon\_details()
-   country\_radar\_dt() =&gt; radar\_dt()
-   country\_radar\_fig() =&gt; radar\_shocks\_fig()
-   find\_pages() =&gt; get\_pages()

\# example

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(rio)
library(DT)
library(TextMiningCrisis)
#> Warning: replacing previous import 'plotly::last_plot' by
#> 'ggplot2::last_plot' when loading 'TextMiningCrisis'
#> Warning: replacing previous import 'plotly::export' by 'rio::export' when
#> loading 'TextMiningCrisis'
#> Warning: replacing previous import 'dplyr::intersect' by
#> 'lubridate::intersect' when loading 'TextMiningCrisis'
#> Warning: replacing previous import 'dplyr::union' by 'lubridate::union'
#> when loading 'TextMiningCrisis'
#> Warning: replacing previous import 'dplyr::setdiff' by 'lubridate::setdiff'
#> when loading 'TextMiningCrisis'
#> Warning: replacing previous import 'lubridate::intersect' by
#> 'dplyr::intersect' when loading 'TextMiningCrisis'
#> Warning: replacing previous import 'lubridate::union' by 'dplyr::union'
#> when loading 'TextMiningCrisis'
#> Warning: replacing previous import 'lubridate::setdiff' by 'dplyr::setdiff'
#> when loading 'TextMiningCrisis'
```

Load the data containing the urls

``` r

set.seed(2)
data("IMF_docs_urls")

url_links=IMF_docs_urls %>%
  mutate(name_file=paste0(ID,"_",period,"_",type_doc_programs))

url_links= url_links %>% filter(ID=="ARG")
url_links=url_links[150:155,]

url_links[,1:5]
#> # A tibble: 6 x 5
#>   ID    period     title                  hierarchy   pdf                  
#>   <chr> <date>     <chr>                  <chr>       <chr>                
#> 1 ARG   1984-12-28 argentina - exchange … <NA>        https://imfbox.box.c…
#> 2 ARG   1984-12-28 argentina - stand-by … <NA>        https://imfbox.box.c…
#> 3 ARG   1984-12-28 argentina - stand-by … EBM/84/191… https://imfbox.box.c…
#> 4 ARG   1984-12-28 argentina - request f… EBM/84/190… https://imfbox.box.c…
#> 5 ARG   1984-12-28 argentina - stand-by … PR/84/43    https://imfbox.box.c…
#> 6 ARG   1985-06-11 argentina - letter on… EBS/85/148  https://imfbox.box.c…
```

Download the files and store in folter "mydocs\_to\_textmining"

``` r

pdf_from_url(url_links,"mydocs_for_textmining")
#> ARG_1984-12-28_exchange system: succesfully downloadedARG_1984-12-28_exchange system : 1/6: 3.094 sec elapsed
#> ARG_1984-12-28_request: succesfully downloadedARG_1984-12-28_request : 2/6: 3.151 sec elapsed
#> ARG_1984-12-28_purchase transac: succesfully downloadedARG_1984-12-28_purchase transac : 3/6: 3.727 sec elapsed
#> ARG_1984-12-28_request: succesfully downloadedARG_1984-12-28_request : 4/6: 4.25 sec elapsed
#> ARG_1984-12-28_request: succesfully downloadedARG_1984-12-28_request : 5/6: 3.722 sec elapsed
#> ARG_1985-06-11_request: succesfully downloadedARG_1985-06-11_request : 6/6: 3.833 sec elapsed
#> urls succesfully downloaded in 'mydocs_for_textmining
#> '
```

Aggregate content of pdfs in folder "mydocs\_to\_textmining" into a single corpus

``` r

corpus=aggregate_corpus("mydocs_for_textmining",only_files = T)
#> Warning in stri_replace_all_regex(string, pattern,
#> fix_replacement(replacement), : argument is not an atomic vector; coercing
#> [1] "mydocs_for_textmining/ARG_1984-12-28_exchange system.pdf"
#> 1/4 ARG_1984-12-28_exchange system: 0.021 sec elapsed
#> Warning in stri_replace_all_regex(string, pattern,
#> fix_replacement(replacement), : argument is not an atomic vector; coercing
#> [1] "mydocs_for_textmining/ARG_1984-12-28_purchase transac.pdf"
#> 2/4 ARG_1984-12-28_purchase transac: 0.037 sec elapsed
#> Warning in stri_replace_all_regex(string, pattern,
#> fix_replacement(replacement), : argument is not an atomic vector; coercing
#> [1] "mydocs_for_textmining/ARG_1984-12-28_request.pdf"
#> 3/4 ARG_1984-12-28_request: 0.04 sec elapsed
#> Warning in stri_replace_all_regex(string, pattern,
#> fix_replacement(replacement), : argument is not an atomic vector; coercing
#> [1] "mydocs_for_textmining/ARG_1985-06-11_request.pdf"
#> 4/4 ARG_1985-06-11_request: 0.274 sec elapsed
save(corpus,file="mycorpus.RData")
```

use the lexicon

``` r
lexicon()["Severe_recession"]
#> $Severe_recession
#>  [1] "severe economic crisis"                 
#>  [2] "very difficult economic circumstances"  
#>  [3] "Severe recession"                       
#>  [4] "severe crisis"                          
#>  [5] "economic crisis"                        
#>  [6] "steep recession"                        
#>  [7] "strong recessionary headwinds"          
#>  [8] "sharp slowdown"                         
#>  [9] "sharp declines in output"               
#> [10] "significant loss of output"             
#> [11] "economic collapse"                      
#> [12] "deeper recession"                       
#> [13] "deepening recession"                    
#> [14] "painful recession"                      
#> [15] "prolonged recession"                    
#> [16] "lengthening recession"                  
#> [17] "severity of the recession"              
#> [18] "economic recession"                     
#> [19] "sharp contraction of economic activity" 
#> [20] "strong contraction of economic activity"
#> [21] "large contraction of economic activity" 
#> [22] "deep recession"                         
#> [23] "large economic slowdown"                
#> [24] "severe recession"                       
#> [25] "profond recession"                      
#> [26] "contraction in output"                  
#> [27] "deep recession"                         
#> [28] "severe contraction"                     
#> [29] "deep contraction"                       
#> [30] "profond contraction"                    
#> [31] "large decline in income per capita"     
#> [32] "deep economic downturn"                 
#> [33] "severe economic downturn"               
#> [34] "deep economic downturn"
```

``` r
lexicon_typology()
#>                  variable                    type           nature_shock
#> 1            Deregulation      adjustment_program    Economic_ajustement
#> 2           Reform_agenda      adjustment_program    Economic_ajustement
#> 3           Trade_reforms      adjustment_program    Economic_ajustement
#> 4       Financial_reforms      adjustment_program    Economic_ajustement
#> 5    Labor_market_reforms      adjustment_program    Economic_ajustement
#> 6             Tax_reforms      adjustment_program    Economic_ajustement
#> 7         Banking_reforms      adjustment_program    Economic_ajustement
#> 8    Fiscal_consolidation      adjustment_program    Economic_ajustement
#> 9    Fiscal_consolidation      adjustment_program    Economic_ajustement
#> 10     Success_of_reforms      adjustment_program    Economic_ajustement
#> 11  Performance_criterion characteristics_program    Economic_ajustement
#> 12      Program_extension characteristics_program    Economic_ajustement
#> 13       Official_support characteristics_program    Economic_ajustement
#> 14   Technical_assistance characteristics_program    Economic_ajustement
#> 15 Precautionary_programs characteristics_program    Economic_ajustement
#> 16         Banking_crisis          economic_shock  Other_Financial_shock
#> 17       Financial_crisis          economic_shock  Other_Financial_shock
#> 18       Inflation_crisis          economic_shock             Real_shock
#> 19           Trade_crisis          economic_shock             Real_shock
#> 20           Trade_crisis          economic_shock             Real_shock
#> 21         World_outcomes          economic_shock  Other_Financial_shock
#> 22         World_outcomes          economic_shock  Other_Financial_shock
#> 23              Contagion          economic_shock  Other_Financial_shock
#> 24              Contagion          economic_shock  Other_Financial_shock
#> 25           Expectations          economic_shock  Other_Financial_shock
#> 26           Expectations          economic_shock  Other_Financial_shock
#> 27 Balance_payment_crisis          economic_shock  Other_Financial_shock
#> 28     Reduction_reserves          economic_shock  Other_Financial_shock
#> 29        Currency_crisis          economic_shock  Other_Financial_shock
#> 30 Currency_crisis_severe          economic_shock  Other_Financial_shock
#> 31       Severe_recession          economic_shock             Real_shock
#> 32       Severe_recession          economic_shock             Real_shock
#> 33         Soft_recession          economic_shock             Real_shock
#> 34         Soft_recession          economic_shock             Real_shock
#> 35              Expansion          economic_shock             Real_shock
#> 36                   Wars      non_economic_shock             Real_shock
#> 37       Natural_disaster      non_economic_shock             Real_shock
#> 38       Commodity_crisis      non_economic_shock             Real_shock
#> 39       Political_crisis      non_economic_shock             Real_shock
#> 40          Social_crisis      non_economic_shock             Real_shock
#> 41        Fiscal_outcomes           debt_outcomes Fiscal_financial_shock
#> 42      Sovereign_default           debt_outcomes Fiscal_financial_shock
#> 43   Concessional_lending          debt_structure Fiscal_financial_shock
#> 44        Short_term_debt          debt_structure Fiscal_financial_shock
#> 45     Floating_rate_debt          debt_structure Fiscal_financial_shock
#> 46           Foreign_debt          debt_structure Fiscal_financial_shock
#> 47           Track_record          debt_structure                   <NA>
#>               domain           channel
#> 1  Structural_policy  Adjustment_shock
#> 2  Structural_policy  Adjustment_shock
#> 3  Structural_policy  Adjustment_shock
#> 4  Structural_policy  Adjustment_shock
#> 5  Structural_policy  Adjustment_shock
#> 6  Structural_policy  Adjustment_shock
#> 7  Structural_policy  Adjustment_shock
#> 8      Fiscal_policy  Adjustment_shock
#> 9  Structural_policy  Adjustment_shock
#> 10 Structural_policy  Adjustment_shock
#> 11 Structural_policy  Adjustment_shock
#> 12 Structural_policy  Adjustment_shock
#> 13 Structural_policy     Revenue_shock
#> 14 Structural_policy  Adjustment_shock
#> 15  forward_guidance  Adjustment_shock
#> 16     Fiscal_policy Expenditure_shock
#> 17     Fiscal_policy Expenditure_shock
#> 18   Monetary_policy     Revenue_shock
#> 19     Fiscal_policy     Revenue_shock
#> 20 Structural_policy     Revenue_shock
#> 21  forward_guidance Expenditure_shock
#> 22  forward_guidance         risk_free
#> 23  forward_guidance Expenditure_shock
#> 24  forward_guidance      Spread_shock
#> 25  forward_guidance Expenditure_shock
#> 26  forward_guidance      Spread_shock
#> 27   Monetary_policy     Revenue_shock
#> 28   Monetary_policy     Revenue_shock
#> 29   Monetary_policy     Revenue_shock
#> 30   Monetary_policy     Revenue_shock
#> 31     Fiscal_policy      Output_shock
#> 32     Fiscal_policy      Output_shock
#> 33     Fiscal_policy      Output_shock
#> 34     Fiscal_policy      Output_shock
#> 35     Fiscal_policy      Output_shock
#> 36     Fiscal_policy     Revenue_shock
#> 37     Fiscal_policy     Revenue_shock
#> 38     Fiscal_policy     Revenue_shock
#> 39     Fiscal_policy  Adjustment_shock
#> 40     Fiscal_policy  Adjustment_shock
#> 41     Fiscal_policy        Debt_shock
#> 42     Fiscal_policy        Debt_shock
#> 43     Fiscal_policy        Debt_shock
#> 44     Fiscal_policy        Debt_shock
#> 45     Fiscal_policy        Debt_shock
#> 46     Fiscal_policy        Debt_shock
#> 47              <NA>              <NA>
```

Find the number of occurence of the word "cotton" by paragraph

``` r
doc_example=corpus[4]
Number_pages_containing_word=eval_pages(doc_example,"debt")
Number_pages_containing_word
#>                                debt
#> ARG_1985-06-11_request 0.0002830189
```

Find the paragraphs containing the word "cotton" by paragraph

``` r

pages_containing_word=get_pages(doc_example$`ARG_1985-06-11_request`,"debt")
pages_containing_word
#> $target
#> [1] "debt"
#> 
#> $N.chars
#> [1] 53000
#> 
#> $N.Occurence
#> $N.Occurence[[1]]
#> [1] "Found in page 2 :3 times"
#> 
#> $N.Occurence[[2]]
#> [1] "Found in page 5 :1 times"
#> 
#> $N.Occurence[[3]]
#> [1] "Found in page 7 :1 times"
#> 
#> $N.Occurence[[4]]
#> [1] "Found in page 8 :2 times"
#> 
#> $N.Occurence[[5]]
#> [1] "Found in page 9 :2 times"
#> 
#> $N.Occurence[[6]]
#> [1] "Found in page 13 :1 times"
#> 
#> $N.Occurence[[7]]
#> [1] "Found in page 14 :1 times"
#> 
#> $N.Occurence[[8]]
#> [1] "Found in page 15 :1 times"
#> 
#> $N.Occurence[[9]]
#> [1] "Found in page 21 :3 times"
#> 
#> 
#> $Tot.occurence
#> [1] 15
#> 
#> $pages
#> $pages[[1]]
#> [1] "c( June 11, 1985, Mr. Jacques de Larosiere, Managing Director, International Monetary Fund, Washington, D.C. 20431, Dear Mr. de Larosiere,, 1. Argentina is facing a deep economic crisis related to both short-, term problems and obstacles to growth stemming from low investment and, from the heavy burden of interest payments on the foreign debt. Struc-, tural rigidities and short-term imbalances have not permitted the country, to escape from its long record of economic stagnation and high inflation., Indeed, during the past ten years the annual rate of inflation averaged, well over 200 percent, while output remained virtually unchanged. The, challenge confronting the Constitutional Government requires firm and, decisive action. It is necessary to break the momentum of inflation to, restore a growing economy and a lasting improvement in living standards., All the economic and social sectors of the country are asked to contribute, to this task, all participating in an equitable manner and in accordance, with their means. Achievement of these objectives requires a drastic, cutback in the fiscal deficit through a reduction of public spending and, the generation of new revenue. Such action would permit a lowering of, the inflation tax, which is particularly burdensome on lower income, groups. A substantial reduction in the fiscal deficit will facilitate, the pursuit of an anti-inflationary monetary policy compatible with an, expansion of credit to the private sector at reasonable levels of, interest rates. On the external front, the combination of an adequate, exchange rate and appropriate institutional arrangements would promote, export growth as a means of reconciling economic reactivation and, external equilibrium. Increased domestic savings and exports will make, it possible to service the external debt at the same time that the eco-, nomy is recovering. The Government is convinced that this approach is, the only one that can help Argentina to reverse the process of economic, deterioration of recent years., 2. In September 1984, the Government adopted an economic program which, sought a substantial reduction of inflation, a sustainable balance of, payments position, and corrective adjustments in relative prices, to, lay the basis for sustained growth of output and employment. This pro-, gram is supported by a stand-by arrangement from the Fund, in an amount, of SDR 1,419 million, which runs through March 27, 1986. The program, includes, inter alia, a sizable reduction in the fiscal deficit and a, tightening of monetary policy, a slowing in the rate of increase of, production costs, a liberalisation of price controls, a downward ad-, justment in the real exchange value of the peso, a reduction of res-, trictions on trade and payments for current international transactions,, and the normalization of Argentina's external debt situation., 3. Significant progress in several of the above-mentioned areas was, made in the last quarter of 1984. Monetary and credit policy was, tightened and interest rates generally became positive in real terms,)"
#> 
#> $pages[[2]]
#> [1] "c( -4-, cutback of spending by these segments of the public sector, as there, was an increase in their floatFng debt and a buildup in unpaid bills., In addition, at the end of 1984 there were delays in the payment of, public sector wages, including in the National Administration. Partly, because of reductions in the backlog of unpaid obligations, the cash, deficit of the nonfinancial public sector increased--counter to the, usual seasonal pattern--to more than 9 percent of GDP in the first, quarter of 1985., 11. In an effort to strengthen the fiscal position in 1985, and partic-, ularly in the second half of the year, in recent weeks the Government, has re-examined the budget that had been submitted to the Congress early, in the year and is about to introduce a revised budget proposal incor-, porating reductions in budgetary credits of the order of 12 percent in, relation to the original proposal. Including the effect of these reduc-, tions and several measures to raise revenue (described below), the, deficit of the nonfinancial public sector on a budget basis is estimated, to decline from 12 314 percent of GDP in 1984 to less than 6 percent of, GDP in 1985. Relative to GDP, expenditure would decline from 34 l/2 per-, cent to less than 31 percent from 1984 to 1985, while revenue would, increase from 1es.s than 22 percent of GDP to nearly 25 percent., 12. Consistent with the fiscal estimates on a budget basis just men-, tioned and the effects of additional measures taken in recent weeks (in, particular, real increases in prices charged by public enterprises),, the cash deficit of the nonfinancial public sector would decline from, about 8 l/2 percent of GDP in 1984 to less than 4 percent of GDP in, 1985. On a semiannual basis, the decline would be from about 8 percent, of GDP in the second half of 1984 to less than 2 percent of GDP in the, second half of 1985. Cash expenditure would decline from 30 l/4 percent, of GDP in the second half of 1984 to 27 3/4 percent of GDP in the second, half of 1985. The decline in expenditure is more marked if adjustment, is made for salary and other payments deferred from the second half of, IL984 into 1985 and for the effect of the employer social security con-, tribution introduced in October 1984. The Government is taking steps, to ensure that the effort to restrain cash spending is reflected fully, in cuts in actual expenditure and not merely in a postponement of pay-, ments. Also, the fiscal plan includes a substantial increase in public, sector revenue in the second half of 1985. Achievement of this objec-, tive requires in part legislation that already has been introduced or, is expected to be submitted shortly to the National Congress; to the, extent that these legislative proposals were not enacted, the Government, would submit alternative legislation or would take administrative action, with an equivalent deficit-reducing effect. A summary of the budget, and cash operations of the public sector 1n terms of GDP, including, projections for the second half of 1985 and the first quarter of 1986,, is presented in Table 1., 13. Wage moderation and a hiring freeze in the public sector are ex-, pected to play an important role in the underlying improvement in the, public finances. In late May 1985 the Government decreed a freeze on)"
#> 
#> $pages[[3]]
#> [1] "c( -6-, facilitate legal action against offenders. In this respect, it is, proposed that the tax office be allowed access to certain information, on transactions through banks and the stock exchange that at present is, protected by the bank secrecy laws. The tax package also seeks to, enhance investment incentives by reducing corporate tax rates and the, value-added tax on specified investment goods., 17. A substantial1 improvement in the public finances is expected to, come from a reduction in the operating losses of the public enterprises., Recently, there have been sizable real increases in the prices charged, for goods and services marketed by the enterprises, and the Government, intends to maintain the present level of these prices in real terms., On the basis of this policy, and a program of spending restraint, it is, expected that the operating losses of the enterprises, which amounted, to 2 percent of GDP in the second half of 1984, will be eliminated in, the second half of 1985. Largely owing to adjustments implemented in, May-June 1985, the effective real level of prices charged by the enter-, prises (net of taxes) in the second half of this year would exceed, on, average, that in the second half of 1984 by 50 percent for liquid fuels,, 34 percent for postal services, 26 percent for natural gas, 25 per-, cent for electricity, 20 percent for telephones, and 18 percent for, railway and air transportation. The overall increase in real prices, between those two periods is estimated at 33 percent, and the value of, sales by the enterprises (net of intra-public sector transactions) is, projected to rise by the equivalent of 2 percent of GDP. Moreover,, spending on goods and nonpersonnel services by the enterprises is to be, strictly curtailed. A plan to eliminate the backlog of unpaid bills in, the second and third quarters of 1985, which already has been put into, effect, is expected to help lower the prices paid by the enterprises, for their inputs as well as their domestic interest costs. In summary,, the overall cash deficit of the enterprises is expected to fall from, almost 4 percent of GDP in the second half of 1984 to around 2 percent, of GDP in the second half of 1985., '18 . To ensure that the program of spending restraint in the public, enterprises is implemented, the Government intends to step up its, monitoring of the enterprises on a current basis through the Public, Enterprises Comptroller's Office; this task will be facilitated by the, requirement established in the 1984 budget that starting in 1985 the, budgets of the public enterprises be subject to approval by Presidential, decree. The Government also will ensure that its wage policy is imple-, mented in the enterprises through the careful screening of proposed, wage increases by the Public Sector Salary Commission. Finally, the, Government is to turn over to the enterprises responsibility for the, payment of an increasing proportion of the interest on their external, debt; this propo-rtion is expected to rise to about 70 percent in 1985, from less than 30 percent in 1984. To ensure compliance with this, objective, a mechanism is being implemented under which enterprises will, make monthly contributions, in accordance with a pre-established schedule,, to an account in the Central Bank to meet foreign interest payments. The, extension of this deposit mechanism to other external obligations of, the enterprises is under active consideration. )"
#> 
#> $pages[[4]]
#> [1] "c( -7-, 19. The public finances had been affected by the elimination of the, 15 percent wage bill tax for social security in 1981 and its replacement, by direct transfers to the social security system from general tax, revenue. As of the fourth quarter of 1984, the drain on general rev-, enues was reduced by one half owing to the restoration of an employer's, contribution equivalent to 7 l/2 percent of wages. In recent years the, social security system has had a deficit, over and above the transfers, just described; this deficit (equivalent to more than l/2 percent of, GDP in 1984) was covered by the Central Bank, either directly or by, allowing bank advances to the system to be counted as part of banks', required reserves. The reform of the financial system in April, (described below) necessitated a change in the financing arrangements, for the social security system, with the Treasury taking over the debt, of the system with banks. For the period ahead, the Government is, committed to the elimination of the system’s deficit. The Government, will seek to maintain the consistency of the monthly adjustments in, benefits with its wage policy, while ensuring the achievement of the, objective as regards the system's finances., 20. As regards the financial arrangements with the provinces, the, intention of the Government is to replace the system that expired at, the end of 1984, which allowed for direct transfers from the Treasury, in addition to the provinces' sharing in most federal taxes, by a, system in which federal funding would take the form of revenue sharing., The greater automaticity of the proposed arrangement would be expected, to enhance budgetary management in the provinces. It has not yet been, possible, however, to implement the new system, and for 1985 it has, been decided basically to maintain the previous one. The fiscal plan, for the current year has been prepared on the basis of federal funding, of the provinces under general revenue sharing and direct transfers,, taken together, equivalent to about 4 percent of GDP, a little less, than in 1984. However, the large increase in taxes that are partly, earmarked for investment purposes under special sharing arrangements, implies that the provinces would receive total funding from the National, Government about l/4 percent of GDP higher in 1985 than in the preceding, year. The projection of the provincial finances assumes that capital, outlays will increase on the basis of higher earmarked transfers, whereas, their current expenditure would decline in real terms, which would be, consistent with the provinces applying spending policies broadly in, line with those instituted in the National Administration., 21. The Central Bank's assumption in 1982 and 1983 of all public sector, debt, on which the public sector now pays virtually no interest, trans-, ferred to the Bank a sizable imbalance of a fiscal nature, and the, problem was compounded by the extension of interest subsidies to the, private sector. In the second half of 1984 the operating deficit of, the Central Bank rose to an estimated 2 l/2 percent of GDP, and is, expected to rise to about 5 percent of GDP in 1985 notwithstanding the, virtual elimination of interest subsidies; some 80 percent of these, losses correspond to the inflation adjustment component on the resources, the Central Bank borrows at market rates to finance the Government. The)"
#> 
#> $pages[[5]]
#> [1] "c( -8-, losses of the Central Bank, together with the cash deficit of the non-, financial public sector (described above), would result in a combined pub-, lic sector deficit of no more than 6.2 percent of GDP in the second half, of 1985 and no more than 2.6 percent of GDP in the first quarter of 1986., 22. Consistent with the objectives of reducing the deficit of the pub-, lic sector and curbing public spending, the Government has set limits, in current pesos,, presented in Table 2, for the combined deficit of the, nonfinancial publlic sector and the Central Bank; a sublimit on the cash, deficit of the nonfinancial public sector; and a limit on treasury out-, Ilays (excluding interest payments). Following the normalization of, public sector payments to the private sector in recent months, the, Government will avoid the incurrence of new domestic arrears and buildup, of floating debt, and the limits on the fiscal deficit have been set on, this basis. The behavior of domestic arrears and floating debt will be, assessed during the October 1985 review of the program to evaluate, fiscal policy; this evaluation will be facilitated by the timely estab-, lishment of mechanisms to monitor these kinds of debt, both in the, National Administration and the public enterprises. In setting the, limits on treasury outlays, it: has been assumed that there will be no, significant changes in the present arrangements regarding the financing, Iof the rest of the public sector; if such arrangements were modified,, the effect of the changes would be taken into account in estimating, treasury outlays. Changes in outstanding treasury drafts (libramientos, impagos) are included in the definition of treasury outlays. The limit, on treasury outlays and the inflation adjustment component of interest, costs included in the central bank losses and in the nonfinancial public, sector deficit will be adjusted as explained in Table 2, if the actual, rate of inflation differs from the projected path., 23. The fiscal lprogram just described, in combination with the balance, of payments objectives set out below, should make it possible to slow, monetary growth without unduly squeezing the private sector. Following, a tightening of monetary policy in the latter part of 1984, the rates, of growth of the monetary aggregates increased in the early months of, 1985, while there was a further reduction in real money balances. The, decline in real balances is expected to continue in coming months, and, only when the anti-inflation program takes hold and inflationary ex-, pectations recede will money holdings show real growth. The growth of, Ml (defined as the sum of the monthly averages of currency in circulation, and total sight deposits) is estimated to have declined from a seasonally, adjusted monthly rate of 22 percent in the period February-April 1985, to about 19 l/2 percent in May and is projected to decline further to, 13 l/2 percent by September 1'985 and to below 10 percent in the first, quarter of 1986. In line with the monetary program for the period May, to September 1985 and the balance of payments objectives, limits have, been set on the net domestic assets of the Central Bank (defined as, currency issue less net foreign assets) through end-September 1985, (Table 3). The limit on the net domestic assets of the Central Bank, for the period through March 1986 will be set on the occasion of the, review of the program that is scheduled for October 1985, taking into, account the behavior of monetary aggregates in the intervening period.)"
#> 
#> $pages[[6]]
#> [1] "c( - 12 -, is analyzing the existing tariff structure may lead to adjustments in, duty rates to correct anomalies in the tariff structure and to provide, some protection for products on which the quantitative restrictions, are to be lifted. It is recognized that exchange rate policy needs to, be consistent with import liberalization to limit import demand follow-, ing the removal of quantitative barriers. In addition, in a move aimed, primarily at strengthening fiscal revenue, the Government has just, introduced a surcharge equal to 10 percent of the value of most imports., Progress in the process of import liberalization will be analyzed on, the occasion of the review of the program scheduled for October 1985,, at which time understandings will be reached on additional steps to be, taken toward the above-mentioned liberalization objectives., 32. In line with the aim of a continuing strengthening of the country's, external position, the overall balance of payments deficit has been, targeted to narrow from USS1.74 billion in 1984 to USS1.65 billion in, 1985. The current account deficit is projected to decline from US$2 l/2, billion in 1984 to USS2 billion in 1985, with about one half this, improvement reflecting the sharp reduction in international interest, rates since September 1984. Also, following a deterioration in the early, months of 1985, the private capital account is expected to strengthen, in the second half of 1985 and into 1986. The exchange rate and credit, policies described above should encourage capital inflows and, in addi-, tion, the Government has taken steps to attract foreign direct invest-, ment in the petroleum sector and has established a scheme whereby foreign, creditors can convert outstanding loans into capital participation., Targets for net international reserves through December 1985 are set, forth in Table 4. The target for end-March 1986 will be agreed on the, occasion of the October review of the program., 33. To finance the overall balance of payments deficit and to provide, both for the elimination of external payments arrears and a needed, increase in the gross reserves of the Central Bank, the Government has, been arranging exceptional finance from official and private creditors., The Paris Club Agreed Minutes of January 1985 provide for the reschedul-, ing of USS2.1 billion in service payments on medium- and long-term debt, including USS1.4 billion that were in arrears at the end of 1984 and, USSO. billion that fall due in 1985; bilateral agreements are being, finalized with some official creditors and the Government is committed, to completing all these agreements as soon as possible. The Government, is near agreement with creditor banks on the rescheduling of USs14.5, billion of principal payments that have matured since April 1982 or will, mature through December 1985. The Government also is near agreement on, new financing of USS4.2 billion from creditor banks and has received, assurances from official creditors of US$l billion in additional trade, finance. A substantial portion of this exceptional finance is to be, used to repay arrears and other foreign obligations. The disbursement, of the new bank financing is to be timed to correspond to the drawings, from the Fund under the stand-by arrangement, and the elimination of, external arrears has been planned accordingly, as shown in Table 5., External payments arrears are to be eliminated by March 31, 1986 and no)"
#> 
#> $pages[[7]]
#> [1] "c( - 13 -, new arrears will be incurred after that date. While arrears are being, eliminated, the Government is interested in maintaining an accurate, presentation by the private sector of requests for official exchange., To this end, the Government intends to institute a deposit scheme whereby, the peso counterpart of external payments arrears will be placed with, the Central Bank at the time of payments requests., 34. The total external debt, including obligations to the Fund and ex-, ternal arrears, increased by about USS1.8 billion in 1984 to USS47.8, billion, and is expected to rise by another USS2.4 billion in 1985., Consistent with this overall growth in debt, limits have been established, for the total and the short-term external debt of the public sector for, the remainder of the arrangement, as presented in Table 6., 35. The Government intends to simplify the exchange and trade system, and eliminate restrictions to the extent permitted by the availability, of foreign exchange. With a view to allocating scarce exchange at a, time of severe balance of payments difficulties, in September 1983, external payments and transfers were subjected to prior approval by the, Central Bank. It is the intention of the Government, pari passu with, the reduction of external payments arrears, to make foreign exchange, available for bona fide payments and transfers for current international, transactions; with respect to the private sector this will be done on, an automatic basis. In pursuing this objective, priority has been, attached to private sector import payments; since August 1984 all pri-, vate sector import payments, other than those expected to be rescheduled,, have been on a current basis and foreign exchange is now being, and will, continue to be, made available automatically for such payments. On the, basis of the expected improvement of the balance of payments and the, external financing to be obtained, the Central Bank intends to provide,, as soon as possible, foreign exchange for interest payments and for, transfers related to nonfinancial services, profits, dividends, and, royalties. In the meantime, the cancellation of obligations in respect, of profits, dividends, and royalties will continue to be permitted, through the delivery of marketable Government of Argentina bonds denomi-, nated in U.S. dollars (BONEX). Moreover, the regulations regarding the, restructuring of loans with exchange rate guarantees which mature in, 1985 will be announced before the end of July 1985., 36. As noted above, in April-May there were significant deposit with-, drawals from some banks, including especially large withdrawals of U.S., dollar deposits which are not guaranteed by the Central Bank. As a, matter of prudential concern, the Central Bank, on May 17, 1985, froze, outstanding dollar deposits in the banking system for 120 days. In, doing so, it provided that deposits maturing during the period of the, freeze could be converted, at the option of the depositor, into BONEX., The Government believes that a sudden withdrawal of U.S. dollar deposits, in present circumstances could compromise the soundness of the financial, system, but it intends to take steps as soon as possible to permit, holders of frozen deposits to use these resources to make external, payments.)"
#> 
#> $pages[[8]]
#> [1] "c( - 14 -, 37. Before the end of October 1985, the authorities will review with, the Fund the progress in implementing their economic program in order, to reach understandings, if necessary, on additional measures to ensure, achievement of the program's objectives. During the review, stock will, be taken of the external financing secured in support of the program, and the objectives with regard to the balance of payments, external, arrears, external debt, the exchange system, and import policy. Limits, for the variables defined in peso terms referred to in paragraphs 22, and 23 will be set for the remainder of the program period; the target, for the net international reserves for end-March 1986 also will be set, in the review., Yours sincerely,, IS/ /S/, J.J. Alfred0 Concepci& Juan Vital Sourrouille, President of the Central Bank Minister of Economy, of the Republic of Argentina)"
#> 
#> $pages[[9]]
#> [1] "c( - 20 -, Table 6. Argentina: Limits on the External Debt of the, Public Sector During the Program Period, (In millions of U.S. dollars), Limit on the total outstanding, disbursed external debt of, the public sector L/ 39,700, Limit on cumulative net disbur-, sements of short-term debt of, of the public sector contracted, after September 30, 1984 -2/ 2,500, L/ The definition of total outstanding disbursed external debt of, the public sector includes all external obligations of the public, sector, including the Central Bank of the Republic of Argentina, and the official banks. However, this definition excludes bonds and, notes issued in lieu of providing foreign exchange to meet principal, payments falling due on private sector debt covered by exchange rate, guarantees, obligations deriving from the assumption by the public, sector of debt of private domestic borrowers after December 31, 1983,, and those categories of obligations not subject to the Central Bank's, debt registration system as of September 15, 1984. It includes loans, covered by swap arrangements undertaken by the Central Bank., 21 Includes cumulated disbursements, net of repayments, of debt, with a maturity up to one year, contracted by public sector entities, after September 30, 1984, other than obligations classified as reserve, liabilities.)"
```

Compute the document term frequency for all the files in the corpus for the category "Currency\_crisis"

``` r

tf_matrix=tf(corpus,"Sovereign_default")

head(tf_matrix)
#> # A tibble: 4 x 2
#>   var[,1] file                           
#>     <dbl> <chr>                          
#> 1       0 ARG_1984-12-28_exchange system 
#> 2      NA ARG_1984-12-28_purchase transac
#> 3       0 ARG_1984-12-28_request         
#> 4       0 ARG_1985-06-11_request
```

Compute the document term frequency for several categories "Currency\_crisis" and "Balance\_payment\_crisis"

``` r

# term frequency matrix for several categories of crisis
mycategories=c('Currency_crisis',"Balance_payment_crisis","Sovereign_default")
tf_matrix_with_several_categories=tf_vector(corpus,lexicon()[mycategories])
#> 
#> (1/3) running: Currency_crisis
#> Currency_crisis: 0.355 sec elapsed
#> 
#>  Finished running: Currency_crisis
#> 
#> (2/3) running: Balance_payment_crisis
#> Balance_payment_crisis: 0.247 sec elapsed
#> 
#>  Finished running: Balance_payment_crisis
#> 
#> (3/3) running: Sovereign_default
#> Sovereign_default: 0.234 sec elapsed
#> 
#>  Finished running: Sovereign_default

head(tf_matrix_with_several_categories)
#> # A tibble: 4 x 4
#>   file             Currency_crisis[,… Balance_payment_cr… Sovereign_defaul…
#>   <chr>                         <dbl>               <dbl>             <dbl>
#> 1 ARG_1984-12-28_…                  0            0                 0       
#> 2 ARG_1984-12-28_…                 NA           NA                NA       
#> 3 ARG_1984-12-28_…                  0            0.000368          0.000368
#> 4 ARG_1985-06-11_…                  0            0                 0.00617
```

Wrapup function for tf

``` r

#Run term frequency matrix

wrapup_for_tf=run_tf(corpus_path = "mycorpus.RData",type_lexicon ="words",keyword_list = c("Currency_crisis","Balance_payment_crisis"),parrallel = F)
#> Loading corpus from mycorpus.RData
#> (1/2) running: Currency_crisis
#> Currency_crisis: 0.159 sec elapsed
#> 
#>  Finished running: Currency_crisis
#> 
#> (2/2) running: Balance_payment_crisis
#> Balance_payment_crisis: 0.183 sec elapsed
#> 
#>  Finished running: Balance_payment_crisis
#> 0.379 sec elapsed
#> [1] "export table in mycorpus.RData"

head(wrapup_for_tf)
#> # A tibble: 4 x 3
#>   file                         Currency_crisis[,1] Balance_payment_crisis[…
#>   <chr>                                      <dbl>                    <dbl>
#> 1 ARG_1984-12-28_exchange sys…                   0                 0       
#> 2 ARG_1984-12-28_purchase tra…                  NA                NA       
#> 3 ARG_1984-12-28_request                         0                 0.000368
#> 4 ARG_1985-06-11_request                         0                 0
```

Wrapup function for run\_tf that allows directly download the files and run the text mining with a single function

``` r
run_tf_by_chunk(urls =url_links,keyword_list = c("Currency_crisis","Balance_payment_crisis"))
#> Warning in dir.create(path): 'temp' already exists
#> Warning in dir.create(path_corpus): 'temp/corpus' already exists
#> Warning in dir.create(path_tf): 'temp/tf' already exists
#> ARG_1984-12-28_exchange system: succesfully downloaded 
#> ARG_1984-12-28_exchange system : 1/6: 2.399 sec elapsed
#> ARG_1984-12-28_request: succesfully downloaded 
#> ARG_1984-12-28_request : 2/6: 3.201 sec elapsed
#> ARG_1984-12-28_purchase transac: succesfully downloaded 
#> ARG_1984-12-28_purchase transac : 3/6: 2.532 sec elapsed
#> ARG_1984-12-28_request: already downloaded, keep existing 
#> ARG_1984-12-28_request : 4/6: 0.014 sec elapsed
#> ARG_1984-12-28_request: already downloaded, keep existing 
#> ARG_1984-12-28_request : 5/6: 0.011 sec elapsed
#> ARG_1985-06-11_request: succesfully downloaded 
#> ARG_1985-06-11_request : 6/6: 3.02 sec elapsed
#> urls succesfully downloaded in 'temp/files
#> '
#> Warning in stri_replace_all_regex(string, pattern,
#> fix_replacement(replacement), : argument is not an atomic vector; coercing
#> [1] "temp/files/ARG_1984-12-28_exchange system.pdf"
#> 1/4 ARG_1984-12-28_exchange system: 0.011 sec elapsed
#> Warning in stri_replace_all_regex(string, pattern,
#> fix_replacement(replacement), : argument is not an atomic vector; coercing
#> [1] "temp/files/ARG_1984-12-28_purchase transac.pdf"
#> 2/4 ARG_1984-12-28_purchase transac: 0.052 sec elapsed
#> Warning in stri_replace_all_regex(string, pattern,
#> fix_replacement(replacement), : argument is not an atomic vector; coercing
#> [1] "temp/files/ARG_1984-12-28_request.pdf"
#> 3/4 ARG_1984-12-28_request: 0.009 sec elapsed
#> Warning in stri_replace_all_regex(string, pattern,
#> fix_replacement(replacement), : argument is not an atomic vector; coercing
#> [1] "temp/files/ARG_1985-06-11_request.pdf"
#> 4/4 ARG_1985-06-11_request: 0.208 sec elapsed
#> delete folder with pdf 
#> Loading corpus from temp/corpus/corpus_1.RData
#> (1/2) running: Currency_crisis
#> Currency_crisis: 0.179 sec elapsed
#> 
#>  Finished running: Currency_crisis
#> 
#> (2/2) running: Balance_payment_crisis
#> Balance_payment_crisis: 0.237 sec elapsed
#> 
#>  Finished running: Balance_payment_crisis
#> 0.452 sec elapsed
#> [1] "export table in temp/corpus/corpus_1.RData"
#> [1] TRUE
```

Update the tf dataframe with additional columns with the new categories to compute

``` r

updated_tf=run_tf_update(path_tf_to_update = "temp/tf/tf_crisis_words_1.RData",
                corpus_path = "temp/corpus/corpus_1.RData",
                keyword_list = c("Fiscal_outcomes","Fiscal_consolidation"),
                export_path = "temp/tf/tf_crisis_words_1_new.RData")
#> updating selected columnsLoading corpus from temp/corpus/corpus_1.RData
#> (1/2) running: Fiscal_outcomes
#> Fiscal_outcomes: 0.218 sec elapsed
#> 
#>  Finished running: Fiscal_outcomes
#> 
#> (2/2) running: Fiscal_consolidation
#> Fiscal_consolidation: 0.2 sec elapsed
#> 
#>  Finished running: Fiscal_consolidation
#> 0.453 sec elapsed
#> [1] "export table in temp/corpus/corpus_1.RData"
#> [1] "Non updated columns:\n\n                 file, Currency_crisis, Balance_payment_crisis"
#> [1] "Updated columns:\n\n                 Fiscal_outcomes, Fiscal_consolidation"

head(updated_tf)
#> # A tibble: 4 x 5
#>   file  Currency_crisis… Balance_payment… Fiscal_outcomes… Fiscal_consolid…
#>   <chr>            <dbl>            <dbl>            <dbl>            <dbl>
#> 1 ARG_…                0                0          0                0      
#> 2 ARG_…               NA               NA         NA               NA      
#> 3 ARG_…                0                0          0                0      
#> 4 ARG_…                0                0          0.00282          0.00121
```
