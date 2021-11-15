Elemzes
================
Balint Mazzag
2021 11 03

2.  Hasonlítsák össze az oktatók (4-es csoport) és az ügyintézők (5 és
    6-os csoport együtt) keresetek szerinti eloszlását a lehető
    legteljesebben!

Az eredményeket foglalják össze, ahol annak helye van érzékeltessék
ábrákkal! Igyekezzenek tömören, lényegretörően végezni a számításokat!
Kérjük, egy word vagy pdf fájlban legyenek az eredmények, elemzések!
Excelt, vagy más szoftvert természetesen használhatnak, de azok outputja
ha feltétlenül kell, függelékként lehet az elemzésükben.

1.  Az egyetemi főiskolai oktatók/tanárok fizetése életkori
    csoportonként hogyan különbözik egymástól? Hasonlítsák össze
    valamilyen benchmark adattal, ezen az egyetemen mennyivel keresnek
    jobban/rosszabbul az egyetemi/főiskolai oktatók/tanárokéletkori
    csoportonkénti bontásban, mint az országos átlag?

![](Elemzes_files/figure-gfm/unnamed-chunk-4-1.pdf)<!-- -->

    ## # A tibble: 6 x 7
    ##   eletkor_group  mean median    sd alpha3 kurtosis     n
    ##   <chr>         <dbl>  <dbl> <dbl>  <dbl>    <dbl> <int>
    ## 1 20-29          398.   372.  96.9  0.636     2.50    37
    ## 2 30-39          482.   443. 171.   1.28      4.24   116
    ## 3 40-49          501.   419. 242.   2.14      8.46   209
    ## 4 50-59          535.   414. 356.   3.16     14.1    191
    ## 5 60-69          561.   458. 286.   1.82      6.06    94
    ## 6 Összesen       511.   425. 274.   3.02     15.6    647

![](Elemzes_files/figure-gfm/unnamed-chunk-7-1.pdf)<!-- -->

![](Elemzes_files/figure-gfm/unnamed-chunk-8-1.pdf)<!-- -->

Életkor alapján való eloszlása a tanári fizetéseknek

Oktatük és ügyintézők kereseti eloszlása

    ## Warning: Using alpha for a discrete variable is not advised.

![](Elemzes_files/figure-gfm/unnamed-chunk-9-1.pdf)<!-- -->

![](Elemzes_files/figure-gfm/unnamed-chunk-10-1.pdf)<!-- -->

3.  Készítsenek elemzést arról, hogy nem (férfi-nő) szerint a havi
    átlagos bruttó keresetekben mekkora az átlagos különbség
    összességében és az egyéb ismérvek hatását kiszűrve, illetve azokkal
    összekapcsolódva! Használjanak az elemzéshez kétféle
    módszertant/modellt és hasonlítsák össze a kétféle módszerrel kapott
    eredmény(eke)t! Írjanak egy összefoglalást is az elemzések
    tapasztalatairól!

p-score, ols, fa, ..

    ## # A tibble: 2 x 2
    ##   nem   `mean(kereset)`
    ##   <chr>           <dbl>
    ## 1 Férfi            539.
    ## 2 Nő               493.

![](Elemzes_files/figure-gfm/unnamed-chunk-12-1.pdf)<!-- -->

    ## # A tibble: 647 x 8
    ##        z nem   eletkor iskvegz      munkakor         kereset eletkor_group    id
    ##    <dbl> <chr>   <int> <ord>        <ord>              <dbl> <ord>         <int>
    ##  1 0.521 Férfi      50 Egyetem/Msc  Legfelsőbb veze~   2411. 50-59             1
    ##  2 0.516 Nő         51 Egyetem/Msc  Legfelsőbb veze~   1073. 50-59             2
    ##  3 0.507 Férfi      53 Egyetem/Msc  Legfelsőbb veze~   1990. 50-59             3
    ##  4 0.455 Nő         64 Egyetem/Msc  Legfelsőbb veze~   1609. 60-69             4
    ##  5 0.393 Férfi      32 Főiskola/Bsc Tanszék/intézet~    706. 30-39             5
    ##  6 0.477 Nő         33 Egyetem/Msc  Tanszék/intézet~    994. 30-39             6
    ##  7 0.384 Nő         34 Főiskola/Bsc Tanszék/intézet~    632. 30-39             7
    ##  8 0.380 Férfi      35 Főiskola/Bsc Tanszék/intézet~    512. 30-39             8
    ##  9 0.468 Férfi      35 Egyetem/Msc  Tanszék/intézet~    987. 30-39             9
    ## 10 0.468 Nő         35 Egyetem/Msc  Tanszék/intézet~    880. 30-39            10
    ## # ... with 637 more rows

    ## # A tibble: 1 x 3
    ##     ate  atet atet_no
    ##   <dbl> <dbl>   <dbl>
    ## 1  27.1  36.8    20.9

    ## Warning: Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables).
    ## To silence this warning:
    ##     Call rpart.plot with roundint=FALSE,
    ##     or rebuild the rpart model with model=TRUE.

![](Elemzes_files/figure-gfm/unnamed-chunk-15-1.pdf)<!-- -->

# Függelék: R kódok

``` r
library(tidyverse)
teacher_df <- readxl::read_excel("3. forduló STAT WARS UNI.xlsx", sheet = 2) %>% 
  mutate(
    nem = case_when(
      nem == 1 ~ "Férfi",
      nem == 2 ~ "Nő"
    ),
    eletkor = as.integer(eletkor),
    iskvegz = factor(iskvegz, levels = 1:3, ordered = TRUE),
    iskvegz = fct_relabel(iskvegz, function(l) {
      case_when(
      l == 1 ~ "Legfeljebb érettségi",
      l == 2 ~ "Főiskola/Bsc",
      l == 3 ~ "Egyetem/Msc"
    )}),
    munkakor = factor(munkakor, levels = 7:1, ordered = TRUE),
    munkakor = fct_relabel(munkakor, function(l) {
    case_when(
      l == 1 ~ "Legfelsőbb vezető",
      l == 2 ~ "Tanszék/intézetvezető",
      l == 3 ~ "Egyéb (gazdasági, jogi, műszaki, stb.)",
      l == 4 ~ "Egyetemi/főiskolai oktató/tanár",
      l == 5 ~ "Magasan képzett ügyintéző",
      l == 6 ~ "Ügyintéző/titkárnő",
      l == 7 ~ "Betanított/segédmunkát végző"
    )})
  )

teacher_df <- teacher_df %>% 
  mutate(
    eletkor_group = cut(eletkor, breaks = c((1:7)*10, Inf), right = FALSE, 
                        labels = FALSE),
    eletkor_group = factor(eletkor_group, levels = 1:6, ordered = TRUE),
    eletkor_group = fct_relabel(eletkor_group, function(l) {
      case_when(
        l == 1 ~ "-19",
        l == 7 ~ "70-",
        TRUE ~ str_c(as.numeric(l)*10, "-", as.numeric(l) * 10 + 9)
      )
    })
  )
teacher_df %>% 
  filter(munkakor == "Egyetemi/főiskolai oktató/tanár") %>% 
  select(-eletkor, -munkakor) %>% 
  GGally::ggpairs(aes(color = eletkor_group))
total_summarise <- function(x, g, ...) {
  bind_rows(
    x %>% 
      group_by({{ g }}) %>% 
      summarise(...) %>% 
      ungroup(),
    x %>% 
      summarise(...) %>% 
      mutate(g = "Összesen") %>% 
    select(g, everything()) %>% 
    rename("{{ g }}" := 1)
  )
}
total_summarise(teacher_df, eletkor_group, 
                mean = mean(kereset),
                median = median(kereset),
                sd = sd(kereset),
                alpha3 = moments::skewness(kereset),
                kurtosis = moments::kurtosis(kereset),
                n = n()
                )
teacher_df %>% 
  group_by(eletkor_group, nem) %>% 
  summarise(m = mean(kereset), s = sd(kereset), n = n()) %>% 
  mutate(
    cl = m - s/(n^.5),
    ch = m + s/(n^.5)
  ) %>% 
  ggplot() +
  aes(m, eletkor_group) +
  geom_linerange(aes(xmin = cl, xmax = ch, color = nem), size = 2, alpha = .5) +
  geom_point(aes(fill = nem), shape = 21, size = 3)

GGally::ggpairs(teacher_df, ggplot2::aes(colour=iskvegz))

teacher_df %>% 
  filter(
    munkakor %in% c("Ügyintéző/titkárnő", "Magasan képzett ügyintéző", "Egyetemi/főiskolai oktató/tanár")
  ) %>% 
  mutate(munkakor_group = ifelse(
    munkakor == "Egyetemi/főiskolai oktató/tanár", "Oktató", "ügyintéző"
  )) %>% 
  group_by(munkakor_group) %>% 
  mutate(iskvegz_ratio = round(sum(iskvegz == "Egyetem/Msc")/n(), 2),
         iskvegz_ratio = scales::percent(iskvegz_ratio),
         mean = mean(kereset)) %>% 
  ungroup() %>% 
  ggplot(aes(x = kereset, group = munkakor_group, color = munkakor_group,
             fill = munkakor_group, alpha = iskvegz_ratio)) +
  geom_density() +
  geom_vline(aes(xintercept = mean, color = munkakor_group), linetype = "dashed", size = 0.9) +
  facet_wrap(~nem)+
  labs(alpha = "Mesterfokú diplomával rendelkezők aránya")
teacher_df %>% 
  filter(
    munkakor %in% c("Ügyintéző/titkárnő", "Magasan képzett ügyintéző", "Egyetemi/főiskolai oktató/tanár")
  ) %>% 
  mutate(munkakor_group = ifelse(
    munkakor == "Egyetemi/főiskolai oktató/tanár", "Oktató", "ügyintéző"
  )) %>% 
  GGally::ggpairs(aes(color = munkakor_group))
teacher_df %>% 
  group_by(nem) %>% 
  summarise(mean(kereset)) # TODO
teacher_df %>% 
  lm(formula = kereset ~ .-eletkor_group) %>% 
  GGally::ggcoef_model()
teacher_df %>% 
  select(- eletkor_group) %>% 
  mutate(nem = nem == "Férfi") %>% 
  glm(formula = nem ~ eletkor + iskvegz + munkakor, family = "binomial") %>% 
  predict( type = "response") %>% 
  cbind(teacher_df) %>% 
  rename(z = 1) %>% 
  tibble() %>% 
  mutate(id = row_number())
teacher_df %>% 
  group_by(nem, iskvegz, munkakor, eletkor_group) %>% 
  summarise(kereset = mean(kereset), n = n()) %>% 
  pivot_wider(names_from = nem, values_from = c(kereset, n)) %>% 
  mutate(
    d = kereset_Férfi - kereset_Nő,
    n = n_Férfi + n_Nő
  ) %>% 
  ungroup() %>% 
  summarise(ate = weighted.mean(d, n, na.rm = T),
            atet = weighted.mean(d, n_Férfi, na.rm = TRUE),
            atet_no = weighted.mean(d, n_Nő, na.rm = TRUE))
teacher_df %>% 
  select(- eletkor_group) %>% 
  rpart::rpart(formula = kereset ~ ., cp = .001) %>% 
  rpart.plot::rpart.plot()
```
