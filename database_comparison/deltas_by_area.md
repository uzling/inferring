Get greatest deltas in segment coverage by macroarea
================
Steven Moran

07 October, 2020

    library(tidyverse)
    library(knitr)
    library(xtable)

    phoible <- read_csv(url('https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true'), col_types=c(InventoryID='i', Marginal='l', .default='c'))

    glottolog <- read_csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-D501-DBB8-65C4-0/languages_and_dialects_geo.csv')

    ## Parsed with column specification:
    ## cols(
    ##   glottocode = col_character(),
    ##   name = col_character(),
    ##   isocodes = col_character(),
    ##   level = col_character(),
    ##   macroarea = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double()
    ## )

    head(glottolog)

    ## # A tibble: 6 x 7
    ##   glottocode name       isocodes level    macroarea latitude longitude
    ##   <chr>      <chr>      <chr>    <chr>    <chr>        <dbl>     <dbl>
    ## 1 3adt1234   3Ad-Tekles <NA>     dialect  Africa       NA         NA  
    ## 2 aala1237   Aalawa     <NA>     dialect  Papunesia    NA         NA  
    ## 3 aant1238   Aantantara <NA>     dialect  Papunesia    NA         NA  
    ## 4 aari1239   Aari       aiw      language Africa        5.95      36.6
    ## 5 aari1240   Aariya     aay      language Eurasia      NA         NA  
    ## 6 aasa1238   Aasax      aas      language Africa       -4.01      36.9

    merged <- left_join(phoible, glottolog, by=c("Glottocode"="glottocode"))

    # Get percentages total
    totals <- merged %>% select(InventoryID, Phoneme) %>% group_by(Phoneme) %>% summarize(count=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    totals$phoible_freq <- totals$count / length(unique(phoible$InventoryID))

    # Get percentages by macroarea
    macros <- merged %>% select(InventoryID, macroarea, Phoneme, SegmentClass)
    # There are some NAs
    table(macros$macroarea, exclude = FALSE)

    ## 
    ##        Africa     Australia       Eurasia North America     Papunesia 
    ##         36332         10464         33866          6421          5198 
    ## South America          <NA> 
    ##         12462           717

    # Identify the NAs.
    missing_areas <- macros %>% filter(is.na(macroarea)) %>% select(InventoryID) %>% distinct()
    missing_langs <- phoible %>% filter(InventoryID %in% missing_areas$InventoryID)
    missing_langs %>% select(InventoryID, Glottocode, LanguageName) %>% distinct() %>% kable()

| InventoryID | Glottocode | LanguageName      |
|------------:|:-----------|:------------------|
|         976 | osse1243   | Ossetian          |
|        1208 | jiar1240   | Jiarong           |
|        1398 | dink1262   | Dinka             |
|        2257 | jiar1240   | Caodeng rGyalrong |
|        2283 | osse1243   | Iron Ossetic      |
|        2324 | osse1243   | Iron Ossetic      |
|        2351 | osse1243   | Iron Ossetic      |
|        2389 | osse1243   | Iron Ossetic      |
|        2408 | osse1243   | Iron Ossetic      |
|        2424 | osse1243   | Iron Ossetic      |
|        2432 | osse1243   | Iron Ossetic      |
|        2464 | mong1331   | Mongolian         |
|        2476 | osse1243   | Iron Ossetic      |
|        2509 | osse1243   | Iron Ossetic      |
|        2626 | osse1243   | Iron Ossetic      |
|        2729 | NA         | Djindewal         |

    # Drop the NAs for the time being
    macros <- macros %>% filter(!is.na(macroarea))
    rownames(macros) <- NULL

Get segment frequencies by area.

    africa <- macros %>% filter(macroarea=="Africa")
    total <- length(unique(africa$InventoryID))
    africa <- africa %>% group_by(Phoneme, SegmentClass) %>% summarize(count=n())

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

    africa$freq <- africa$count / total
    africa$macroarea <- 'Africa'

    australia <- macros %>% filter(macroarea=="Australia")
    total <- length(unique(australia$InventoryID))
    australia <- australia %>% group_by(Phoneme, SegmentClass) %>% summarize(count=n())

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

    australia$freq <- australia$count / total
    australia$macroarea <- 'Australia'

    eurasia <- macros %>% filter(macroarea=="Eurasia")
    total <- length(unique(eurasia$InventoryID))
    eurasia <- eurasia %>% group_by(Phoneme, SegmentClass) %>% summarize(count=n())

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

    eurasia$freq <- eurasia$count / total
    eurasia$macroarea <- 'Eurasia'

    na <- macros %>% filter(macroarea=="North America")
    total <- length(unique(na$InventoryID))
    na <- na %>% group_by(Phoneme, SegmentClass) %>% summarize(count=n())

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

    na$freq <- na$count / total
    na$macroarea <- 'North America'

    papunesia <- macros %>% filter(macroarea=="Papunesia")
    total <- length(unique(papunesia$InventoryID))
    papunesia <- papunesia %>% group_by(Phoneme, SegmentClass) %>% summarize(count=n())

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

    papunesia$freq <- papunesia$count / total
    papunesia$macroarea <- 'Papunesia'

    sa <- macros %>% filter(macroarea=="South America")
    total <- length(unique(sa$InventoryID))
    sa <- sa %>% group_by(Phoneme, SegmentClass) %>% summarize(count=n())

    ## `summarise()` regrouping output by 'Phoneme' (override with `.groups` argument)

    sa$freq <- sa$count / total
    sa$macroarea <- 'South America'

    # Drop the count and rbind them the area-specific dataframes.
    africa <- africa %>% select(macroarea, Phoneme, SegmentClass, freq)
    australia <- australia %>% select(macroarea, Phoneme, SegmentClass, freq)
    eurasia <- eurasia %>% select(macroarea, Phoneme, SegmentClass, freq)
    papunesia <- papunesia %>% select(macroarea, Phoneme, SegmentClass, freq)
    na <- na %>% select(macroarea, Phoneme, SegmentClass, freq)
    sa <- sa %>% select(macroarea, Phoneme, SegmentClass, freq)

    all <- bind_rows(africa, australia, eurasia, papunesia, na, sa)

    # Get delta between phoneme frequencies by area
    totals <- totals %>% select(Phoneme, phoible_freq)
    all <- left_join(all, totals, by=c("Phoneme"="Phoneme"))
    all$delta <- all$freq-all$phoible_freq

    # Where are the largest deltas by area?
    all %>% group_by(macroarea) %>% slice_max(order_by = delta, n = 10) %>% kable()

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |     delta |
|:--------------|:--------|:-------------|----------:|--------------:|----------:|
| Africa        | f       | consonant    | 0.8418079 |     0.4403974 | 0.4014106 |
| Africa        | ˨       | tone         | 0.5785311 |     0.1807947 | 0.3977364 |
| Africa        | ˦       | tone         | 0.5796610 |     0.1831126 | 0.3965484 |
| Africa        | d       | consonant    | 0.8022599 |     0.4556291 | 0.3466307 |
| Africa        | ɡ       | consonant    | 0.8779661 |     0.5668874 | 0.3110787 |
| Africa        | ɔ       | vowel        | 0.6621469 |     0.3543046 | 0.3078423 |
| Africa        | ɡb      | consonant    | 0.4203390 |     0.1238411 | 0.2964979 |
| Africa        | kp      | consonant    | 0.4192090 |     0.1235099 | 0.2956991 |
| Africa        | ɲ       | consonant    | 0.7062147 |     0.4158940 | 0.2903206 |
| Africa        | b       | consonant    | 0.9129944 |     0.6311258 | 0.2818685 |
| Australia     | ȵ       | consonant    | 0.8692661 |     0.1258278 | 0.7434382 |
| Australia     | ȶ       | consonant    | 0.7155963 |     0.1036424 | 0.6119539 |
| Australia     | ɻ       | consonant    | 0.6582569 |     0.1013245 | 0.5569324 |
| Australia     | ɳ       | consonant    | 0.6582569 |     0.1321192 | 0.5261377 |
| Australia     | ɭ       | consonant    | 0.6330275 |     0.1188742 | 0.5141534 |
| Australia     | ʈ       | consonant    | 0.5665138 |     0.1592715 | 0.4072422 |
| Australia     | n̪       | consonant    | 0.5756881 |     0.1764901 | 0.3991980 |
| Australia     | ȴ       | consonant    | 0.4633028 |     0.0668874 | 0.3964153 |
| Australia     | ŋ       | consonant    | 1.0000000 |     0.6284768 | 0.3715232 |
| Australia     | t̪       | consonant    | 0.4954128 |     0.2344371 | 0.2609758 |
| Eurasia       | pʰ      | consonant    | 0.5223881 |     0.1963576 | 0.3260304 |
| Eurasia       | kʰ      | consonant    | 0.5161692 |     0.2006623 | 0.3155069 |
| Eurasia       | d̪       | consonant    | 0.3582090 |     0.1440397 | 0.2141692 |
| Eurasia       | tʰ      | consonant    | 0.3097015 |     0.1337748 | 0.1759267 |
| Eurasia       | x       | consonant    | 0.3606965 |     0.1900662 | 0.1706303 |
| Eurasia       | b       | consonant    | 0.7997512 |     0.6311258 | 0.1686254 |
| Eurasia       | ɖ       | consonant    | 0.2425373 |     0.0850993 | 0.1574380 |
| Eurasia       | ɡ       | consonant    | 0.7213930 |     0.5668874 | 0.1545056 |
| Eurasia       | t̪ʰ      | consonant    | 0.1915423 |     0.0592715 | 0.1322708 |
| Eurasia       | ʒ       | consonant    | 0.2898010 |     0.1582781 | 0.1315228 |
| North America | ʔ       | consonant    | 0.8586957 |     0.3748344 | 0.4838612 |
| North America | kʼ      | consonant    | 0.4184783 |     0.0804636 | 0.3380147 |
| North America | h       | consonant    | 0.8478261 |     0.5639073 | 0.2839188 |
| North America | ʃ       | consonant    | 0.6413043 |     0.3655629 | 0.2757414 |
| North America | t̠ʃ      | consonant    | 0.6684783 |     0.4033113 | 0.2651670 |
| North America | t̠ʃʼ     | consonant    | 0.3152174 |     0.0612583 | 0.2539591 |
| North America | tsʼ     | consonant    | 0.2934783 |     0.0427152 | 0.2507630 |
| North America | tʼ      | consonant    | 0.2989130 |     0.0519868 | 0.2469263 |
| North America | kʷ      | consonant    | 0.3532609 |     0.1231788 | 0.2300821 |
| North America | pʼ      | consonant    | 0.2880435 |     0.0592715 | 0.2287720 |
| Papunesia     | ʔ       | consonant    | 0.5185185 |     0.3748344 | 0.1436841 |
| Papunesia     | o̞       | vowel        | 0.1990741 |     0.0956954 | 0.1033787 |
| Papunesia     | s       | consonant    | 0.7592593 |     0.6692053 | 0.0900540 |
| Papunesia     | β       | consonant    | 0.1712963 |     0.1013245 | 0.0699718 |
| Papunesia     | e̞       | vowel        | 0.1574074 |     0.0930464 | 0.0643610 |
| Papunesia     | d       | consonant    | 0.5185185 |     0.4556291 | 0.0628894 |
| Papunesia     | u       | vowel        | 0.9351852 |     0.8761589 | 0.0590262 |
| Papunesia     | ɡ       | consonant    | 0.6250000 |     0.5668874 | 0.0581126 |
| Papunesia     | ŋ       | consonant    | 0.6851852 |     0.6284768 | 0.0567084 |
| Papunesia     | n̪\|n    | consonant    | 0.1064815 |     0.0533113 | 0.0531702 |
| South America | ɾ       | consonant    | 0.7244259 |     0.2562914 | 0.4681345 |
| South America | ɨ       | vowel        | 0.5114823 |     0.1625828 | 0.3488995 |
| South America | ã       | vowel        | 0.4237996 |     0.1725166 | 0.2512830 |
| South America | ĩ       | vowel        | 0.4175365 |     0.1794702 | 0.2380663 |
| South America | t       | consonant    | 0.9102296 |     0.6834437 | 0.2267859 |
| South America | t̠ʃ      | consonant    | 0.6263048 |     0.4033113 | 0.2229935 |
| South America | ɨ̃       | vowel        | 0.2484342 |     0.0427152 | 0.2057190 |
| South America | ẽ       | vowel        | 0.2922756 |     0.1069536 | 0.1853219 |
| South America | õ       | vowel        | 0.2964509 |     0.1142384 | 0.1822125 |
| South America | ũ       | vowel        | 0.3382046 |     0.1635762 | 0.1746284 |

    # Obstruents in SA
    voiced_obstruents <- c('b', 'd', 'ɡ', 'β', 'v', 'ð', 'z', 'ʒ', 'ɣ')
    all %>% filter(macroarea == "South America") %>% filter(Phoneme %in% voiced_obstruents) %>% slice(match(voiced_obstruents, Phoneme)) %>% kable()

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |      delta |
|:--------------|:--------|:-------------|----------:|--------------:|-----------:|
| South America | b       | consonant    | 0.4405010 |     0.6311258 | -0.1906248 |
| South America | d       | consonant    | 0.3653445 |     0.4556291 | -0.0902847 |
| South America | ð       | consonant    | 0.0229645 |     0.0529801 | -0.0300156 |
| South America | ɡ       | consonant    | 0.2922756 |     0.5668874 | -0.2746118 |
| South America | ɣ       | consonant    | 0.0584551 |     0.1443709 | -0.0859157 |
| South America | v       | consonant    | 0.0313152 |     0.2701987 | -0.2388834 |
| South America | z       | consonant    | 0.0647182 |     0.2956954 | -0.2309772 |
| South America | ʒ       | consonant    | 0.0584551 |     0.1582781 | -0.0998230 |
| South America | β       | consonant    | 0.1711900 |     0.1013245 |  0.0698655 |

    all %>% group_by(macroarea) %>% slice_max(order_by = delta, n = 10) %>% kable()

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |     delta |
|:--------------|:--------|:-------------|----------:|--------------:|----------:|
| Africa        | f       | consonant    | 0.8418079 |     0.4403974 | 0.4014106 |
| Africa        | ˨       | tone         | 0.5785311 |     0.1807947 | 0.3977364 |
| Africa        | ˦       | tone         | 0.5796610 |     0.1831126 | 0.3965484 |
| Africa        | d       | consonant    | 0.8022599 |     0.4556291 | 0.3466307 |
| Africa        | ɡ       | consonant    | 0.8779661 |     0.5668874 | 0.3110787 |
| Africa        | ɔ       | vowel        | 0.6621469 |     0.3543046 | 0.3078423 |
| Africa        | ɡb      | consonant    | 0.4203390 |     0.1238411 | 0.2964979 |
| Africa        | kp      | consonant    | 0.4192090 |     0.1235099 | 0.2956991 |
| Africa        | ɲ       | consonant    | 0.7062147 |     0.4158940 | 0.2903206 |
| Africa        | b       | consonant    | 0.9129944 |     0.6311258 | 0.2818685 |
| Australia     | ȵ       | consonant    | 0.8692661 |     0.1258278 | 0.7434382 |
| Australia     | ȶ       | consonant    | 0.7155963 |     0.1036424 | 0.6119539 |
| Australia     | ɻ       | consonant    | 0.6582569 |     0.1013245 | 0.5569324 |
| Australia     | ɳ       | consonant    | 0.6582569 |     0.1321192 | 0.5261377 |
| Australia     | ɭ       | consonant    | 0.6330275 |     0.1188742 | 0.5141534 |
| Australia     | ʈ       | consonant    | 0.5665138 |     0.1592715 | 0.4072422 |
| Australia     | n̪       | consonant    | 0.5756881 |     0.1764901 | 0.3991980 |
| Australia     | ȴ       | consonant    | 0.4633028 |     0.0668874 | 0.3964153 |
| Australia     | ŋ       | consonant    | 1.0000000 |     0.6284768 | 0.3715232 |
| Australia     | t̪       | consonant    | 0.4954128 |     0.2344371 | 0.2609758 |
| Eurasia       | pʰ      | consonant    | 0.5223881 |     0.1963576 | 0.3260304 |
| Eurasia       | kʰ      | consonant    | 0.5161692 |     0.2006623 | 0.3155069 |
| Eurasia       | d̪       | consonant    | 0.3582090 |     0.1440397 | 0.2141692 |
| Eurasia       | tʰ      | consonant    | 0.3097015 |     0.1337748 | 0.1759267 |
| Eurasia       | x       | consonant    | 0.3606965 |     0.1900662 | 0.1706303 |
| Eurasia       | b       | consonant    | 0.7997512 |     0.6311258 | 0.1686254 |
| Eurasia       | ɖ       | consonant    | 0.2425373 |     0.0850993 | 0.1574380 |
| Eurasia       | ɡ       | consonant    | 0.7213930 |     0.5668874 | 0.1545056 |
| Eurasia       | t̪ʰ      | consonant    | 0.1915423 |     0.0592715 | 0.1322708 |
| Eurasia       | ʒ       | consonant    | 0.2898010 |     0.1582781 | 0.1315228 |
| North America | ʔ       | consonant    | 0.8586957 |     0.3748344 | 0.4838612 |
| North America | kʼ      | consonant    | 0.4184783 |     0.0804636 | 0.3380147 |
| North America | h       | consonant    | 0.8478261 |     0.5639073 | 0.2839188 |
| North America | ʃ       | consonant    | 0.6413043 |     0.3655629 | 0.2757414 |
| North America | t̠ʃ      | consonant    | 0.6684783 |     0.4033113 | 0.2651670 |
| North America | t̠ʃʼ     | consonant    | 0.3152174 |     0.0612583 | 0.2539591 |
| North America | tsʼ     | consonant    | 0.2934783 |     0.0427152 | 0.2507630 |
| North America | tʼ      | consonant    | 0.2989130 |     0.0519868 | 0.2469263 |
| North America | kʷ      | consonant    | 0.3532609 |     0.1231788 | 0.2300821 |
| North America | pʼ      | consonant    | 0.2880435 |     0.0592715 | 0.2287720 |
| Papunesia     | ʔ       | consonant    | 0.5185185 |     0.3748344 | 0.1436841 |
| Papunesia     | o̞       | vowel        | 0.1990741 |     0.0956954 | 0.1033787 |
| Papunesia     | s       | consonant    | 0.7592593 |     0.6692053 | 0.0900540 |
| Papunesia     | β       | consonant    | 0.1712963 |     0.1013245 | 0.0699718 |
| Papunesia     | e̞       | vowel        | 0.1574074 |     0.0930464 | 0.0643610 |
| Papunesia     | d       | consonant    | 0.5185185 |     0.4556291 | 0.0628894 |
| Papunesia     | u       | vowel        | 0.9351852 |     0.8761589 | 0.0590262 |
| Papunesia     | ɡ       | consonant    | 0.6250000 |     0.5668874 | 0.0581126 |
| Papunesia     | ŋ       | consonant    | 0.6851852 |     0.6284768 | 0.0567084 |
| Papunesia     | n̪\|n    | consonant    | 0.1064815 |     0.0533113 | 0.0531702 |
| South America | ɾ       | consonant    | 0.7244259 |     0.2562914 | 0.4681345 |
| South America | ɨ       | vowel        | 0.5114823 |     0.1625828 | 0.3488995 |
| South America | ã       | vowel        | 0.4237996 |     0.1725166 | 0.2512830 |
| South America | ĩ       | vowel        | 0.4175365 |     0.1794702 | 0.2380663 |
| South America | t       | consonant    | 0.9102296 |     0.6834437 | 0.2267859 |
| South America | t̠ʃ      | consonant    | 0.6263048 |     0.4033113 | 0.2229935 |
| South America | ɨ̃       | vowel        | 0.2484342 |     0.0427152 | 0.2057190 |
| South America | ẽ       | vowel        | 0.2922756 |     0.1069536 | 0.1853219 |
| South America | õ       | vowel        | 0.2964509 |     0.1142384 | 0.1822125 |
| South America | ũ       | vowel        | 0.3382046 |     0.1635762 | 0.1746284 |

    all %>% group_by(macroarea) %>% slice_min(order_by = delta, n = 10) %>% kable()

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |      delta |
|:--------------|:--------|:-------------|----------:|--------------:|-----------:|
| Africa        | ʈ       | consonant    | 0.0259887 |     0.1592715 | -0.1332828 |
| Africa        | pʰ      | consonant    | 0.0632768 |     0.1963576 | -0.1330808 |
| Africa        | kʰ      | consonant    | 0.0677966 |     0.2006623 | -0.1328656 |
| Africa        | ɳ       | consonant    | 0.0033898 |     0.1321192 | -0.1287294 |
| Africa        | n̪       | consonant    | 0.0519774 |     0.1764901 | -0.1245127 |
| Africa        | ɾ       | consonant    | 0.1389831 |     0.2562914 | -0.1173083 |
| Africa        | ɭ       | consonant    | 0.0056497 |     0.1188742 | -0.1132245 |
| Africa        | t̪       | consonant    | 0.1333333 |     0.2344371 | -0.1011038 |
| Africa        | ɻ       | consonant    | 0.0045198 |     0.1013245 | -0.0968047 |
| Africa        | tʰ      | consonant    | 0.0576271 |     0.1337748 | -0.0761477 |
| Australia     | s       | consonant    | 0.0022936 |     0.6692053 | -0.6669117 |
| Australia     | b       | consonant    | 0.0229358 |     0.6311258 | -0.6081900 |
| Australia     | h       | consonant    | 0.0091743 |     0.5639073 | -0.5547330 |
| Australia     | ɡ       | consonant    | 0.0229358 |     0.5668874 | -0.5439516 |
| Australia     | d       | consonant    | 0.0183486 |     0.4556291 | -0.4372805 |
| Australia     | f       | consonant    | 0.0045872 |     0.4403974 | -0.4358102 |
| Australia     | t̠ʃ      | consonant    | 0.0022936 |     0.4033113 | -0.4010177 |
| Australia     | ɲ       | consonant    | 0.0389908 |     0.4158940 | -0.3769032 |
| Australia     | o       | vowel        | 0.2362385 |     0.6046358 | -0.3683972 |
| Australia     | ɛ       | vowel        | 0.0252294 |     0.3738411 | -0.3486117 |
| Eurasia       | t       | consonant    | 0.4502488 |     0.6834437 | -0.2331950 |
| Eurasia       | w       | consonant    | 0.5945274 |     0.8221854 | -0.2276581 |
| Eurasia       | a       | vowel        | 0.6380597 |     0.8609272 | -0.2228675 |
| Eurasia       | ˨       | tone         | 0.0174129 |     0.1807947 | -0.1633818 |
| Eurasia       | ˦       | tone         | 0.0236318 |     0.1831126 | -0.1594807 |
| Eurasia       | n       | consonant    | 0.6741294 |     0.7781457 | -0.1040163 |
| Eurasia       | ɻ       | consonant    | 0.0087065 |     0.1013245 | -0.0926180 |
| Eurasia       | i       | vowel        | 0.8283582 |     0.9201987 | -0.0918405 |
| Eurasia       | kʷ      | consonant    | 0.0323383 |     0.1231788 | -0.0908405 |
| Eurasia       | ã       | vowel        | 0.0845771 |     0.1725166 | -0.0879394 |
| North America | ŋ       | consonant    | 0.2445652 |     0.6284768 | -0.3839116 |
| North America | ɲ       | consonant    | 0.1141304 |     0.4158940 | -0.3017636 |
| North America | e       | vowel        | 0.3586957 |     0.6096026 | -0.2509070 |
| North America | f       | consonant    | 0.2010870 |     0.4403974 | -0.2393104 |
| North America | ɡ       | consonant    | 0.3532609 |     0.5668874 | -0.2136265 |
| North America | r       | consonant    | 0.2391304 |     0.4410596 | -0.2019292 |
| North America | z       | consonant    | 0.1141304 |     0.2956954 | -0.1815649 |
| North America | v       | consonant    | 0.0923913 |     0.2701987 | -0.1778074 |
| North America | b       | consonant    | 0.4619565 |     0.6311258 | -0.1691693 |
| North America | d       | consonant    | 0.2934783 |     0.4556291 | -0.1621509 |
| Papunesia     | ʃ       | consonant    | 0.0694444 |     0.3655629 | -0.2961185 |
| Papunesia     | z       | consonant    | 0.0740741 |     0.2956954 | -0.2216213 |
| Papunesia     | iː      | vowel        | 0.1250000 |     0.3178808 | -0.1928808 |
| Papunesia     | j       | consonant    | 0.7083333 |     0.8993377 | -0.1910044 |
| Papunesia     | uː      | vowel        | 0.1111111 |     0.2937086 | -0.1825975 |
| Papunesia     | aː      | vowel        | 0.1203704 |     0.2953642 | -0.1749939 |
| Papunesia     | v       | consonant    | 0.1018519 |     0.2701987 | -0.1683468 |
| Papunesia     | ˦       | tone         | 0.0185185 |     0.1831126 | -0.1645941 |
| Papunesia     | t̠ʃ      | consonant    | 0.2407407 |     0.4033113 | -0.1625705 |
| Papunesia     | ˨       | tone         | 0.0185185 |     0.1807947 | -0.1622762 |
| South America | ŋ       | consonant    | 0.2150313 |     0.6284768 | -0.4134455 |
| South America | r       | consonant    | 0.0605428 |     0.4410596 | -0.3805168 |
| South America | f       | consonant    | 0.0751566 |     0.4403974 | -0.3652408 |
| South America | l       | consonant    | 0.3235908 |     0.6768212 | -0.3532304 |
| South America | ɡ       | consonant    | 0.2922756 |     0.5668874 | -0.2746118 |
| South America | ɔ       | vowel        | 0.1106472 |     0.3543046 | -0.2436575 |
| South America | v       | consonant    | 0.0313152 |     0.2701987 | -0.2388834 |
| South America | z       | consonant    | 0.0647182 |     0.2956954 | -0.2309772 |
| South America | ɛ       | vowel        | 0.1565762 |     0.3738411 | -0.2172649 |
| South America | b       | consonant    | 0.4405010 |     0.6311258 | -0.1906248 |

    pos <- all %>% group_by(macroarea) %>% filter(SegmentClass=="consonant") %>% slice_max(order_by = delta, n = 10)
    pos %>% kable()

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |     delta |
|:--------------|:--------|:-------------|----------:|--------------:|----------:|
| Africa        | f       | consonant    | 0.8418079 |     0.4403974 | 0.4014106 |
| Africa        | d       | consonant    | 0.8022599 |     0.4556291 | 0.3466307 |
| Africa        | ɡ       | consonant    | 0.8779661 |     0.5668874 | 0.3110787 |
| Africa        | ɡb      | consonant    | 0.4203390 |     0.1238411 | 0.2964979 |
| Africa        | kp      | consonant    | 0.4192090 |     0.1235099 | 0.2956991 |
| Africa        | ɲ       | consonant    | 0.7062147 |     0.4158940 | 0.2903206 |
| Africa        | b       | consonant    | 0.9129944 |     0.6311258 | 0.2818685 |
| Africa        | z       | consonant    | 0.5581921 |     0.2956954 | 0.2624967 |
| Africa        | v       | consonant    | 0.5175141 |     0.2701987 | 0.2473154 |
| Africa        | s       | consonant    | 0.8937853 |     0.6692053 | 0.2245800 |
| Australia     | ȵ       | consonant    | 0.8692661 |     0.1258278 | 0.7434382 |
| Australia     | ȶ       | consonant    | 0.7155963 |     0.1036424 | 0.6119539 |
| Australia     | ɻ       | consonant    | 0.6582569 |     0.1013245 | 0.5569324 |
| Australia     | ɳ       | consonant    | 0.6582569 |     0.1321192 | 0.5261377 |
| Australia     | ɭ       | consonant    | 0.6330275 |     0.1188742 | 0.5141534 |
| Australia     | ʈ       | consonant    | 0.5665138 |     0.1592715 | 0.4072422 |
| Australia     | n̪       | consonant    | 0.5756881 |     0.1764901 | 0.3991980 |
| Australia     | ȴ       | consonant    | 0.4633028 |     0.0668874 | 0.3964153 |
| Australia     | ŋ       | consonant    | 1.0000000 |     0.6284768 | 0.3715232 |
| Australia     | t̪       | consonant    | 0.4954128 |     0.2344371 | 0.2609758 |
| Eurasia       | pʰ      | consonant    | 0.5223881 |     0.1963576 | 0.3260304 |
| Eurasia       | kʰ      | consonant    | 0.5161692 |     0.2006623 | 0.3155069 |
| Eurasia       | d̪       | consonant    | 0.3582090 |     0.1440397 | 0.2141692 |
| Eurasia       | tʰ      | consonant    | 0.3097015 |     0.1337748 | 0.1759267 |
| Eurasia       | x       | consonant    | 0.3606965 |     0.1900662 | 0.1706303 |
| Eurasia       | b       | consonant    | 0.7997512 |     0.6311258 | 0.1686254 |
| Eurasia       | ɖ       | consonant    | 0.2425373 |     0.0850993 | 0.1574380 |
| Eurasia       | ɡ       | consonant    | 0.7213930 |     0.5668874 | 0.1545056 |
| Eurasia       | t̪ʰ      | consonant    | 0.1915423 |     0.0592715 | 0.1322708 |
| Eurasia       | ʒ       | consonant    | 0.2898010 |     0.1582781 | 0.1315228 |
| North America | ʔ       | consonant    | 0.8586957 |     0.3748344 | 0.4838612 |
| North America | kʼ      | consonant    | 0.4184783 |     0.0804636 | 0.3380147 |
| North America | h       | consonant    | 0.8478261 |     0.5639073 | 0.2839188 |
| North America | ʃ       | consonant    | 0.6413043 |     0.3655629 | 0.2757414 |
| North America | t̠ʃ      | consonant    | 0.6684783 |     0.4033113 | 0.2651670 |
| North America | t̠ʃʼ     | consonant    | 0.3152174 |     0.0612583 | 0.2539591 |
| North America | tsʼ     | consonant    | 0.2934783 |     0.0427152 | 0.2507630 |
| North America | tʼ      | consonant    | 0.2989130 |     0.0519868 | 0.2469263 |
| North America | kʷ      | consonant    | 0.3532609 |     0.1231788 | 0.2300821 |
| North America | pʼ      | consonant    | 0.2880435 |     0.0592715 | 0.2287720 |
| Papunesia     | ʔ       | consonant    | 0.5185185 |     0.3748344 | 0.1436841 |
| Papunesia     | s       | consonant    | 0.7592593 |     0.6692053 | 0.0900540 |
| Papunesia     | β       | consonant    | 0.1712963 |     0.1013245 | 0.0699718 |
| Papunesia     | d       | consonant    | 0.5185185 |     0.4556291 | 0.0628894 |
| Papunesia     | ɡ       | consonant    | 0.6250000 |     0.5668874 | 0.0581126 |
| Papunesia     | ŋ       | consonant    | 0.6851852 |     0.6284768 | 0.0567084 |
| Papunesia     | n̪\|n    | consonant    | 0.1064815 |     0.0533113 | 0.0531702 |
| Papunesia     | ɸ       | consonant    | 0.0972222 |     0.0506623 | 0.0465600 |
| Papunesia     | t̪\|t    | consonant    | 0.0972222 |     0.0506623 | 0.0465600 |
| Papunesia     | mb      | consonant    | 0.1481481 |     0.1046358 | 0.0435124 |
| South America | ɾ       | consonant    | 0.7244259 |     0.2562914 | 0.4681345 |
| South America | t       | consonant    | 0.9102296 |     0.6834437 | 0.2267859 |
| South America | t̠ʃ      | consonant    | 0.6263048 |     0.4033113 | 0.2229935 |
| South America | h       | consonant    | 0.7139875 |     0.5639073 | 0.1500802 |
| South America | ʔ       | consonant    | 0.5177453 |     0.3748344 | 0.1429109 |
| South America | ʃ       | consonant    | 0.4697286 |     0.3655629 | 0.1041657 |
| South America | ts      | consonant    | 0.3215031 |     0.2208609 | 0.1006422 |
| South America | p       | consonant    | 0.9457203 |     0.8586093 | 0.0871110 |
| South America | k       | consonant    | 0.9832985 |     0.9036424 | 0.0796562 |
| South America | β       | consonant    | 0.1711900 |     0.1013245 | 0.0698655 |

    neg <- all %>% group_by(macroarea) %>% filter(SegmentClass=="consonant") %>% slice_min(order_by = delta, n = 10)
    neg %>% kable()

| macroarea     | Phoneme | SegmentClass |      freq | phoible\_freq |      delta |
|:--------------|:--------|:-------------|----------:|--------------:|-----------:|
| Africa        | ʈ       | consonant    | 0.0259887 |     0.1592715 | -0.1332828 |
| Africa        | pʰ      | consonant    | 0.0632768 |     0.1963576 | -0.1330808 |
| Africa        | kʰ      | consonant    | 0.0677966 |     0.2006623 | -0.1328656 |
| Africa        | ɳ       | consonant    | 0.0033898 |     0.1321192 | -0.1287294 |
| Africa        | n̪       | consonant    | 0.0519774 |     0.1764901 | -0.1245127 |
| Africa        | ɾ       | consonant    | 0.1389831 |     0.2562914 | -0.1173083 |
| Africa        | ɭ       | consonant    | 0.0056497 |     0.1188742 | -0.1132245 |
| Africa        | t̪       | consonant    | 0.1333333 |     0.2344371 | -0.1011038 |
| Africa        | ɻ       | consonant    | 0.0045198 |     0.1013245 | -0.0968047 |
| Africa        | tʰ      | consonant    | 0.0576271 |     0.1337748 | -0.0761477 |
| Australia     | s       | consonant    | 0.0022936 |     0.6692053 | -0.6669117 |
| Australia     | b       | consonant    | 0.0229358 |     0.6311258 | -0.6081900 |
| Australia     | h       | consonant    | 0.0091743 |     0.5639073 | -0.5547330 |
| Australia     | ɡ       | consonant    | 0.0229358 |     0.5668874 | -0.5439516 |
| Australia     | d       | consonant    | 0.0183486 |     0.4556291 | -0.4372805 |
| Australia     | f       | consonant    | 0.0045872 |     0.4403974 | -0.4358102 |
| Australia     | t̠ʃ      | consonant    | 0.0022936 |     0.4033113 | -0.4010177 |
| Australia     | ɲ       | consonant    | 0.0389908 |     0.4158940 | -0.3769032 |
| Australia     | z       | consonant    | 0.0022936 |     0.2956954 | -0.2934018 |
| Australia     | d̠ʒ      | consonant    | 0.0022936 |     0.2715232 | -0.2692296 |
| Eurasia       | t       | consonant    | 0.4502488 |     0.6834437 | -0.2331950 |
| Eurasia       | w       | consonant    | 0.5945274 |     0.8221854 | -0.2276581 |
| Eurasia       | n       | consonant    | 0.6741294 |     0.7781457 | -0.1040163 |
| Eurasia       | ɻ       | consonant    | 0.0087065 |     0.1013245 | -0.0926180 |
| Eurasia       | kʷ      | consonant    | 0.0323383 |     0.1231788 | -0.0908405 |
| Eurasia       | ɓ       | consonant    | 0.0248756 |     0.0993377 | -0.0744621 |
| Eurasia       | mb      | consonant    | 0.0385572 |     0.1046358 | -0.0660785 |
| Eurasia       | d       | consonant    | 0.3917910 |     0.4556291 | -0.0638381 |
| Eurasia       | ŋɡ      | consonant    | 0.0323383 |     0.0960265 | -0.0636882 |
| Eurasia       | nd      | consonant    | 0.0348259 |     0.0970199 | -0.0621940 |
| North America | ŋ       | consonant    | 0.2445652 |     0.6284768 | -0.3839116 |
| North America | ɲ       | consonant    | 0.1141304 |     0.4158940 | -0.3017636 |
| North America | f       | consonant    | 0.2010870 |     0.4403974 | -0.2393104 |
| North America | ɡ       | consonant    | 0.3532609 |     0.5668874 | -0.2136265 |
| North America | r       | consonant    | 0.2391304 |     0.4410596 | -0.2019292 |
| North America | z       | consonant    | 0.1141304 |     0.2956954 | -0.1815649 |
| North America | v       | consonant    | 0.0923913 |     0.2701987 | -0.1778074 |
| North America | b       | consonant    | 0.4619565 |     0.6311258 | -0.1691693 |
| North America | d       | consonant    | 0.2934783 |     0.4556291 | -0.1621509 |
| North America | d̠ʒ      | consonant    | 0.1250000 |     0.2715232 | -0.1465232 |
| Papunesia     | ʃ       | consonant    | 0.0694444 |     0.3655629 | -0.2961185 |
| Papunesia     | z       | consonant    | 0.0740741 |     0.2956954 | -0.2216213 |
| Papunesia     | j       | consonant    | 0.7083333 |     0.8993377 | -0.1910044 |
| Papunesia     | v       | consonant    | 0.1018519 |     0.2701987 | -0.1683468 |
| Papunesia     | t̠ʃ      | consonant    | 0.2407407 |     0.4033113 | -0.1625705 |
| Papunesia     | ɲ       | consonant    | 0.2592593 |     0.4158940 | -0.1566348 |
| Papunesia     | ts      | consonant    | 0.0648148 |     0.2208609 | -0.1560461 |
| Papunesia     | ʒ       | consonant    | 0.0138889 |     0.1582781 | -0.1443893 |
| Papunesia     | pʰ      | consonant    | 0.0601852 |     0.1963576 | -0.1361724 |
| Papunesia     | kʰ      | consonant    | 0.0648148 |     0.2006623 | -0.1358474 |
| South America | ŋ       | consonant    | 0.2150313 |     0.6284768 | -0.4134455 |
| South America | r       | consonant    | 0.0605428 |     0.4410596 | -0.3805168 |
| South America | f       | consonant    | 0.0751566 |     0.4403974 | -0.3652408 |
| South America | l       | consonant    | 0.3235908 |     0.6768212 | -0.3532304 |
| South America | ɡ       | consonant    | 0.2922756 |     0.5668874 | -0.2746118 |
| South America | v       | consonant    | 0.0313152 |     0.2701987 | -0.2388834 |
| South America | z       | consonant    | 0.0647182 |     0.2956954 | -0.2309772 |
| South America | b       | consonant    | 0.4405010 |     0.6311258 | -0.1906248 |
| South America | t̪       | consonant    | 0.0459290 |     0.2344371 | -0.1885081 |
| South America | n̪       | consonant    | 0.0187891 |     0.1764901 | -0.1577009 |

Tables for the paper
====================

    pos <- pos %>% select(macroarea, Phoneme, freq, phoible_freq, delta)
    print(xtable(pos), include.rownames=FALSE)

    ## % latex table generated in R 4.0.2 by xtable 1.8-4 package
    ## % Wed Oct  7 11:40:35 2020
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{llrrr}
    ##   \hline
    ## macroarea & Phoneme & freq & phoible\_freq & delta \\ 
    ##   \hline
    ## Africa & f & 0.84 & 0.44 & 0.40 \\ 
    ##   Africa & d & 0.80 & 0.46 & 0.35 \\ 
    ##   Africa & ɡ & 0.88 & 0.57 & 0.31 \\ 
    ##   Africa & ɡb & 0.42 & 0.12 & 0.30 \\ 
    ##   Africa & kp & 0.42 & 0.12 & 0.30 \\ 
    ##   Africa & ɲ & 0.71 & 0.42 & 0.29 \\ 
    ##   Africa & b & 0.91 & 0.63 & 0.28 \\ 
    ##   Africa & z & 0.56 & 0.30 & 0.26 \\ 
    ##   Africa & v & 0.52 & 0.27 & 0.25 \\ 
    ##   Africa & s & 0.89 & 0.67 & 0.22 \\ 
    ##   Australia & ȵ & 0.87 & 0.13 & 0.74 \\ 
    ##   Australia & ȶ & 0.72 & 0.10 & 0.61 \\ 
    ##   Australia & ɻ & 0.66 & 0.10 & 0.56 \\ 
    ##   Australia & ɳ & 0.66 & 0.13 & 0.53 \\ 
    ##   Australia & ɭ & 0.63 & 0.12 & 0.51 \\ 
    ##   Australia & ʈ & 0.57 & 0.16 & 0.41 \\ 
    ##   Australia & n̪ & 0.58 & 0.18 & 0.40 \\ 
    ##   Australia & ȴ & 0.46 & 0.07 & 0.40 \\ 
    ##   Australia & ŋ & 1.00 & 0.63 & 0.37 \\ 
    ##   Australia & t̪ & 0.50 & 0.23 & 0.26 \\ 
    ##   Eurasia & pʰ & 0.52 & 0.20 & 0.33 \\ 
    ##   Eurasia & kʰ & 0.52 & 0.20 & 0.32 \\ 
    ##   Eurasia & d̪ & 0.36 & 0.14 & 0.21 \\ 
    ##   Eurasia & tʰ & 0.31 & 0.13 & 0.18 \\ 
    ##   Eurasia & x & 0.36 & 0.19 & 0.17 \\ 
    ##   Eurasia & b & 0.80 & 0.63 & 0.17 \\ 
    ##   Eurasia & ɖ & 0.24 & 0.09 & 0.16 \\ 
    ##   Eurasia & ɡ & 0.72 & 0.57 & 0.15 \\ 
    ##   Eurasia & t̪ʰ & 0.19 & 0.06 & 0.13 \\ 
    ##   Eurasia & ʒ & 0.29 & 0.16 & 0.13 \\ 
    ##   North America & ʔ & 0.86 & 0.37 & 0.48 \\ 
    ##   North America & kʼ & 0.42 & 0.08 & 0.34 \\ 
    ##   North America & h & 0.85 & 0.56 & 0.28 \\ 
    ##   North America & ʃ & 0.64 & 0.37 & 0.28 \\ 
    ##   North America & t̠ʃ & 0.67 & 0.40 & 0.27 \\ 
    ##   North America & t̠ʃʼ & 0.32 & 0.06 & 0.25 \\ 
    ##   North America & tsʼ & 0.29 & 0.04 & 0.25 \\ 
    ##   North America & tʼ & 0.30 & 0.05 & 0.25 \\ 
    ##   North America & kʷ & 0.35 & 0.12 & 0.23 \\ 
    ##   North America & pʼ & 0.29 & 0.06 & 0.23 \\ 
    ##   Papunesia & ʔ & 0.52 & 0.37 & 0.14 \\ 
    ##   Papunesia & s & 0.76 & 0.67 & 0.09 \\ 
    ##   Papunesia & β & 0.17 & 0.10 & 0.07 \\ 
    ##   Papunesia & d & 0.52 & 0.46 & 0.06 \\ 
    ##   Papunesia & ɡ & 0.62 & 0.57 & 0.06 \\ 
    ##   Papunesia & ŋ & 0.69 & 0.63 & 0.06 \\ 
    ##   Papunesia & n̪$|$n & 0.11 & 0.05 & 0.05 \\ 
    ##   Papunesia & ɸ & 0.10 & 0.05 & 0.05 \\ 
    ##   Papunesia & t̪$|$t & 0.10 & 0.05 & 0.05 \\ 
    ##   Papunesia & mb & 0.15 & 0.10 & 0.04 \\ 
    ##   South America & ɾ & 0.72 & 0.26 & 0.47 \\ 
    ##   South America & t & 0.91 & 0.68 & 0.23 \\ 
    ##   South America & t̠ʃ & 0.63 & 0.40 & 0.22 \\ 
    ##   South America & h & 0.71 & 0.56 & 0.15 \\ 
    ##   South America & ʔ & 0.52 & 0.37 & 0.14 \\ 
    ##   South America & ʃ & 0.47 & 0.37 & 0.10 \\ 
    ##   South America & ts & 0.32 & 0.22 & 0.10 \\ 
    ##   South America & p & 0.95 & 0.86 & 0.09 \\ 
    ##   South America & k & 0.98 & 0.90 & 0.08 \\ 
    ##   South America & β & 0.17 & 0.10 & 0.07 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

    neg <- neg %>% select(macroarea, Phoneme, freq, phoible_freq, delta)
    print(xtable(neg), include.rownames=FALSE)

    ## % latex table generated in R 4.0.2 by xtable 1.8-4 package
    ## % Wed Oct  7 11:40:35 2020
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{llrrr}
    ##   \hline
    ## macroarea & Phoneme & freq & phoible\_freq & delta \\ 
    ##   \hline
    ## Africa & ʈ & 0.03 & 0.16 & -0.13 \\ 
    ##   Africa & pʰ & 0.06 & 0.20 & -0.13 \\ 
    ##   Africa & kʰ & 0.07 & 0.20 & -0.13 \\ 
    ##   Africa & ɳ & 0.00 & 0.13 & -0.13 \\ 
    ##   Africa & n̪ & 0.05 & 0.18 & -0.12 \\ 
    ##   Africa & ɾ & 0.14 & 0.26 & -0.12 \\ 
    ##   Africa & ɭ & 0.01 & 0.12 & -0.11 \\ 
    ##   Africa & t̪ & 0.13 & 0.23 & -0.10 \\ 
    ##   Africa & ɻ & 0.00 & 0.10 & -0.10 \\ 
    ##   Africa & tʰ & 0.06 & 0.13 & -0.08 \\ 
    ##   Australia & s & 0.00 & 0.67 & -0.67 \\ 
    ##   Australia & b & 0.02 & 0.63 & -0.61 \\ 
    ##   Australia & h & 0.01 & 0.56 & -0.55 \\ 
    ##   Australia & ɡ & 0.02 & 0.57 & -0.54 \\ 
    ##   Australia & d & 0.02 & 0.46 & -0.44 \\ 
    ##   Australia & f & 0.00 & 0.44 & -0.44 \\ 
    ##   Australia & t̠ʃ & 0.00 & 0.40 & -0.40 \\ 
    ##   Australia & ɲ & 0.04 & 0.42 & -0.38 \\ 
    ##   Australia & z & 0.00 & 0.30 & -0.29 \\ 
    ##   Australia & d̠ʒ & 0.00 & 0.27 & -0.27 \\ 
    ##   Eurasia & t & 0.45 & 0.68 & -0.23 \\ 
    ##   Eurasia & w & 0.59 & 0.82 & -0.23 \\ 
    ##   Eurasia & n & 0.67 & 0.78 & -0.10 \\ 
    ##   Eurasia & ɻ & 0.01 & 0.10 & -0.09 \\ 
    ##   Eurasia & kʷ & 0.03 & 0.12 & -0.09 \\ 
    ##   Eurasia & ɓ & 0.02 & 0.10 & -0.07 \\ 
    ##   Eurasia & mb & 0.04 & 0.10 & -0.07 \\ 
    ##   Eurasia & d & 0.39 & 0.46 & -0.06 \\ 
    ##   Eurasia & ŋɡ & 0.03 & 0.10 & -0.06 \\ 
    ##   Eurasia & nd & 0.03 & 0.10 & -0.06 \\ 
    ##   North America & ŋ & 0.24 & 0.63 & -0.38 \\ 
    ##   North America & ɲ & 0.11 & 0.42 & -0.30 \\ 
    ##   North America & f & 0.20 & 0.44 & -0.24 \\ 
    ##   North America & ɡ & 0.35 & 0.57 & -0.21 \\ 
    ##   North America & r & 0.24 & 0.44 & -0.20 \\ 
    ##   North America & z & 0.11 & 0.30 & -0.18 \\ 
    ##   North America & v & 0.09 & 0.27 & -0.18 \\ 
    ##   North America & b & 0.46 & 0.63 & -0.17 \\ 
    ##   North America & d & 0.29 & 0.46 & -0.16 \\ 
    ##   North America & d̠ʒ & 0.12 & 0.27 & -0.15 \\ 
    ##   Papunesia & ʃ & 0.07 & 0.37 & -0.30 \\ 
    ##   Papunesia & z & 0.07 & 0.30 & -0.22 \\ 
    ##   Papunesia & j & 0.71 & 0.90 & -0.19 \\ 
    ##   Papunesia & v & 0.10 & 0.27 & -0.17 \\ 
    ##   Papunesia & t̠ʃ & 0.24 & 0.40 & -0.16 \\ 
    ##   Papunesia & ɲ & 0.26 & 0.42 & -0.16 \\ 
    ##   Papunesia & ts & 0.06 & 0.22 & -0.16 \\ 
    ##   Papunesia & ʒ & 0.01 & 0.16 & -0.14 \\ 
    ##   Papunesia & pʰ & 0.06 & 0.20 & -0.14 \\ 
    ##   Papunesia & kʰ & 0.06 & 0.20 & -0.14 \\ 
    ##   South America & ŋ & 0.22 & 0.63 & -0.41 \\ 
    ##   South America & r & 0.06 & 0.44 & -0.38 \\ 
    ##   South America & f & 0.08 & 0.44 & -0.37 \\ 
    ##   South America & l & 0.32 & 0.68 & -0.35 \\ 
    ##   South America & ɡ & 0.29 & 0.57 & -0.27 \\ 
    ##   South America & v & 0.03 & 0.27 & -0.24 \\ 
    ##   South America & z & 0.06 & 0.30 & -0.23 \\ 
    ##   South America & b & 0.44 & 0.63 & -0.19 \\ 
    ##   South America & t̪ & 0.05 & 0.23 & -0.19 \\ 
    ##   South America & n̪ & 0.02 & 0.18 & -0.16 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

    library(xtable)
    print(xtable(all %>% group_by(macroarea) %>% slice_max(order_by = delta, n = 10)))

    ## % latex table generated in R 4.0.2 by xtable 1.8-4 package
    ## % Wed Oct  7 11:40:35 2020
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlllrrr}
    ##   \hline
    ##  & macroarea & Phoneme & SegmentClass & freq & phoible\_freq & delta \\ 
    ##   \hline
    ## 1 & Africa & f & consonant & 0.84 & 0.44 & 0.40 \\ 
    ##   2 & Africa & ˨ & tone & 0.58 & 0.18 & 0.40 \\ 
    ##   3 & Africa & ˦ & tone & 0.58 & 0.18 & 0.40 \\ 
    ##   4 & Africa & d & consonant & 0.80 & 0.46 & 0.35 \\ 
    ##   5 & Africa & ɡ & consonant & 0.88 & 0.57 & 0.31 \\ 
    ##   6 & Africa & ɔ & vowel & 0.66 & 0.35 & 0.31 \\ 
    ##   7 & Africa & ɡb & consonant & 0.42 & 0.12 & 0.30 \\ 
    ##   8 & Africa & kp & consonant & 0.42 & 0.12 & 0.30 \\ 
    ##   9 & Africa & ɲ & consonant & 0.71 & 0.42 & 0.29 \\ 
    ##   10 & Africa & b & consonant & 0.91 & 0.63 & 0.28 \\ 
    ##   11 & Australia & ȵ & consonant & 0.87 & 0.13 & 0.74 \\ 
    ##   12 & Australia & ȶ & consonant & 0.72 & 0.10 & 0.61 \\ 
    ##   13 & Australia & ɻ & consonant & 0.66 & 0.10 & 0.56 \\ 
    ##   14 & Australia & ɳ & consonant & 0.66 & 0.13 & 0.53 \\ 
    ##   15 & Australia & ɭ & consonant & 0.63 & 0.12 & 0.51 \\ 
    ##   16 & Australia & ʈ & consonant & 0.57 & 0.16 & 0.41 \\ 
    ##   17 & Australia & n̪ & consonant & 0.58 & 0.18 & 0.40 \\ 
    ##   18 & Australia & ȴ & consonant & 0.46 & 0.07 & 0.40 \\ 
    ##   19 & Australia & ŋ & consonant & 1.00 & 0.63 & 0.37 \\ 
    ##   20 & Australia & t̪ & consonant & 0.50 & 0.23 & 0.26 \\ 
    ##   21 & Eurasia & pʰ & consonant & 0.52 & 0.20 & 0.33 \\ 
    ##   22 & Eurasia & kʰ & consonant & 0.52 & 0.20 & 0.32 \\ 
    ##   23 & Eurasia & d̪ & consonant & 0.36 & 0.14 & 0.21 \\ 
    ##   24 & Eurasia & tʰ & consonant & 0.31 & 0.13 & 0.18 \\ 
    ##   25 & Eurasia & x & consonant & 0.36 & 0.19 & 0.17 \\ 
    ##   26 & Eurasia & b & consonant & 0.80 & 0.63 & 0.17 \\ 
    ##   27 & Eurasia & ɖ & consonant & 0.24 & 0.09 & 0.16 \\ 
    ##   28 & Eurasia & ɡ & consonant & 0.72 & 0.57 & 0.15 \\ 
    ##   29 & Eurasia & t̪ʰ & consonant & 0.19 & 0.06 & 0.13 \\ 
    ##   30 & Eurasia & ʒ & consonant & 0.29 & 0.16 & 0.13 \\ 
    ##   31 & North America & ʔ & consonant & 0.86 & 0.37 & 0.48 \\ 
    ##   32 & North America & kʼ & consonant & 0.42 & 0.08 & 0.34 \\ 
    ##   33 & North America & h & consonant & 0.85 & 0.56 & 0.28 \\ 
    ##   34 & North America & ʃ & consonant & 0.64 & 0.37 & 0.28 \\ 
    ##   35 & North America & t̠ʃ & consonant & 0.67 & 0.40 & 0.27 \\ 
    ##   36 & North America & t̠ʃʼ & consonant & 0.32 & 0.06 & 0.25 \\ 
    ##   37 & North America & tsʼ & consonant & 0.29 & 0.04 & 0.25 \\ 
    ##   38 & North America & tʼ & consonant & 0.30 & 0.05 & 0.25 \\ 
    ##   39 & North America & kʷ & consonant & 0.35 & 0.12 & 0.23 \\ 
    ##   40 & North America & pʼ & consonant & 0.29 & 0.06 & 0.23 \\ 
    ##   41 & Papunesia & ʔ & consonant & 0.52 & 0.37 & 0.14 \\ 
    ##   42 & Papunesia & o̞ & vowel & 0.20 & 0.10 & 0.10 \\ 
    ##   43 & Papunesia & s & consonant & 0.76 & 0.67 & 0.09 \\ 
    ##   44 & Papunesia & β & consonant & 0.17 & 0.10 & 0.07 \\ 
    ##   45 & Papunesia & e̞ & vowel & 0.16 & 0.09 & 0.06 \\ 
    ##   46 & Papunesia & d & consonant & 0.52 & 0.46 & 0.06 \\ 
    ##   47 & Papunesia & u & vowel & 0.94 & 0.88 & 0.06 \\ 
    ##   48 & Papunesia & ɡ & consonant & 0.62 & 0.57 & 0.06 \\ 
    ##   49 & Papunesia & ŋ & consonant & 0.69 & 0.63 & 0.06 \\ 
    ##   50 & Papunesia & n̪$|$n & consonant & 0.11 & 0.05 & 0.05 \\ 
    ##   51 & South America & ɾ & consonant & 0.72 & 0.26 & 0.47 \\ 
    ##   52 & South America & ɨ & vowel & 0.51 & 0.16 & 0.35 \\ 
    ##   53 & South America & ã & vowel & 0.42 & 0.17 & 0.25 \\ 
    ##   54 & South America & ĩ & vowel & 0.42 & 0.18 & 0.24 \\ 
    ##   55 & South America & t & consonant & 0.91 & 0.68 & 0.23 \\ 
    ##   56 & South America & t̠ʃ & consonant & 0.63 & 0.40 & 0.22 \\ 
    ##   57 & South America & ɨ̃ & vowel & 0.25 & 0.04 & 0.21 \\ 
    ##   58 & South America & ẽ & vowel & 0.29 & 0.11 & 0.19 \\ 
    ##   59 & South America & õ & vowel & 0.30 & 0.11 & 0.18 \\ 
    ##   60 & South America & ũ & vowel & 0.34 & 0.16 & 0.17 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

    print(xtable(all %>% group_by(macroarea) %>% slice_min(order_by = delta, n = 10)))

    ## % latex table generated in R 4.0.2 by xtable 1.8-4 package
    ## % Wed Oct  7 11:40:35 2020
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlllrrr}
    ##   \hline
    ##  & macroarea & Phoneme & SegmentClass & freq & phoible\_freq & delta \\ 
    ##   \hline
    ## 1 & Africa & ʈ & consonant & 0.03 & 0.16 & -0.13 \\ 
    ##   2 & Africa & pʰ & consonant & 0.06 & 0.20 & -0.13 \\ 
    ##   3 & Africa & kʰ & consonant & 0.07 & 0.20 & -0.13 \\ 
    ##   4 & Africa & ɳ & consonant & 0.00 & 0.13 & -0.13 \\ 
    ##   5 & Africa & n̪ & consonant & 0.05 & 0.18 & -0.12 \\ 
    ##   6 & Africa & ɾ & consonant & 0.14 & 0.26 & -0.12 \\ 
    ##   7 & Africa & ɭ & consonant & 0.01 & 0.12 & -0.11 \\ 
    ##   8 & Africa & t̪ & consonant & 0.13 & 0.23 & -0.10 \\ 
    ##   9 & Africa & ɻ & consonant & 0.00 & 0.10 & -0.10 \\ 
    ##   10 & Africa & tʰ & consonant & 0.06 & 0.13 & -0.08 \\ 
    ##   11 & Australia & s & consonant & 0.00 & 0.67 & -0.67 \\ 
    ##   12 & Australia & b & consonant & 0.02 & 0.63 & -0.61 \\ 
    ##   13 & Australia & h & consonant & 0.01 & 0.56 & -0.55 \\ 
    ##   14 & Australia & ɡ & consonant & 0.02 & 0.57 & -0.54 \\ 
    ##   15 & Australia & d & consonant & 0.02 & 0.46 & -0.44 \\ 
    ##   16 & Australia & f & consonant & 0.00 & 0.44 & -0.44 \\ 
    ##   17 & Australia & t̠ʃ & consonant & 0.00 & 0.40 & -0.40 \\ 
    ##   18 & Australia & ɲ & consonant & 0.04 & 0.42 & -0.38 \\ 
    ##   19 & Australia & o & vowel & 0.24 & 0.60 & -0.37 \\ 
    ##   20 & Australia & ɛ & vowel & 0.03 & 0.37 & -0.35 \\ 
    ##   21 & Eurasia & t & consonant & 0.45 & 0.68 & -0.23 \\ 
    ##   22 & Eurasia & w & consonant & 0.59 & 0.82 & -0.23 \\ 
    ##   23 & Eurasia & a & vowel & 0.64 & 0.86 & -0.22 \\ 
    ##   24 & Eurasia & ˨ & tone & 0.02 & 0.18 & -0.16 \\ 
    ##   25 & Eurasia & ˦ & tone & 0.02 & 0.18 & -0.16 \\ 
    ##   26 & Eurasia & n & consonant & 0.67 & 0.78 & -0.10 \\ 
    ##   27 & Eurasia & ɻ & consonant & 0.01 & 0.10 & -0.09 \\ 
    ##   28 & Eurasia & i & vowel & 0.83 & 0.92 & -0.09 \\ 
    ##   29 & Eurasia & kʷ & consonant & 0.03 & 0.12 & -0.09 \\ 
    ##   30 & Eurasia & ã & vowel & 0.08 & 0.17 & -0.09 \\ 
    ##   31 & North America & ŋ & consonant & 0.24 & 0.63 & -0.38 \\ 
    ##   32 & North America & ɲ & consonant & 0.11 & 0.42 & -0.30 \\ 
    ##   33 & North America & e & vowel & 0.36 & 0.61 & -0.25 \\ 
    ##   34 & North America & f & consonant & 0.20 & 0.44 & -0.24 \\ 
    ##   35 & North America & ɡ & consonant & 0.35 & 0.57 & -0.21 \\ 
    ##   36 & North America & r & consonant & 0.24 & 0.44 & -0.20 \\ 
    ##   37 & North America & z & consonant & 0.11 & 0.30 & -0.18 \\ 
    ##   38 & North America & v & consonant & 0.09 & 0.27 & -0.18 \\ 
    ##   39 & North America & b & consonant & 0.46 & 0.63 & -0.17 \\ 
    ##   40 & North America & d & consonant & 0.29 & 0.46 & -0.16 \\ 
    ##   41 & Papunesia & ʃ & consonant & 0.07 & 0.37 & -0.30 \\ 
    ##   42 & Papunesia & z & consonant & 0.07 & 0.30 & -0.22 \\ 
    ##   43 & Papunesia & iː & vowel & 0.12 & 0.32 & -0.19 \\ 
    ##   44 & Papunesia & j & consonant & 0.71 & 0.90 & -0.19 \\ 
    ##   45 & Papunesia & uː & vowel & 0.11 & 0.29 & -0.18 \\ 
    ##   46 & Papunesia & aː & vowel & 0.12 & 0.30 & -0.17 \\ 
    ##   47 & Papunesia & v & consonant & 0.10 & 0.27 & -0.17 \\ 
    ##   48 & Papunesia & ˦ & tone & 0.02 & 0.18 & -0.16 \\ 
    ##   49 & Papunesia & t̠ʃ & consonant & 0.24 & 0.40 & -0.16 \\ 
    ##   50 & Papunesia & ˨ & tone & 0.02 & 0.18 & -0.16 \\ 
    ##   51 & South America & ŋ & consonant & 0.22 & 0.63 & -0.41 \\ 
    ##   52 & South America & r & consonant & 0.06 & 0.44 & -0.38 \\ 
    ##   53 & South America & f & consonant & 0.08 & 0.44 & -0.37 \\ 
    ##   54 & South America & l & consonant & 0.32 & 0.68 & -0.35 \\ 
    ##   55 & South America & ɡ & consonant & 0.29 & 0.57 & -0.27 \\ 
    ##   56 & South America & ɔ & vowel & 0.11 & 0.35 & -0.24 \\ 
    ##   57 & South America & v & consonant & 0.03 & 0.27 & -0.24 \\ 
    ##   58 & South America & z & consonant & 0.06 & 0.30 & -0.23 \\ 
    ##   59 & South America & ɛ & vowel & 0.16 & 0.37 & -0.22 \\ 
    ##   60 & South America & b & consonant & 0.44 & 0.63 & -0.19 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}
