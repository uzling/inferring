Data prep for JSD analysis with PHOIBLE, BDPROTO and SegBo
================
Steven Moran

09 October, 2020

    library(tidyverse)
    library(knitr)

Overview
========

This script create various dataframes from the [PHOIBLE
Online](https://phoible.org/),
[BDPROTO](https://github.com/bdproto/bdproto),
[SegBo](https://github.com/segbo-db/segbo) datasets for Jensen-Shannon
divergence [analysis](../analysis/jsd_testing.md).

In each of the three databases, inventories and borrowed segments were
interpreted by experts. Because linguists tend to differ in their
idiosyncratic use of phonetic and phonemic transcription practices,
segments in each database were codified according to standardized
Unicode conventions (Moran & Cysouw, 2018) for the International
Phonetic Alphabet (International Phonetic Association 1999) and
typologized into a [well-defined phonetic notational
convention](http://phoible.github.io/conventions/).

To make the data from the three resources comparable, we had to extend
the datasets to include additional metadata not currently available in
these resources. For example, to compare language family level
phonological inventories in BDPROTO against the current daughter
languages of those families represented in PHOIBLE, we had to code the
data points in BDPROTO for language family “root”’’" Glottolog codes
(“Glottocode”), when they are available. For example, BDPROTO contains
multiple reconstructions for intermediate reconstructions of
Indo-European, e.g. Proto-Germanic, Proto-Italic, Proto-Slavic. Each
reconstruction is situated within the larger Indo-European language
family tree. As such, we tagged each intermediate reconstruction with a
Glottocode (e.g. Proto-Germanic is assigned
[germ1287](https://glottolog.org/resource/languoid/id/germ1287), but
also its “LanguageFamilyRoot” Glottocode, i.e. Indo-European
[indo1319](https://glottolog.org/resource/languoid/id/indo1319). This
allows us not only to compare segments and inventories from the daughter
languages and their reconstructed proto-language, but it also us to
randomly sample from the pool of languages within a particular branch in
the language phylogeny. The latter is a first step towards addressing
temporal bias. We discuss our random sampling procedures in the Methods
section of our paper. Tagging each database for family-level Glottocodes
also allows us to take the intersection between BDPROTO and PHOIBLE, so
that we can not only compare the distribution of segments in both full
language samples, but also just between the language families that they
share.

The three datasets used in our paper were also expanded to include a
geographic classification based on so-called macro-areas, as defined in
[Glottolog](https://glottolog.org/). These include Africa, Australia,
Eurasia, North America, South America, and Papunesia (cf. Hammarstrom,
2014). Comparative linguists use macro-area categories to identify or
rule out language contact as a factor in the typological distribution of
linguistic features. Languages are the way they are today due to two
factors: genealogical descent, i.e. the aspects of a language, such as
its phonological inventory or grammatical properties, are passed from
one generation to the next; and language contact, i.e. interaction
between different language speaking groups can result in the exchange of
linguistic material through processes such as lexical and grammatical
borrowing. Whereas the former factor can be investigated by applying the
historical-comparative method to modern-day vocabularies to reconstruct
past languages and cultures, the latter factor is more difficult because
of the lack of information on human contact patterns in the historical
record (and in particular what those different groups may have spoken,
since in pre-history speech leaves no archaeological traces).
Nevertheless, language contact is crucial for the evolutionary
understanding how languages have become the way they are today. Hence,
when undertaking statistical inference on linguistic features from
large-scale databases, macro-areas are often taken into account as a
random factor in statistical models to account for issues such as
auto-correlation, e.g. Blasi et al., 2019.

Lastly, the data preparation simply involved extracting various
dataframes from the three datasets for analysis. For example, one
dataframe contains all languages’ segments, their language families, and
the macro-areas areas in which they are spoken per database. This allows
us to resample the datasets at different levels, e.g. one language per
family, one segment per language, etc. Another dataframe includes simply
the number of counts per segment per database and their probability of
occurrence in each dataset. These data are used as input to the methods
we use, including Jensen-Shannon divergence, to measure the difference
between the probability distribution of segments in the different
databases. In the next Section, we discuss the methods we use for
comparative analysis.

Data for resampling: all segments, with families and macroareas
===============================================================

    phoible <- read_csv(url('https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true'), col_types=c(InventoryID='i', Marginal='l', .default='c'))

    load("./glottolog-families-isolates.Rdata")

    # Add the ISO data
    phoible_enriched = merge(phoible, families.glottocodes[, c("iso639P3code", "family_id")], by.x="ISO6393", by.y="iso639P3code")

    # Add in the macroareas
    languages.geo <- ungroup(languages.geo)
    geo <- languages.geo %>% select(isocodes, macroarea)
    phoible_enriched <- left_join(phoible_enriched, geo, by=c("ISO6393"="isocodes"))

    # Check that there are no NAs
    phoible_enriched %>% filter(is.na(macroarea))

    ##  [1] ISO6393                InventoryID            Glottocode            
    ##  [4] LanguageName           SpecificDialect        GlyphID               
    ##  [7] Phoneme                Allophones             Marginal              
    ## [10] SegmentClass           Source                 tone                  
    ## [13] stress                 syllabic               short                 
    ## [16] long                   consonantal            sonorant              
    ## [19] continuant             delayedRelease         approximant           
    ## [22] tap                    trill                  nasal                 
    ## [25] lateral                labial                 round                 
    ## [28] labiodental            coronal                anterior              
    ## [31] distributed            strident               dorsal                
    ## [34] high                   low                    front                 
    ## [37] back                   tense                  retractedTongueRoot   
    ## [40] advancedTongueRoot     periodicGlottalSource  epilaryngealSource    
    ## [43] spreadGlottis          constrictedGlottis     fortis                
    ## [46] raisedLarynxEjective   loweredLarynxImplosive click                 
    ## [49] family_id              macroarea             
    ## <0 rows> (or 0-length row.names)

    phoible_enriched %>% filter(macroarea=="")

    ##  [1] ISO6393                InventoryID            Glottocode            
    ##  [4] LanguageName           SpecificDialect        GlyphID               
    ##  [7] Phoneme                Allophones             Marginal              
    ## [10] SegmentClass           Source                 tone                  
    ## [13] stress                 syllabic               short                 
    ## [16] long                   consonantal            sonorant              
    ## [19] continuant             delayedRelease         approximant           
    ## [22] tap                    trill                  nasal                 
    ## [25] lateral                labial                 round                 
    ## [28] labiodental            coronal                anterior              
    ## [31] distributed            strident               dorsal                
    ## [34] high                   low                    front                 
    ## [37] back                   tense                  retractedTongueRoot   
    ## [40] advancedTongueRoot     periodicGlottalSource  epilaryngealSource    
    ## [43] spreadGlottis          constrictedGlottis     fortis                
    ## [46] raisedLarynxEjective   loweredLarynxImplosive click                 
    ## [49] family_id              macroarea             
    ## <0 rows> (or 0-length row.names)

    table(phoible_enriched$macroarea, exclude = FALSE)

    ## 
    ##        Africa     Australia       Eurasia North America     Papunesia 
    ##         35731          8678         32959          5785          5112 
    ## South America 
    ##         10592

    # Get all segment values
    segment_value <- phoible %>% select(Phoneme, SegmentClass) %>% distinct()

    # We have some duplicates to clean up...
    phoible_enriched %>% select(InventoryID, Phoneme) %>% group_by(InventoryID, Phoneme) %>% filter(n()>1)

    ## # A tibble: 0 x 2
    ## # Groups:   InventoryID, Phoneme [0]
    ## # … with 2 variables: InventoryID <int>, Phoneme <chr>

    # Clean up
    rm(phoible, languages.geo, isolates, families.counts, families.glottocodes, geo)

    # load and manipulate bdproto
    load('bdproto.Rdata')
    bdproto <- inventories

    # Let's exlude the questionable language families, e.g. Nostratic
    exclude <- read_csv('what_to_exclude.csv')

    ## Parsed with column specification:
    ## cols(
    ##   BdprotoID = col_double(),
    ##   Exclude = col_logical()
    ## )

    bdproto <- left_join(bdproto, exclude)

    ## Joining, by = "BdprotoID"

    bdproto <- bdproto %>% filter(!Exclude)

    # Let's do some column renaming for convenience sake
    bdproto <- bdproto %>% rename(family_id = FamilyID)
    bdproto <- bdproto %>% rename(InventoryID = BdprotoID)
    bdproto <- bdproto %>% rename(macroarea = Macroarea)

    # Check that the macroareas are sane
    table(bdproto$macroarea, exclude = FALSE)

    ## 
    ##        Africa     Australia       Eurasia North America     Papunesia 
    ##           978           141          3254          1713           470 
    ## South America 
    ##           454

    # Any dupliccate segments?
    bdproto %>% select(InventoryID, Phoneme) %>% group_by(InventoryID, Phoneme) %>% filter(n()>1)

    ## # A tibble: 0 x 2
    ## # Groups:   InventoryID, Phoneme [0]
    ## # … with 2 variables: InventoryID <dbl>, Phoneme <chr>

    # Clean up
    rm(exclude, inventories)

    # load and extend segbo
    segbo <- read_csv('segbo_with_glottolog.csv')

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   InventoryID = col_double(),
    ##   PhoibleID = col_double(),
    ##   ClosestNeighbor = col_logical(),
    ##   bookkeeping = col_logical(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   description = col_logical(),
    ##   markup_description = col_logical(),
    ##   child_family_count = col_double(),
    ##   child_language_count = col_double(),
    ##   child_dialect_count = col_double()
    ## )

    ## See spec(...) for full column specifications.

    segbo <- segbo %>% rename(Phoneme = BorrowedSound)
    extra <- read_csv('segbo_missing_codes.csv')

    ## Parsed with column specification:
    ## cols(
    ##   LanguageName = col_character(),
    ##   family_id = col_character()
    ## )

    # Quick hack to get in the hand curated codes
    library(ddpcr)

    ## 
    ## Attaching package: 'ddpcr'

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    segbo_enriched <- merge_dfs_overwrite_col(segbo, extra, cols = "family_id", bycol = "LanguageName")

    ## Warning: `rename_()` is deprecated as of dplyr 0.7.0.
    ## Please use `rename()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `select_()` is deprecated as of dplyr 0.7.0.
    ## Please use `select()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    rm(segbo, extra)

    # Add the macroareas
    load("./glottolog-families-isolates.Rdata")
    languages.geo <- as.data.frame(ungroup(languages.geo))
    macroareas <- languages.geo %>% select(glottocode, macroarea)

    # Some are NAs from the input / glottolog data inclusion
    table(segbo_enriched$macroarea, exclude = FALSE)

    ## 
    ##        Africa     Australia       Eurasia North America     Papunesia 
    ##           207            38           509           168           583 
    ## South America          <NA> 
    ##           135            26

    segbo_enriched %>% select(InventoryID, Glottocode, macroarea) %>% distinct() %>% filter(is.na(macroarea))

    ## # A tibble: 10 x 3
    ##    InventoryID Glottocode macroarea
    ##          <dbl> <chr>      <chr>    
    ##  1          38 azer1255   <NA>     
    ##  2         154 mong1331   <NA>     
    ##  3         224 taki1251   <NA>     
    ##  4         240 ttgr1237   <NA>     
    ##  5         254 uzbe1247   <NA>     
    ##  6         310 qelt1235   <NA>     
    ##  7         325 waal1238   <NA>     
    ##  8         331 jami1236   <NA>     
    ##  9         538 mait1255   <NA>     
    ## 10         542 pash1269   <NA>

    # We fixed these for the time being by hand
    missing_codes_for_macroarea <- read_csv('segbo_update_macroareas.csv')

    ## Parsed with column specification:
    ## cols(
    ##   InventoryID = col_double(),
    ##   Glottocode = col_character(),
    ##   macroarea = col_character()
    ## )

    missing_codes_for_macroarea <- missing_codes_for_macroarea %>% select(InventoryID, macroarea)
    segbo_enriched <- merge_dfs_overwrite_col(segbo_enriched, missing_codes_for_macroarea, cols = "macroarea", bycol = "InventoryID")

    # Any NAs left? Should be no.
    table(segbo_enriched$macroarea, exclude = FALSE)

    ## 
    ##        Africa     Australia       Eurasia North America     Papunesia 
    ##           208            40           529           168           586 
    ## South America 
    ##           135

    # Any dupliccate segments? Nope
    segbo_enriched %>% select(InventoryID, Phoneme) %>% group_by(InventoryID, Phoneme) %>% filter(n()>1)

    ## # A tibble: 0 x 2
    ## # Groups:   InventoryID, Phoneme [0]
    ## # … with 2 variables: InventoryID <dbl>, Phoneme <chr>

    # Clean up
    rm(families.counts, families.glottocodes, isolates, languages.geo, macroareas, missing_codes_for_macroarea)

Let’s combine them. If all the columns are the same.

    # p.cut <- phoible_enriched %>% select(InventoryID, Glottocode, family_id, Phoneme, macroarea)
    # p.cut$Database <- "phoible"
    # p.cut$InventoryID <- paste0(p.cut$InventoryID, "_phoible")

    # b.cut <- bdproto %>% select(InventoryID, Glottocode, family_id, Phoneme, macroarea) %>% filter(!is.na(Glottocode))
    # b.cut$Database <- "bdproto"
    # b.cut$InventoryID <- paste0(b.cut$InventoryID, "_bdproto")

    # s.cut <- segbo_enriched %>% select(InventoryID, Glottocode, family_id, Phoneme, macroarea)
    # s.cut$Database <- "segbo"
    # s.cut$InventoryID <- paste0(s.cut$InventoryID, "_segbo")
    # 
    # If the columns are the same
    # all_dbs_all_segments_temp <- do.call("rbind", list(p.cut, b.cut, s.cut))

    # Any duplidate Phonemes per source?
    # all_dbs_all_segments_temp %>% select(InventoryID, Database, Phoneme) %>% group_by(InventoryID, Phoneme) %>% filter(n()>1) %>% arrange(InventoryID, Phoneme)

Let’s combine them. Here we add additional columns.

    p.cut <- phoible_enriched %>% select(InventoryID, Glottocode, family_id, Phoneme, macroarea, SegmentClass)
    p.cut$Database <- "phoible"
    p.cut$InventoryID <- paste0(p.cut$InventoryID, "_phoible")
    p.cut$InventoryType <- "all"

    b.cut <- bdproto %>% select(InventoryID, Glottocode, family_id, Phoneme, macroarea, InventoryType) %>% filter(!is.na(Glottocode))
    b.cut$Database <- "bdproto"
    b.cut$InventoryID <- paste0(b.cut$InventoryID, "_bdproto")

    s.cut <- segbo_enriched %>% select(InventoryID, Glottocode, family_id, Phoneme, macroarea)
    s.cut$Database <- "segbo"
    s.cut$InventoryID <- paste0(s.cut$InventoryID, "_segbo")
    s.cut$InventoryType <- NA

    # If the columns are different and extended with additional database-specific stuff, then use `bind_rows`
    all_dbs_all_segments <- bind_rows(p.cut, b.cut)
    all_dbs_all_segments <- bind_rows(all_dbs_all_segments, s.cut)

    # Add in the missing segment classes
    all_dbs_all_segments <- merge_dfs_overwrite_col(all_dbs_all_segments, segment_value, cols = "SegmentClass", bycol = "Phoneme")

    # Fix the missing ones by hand
    missing_segment_classes <- read_csv('missing_segment_classes2.csv')

    ## Parsed with column specification:
    ## cols(
    ##   Phoneme = col_character(),
    ##   SegmentClass = col_character()
    ## )

    all_dbs_all_segments <- merge_dfs_overwrite_col(all_dbs_all_segments, missing_segment_classes, cols = "SegmentClass", bycol = "Phoneme")

    # Any duplidate phonemes per source?
    all_dbs_all_segments %>% select(InventoryID, Database, Phoneme) %>% group_by(InventoryID, Phoneme) %>% filter(n()>1) %>% arrange(InventoryID, Phoneme)

    ## # A tibble: 0 x 3
    ## # Groups:   InventoryID, Phoneme [0]
    ## # … with 3 variables: InventoryID <chr>, Database <chr>, Phoneme <chr>

    table(all_dbs_all_segments$SegmentClass, exclude=FALSE)

    ## 
    ## consonant      tone     vowel 
    ##     73983      2123     31152

    # Check that nothing is weird
    all_dbs_all_segments %>% filter(is.na(Glottocode)) %>% select(Glottocode, Database) %>% distinct()

    ## [1] Glottocode Database  
    ## <0 rows> (or 0-length row.names)

    all_dbs_all_segments %>% filter(is.na(Phoneme))

    ## [1] InventoryID   Glottocode    family_id     Phoneme       macroarea    
    ## [6] SegmentClass  Database      InventoryType
    ## <0 rows> (or 0-length row.names)

    all_dbs_all_segments %>% filter(is.na(macroarea))

    ## [1] InventoryID   Glottocode    family_id     Phoneme       macroarea    
    ## [6] SegmentClass  Database      InventoryType
    ## <0 rows> (or 0-length row.names)

    all_dbs_all_segments %>% filter(is.na(SegmentClass))

    ## [1] InventoryID   Glottocode    family_id     Phoneme       macroarea    
    ## [6] SegmentClass  Database      InventoryType
    ## <0 rows> (or 0-length row.names)

    # Clean up
    rm(p.cut, b.cut, s.cut, phoible_enriched, segbo_enriched, bdproto, missing_segment_classes)

All segments
============

Let’s create a dataframe such that columns are segments, rows are
databases, and cells are counts (with zeros for segments that don’t
appear in one or other database) for *ALL* segments in each database.

    # load phoible
    # load("./phoible.RData")
    phoible <- read_csv(url('https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true'), col_types=c(InventoryID='i', Marginal='l', .default='c'))

    # load and manipulate bdproto
    load('bdproto.Rdata')
    bdproto <- inventories

    # Let's exlude the questionable language families, e.g. Nostratic
    exclude <- read_csv('what_to_exclude.csv')

    ## Parsed with column specification:
    ## cols(
    ##   BdprotoID = col_double(),
    ##   Exclude = col_logical()
    ## )

    bdproto <- left_join(bdproto, exclude)

    ## Joining, by = "BdprotoID"

    bdproto <- bdproto %>% filter(!Exclude)

    # Let's do some column renaming for convenience sake
    bdproto <- bdproto %>% rename(family_id = FamilyID)
    bdproto <- bdproto %>% rename(InventoryID = BdprotoID)

    # load and manipulate segbo
    segbo <- read_csv('segbo_with_glottolog.csv')

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   InventoryID = col_double(),
    ##   PhoibleID = col_double(),
    ##   ClosestNeighbor = col_logical(),
    ##   bookkeeping = col_logical(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   description = col_logical(),
    ##   markup_description = col_logical(),
    ##   child_family_count = col_double(),
    ##   child_language_count = col_double(),
    ##   child_dialect_count = col_double()
    ## )

    ## See spec(...) for full column specifications.

    segbo_inventories <- segbo %>% select(InventoryID) %>% distinct()
    segbo <- segbo %>% select(BorrowedSound)
    segbo <- segbo %>% rename(Phoneme = BorrowedSound)

First, let’s get the union of the sets of segments.

    all_segments_long <- as_tibble(union(bdproto$Phoneme, phoible$Phoneme))
    all_segments_long <- as_tibble(union(all_segments_long$value, segbo$Phoneme))
    all_segments_long <- all_segments_long %>% rename(Phoneme = value)

Next, let’s merge in the counts from the two databases.

    phoible_segments <- phoible %>% select(Phoneme) %>% group_by(Phoneme) %>% summarize(phoible=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    bdproto_segments <- bdproto %>% select(Phoneme) %>% group_by(Phoneme) %>% summarize(bdproto=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    segbo_segments <- segbo %>% select(Phoneme) %>% group_by(Phoneme) %>% summarize(segbo=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    all_segments_long <- left_join(all_segments_long, phoible_segments)

    ## Joining, by = "Phoneme"

    all_segments_long <- left_join(all_segments_long, bdproto_segments)

    ## Joining, by = "Phoneme"

    all_segments_long <- left_join(all_segments_long, segbo_segments)

    ## Joining, by = "Phoneme"

Let’s sort them by frequency in phoible for convenience.

    all_segments_long <- all_segments_long %>% arrange(desc(phoible))
    all_segments_long %>% head() %>% kable()

| Phoneme | phoible | bdproto | segbo |
|:--------|--------:|--------:|------:|
| m       |    2915 |     216 |     3 |
| i       |    2779 |     194 |    NA |
| k       |    2729 |     222 |    19 |
| j       |    2716 |     198 |    19 |
| u       |    2646 |     180 |     2 |
| a       |    2600 |     168 |     1 |

Are there any NAs in phoible and bdproto now that we’ve added segbo,
i.e. are there segments in segbo not in the other two data sets? Yes
there is. TODO: what to do about these?

    all_segments_long %>% filter(is.na(phoible) & is.na(bdproto))

    ## # A tibble: 5 x 4
    ##   Phoneme phoible bdproto segbo
    ##   <chr>     <int>   <int> <int>
    ## 1 ʕ̞            NA      NA     1
    ## 2 ɹ̤            NA      NA     1
    ## 3 ɨə̯           NA      NA     1
    ## 4 ʊai          NA      NA     1
    ## 5 n̺d̺z̺          NA      NA     1

Now we have a dataframe of phonemes and their counts in phoible and
bdproto and segbo. Some segments will be missing in phoible (they are
not in bdproto) and vice versa.

    all_segments_long %>% filter(is.na(phoible)) %>% head() %>% kable()

| Phoneme | phoible | bdproto | segbo |
|:--------|--------:|--------:|------:|
| xʷʲ     |      NA |       1 |    NA |
| i̝       |      NA |       3 |    NA |
| u̝       |      NA |       3 |    NA |
| ǁ       |      NA |       2 |    NA |
| ǀ       |      NA |       2 |    NA |
| ǂ       |      NA |       2 |    NA |

Let’s replace all NAs with zeros.

    all_segments_long[is.na(all_segments_long)] <- 0

Check it.

    all_segments_long %>% filter(is.na(phoible))

    ## # A tibble: 0 x 4
    ## # … with 4 variables: Phoneme <chr>, phoible <int>, bdproto <int>, segbo <int>

Now let’s add the probability of a segment’s occurrence.

    all_segments_long$phoible_prob <- all_segments_long$phoible / sum(all_segments_long$phoible)
    all_segments_long$bdproto_prob <- all_segments_long$bdproto / sum(all_segments_long$bdproto)
    all_segments_long$segbo_prob <- all_segments_long$segbo / sum(all_segments_long$segbo)

    # Now let's reshape the data into a wide format. We can simply transpose it.
    # n <- all_segments_long$Phoneme
    # all_segments_wide <- as.data.frame(t(all_segments_long[,-1]))
    # colnames(all_segments_wide) <- n

Let’s add the cross-linguistic frequencies of each segment within each
database.

    all_segments_long$phoible_freq <- all_segments_long$phoible / length(unique(phoible$InventoryID))
    all_segments_long$bdproto_freq <- all_segments_long$bdproto / length(unique(bdproto$InventoryID))
    all_segments_long$segbo_freq <- all_segments_long$segbo / nrow(segbo_inventories)

Have a look.

    head(all_segments_long) %>% kable()

| Phoneme | phoible | bdproto | segbo | phoible\_prob | bdproto\_prob | segbo\_prob | phoible\_freq | bdproto\_freq | segbo\_freq |
|:--------|--------:|--------:|------:|--------------:|--------------:|------------:|--------------:|--------------:|------------:|
| m       |    2915 |     216 |     3 |     0.0276408 |     0.0308131 |   0.0018007 |     0.9652318 |     0.9037657 |   0.0056497 |
| i       |    2779 |     194 |     0 |     0.0263512 |     0.0276748 |   0.0000000 |     0.9201987 |     0.8117155 |   0.0000000 |
| k       |    2729 |     222 |    19 |     0.0258771 |     0.0316690 |   0.0114046 |     0.9036424 |     0.9288703 |   0.0357815 |
| j       |    2716 |     198 |    19 |     0.0257538 |     0.0282454 |   0.0114046 |     0.8993377 |     0.8284519 |   0.0357815 |
| u       |    2646 |     180 |     2 |     0.0250901 |     0.0256776 |   0.0012005 |     0.8761589 |     0.7531381 |   0.0037665 |
| a       |    2600 |     168 |     1 |     0.0246539 |     0.0239658 |   0.0006002 |     0.8609272 |     0.7029289 |   0.0018832 |

Some clean up.

    rm(bdproto, bdproto_segments, exclude, inventories, phoible, phoible_segments, n, segbo, segbo_segments, segbo_inventories)

    ## Warning in rm(bdproto, bdproto_segments, exclude, inventories, phoible, : object
    ## 'n' not found

Get segments in the subset of matching language families for bdproto and phoible
================================================================================

Load phoible and add the glottolog family info.

    load("./phoible.RData")
    load("./glottolog-families-isolates.Rdata")

    # Add the ISO data
    phoible_enriched = merge(phoible, families.glottocodes[, c("iso639P3code", "family_id")], by.x="ISO6393", by.y="iso639P3code")
    rm(phoible, languages.geo, isolates, families.counts, families.glottocodes)

Load bdproto and exclude data points like Nostratic. Todo: consonants
versus vowels.

    load('bdproto.Rdata')
    bdproto <- inventories
    exclude <- read_csv('what_to_exclude.csv')

    ## Parsed with column specification:
    ## cols(
    ##   BdprotoID = col_double(),
    ##   Exclude = col_logical()
    ## )

    bdproto <- left_join(bdproto, exclude)

    ## Joining, by = "BdprotoID"

    bdproto <- bdproto %>% filter(!Exclude)

    # Let's do some column renaming for convenience sake
    bdproto <- bdproto %>% rename(family_id = FamilyID)
    bdproto <- bdproto %>% rename(InventoryID = BdprotoID)
    bdproto <- bdproto %>% filter(!is.na(family_id))
    rm(inventories, exclude)

Let’s check how many language families in each source.

    length(table(phoible_enriched$family_id))

    ## [1] 174

    length(table(bdproto$family_id))

    ## [1] 75

To make the samples comparable, let’s remove all families in phoible not
in bdproto. (This could simply be done with taking the intersection of
the family\_id variable…)

    phoible_enriched <- phoible_enriched %>% filter(family_id %in% bdproto$family_id)
    length(table(phoible_enriched$family_id))

    ## [1] 68

    phoible_family_ids <- phoible_enriched %>% select(family_id) %>% distinct()

There are also reconstructions in bdproto that have no daughter
languages represented in phoible, e.g. Lakes Plain, Yokutsan, and
Takelman. Other entries are ancient languages, known from corpora, which
are now extinct (Elamian, Hattian, Hurro-Urartian and Sumerian).

    bdproto_family_ids <- bdproto %>% select(LanguageFamilyRoot, family_id, Type) %>% distinct()
    not_in_phoible <- bdproto_family_ids[which(!(bdproto_family_ids$family_id %in% phoible_family_ids$family_id)),]
    not_in_phoible %>% kable()

| LanguageFamilyRoot | family\_id | Type          |
|:-------------------|:-----------|:--------------|
| Lakes Plain        | lake1255   | Reconstructed |
| Sumerian           | sume1241   | Attested      |
| Yokutsan           | yoku1255   | Reconstructed |
| Elamian            | elam1244   | Attested      |
| Hattian            | hatt1246   | Attested      |
| Hurro-Urartian     | hurr1239   | Attested      |
| Takelman           | take1257   | Reconstructed |

Let’s remove these from bdproto for the analysis.

    bdproto <- bdproto %>% filter(!(family_id %in% not_in_phoible$family_id))

Double check that both datasets have the same number of language
familes.

    length(table(phoible_enriched$family_id))

    ## [1] 68

    length(table(bdproto$family_id))

    ## [1] 68

    rm(not_in_phoible, phoible_family_ids, bdproto_family_ids)

Now, let’s get the union of the two sets of segments.

    families_segments_long <- as_tibble(union(bdproto$Phoneme, phoible_enriched$Phoneme))
    families_segments_long <- families_segments_long %>% rename(Phoneme = value)

Next, let’s merge in the counts from the two databases.

    phoible_segments <- phoible_enriched %>% select(Phoneme) %>% group_by(Phoneme) %>% summarize(phoible=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    bdproto_segments <- bdproto %>% select(Phoneme) %>% group_by(Phoneme) %>% summarize(bdproto=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    families_segments_long <- left_join(families_segments_long, phoible_segments)

    ## Joining, by = "Phoneme"

    families_segments_long <- left_join(families_segments_long, bdproto_segments)

    ## Joining, by = "Phoneme"

Let’s sort them by frequency in phoible for convenience.

    families_segments_long <- families_segments_long %>% arrange(desc(phoible))
    families_segments_long %>% head() %>% kable()

| Phoneme | phoible | bdproto |
|:--------|--------:|--------:|
| m       |    2350 |     204 |
| i       |    2236 |     179 |
| k       |    2218 |     207 |
| j       |    2211 |     188 |
| u       |    2137 |     165 |
| p       |    2111 |     186 |

Now we have a dataframe of phonemes and their counts in phoible and
bdproto. Some segments will be missing in phoible (they are not in
bdproto) and vice versa.

    families_segments_long %>% filter(is.na(phoible)) %>% head() %>% kable()

| Phoneme | phoible | bdproto |
|:--------|--------:|--------:|
| χʷʼ     |      NA |       3 |
| dɮ      |      NA |       2 |
| kʷʲ     |      NA |       1 |
| xʷʲ     |      NA |       1 |
| i̝       |      NA |       2 |
| u̝       |      NA |       2 |

Let’s replace all NAs with zeros.

    families_segments_long[is.na(families_segments_long)] <- 0

Check it.

    families_segments_long %>% filter(is.na(phoible))

    ## # A tibble: 0 x 3
    ## # … with 3 variables: Phoneme <chr>, phoible <int>, bdproto <int>

Now let’s add the probability of a segment’s occurrence.

    families_segments_long$phoible_prob <- families_segments_long$phoible / sum(families_segments_long$phoible)
    families_segments_long$bdproto_prob <- families_segments_long$bdproto / sum(families_segments_long$bdproto)
    families_segments_long %>% head() %>% kable()

| Phoneme | phoible | bdproto | phoible\_prob | bdproto\_prob |
|:--------|--------:|--------:|--------------:|--------------:|
| m       |    2350 |     204 |     0.0273721 |     0.0309466 |
| i       |    2236 |     179 |     0.0260442 |     0.0271541 |
| k       |    2218 |     207 |     0.0258346 |     0.0314017 |
| j       |    2211 |     188 |     0.0257530 |     0.0285194 |
| u       |    2137 |     165 |     0.0248911 |     0.0250303 |
| p       |    2111 |     186 |     0.0245883 |     0.0282160 |

    # Now let's reshape the data into a wide format. We can simply transpose it.
    # n <- families_segments_long$Phoneme
    # families_segments_wide <- as.data.frame(t(families_segments_long[,-1]))
    # colnames(all_segments_wide) <- n

Some clean up.

    rm(bdproto, bdproto_segments, exclude, inventories, phoible_enriched, phoible_segments, n)

    ## Warning in rm(bdproto, bdproto_segments, exclude, inventories,
    ## phoible_enriched, : object 'exclude' not found

    ## Warning in rm(bdproto, bdproto_segments, exclude, inventories,
    ## phoible_enriched, : object 'inventories' not found

    ## Warning in rm(bdproto, bdproto_segments, exclude, inventories,
    ## phoible_enriched, : object 'n' not found

Get segments in the intersection of all three databases
=======================================================

Load phoible and add the glottolog family info.

    load("./phoible.RData")
    load("./glottolog-families-isolates.Rdata")

    # Add the ISO data
    phoible_enriched = merge(phoible, families.glottocodes[, c("iso639P3code", "family_id")], by.x="ISO6393", by.y="iso639P3code")
    rm(phoible, languages.geo, isolates, families.counts, families.glottocodes)

Load segbo and the glottolog family info.

    # load and extend segbo
    segbo <- read_csv('segbo_with_glottolog.csv')

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   InventoryID = col_double(),
    ##   PhoibleID = col_double(),
    ##   ClosestNeighbor = col_logical(),
    ##   bookkeeping = col_logical(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   description = col_logical(),
    ##   markup_description = col_logical(),
    ##   child_family_count = col_double(),
    ##   child_language_count = col_double(),
    ##   child_dialect_count = col_double()
    ## )

    ## See spec(...) for full column specifications.

    segbo <- segbo %>% rename(Phoneme = BorrowedSound)
    extra <- read_csv('segbo_missing_codes.csv')

    ## Parsed with column specification:
    ## cols(
    ##   LanguageName = col_character(),
    ##   family_id = col_character()
    ## )

    # Quick hack to get in the hand curated codes
    library(ddpcr)
    segbo_enriched <- merge_dfs_overwrite_col(segbo, extra, cols = "family_id", bycol = "LanguageName")
    rm(segbo, extra)

    segbo_enriched %>% filter(is.na(Phoneme))

    ## # A tibble: 0 x 35
    ## # … with 35 variables: InventoryID <dbl>, BorrowingLanguageGlottocode <chr>,
    ## #   Phoneme <chr>, SourceLanguageGlottocode <chr>, OnlyInLoanwords <chr>,
    ## #   Result <chr>, NewDistinction <chr>, PhonemeComments <chr>, Verified <chr>,
    ## #   Glottocode <chr>, LanguageName <chr>, BibTexKey <chr>, Filename <chr>,
    ## #   Contributor <chr>, MetadataComments <chr>, PhoibleID <dbl>,
    ## #   ClosestNeighbor <lgl>, family_id <chr>, parent_id <chr>, name <chr>,
    ## #   bookkeeping <lgl>, level <chr>, status <chr>, latitude <dbl>,
    ## #   longitude <dbl>, iso639P3code <chr>, description <lgl>,
    ## #   markup_description <lgl>, child_family_count <dbl>,
    ## #   child_language_count <dbl>, child_dialect_count <dbl>, country_ids <chr>,
    ## #   glottocode <chr>, isocodes <chr>, macroarea <chr>

Load bdproto and exclude data points like Nostratic. Todo: consonants
versus vowels.

    load('bdproto.Rdata')
    bdproto <- inventories
    exclude <- read_csv('what_to_exclude.csv')

    ## Parsed with column specification:
    ## cols(
    ##   BdprotoID = col_double(),
    ##   Exclude = col_logical()
    ## )

    bdproto <- left_join(bdproto, exclude)

    ## Joining, by = "BdprotoID"

    bdproto <- bdproto %>% filter(!Exclude)

    # Let's do some column renaming for convenience sake
    bdproto <- bdproto %>% rename(family_id = FamilyID)
    bdproto <- bdproto %>% rename(InventoryID = BdprotoID)
    bdproto <- bdproto %>% filter(!is.na(family_id))
    rm(inventories, exclude)

Let’s check how many language families in each source.

    length(table(phoible_enriched$family_id))

    ## [1] 174

    length(table(bdproto$family_id))

    ## [1] 75

    length(table(segbo_enriched$family_id))

    ## [1] 113

Let’s see what’s shared across all three.

    families_shared <- as_tibble(union(phoible_enriched$family_id, bdproto$family_id))
    families_shared <- as_tibble(union(families_shared$value, segbo_enriched$family_id))
    families_shared

    ## # A tibble: 208 x 1
    ##    value   
    ##    <chr>   
    ##  1 indo1319
    ##  2 afro1255
    ##  3 cari1283
    ##  4 sepi1257
    ##  5 atla1278
    ##  6 abkh1242
    ##  7 aust1307
    ##  8 nduu1242
    ##  9 araw1281
    ## 10 nilo1247
    ## # … with 198 more rows

Which families are in the intersection of all three? Much less.

    intersected_families <- Reduce(intersect, list(phoible_enriched$family_id, bdproto$family_id, segbo_enriched$family_id))
    intersected_families

    ##  [1] "indo1319" "afro1255" "cari1283" "sepi1257" "atla1278" "aust1307"
    ##  [7] "araw1281" "nilo1247" "sino1245" "pama1250" "timo1261" "tupi1275"
    ## [13] "nucl1709" "eski1264" "pano1259" "iwai1246" "otom1299" "gunw1250"
    ## [19] "nucl1710" "chib1249" "turk1311" "cent2225" "guai1249" "ayma1253"
    ## [25] "mand1469" "tuca1253" "drav1251" "aust1305" "algi1248" "sali1255"
    ## [31] "siou1252" "mong1329" "barb1265" "maya1287" "iroq1247" "utoa1244"
    ## [37] "taik1256" "ural1272" "khoe1240" "quec1387" "japo1237" "kart1248"
    ## [43] "kiow1265" "skoo1245" "maba1274" "nort2923" "mixe1284" "pomo1273"
    ## [49] "chim1311" "toto1251"

To make the samples comparable, let’s remove all families not in the
intersection of each database.

    phoible_enriched <- phoible_enriched %>% filter(family_id %in% intersected_families)
    length(table(phoible_enriched$family_id))

    ## [1] 50

    bdproto <- bdproto %>% filter(family_id %in% intersected_families)

    segbo_enriched <- segbo_enriched %>% filter(family_id %in% intersected_families)

Double check that both datasets have the same number of language
familes.

    length(table(phoible_enriched$family_id))

    ## [1] 50

    length(table(bdproto$family_id))

    ## [1] 50

    length(table(segbo_enriched$family_id))

    ## [1] 50

Now, let’s get the union of all the segments.

    intersect_families_segments_long <- as.tibble(Reduce(union, list(bdproto$Phoneme, phoible_enriched$Phoneme, segbo_enriched$Phoneme)))

    ## Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    intersect_families_segments_long <- intersect_families_segments_long %>% rename(Phoneme = value)
    intersect_families_segments_long %>% filter(is.na(Phoneme))

    ## # A tibble: 0 x 1
    ## # … with 1 variable: Phoneme <chr>

Next, let’s merge in the counts from the databases.

    phoible_segments <- phoible_enriched %>% select(Phoneme) %>% group_by(Phoneme) %>% summarize(phoible=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    bdproto_segments <- bdproto %>% select(Phoneme) %>% group_by(Phoneme) %>% summarize(bdproto=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    segbo_segments <- segbo_enriched %>% select(Phoneme) %>% group_by(Phoneme) %>% summarize(segbo=n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    intersect_families_segments_long <- left_join(intersect_families_segments_long, phoible_segments)

    ## Joining, by = "Phoneme"

    intersect_families_segments_long <- left_join(intersect_families_segments_long, bdproto_segments)

    ## Joining, by = "Phoneme"

    intersect_families_segments_long <- left_join(intersect_families_segments_long, segbo_segments)

    ## Joining, by = "Phoneme"

Let’s sort them by frequency in phoible for convenience.

    intersect_families_segments_long <- intersect_families_segments_long %>% arrange(desc(phoible))
    intersect_families_segments_long %>% head() %>% kable()

| Phoneme | phoible | bdproto | segbo |
|:--------|--------:|--------:|------:|
| m       |    2290 |     183 |     2 |
| i       |    2180 |     158 |    NA |
| k       |    2163 |     185 |    16 |
| j       |    2155 |     168 |    19 |
| u       |    2084 |     146 |     2 |
| p       |    2059 |     165 |    37 |

Let’s replace all NAs with zeros.

    intersect_families_segments_long %>% filter(is.na(Phoneme))

    ## # A tibble: 0 x 4
    ## # … with 4 variables: Phoneme <chr>, phoible <int>, bdproto <int>, segbo <int>

    intersect_families_segments_long[is.na(intersect_families_segments_long)] <- 0

Check it.

    intersect_families_segments_long %>% filter(is.na(phoible))

    ## # A tibble: 0 x 4
    ## # … with 4 variables: Phoneme <chr>, phoible <int>, bdproto <int>, segbo <int>

Now let’s add the probability of a segment’s occurrence.

    intersect_families_segments_long$phoible_prob <- intersect_families_segments_long$phoible / sum(intersect_families_segments_long$phoible)
    intersect_families_segments_long$bdproto_prob <- intersect_families_segments_long$bdproto / sum(intersect_families_segments_long$bdproto)
    intersect_families_segments_long$segbo_prob <- intersect_families_segments_long$segbo / sum(intersect_families_segments_long$segbo)
    intersect_families_segments_long %>% head() %>% kable()

| Phoneme | phoible | bdproto | segbo | phoible\_prob | bdproto\_prob | segbo\_prob |
|:--------|--------:|--------:|------:|--------------:|--------------:|------------:|
| m       |    2290 |     183 |     2 |     0.0272233 |     0.0307408 |   0.0013450 |
| i       |    2180 |     158 |     0 |     0.0259157 |     0.0265412 |   0.0000000 |
| k       |    2163 |     185 |    16 |     0.0257136 |     0.0310768 |   0.0107599 |
| j       |    2155 |     168 |    19 |     0.0256185 |     0.0282211 |   0.0127774 |
| u       |    2084 |     146 |     2 |     0.0247744 |     0.0245254 |   0.0013450 |
| p       |    2059 |     165 |    37 |     0.0244772 |     0.0277171 |   0.0248823 |

Some clean up.

    rm(bdproto, bdproto_segments, phoible_enriched, phoible_segments, segbo_enriched, segbo_segments, families_shared)

Write the dataframes to disk
============================

Let’s save the datarames in an RData file for the analyses.

    save(all_dbs_all_segments, all_dbs_all_segments, all_segments_long, families_segments_long, intersect_families_segments_long, file="dfs_for_analysis_csv_vs.RData")
