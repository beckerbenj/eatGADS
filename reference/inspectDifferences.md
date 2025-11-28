# Inspect differences in a variable.

Inspect differences within a single `GADSdat` or between two `GADSdat`
objects for a specific variable.

## Usage

``` r
inspectDifferences(
  GADSdat,
  varName,
  other_GADSdat = GADSdat,
  other_varName = varName,
  id
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- varName:

  A character vector of length 1 containing the variable name.

- other_GADSdat:

  A second `GADSdat` object. If omitted, it is assumed that both
  variables are part of the first `GADSdat`.

- other_varName:

  A character vector of length 1 containing the other variable name. If
  omitted, it is assumed that both variables have identical names (as
  supplied in `varName`).

- id:

  A character vector of length 1 containing the unique identifier column
  of both `GADSdat`.

## Value

A list.

## Details

Two `GADSdat` objects can be compared using
[`equalGADS`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md).
If differences in the data for specific variables in the two objects
occur, these variables can be further inspected using
`inspectDifferences`. Differences on meta data-level can be inspected
via
[`inspectMetaDifferences`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md).

## Examples

``` r
# create a second GADS with different data
pisa2 <- pisa
pisa2$dat$age[400:nrow(pisa$dat)] <- sample(pisa2$dat$age[400:nrow(pisa$dat)])

# inspect via equalGADS()
equalGADS(pisa, pisa2)
#> $names_not_in_1
#> character(0)
#> 
#> $names_not_in_2
#> character(0)
#> 
#> $data_differences
#> [1] "age"
#> 
#> $data_nrow
#> [1] "all.equal"
#> 
#> $meta_data_differences
#> character(0)
#> 

# inspect via inspectDifferences()
inspectDifferences(GADSdat = pisa, varName = "age", other_GADSdat = pisa2, id = "idstud")
#> $cross_table
#>        other_GADSdat
#> GADSdat 13.92 14 14.42 14.5 14.58 14.67 14.75 14.83 14.92 15 15.08 15.17 15.25
#>   13.92     1  0     0    0     0     0     0     0     0  0     0     0     0
#>   14        0  1     0    0     0     0     0     0     0  0     0     0     0
#>   14.42     0  0     1    0     0     0     0     0     0  0     0     0     0
#>   14.5      0  0     0    2     0     0     0     0     0  0     0     0     0
#>   14.58     0  0     0    0     8     0     0     1     1  0     0     0     0
#>   14.67     0  0     0    0     0     7     0     0     0  0     0     0     0
#>   14.75     0  0     0    0     0     0    10     0     0  1     0     0     0
#>   14.83     0  0     0    0     0     0     0    12     0  0     1     0     1
#>   14.92     0  0     0    0     0     0     0     0    27  0     0     1     0
#>   15        0  0     0    0     0     0     0     0     0 31     0     0     0
#>   15.08     0  0     0    0     0     0     0     0     0  0    21     1     0
#>   15.17     0  0     0    0     0     0     0     0     0  0     0    31     0
#>   15.25     0  0     0    0     1     0     0     0     0  1     1     1    17
#>   15.33     0  0     0    0     0     0     0     1     1  0     1     1     1
#>   15.42     0  0     0    0     0     0     0     0     0  1     0     0     0
#>   15.5      0  0     0    0     0     0     0     1     0  1     0     0     1
#>   15.58     0  0     0    0     0     0     1     0     0  0     0     1     0
#>   15.67     0  0     0    0     0     0     0     0     1  0     0     1     0
#>   15.75     0  0     0    0     0     0     1     1     1  0     0     0     0
#>   15.83     0  0     0    0     1     0     1     0     0  0     0     0     0
#>   15.92     0  0     0    0     0     0     0     0     0  0     0     0     0
#>   16        0  0     0    0     0     0     0     0     0  0     0     0     0
#>   16.08     0  0     0    0     0     0     0     0     0  0     0     0     0
#>   16.17     0  0     0    0     0     0     0     0     0  0     0     0     0
#>   16.25     0  0     0    0     0     0     0     0     0  1     0     0     1
#>   16.33     0  0     0    0     0     0     0     0     0  0     0     1     1
#>        other_GADSdat
#> GADSdat 15.33 15.42 15.5 15.58 15.67 15.75 15.83 15.92 16 16.08 16.17 16.25
#>   13.92     0     0    0     0     0     0     0     0  0     0     0     0
#>   14        0     0    0     0     0     0     0     0  0     0     0     0
#>   14.42     0     0    0     0     0     0     0     0  0     0     0     0
#>   14.5      0     0    0     0     0     0     0     0  0     0     0     0
#>   14.58     0     0    0     0     0     0     0     0  0     0     0     0
#>   14.67     0     0    0     0     0     0     0     0  0     0     0     0
#>   14.75     0     1    0     1     0     0     0     0  0     0     0     0
#>   14.83     0     0    1     0     0     0     0     0  1     0     0     0
#>   14.92     0     2    0     0     0     0     1     0  0     0     0     0
#>   15        0     0    1     2     0     1     0     0  0     1     0     0
#>   15.08     0     0    0     0     0     2     0     0  0     0     0     0
#>   15.17     0     1    1     1     1     0     2     1  0     0     0     0
#>   15.25     1     0    0     0     0     0     0     0  0     0     0     0
#>   15.33    20     0    1     0     0     0     0     1  0     0     0     1
#>   15.42     1    28    1     0     0     1     0     0  0     0     1     0
#>   15.5      1     1   35     1     1     0     1     0  0     0     0     0
#>   15.58     1     0    1    24     2     0     0     0  0     0     0     0
#>   15.67     0     0    1     1    24     2     0     0  1     0     0     0
#>   15.75     2     0    0     0     1    20     0     0  0     1     0     0
#>   15.83     0     0    0     0     0     1    23     0  1     0     0     1
#>   15.92     0     0    1     0     0     0     0    10  0     0     0     1
#>   16        1     0    0     0     1     0     1     0  7     0     0     0
#>   16.08     0     0    0     1     0     0     0     0  0     3     0     0
#>   16.17     1     0    0     0     0     0     0     0  0     0     7     0
#>   16.25     1     0    0     0     0     0     0     0  0     0     0     0
#>   16.33     0     0    0     0     1     1     0     0  0     0     0     0
#>        other_GADSdat
#> GADSdat 16.33
#>   13.92     0
#>   14        0
#>   14.42     0
#>   14.5      0
#>   14.58     0
#>   14.67     0
#>   14.75     0
#>   14.83     0
#>   14.92     0
#>   15        0
#>   15.08     0
#>   15.17     0
#>   15.25     0
#>   15.33     1
#>   15.42     0
#>   15.5      0
#>   15.58     1
#>   15.67     0
#>   15.75     1
#>   15.83     0
#>   15.92     0
#>   16        0
#>   16.08     1
#>   16.17     0
#>   16.25     0
#>   16.33    33
#> 
#> $unequal_IDs
#>  [1] 402 403 404 405 406 407 408 409 410 411 412 413 414 416 417 418 419 420 421
#> [20] 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440
#> [39] 441 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456 457 458 459
#> [58] 460 461 462 463 464 466 467 468 469 470 471 472 473 474 475 476 477 478 479
#> [77] 480 481 482 483 485 486 487 488 489 490 492 493 494 495 496 497 498 499 500
#> [96] 501 502
#> 
```
