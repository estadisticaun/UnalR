## UnalR 1.0.1 (2025-09-15)

Updated tests/testthat/test-Plot_Series.R to ensure compatibility with ggplot2 v4.0.0, addressing the object class and type expectations. This change ensures the package passes validation checks with the upcoming ggplot2 major release.

## Test environments

-   local Windows 10 install, R 4.5.1
-   local Windows 11 install, R 4.5.1
-   winbuilder (devel & release)
-   rhub check_for_cran

## R CMD check results

> checking package dependencies ... NOTE Imports includes 38 non-default packages.

> checking installed package size ... NOTE installed size is 5.4Mb

```
0 errors | 0 warnings | 2 note
```

Despite our efforts, it was not possible to address or bypass the following issues:

While we are aware of the recommendation not to depend on more than 20 packages, our principle of innovation (*our package consolidates several libraries into a single function and allows for a very quick, easy, and friendly way to navigate the world of interactive and static graphics*) does not permit us to comply. It should be noted that we did attempt to minimize dependencies.

Although we know the recommendation not to exceed `5MB` (*we only exceeded by* `400kB`), it was not possible to reduce this because one of our sources (the one responsible for creating the maps) uses the official and updated polygons of Colombia, sourced directly from the National Administrative Department of Statistics - **DANE**, which is the entity responsible for the planning, collection, processing, analysis, and dissemination of official statistics in Colombia. And this is how it needs to be.

We hope for your understanding and support in publishing this package, which will have a significant impact on the management and publication of official statistics by the country’s leading university.

## CRAN package review response

Additional comments by reviewer:

+ **Solved**

Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.: -> R/Plot_Radar.R

```
oldpar <- par(no.readonly = TRUE) # code line i
on.exit(par(oldpar))              # code line i + 1
```
```
par(mfrow=c(2,2))                 # somewhere after
```

+ **Solved** Please also add an english translation of your description in the DESCRIPTION file after the spanish version.
+ **Solved** Please omit "+ file LICENSE" and the file itself which is part of R anyway. It is only used to specify additional restrictions to the GPL such as attribution requirements.

> Thank you for your time and review :)
