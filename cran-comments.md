## Test environments

-   local Windows 10 install, R 4.3.0
-   local Windows 11 install, R 4.3.0
-   winbuilder
-   rhub check_for_cran

## R CMD check results

> checking package dependencies ... NOTE Imports includes 38 non-default packages.

> checking installed package size ... NOTE installed size is 5.4Mb

> > 0 errors | 0 warnings | 2 note

Despite our efforts, it was not possible to address or bypass the following issues:

While we are aware of the recommendation not to depend on more than 20 packages, our principle of innovation (*our package consolidates several libraries into a single function and allows for a very quick, easy, and friendly way to navigate the world of interactive and static graphics*) does not permit us to comply. It should be noted that we did attempt to minimize dependencies.

Although we know the recommendation not to exceed `5MB` (*we only exceeded by* `400kB`), it was not possible to reduce this because one of our sources (the one responsible for creating the maps) uses the official and updated polygons of Colombia, sourced directly from the National Administrative Department of Statistics - **DANE**, which is the entity responsible for the planning, collection, processing, analysis, and dissemination of official statistics in Colombia. And this is how it needs to be.

We hope for your understanding and support in publishing this package, which will have a significant impact on the management and publication of official statistics by the country’s leading university.
