## Resubmission

This is a resubmission. The package was previously archived on 2026-02-19 because it required the archived package 'leaflet.extras'. This dependency issue has been completely resolved.

In this version, we have addressed the latest feedback from Uwe Ligges regarding the package's scope:

* **Scope and Applicability:** Solved. As discussed via email, the package is a general-purpose wrapper with broad applicability (having accrued over 3,000 downloads previously). We have updated the `Title` and `Description` to explicitly reflect its universal utility for data visualization, clarifying that it was only *originally* developed at the National University of Colombia, but is not limited to it.

All previous reviewer comments have also been successfully addressed in this version:
* **Execution time > 10s:** Solved. Examples optimized under 5 seconds.
* **Modifying user's `par` options:** Solved with immediate `on.exit(par(oldpar))`.
* **Title in English and Title Case:** Solved.
* **English translation in DESCRIPTION:** Solved.
* **Omit "+ file LICENSE":** Solved.

## Test environments

* Local Windows 11 install, R 4.6.1
* win-builder (devel & release)
* Ubuntu (via GitHub Actions)
* macOS (via GitHub Actions)

## R CMD check results

0 errors | 0 warnings | 3 notes

**1. Note: "New submission" / "Package was archived on CRAN"**
This is expected as we are resubmitting a previously archived package.

**2. Note: "Imports includes 38 non-default packages"**
While we acknowledge the recommendation to minimize dependencies, this package serves as a consolidated tool that wraps multiple interactive and static graphing libraries into unified functions. We have moved as many dependencies to 'Suggests' as possible without breaking the core functionality.

**3. Note: "installed size is 5.4Mb" (Exceeds 5MB limit)**
The package size slightly exceeds the 5MB limit by ~400kB. This is strictly unavoidable because the package includes official geographic polygons of Colombia sourced directly from DANE (National Administrative Department of Statistics), which are required to render the map visualizations offline and accurately.

**Additional Comment regarding URL check:**
The URL `https://unal.edu.co` occasionally triggers a timeout during automated checks. This is a valid, active institutional URL, but its firewall sometimes blocks automated server pings.

Thank you for your time and review.