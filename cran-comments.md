## Resubmission

This is a resubmission. The package was previously archived on 2026-02-19 because it required the archived package 'leaflet.extras'. This dependency issue has been completely resolved. 

In this version, we have also addressed all previous comments from the CRAN reviewers:

* **Execution time > 10s:** Solved. The examples for `Plot.Mundo` and `Plot.Mapa` have been optimized and now execute well under the 5-second limit.
* **Modifying user's `par` options:** Solved. We now use an immediate call to `on.exit(par(oldpar))` in functions like `Plot_Radar()` to ensure settings are properly reset.
* **English translation in DESCRIPTION:** Solved. An English translation of the description has been added after the Spanish version.
* **Omit "+ file LICENSE":** Solved. We have removed it from the DESCRIPTION file as requested.

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