# Avionics24

## Introduction

Avionic24 manages MRIO databases for GVC and Ecological Footprint analysis

Avionic24 is a package designed to reproduce the results of the INSEE working paper "Made in France and reindustrialization: an approach using input-output tables" (Alexandre BOURGEOIS and Jeremi MONTORNES 2024).

More generally, this package can also be used to manipulate multi-regional databases, to calculate carbon footprints using MRIO, to calculate structural path analysis (SPA), and to use the functions created as part of the working paper to apply the results to other countries, for example, or with additional modeling.

To install the package:

``` r
devtools::install_github("InseeFrLab/avionic24")
```

## Structure of the Code

**An ecosystem architecture

The principle is as follows: using the original MRIO databases, which can be downloaded from the Internet, we create a standardized version of these databases so that they fit into the same framework (same country names, same column headings, etc.). For this standardization, we use an Excel file called 'StructDocs...', which contains all the necessary transition tables (tables that can also be used for aggregations). 

MRIO tables are represented in database form (flat files), without tabulation, to limit adherence to data formats. MRIOs can be easily stacked to reconstruct series.

Finally, functions can be used to reconstruct MRIO objects from these standardized files (in particular the CompoMRIO function) with its various components, the calculation of technical coefficients, the Leontief inverse, etc. Extension functions can also be used to add carbon emission vectors to this object to calculate carbon footprints.

Worth noting: since MRIOs are often large files, handling them can lead to memory saturation. Therefore, we have proposed in the codes the possibility to work on temporal MRIO databases (which are heavy and require memory), or to work on annual MRIO databases to lighten the load.

**Using this architecture to replicate the results of the working document**.

The code consists of a large number of functions called within 4 R notebooks to reproduce the various computational steps required to reproduce the results.

The code reproduction files are as follows:
1. BuildDatabase_CalcMadeInAndContentVAExports.Rmd: used to build standardized databases for each MRIO from raw MRIO data, and then to build intermediate databases of indicator results. In some cases, it can also be used to backcast series to create long series (e.g. for MadeIn and Value Added content of exports). Finally, it can be used to produce results on the made-in and value-added content of exports.
2. Vulnerability and concentration indicators: Used to calculate the vulnerability indicators presented in the working paper.
3. HRM simulations.rmd: allows you to carry out simulations using the HRM (Hypothetical Repatriation Method) presented in the working document. This method provides results in terms of value added, production, employment, carbon emissions from production and carbon footprint.
4. SPA - Path Analysis: Structural Path Analysis (SPA) method applied to HRM variants in value added and carbon emissions to obtain the detailed paths that are mainly affected by the variants.




