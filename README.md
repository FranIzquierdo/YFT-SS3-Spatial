# Yellowfin Tuna spatial stock assessment (SS3) - article R code

This repository contains the R scripts and SS3 model configurations associated with the manuscript:

> Izquierdo, F., Cousido-Rocha, M., Correa, G. M., Pennino, M.G., Berger, A.M., Goethel, D.R., Hoyle, S. D., Lynch, P. D. & Cerviño, S. (accepted). Canadian Journal of Fisheries and Aquatic Sciences. *Tuning into Tuna: Investigating Spatial Complexity Tradeoffs in Stock Assessment Models Based on a High-Resolution Simulation Experiment of Yellowfin Tuna in the Indian Ocean*.

This study was conducted as part of the **Spatial Stock Assessment Simulation Experiment Workshop**, jointly organized by NOAA and NIWA. For more details and access to the full set of simulation materials and documentation, visit:

🔗 <https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop>

------------------------------------------------------------------------

## Code Structure of the Article

This repository is organized in two main parts, following the structure of the manuscript:

### 1. Model Development – Reference Dataset

This section includes: - All R scripts used to run the sequential SS3 configurations, from the initial setup (S0) to the final selected model (S6.3) - Associated SS3 input files (`.ctl`, `.dat`, ss.exe, etc.) for each configuration step

These model setups were applied to the reference (blinded) dataset provided by the simulation experiment.

### 2. Model Testing – 100 Simulated Datasets

This section includes: - R scripts used to apply selected SS3 configurations from the previous section to the 100 simulated operating model datasets

Note that the files for these runs are not included in this repository due to their large size.\

------------------------------------------------------------------------

## Spatio-temporal CPUE Standardization

The spatiotemporal CPUE standardized indices by region used as input in the SS3 spatial stock assessment model were derived using three candidate modelling options, available at:

🔗 <https://github.com/FranIzquierdo/YFT-lattice-st-CPUE-models>

Among these options, the **Besag spatio-temporal interaction model** was selected as the final input CPUE ST index for the SS3 spatial stock assessment configurations.

------------------------------------------------------------------------

## Related Workshop Articles

This study is part of a broader multi-team international initiative. Additional articles and code repositories include:

-   **Goethel et al. (2024)**

    Goethel, D. R., Berger, A. M., Hoyle, S. D., Lynch, P. D., Barceló, C., Deroba, J., ... & Urtizberea, A. (2024). ‘Drivin'with your eyes closed’: Results from an international, blinded simulation experiment to evaluate spatial stock assessments. *Fish and Fisheries*, *25*(3), 471-490. [https://doi.org/10.1111/faf.12819](#0)\
    → SS3_A (ICES Team) code presented in this paper is available at:\
    🔗 [https://github.com/GiancarloMCorrea/SpatialStockAssessment_SpanishGroup](#0)

-   **Berger et al. (2026)**

    Berger, A. M., Goethel, D. R., Hoyle, S. D., Lynch, P., Barceló, C., Dunn, A., ... & Urtizberea, A. (2026). ‘Building the (Im) perfect Beast’: Strategies for Identifying Appropriate Spatial Stock Assessment Model Complexity From an International, Blinded High‐Resolution Simulation Experiment. Fish and Fisheries, 27(2), 196-212.

------------------------------------------------------------------------

## Contact

**Francisco Izquierdo \|** [francisco.izqtar\@gmail.com](mailto:francisco.izqtar@gmail.com)\

------------------------------------------------------------------------

## License and data

Code and model configurations in this repository are released under the MIT License.

Note that this repository does not contain the full simulation experiment project, which is described in Goethel et al. (2024). It only includes the Stock Synthesis input files of the model configurations presented in this specific manuscript, based on a single reference simulated dataset. For workshop documentation and related materials, see the repository linked above.
