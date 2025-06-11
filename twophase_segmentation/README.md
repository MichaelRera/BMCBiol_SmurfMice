## Content of this repository

1. Main functions:
  - `pipeline_akrj_f.R`, `pipeline_c57_m.R`, and `pipeline_c57_f.R`: run the whole method on the three datasets
Figures are automatically saved to `figure/`.
  - `pipeline_akrj_f_wrt_age.R`, `pipeline_c57_m_wrt_age.R`, and `pipeline_c57_f_wrt_age.R`: does the same but with age as the time variable (instead of the time before death).
Figures are automatically saved to `figure/`.

2. Pre-processing:
  - `clean_data.R`: clean the three datasets, removing outlying values. All removed datapoints are explicited in the commented code of this script.

3. Post-processing:
  - `save_nb_of_bp.R` : save the number of breakpoints detected to `results/nb_of_bp_*.rds`
  - `save_me_used.R`: save whether the correction procedure for mixed effects (`me`) are used to `results\me_used_*.rds`.

4. Data:
  - `data/`: raw and pre-processed datasets

5. Utility functions in `utils/`:
  - `utils/utils_pipeline_function.R`: defines the functions for:
    - the whole estimation pipeline (`run_pipeline`) 
    - estimating whether to compensate for mixed effects (`is_me_necessary`).
  - `utils/zeroing_out_functions.R`: defines the function that zeroes out the effect of mixed effects.

6. Figures in figure/

7. 

