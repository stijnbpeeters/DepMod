#' Intervention: prevention of sub-clinical depression (base)
#'
#' This dataset contains baseline intervention parameters for the prevention of
#' sub-clinical depression in the DepMod model. It includes coverage, adherence,
#' effectiveness, sample size, and healthcare costs.
#'
#' @format A data frame with one row per intervention strategy and columns:
#' \describe{
#'   \item{cov}{Coverage of the intervention (proportion of target population).}
#'   \item{adh}{Adherence to the intervention (proportion).}
#'   \item{1-RR}{Effect size or relative risk reduction (numeric).}
#'   \item{n}{Sample size or study population used for the parameter estimate.}
#'   \item{healthcare costs}{Estimated healthcare costs per person.}
#' }
#'
#' @usage data(data_prev_sub_base)
#'
#' @details
#' Used to compute the overall preventive effect for sub-clinical depression in
#' the simulation model.
"data_prev_sub_base"


#' Intervention: prevention of sub-clinical depression (alternative)
#'
#' This dataset contains alternative scenario intervention parameters for the
#' prevention of sub-clinical depression in the DepMod model. The structure is
#' identical to the base dataset but can represent alternative modelling
#' assumptions. In this dataset, the same numbers are provided as in the base case.
#'
#' @format A data frame with the same columns as
#' \code{data_prev_sub_alt}.
#'
#' @usage data(data_prev_sub_alt)
"data_prev_sub_alt"


#' Intervention: treatment of mild depression (base)
#'
#' Baseline intervention parameters for the treatment of mild depression
#' episodes. Includes coverage, adherence, effectiveness, sample size, and
#' healthcare costs.
#'
#' @format A data frame with one row per intervention strategy and columns:
#' \describe{
#'   \item{cov}{Coverage of the intervention (proportion of mild cases).}
#'   \item{adh}{Adherence to the intervention (proportion).}
#'   \item{d}{Effect size or relative risk reduction (numeric).}
#'   \item{n}{Sample size or study population used for the estimate.}
#'   \item{healthcare costs}{Estimated healthcare costs per person.}
#' }
#'
#' @usage data(data_tr_mild_base)
"data_tr_mild_base"


#' Intervention: treatment of mild depression (alternative)
#'
#' Alternative scenario parameters for the treatment of mild depression. The
#' structure matches the base dataset but values can be adjusted to reflect alternative modelling
#' assumptions. In this dataset, the same numbers are provided as in the base case.
#'
#' @format Same structure as \code{data_tr_mild_base}.
#'
#' @usage data(data_tr_mild_alt)
"data_tr_mild_alt"


#' Intervention: treatment of moderate depression (base)
#'
#' Baseline intervention parameters for the treatment of moderate depression
#' episodes. Includes coverage, adherence, effect size, sample size, and
#' healthcare costs.
#'
#' @format Same structure as \code{data_tr_mild_base}.
#'
#' @usage data(data_tr_mod_base)
"data_tr_mod_base"


#' Intervention: treatment of moderate depression (alternative)
#'
#' Alternative scenario parameters for the treatment of moderate depression,
#' structurally identical to the base dataset. Values can be adjusted to reflect alternative modelling
#' assumptions. In this dataset, the same numbers are provided as in the base case.
#'
#' @format Same structure as \code{data_tr_mod_base}.
#'
#' @usage data(data_tr_mod_alt)
"data_tr_mod_alt"


#' Intervention: treatment of severe depression (base)
#'
#' Baseline intervention parameters for the treatment of severe depression
#' episodes. Includes coverage, adherence, effectiveness, sample size, and
#' healthcare costs.
#'
#' @format Same structure as \code{data_tr_sev_base}.
#'
#' @usage data(data_tr_sev_base)
"data_tr_sev_base"


#' Intervention: treatment of severe depression (alternative)
#'
#' Alternative intervention parameters for the treatment of severe depression
#' episodes. Structure matches the base dataset. Values can be adjusted to reflect alternative modelling
#' assumptions. In this dataset, the same numbers are provided as in the base case.
#'
#' @format Same structure as \code{data_tr_sev_alt}.
#'
#' @usage data(data_tr_sev_alt)
"data_tr_sev_alt"


#' Intervention: prevention of recurrent depression (base)
#'
#' Baseline intervention parameters for the prevention of recurrent depression
#' among individuals with prior depressive episodes. Includes coverage,
#' adherence, effect size, sample size, and healthcare costs.
#'
#' @format Same structure as \code{data_prev_sub_base}.
#'
#' @usage data(data_prev_rec_base)
"data_prev_rec_base"


#' Intervention: prevention of recurrent depression (alternative)
#'
#' Alternative scenario intervention parameters for the prevention of recurrent
#' depression. Structure matches the base dataset. Values can be adjusted to reflect alternative modelling
#' assumptions. In this dataset, the same numbers are provided as in the base case.
#'
#' @format Same structure as \code{data_prev_rec_alt}.
#'
#' @usage data(data_prev_rec_alt)
"data_prev_rec_alt"
