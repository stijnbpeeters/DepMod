#' Model parameters list
#'
#' A named list of scalar parameters used in the disease progression and
#' cost-effectiveness model. Each element is a single numeric value.
#'
#' @format A named list with 40 elements:
#' \describe{
#'   \item{excess mortality}{Excess mortality multiplier.}
#'   \item{retirement rate}{Annual retirement rate.}
#'   \item{death rate}{Baseline annual death rate.}
#'   \item{mean duration of chronicity (year)}{Mean duration of chronic disease (years).}
#'
#'   \item{increased relapse 1}{Relapse multiplier for category 1.}
#'   \item{increased relapse 2}{Relapse multiplier for category 2.}
#'   \item{increased relapse 3}{Relapse multiplier for category 3.}
#'   \item{increased relapse 4}{Relapse multiplier for category 4.}
#'   \item{increased relapse 5}{Relapse multiplier for category 5.}
#'
#'   \item{discount rate daly averted}{Annual discount rate applied to DALYs averted.}
#'   \item{discount rate costs}{Annual discount rate applied to costs.}
#'
#'   \item{dw conversion factor}{Disability weight conversion factor.}
#'   \item{Lower range dw conversion factor}{Lower bound of the disability weight conversion factor.}
#'   \item{Upper range dw conversion factor}{Upper bound of the disability weight conversion factor.}
#'
#'   \item{Scale/shape Gamma cost distribution}{Scale/shape parameter for a Gamma cost distribution.}
#'   \item{Incidence no history}{Incidence among individuals with no prior history.}
#'
#'   \item{pmild}{Proportion of incident cases that are mild.}
#'   \item{pmoderate}{Proportion of incident cases that are moderate.}
#'   \item{psevere}{Proportion of incident cases that are severe.}
#'
#'   \item{mildrecovery}{Probability of full recovery from mild disease.}
#'   \item{mildpartial}{Probability of partial recovery from mild disease.}
#'   \item{mildchronic}{Probability of chronic course after mild disease.}
#'
#'   \item{moderaterecovery}{Probability of full recovery from moderate disease.}
#'   \item{moderatepartial}{Probability of partial recovery from moderate disease.}
#'   \item{moderatechronic}{Probability of chronic course after moderate disease.}
#'
#'   \item{severerecovery}{Probability of full recovery from severe disease.}
#'   \item{severepartial}{Probability of partial recovery from severe disease.}
#'   \item{severechronic}{Probability of chronic course after severe disease.}
#'
#'   \item{mildrecoverycured}{Among mild recoveries, probability of being cured.}
#'   \item{mildrecoveryrelapse}{Among mild recoveries, probability of relapse.}
#'   \item{mildpartialcured}{Among mild partial recoveries, probability of being cured.}
#'   \item{mildpartialrelapse}{Among mild partial recoveries, probability of relapse.}
#'
#'   \item{moderaterecoverycured}{Among moderate recoveries, probability of being cured.}
#'   \item{moderaterecoveryrelapse}{Among moderate recoveries, probability of relapse.}
#'   \item{moderatepartialcured}{Among moderate partial recoveries, probability of being cured.}
#'   \item{moderatepartialrelapse}{Among moderate partial recoveries, probability of relapse.}
#'
#'   \item{severerecoverycured}{Among severe recoveries, probability of being cured.}
#'   \item{severerecoveryrelapse}{Among severe recoveries, probability of relapse.}
#'   \item{severepartialcured}{Among severe partial recoveries, probability of being cured.}
#'   \item{severepartialrelapse}{Among severe partial recoveries, probability of relapse.}
#' }
#'
#' @usage data(parameters)
#' @keywords datasets
#'
#' @examples
#' data(parameters)
#' names(parameters)
#' parameters[["excess mortality"]]
"parameters"
