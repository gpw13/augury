% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_lme4.R
\name{predict_lme4_data}
\alias{predict_lme4_data}
\title{Generate prediction from model object}
\usage{
predict_lme4_data(df, model, pred_col, pred_upper_col, pred_lower_col)
}
\arguments{
\item{df}{Data frame of model data.}

\item{model}{An lme4 function that outputs a merMod object with that can be
passed to \code{\link[merTools:predictInterval]{merTools::predictInterval()}}. This should be one of \code{\link[lme4:lmer]{lme4::lmer()}},
\code{\link[lme4:glmer]{lme4::glmer()}}, or \code{\link[lme4:nlmer]{lme4::nlmer()}}.}

\item{pred_col}{Column name to store predicted value.}

\item{pred_upper_col}{Column name to store upper bound of confidence interval
generated by the \code{predict_...} function. This stores the full set of generated
values for the upper bound.}

\item{pred_lower_col}{Column name to store lower bound of confidence interval
generated by the \code{predict_...} function. This stores the full set of generated
values for the lower bound.}
}
\value{
A data frame.
}
\description{
\code{predict_lme4_data()} generates a prediction vector from a merMod object and full
data frame, putting this prediction back into the data frame.
}
