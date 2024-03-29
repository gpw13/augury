% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_inla.R
\name{predict_inla_data}
\alias{predict_inla_data}
\title{Generate prediction from an INLA output object}
\usage{
predict_inla_data(df, model, pred_col, pred_upper_col, pred_lower_col)
}
\arguments{
\item{df}{Data frame of model data.}

\item{model}{INLA model object returned by \code{\link[INLA:inla]{INLA::inla()}} which contains
\code{summary.fitted.values}.}

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
\code{predict_inla_data()} generates a prediction vector from an \code{\link[INLA:inla]{INLA::inla()}}
output object, putting this prediction back into the data frame.
}
