#' By State Data on Teacher Pay (and more)
#'
#' A dataset containing the pay of teachers (real and COLA) along with
#' some political and population data for 50 states and D.C. Original source
#' is NPR with additions made from various net sources (mainly wikipedia)
#'
#'
#' @format A data frame with 51 rows and 15 variables:
#' \describe{
#'   \item{state}{Name of the State}
#'   \item{abbreviation}{State Postal Abbreviation}
#'   \item{adjusted_pay} {The cost of living adjusted average pay of teachers in that state}
#'   \item {actual_pay}{The actual average pay of teachers in that state}
#'   \item {strike2018_2019}{Whether there was a reported teachers strike in that state in the years 2018-2019}
#'   \item {clinton_votes_2016} {total Clinton votes in 2016 election}
#'   \item {trump_votes_2016} {total Trump votes in 2016 electio}
#'   \item {population2018} {reported population in 2018}
#'   \item {percent_union2018} {percent of workers in the state who are reported to belong to any union in 2018}
#'   \item {pct_clinton}{Clinton votes over total population * 100}
#'   \item {pct_trump}{Trump votes over total population * 100}
#'   \item {log_pop}{Log base 10 of population }
#'   \item {division}{US Census Division Number}
#'   \item {div_name}{US Census Division Name}
#'   \item {pct_white_2012}{Percent self-identifying as White in 2012}
#'
#' }
#' @source \url{https://www.npr.org/sections/ed/2018/03/16/592221378/the-fight-over-teacher-salaries-a-look-at-the-numbers}
"teacher_pay"
