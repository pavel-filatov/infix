#' @importFrom rlang enquo quo_name get_env
#' @importFrom purrr map_lgl

infix_input_checker <- function(...) {
  if (!all(purrr::map_lgl(list(...), is.numeric))) {
    stop("Only numeric inputs allowed!")
  }
}

get_lhs_env <- function(.lhs) {
  .lhs_quo <- rlang::enquo(.lhs)
  rlang::get_env(.lhs_quo)
}

#' @export
"%+=%" <- function(.lhs, .rhs) {
  .lhs_quo <- rlang::enquo(.lhs)
  .lhs_env <- rlang::get_env(.lhs_quo)

  infix_input_checker(.lhs, .rhs)

  .lhs_name <- rlang::quo_name(.lhs_quo)

  assign(.lhs_name, .lhs + .rhs, envir = .lhs_env)
}

#' @export
"%*=%" <- function(.lhs, .rhs) {
  .lhs_quo <- rlang::enquo(.lhs)
  .lhs_env <- rlang::get_env(.lhs_quo)

  infix_input_checker(.lhs, .rhs)

  .lhs_name <- rlang::quo_name(.lhs_quo)

  assign(.lhs_name, .lhs * .rhs, envir = .lhs_env)
}

#' @export
"%-=%" <- function(.lhs, .rhs) {
  .lhs_quo <- rlang::enquo(.lhs)
  .lhs_env <- rlang::get_env(.lhs_quo)

  infix_input_checker(.lhs, .rhs)

  .lhs_name <- rlang::quo_name(.lhs_quo)

  assign(.lhs_name, .lhs - .rhs, envir = .lhs_env)
}

#' @export
"%/=%" <- function(.lhs, .rhs) {
  if (.rhs == 0) {
    message("Zero dividing!")
  }

  .lhs_quo <- rlang::enquo(.lhs)
  .lhs_env <- rlang::get_env(.lhs_quo)

  infix_input_checker(.lhs, .rhs)

  .lhs_name <- rlang::quo_name(.lhs_quo)

  assign(.lhs_name, .lhs / .rhs, envir = .lhs_env)
}
