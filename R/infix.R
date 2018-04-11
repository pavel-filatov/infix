#' @importFrom rlang enquo quo_name get_env
#' @importFrom purrr map_lgl

infix_input_checker <- function(...) {
  .dots <- list(...)
  if (!all(purrr::map_lgl(.dots, is.numeric))) {
    stop(message("Only numeric inputs allowed!"))
  }
}

#' @export
"%+=%" <- function(.lhs, .rhs) {
  infix_input_checker(.lhs, .rhs)

  .lhs_quo <- rlang::enquo(.lhs)
  .lhs_name <- rlang::quo_name(.lhs_quo)

  assign(.lhs_name, .lhs + .rhs, envir = rlang::get_env(.lhs_quo))
}

#' @export
"%-=%" <- function(.lhs, .rhs) {
  infix_input_checker(.lhs, .rhs)

  .lhs_quo <- rlang::enquo(.lhs)
  .lhs_name <- rlang::quo_name(.lhs_quo)

  assign(.lhs_name, .lhs - .rhs, envir = rlang::get_env(.lhs_quo))
}

#' @export
"%*=%" <- function(.lhs, .rhs) {
  infix_input_checker(.lhs, .rhs)

  .lhs_quo <- rlang::enquo(.lhs)
  .lhs_name <- rlang::quo_name(.lhs_quo)

  assign(.lhs_name, .lhs * .rhs, envir = rlang::get_env(.lhs_quo))
}

#' @export
"%/=%" <- function(.lhs, .rhs) {
  infix_input_checker(.lhs, .rhs)

  if (.rhs == 0) {
    message("You divide by zero!")
  }

  .lhs_quo <- rlang::enquo(.lhs)
  .lhs_name <- rlang::quo_name(.lhs_quo)

  assign(.lhs_name, .lhs / .rhs, envir = rlang::get_env(.lhs_quo))
}
