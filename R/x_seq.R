#' Creates sequences of x's
#' @export x_seq
x_seq <- function(limits, x) {

  limits %>%
    nest() %>%
    mutate(temp = map(data,
                      ~tibble(!!quo_name(x) := seq(.$xmin, .$xmax,
                                                   length = 300)))) %>%
    select(-data) %>%
    unnest(temp) %>%
    group_by(UQS(groups(limits)))

}

