

args <- list(x = "Sample Size", y = NULL)
labs_call <- eval(rlang::call2("labs", !!!args, .ns = "ggplot2"))

rlang::call_modify(
  rlang::call2("labs", !!!args, .ns = "ggplot2"),
  alpha = "hello"
)

ggplot() +
  labs_call



call("labs", list(x = "Sample Size", y = NULL), alpha = "hello", quote(color = ))
