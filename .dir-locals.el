;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((ess-r-mode . ((flycheck-lintr-linters . "with_defaults(
  line_length_linter(80),
  nonportable_path_linter,
  object_name_linter(styles = c(\"camelCase\", \"snake_case\")),
  T_and_F_symbol_linter
  )")
                (ess-style . RStudio))))
