#lang mzscheme

(require mzlib/kw
         (prefix q: (file "sql-lang.ss")))

; Procedures -------------------------------------

(define/kw (mzscheme:select #:key [what #f] [distinct #f] from [where #f] [group null] [order null] [having #f] [limit #f] [offset #f])
  (q:select/internal what distinct from where group order having limit offset))

; Provide statements -----------------------------

(provide (all-from-except (file "sql-lang.ss") q:select q:select/internal)
         (rename mzscheme:select q:select))