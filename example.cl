(load (merge-pathnames "cgen.cl" *load-truename*))
(write-code 
  (codes (cprepro-include "blah")
         (cprepro-include "blur")
         (cprepro-include "stdio.h" t)
         (cdef-function "void" 
                        "test_func" 
                        '() (codes (cstmt-if 
                                     "1 == 1"
                                     '("a;" "b;")
                                     '(("1 == 2" "b;")
                                       (nil "a;")))
                                   (cstmt-for "int i = 0"
                                              "i < n"
                                              "i++"
                                              (codes "a;" "b;"))
                                   (cstmt-while "true" '("bbb;"))
                                   (cstmt-switch "testexpr" 
                                                 '(
                                                   ("1" "hi;")
                                                   (("2" "3") "hoy;" "yeah;")
                                                   (("4" "5") "huh?")
                                                   (nil "yoshi."))))))
  "/dev/stdout")

