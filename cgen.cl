; This library is published under WTFPL. The content is given as following:
; 
;         DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
;                     Version 2, December 2004 
; 
;  Copyright (C) 2025 27Onion Nebell <zzy20080201@gmail.com> 
; 
;  Everyone is permitted to copy and distribute verbatim or modified 
;  copies of this license document, and changing it is allowed as long 
;  as the name is changed. 
; 
;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
;    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 
; 
;   0. You just DO WHAT THE FUCK YOU WANT TO.

(require :uiop)
;; Write code to target file.
;; Code is a list of code lines, and file-name is the name of (path to) the target file.
(defun write-code (code file-name)
  (with-open-file (out-file file-name
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (loop for line in code do (format out-file "~A~%" line))))

;; A bunch of codes. The element can be string or list of strings.
(defun codes (&rest codes-args) 
  (reduce (lambda (a b) (if
                          (or (typep b 'cons)
                              (typep b 'null))
                          (append a b) (append a `(,b))))
          (append '(nil) codes-args)))

(defun cdpr (condexpr &rest codes-args)
  (append `(,condexpr)
     (reduce (lambda (a b) (if
                              (or (typep b 'cons)
                                  (typep b 'null))
                              (append a b) (append a `(,b))))
              (append '(nil) codes-args))))

(defun indent-codes (lines)
  (map 'list
       (lambda (line) (concatenate 'string "    " line))
       lines))

;; Flatten a tree-like code.
(defun flatten-codes (c)
  (if (null c) nil
    (if (atom (first c))
      (cons (first c) (flatten-codes (rest c)))
      (append (flatten-codes (first c)) (flatten-codes (rest c))))))

;; A function definition.
;; return-type is a string, denotes the return type.
;; func-name is the name of the function.
;; arguments could be list of arguments, or a plain string containing all arguments.
;; body is a list of string, containing codes.
(defun cdef-function (return-type func-name arguments body)
  (let ((normalize-args (if
                          (or (typep arguments 'cons) 
                              (typep arguments 'null)) 
                          (format nil "~{~A~^, ~}" arguments)
                          arguments)))
    (append `(,(format nil "~A ~A(~A) {" return-type func-name normalize-args))
            (map 'list
                 (lambda (line) (concatenate 'string "    " line))
                 body)
            '("}"))))

;; An `if` statement.
;; condi is a string, the condition;
;; body is the body of the statement, a list of string;
;; the options elses contains a list of lists. In every element, the first element of it should be a condition or nil, and the rest will be treat as body.
(defun cstmt-if (condi body &optional elses)
  (append `(,(format nil "if (~A) {" condi))
          (map 'list
               (lambda (line) (concatenate 'string "    " line))
               body)
          (if (not elses) 
            '("}") 
            (funcall
              (lambda (f body) (funcall f f body)) 
              (lambda (f body) 
                (if (null (cdr body)) 
                  (append `(,(format nil
                                     "} else~A {" 
                                     (if (car (car body))
                                       (format nil 
                                               " if (~A)"
                                               (car (car body)))
                                       "")))
                          (map 'list
                               (lambda (line) (concatenate 'string "    " line))
                               (cdr (car body)))
                          '("}")) 
                  (if (car (car body)) 
                    (append `(,(format nil
                                       "} else if (~A) {"
                                       (car (car body))))
                            (map 'list
                                 (lambda (line) (concatenate 'string "    " line))
                                 (cdr (car body)))
                            (funcall f f (cdr body)))
                    (error "Cannot have else before else ifs")))) 
              elses))))

;; Shorthand for it with an else branch.
;; cond-expr is a string represents condition.
;; body-if is the body of if, a list of strings.
;; body-else is the body of else, another list of strings.
(defun cstmt-ifelse (cond-expr body-if body-else)
  (cstmt-if cond-expr body-if `( ,(append '(nil) body-else))))

;; A `switch` statement.
;; var-expr is the expression to switch.
;; cases is list of lists; each list in the lists starts with a string or a list, denoting cases, or nil, denoting the default case.
;; safe-wrap enables safe wrapper around each cases (case A: case B: {} break;).
(defun cstmt-switch (var-expr cases &optional (safe-wrap t))
  (codes (format nil "switch (~A) {" var-expr)
         (flatten-codes
           (map 'list (lambda (a-case)
                        (if (car a-case)
                          (if (typep (car a-case) 'cons)
                            (codes
                              (loop for i in (car a-case) collect (format nil "case ~A:" i))
                              (if safe-wrap "{" "")
                              (if safe-wrap (indent-codes (cdr a-case)) (cdr a-case))
                              (if safe-wrap "} break;" ""))

                            (codes
                              (if safe-wrap (format nil "case ~A: {" (car a-case)) (format nil "case ~A:" (car a-case)))
                              (if safe-wrap (indent-codes (cdr a-case)) (cdr a-case))
                              (if safe-wrap "} break;" "")))
                          `(,(if safe-wrap "default: {" "default: ")
                             ,(if safe-wrap (indent-codes (cdr a-case)) (cdr a-case))
                             ,(if safe-wrap "} break;" "")
                             )))
                cases))))

;; A for loop.
;; init-stmt is the initializaiton statement.
;; cond-expr is the condition.
;; after-expr is the after expression.
;; the above are all strings, and then the body, list of strings.
(defun cstmt-for (init-stmt cond-expr after-expr body) 
  (append `(,(format nil "for (~A; ~A; ~A) {" init-stmt cond-expr after-expr)) 
          (map 'list
               (lambda (line) (concatenate 'string "    " line))
               body)
          '("}")))

;; A while loop.
;; cond-expr is the condition.
;; the above are all strings, and then the body, list of strings.
(defun cstmt-while (cond-expr body) 
  (append `(,(format nil "while (~A) {" cond-expr)) 
          (map 'list
               (lambda (line) (concatenate 'string "    " line))
               body)
          '("}")))

;; A while loop.
;; body is a list of code lines, and cond-expr is the condition.
(defun cstmt-do-while (body cond-expr) 
  (append '("do {") 
          (map 'list
               (lambda (line) (concatenate 'string "    " line))
               body)
          `(,(format nil "} while (~A);" cond-expr))))

;; An include directive
(defun cprepro-include (name &optional (angled-quote nil))
  (format nil "#include ~C~A~C"
          (if angled-quote #\< #\")
          name
          (if angled-quote #\> #\")))


;; Run generator program.
;; Program reads file name from command-line arguments, and pass to fn.
(defun run-gen (fn)
  (let ((args (uiop:command-line-arguments)))
    (if (null args)
      (format *error-output* "Please specify a filename. ~%")
      (funcall fn (car args)))))

