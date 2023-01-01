;;; create-ob-npx.el --- Builder macro for org babel packages that delegate to npx

(defmacro create-ob-npx (name language)
  (let ((default-header-args (make-symbol (format "org-babel-default-header-args:%2$s" name language)))
        (variable-assignments (make-symbol (format "org-babel-variable-assignments:%2$s" name language)))
        (var-to-language (make-symbol (format "org-babel-%1$s-variable-to-%2$s" name language)))
        (org-babel-execute-language (make-symbol (format "org-babel-execute:%2$s" name language))))
   `(progn
     (require 'ob)

     (defvar ,default-header-args '())

     (defun ,variable-assignments (params)
       "Return list of javascript-compatible statements assigning the block's variables."
       (-map (-lambda ((name . value)) (format "let %s=%s;"
                                               name
                                               (,var-to-language value)))
             (org-babel--get-vars params)))

     (defun ,var-to-language (value)
      "Convert Value to language-specific syntax."
      (json--with-output-to-string (json--print value)))

     (defun ,org-babel-execute-language (body params)
       "Execute a block of typescript code using ts-node. This function is called by `org-babel-execute-src-block'"
       (let* ((tmp-src-file (org-babel-temp-file "ts-src-" ".ts")))
         (with-temp-file tmp-src-file
           (insert (org-babel-expand-body:generic)
                   body params
                   (org-babel-variable-assignments:typescript params)))
         (org-babel-eval (format "npx ts-node %s"
                                 tmp-src-file)
                         ""))))))

