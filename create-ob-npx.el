;;; create-ob-npx.el --- Builder macro for org babel packages that delegate to npx

;; Author: George Mauer
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/togakangaroo/create-ob-npx

;;; License:
;; MIT <https://opensource.org/licenses/MIT>

;;; Code:

(defvar create-ob-npx:npx-command "npx"
  "Command to be executed in order to invoke npx. Potentially the path to npx, or just \"npx\" if already on PATH")

(cl-defmacro create-ob-npx (&key (name (user-error "name keyword required"))
                            &key (language (user-error "language keyword required"))
                            &key (npx-arguments (user-error "npx-arguments keyword required"))
                            &key (file-extension "")
                            &key (post-process (function 'identity)))
  "Define an org babel language that, when the associated src block is executed, will run it through npx.

  Parameters:

  All parameters are are labeled with keywords. No positional parameters

  name - name of the plugin to be used in symbol names and file names
  language - name of the language to be used in symbol names
  npx-arguments - ultimately we run npx <npx-arguments> org-babel-default-header-args:<language> temp-script-filename this argument should at a minimum contain the npx package to use but might contain flags and other paramters as well
  file-extension - OPTIONAL if provided, file extension that will be used when writing code to temp-script-filename
  post-process - OPTIONAL a function that will be used to post-process output. This will be passed a string containing the stdout produced by the npx command. Useful for when you want to discard stdout and for example insert a fixed string or file contents

  Results of execution:
  Defines the following that can be customized as needed (see each for further documentation)

    org-babel-default-header-args:<language> variable
    org-babel-variable-assignments:<language> function
    org-babel-<name>-variable-to-<language> function
    org-babel-execute:<language> function
  "
  (let ((default-header-args (intern (format "org-babel-default-header-args:%2$s" name language)))
        (variable-assignments (intern (format "org-babel-variable-assignments:%2$s" name language)))
        (var-to-language (intern (format "org-babel-%1$s-variable-to-%2$s" name language)))
        (org-babel-execute-language (intern (format "org-babel-execute:%2$s" name language))))
   `(progn
     (require 'ob)
     (require 'dash)

     (defvar ,default-header-args '()
       ,(format "Default header arguments to be used when executing %s" language))

     (defun ,variable-assignments (params)
       "Return list of javascript-compatible statements assigning the block's variables."
       (-map (-lambda ((name . value)) (format "let %s=%s;"
                                               name
                                               (,var-to-language value)))
             (org-babel--get-vars params)))

     (defun ,var-to-language (value)
      ,(format "Convert value to %s-specific syntax." language)
      ;; Note that in the case of plain-ol javascript which is the default, variable values when supplied from org tables or lists will be lists of lists. The json-encode function assumes this is a plist and tries to convert it to an object. For nested lists, we shoudl be calling json-encode-array.
      (if-let* ((first-item (and (listp value) (car value)))
                (length-of-first-item (and (listp first-item) (length first-item)))
                (is-table (--every (and (listp it) (eq length-of-first-item (length it))) value)))
               (json-encode-array value)
               (json-encode value)))


     (defun ,org-babel-execute-language (body params)
       ,(format "Execute a block of %s code using npx with arguments: '%s'. This function is called by org-babel-execute-src-block" language npx-arguments)
       (let* ((tmp-src-file (org-babel-temp-file ,name ,file-extension))
              (var-lines (,variable-assignments params)))
         (with-temp-file tmp-src-file
           (insert (org-babel-expand-body:generic body params var-lines)))
         (--> (list ,create-ob-npx:npx-command
                    ,npx-arguments
                    ,default-header-args
                    tmp-src-file)
              -non-nil
              (mapconcat 'identity it " ")
              (org-babel-eval it " ")
              (funcall ,post-process it)))))))
