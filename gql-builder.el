;;; query-builder.el --- Query builder for GraphQL  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; GraphQL reference: https://spec.graphql.org/October2021/#sec-Introspection
;;
;; TODO:
;; 1. Add support union

;;; Code:

(require 'plz)
(require 'url-parse)
(require 'rx)

(defgroup gql-builder nil
  "GraphQL query builder.")

(defface gql-builder-field-name
  (let ((display t)
        (atts nil))
    `((,display . ,atts)))
  "Face for the name of each field in a query.")

(defface gql-builder-marked-field-name
  (let ((display t)
        (atts '(:inherit gql-builder-field-name :weight bold)))
    `((,display . ,atts)))
  "Face for the name of a marked field in a query.")

(defface gql-builder-field-type
  (let ((display t)
        (atts '(:inherit shadow)))
    `((,display . ,atts)))
  "Face for the type shown after each field.")

(defface gql-builder-arg-marker
  (let ((display t)
        (atts '(:inherit warning)))
    `((,display . ,atts)))
  "Face for the ARG marker in front of a arg for a field.")

(defvar gql-builder-marker-marked "[*] "
  "A string used to mark a marked field.")

(defvar gql-builder-marker-unmarked "[ ] "
  "A string used to mark an unmarded field.")

(defvar gql-builder-query-and-mutation-query
  "query QueryAndMutation {
    __schema {
        queryType {
            name
        }
        mutationType {
            name
        }
        types {
            name
            inputFields {
                name
                type {
                    ...TypeRef
                }
            }
            fields(includeDeprecated: true) {
                args {
                    name
                    type {
                        name
                        kind
                        ...TypeRef
                    }
                }
                name
                isDeprecated
                type {
                    name
                    kind
                    ofType {
                        name
                        kind
                        ...TypeRef
                    }
                }
            }
            interfaces {
                ...TypeRef
            }
            enumValues(includeDeprecated: true) {
                name
                description
                isDeprecated
                deprecationReason
            }
            possibleTypes {
                ...TypeRef
            }
        }
    }
}
fragment TypeRef on __Type {
    kind
    name
    ofType {
        kind
        name
        ofType {
            kind
            name
            ofType {
                kind
                name
            }
        }
    }
}"
  "Query we use to get all the queries, mutations, and types.")


;;;; Types

(cl-defstruct gql-builder-field
  "A field or arg in a query.

Queries and mutations are also fields. Queries are fields of a special
type \"Query\". (By convention the type is \"Query\", but a schema can
choose another value by the queryType field), likewise for mutations.

If TYPE is a (Type <string>), it’s either a named type, a scalar, an
enum, a union, or an interface."
  (name nil :type string :documentation "Name of the field.")
  (type nil :type list :documentation
        "Either (Type <string>), (List TYPE), or (Non-null TYPE).")
  (args nil :type (list gql-builder-field) :documentation
        "Arguments of a query. A list of input fields.")
  (subfields nil :type (list gql-builder-field) :documentation
             "Subfields of this field.
BEWARE! we load subfields lazily, so subfields being nil doesn’t mean
the field doesn’t have subfields. But if it’s expanded and subfield is
still nil, then it means this field doesn’t have subfields.")
  (mutation-p nil :type boolean :documentation
              "If non-nil, this is a mutation rather than a query.
Only makes sense for top-level fields.")
  (input-p nil :type boolean :documentation
           "If non-nil, this field is an input field (aka arg).")
  (expanded nil :type boolean :documentation
            "Whether this field has been expanded.")
  (marked nil :type boolean :documentation
          "Whether this field has been marked for export.")
  (arg-val nil :type string :documentation
           "The value for this field, if it’s an arg (aka input field).")
  (index 0 :type number :documentation
         "The index of this field among its siblings, used for display."))

;; Default printer doesn’t support versioning, so we convert fields
;; into JSON for serialization.
(defun gql-builder--serialize-field (field)
  "Serialize FIELD (a ‘gql-builder-field’) into a plist."
  (pcase-let (((cl-struct gql-builder-field
                          name type args subfields input-p
                          expanded marked arg-val index)
               field))
    (list :name name :type (prin1-to-string type)
          :args (apply #'vector (mapcar #'gql-builder--serialize-field args))
          :subfields (apply #'vector
                            (mapcar #'gql-builder--serialize-field subfields))
          :input-p input-p :expanded expanded :marked marked :arg-val arg-val
          :index index
          :version "2")))

(defun gql-builder--deserialize-field (json-field)
  "Deserialize JSON-FIELD into a ‘gql-builder-field’."
  (make-gql-builder-field
   :name (plist-get json-field :name)
   :type (read (plist-get json-field :type))
   :args (seq-map #'gql-builder--deserialize-field
                  (plist-get json-field :args))
   :subfields (seq-map #'gql-builder--deserialize-field
                       (plist-get json-field :subfields))
   :input-p (plist-get json-field :input-p)
   :expanded (plist-get json-field :expanded)
   :marked (plist-get json-field :marked)
   :arg-val (plist-get json-field :arg-val)
   :index (plist-get json-field :index)
   :mutation-p (plist-get json-field :mutation-p)))

;;;; UI state

(defvar-local gql-builder--fields '()
  "Field data for the current builder buffer. A list of ‘gql-builder-field’s.")

(defun gql-builder--rebuild-fields (old-fields new-fields schema)
  "Populate NEW-FIELDS with data from OLD-FIELDS using SCHEMA.

Specifically, copy the value of ‘expanded’, ‘marked’, ‘arg-val’, and
‘index’."
  (dolist (new-field new-fields)
    (when-let* ((new-name (gql-builder-field-name new-field))
                (old-field (seq-find
                            (lambda (old-field)
                              (equal new-name
                                     (gql-builder-field-name old-field)))
                            old-fields)))
      (setf (gql-builder-field-expanded new-field)
            (gql-builder-field-expanded old-field)

            (gql-builder-field-marked new-field)
            (gql-builder-field-marked old-field)

            (gql-builder-field-arg-val new-field)
            (gql-builder-field-arg-val old-field)

            (gql-builder-field-index new-field)
            (gql-builder-field-index old-field))

      (let ((new-subfields (gql-builder-field-subfields new-field))
            (old-subfields (gql-builder-field-subfields old-field))
            (new-args (gql-builder-field-args new-field))
            (old-args (gql-builder-field-args old-field)))
        (when (or old-subfields new-subfields)
          (setq new-subfields (gql-builder--load-subfields new-field schema))
          (gql-builder--rebuild-fields old-subfields new-subfields schema))
        (when new-args
          (gql-builder--rebuild-fields old-args new-args schema))))))

(defmacro gql-builder--get-state-at-point (key)
  "Return the field state for KEY of the field at point.
Return nil if no state exists. KEY doesn’t need to be quoted."
  (let ((field-sym (gensym)))
    `(when-let ((,field-sym (get-text-property (point) 'gql-builder-field)))
       (,(intern (concat "gql-builder-field-" (symbol-name key))) ,field-sym))))

(defmacro gql-builder--set-state-at-point (key val)
  "Set the field state for KEY to VAL for the field at point.
KEY doesn’t need to be quoted."
  (let ((field-sym (gensym)))
    `(when-let ((,field-sym (get-text-property (point) 'gql-builder-field)))
       (setf (,(intern (concat "gql-builder-field-" (symbol-name key)))
              ,field-sym)
             ,val))))

;;;; Utilities

(defsubst gql-builder--alist-get (chain alist)
  "Chained ‘alist-get’ call on ALIST.

CHAIN should be a list of symbols like (KEY1 KEY2 KEY 3).
Return (alist-get KEY3 (alist-get KEY2 (alist-get KEY1 alist))). If
chain is nil, return ALIST."
  (dolist (key chain alist)
    (setq alist (alist-get key alist))))

;;;; Retreiving and inspecting schema

(defvar gql-builder--schema-cache nil
  "An alist mapping GraqphQL endpoint URL to schema JSON object.")

(defvar-local gql-builder--schema nil
  "The schema object.")

(defvar-local gql-builder--endpoint nil
  "The endpoint URL.")

(defvar-local gql-builder--initial-body nil
  "The initial GraphQL request body the builder started with.")

(defvar-local gql-builder--restclient-state nil
  "The states of restclient request that initiated this query builder.
The value should be an alist with the following keys:

  - ‘body’: The request body.
  - ‘buffer’: The restclient buffer
  - ‘point’: The point.")

(defvar-local gql-builder--orig-window-config nil
  "The window configuration before we popped the query builder buffer.")

(defun gql-builder--decode-type (type)
  "Decode TYPE into our internal structure FIELD-TYPE.

See docstring of ‘gql-builder-field’ for possible shapes of FIELD-TYPE.
TYPE is a JSON object from the schema."
  (pcase (alist-get 'kind type)
    ("LIST" `(List ,(gql-builder--decode-type
                     (alist-get 'ofType type))))
    ("NON_NULL" `(Non-null ,(gql-builder--decode-type
                             (alist-get 'ofType type))))
    ((or "OBJECT" "INPUT_OBJECT" "SCALAR" "ENUM" "UNION" "INTERFACE")
     (alist-get 'name type))
    (kind (signal 'gql-builder-schema-error
                  (list "Unexpected kind of a type" kind type)))))

(defun gql-builder--type-name (type)
  "Return TYPE’s plain name.
TYPE is a FIELD-TYPE, return it’s name without the List, Non-null wrappers."
  (pcase type
    (`(List ,name) (gql-builder--type-name name))
    (`(Non-null ,name) (gql-builder--type-name name))
    (name name)))

(defun gql-builder--check-type (type target)
  "Return t if TYPE is a TARGET type.
TARGET can be \"String\", \"Int\", or \"Boolean\"."
  (pcase type
    ((pred stringp) (equal type target))
    (`(List ,inner-type) (gql-builder--type-string-p inner-type))
    (`(Non-null ,inner-type) (gql-builder--type-string-p inner-type))))

(defun gql-builder--get-schema (url &optional headers new)
  "Reuturn the schema at URL as a JSON object.

If HEADERS is non-nil, add those headers. It should be an alist that
looks like ((\"Content-Type\" . \"application/json\")).

If NEW is non-nil, skip the schema cache and always get from remote."
  (dolist (header `(("Accept" . "*/*")
                    ("Content-Type" . "application/json")
                    ("Host" . ,(url-host (url-generic-parse-url url)))))
    (unless (alist-get (car header) headers nil nil #'equal)
      (push header headers)))

  (or (and (not new)
           (alist-get url gql-builder--schema-cache nil t #'equal))
      (let ((schema
             (plz 'post url
               :headers headers
               :body (json-serialize
                      `(:query ,gql-builder-query-and-mutation-query))
               :as (lambda ()
                     (json-parse-buffer
                      :object-type 'alist :null-object nil)))))
        (setf (alist-get url gql-builder--schema-cache nil t #'equal)
              schema)
        schema)))

(defun gql-builder--get-all-queries-and-mutations (schema)
  "Get the list of queries in SCHEMA.

SCHEMA is a JSON object returned from ‘queery-builder--get-schema’.
Return a list of ‘gql-builder-field’s."
  (let* ((query-type-name
          (gql-builder--alist-get '(data __schema queryType name) schema))
         (mutation-type-name
          (gql-builder--alist-get '(data __schema mutationType name) schema))
         (queries (gql-builder--get-fields-for-type schema query-type-name))
         (mutations (gql-builder--get-fields-for-type schema mutation-type-name)))
    (dolist (mutation mutations)
      (setf (gql-builder-field-mutation-p mutation) t))
    (append queries mutations)))

(defun gql-builder--make-field (field &optional input-field)
  "Create a ‘gql-builder-field’ from FIELD using SCHEMA.

FIELD is an alist with ‘name’, ‘type’ as its keys. INPUT-FIELD non-nil
mean this field is an input field (aka an arg)."
  (let* ((name (gql-builder--alist-get '(name) field))
         (type (gql-builder--decode-type
                (gql-builder--alist-get '(type) field)))
         (type-name (gql-builder--type-name type))
         (args (gql-builder--alist-get '(args) field)))
    (make-gql-builder-field
     :name name
     :type type
     :args (when args
             (mapcar (lambda (field)
                       (gql-builder--make-field field t))
                     args))
     :input-p input-field)))

(defun gql-builder--make-possible-type-field (possible-type)
  "Create a ‘gql-builder-field’ from POSSIBLE-TYPE.

POSSIBLE-TYPE is an alist with ‘name’, ‘kind’ as its keys.
The created field will have name as ... on NAME, and type as NAME."
  (let* ((name (gql-builder--alist-get '(name) possible-type)))
    (make-gql-builder-field :name (format "... on %s" name) :type name)))

(defun gql-builder--get-fields-for-type
    (schema type-name &optional input-fields)
  "Get the list of fields for TYPE-NAME in SCHEMA.

SCHEMA is a JSON object returned from ‘queery-builder--get-schema’.
Return each field as a ‘gql-builder-field’.

If input-fields is non-nil, get input fields instead."
  (let* ((type-obj
          (seq-find (lambda (type)
                      (equal (gql-builder--alist-get '(name) type)
                             type-name))
                    (gql-builder--alist-get '(data __schema types) schema)))
         (fields (gql-builder--alist-get (if input-fields '(inputFields)
                                           '(fields))
                                         type-obj))
         (possible-types
          (and (not input-fields)
               (gql-builder--alist-get '(possibleTypes) type-obj))))
    (cond
     (fields
      (seq-map (lambda (field)
                 (gql-builder--make-field field input-fields))
               fields))
     ((and (null input-fields) possible-types)
      (seq-map #'gql-builder--make-possible-type-field possible-types)))))

(defun gql-builder--load-subfields (field schema)
  "Load subfields of FIELD from SCHEMA.
Return the loaded subfields. If FIELD already have subfields, just
return them."
  (pcase-let (((cl-struct gql-builder-field type subfields input-p) field))
    (or subfields
        (when-let ((new-subfields (gql-builder--get-fields-for-type
                                   schema
                                   (gql-builder--type-name type)
                                   input-p)))
          (setf (gql-builder-field-subfields field) new-subfields)))))

;;;; Building query

(defun gql-builder--serialize-fields (fields &optional indent)
  "Serialize FIELDS to a GraphQL query string.

If INDENT is non-nil, it should be the indent level, a number, and this
function will pretty print the query."
  (let ((indent-string (if indent
                           (make-string (* gql-builder-indent-steps indent) ?\s)
                         nil)))
    (string-join
     (mapcar
      (lambda (field)
        (let* ((name (gql-builder-field-name field))
               (subfields (seq-filter #'gql-builder-field-marked
                                      (gql-builder-field-subfields field)))
               (args (seq-filter #'gql-builder-field-marked
                                 (gql-builder-field-args field)))
               (serialized-args
                (if args
                    (concat
                     "("
                     (string-join
                      (mapcar #'gql-builder--serialize-arg args)
                      ", ")
                     ")")
                  "")))
          (if subfields
              (if indent-string
                  (concat indent-string name serialized-args " {\n"
                          (gql-builder--serialize-fields
                           subfields (and indent (1+ indent)))
                          indent-string "}\n")
                (format "%s%s { %s }"
                        name
                        serialized-args
                        (gql-builder--serialize-fields subfields)))
            (if indent-string
                (concat indent-string name "\n")
              name))))
      fields)
     (if indent nil " "))))

(defun gql-builder--serialize-arg (arg)
  "Serialize ARG (‘gql-builder-field’) to a GraphQL arg.
Return a string that looks like “NAME: VAL”."
  (let* ((subfields (gql-builder-field-subfields arg))
         (marked-subfields (seq-filter #'gql-builder-field-marked subfields))
         (serialized-fields (when marked-subfields
                              (concat "{ "
                                      (string-join
                                       (mapcar #'gql-builder--serialize-arg
                                               marked-subfields)
                                       ", ")
                                      " }")))
         (val (gql-builder-field-arg-val arg)))
    (format "%s: %s" (gql-builder-field-name arg)
            (or serialized-fields val "null"))))

;;;; UI: drawing UI, toggling fields

(defvar gql-builder-indent-steps 2
  "Number of spaces for each level of indentation.")

(defvar gql-builder-field-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'gql-builder-toggle-expanded)
    (define-key map (kbd "m") #'gql-builder-mark)
    (define-key map (kbd "u") #'gql-builder-unmark)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Local keymap for when point is on a field.")

;; (define-button-type 'gql-builder-field-button
;;   'action #'gql-builder-toggle-field
;;   'follow-link t
;;   'face nil)

(defun gql-builder--redraw-field (field)
  "Redraw the section of FIELD (a ‘gql-builder-field’)."
  (let ((orig-pos (point))
        (inhibit-read-only t))
    (goto-char (point-min))
    (when-let ((match (text-property-search-forward
                       'gql-builder-field field #'eq)))
      (goto-char (prop-match-beginning match))
      (let ((indentation (floor (/ (current-indentation)
                                   gql-builder-indent-steps))))
        ;; Remove child fields.
        (gql-builder--remove-fields-after-point indentation)
        ;; Remove the field itself.
        (goto-char (prop-match-beginning match))
        (let ((beg (pos-bol)))
          (forward-line 1)
          (delete-region beg (point)))
        (gql-builder--insert-fields (list field) indentation)
        (goto-char orig-pos)))))

(defun gql-builder--render-type (type &optional base)
  "Return a string that represents TYPE.

TYPE has the shape of FIELD-TYPE. List types are rendered as [TYPE],
Non-null types are rendered as TYPE!. If BASE is non-nil, don’t add the
[] and ! to the type name."
  (pcase type
    (`(List ,inner-type)
     (if base
         (gql-builder--render-type inner-type base)
       (format "[%s]" (gql-builder--render-type inner-type base))))
    (`(Non-null ,inner-type)
     (if base
         (gql-builder--render-type inner-type base)
       (format "%s!" (gql-builder--render-type inner-type base))))
    ((pred stringp) type)
    (_ (signal 'gql-builder-render-error
               (list "Unexpect shape for a FIELD-TYPE" type)))))

(defun gql-builder--sort-by-marked (fields)
  "Sort FIELDS by putting marked ones in the front.

FIELDS is a list of ‘gql-builder-field’s. Sort in-place by setting the
‘index’ field of each FIELD."
  (let ((sorted (seq-sort (lambda (a b)
                            (let ((a-marked (gql-builder-field-marked a))
                                  (b-marked (gql-builder-field-marked b))
                                  (a-name (gql-builder-field-name a))
                                  (b-name (gql-builder-field-name b)))
                              (cond
                               ((and a-marked (not b-marked)) t)
                               ((and b-marked (not a-marked)) nil)
                               (t (string< a-name b-name)))))
                          fields)))
    (cl-loop for idx = 0 then (1+ idx)
             for field in sorted
             do (setf (gql-builder-field-index field) idx))))

(defun gql-builder--sort-by-marked-recursive (fields)
  "Sort subfields of each field in FIELDS and their subfields too.
Sort by ‘gql-builder--sort-by-marked’."
  (gql-builder--sort-by-marked fields)
  (dolist (field fields)
    (let ((subfields (gql-builder-field-subfields field))
          (args (gql-builder-field-args field)))
      (gql-builder--sort-by-marked-recursive subfields)
      (when args
        (gql-builder--sort-by-marked-recursive args)))))

(defun gql-builder--insert-fields (fields indent-level)
  "Insert FIELDS at point.

FIELDS is a list of ‘gql-builder-field’. INDENT-LEVEL is the nesting
level of the fields."
  (dolist (field (seq-sort-by #'gql-builder-field-index #'< fields))
    (pcase-let (((cl-struct gql-builder-field
                            name type args subfields marked expanded arg-val input-p)
                 field))
      (insert (propertize
               (concat
                (make-string (* gql-builder-indent-steps indent-level) ?\s)
                ;; Marker box.
                (if marked
                    gql-builder-marker-marked
                  gql-builder-marker-unmarked)
                ;; ARG.
                (if input-p
                    (propertize "ARG " 'face 'gql-builder-arg-marker)
                  "")
                ;; Field name.
                (propertize (or name "N/A")
                            'face (if marked
                                      'gql-builder-marked-field-name
                                    'gql-builder-field-name))
                " "
                ;; Type.
                (if (string-match-p (rx bol "...") name)
                    ""
                  (propertize (gql-builder--render-type (or type "N/A"))
                              'face 'gql-builder-field-type))
                ;; Arg val.
                (if arg-val (gql-builder--format-arg-val arg-val) ""))
               'gql-builder-field field
               'gql-builder-indent-level indent-level
               'keymap gql-builder-field-map)
              "\n")
      ;; Insert subfields if this field is expanded.
      (when expanded
        ;; Insert args.
        (gql-builder--insert-fields args (1+ indent-level))
        ;; Insert subfields. We create subfields lazily, so if
        ;; subfields is nil, maybe we just haven’t created it.
        (setq subfields (gql-builder--load-subfields field gql-builder--schema))
        (gql-builder--insert-fields subfields (1+ indent-level))))))

(defun gql-builder--remove-fields-after-point (indent-level)
  "Remove fields after point that has an indent-level higher than INDENT-LEVEL.

Remove fields starting from the next line, regardless of whether the
current line satisfies the requirement."
  (let* ((beg (pos-bol 2))
         (match (text-property-search-forward
                 'gql-builder-indent-level indent-level
                 (lambda (indent-level level-at-point)
                   (and level-at-point
                        (<= level-at-point indent-level)))
                 t))
         (end (if match (prop-match-beginning match) (point-max))))
    (delete-region beg end)))

(defun gql-builder-toggle-expanded (&optional flag)
  "Toggle expanding the field at point.
If FLAG is 1 or -1, expand or collapse regardless of current expansion
state."
  (interactive)
  (let* ((orig-point (point))
         (field (get-text-property (point) 'gql-builder-field))
         (expanded (gql-builder-field-expanded field)))
    (setf (gql-builder-field-expanded field)
          (not expanded))
    (gql-builder--redraw-field field)
    (when (and (not expanded)
               (null (gql-builder-field-subfields field)))
      (message "Can’t find any fields for %s" (gql-builder-field-name field)))
    (goto-char orig-point)))

;; If a field is marked, it will be included in the final query that
;; we build. We store the marked/unmkared state in the UI state, like
;; we do for expanded state.
(defun gql-builder-mark ()
  "Mark the field at point."
  (interactive)
  (let* ((orig-point (point))
         (field (get-text-property (point) 'gql-builder-field))
         (marked (gql-builder-field-marked field)))
    (when (not marked)
      (setf (gql-builder-field-marked field) t
            (gql-builder-field-expanded field) t)
      (gql-builder--redraw-field field)
      (goto-char orig-point)))
  (forward-line 1))

(defun gql-builder-unmark ()
  "Unmark the field at point."
  (interactive)
  (let* ((orig-point (point))
         (field (get-text-property (point) 'gql-builder-field))
         (marked (gql-builder-field-marked field)))
    (when marked
      (setf (gql-builder-field-marked field) nil)
      (gql-builder--redraw-field field)
      (goto-char orig-point)))
  (forward-line 1))

(defsubst gql-builder--format-arg-val (val)
  "Format value of an arg, VAL.
VAL can be a string, a number, t, or :false."
  (format " [%s]" val))

(defun gql-builder-set-arg ()
  "Set the value for the arg at point."
  (interactive)
  (let* ((orig-point (point))
         (field (get-text-property (point) 'gql-builder-field))
         (type (gql-builder-field-type field))
         (old-val (gql-builder-field-arg-val field))
         (arg-p (gql-builder-field-input-p field)))
    (when arg-p
      (if (not (string-match-p (rx bos
                                   (or "String" "Int" "Boolean" "Float" "ID")
                                   (* "!")
                                   eos)
                               (gql-builder--render-type type)))
          (message "Editing array arg is not supported: %s"
                   (gql-builder--render-type type))
        (let ((val (read-string "Value: " old-val)))
          (setf (gql-builder-field-arg-val field) val
                (gql-builder-field-marked field) t)
          (gql-builder--redraw-field field)
          (goto-char orig-point)))
      (forward-line 1))))

(defun gql-builder-toggle-marked-all ()
  "Mark/unmark all the fields under this field."
  (interactive)
  (ignore 'todo))

;;;; Major mode

(defvar gql-builder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'gql-builder-refresh)
    (define-key map (kbd "r") #'gql-builder-reorder)
    (define-key map (kbd "C-c C-c") #'gql-builder-save-and-quit)
    (define-key map (kbd "v") #'gql-builder-set-arg)
    (define-key map (kbd "E") #'gql-builder-show-query-and-mutation)
    map)
  "Mode map for ‘gql-builder-mode’.")

(define-derived-mode gql-builder-mode special-mode "QueryBuilder"
  "Major mode for building GraphQL queries."
  :keymap gql-builder-mode-map
  (font-lock-mode -1))

(defun gql-builder (url &optional headers)
  "Start or resume a query builder session with GraphQL endpoint at URL.
If HEADERS is non-nil, add those headers. It should be an alist that
looks like ((\"Content-Type\" . \"application/json\"))."
  (interactive "sEndpoint: ")
  (pop-to-buffer
   (get-buffer-create (format "<query builder for %s>" url)))
  (gql-builder-mode)
  (let ((inhibit-read-only t))
    (condition-case err
        (progn
          (unless gql-builder--schema
            (setq gql-builder--schema (gql-builder--get-schema url headers)))
          (setq gql-builder--endpoint url)
          (erase-buffer)
          (save-excursion
            (gql-builder--insert-fields
             (or gql-builder--fields
                 (setq gql-builder--fields
                       (gql-builder--get-all-queries-and-mutations
                        gql-builder--schema)))
             0)))
      (plz-http-error
       (erase-buffer)
       (insert "Can’t retrieve schema from " url ":\n")
       (print err (current-buffer))))))

(defun gql-builder-refresh ()
  "Refresh schema and buffer."
  (interactive)
  ;; TODO keep ui state.
  (setq gql-builder--schema
        (gql-builder--get-schema gql-builder--endpoint nil t))
  (let ((new-fields (gql-builder--get-all-queries-and-mutations
                     gql-builder--schema))
        (old-fields gql-builder--fields))
    (gql-builder--rebuild-fields old-fields new-fields gql-builder--schema)
    (setq gql-builder--fields new-fields))
  (gql-builder-reorder))

(defun gql-builder-clear-schema-cache ()
  "Clear the schema cache."
  (interactive)
  (setq gql-builder--schema-cache nil))

(defun gql-builder-reorder ()
  "Reorder fields so marked ones come first."
  (interactive)
  (let ((inhibit-read-only t)
        (orig-point (point))
        (queries (cl-remove-if #'gql-builder-field-mutation-p
                               gql-builder--fields))
        (mutations (cl-remove-if-not #'gql-builder-field-mutation-p
                                     gql-builder--fields)))
    (erase-buffer)
    (gql-builder--sort-by-marked-recursive queries)
    (gql-builder--sort-by-marked-recursive mutations)
    (insert "Queries:\n\n")
    (gql-builder--insert-fields queries 0)
    (insert "\nMutations:\n\n")
    (gql-builder--insert-fields mutations 0)
    (goto-char (min orig-point (point-max)))))

(defun gql-builder-show-query-and-mutation ()
  "Show the GraphQL query this builder will generate."
  (interactive)
  (let* ((query-fields (seq-filter #'gql-builder-field-marked
                                   (cl-remove-if #'gql-builder-field-mutation-p
                                                 gql-builder--fields)))
         (mutation-fields (seq-filter #'gql-builder-field-marked
                                      (seq-filter #'gql-builder-field-mutation-p
                                                  gql-builder--fields)))
         (inner-query (gql-builder--serialize-fields query-fields 0))
         (inner-mutation (gql-builder--serialize-fields mutation-fields 0)))
    (pop-to-buffer "*gql-builder show query and mutation*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert inner-query inner-mutation)
      (goto-char (point-min))
      (insert "  ")
      (while (eq 0 (forward-line 1))
        (unless (looking-at (rx (* whitespace) eol))
          (insert "  ")))
      (insert "}")
      (goto-char (point-min))
      (insert "{\n"))
    (special-mode)))

;;; Restclient integration

(defvar gql-builder--data-store-location
  (if (file-exists-p (expand-file-name "var" user-emacs-directory))
      (expand-file-name "var/gql-builder/gql-builder-data.el"
                        user-emacs-directory)
    (expand-file-name "gql-builder-data.el" user-emacs-directory))
  "Location of the data store.")

(defvar gql-builder--data-store nil
  "Maps GraphQL query string to the fields state for the query builder.
The value is an alist mapping (ENDPOINT-URL QUERY-STRING) to UI
states (list of ‘gql-builder-field’s). QUERY-STRING doesn’t have
trailing whitespace/newline.")

(defun gql-builder--load-data-store ()
  "Load cached queries form data store file."
  (when (file-exists-p gql-builder--data-store-location)
    (with-temp-buffer
      (insert-file-contents gql-builder--data-store-location)
      (goto-char (point-min))
      (setq gql-builder--data-store
            (mapcar (lambda (entry)
                      (cons (plist-get entry :key)
                            (mapcar #'gql-builder--deserialize-field
                                    (plist-get entry :fields))))
                    (json-parse-buffer :object-type 'plist
                                       :array-type 'list
                                       :false-object nil))))))

(defun gql-builder--save-fields ()
  "Save the current UI state to query cache."
  (unless gql-builder--endpoint
    (signal 'gql-builder-error '("Current buffer doesn’t have a saved GraphQL endpoint (‘gql-builder--endpoint’)")))
  (unless gql-builder--initial-body
    (signal 'gql-builder-error '("Current buffer doesn’t have a saved initial query (‘gql-builder--initial-body’)")))

  (setf (alist-get (list gql-builder--endpoint gql-builder--initial-body)
                   gql-builder--data-store nil t #'equal)
        gql-builder--fields))

(defun gql-builder--save-data-store ()
  "Save cached queries into data store file."
  (when gql-builder--data-store-location
    (let ((dir (file-name-directory gql-builder--data-store-location)))
      (unless (file-exists-p dir)
        (mkdir dir t)))
    (with-temp-buffer
      (print gql-builder--data-store (current-buffer))
      (write-region (point-min) (point-max) gql-builder--data-store-location))))

(defun gql-builder--restclient-show-gql-builder
    (method url headers body pos)
  "Show query builder for this request with METHOD, URL, HEADERS, BODY.

POS is the position of point when user invoked ‘restclient-gql-builder’.

This function is supposed to be called by
‘restclient-http-parse-current-and-do’."
  (unless (equal method "GQL")
    (signal 'gql-builder-error '("GraphQL request should have GQL for the method")))

  (unless gql-builder--data-store
    (gql-builder--load-data-store))

  (let ((buf (current-buffer))
        (window-config (current-window-configuration))
        ;; This is a bit hacky, but it’s the only way to get the
        ;; original body before restclient substitutes variables in
        ;; it.
        (unsubstituted-body
         (let ((cmax (restclient-current-max)))
           (restclient-parse-body
            (buffer-substring (min (point) cmax) cmax) nil))))

    (gql-builder url headers)
    (setq gql-builder--orig-window-config window-config)
    (setq gql-builder--restclient-state (list (cons 'body unsubstituted-body)
                                              (cons 'buffer buf)
                                              (cons 'point pos)))
    (when (and unsubstituted-body (not (equal unsubstituted-body "")))
      (let ((fields (alist-get (list url (string-trim unsubstituted-body))
                               gql-builder--data-store
                               nil t #'equal)))
        (setq gql-builder--initial-body (string-trim unsubstituted-body))
        (if (null fields)
            (message "Can’t resume the query, query builder can only resume query built by itself, it can’t parse an existing query")
          (setq gql-builder--fields fields)
          (gql-builder-reorder))))))

(defun restclient-gql-builder ()
  "Popup a query builder buffer to edit the query in the current request."
  (interactive)
  (restclient-http-parse-current-and-do
   #'gql-builder--restclient-show-gql-builder
   (point)))

(defun gql-builder-save-and-quit ()
  "Save the query in the query builder to the original restclient buffer.
And quit the query builder."
  (interactive)
  (let* ((body (alist-get 'body gql-builder--restclient-state))
         (buffer (alist-get 'buffer gql-builder--restclient-state))
         (pos (alist-get 'point gql-builder--restclient-state))
         (query-fields (seq-filter #'gql-builder-field-marked
                                   (cl-remove-if #'gql-builder-field-mutation-p
                                                 gql-builder--fields)))
         (muation-fields (seq-filter #'gql-builder-field-marked
                                     (seq-filter #'gql-builder-field-mutation-p
                                                 gql-builder--fields)))
         (query (concat (if query-fields "{\n" "mutation {\n")
                        (gql-builder--serialize-fields
                         (or query-fields muation-fields) 1)
                        "}"))
         (new-body query))
    (setf (alist-get (list gql-builder--endpoint (string-trim new-body))
                     gql-builder--data-store nil t #'equal)
          gql-builder--fields)
    (gql-builder--save-data-store)
    (when buffer
      (with-current-buffer buffer
        (goto-char pos)
        (goto-char (restclient-current-min))
        (if (or (null body) (equal body ""))
            ;; Empty body
            (if (not (re-search-forward
                      restclient-method-url-regexp (point-max) t))
                (signal 'gql-builder-error
                        '("Can’t find the original request header"))
              (forward-line)
              (while (or (looking-at restclient-response-hook-regexp)
                         (and (looking-at restclient-header-regexp) (not (looking-at restclient-empty-line-regexp)))
                         (looking-at restclient-use-var-regexp))
                (forward-line))
              (insert "\n" new-body))
          ;; Non-empty body
          (if (search-forward body nil t)
              (progn
                (replace-match new-body t t)
                (insert "\n"))
            (if (yes-or-no-p "Can’t find the original request body, insert at the end? ")
                (progn
                  (goto-char (restclient-current-max))
                  (insert new-body "\n"))
              (signal 'gql-builder-error
                      '("Can’t find the original request body"))))))))

  (when gql-builder--orig-window-config
    (set-window-configuration gql-builder--orig-window-config)))

(provide 'gql-builder)

;;; gql-builder.el ends here
