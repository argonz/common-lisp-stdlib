(in-package #:cl-user)
(defpackage #:masd
  (:use 
   #:common-lisp
   #:iterate
   #:ch-util)

  (:export 

   ;; symbols&macros
   #:lisp-code->varsymbols
   #:lisp-code->counted-varsymbols
   #:replace-symbol
   #:replace-clause
   #:replace-clauses-list-form
   #:replace-clauses
   #:gensyms
   #:oncesyms
   #:defmch
   #:count-symbol
   #:s-symbols
   #:not-s-symbols
   #:find-sexpr-clauses
   #:labels*
   #:defstruct-export
   #:lambda-self
   #:lots-of
   #:concatenate-symbol
   #:consym
   #:concat-symbol			;torolni !! 
   #:concat-symbol-f
   #:concat-symbol-into-package
   #:concat-symbol-f-into-package
   #:symbol-into-package
   #:make-keyword
   #:symbol->keyword
   #:sym-intern-copy
   #:sexp-intern-copy
   #:sexp-intern-copy-selective
   #:internalize-sexp
   #:substit-symbols-in-symbol
   

   ;; math
   #:most-rel-seq
   #:max-seq
   #:min-seq
   #:max-list
   #:min-list
   #:sqr
   #:summa
   #:summa-abs
   #:summa-sqr
   #:product
   #:avg
   #:avg-sequence
   #:geometric-mean
   #:arithmetic-mean
   #:-sequences
   #:+sequences
   #:/sequences
   #:*sequences
   #:-sequence
   #:+sequence
   #:/sequence
   #:*sequence
   #:cross-product
   #:distance
   #:norm
   #:norm-sequence
   #:norm-direction
   #:scalar-product
   #:cos-of-sequences
   #:angle-of-sequnces
   #:1/+
   #:un1/+
   #:statistics

   ;; random
   #:random-banned
   #:random-banned-n
   #:random-element
   #:random-element-n
   #:random-element-banned
   #:random-element-banned-n

   ;; sequence
   #:selection
   #:cons-unique
   #:transpose-list
   #:lref
   #:tupelize
   #:sequenize
   #:tuples-from-list
   #:list-from-tuples
   #:select-indicators
   #:remove-indicators
   #:keyword-successives
   #:remove-keyword-successives
   #:split
   #:rotate-left
   #:rotate-right
   #:rotate
   #:inslode
   #:relist
   #:mapcdr
   #:mapcar-append
   #:mapcar-selective
   #:remove-once
   #:group
   #:deletes
   #:removes
   #:push-unique
   #:set-same
   #:equalv
   #:elements-with-pattern
   #:associate
   #:ortonalite
 
   
   ;; structure
   #:class-slots
   #:class-slot-names
   #:class-slot-names-keywords
   #:make-instance-un
   #:make-instance-mun
   #:struct-slot-names
   #:struct-slot-name-keywords
   #:struct-slot-names-m
   #:defstruct-unio
   #:diff-copy
   #:unbound-slots-to-nil
   #:indiff-name
   #:make-instance-un
   #:defstructi
   #:defclassi
   #:defclassia

   ;;class
   #:cmp-slot-get-name
   #:cmp-slot-set-name
   #:cmp-type?-name
   #:cmp-component?-name
   #:cmp-init-name
   #:cmp-component-init-name
   #:cmp-copy-name
   #:cmp-copy-selective-name
   #:cmp-copy-into-name
   #:cmp-copy-into-selective-name
   #:cmp-copy-different
   #:defcmp   


   ;; clause
   #:ifit
   #:ifnil

   ;; array
   #:arr-length
   #:arr-last
   #:arr-butlast
   #:copy-array
   #:list->array
   #:array->list
   
   ;; string
   #:chars->string
   #:string->chars
   #:split-by-character
   #:implode
   #:string-to-nr

   ;; hash
   #:hash-keys 
   #:hash-values 
   #:hash-key-value-pairs 
   #:hash-key->value 
   #:hash-keys->collect-values 
   #:hash-keys->append-values 
   #:hash-value->key 
   #:hash-value->all-keys 
   #:hash-has-key? 
   #:hash-has-value? 
   #:hash-key->value! 
   #:hash-key->cons-value! 

   #:hash-set! 
   #:hash-set-if-new! 
   #:hash-push! 
   #:hash-pushnew! 

   #:hash-set-by-pairs! 
   #:hash-set-if-new-by-pairs! 
   #:hash-push-by-pairs! 
   #:hash-pushnew-by-pairs! 

   #:hash-set-by-hash! 
   #:hash-set-if-new-by-hash! 
   #:hash-push-by-hash! 
   #:hash-pushnew-by-hash!

   #:hash-set-by-hashes! 
   #:hash-set-if-new-by-hashes! 
   #:hash-push-by-hashes! 
   #:hash-pushnew-by-hashes!
   
   #:hash-init-by-pairs 
   #:hash-init-by-hashes 
   #:hash-init-pushnew-by-hashes 
 
   ;; hash-deprecated
   #:iter-in-hash
   #:put-to-hash-from-list
   #:hash-table->string
   #:hash-key-values
   #:gethash-or-make
   #:copy-hash-table
   #:make-hash-table-initialized
   #:make-indexed-hash-from-list
   #:random-hash-key
   #:make-hash-from-key-list
   #:make-hash-from-list
   #:make-tuple-list-from-hash


   ;; file
   #:file->lines
   #:file->string))