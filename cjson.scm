(module cjson *

(import chicken scheme foreign)
(use srfi-1)

(foreign-declare "#include \"cJSON.c\"")

(define-record-type cjson (%make-cjson pointer)
  %cjson?
  (pointer %cjson-pointer))

(define-foreign-type cjson (c-pointer (struct "cJSON"))
  %cjson-pointer
  (lambda (x) (and x (%make-cjson x))))

;; memory-leaking version. must call free on returned object sometime.
(define str->cjson*
  (foreign-lambda* cjson ((nonnull-c-string json_str))
                   "return(cJSON_Parse(json_str));"))

(define (cjson-assert cjson)
  (if (%cjson-pointer cjson) cjson))

(define cjson-free (foreign-lambda* void ((cjson x)) "cJSON_Delete(x);"))

;; finelizers don't always work out too well if there are many of them
;; (according to docs). how many are too many?
(define (str->cjson str)
  (set-finalizer! (let ((cjson (str->cjson* str)))
                    (if cjson cjson
                        (error "unparseable json" str)))
                  cjson-free))

(define (cjson->str cjson #!optional (pp #t))
  ((if pp
       (foreign-lambda* c-string* ((cjson json)) "return (cJSON_Print(json));")
       (foreign-lambda* c-string* ((cjson json)) "return (cJSON_PrintUnformatted(json));"))
   cjson))


(define cJSON_False  0)
(define cJSON_True   1)
(define cJSON_NULL   2)
(define cJSON_Number 3)
(define cJSON_String 4)
(define cJSON_Array  5)
(define cJSON_Object 6)

;; no error checking!
(define cjson-type   (foreign-lambda* int      ((cjson x)) "return(x->type);"))
(define cjson-int    (foreign-lambda* int      ((cjson x)) "return(x->valueint);"))
(define cjson-double (foreign-lambda* int      ((cjson x)) "return(x->valuedouble);"))
(define cjson-string (foreign-lambda* c-string ((cjson x)) "return(x->valuestring);"))
(define cjson-key    (foreign-lambda* c-string ((cjson x)) "return(x->string);"))

(define cjson-array-size (foreign-lambda int cJSON_GetArraySize cjson))
(define cjson-array-ref  (foreign-lambda cjson cJSON_GetArrayItem cjson int))
(define cjson-obj-ref    (foreign-lambda cjson cJSON_GetObjectItem cjson nonnull-c-string))

(define cjson-child   (foreign-lambda* cjson ((cjson x)) "return(x->child);" ))
(define cjson-next    (foreign-lambda* cjson ((cjson x)) "return(x->next);" ))

;;cJSON *c=object->child; while (c && cJSON_strcasecmp(c->string,string)) c=c->next; return c;
(define (cjson-obj-keys cjson)
  (let loop ((c (cjson-child cjson))
             (res '()))
    (if (and c (%cjson-pointer c))
        (loop (cjson-next c) (cons (cjson-key c) res))
        res)))

;; number => number, string => string
;; nil => (void), true/false => #t/#f
;; array => vector
;; obj => alist
(define (cjson-schemify cjson)
  (case (cjson-type cjson)
    ((0) #f)
    ((1) #t)
    ((2) (void))               ;; null
    ((3) (cjson-double cjson)) ;; cJSON_Number
    ((4) (cjson-string cjson))
    ((5) (list->vector
          (map cjson-schemify (list-tabulate (cjson-array-size cjson)
                                             (lambda (i) (cjson-array-ref cjson i))))))
    ((6) (map (lambda (key)
                (cons (string->symbol key)
                      (cjson-schemify (cjson-obj-ref cjson key))))
              (cjson-obj-keys cjson)))
    (else (error "unknown cjson type" (cjson-type cjson)))))

;; finalizers are expensive. this version avoids their use:
(define (str->json str)
  (let* ((j (str->cjson* str))
         (s (cjson-schemify j)))
    (cjson-free j)
    s))

)
