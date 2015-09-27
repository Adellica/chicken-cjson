chicken-cjson
===============

 [Chicken Scheme]: http://call-cc.org/
 [cjson]: https://github.com/kbranigan/cJSON
 [medea]: http://wiki.call-cc.org/eggref/4/medea
 [json]: http://wiki.call-cc.org/eggref/4/json

[Chicken Scheme] bindings for the JSON parser [cjson]. chicken-cjson
offers a significant performance improvement over [medea] and [json],
but comes with a major drawback: all data must be availabe as a
string. This means you cannot parse JSON coming from a port unless you
know how much data to hand over to chicken-cjson.

chicken-cjson offers an alternative API which exposes a JSON c-struct
as a `#<cjson>` scheme record. This is for performance reasons and
allows you to pick apart JSON objects using lolevel C-functions,
without transitioning into scheme objects. This is a little faster but
is a lot more verbose.

The `#<cjson>` record-printer will output the object's JSON
representaion for easier debugging:

```bash
$ csi -R cjson -p '(string->cjson "[1,2,3]")'
#<cjson [1,2,3]>
```

## Requirements

None. [cjson] comes bundled.

## API

    [procedure] (string->cjson string)

Parse the string and return a `#<cjson>` record which holds a c-struct
representing the JSON. The returned record has a finalizer attached to
it so the underlying c-struct gets freed on garbage collection.

    [procedure] (string->cjson* string)

Like `string->cjson` but does not attach a finalizer to the `#<cjson>`
record. `cjson-free` must be explicitly called later on the returned
value to avoid memory leaks. This is sometimes faster than attaching
finalizers, particularly if there are large numbers of `#<cjson>`
objects.

    [procedure] (cjson->string cjson [pretty-print?])

Convert the `#<cjson>` object to its JSON-representation, returned as
a string. `pretty-print?` defaults to true.

    [procedure] (cjson-schemify cjson)

Convert the `#<cjson>` object to scheme data-structures. The
data-structures are the same as [medea]'s, where `array => vector` and
`object => alist`.

    [procedure] (string->json string)

Make scheme data-structures of the json data in `string` using CJSON:

```scheme
(define (string->json str)
  (let* ((j (string->cjson* str))
         (s (cjson-schemify j)))
    (cjson-free j)
    s))
```

For string inputs, this should be API-equivalent of [medea]'s
`(read-json)`.

    [procedure] (cjson-type cjson)

Pick out the type of a CJSON record. Returns a fixnum.

    [variable] cjson/false
    [variable] cjson/true
    [variable] cjson/null
    [variable] cjson/number
    [variable] cjson/string
    [variable] cjson/array
    [variable] cjson/object

Exposes the cjson-type constants.

    [procedure] (cjson-double cjson)
    [procedure] (cjson-string cjson)
    [procedure] (cjson-key cjson)

"Unbox" the json value. `cjson-type` must match like this:

```scheme
(select (cjson-type cjson)
   ((cjson/false) #f)
   ((cjson/true)  #t)
   ((cjson/null) (void))
   ((cjson/number) (cjson-double cjson))
   ((csjon/string) (cjson-string cjson))
   (else (error "probably a vector or object")))
```

    [procedure] (cjson-array-size cjson)

Return the number of elements in the array. If `cjson`'s type is not
an array, this is undefined bahaviour.

    [procedure] (cjson-array-ref cjson index)

Return the element of `cjson` at position `index`. `index` must be a
fixnum. Undefined behaviour if `cjson` is not an array.

    [procedure] (cjson-obj-ref cjson key)

Select field `key` from `cjson`. Key must be a string. Undefined
behaviour if `cjson` is not of type `cjosn/object`.


## Performance

The performance characteristics of JSON parsing is mysterious. It is
recommended to use [medea] or [json] for normal usage because they
integrate better with the scheme-world. For particular cases, however,
there may be significant performace improvements in using cjson.

Sometimes medea is faster than cjson:

```bash
$ (echo '[' ; for i in {0..1000} ; do echo '"str", 1, 2, 3, 4,' ; done ; echo ' 0]') > bigjson
$ time csi -R medea -e '(pp (read-json))' < bigjson >/dev/null

real    0m0.152s
user    0m0.140s
sys     0m0.013s
$ time csi -R cjson -e '(pp (string->json (read-string)))' < bigjson >/dev/null

real    0m0.114s
user    0m0.103s
sys     0m0.010s
```

The slowdown could be because of `read-string`'s memory allocations. cjson may shine when you already have the JSON data in memory:

```bash
$ for i in {0..100000} ; do echo '{"field" : {"id" : 1}}' ; done > jsonlines
$ time csi -R medea -R ports -e '(port-for-each (lambda (line) (pp (alist-ref `field (read-json line)))) read-line)' < jsonlines  > /dev/null

real    0m3.997s
user    0m3.933s
sys     0m0.067s
$ time csi -R cjson -R ports -e '(port-for-each (lambda (line) (pp (alist-ref `field (string->json line)))) read-line)' < jsonlines  > /dev/null

real    0m1.099s
user    0m1.083s
sys     0m0.017s
```

That's four times faster. Probably doesn't justify sacrificing a
convenient API. In some cases, though, parsing JSON in C and using
it's combersome API can pay off:

```bash
$ JSON='{"field" : {"id" : "ID"} , "a":1, "b":2, "c":[1,{"x":{"y":"y"}},3],"d":{"e":[]}}'
$ for i in {0..100000} ; do echo $JSON ; done > jsonlines
$ for f in test*.scm ; do echo ===== $f === ; cat $f ; done
===== test-cjson.scm ===
(use cjson ports)

(port-for-each
 (lambda (line)
   (let ((cjson (string->cjson* line)))
     (print (cjson-string (cjson-obj-ref (cjson-obj-ref cjson "field") "id")))
     (cjson-free cjson)))
 read-line)

===== test-medea.scm ===
(use medea ports)

(port-for-each
 (lambda (line)
   (print (alist-ref 'id (alist-ref 'field (read-json line)))))
   read-line)

$ for f in test*.scm ; do echo $f ; time csi -s $f < jsonlines >/dev/null ; done
test-cjson.scm

real    0m0.541s
user    0m0.523s
sys     0m0.020s
test-medea.scm

real    0m13.870s
user    0m13.717s
sys     0m0.153s
$ time jq '.field.id' < jsonlines  > /dev/null

real    0m0.513s
user    0m0.500s
sys     0m0.017s
```

In this particular run, CJSON is 25 times faster than medea and
performas as well as `jq`. This would be in situations where you are
parsing a lot of JSON, but only a small part of that needs to go back
into scheme.

Note that this example works because we have one JSON object per line,
effectively giving us a json-object delimitar.
