chicken-cjson
===============

 [Chicken Scheme]: http://call-cc.org/
 [cjson]: https://github.com/DaveGamble/cJSON
 [medea]: http://wiki.call-cc.org/eggref/4/medea
 [json]: http://wiki.call-cc.org/eggref/4/json
 [jq]: https://stedolan.github.io/jq/

[Chicken Scheme] bindings for the JSON parser [cjson]. It cannot read
from ports and must have the entire JSON object in
memory. `chicken-cjson` only parses JSON, does not serialize.

The `string->json` procedure returns the same datastrucures as
`medea`'s, `read-json`, so if you're passing that strings already,
`string->json` should be a drop-in replacement:

```bash
$ printf '{"array":[1,2,3],"null":null}' | csi -R cjson -p '(string->json (read-line))'
((null . null) (array . #(1.0 2.0 3.0)))
$ printf '{"array":[1,2,3],"null":null}' | csi -R medea -p '(read-json (read-line))'
((array . #(1 2 3)) (null . null))
```

`chicken-cjson` may offer a significant performance improvement over
[medea] and [json], but comes at a price: all data must be availabe as
a string. This means you cannot parse JSON coming in from a port
directly, and that's why there is no `read-json` here.

`chicken-cjson` offers an alternative API which exposes a JSON c-struct
as a `#<cjson>` scheme record, and accompanying procedures like
`cjson-string`. This is for performance reasons and allows you to pick
apart JSON objects using lolevel C-functions, without transitioning
into the Scheme data-structure. This may be faster but is a lot uglier:

```bash
$ printf '{"array":[1,2,3],"null":null}' |\
  csi -R cjson -p '(cjson-double (cjson-array-ref (cjson-obj-ref (string->cjson (read-line)) "array") 1))'
2.0
```

Note that if you know that a number is fixnum, you can use `cjson-int`
instead.

## Requirements

None. [cjson] comes bundled.

## TODO

- Make `cjson-schemify` faster yet by avoiding the O(n) procedures
  `cjson-obj-ref` and `cjson-array-ref`
- Update to [cjson] version [1.2.1](https://github.com/DaveGamble/cJSON/releases/tag/v1.2.1)
- Add support for modifying `cjson` records like `cJSON_AddItemToArray`
- Make `with-cjson` that allocates and deallocates in block, and does
  it fast?
- Make a common JSON API for all json parsers in CHICKEN?

## API

    [procedure] (string->cjson string)

Note that that's not `string->json`! Parses the string and returns a
`#<cjson>` record which holds a c-struct representing the JSON. The
returned record has a finalizer attached to it so the underlying
c-struct gets freed on garbage collection.

    [procedure] (string->cjson* string)

Like `string->cjson` but does not attach a finalizer to the `#<cjson>`
record. `cjson-free` must be explicitly called later on the returned
value to avoid memory leaks. This is sometimes faster than attaching
finalizers, particularly if there are large numbers of `#<cjson>`
objects.

    [procedure] (cjson->string cjson [pretty-print?])

Convert the `#<cjson>` object to its JSON-representation, returned as
a string. `pretty-print?` defaults to true. Note that this can only
serialize [cjson] records, and not scheme objects.

    [procedure] (cjson-schemify cjson)

Convert the `#<cjson>` object to scheme data-structures. The
data-structures are the same as [medea]'s, where `array => vector` and
`object => alist`.

    [procedure] (string->json string)

Make scheme data-structures of the json data in `string` using [cjson]:

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

Pick out the type of a [cjson] record. Returns a fixnum.

    [variable] cjson/false
    [variable] cjson/true
    [variable] cjson/null
    [variable] cjson/number
    [variable] cjson/string
    [variable] cjson/array
    [variable] cjson/object

Exposes the [cjson]-type constants.

    [procedure] (cjson-int cjson)
    [procedure] (cjson-double cjson)
    [procedure] (cjson-string cjson)
    [procedure] (cjson-key cjson)

"Unbox" the json value. `cjson-type` must match like this:

```scheme
(select (cjson-type cjson)
   ((cjson/false) #f)
   ((cjson/true)  #t)
   ((cjson/null) 'null)
   ((cjson/number) (cjson-double cjson))
   ((csjon/string) (cjson-string cjson))
   (else (error "probably a vector or object")))
```

    [procedure] (cjson-array-size cjson)

Return the number of elements in the array. If [cjson]'s type is not
an array, this is undefined bahaviour.

    [procedure] (cjson-array-ref cjson index)

Return the element of `cjson` at position `index`. `index` must be a
fixnum. Undefined behaviour if `cjson` is not an array.

    [procedure] (cjson-obj-ref cjson key)

Select field `key` from `cjson`. Key must be a string. Undefined
behaviour if `cjson` is not of type `cjosn/object`.


## Performance

The performance characteristics of JSON parsing is mysterious. It is
recommended to use [medea] or [json] in most usage-cases because they
can parse directly off ports and they also serialize. For particular
cases, however, there may be significant performace improvements in
using [cjson].

Sometimes the speedup is negligible:

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

Here, having to doing a `read-string` first isn't great. [cjson] may
shine when you already have the JSON data as a string:

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

That's four times faster. In some cases, though, keeping the
`#<cjson>` record and using its combersome API can pay off:

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

In this particular run, `chicken-cjson` is 25 times faster than medea,
and performas about as well as [jq]. This speedup typically only
happens where you are parsing a lot of JSON, but only a small part of
that needs to go back into scheme. Also, this only works because we
have one JSON object per line, effectively giving us a json-object
delimiter.

