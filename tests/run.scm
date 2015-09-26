(use cjson test)

(test-group
 "string->json"
 (test "empty object"  '() (string->json "{}"))
 (test "empty vector" '#() (string->json "[]"))

 (test "number" 12.5 (string->json "12.5"))
 (test "false" #f (string->json "false"))
 (test "true" #t (string->json "true"))
 (test "null" (void) (string->json "null"))

 (test "nested"
       '#( ((b . 2.0)
            (a . 1.0))
           ((x . -1.0)))
       (string->json "[{\"a\" : 1 , \"b\" : 2}, {\"x\" : -1 }]")))
