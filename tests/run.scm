(use cjson test)

(test-group
 "string->json"
 (test "empty object"  '() (string->json "{}"))
 (test "empty vector" '#() (string->json "[]"))

 (test "number" 12.5 (string->json "12.5")))
