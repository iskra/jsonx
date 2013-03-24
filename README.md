
JSONX is an Erlang library for efficient decode and encode JSON, written in C.
Works with binaries as strings, arrays as lists and it only knows how to decode UTF-8 (and ASCII).

Decode (json -> erlang)
=======================

 - null   -> atom null
 - true   -> atom true
 - false  -> atom false
 - string -> binary
 - number -> number
 - array  -> list
 - object -> {struct, PropList}, optional eep18 or proplist.

Encode (erlang -> json)
=======================

 - atom null 	      -> null
 - atom true 	      -> true
 - atom true 	      -> false
 - any other atom     -> string
 - binary             -> string
 - number             -> number
 - {struct, PropList} -> object
 - {PropList}         -> object
 - PropList           -> object
 - {json, IOList}     -> include IOList with no validation
