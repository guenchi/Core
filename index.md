

### (core alist)

(**ref** association-list key)

`=> value or #f`

(**val** association-list value)

`=> key or #f`

(**alter** association-list key value)

`=> new association-list which has been modified by key`

(**drop** association-list key)

`=> new association-list with drop the key's pair`

(**push** association-list key value)

`=> new association-list which add a new pair by right`

(**pop** association-list)

`=> procedure call by rightest pair et new association-list which drop the rightest pair`
       
(**insert** association-list key value)

`=> new association-list which add a new pair by left`
       
(**eject** association-list)

`=> procedure call by leftest pair et new association-list which drop the leftest pair`
        
(**alter!** association-list key value)

`=> #t for success #f for fails, the old association-list will be modified by key`
        
(**drop!** association-list key)

`=> #t for success #f for fails, the old association-list will be modified: the key's pair is dropped`
        
(**push!** association-list key value)

`=> #t for success, the old association-list will be modified: add a key value pair by right`
        
(**pop!** association-list)

`=> pair, take the rightest pair, and delete it in the old association-list`
        
(**insert!** association-list key value)

`=> #t for success, the old association-list will be modified: add a key value pair by left`
        
(**eject!** association-list)

`=> pair, take the leftest pair, and delete it in the old association-list`

(**alist->vector** association-list)	
		
`((0 . value1)(1 . value2)(2 . value3) ...)     =>     #(value1 value2 value3 ...)`

(**vector->alist** vector)	

`#(value1 value2 value3 ...)                    =>     ((0 . value1)(1 . value2)(2 . value3) ...)`	


### (core string)

(**split** string char)   

`=> list`

### (core exception)

(**try** procedure (**catch** procedure))

### (core loop)

(**for** variable in collection do-something ...)

(**for/sum** variable in collection do-something ...)

`=> sum of (begin do-something ...)s`

(**for/list** variable in collection do-something ...)

`=> list of (begin do-something ...)s`

(**for/vector** variable in collection do-something ...)

`=> vector of (begin do-something ...)s`

(**for/min** variable in collection do-something ...)

`=> min number of (begin do-something ...)s`

(**for/max** variable in collection do-something ...)

`=> max number of (begin do-something ...)s`

### (core string)

(**split** str char)

`=> list of substrings of str that are separated by char`

(**split*** str char)

`=> list of substrings of str that are separated by char, but ignore emputy string`

(**string-split** str sep)

`=> list of substrings of str that are separated by string sep`

(**string-prefix?** str pref)

`=> if pref is the prefix of str,return true,otherwise false.`

(**string-find** str to-find)

`=> list of pairs (positions of to-find in the string str)`

(**string-replace** str <from:string or listof string> <to:string or listof string>)

`=> string that froms are replaced by to`
