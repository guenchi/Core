# Core
Scheme's commonly used small functions


### (core alist)

(**ref** association-list key)

`=> value`

(**val** association-list value)

`=> key`

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

(**vector->alist** vector)	

`#(value1 value2 value3 ...)                    =>     ((0 . value1)(1 . value2)(2 . value3) ...)`	
		
(**alist->vector** association-list)	
		
`((0 . value1)(1 . value2)(2 . value3) ...)     =>     #(value1 value2 value3 ...)`


### (core string)

(**split** string char)   

`=> list`

### (core exception)

(**try** procedure (**catch** procedure))
