# Core
Scheme's commonly used small functions


### (core alist)

(ref association-list key)

`=> value`

(val association-list value)

`=> key`

(alter association-list key value)

`=> new association-list`

(drop association-list key)

`=> new association-list`

(push association-list key value)

`=> new association-list`

(pop association-list)

`=> procedure call by right pair et new association-list`
       
(insert association-list key value)

`=> new association-list`
       
(eject association-list)

`=> procedure call by leftest pair et new association-list`
        
(alter! association-list key value)

`=> #t for success #f for fails, the old association-list will be modified`
        
(drop! association-list key)

`=> #t for success #f for fails, the old association-list will be modified`
        
(push! association-list key value)

`=> #t for success, the old association-list will be modified`
        
(pop! association-list)

`=> rightest pair, the old association-list will be modified`
        
(insert! association-list key value)

`=> #t for success, the old association-list will be modified`
        
(eject! association-list)

`=> leftest pair, the old association-list will be modified`

(vector->alist vector)	

`#(value1 value2 value3 ...)                    =>     ((0 . value1)(1 . value2)(2 . value3) ...)`	
		
(vector->array association-list)	
		
`((0 . value1)(1 . value2)(2 . value3) ...)     =>     #(value1 value2 value3 ...)`


### (core string)

(split string char)   

`=> list`

### (core exception)

(try procedure (catch procedure))
