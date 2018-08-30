# core
Scheme's tiny function


***(core alist)***

(ref association-list key)
`=> value`

(val association-list value)
`=> key`

(vector->alist vector)	

`#(value1 value2 value3 ...)                    =>     ((0 . value1)(1 . value2)(2 . value3) ...)`	
		
(vector->array association-list)	
		
`((0 . value1)(1 . value2)(2 . value3) ...)     =>     #(value1 value2 value3 ...)`


***(core string)***

(split string char)   
`=> list`

***(core exception)***

(try procedure (catch procedure))
