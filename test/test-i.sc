

(import 
    (chezjs compiler))


;; test split

(chezjs "var i = 89; var j = 100; function f(x, y){ x + y;} f(i, j);")

