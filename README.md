# imperative
sometimes you just want a for loop. This DSL makes the syntax to a bit more friendly to array access, mutation, and generally programming more similarly to C.

## Usage
New constructs in this DSL
:= - is for initialization\
= - is for mutation\
:: - is for array indexing (having x[y][z] syntax caused to many ambiguities without adding more annoying constructs like line ends)\
break! - acts like a traditional break statement\
return - works as expected\
if, elif, and else - work as expected in other languages and are not nested within each other\
for loops\
while loops\


## Examples

Here is a general overview with a quick sort example. il starts the DSL 
```
(define partition
      (lambda (arr low high)
        (il
          pivot := arr :: high
          i := (- low 1)
          (for (j := low) (<= j (- high 1)) (+ j 1)
               arrj := arr :: j
               (if (< arrj pivot)
                   i = (+ 1 i)
                   temp := arr :: i
                   arr :: i = arr :: j
                   arr :: j = temp))
          temp := arr :: (+ 1 i)
          arr :: (+ 1 i) = arr :: high
          arr :: high = temp
          (+ i 1))))

(define qsort
    (lambda (arr low high)
      (il
        (if (< low high)
            pi := (partition arr low high)
            (qsort arr low (- pi 1))
            (qsort arr (+ pi 1) high)))))
```

notice how the dsl ends when you need to start calling functions again, sometimes you still want the array indexing syntax for convienience in which case you can call vi with which we can turn something like
```
arrj := arr :: j
(if (< arrj pivot) ...)
```
into 
```
(if (< (vi arr :: j) pivot) ...)
```

also a more involved example of if statements
```
(il
    (for (i := 0) (< i 10) (fx+ i 1)
        (if (< i 2)
            (display "lt2")
            (newline))
        (elif (< i 8)
            (display "lt8")
            (newline)
            break!
            (display "will never display"))
        (else
            (display "will never display"))))
```


