let fib (n) =
    let rec loop n prev1 prev2 =
        match n with
        | 1 -> 0
        | 2 -> prev1
        | _ -> loop (n - 1) (prev1 + prev2) prev1

    loop n 1 0

let testFib =
    assert (0 = (fib 1))
    assert (1 = (fib 2))
    assert (1 = (fib 3))
    assert (2 = (fib 4))
    assert (3 = (fib 5))
    assert (34 = (fib 10))

let isSorted (arr: 'a list) f =
    let rec loop n =
        if n + 1 >= arr.Length then
            true
        elif f arr.[n] arr.[n + 1] then
            loop (n + 1)
        else
            false

    loop 0

let testIsSorted =
    assert not (isSorted [ 1; 2; 5; 4 ] (<))
    assert isSorted [ 'a'; 'b'; 'c' ] (<)
    assert not (isSorted [ "dog"; "cat"; "mouse" ] (<))

let curry f = fun a b -> f (a, b)

let uncurry f = fun (a, b) -> f a b

let testCurry =
    let add (x, y) = x + y
    let curriedAdd = curry add
    assert (curriedAdd 1 2 = add (1, 2))
    assert (uncurry curriedAdd (1, 2) = add (1, 2))

let compose ((f: 'b -> 'c), g) = fun a -> f (g a)

let testCompose =
    let f x = x * 2
    let g x = x + 2
    assert (compose (f, g) 2 = (g >> f) 2)

testFib
testIsSorted
testCurry
testCompose
