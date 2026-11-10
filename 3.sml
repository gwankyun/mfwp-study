val check = Check.check

fun null []     = true
  | null (_::_) = false

fun hd (x::_) = x
  | hd [] = raise Empty

fun fl (_::xs) = xs
  | fl [] = raise Empty

(* 練習3.1 *)

fun maxl ns =
  let val h = hd ns
      val t = tl ns
  in
    if null t then h
    else
      let val t = maxl t in
        if h > t then h
        else t
      end
  end

(* ---- maxl 測試 ---- *)
val () = check "maxl [1] = 1" (maxl [1] = 1)
val () = check "maxl [1,2,3] = 3" (maxl [1,2,3] = 3)
val () = check "maxl [3,2,1] = 3" (maxl [3,2,1] = 3)
val () = check "maxl [2,5,1,4] = 5" (maxl [2,5,1,4] = 5)
val () = check "maxl [7,7,3] = 7" (maxl [7,7,3] = 7)
val () = check "maxl [5,1,5] = 5" (maxl [5,1,5] = 5)
val () = check "maxl [~1,~5,~3] = ~1" (maxl [~1,~5,~3] = ~1)
val () = check "maxl [10,9,8,7,6,5,4,3,2,1] = 10" (maxl [10,9,8,7,6,5,4,3,2,1] = 10)

(* 練習3.2 *)

fun last [x] = x
  | last (_::xs) = last xs
  | last [] = raise Empty

(* ---- last 測試 ---- *)
val () = check "last [1] = 1" (last [1] = 1)
val () = check "last [1,2] = 2" (last [1,2] = 2)
val () = check "last [1,2,3] = 3" (last [1,2,3] = 3)
val () = check "last [1,1,1] = 1" (last [1,1,1] = 1)
val () = check "last [~1,~2] = ~2" (last [~1,~2] = ~2)
val () = check "last [\"a\",\"b\"] = \"b\"" (last ["a","b"] = "b")
val () = check "last [(1,2),(3,4)] = (3,4)" (last [(1,2),(3,4)] = (3,4))
val () = check "last [[1],[2],[3]] = [3]" (last [[1],[2],[3]] = [3])

local
  fun addLen (n, [])   = n
    | addLen (n, _::l) = addLen (n + 1, l)
in
  fun length l = addLen (0, l)
end

fun take ([], _) = []
  | take (x::xs, i) =
    if i > 0 then x::take(xs, i - 1)
    else []

fun drop ([], _) = []
  | drop (x::xs, i) =
    if i > 0 then drop(xs, i - 1)
    else x::xs

(* 練習3.3
take在i > length(l)時返回l，在i < 0時死循環
drop在i > length(l)時返回[]，在i < 0時死循環
*)

(* 練習3.4 *)

fun nth ([], _) = raise Subscript
  | nth (x::xs, n) =
    if n = 0 then x else nth(xs, n - 1)

(* ---- nth 測試 ---- *)
(* 用 handle 包住呼叫，讓失敗顯示 FAIL 而不是中斷整個腳本 *)
val () = check "nth ([10,20,30], 0) = 10"
  ((nth ([10,20,30], 0) = 10) handle Subscript => false | Match => false)
val () = check "nth ([10,20,30], 1) = 20"
  ((nth ([10,20,30], 1) = 20) handle Subscript => false | Match => false)
val () = check "nth ([10,20,30], 2) = 30"
  ((nth ([10,20,30], 2) = 30) handle Subscript => false | Match => false)
val () = check "nth ([42], 0) = 42"
  ((nth ([42], 0) = 42) handle Subscript => false | Match => false)

val () = check "nth ([1,2,3], 3) raises Subscript"
  ((ignore (nth ([1,2,3], 3)); false) handle Subscript => true | Match => false)
val () = check "nth ([], 0) raises Subscript"
  ((ignore (nth ([], 0)); false) handle Subscript => true | Match => false)
val () = check "nth ([1,2], ~1) raises Subscript"
  ((ignore (nth ([1,2], ~1)); false) handle Subscript => true | Match => false)

val () = check "nth ([\"a\",\"b\"], 1) = \"b\""
  ((nth (["a","b"], 1) = "b") handle Subscript => false | Match => false)
val () = check "nth ([(1,2),(3,4)], 0) = (1,2)"
  ((nth ([(1,2),(3,4)], 0) = (1,2)) handle Subscript => false | Match => false)

(* 練習3.5 *)

fun append ([], ys) = ys
  | append (xs, []) = xs
  | append (x::xs, ys) = x :: (append(xs, ys))

(* 練習3.6
類型不對
*)

(* 練習3.7
nrev [1, 2, 3, 4]
=> (nrev 1::[2, 3, 4])
=> (nrev [2, 3, 4]) @ [1]
=> (nrev 2::[3, 4]) @ [1]
=> (nrev [3, 4] @ [2]) @ [1]
=> (nrev 3::[4] @ [2]) @ [1]
=> ((nrev [4] @ [3]) @ [2]) @ [1]
=> ((nrev 4::[] @ [3]) @ [2]) @ [1]
=> (((nrev [] @ [4]) @ [3]) @ [2]) @ [1]
=> ((([] @ [4]) @ [3]) @ [2]) @ [1]
=> (([4] @ [3]) @ [2]) @ [1]
=> ([4, 3] @ [2]) @ [1]
=> [4, 3, 2] @ [1]
=> [4, 3, 2, 1]

rev [1, 2, 3, 4]
=> revAppend ([1, 2, 3, 4], [])
=> revAppend (1::[2, 3, 4], [])
=> revAppend ([2, 3, 4], 1::[])
=> revAppend (2::[3, 4], 1::[])
=> revAppend ([3, 4], 2::1::[])
=> revAppend (3::[4], 2::1::[])
=> revAppend ([4], 3::2::1::[])
=> revAppend (4::[], 3::2::1::[])
=> revAppend ([], 4::3::2::1::[])
=> [4, 3, 2, 1]
*)

(* 練習3.8
和concat作用一樣，有一點優化
f [[1, 2], [3, 4]]
=> f (1::[2])::[[3, 4]]
=> 1 :: f([2]::[[3, 4]])
=> 1 :: f([[2], [3, 4]])
=> 1 :: f( (2::[])::[[3, 4]])
=> 1 :: (2:: f([]::[[3, 4]]))

=> 1 :: (2:: f([[3, 4]]))
=> 1 :: (2:: f((3::[4])::[[]]))
=> 1 :: (2:: 3 ::(f[4]::[[]])))
=> 1 :: (2:: 3 ::(f[[4], []]))
=> 1 :: (2:: 3 ::(f (4::[])::[[]]))
=> 1 :: (2:: 3 ::(f (4::[])::[[]]))
*)

fun f []= []
  | f ([]::ls)                     = f ls
  | f ((x::l)::ls)                 = x :: f(l::ls)

val () = check "" ((f [[1, 2], [3, 4]]) = [1, 2, 3, 4])
val () = check "" ((f [[1, 2], [3, 4]]) = f((1::[2])::[[3, 4]]))
val () = check "" ((f [[1, 2], [3, 4]]) = 1 :: f([2]::[[3, 4]]))
val () = check "" ((f [[1, 2], [3, 4]]) = (1 :: f [[2], [3, 4]]))
(* val () = check "" ((f [[1, 2], [3, 4]]) = (1 :: (f (2::[])::[[3, 4]]))) *)

val () = Check.done ()
