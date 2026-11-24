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
fun testMaxl () = (
  check "maxl [1] = 1" (maxl [1] = 1);
  check "maxl [1,2,3] = 3" (maxl [1,2,3] = 3);
  check "maxl [3,2,1] = 3" (maxl [3,2,1] = 3);
  check "maxl [2,5,1,4] = 5" (maxl [2,5,1,4] = 5);
  check "maxl [7,7,3] = 7" (maxl [7,7,3] = 7);
  check "maxl [5,1,5] = 5" (maxl [5,1,5] = 5);
  check "maxl [~1,~5,~3] = ~1" (maxl [~1,~5,~3] = ~1);
  check "maxl [10,9,8,7,6,5,4,3,2,1] = 10" (maxl [10,9,8,7,6,5,4,3,2,1] = 10);
  ()
)
val () = testMaxl ()

(* 練習3.2 *)

fun last [x] = x
  | last (_::xs) = last xs
  | last [] = raise Empty

(* ---- last 測試 ---- *)
fun testLast () = (
  check "last [1] = 1" (last [1] = 1);
  check "last [1,2] = 2" (last [1,2] = 2);
  check "last [1,2,3] = 3" (last [1,2,3] = 3);
  check "last [1,1,1] = 1" (last [1,1,1] = 1);
  check "last [~1,~2] = ~2" (last [~1,~2] = ~2);
  check "last [\"a\",\"b\"] = \"b\"" (last ["a","b"] = "b");
  check "last [(1,2),(3,4)] = (3,4)" (last [(1,2),(3,4)] = (3,4));
  check "last [[1],[2],[3]] = [3]" (last [[1],[2],[3]] = [3]);
  ()
)
val () = testLast ()

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
fun testNth () = (
  check "nth ([10,20,30], 0) = 10"
    ((nth ([10,20,30], 0) = 10) handle Subscript => false | Match => false);
  check "nth ([10,20,30], 1) = 20"
    ((nth ([10,20,30], 1) = 20) handle Subscript => false | Match => false);
  check "nth ([10,20,30], 2) = 30"
    ((nth ([10,20,30], 2) = 30) handle Subscript => false | Match => false);
  check "nth ([42], 0) = 42"
    ((nth ([42], 0) = 42) handle Subscript => false | Match => false);

  check "nth ([1,2,3], 3) raises Subscript"
    ((ignore (nth ([1,2,3], 3)); false) handle Subscript => true | Match => false);
  check "nth ([], 0) raises Subscript"
    ((ignore (nth ([], 0)); false) handle Subscript => true | Match => false);
  check "nth ([1,2], ~1) raises Subscript"
    ((ignore (nth ([1,2], ~1)); false) handle Subscript => true | Match => false);

  check "nth ([\"a\",\"b\"], 1) = \"b\""
    ((nth (["a","b"], 1) = "b") handle Subscript => false | Match => false);
  check "nth ([(1,2),(3,4)], 0) = (1,2)"
    ((nth ([(1,2),(3,4)], 0) = (1,2)) handle Subscript => false | Match => false);
  ()
)
val () = testNth ()

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

fun testF () = (
  check "" ((f [[1, 2], [3, 4]]) = [1, 2, 3, 4]);
  check "" ((f [[1, 2], [3, 4]]) = f((1::[2])::[[3, 4]]));
  check "" ((f [[1, 2], [3, 4]]) = 1 :: f([2]::[[3, 4]]));
  check "" ((f [[1, 2], [3, 4]]) = (1 :: f [[2], [3, 4]]));
  ()
)
val () = testF ()
(* val () = check "" ((f [[1, 2], [3, 4]]) = (1 :: f ((2::[])::[[3, 4]]))) *)
(* val () = check "" ((f [[1, 2], [3, 4]]) = (1 :: (f (2::[])::[[3, 4]]))) *)

(* 練習3.9 *)

fun zip(x::xs, y::ys) = (x, y)::zip(xs, ys)
  | zip([], _::_) = []
  | zip(_::_, []) = []
  | zip([], []) = []

(* 練習3.10
和i的位置有關
*)

(* 3.7 找零錢 *)

fun change (_, 0) = []
  | change ([], _) = []
  | change (c::coinvals, amount) = (* c是硬幣值，amount是當前錢數 *)
    if amount < c then change(coinvals, amount) (* c太大了，不用 *)
      else c :: change(c::coinvals, amount - c) (* 用一個c *)

fun allChange (coins, _, 0) = [coins]
  | allChange (_, [], _) = []
  | allChange (coins, c::coinvals, amount) =
    (* coins 已取硬幣列表
       c::coinvals 可取硬幣列表
       amount 賬戶
    *)
    if amount < 0 then []
      else allChange(c::coins, c::coinvals, amount - c) @ (* 用一個c *)
        allChange(coins, coinvals, amount) (* 其他組合 *)

(* 練習3.11 *)

fun groupCons value [] = [(value, 1)]
  | groupCons value ((y, n)::xs) =
    if value = y then (value, n + 1)::xs
    else (value, 1) :: (y, n) :: xs

fun group [] = []
  | group (x::xs) = groupCons x (group xs)

fun repeatString s n =
    if n <= 0 then ""
    else String.concat (List.tabulate (n, fn _ => s))

fun repeatChar c n =
  repeatString (String.str c) n

local fun tran n v1 v5 v10 =
  let fun help 9 = v1 ^ v10
    | help 5 = v5
    | help 4 = v1 ^ v5
    | help m =
      if m > 9 then ""
      else if m > 5 andalso m < 9 then v5 ^ repeatString v1 (m - 5)
      else repeatString v1 m
  in
    help n
  end
in fun toRoman 1000 n =
    if n > 3 then ""
    else repeatString "M" n
  | toRoman 100 n =
    tran n "C" "D" "M"
  | toRoman 10 n =
    tran n "X" "L" "C"
  | toRoman 1 n =
    tran n "I" "V" "X"
  | toRoman _ _ = ""
end

(* ---- toRoman 測試 ---- *)
fun testToRoman () = (
  check "toRoman 1000 0 = empty" (toRoman 1000 0 = "");
  check "toRoman 1000 1 = M" (toRoman 1000 1 = "M");
  check "toRoman 1000 3 = MMM" (toRoman 1000 3 = "MMM");
  check "toRoman 1000 4 = empty" (toRoman 1000 4 = "");
  check "toRoman 100 1 = C" (toRoman 100 1 = "C");
  check "toRoman 100 4 = CD" (toRoman 100 4 = "CD");
  check "toRoman 100 5 = D" (toRoman 100 5 = "D");
  check "toRoman 100 8 = DCCC" (toRoman 100 8 = "DCCC");
  check "toRoman 100 9 = CM" (toRoman 100 9 = "CM");
  check "toRoman 10 4 = XL" (toRoman 10 4 = "XL");
  check "toRoman 10 5 = L" (toRoman 10 5 = "L");
  check "toRoman 10 8 = LXXX" (toRoman 10 8 = "LXXX");
  check "toRoman 10 9 = XC" (toRoman 10 9 = "XC");
  check "toRoman 1 0 = empty" (toRoman 1 0 = "");
  check "toRoman 1 1 = I" (toRoman 1 1 = "I");
  check "toRoman 1 3 = III" (toRoman 1 3 = "III");
  check "toRoman 1 4 = IV" (toRoman 1 4 = "IV");
  check "toRoman 1 5 = V" (toRoman 1 5 = "V");
  check "toRoman 1 6 = VI" (toRoman 1 6 = "VI");
  check "toRoman 1 8 = VIII" (toRoman 1 8 = "VIII");
  check "toRoman 1 9 = IX" (toRoman 1 9 = "IX");
  check "toRoman 5 1 = empty (invalid place)" (toRoman 5 1 = "")
)

val () = testToRoman ()

fun numberToRoman amount =
  let
    val result =
      List.map (fn (value, count) => toRoman value count)
        (group (change ([1000, 100, 10, 1], amount)))
  in
    String.concat result
  end

fun testNumberToRoman () = (
  check "4 = IV" (numberToRoman 4 = "IV");
  check "9 = IX" (numberToRoman 9 = "IX");
  check "40 = XL" (numberToRoman 40 = "XL");
  check "1994 = MCMXCIV" (numberToRoman 1994 = "MCMXCIV");
  check "2025 = MMXXV" (numberToRoman 2025 = "MMXXV");
  check "3999 = MMMCMXCIX" (numberToRoman 3999 = "MMMCMXCIX");
  check "0 = empty" (numberToRoman 0 = "")
)

val () = testNumberToRoman ()

(* 練習3.12
會跳過大額面值
*)

(* 練習3.13 *)

fun allChangePurse (coins, _, 0) = [coins]
  | allChangePurse (_, [], _) = []
  | allChangePurse (coins, (c, n)::coinvals, amount) =
    (* coins 已取硬幣列表
       (c, n)::coinvals 可取硬幣對列表，c為幣值，n為數量
       amount 賬戶
    *)
    if amount < 0 then []
    else if n > 0 then allChangePurse(c::coins, (c, n - 1)::coinvals, amount - c) @ (* 用一個c *)
      allChangePurse(coins, coinvals, amount) (* 其他組合 *)
    else allChangePurse(coins, coinvals, amount) (* 其他組合 *)

(* ---- allChangePurse 測試 ---- *)
fun testAllChangePurse () = (
  check "amount 0 = [[]]" (allChangePurse ([], [(1,1)], 0) = [[]]);
  check "empty purse, amount 1 = []" (List.null (allChangePurse ([], [], 1)));
  check "not enough coins (1x2, amount 3) = []"
        (List.null (allChangePurse ([], [(1,2)], 3)));
  check "exact two 2s = [[2,2]]"
        (allChangePurse ([], [(2,2)], 4) = [[2,2]]);
  check "negative amount = []"
        (List.null (allChangePurse ([], [(1,1)], ~1)));

  let val r = allChangePurse ([], [(1,3),(2,1)], 3) in
    check "3 with (1x3,2x1) has 2 ways" (List.length r = 2);
    check "3 includes 1+1+1" (List.exists (fn xs => xs = [1,1,1]) r);
    check "3 includes 2+1" (List.exists (fn xs => xs = [2,1]) r)
  end;

  let val r = allChangePurse ([], [(1,5),(5,1)], 5) in
    check "5 with (1x5,5x1) has 2 ways" (List.length r = 2);
    check "5 includes 1+1+1+1+1" (List.exists (fn xs => xs = [1,1,1,1,1]) r);
    check "5 includes 5" (List.exists (fn xs => xs = [5]) r)
  end;

  let val r = allChangePurse ([], [(1,3),(2,2)], 4) in
    check "4 with (1x3,2x2) has 2 ways" (List.length r = 2);
    check "4 includes 2+2" (List.exists (fn xs => xs = [2,2]) r);
    check "4 includes 2+1+1" (List.exists (fn xs => xs = [2,1,1]) r)
  end;

  ()
)

val () = testAllChangePurse ()

(* 練習3.14 *)

fun allChangeAcc (coins, _, 0, acc) = coins :: acc
  | allChangeAcc (_, [], _, acc) = acc
  | allChangeAcc (coins, c::coinvals, amount, acc) =
    (* coins 已取硬幣列表
       c::coinvals 可取硬幣列表
       amount 賬戶
    *)
    if amount < 0 then acc
      else allChangeAcc(coins, coinvals, amount, allChangeAcc(c::coins, c::coinvals, amount - c, acc))

(* ---- allChangeAcc 測試 ---- *)
fun testAllChangeAcc () = (
  check "amount 0 = [[]]" (allChangeAcc ([], [1], 0, []) = [[]]);
  check "empty coinvals = []" (List.null (allChangeAcc ([], [], 5, [])));
  check "no solution (2, amount 3) = []"
        (List.null (allChangeAcc ([], [2], 3, [])));
  check "only 1s: [1],3 = [[1,1,1]]"
        (allChangeAcc ([], [1], 3, []) = [[1,1,1]]);

  check "1,2 amount 3 = [[2,1],[1,1,1]]"
        (allChangeAcc ([], [1,2], 3, []) = [[2,1],[1,1,1]]);
  check "rev matches naive allChange (1,2; 3)"
        (List.rev (allChangeAcc ([], [1,2], 3, [])) = allChange ([], [1,2], 3));
  check "1,2,5 amount 5 has 4 solutions"
        (List.length (allChangeAcc ([], [1,2,5], 5, [])) = 4);
  check "rev matches naive allChange (1,2,5; 5)"
        (List.rev (allChangeAcc ([], [1,2,5], 5, [])) = allChange ([], [1,2,5], 5));

  check "negative amount keeps acc"
        (allChangeAcc ([], [1], ~1, [[9]]) = [[9]]);
  check "amount 0 conses onto acc"
        (allChangeAcc ([], [1], 0, [[9]]) = [[],[9]]);
  check "chosen coins kept"
        (allChangeAcc ([1,2], [1], 0, []) = [[1,2]]);
  check "initial acc preserved"
        (allChangeAcc ([], [1], 1, [[9]]) = [[1],[9]]);
  ()
)

val () = testAllChangeAcc ()

(* 3.8 二進制算術 *)

fun bincarry (0, ps) = ps
  | bincarry (1, []) = [1]
  (* | bincarry (1, p::ps) = (1 - p) :: bincarry(p, ps) *)
  | bincarry (1, p::ps) =
    let
      val s = p + 1
    in
      (s mod 2) :: bincarry (s div 2, ps) (* 向右進位 *)
    end
  | bincarry _ = raise (Fail "bincarry bad arg")

fun binsum (c, [], qs) = bincarry (c, qs)
  | binsum (c, ps, []) = bincarry (c, ps)
  | binsum (c, p::ps, q::qs) =
    let
      val s = c + p + q
    in
      (s mod 2) :: binsum(s div 2, ps, qs)
    end

fun binprod ([], _) = []
  | binprod (0::ps, qs) = 0::binprod(ps, qs) (* 直接後面加0 *)
  | binprod (1::ps, qs) = binsum(0, qs, 0::binprod(ps, qs)) (* 移位加 *)
  | binprod _ = raise (Fail "binprod bad arg")

structure Bin = struct
  fun carry (q, p) = bincarry(q, p)
  fun sum (c, q, p) = binsum(c, q, p)
  fun prod (q, p) = binprod(q, p)
end

(* 先跳過 *)

(* 3.9 矩陣的轉置 *)

val matrix =
  [ ["a", "b", "c"],
    ["d", "e", "f"] ]

fun headcol [] = []
  | headcol ((x::_) :: rows) = x :: headcol rows
  | headcol _ = raise (Fail "invalid argument")

fun tailcol [] = []
  | tailcol ((_::xs) :: rows) = xs :: tailcol rows
  | tailcol _ = raise (Fail "invalid argument")

fun transp ([]::_) = []
  | transp rows = headcol rows :: transp (tailcol rows)

(* 練習 3.19
有空位處理不了
*)

(* 練習 3.20
報異常
*)

(* 先跳過 *)

(* 3.10 矩陣乘法 *)

(* 先跳過 *)

(* 3.11 高斯消元法 *)

(* 先跳過 *)

(* 3.12 分解一個數為兩個平方數之和 *)

(* 3.13 求後繼排列的問題 *)

(* 先跳過 *)

(* 3.14 相等類型 *)

infix mem

fun (_ mem []) = false
  | (x mem (y::l)) = (x = y) orelse (x mem l)

(* 3.15 多態集合操作 *)

fun newmem (x, xs) =
  if x mem xs then xs else x::xs

fun setof [] = []
  | setof (x::xs) = newmem(x, setof xs)

fun union ([], ys) = ys
  | union (x::xs, ys) = newmem(x, union(xs, ys))

fun inter ([], _) = []
  | inter (x::xs, ys) =
    if x mem ys then x::inter(xs, ys)
    else inter(xs, ys)

infix subs

fun ([] subs _) = true
  | ((x::xs) subs ys) = (x mem ys) andalso (xs subs ys)

infix seq

fun (xs seq ys) = (xs subs ys) andalso (ys subs xs)

fun powset ([], base) = [base]
  | powset (x::xs, base) =
      powset(xs, base) @ powset(xs, x::base)

fun cartprod ([], _) = []
  | cartprod (x::xs, ys) =
      let val xsprod = cartprod(xs, ys)
          fun pairx [] = xsprod
            | pairx (y::ytail) = (x, y) :: (pairx ytail)
      in pairx ys end

(* 3.16 關聯表 *)

fun assoc ([], _) = []
  | assoc ((x, y)::pairs, a) =
      if a = x then [y]
      else assoc(pairs, a)

(* 3.17 圖的算法 *)

(* 3.18 隨機數 *)

local
  val a = 16807.0 and m = 2147483647.0
in fun nextrand seed =
  let val t = a * seed
  in t - m * real(floor(t / m)) end
end

fun randlist (n, seed, tail) =
  if n = 0 then (seed, tail)
  else randlist(n - 1, nextrand seed, seed::tail)

val (seed, rs) = randlist(10000, 1.0, [])

(* 3.19 插入排序 *)

fun ins (x, []) : real list = [x]
  | ins (x, y::ys) =
      if x <= y then x::y::ys
      else y::ins(x, ys)

fun insort [] = []
  | insort (x::xs) = ins(x, insort xs)

(* 3.20 快速排序 *)

fun quick [] = []
  | quick [x] = [x]
  | quick (a::bs) =
      let fun partition (left, right, []) : real list =
            (quick left) @ (a :: quick right)
          | partition (left, right, x::xs) =
            (* 按大小分開 *)
            if x <= a then partition (x::left, right, xs)
            else partition (left, x::right, xs)
      in partition([], [], bs) end

val () = Check.done ()
