val check = Check.check

val seconds = 60

fun area r = Math.pi * r * r

(* 練習2.1 *)

(* 練習2.2
不同意，實數=和整數意義都不一樣
*)

(* 練習2.3
1和3要，2不需要，要有没有用到重載的函數或者操作符
*)

(* *重載 *)
fun double n = 2 * n

fun f u = Math.sin u / u

(* ~和*重載 *)
fun g k = ~ k * k

(* 練習2.4
第一個版本：~1返回前面那個數，10返回非數字
實際上都是非數字
第二個版本：~1報錯，10越界
*)

fun digit i = chr(i + ord #"0")

fun digit i = String.sub("0123456789", i)

(* 練習2.5 *)

fun is_date (d, m) =
  if m = "January" orelse m = "March" orelse m = "May" orelse
    m = "July" orelse m = "August" orelse m = "October" orelse
    m = "December" then
    1 <= d andalso d <= 31
  else if m = "April" orelse m = "June" orelse m = "September" orelse
    m = "November" then
    1 <= d andalso d <= 30
  else m = "February" andalso (1 <= d andalso d <= 28)

val zerovec = (0.0, 0.0)

fun lengthvec (x, y) = Math.sqrt (x * x + y * y)

fun negvec (x, y) : real * real = (~ x, ~ y)

type vec = real * real

fun addvec ((x1, y1), (x2, y2)) : vec = (x1 + x2, y1 + y2)

fun subvec (v1, v2) = addvec (v1, negvec v2)

fun distance pairv = lengthvec (subvec pairv)

(* ---- 測試用例 ---- *)
val () = check "31 January true"   (is_date (31, "January"))
val () = check "31 April false"     (not (is_date (31, "April")))
val () = check "29 February false"  (not (is_date (29, "February")))
val () = check "0 March false"      (not (is_date (0, "March")))

(* 練習2.6 *)

type time = (int * int * string)

fun time12to24 (h, m, p) =
  (* 凌晨十二點算零點 *)
  if p = "AM" andalso h = 12 then
    (0, m)
  (* 12:00PM至1:00PM之間需特殊處理 *)
  else if (p = "AM") orelse (h = 12 andalso p = "PM") then
    (h, m)
  else if p = "PM" then
    (h + 12, m)
  else
    (h, m)

fun timeBeforeHelp (ah, am) (bh, bm) =
  (ah < bh) orelse ((ah = bh) andalso (am < bm))

fun timeBefore a b =
  timeBeforeHelp (time12to24 a) (time12to24 b) 

(* ---- timeBefore 測試 ---- *)
val () = check "10:00AM < 11:00AM"          (timeBefore (10,0,"AM") (11,0,"AM"))
val () = check "11:00AM < 10:00AM = false"  (not (timeBefore (11,0,"AM") (10,0,"AM")))
val () = check "10:30AM < 10:45AM"          (timeBefore (10,30,"AM") (10,45,"AM"))
val () = check "10:45AM < 10:30AM = false"  (not (timeBefore (10,45,"AM") (10,30,"AM")))
val () = check "10:30AM < 10:30AM = false"  (not (timeBefore (10,30,"AM") (10,30,"AM")))
val () = check "11:59AM < 12:00PM"          (timeBefore (11,59,"AM") (12,0,"PM"))
val () = check "1:00PM < 1:00AM = false"    (not (timeBefore (1,0,"PM") (1,0,"AM")))
val () = check "11:59PM < 12:00AM = false"  (not (timeBefore (11,59,"PM") (12,0,"AM")))
val () = check "12:00AM < 12:00PM"          (timeBefore (12,0,"AM") (12,0,"PM"))
val () = check "12:00PM < 12:00AM = false"  (not (timeBefore (12,0,"PM") (12,0,"AM")))
val () = check "12:30AM < 1:00AM"           (timeBefore (12,30,"AM") (1,0,"AM"))
val () = check "12:30PM < 1:00PM"           (timeBefore (12,30,"PM") (1,0,"PM"))
val () = check "12:00PM < 11:59PM"          (timeBefore (12,0,"PM") (11,59,"PM"))
val () = check "1:00PM < 12:30PM = false"   (not (timeBefore (1,0,"PM") (12,30,"PM")))
val () = check "12:00AM < 12:01AM"          (timeBefore (12,0,"AM") (12,1,"AM"))

(* 練習2.7 *)

type money = (int * int * int)

fun toPence (apo, ash, ape) =
  apo * 20 * 12 + ash * 12 + ape

fun ofPence (pences: int) : money =
  ((pences div (20 * 12)), (pences mod (20 * 12)) div 12, (pences mod (20 * 12)) mod 12)

fun moneyPlus a b : money =
  ofPence (toPence a + toPence b)

fun moneyMinus a b :money =
  ofPence (toPence a - toPence b)

(* ---- moneyPlus / moneyMinus 測試 ---- *)
val () = check "moneyPlus (1,2,3) (4,5,6) = (5,7,9)"
                (moneyPlus (1,2,3) (4,5,6) = (5,7,9))
val () = check "moneyPlus (0,0,11) (0,0,1) = (0,1,0)"
                (moneyPlus (0,0,11) (0,0,1) = (0,1,0))
val () = check "moneyPlus (0,19,11) (0,0,1) = (1,0,0)"
                (moneyPlus (0,19,11) (0,0,1) = (1,0,0))
val () = check "moneyPlus (0,19,0) (0,2,0) = (1,1,0)"
                (moneyPlus (0,19,0) (0,2,0) = (1,1,0))
val () = check "moneyPlus (0,0,0) (0,0,0) = (0,0,0)"
                (moneyPlus (0,0,0) (0,0,0) = (0,0,0))
val () = check "moneyPlus (10,10,10) (20,15,5) = (31,6,3)"
                (moneyPlus (10,10,10) (20,15,5) = (31,6,3))

val () = check "moneyMinus (5,7,9) (4,5,6) = (1,2,3)"
                (moneyMinus (5,7,9) (4,5,6) = (1,2,3))
val () = check "moneyMinus (1,0,0) (0,0,1) = (0,19,11)"
                (moneyMinus (1,0,0) (0,0,1) = (0,19,11))
val () = check "moneyMinus (0,1,0) (0,0,1) = (0,0,11)"
                (moneyMinus (0,1,0) (0,0,1) = (0,0,11))
val () = check "moneyMinus (1,0,0) (1,0,0) = (0,0,0)"
                (moneyMinus (1,0,0) (1,0,0) = (0,0,0))
val () = check "moneyMinus (2,0,0) (1,19,11) = (0,0,1)"
                (moneyMinus (2,0,0) (1,19,11) = (0,0,1))
val () = check "moneyMinus (10,10,10) (5,5,5) = (5,5,5)"
                (moneyMinus (10,10,10) (5,5,5) = (5,5,5))

val () = check "toPence/ofPence roundtrip (1,2,3)"
                (ofPence (toPence (1,2,3)) = (1,2,3))
val () = check "toPence/ofPence roundtrip (0,19,11)"
                (ofPence (toPence (0,19,11)) = (0,19,11))

(* 2.9 紀錄 *)

type king = {
  name : string,
  born : int,
  crowned : int,
  died: int,
  quote : string
}

fun lifetime (k: king) = #died k - #born k

val henryV = {
  name = "Henry V",
  born = 1387,
  crowned = 1413,
  died = 1422,
  quote = "Bid them achieve me and then sell my bones."
}

(* 練習2.8
泛型
*)

fun lifetime {name, born, crowned, died, quote} = died - born

fun born_at {born} = born

(* 練習2.9
類型會不對
*)
(* val () = check "born_at" (born_at henryV = 1387) *)

infix xor

fun (p xor q) = (p orelse q) andalso not (p andalso q)

val () = check "(true xor false) = true" (true xor false)
val () = check "(false xor true) = true" (false xor true)
val () = check "(true xor true) = false" (not (true xor true))
val () = check "(false xor false) = false" (not (false xor false))

fun even n = n mod 2 = 0

fun powoftwo n =
  (n = 1) orelse ((even n) andalso powoftwo (n div 2))

val () = check "(powoftwo 6) = false" (not (powoftwo 6))

(* 練習2.10
powoftwo 8 => (8 = 1) orelse ((even 8) andalso powoftwo (8 div 2))
           => (even 8) andalso powoftwo (8 div 2)
           => powoftwo 4
           => (4 = 1) orelse ((even 4) andalso powoftwo (4 div 2))
           => (even 4) andalso powoftwo (4 div 2)
           => powoftwo 2
           => (2 = 1) orelse ((even 2) andalso powoftwo (2 div 2))
           => (even 2) andalso powoftwo (2 div 2)
           => powoftwo 1
           => (1 = 1) orelse ((even 1) andalso powoftwo (1 div 2))
           => true
*)

val () = check "(powoftwo 8) = false" (powoftwo 8)

(* 練習2.11
是，powoftwo的調用就是返回值
*)

fun power (x, k) : real =
  if k = 1 then x
  else if k mod 2 = 0 then power (x * x, k div 2)
  else x * power (x * x, k div 2)

(* 練習2.12
power (2.0, 29) =>
    if 29 = 1 then 2.0
    else if 29 mod 2 = 0 then power (2.0 * 2.0, 29 div 2)
    else 2.0 * power (2.0 * 2.0, 29 div 2)
    =>
    2.0 * power (2.0 * 2.0, 29 div 2)
    =>
    2.0 * power (4.0, 14)
    =>
    2.0 * (if 14 = 1 then 4.0
    else if 14 mod 2 = 0 then power (4.0 * 4.0, 14 div 2)
    else 4.0 * power (4.0 * 4.0, 14 div 2))
    =>
    2.0 * (power (4.0 * 4.0, 14 div 2))
    =>
    2.0 * (power (16.0, 7))
    =>
    2.0 * (if 7 = 1 then 16.0
    else if 7 mod 2 = 0 then power (16.0 * 16.0, 7 div 2)
    else 16.0 * power (16.0 * 16.0, 7 div 2))
    =>
    2.0 * (16.0 * power (16.0 * 16.0, 7 div 2))
    =>
    2.0 * (16.0 * power (256.0, 3))
    =>
    2.0 * (16.0 * if 3 = 1 then 256.0
    else if 3 mod 2 = 0 then power (256.0 * 256.0, 3 div 2)
    else 256.0 * power (256.0 * 256.0, 3 div 2))
    =>
    2.0 * (16.0 * (256.0 * power (65536.0, 1)))
    =>
    2.0 * (16.0 * (256.0 * (if 1 = 1 then 65536.0
    else if 1 mod 2 = 0 then power (65536.0 * 65536.0, 1 div 2)
    else 65536.0 * power (65536.0 * 65536.0, 1 div 2))))
    =>
    2.0 * (16.0 * (256.0 * 65536.0))
*)

(* 練習2.13
log次？
*)

(* 練習2.14
k = 0，等於1，沒意義
*)

fun nextfib (prev, curr :int) = (curr, prev + curr)

(* 递歸 *)
fun fibpair n =
  if n = 1 then (0, 1)
  else nextfib(fibpair(n - 1))

(* 迭代 *)
fun itfib(n, prev, curr) : int =
  if n = 1 then curr
  else itfib (n - 1, curr, prev + curr)

(* 練習2.15
如果是傳名就衹用計算一次
*)

(* 練習2.16
*)

(* 練習2.17
n = 1時，F(k)
n != 1時，n - 1, F(k), F(k - 1) + F(k)
*)

fun increase (k, n) =
  if (k + 1) * (k + 1) > n then k
  else k + 1

fun introot n =
  if n = 0 then 0
  else increase(2 * introot(n div 4), n)

val () = check "introot(123456789) = 11111" (introot 123456789 = 11111)

(* 練習2.18
*)

(* 練習2.19
*)

fun even n =
  n mod 2 = 0

fun gcd (m, n) =
  if (m = 0) then abs n
  else if (n = 0) then abs m
  else if (m = 0) andalso (n = 0) then 0
  else if m = n then m
  (* 兩個偶數 *)
  else if (even m) andalso (even n) then 2 * gcd(m div 2, n div 2)
  (* 一奇一偶 *)
  else if (even m) andalso not (even n) then gcd(m div 2, n)
  else if not (even m) andalso (even n) then gcd(n, m)
  (* 兩個奇數 *)
  else
    if m < n then
      gcd(((n - 1) div 2) - ((m - 1) div 2), m)
    else gcd(n, m)

(* ---- gcd 測試 ---- *)

(* 基本：1、互質、相等 *)
val () = check "gcd(1,1) = 1"           (gcd (1,1) = 1)
val () = check "gcd(1,5) = 1"           (gcd (1,5) = 1)
val () = check "gcd(5,1) = 1"           (gcd (5,1) = 1)
val () = check "gcd(17,5) = 1"          (gcd (17,5) = 1)
val () = check "gcd(2,3) = 1"           (gcd (2,3) = 1)
val () = check "gcd(3,2) = 1"           (gcd (3,2) = 1)
val () = check "gcd(999,1001) = 1"      (gcd (999,1001) = 1)
val () = check "gcd(2,2) = 2"           (gcd (2,2) = 2)
val () = check "gcd(17,17) = 17"        (gcd (17,17) = 17)

(* 對稱性：gcd(a,b) = gcd(b,a) *)
val () = check "gcd(6,8) = 2"           (gcd (6,8) = 2)
val () = check "gcd(8,6) = 2"           (gcd (8,6) = 2)
val () = check "gcd(12,18) = 6"         (gcd (12,18) = 6)
val () = check "gcd(18,12) = 6"         (gcd (18,12) = 6)
val () = check "gcd(34,17) = 17"        (gcd (34,17) = 17)
val () = check "gcd(17,34) = 17"        (gcd (17,34) = 17)

(* 四種奇偶組合 *)
val () = check "gcd(2,4) = 2"           (gcd (2,4) = 2)
val () = check "gcd(4,6) = 2"           (gcd (4,6) = 2)
val () = check "gcd(15,25) = 5"         (gcd (15,25) = 5)
val () = check "gcd(49,14) = 7"         (gcd (49,14) = 7)
val () = check "gcd(36,60) = 12"        (gcd (36,60) = 12)
val () = check "gcd(100,75) = 25"       (gcd (100,75) = 25)
val () = check "gcd(270,192) = 6"       (gcd (270,192) = 6)

(* 2 的冪 *)
val () = check "gcd(8,32) = 8"          (gcd (8,32) = 8)
val () = check "gcd(1024,2048) = 1024"  (gcd (1024,2048) = 1024)

(* 較大的數 / 倍數 *)
val () = check "gcd(45,30) = 15"        (gcd (45,30) = 15)
val () = check "gcd(48,180) = 12"       (gcd (48,180) = 12)
val () = check "gcd(91,77) = 7"         (gcd (91,77) = 7)
val () = check "gcd(1071,462) = 21"     (gcd (1071,462) = 21)
val () = check "gcd(123456,7890) = 6"   (gcd (123456,7890) = 6)

(* 縮放性質：gcd(ka,kb) = k * gcd(a,b) *)
val () = check "gcd(6,10) = 2"          (gcd (6,10) = 2)
val () = check "gcd(12,20) = 4"         (gcd (12,20) = 4)

(* 0,0 的約定 *)
val () = check "gcd(0,0) = 0"           (gcd (0,0) = 0)
val () = check "gcd(0,5) = 5"           (gcd (0,5) = 5)
val () = check "gcd(5,0) = 5"           (gcd (5,0) = 5)

fun fraction (n, d) =
  let val com = gcd(n, d) in
    (n div com, d div com)
  end

(* 練習2.20
參數可以重名，都可以用n
*)

(* 練習2.21
*)

(* 練習2.22
要用聯立
*)

(* 練習2.23
*)

structure Complex = struct
  type t = real * real

  val zero = (0.0, 0.0)

  fun sum ((x, y), (x', y')) =
    (x + x', y + y') : t

  fun diff ((x, y), (x', y')) =
    (x - x', y - y') : t

  fun prod ((x, y), (x', y')) =
    (x * x' - y * y', x * y' + x' * y) : t

  fun recip (x, y) =
    let val t = x * x + y * y in
        (x / t, ~ y / t)
    end
  
  fun quo (z, z') =
    prod(z, recip z')
end

signature ARITH = sig
  type t
  val zero : t
  val sum : t * t -> t
  val diff : t * t -> t
  val prod : t * t -> t
  val quo : t * t -> t
end

(* 練習2.24
*)

structure RealArith : ARITH = struct
  type t = real

  val zero = 0.0

  fun sum (x, y) = x + y : t

  fun diff (x, y) = x - y : t

  fun prod (x, y) = x * y : t

  fun quo (x, y) = x / y : t
end

(* ---- RealArith 測試 ---- *)
(* real 不是 equality type，所以用 Basis 的 Real.== 做精確比較 *)
val () = check "RealArith.zero = 0.0"
                (Real.== (RealArith.zero, 0.0))
val () = check "RealArith.sum (1.5, 2.25) = 3.75"
                (Real.== (RealArith.sum (1.5, 2.25), 3.75))
val () = check "RealArith.diff (5.0, 2.5) = 2.5"
                (Real.== (RealArith.diff (5.0, 2.5), 2.5))
val () = check "RealArith.prod (2.5, 4.0) = 10.0"
                (Real.== (RealArith.prod (2.5, 4.0), 10.0))
val () = check "RealArith.quo (7.5, 2.5) = 3.0"
                (Real.== (RealArith.quo (7.5, 2.5), 3.0))

val () = check "RealArith.sum (~1.5, 2.0) = 0.5"
                (Real.== (RealArith.sum (~1.5, 2.0), 0.5))
val () = check "RealArith.diff (2.0, 5.0) = ~3.0"
                (Real.== (RealArith.diff (2.0, 5.0), ~3.0))
val () = check "RealArith.prod (~2.0, 3.0) = ~6.0"
                (Real.== (RealArith.prod (~2.0, 3.0), ~6.0))
val () = check "RealArith.quo (~7.0, 2.0) = ~3.5"
                (Real.== (RealArith.quo (~7.0, 2.0), ~3.5))
val () = check "RealArith.quo (1.0, 4.0) = 0.25"
                (Real.== (RealArith.quo (1.0, 4.0), 0.25))

val () = check "RealArith.sum (3.25, zero) = 3.25"
                (Real.== (RealArith.sum (3.25, RealArith.zero), 3.25))
val () = check "RealArith.diff (3.25, 3.25) = 0.0"
                (Real.== (RealArith.diff (3.25, 3.25), 0.0))
val () = check "RealArith.prod (3.25, zero) = 0.0"
                (Real.== (RealArith.prod (3.25, RealArith.zero), 0.0))
val () = check "RealArith.quo (3.25, 3.25) = 1.0"
                (Real.== (RealArith.quo (3.25, 3.25), 1.0))

val () = check "RealArith.prod (1.5, sum (2.0, 3.0)) = 7.5"
                (Real.== (RealArith.prod (1.5, RealArith.sum (2.0, 3.0)), 7.5))
val () = check "RealArith.sum (prod (1.5, 2.0), prod (1.5, 3.0)) = 7.5"
                (Real.== (RealArith.sum (RealArith.prod (1.5, 2.0),
                                          RealArith.prod (1.5, 3.0)), 7.5))

(* 練習2.25
*)

structure Rational : ARITH = struct
  type t = int * int

  val zero = (0, 1)

  fun simply (n, d) =
    let val g = gcd(n, d)
        val n = n div g
        val d = d div g
        val (n, d) =
          if d > 0 then (n, d)
          else (~ n, ~ d)
    in
      (n, d)
    end

  fun sum ((n, d), (n', d')) =
    let val n = n * d' + n' * d
      val d = d * d'
    in
      simply (n, d)
    end

  fun diff ((n, d), (n', d')) =
    let val n = n * d' - n' * d
        val d = d * d'
    in
      simply (n, d)
    end

  fun prod ((n, d), (n', d')) =
    simply (n * n', d * d')

  fun quo (a, (n', d')) =
    simply (prod (a, (d', n')))
end

(* ---- Rational 測試 ---- *)
val () = check "Rational.zero = (0,1)"
                (Rational.zero = (0,1))

(* 加法 *)
val () = check "1/2 + 1/3 = 5/6"
                (Rational.sum ((1,2),(1,3)) = (5,6))
val () = check "1/2 + 1/2 = 1/1"
                (Rational.sum ((1,2),(1,2)) = (1,1))
val () = check "1/3 + 1/6 = 1/2"
                (Rational.sum ((1,3),(1,6)) = (1,2))
val () = check "2/3 + 1/6 = 5/6"
                (Rational.sum ((2,3),(1,6)) = (5,6))
val () = check "1/2 + 2/3 = 7/6"
                (Rational.sum ((1,2),(2,3)) = (7,6))
val () = check "3/5 + zero = 3/5"
                (Rational.sum ((3,5), Rational.zero) = (3,5))

(* 減法（結果為正） *)
val () = check "3/4 - 1/4 = 1/2"
                (Rational.diff ((3,4),(1,4)) = (1,2))
val () = check "5/6 - 1/3 = 1/2"
                (Rational.diff ((5,6),(1,3)) = (1,2))
val () = check "7/8 - 1/8 = 3/4"
                (Rational.diff ((7,8),(1,8)) = (3,4))
val () = check "3/5 - zero = 3/5"
                (Rational.diff ((3,5), Rational.zero) = (3,5))

(* 乘法 *)
val () = check "1/2 * 2/3 = 1/3"
                (Rational.prod ((1,2),(2,3)) = (1,3))
val () = check "3/4 * 2/5 = 3/10"
                (Rational.prod ((3,4),(2,5)) = (3,10))
val () = check "2/3 * 3/4 = 1/2"
                (Rational.prod ((2,3),(3,4)) = (1,2))
val () = check "3/5 * 1/1 = 3/5"
                (Rational.prod ((3,5),(1,1)) = (3,5))

(* 除法 *)
val () = check "(1/2) / (3/4) = 2/3"
                (Rational.quo ((1,2),(3,4)) = (2,3))
val () = check "(3/4) / (1/2) = 3/2"
                (Rational.quo ((3,4),(1,2)) = (3,2))
val () = check "(2/3) / (2/3) = 1/1"
                (Rational.quo ((2,3),(2,3)) = (1,1))
val () = check "(3/5) / (1/1) = 3/5"
                (Rational.quo ((3,5),(1,1)) = (3,5))

(* 化簡 *)
val () = check "2/4 simplifies to 1/2"
                (Rational.prod ((2,4),(1,1)) = (1,2))
val () = check "6/8 simplifies to 3/4"
                (Rational.sum ((6,8), Rational.zero) = (3,4))

(* 代數性質 *)
val () = check "x + y = y + x"
                (Rational.sum ((1,3),(2,5)) = Rational.sum ((2,5),(1,3)))
val () = check "(x*y)*z = x*(y*z)"
                (Rational.prod (Rational.prod ((1,2),(2,3)), (3,4)) =
                 Rational.prod ((1,2), Rational.prod ((2,3),(3,4))))
val () = check "x*(y+z) = x*y + x*z"
                (Rational.prod ((1,2), Rational.sum ((1,3),(1,4))) =
                 Rational.sum (Rational.prod ((1,2),(1,3)),
                               Rational.prod ((1,2),(1,4))))

(* 練習2.26
1是int -> n是int -> curr是int -> itfib (n - 1, curr, prev + curr)是int -> prev是int
int * int * int -> int
*)

(* 練習2.27
0是int -> k = 0 -> k是int
出錯
*)

(* 由 runner 載入 check.sml；這裡只收尾 *)
val () = Check.done ()
