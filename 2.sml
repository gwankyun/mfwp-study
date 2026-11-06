val failures = ref 0

fun check name ok =
    if ok then
        (* print ("PASS " ^ name ^ "\n") *)
        ()
    else (
        failures := !failures + 1;
        print ("FAIL " ^ name ^ "\n")
    )

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

(* 有失敗就以非零退出碼結束，方便腳本判斷 *)
val () = if !failures = 0 then () else OS.Process.exit OS.Process.failure
