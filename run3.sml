(* run3.sml — 載入共用 harness 後執行 3.ml *)
use "check.sml";

(* 3.ml 原本會印 PASS；改成 false 可關閉 *)
Check.verbose := false;

use "3.sml";
