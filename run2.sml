(* run2.sml — 載入共用 harness 後執行 2.sml *)
use "check.sml";

(* 想看 PASS 就改成 true *)
Check.verbose := false;

use "2.sml";
