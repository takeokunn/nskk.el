;;; nskk-azik.el --- AZIK extended romaji input support -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 takeokunn
;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; This file is part of NSKK (Next-generation SKK).
;; GNU General Public License v3+

;;; Commentary:
;; AZIK (Extended Romaji) input support for NSKK.
;; Implements the AZIK specification for efficient Japanese input.

;;; Code:

(require 'nskk-converter)

(defun nskk--init-azik-rules ()
  "Initialize AZIK romaji rules."

  ;; ============================================================
  ;; Basic vowels
  ;; ============================================================
  (nskk-converter-add-rule "a" "あ")
  (nskk-converter-add-rule "i" "い")
  (nskk-converter-add-rule "u" "う")
  (nskk-converter-add-rule "e" "え")
  (nskk-converter-add-rule "o" "お")

  ;; ============================================================
  ;; 1. Special keys
  ;; ============================================================
  (nskk-converter-add-rule ";" "っ")
  (nskk-converter-add-rule ":" "ー")

  ;; ============================================================
  ;; 2. Consonant compatibility keys (x/c prefixes)
  ;; ============================================================
  ;; x prefix - しゃ行互換
  (nskk-converter-add-rule "xa" "しゃ")
  (nskk-converter-add-rule "xi" "し")
  (nskk-converter-add-rule "xu" "しゅ")
  (nskk-converter-add-rule "xe" "しぇ")
  (nskk-converter-add-rule "xo" "しょ")

  ;; c prefix - ちゃ行互換
  (nskk-converter-add-rule "ca" "ちゃ")
  (nskk-converter-add-rule "ci" "ち")
  (nskk-converter-add-rule "cu" "ちゅ")
  (nskk-converter-add-rule "ce" "しぇ")
  (nskk-converter-add-rule "co" "ちょ")

  ;; ============================================================
  ;; 3. 撥音拡張 (consonant + extension key = consonant + vowel + ん)
  ;; Extension keys: z=a+n, k=i+n, j=u+n, d=e+n, l=o+n
  ;; ============================================================

  ;; か行 (k)
  (nskk-converter-add-rule "kz" "かん")
  (nskk-converter-add-rule "kk" "きん")
  (nskk-converter-add-rule "kj" "くん")
  (nskk-converter-add-rule "kd" "けん")
  (nskk-converter-add-rule "kl" "こん")

  ;; さ行 (s)
  (nskk-converter-add-rule "sz" "さん")
  (nskk-converter-add-rule "sk" "しん")
  (nskk-converter-add-rule "sj" "すん")
  (nskk-converter-add-rule "sd" "せん")
  (nskk-converter-add-rule "sl" "そん")

  ;; た行 (t)
  (nskk-converter-add-rule "tz" "たん")
  (nskk-converter-add-rule "tk" "ちん")
  (nskk-converter-add-rule "tj" "つん")
  (nskk-converter-add-rule "td" "てん")
  (nskk-converter-add-rule "tl" "とん")

  ;; な行 (n)
  (nskk-converter-add-rule "nz" "なん")
  (nskk-converter-add-rule "nk" "にん")
  (nskk-converter-add-rule "nj" "ぬん")
  (nskk-converter-add-rule "nd" "ねん")
  (nskk-converter-add-rule "nl" "のん")

  ;; は行 (h)
  (nskk-converter-add-rule "hz" "はん")
  (nskk-converter-add-rule "hk" "ひん")
  (nskk-converter-add-rule "hj" "ふん")
  (nskk-converter-add-rule "hd" "へん")
  (nskk-converter-add-rule "hl" "ほん")

  ;; ま行 (m)
  (nskk-converter-add-rule "mz" "まん")
  (nskk-converter-add-rule "mk" "みん")
  (nskk-converter-add-rule "mj" "むん")
  (nskk-converter-add-rule "md" "めん")
  (nskk-converter-add-rule "ml" "もん")

  ;; や行 (y)
  (nskk-converter-add-rule "yz" "やん")
  (nskk-converter-add-rule "yk" "いん")
  (nskk-converter-add-rule "yj" "ゆん")
  (nskk-converter-add-rule "yd" "えん")
  (nskk-converter-add-rule "yl" "よん")

  ;; ら行 (r)
  (nskk-converter-add-rule "rz" "らん")
  (nskk-converter-add-rule "rk" "りん")
  (nskk-converter-add-rule "rj" "るん")
  (nskk-converter-add-rule "rd" "れん")
  (nskk-converter-add-rule "rl" "ろん")

  ;; わ行 (w)
  (nskk-converter-add-rule "wz" "わん")
  (nskk-converter-add-rule "wk" "うぃん")
  (nskk-converter-add-rule "wj" "うん")
  (nskk-converter-add-rule "wd" "うぇん")
  (nskk-converter-add-rule "wl" "をん")

  ;; が行 (g)
  (nskk-converter-add-rule "gz" "がん")
  (nskk-converter-add-rule "gk" "ぎん")
  (nskk-converter-add-rule "gj" "ぐん")
  (nskk-converter-add-rule "gd" "げん")
  (nskk-converter-add-rule "gl" "ごん")

  ;; ざ行 (z)
  (nskk-converter-add-rule "zz" "ざん")
  (nskk-converter-add-rule "zk" "じん")
  (nskk-converter-add-rule "zj" "ずん")
  (nskk-converter-add-rule "zd" "ぜん")
  (nskk-converter-add-rule "zl" "ぞん")

  ;; だ行 (d)
  (nskk-converter-add-rule "dz" "だん")
  (nskk-converter-add-rule "dk" "ぢん")
  (nskk-converter-add-rule "dj" "づん")
  (nskk-converter-add-rule "dd" "でん")
  (nskk-converter-add-rule "dl" "どん")

  ;; ば行 (b)
  (nskk-converter-add-rule "bz" "ばん")
  (nskk-converter-add-rule "bk" "びん")
  (nskk-converter-add-rule "bj" "ぶん")
  (nskk-converter-add-rule "bd" "べん")
  (nskk-converter-add-rule "bl" "ぼん")

  ;; ぱ行 (p)
  (nskk-converter-add-rule "pz" "ぱん")
  (nskk-converter-add-rule "pk" "ぴん")
  (nskk-converter-add-rule "pj" "ぷん")
  (nskk-converter-add-rule "pd" "ぺん")
  (nskk-converter-add-rule "pl" "ぽん")

  ;; ============================================================
  ;; 4. 二重母音拡張 (consonant + extension key = consonant + vowel + vowel)
  ;; Extension keys: q=ai, h=uu, w=ei, p=ou
  ;; NOT for あ行
  ;; ============================================================

  ;; か行 (k)
  (nskk-converter-add-rule "kq" "かい")
  (nskk-converter-add-rule "kh" "くう")
  (nskk-converter-add-rule "kw" "けい")
  (nskk-converter-add-rule "kp" "こう")

  ;; さ行 (s)
  (nskk-converter-add-rule "sq" "さい")
  (nskk-converter-add-rule "sh" "すう")
  (nskk-converter-add-rule "sw" "せい")
  (nskk-converter-add-rule "sp" "そう")

  ;; た行 (t)
  (nskk-converter-add-rule "tq" "たい")
  (nskk-converter-add-rule "th" "つう")
  (nskk-converter-add-rule "tw" "てい")
  (nskk-converter-add-rule "tp" "とう")

  ;; な行 (n)
  (nskk-converter-add-rule "nq" "ない")
  (nskk-converter-add-rule "nh" "ぬう")
  (nskk-converter-add-rule "nw" "ねい")
  (nskk-converter-add-rule "np" "のう")

  ;; は行 (h)
  (nskk-converter-add-rule "hq" "はい")
  (nskk-converter-add-rule "hh" "ふう")
  (nskk-converter-add-rule "hw" "へい")
  (nskk-converter-add-rule "hp" "ほう")

  ;; ま行 (m)
  (nskk-converter-add-rule "mq" "まい")
  (nskk-converter-add-rule "mh" "むう")
  (nskk-converter-add-rule "mw" "めい")
  (nskk-converter-add-rule "mp" "もう")

  ;; や行 (y)
  (nskk-converter-add-rule "yq" "やい")
  (nskk-converter-add-rule "yh" "ゆう")
  (nskk-converter-add-rule "yw" "えい")
  (nskk-converter-add-rule "yp" "よう")

  ;; ら行 (r)
  (nskk-converter-add-rule "rq" "らい")
  (nskk-converter-add-rule "rh" "るう")
  (nskk-converter-add-rule "rw" "れい")
  (nskk-converter-add-rule "rp" "ろう")

  ;; わ行 (w)
  (nskk-converter-add-rule "wq" "わい")
  (nskk-converter-add-rule "wh" "うう")
  (nskk-converter-add-rule "ww" "うぇい")
  (nskk-converter-add-rule "wp" "うぉう")

  ;; が行 (g)
  (nskk-converter-add-rule "gq" "がい")
  (nskk-converter-add-rule "gh" "ぐう")
  (nskk-converter-add-rule "gw" "げい")
  (nskk-converter-add-rule "gp" "ごう")

  ;; ざ行 (z)
  (nskk-converter-add-rule "zq" "ざい")
  (nskk-converter-add-rule "zh" "ずう")
  (nskk-converter-add-rule "zw" "ぜい")
  (nskk-converter-add-rule "zp" "ぞう")

  ;; だ行 (d)
  (nskk-converter-add-rule "dq" "だい")
  (nskk-converter-add-rule "dh" "づう")
  (nskk-converter-add-rule "dw" "でい")
  (nskk-converter-add-rule "dp" "どう")

  ;; ば行 (b)
  (nskk-converter-add-rule "bq" "ばい")
  (nskk-converter-add-rule "bh" "ぶう")
  (nskk-converter-add-rule "bw" "べい")
  (nskk-converter-add-rule "bp" "ぼう")

  ;; ぱ行 (p)
  (nskk-converter-add-rule "pq" "ぱい")
  (nskk-converter-add-rule "ph" "ぷう")
  (nskk-converter-add-rule "pw" "ぺい")
  (nskk-converter-add-rule "pp" "ぽう")

  ;; ============================================================
  ;; 5. 拗音互換キー (g substitutes for y)
  ;; ============================================================

  ;; きゃ行 (kg = ky)
  (nskk-converter-add-rule "kga" "きゃ")
  (nskk-converter-add-rule "kgu" "きゅ")
  (nskk-converter-add-rule "kge" "きぇ")
  (nskk-converter-add-rule "kgo" "きょ")

  ;; きゃ行 + 撥音拡張
  (nskk-converter-add-rule "kgz" "きゃん")
  (nskk-converter-add-rule "kgk" "きぃん")
  (nskk-converter-add-rule "kgj" "きゅん")
  (nskk-converter-add-rule "kgd" "きぇん")
  (nskk-converter-add-rule "kgl" "きょん")

  ;; きゃ行 + 二重母音拡張
  (nskk-converter-add-rule "kgq" "きゃい")
  (nskk-converter-add-rule "kgh" "きゅう")
  (nskk-converter-add-rule "kgw" "きぇい")
  (nskk-converter-add-rule "kgp" "きょう")

  ;; ひゃ行 (hg = hy)
  (nskk-converter-add-rule "hga" "ひゃ")
  (nskk-converter-add-rule "hgu" "ひゅ")
  (nskk-converter-add-rule "hge" "ひぇ")
  (nskk-converter-add-rule "hgo" "ひょ")

  ;; ひゃ行 + 撥音拡張
  (nskk-converter-add-rule "hgz" "ひゃん")
  (nskk-converter-add-rule "hgk" "ひぃん")
  (nskk-converter-add-rule "hgj" "ひゅん")
  (nskk-converter-add-rule "hgd" "ひぇん")
  (nskk-converter-add-rule "hgl" "ひょん")

  ;; ひゃ行 + 二重母音拡張
  (nskk-converter-add-rule "hgq" "ひゃい")
  (nskk-converter-add-rule "hgh" "ひゅう")
  (nskk-converter-add-rule "hgw" "ひぇい")
  (nskk-converter-add-rule "hgp" "ひょう")

  ;; みゃ行 (mg = my)
  (nskk-converter-add-rule "mga" "みゃ")
  (nskk-converter-add-rule "mgu" "みゅ")
  (nskk-converter-add-rule "mge" "みぇ")
  (nskk-converter-add-rule "mgo" "みょ")

  ;; みゃ行 + 撥音拡張
  (nskk-converter-add-rule "mgz" "みゃん")
  (nskk-converter-add-rule "mgk" "みぃん")
  (nskk-converter-add-rule "mgj" "みゅん")
  (nskk-converter-add-rule "mgd" "みぇん")
  (nskk-converter-add-rule "mgl" "みょん")

  ;; みゃ行 + 二重母音拡張
  (nskk-converter-add-rule "mgq" "みゃい")
  (nskk-converter-add-rule "mgh" "みゅう")
  (nskk-converter-add-rule "mgw" "みぇい")
  (nskk-converter-add-rule "mgp" "みょう")

  ;; りゃ行 (rg = ry)
  (nskk-converter-add-rule "rga" "りゃ")
  (nskk-converter-add-rule "rgu" "りゅ")
  (nskk-converter-add-rule "rge" "りぇ")
  (nskk-converter-add-rule "rgo" "りょ")

  ;; りゃ行 + 撥音拡張
  (nskk-converter-add-rule "rgz" "りゃん")
  (nskk-converter-add-rule "rgk" "りぃん")
  (nskk-converter-add-rule "rgj" "りゅん")
  (nskk-converter-add-rule "rgd" "りぇん")
  (nskk-converter-add-rule "rgl" "りょん")

  ;; りゃ行 + 二重母音拡張
  (nskk-converter-add-rule "rgq" "りゃい")
  (nskk-converter-add-rule "rgh" "りゅう")
  (nskk-converter-add-rule "rgw" "りぇい")
  (nskk-converter-add-rule "rgp" "りょう")

  ;; ぎゃ行 (gg = gy)
  (nskk-converter-add-rule "gga" "ぎゃ")
  (nskk-converter-add-rule "ggu" "ぎゅ")
  (nskk-converter-add-rule "gge" "ぎぇ")
  (nskk-converter-add-rule "ggo" "ぎょ")

  ;; ぎゃ行 + 撥音拡張
  (nskk-converter-add-rule "ggz" "ぎゃん")
  (nskk-converter-add-rule "ggk" "ぎぃん")
  (nskk-converter-add-rule "ggj" "ぎゅん")
  (nskk-converter-add-rule "ggd" "ぎぇん")
  (nskk-converter-add-rule "ggl" "ぎょん")

  ;; ぎゃ行 + 二重母音拡張
  (nskk-converter-add-rule "ggq" "ぎゃい")
  (nskk-converter-add-rule "ggh" "ぎゅう")
  (nskk-converter-add-rule "ggw" "ぎぇい")
  (nskk-converter-add-rule "ggp" "ぎょう")

  ;; じゃ行 (jg = jy)
  (nskk-converter-add-rule "jga" "じゃ")
  (nskk-converter-add-rule "jgu" "じゅ")
  (nskk-converter-add-rule "jge" "じぇ")
  (nskk-converter-add-rule "jgo" "じょ")

  ;; じゃ行 + 撥音拡張
  (nskk-converter-add-rule "jgz" "じゃん")
  (nskk-converter-add-rule "jgk" "じぃん")
  (nskk-converter-add-rule "jgj" "じゅん")
  (nskk-converter-add-rule "jgd" "じぇん")
  (nskk-converter-add-rule "jgl" "じょん")

  ;; じゃ行 + 二重母音拡張
  (nskk-converter-add-rule "jgq" "じゃい")
  (nskk-converter-add-rule "jgh" "じゅう")
  (nskk-converter-add-rule "jgw" "じぇい")
  (nskk-converter-add-rule "jgp" "じょう")

  ;; びゃ行 (bg = by)
  (nskk-converter-add-rule "bga" "びゃ")
  (nskk-converter-add-rule "bgu" "びゅ")
  (nskk-converter-add-rule "bge" "びぇ")
  (nskk-converter-add-rule "bgo" "びょ")

  ;; びゃ行 + 撥音拡張
  (nskk-converter-add-rule "bgz" "びゃん")
  (nskk-converter-add-rule "bgk" "びぃん")
  (nskk-converter-add-rule "bgj" "びゅん")
  (nskk-converter-add-rule "bgd" "びぇん")
  (nskk-converter-add-rule "bgl" "びょん")

  ;; びゃ行 + 二重母音拡張
  (nskk-converter-add-rule "bgq" "びゃい")
  (nskk-converter-add-rule "bgh" "びゅう")
  (nskk-converter-add-rule "bgw" "びぇい")
  (nskk-converter-add-rule "bgp" "びょう")

  ;; ぴゃ行 (pg = py)
  (nskk-converter-add-rule "pga" "ぴゃ")
  (nskk-converter-add-rule "pgu" "ぴゅ")
  (nskk-converter-add-rule "pge" "ぴぇ")
  (nskk-converter-add-rule "pgo" "ぴょ")

  ;; ぴゃ行 + 撥音拡張
  (nskk-converter-add-rule "pgz" "ぴゃん")
  (nskk-converter-add-rule "pgk" "ぴぃん")
  (nskk-converter-add-rule "pgj" "ぴゅん")
  (nskk-converter-add-rule "pgd" "ぴぇん")
  (nskk-converter-add-rule "pgl" "ぴょん")

  ;; ぴゃ行 + 二重母音拡張
  (nskk-converter-add-rule "pgq" "ぴゃい")
  (nskk-converter-add-rule "pgh" "ぴゅう")
  (nskk-converter-add-rule "pgw" "ぴぇい")
  (nskk-converter-add-rule "pgp" "ぴょう")

  ;; ============================================================
  ;; 6. 同指打鍵互換キー (f alternatives)
  ;; ============================================================
  (nskk-converter-add-rule "kf" "き")
  (nskk-converter-add-rule "nf" "ぬ")
  (nskk-converter-add-rule "mf" "む")
  (nskk-converter-add-rule "gf" "ぐ")
  (nskk-converter-add-rule "pf" "ぷ")
  (nskk-converter-add-rule "rf" "る")
  (nskk-converter-add-rule "yf" "ゆ")

  ;; ============================================================
  ;; 7. 特殊拡張 (word shortcuts)
  ;; ============================================================
  (nskk-converter-add-rule "km" "かも")
  (nskk-converter-add-rule "kr" "から")
  (nskk-converter-add-rule "gr" "がら")
  (nskk-converter-add-rule "kt" "こと")
  (nskk-converter-add-rule "gt" "ごと")

  (nskk-converter-add-rule "zr" "ざる")
  (nskk-converter-add-rule "st" "した")
  (nskk-converter-add-rule "sr" "する")
  (nskk-converter-add-rule "tt" "たち")
  (nskk-converter-add-rule "dt" "だち")

  (nskk-converter-add-rule "tb" "たび")
  (nskk-converter-add-rule "tm" "ため")
  (nskk-converter-add-rule "tr" "たら")
  (nskk-converter-add-rule "ds" "です")
  (nskk-converter-add-rule "dm" "でも")

  (nskk-converter-add-rule "nr" "なる")
  (nskk-converter-add-rule "nt" "にち")
  (nskk-converter-add-rule "nb" "ねば")
  (nskk-converter-add-rule "ht" "ひと")
  (nskk-converter-add-rule "bt" "びと")

  (nskk-converter-add-rule "ms" "ます")
  (nskk-converter-add-rule "mt" "また")
  (nskk-converter-add-rule "mn" "もの")
  (nskk-converter-add-rule "yr" "よる")

  (nskk-converter-add-rule "rr" "られ")
  (nskk-converter-add-rule "wt" "わた")
  (nskk-converter-add-rule "wr" "われ")

  ;; ============================================================
  ;; 8. 外来語拡張
  ;; ============================================================
  (nskk-converter-add-rule "tgi" "てぃ")
  (nskk-converter-add-rule "tgu" "とぅ")
  (nskk-converter-add-rule "dci" "でぃ")
  (nskk-converter-add-rule "dcu" "どぅ")
  (nskk-converter-add-rule "wso" "うぉ")

  ;; ============================================================
  ;; Standard romaji rules (for compatibility)
  ;; ============================================================

  ;; か行
  (nskk-converter-add-rule "ka" "か")
  (nskk-converter-add-rule "ki" "き")
  (nskk-converter-add-rule "ku" "く")
  (nskk-converter-add-rule "ke" "け")
  (nskk-converter-add-rule "ko" "こ")

  ;; が行
  (nskk-converter-add-rule "ga" "が")
  (nskk-converter-add-rule "gi" "ぎ")
  (nskk-converter-add-rule "gu" "ぐ")
  (nskk-converter-add-rule "ge" "げ")
  (nskk-converter-add-rule "go" "ご")

  ;; さ行
  (nskk-converter-add-rule "sa" "さ")
  (nskk-converter-add-rule "si" "し")
  (nskk-converter-add-rule "shi" "し")
  (nskk-converter-add-rule "su" "す")
  (nskk-converter-add-rule "se" "せ")
  (nskk-converter-add-rule "so" "そ")

  ;; ざ行
  (nskk-converter-add-rule "za" "ざ")
  (nskk-converter-add-rule "zi" "じ")
  (nskk-converter-add-rule "ji" "じ")
  (nskk-converter-add-rule "zu" "ず")
  (nskk-converter-add-rule "ze" "ぜ")
  (nskk-converter-add-rule "zo" "ぞ")

  ;; た行
  (nskk-converter-add-rule "ta" "た")
  (nskk-converter-add-rule "ti" "ち")
  (nskk-converter-add-rule "chi" "ち")
  (nskk-converter-add-rule "tu" "つ")
  (nskk-converter-add-rule "tsu" "つ")
  (nskk-converter-add-rule "te" "て")
  (nskk-converter-add-rule "to" "と")

  ;; だ行
  (nskk-converter-add-rule "da" "だ")
  (nskk-converter-add-rule "di" "ぢ")
  (nskk-converter-add-rule "du" "づ")
  (nskk-converter-add-rule "de" "で")
  (nskk-converter-add-rule "do" "ど")

  ;; な行
  (nskk-converter-add-rule "na" "な")
  (nskk-converter-add-rule "ni" "に")
  (nskk-converter-add-rule "nu" "ぬ")
  (nskk-converter-add-rule "ne" "ね")
  (nskk-converter-add-rule "no" "の")
  (nskk-converter-add-rule "nn" "ん")
  (nskk-converter-add-rule "n'" "ん")

  ;; は行
  (nskk-converter-add-rule "ha" "は")
  (nskk-converter-add-rule "hi" "ひ")
  (nskk-converter-add-rule "hu" "ふ")
  (nskk-converter-add-rule "fu" "ふ")
  (nskk-converter-add-rule "he" "へ")
  (nskk-converter-add-rule "ho" "ほ")

  ;; ば行
  (nskk-converter-add-rule "ba" "ば")
  (nskk-converter-add-rule "bi" "び")
  (nskk-converter-add-rule "bu" "ぶ")
  (nskk-converter-add-rule "be" "べ")
  (nskk-converter-add-rule "bo" "ぼ")

  ;; ぱ行
  (nskk-converter-add-rule "pa" "ぱ")
  (nskk-converter-add-rule "pi" "ぴ")
  (nskk-converter-add-rule "pu" "ぷ")
  (nskk-converter-add-rule "pe" "ぺ")
  (nskk-converter-add-rule "po" "ぽ")

  ;; ま行
  (nskk-converter-add-rule "ma" "ま")
  (nskk-converter-add-rule "mi" "み")
  (nskk-converter-add-rule "mu" "む")
  (nskk-converter-add-rule "me" "め")
  (nskk-converter-add-rule "mo" "も")

  ;; や行
  (nskk-converter-add-rule "ya" "や")
  (nskk-converter-add-rule "yu" "ゆ")
  (nskk-converter-add-rule "yo" "よ")

  ;; ら行
  (nskk-converter-add-rule "ra" "ら")
  (nskk-converter-add-rule "ri" "り")
  (nskk-converter-add-rule "ru" "る")
  (nskk-converter-add-rule "re" "れ")
  (nskk-converter-add-rule "ro" "ろ")

  ;; わ行
  (nskk-converter-add-rule "wa" "わ")
  (nskk-converter-add-rule "wo" "を")

  ;; 拗音 (standard)
  (nskk-converter-add-rule "kya" "きゃ")
  (nskk-converter-add-rule "kyu" "きゅ")
  (nskk-converter-add-rule "kye" "きぇ")
  (nskk-converter-add-rule "kyo" "きょ")

  (nskk-converter-add-rule "gya" "ぎゃ")
  (nskk-converter-add-rule "gyu" "ぎゅ")
  (nskk-converter-add-rule "gye" "ぎぇ")
  (nskk-converter-add-rule "gyo" "ぎょ")

  (nskk-converter-add-rule "sya" "しゃ")
  (nskk-converter-add-rule "sha" "しゃ")
  (nskk-converter-add-rule "syu" "しゅ")
  (nskk-converter-add-rule "shu" "しゅ")
  (nskk-converter-add-rule "sye" "しぇ")
  (nskk-converter-add-rule "she" "しぇ")
  (nskk-converter-add-rule "syo" "しょ")
  (nskk-converter-add-rule "sho" "しょ")

  (nskk-converter-add-rule "zya" "じゃ")
  (nskk-converter-add-rule "ja" "じゃ")
  (nskk-converter-add-rule "zyu" "じゅ")
  (nskk-converter-add-rule "ju" "じゅ")
  (nskk-converter-add-rule "zye" "じぇ")
  (nskk-converter-add-rule "je" "じぇ")
  (nskk-converter-add-rule "zyo" "じょ")
  (nskk-converter-add-rule "jo" "じょ")

  (nskk-converter-add-rule "tya" "ちゃ")
  (nskk-converter-add-rule "cha" "ちゃ")
  (nskk-converter-add-rule "tyu" "ちゅ")
  (nskk-converter-add-rule "chu" "ちゅ")
  (nskk-converter-add-rule "tye" "ちぇ")
  (nskk-converter-add-rule "che" "ちぇ")
  (nskk-converter-add-rule "tyo" "ちょ")
  (nskk-converter-add-rule "cho" "ちょ")

  (nskk-converter-add-rule "dya" "ぢゃ")
  (nskk-converter-add-rule "dyu" "ぢゅ")
  (nskk-converter-add-rule "dye" "ぢぇ")
  (nskk-converter-add-rule "dyo" "ぢょ")

  (nskk-converter-add-rule "nya" "にゃ")
  (nskk-converter-add-rule "nyu" "にゅ")
  (nskk-converter-add-rule "nye" "にぇ")
  (nskk-converter-add-rule "nyo" "にょ")

  (nskk-converter-add-rule "hya" "ひゃ")
  (nskk-converter-add-rule "hyu" "ひゅ")
  (nskk-converter-add-rule "hye" "ひぇ")
  (nskk-converter-add-rule "hyo" "ひょ")

  (nskk-converter-add-rule "bya" "びゃ")
  (nskk-converter-add-rule "byu" "びゅ")
  (nskk-converter-add-rule "bye" "びぇ")
  (nskk-converter-add-rule "byo" "びょ")

  (nskk-converter-add-rule "pya" "ぴゃ")
  (nskk-converter-add-rule "pyu" "ぴゅ")
  (nskk-converter-add-rule "pye" "ぴぇ")
  (nskk-converter-add-rule "pyo" "ぴょ")

  (nskk-converter-add-rule "mya" "みゃ")
  (nskk-converter-add-rule "myu" "みゅ")
  (nskk-converter-add-rule "mye" "みぇ")
  (nskk-converter-add-rule "myo" "みょ")

  (nskk-converter-add-rule "rya" "りゃ")
  (nskk-converter-add-rule "ryu" "りゅ")
  (nskk-converter-add-rule "rye" "りぇ")
  (nskk-converter-add-rule "ryo" "りょ")

  ;; ふぁ行
  (nskk-converter-add-rule "fa" "ふぁ")
  (nskk-converter-add-rule "fi" "ふぃ")
  (nskk-converter-add-rule "fe" "ふぇ")
  (nskk-converter-add-rule "fo" "ふぉ")

  ;; つぁ行
  (nskk-converter-add-rule "tsa" "つぁ")
  (nskk-converter-add-rule "tsi" "つぃ")
  (nskk-converter-add-rule "tse" "つぇ")
  (nskk-converter-add-rule "tso" "つぉ")

  ;; 長音
  (nskk-converter-add-rule "-" "ー")

  ;; 小さい文字
  (nskk-converter-add-rule "la" "ぁ")
  (nskk-converter-add-rule "li" "ぃ")
  (nskk-converter-add-rule "lu" "ぅ")
  (nskk-converter-add-rule "le" "ぇ")
  (nskk-converter-add-rule "lo" "ぉ")
  (nskk-converter-add-rule "lya" "ゃ")
  (nskk-converter-add-rule "lyu" "ゅ")
  (nskk-converter-add-rule "lyo" "ょ")
  (nskk-converter-add-rule "ltu" "っ")
  (nskk-converter-add-rule "ltsu" "っ")
  (nskk-converter-add-rule "xtu" "っ")
  (nskk-converter-add-rule "xtsu" "っ")

  ;; Consonant-only (for partial match)
  (nskk-converter-add-rule "k" :incomplete)
  (nskk-converter-add-rule "g" :incomplete)
  (nskk-converter-add-rule "s" :incomplete)
  (nskk-converter-add-rule "z" :incomplete)
  (nskk-converter-add-rule "t" :incomplete)
  (nskk-converter-add-rule "d" :incomplete)
  (nskk-converter-add-rule "n" :incomplete)
  (nskk-converter-add-rule "h" :incomplete)
  (nskk-converter-add-rule "b" :incomplete)
  (nskk-converter-add-rule "p" :incomplete)
  (nskk-converter-add-rule "m" :incomplete)
  (nskk-converter-add-rule "y" :incomplete)
  (nskk-converter-add-rule "r" :incomplete)
  (nskk-converter-add-rule "w" :incomplete))

;; Register AZIK style
(nskk-converter-register-style 'azik #'nskk--init-azik-rules)

(provide 'nskk-azik)

;;; nskk-azik.el ends here
