;; json2dir: create the directory tree a JSON document describes, in the current directory.
;; Implements RFC J2D-1 (https://github.com/kitsunoff/awesome-json2dir/blob/main/spec/rfc-json2dir.md).
;; clojure.core + JDK interop only: the JSON parser is hand-written over the input bytes.
(ns json2dir.core
  (:gen-class)
  (:import (java.io ByteArrayOutputStream)
           (java.nio ByteBuffer)
           (java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets)
           (java.nio.file Files LinkOption NoSuchFileException OpenOption Path Paths StandardOpenOption)
           (java.nio.file.attribute BasicFileAttributes FileAttribute PosixFilePermission)
           (java.util Arrays)))

(set! *warn-on-reflection* true)

(defn- fail [msg] (throw (ex-info msg {:json2dir true})))

;; ---- §3: strict UTF-8 and strict JSON ----

(defn- check-utf8 [^bytes bs]
  ;; The JDK decoder rejects overlong forms, encoded surrogates and code points above U+10FFFF.
  (try
    (-> StandardCharsets/UTF_8 .newDecoder
        (.onMalformedInput CodingErrorAction/REPORT)
        (.onUnmappableCharacter CodingErrorAction/REPORT)
        (.decode (ByteBuffer/wrap bs)))
    (catch CharacterCodingException _ (fail "input is not valid UTF-8"))))

;; Parser state: the bytes and a one-element position array.
(defn- parse [^bytes bs]
  (let [n (alength bs)
        pos (int-array 1)
        at (fn ^long [] (aget pos 0))
        peek (fn ^long [] (let [i (aget pos 0)] (if (< i n) (bit-and (aget bs i) 0xff) -1)))
        adv (fn [k] (aset pos 0 (int (+ (aget pos 0) k))))
        err (fn [what] (fail (str "input is not valid JSON: " what " at offset " (at))))
        ws (fn [] (loop [] (when (#{0x20 0x09 0x0a 0x0d} (peek)) (adv 1) (recur))))
        expect (fn [^String word]
                 (doseq [c word]
                   (when (not= (peek) (int c)) (err "invalid literal"))
                   (adv 1)))
        digits (fn [] (let [start (at)]
                        (loop [] (when (<= 0x30 (peek) 0x39) (adv 1) (recur)))
                        (> (at) start)))
        number (fn []
                 (when (= (peek) 0x2d) (adv 1))
                 (if (= (peek) 0x30) (adv 1) (when-not (digits) (err "invalid number")))
                 (when (= (peek) 0x2e) (adv 1) (when-not (digits) (err "invalid number")))
                 (when (#{0x65 0x45} (peek))
                   (adv 1)
                   (when (#{0x2b 0x2d} (peek)) (adv 1))
                   (when-not (digits) (err "invalid number")))
                 :number)
        hex4 (fn []
               (when (> (+ (at) 4) n) (err "invalid \\u escape"))
               (let [v (reduce (fn [acc k]
                                 (let [d (Character/digit (char (bit-and (aget bs (+ (at) k)) 0xff)) 16)]
                                   (when (neg? d) (err "invalid \\u escape"))
                                   (+ (* acc 16) d)))
                               0 (range 4))]
                 (adv 4)
                 v))
        string (fn []
                 (adv 1)
                 (let [out (ByteArrayOutputStream.)]
                   (loop []
                     (let [c (peek)]
                       (cond
                         (= c -1) (err "unterminated string")
                         (= c 0x22) (adv 1)
                         (< c 0x20) (err "control character in string")
                         (not= c 0x5c) (do (.write out (int c)) (adv 1) (recur))
                         :else
                         (do (adv 1)
                             (let [e (peek)]
                               (adv 1)
                               (case (int e)
                                 0x22 (.write out 0x22)
                                 0x5c (.write out 0x5c)
                                 0x2f (.write out 0x2f)
                                 0x62 (.write out 0x08)
                                 0x66 (.write out 0x0c)
                                 0x6e (.write out 0x0a)
                                 0x72 (.write out 0x0d)
                                 0x74 (.write out 0x09)
                                 0x75 (let [u (hex4)
                                            cp (cond
                                                 (<= 0xd800 u 0xdbff)
                                                 (if (and (< (+ (at) 1) n)
                                                          (= (peek) 0x5c)
                                                          (= (bit-and (aget bs (inc (at))) 0xff) 0x75))
                                                   (do (adv 2)
                                                       (let [lo (hex4)]
                                                         (if (<= 0xdc00 lo 0xdfff)
                                                           (+ 0x10000 (bit-shift-left (- u 0xd800) 10) (- lo 0xdc00))
                                                           (fail "input contains an unpaired surrogate"))))
                                                   (fail "input contains an unpaired surrogate"))
                                                 (<= 0xdc00 u 0xdfff) (fail "input contains an unpaired surrogate")
                                                 :else u)
                                            ^bytes ub (.getBytes (String. (Character/toChars (int cp))) StandardCharsets/UTF_8)]
                                        (.write out ub 0 (alength ub)))
                                 (do (adv -1) (err "invalid escape"))))
                             (recur)))))
                   (String. (.toByteArray out) StandardCharsets/UTF_8)))]
    (letfn [(value []
              (case (int (peek))
                0x7b (object)
                0x5b (array)
                0x22 (string)
                0x74 (do (expect "true") :true)
                0x66 (do (expect "false") :false)
                0x6e (do (expect "null") :null)
                -1 (err "unexpected end of input")
                (if (or (= (peek) 0x2d) (<= 0x30 (peek) 0x39)) (number) (err "unexpected character"))))
            (object []
              (adv 1)
              (ws)
              (if (= (peek) 0x7d)
                (do (adv 1) {})
                (loop [m {}]
                  (ws)
                  (when (not= (peek) 0x22) (err "expected a member name"))
                  (let [k (string)]
                    (ws)
                    (when (not= (peek) 0x3a) (err "expected ':'"))
                    (adv 1)
                    (ws)
                    (let [m (assoc m k (value))] ; duplicates: the last occurrence wins
                      (ws)
                      (case (int (peek))
                        0x2c (do (adv 1) (recur m))
                        0x7d (do (adv 1) m)
                        (err "expected ',' or '}'")))))))
            (array []
              (adv 1)
              (ws)
              (if (= (peek) 0x5d)
                (do (adv 1) [])
                (loop [v []]
                  (ws)
                  (let [v (conj v (value))]
                    (ws)
                    (case (int (peek))
                      0x2c (do (adv 1) (recur v))
                      0x5d (do (adv 1) v)
                      (err "expected ',' or ']'"))))))]
      ;; A leading byte order mark is ignored, as §3 allows.
      (when (and (>= n 3) (= (bit-and (aget bs 0) 0xff) 0xef)
                 (= (bit-and (aget bs 1) 0xff) 0xbb) (= (bit-and (aget bs 2) 0xff) 0xbf))
        (adv 3))
      (ws)
      (let [v (value)]
        (ws)
        (when (< (at) n) (err "unexpected data after the value"))
        v))))

;; ---- §4, §6: validate the whole document before touching the file system ----

(defn- quote-str [^String s] (str "\"" (-> s (.replace "\\" "\\\\") (.replace "\"" "\\\"")) "\""))

(defn- validate [v where]
  (cond
    (string? v) nil
    (vector? v)
    (let [[kind payload] v]
      (when-not (and (= 2 (count v)) (string? kind) (string? payload))
        (fail (str where ": an array must be [\"link\", target] or [\"script\", content]")))
      (when-not (#{"link" "script"} kind)
        (fail (str where ": unknown array kind " (quote-str kind))))
      (when (and (= kind "link") (.contains ^String payload "\u0000"))
        (fail (str where ": a link target cannot contain NUL"))))
    (map? v)
    (doseq [[^String name child] v]
      (let [w (if (= where ".") name (str where "/" name))]
        ;; §4.2.1: names are used exactly; trailing "/" or "/." forms are rejected, not trimmed.
        (when (or (= name "") (= name ".") (= name "..") (.contains name "/") (.contains name "\u0000"))
          (fail (str w ": invalid name " (quote-str name))))
        (validate child w)))
    :else (fail (str where ": " (name v) " values are not allowed"))))

;; ---- §5: create the tree ----

(def ^:private nofollow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
(def ^:private no-attrs (into-array FileAttribute []))

(defn- utf8 ^bytes [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn- lstat ^BasicFileAttributes [^Path p]
  (try (Files/readAttributes p BasicFileAttributes ^"[Ljava.nio.file.LinkOption;" nofollow)
       (catch NoSuchFileException _ nil)))

;; §5.2, §5.3: an existing non-directory is removed (a symlink itself, never its target);
;; §5.4: a directory in the way of a non-object is an error.
(defn- clear [^Path p ^BasicFileAttributes existing]
  (when existing
    (when (.isDirectory existing) (fail (str p ": a directory is in the way")))
    (Files/delete p)))

(defn- write-file [^Path p ^String content executable]
  ;; CREATE_NEW = O_CREAT | O_EXCL, mode 0666 & ~umask.
  (Files/write p (utf8 content)
               ^"[Ljava.nio.file.OpenOption;"
               (into-array OpenOption [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE]))
  (when executable
    (let [perms (Files/getPosixFilePermissions p ^"[Ljava.nio.file.LinkOption;" nofollow)]
      (.add perms PosixFilePermission/OWNER_EXECUTE)
      (.add perms PosixFilePermission/GROUP_EXECUTE)
      (.add perms PosixFilePermission/OTHERS_EXECUTE)
      (Files/setPosixFilePermissions p perms))))

(defn- apply-tree [^Path dir tree]
  ;; Members in ascending order of the UTF-8 bytes of their names, like the reference.
  (doseq [^String name (sort (fn [a b] (Arrays/compareUnsigned (utf8 a) (utf8 b))) (keys tree))]
    (let [p (.resolve dir name)
          v (get tree name)
          existing (lstat p)]
      (cond
        (string? v) (do (clear p existing) (write-file p v false))
        (vector? v) (do (clear p existing)
                        (if (= (first v) "link")
                          ;; Note: Path normalizes the target (e.g. drops repeated or trailing "/"),
                          ;; so such link targets are not stored byte for byte.
                          (Files/createSymbolicLink p (Paths/get ^String (second v) (make-array String 0))
                                                    ^"[Ljava.nio.file.attribute.FileAttribute;" no-attrs)
                          (write-file p (second v) true)))
        :else (do (when-not (and existing (.isDirectory existing))
                    (when existing (Files/delete p))
                    (Files/createDirectory p ^"[Ljava.nio.file.attribute.FileAttribute;" no-attrs))
                  (apply-tree p v))))))

(defn -main [& args]
  (when (seq args)
    (binding [*out* *err*] (println "usage: json2dir < document.json"))
    (System/exit 2))
  (System/exit
   (try
     (let [bs (.readAllBytes System/in)]
       (check-utf8 bs)
       (let [doc (parse bs)]
         (when-not (map? doc) (fail "the root of the document must be an object"))
         (validate doc ".")
         (apply-tree (Paths/get "" (make-array String 0)) doc)
         0))
     (catch clojure.lang.ExceptionInfo e
       (binding [*out* *err*] (println (str "json2dir: " (ex-message e))))
       1)
     (catch StackOverflowError _
       (binding [*out* *err*] (println "json2dir: the document is nested too deeply"))
       1)
     (catch Exception e
       (binding [*out* *err*] (println (str "json2dir: " e)))
       1))))
