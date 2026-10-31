(ns tachocardia.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.charset Charset]
           [java.time Instant ZoneOffset])
  (:gen-class))

(defn read-bytes [path]
  (java.nio.file.Files/readAllBytes (.toPath (io/file path))))

;; java bytes are signed, of course
(defn ubyte [b]
  (bit-and (int b) 0xff))

(defn u16-be
  "2 bytes big endian"
  [data i]
  (+ (* (ubyte (aget data i)) 256)
     (ubyte (aget data (+ i 1)))))

(defn u32-be
  "4 bytes big endian"
  [data i]
  (+ (* (ubyte (aget data i)) 16777216)
     (* (ubyte (aget data (+ i 1))) 65536)
     (* (ubyte (aget data (+ i 2))) 256)
     (ubyte (aget data (+ i 3)))))

;; TODO: there might be a more cojure-like way of doing this...
(defn parse-tlv
  "file is a list of TLVs: 3 byte tag, 2 byte length, then value"
  [data]
  (let [n (alength data)
        result (atom [])]
    (loop [i 0]
      (if (> (+ i 5) n)
        @result
        (let [;; tag is 3 bytes
              tag (+ (* (ubyte (aget data i)) 65536)
                     (* (ubyte (aget data (+ i 1))) 256)
                     (ubyte (aget data (+ i 2))))
              len (u16-be data (+ i 3))
              start (+ i 5)]
          (if (> (+ start len) n)
            @result
            (do
              ;; copy the value
              (let [value (byte-array len)]
                (dotimes [k len]
                  (aset value k (aget data (+ start k))))
                (swap! result conj {:tag tag :length len :value value}))
              (recur (+ start len)))))))))

(defn iso8859 [code-page bs]
  (let [name (str "ISO-8859-" code-page)
        charset (try
                  (Charset/forName name)
                  ;; exceptions are still a question for me in this weird environment
                  (catch Exception e
                    ;; fallback
                    (Charset/forName "ISO-8859-1")))]
    (str/replace (String. bs charset) #"[\u0000 ]+$" "")))

(defn read-name
  ;; name = 1 byte code page + 35 bytes text
  [data i]
  (let [code-page (ubyte (aget data i))
        raw (byte-array 35)]
    (dotimes [k 35]
      (aset raw k (aget data (+ i 1 k))))
    {:code-page code-page
     :text (iso8859 code-page raw)
     :next (+ i 36)}))

(defn unix->date [data i]
  (let [t (u32-be data i)]
    (if (= t 0)
      nil
      (str (.toLocalDate (.atOffset (Instant/ofEpochSecond t) ZoneOffset/UTC))))))

(defn birth->date [data i]
  ;; birth date is stored as BCD / hex digits YYYYMMDD
  (let [hex (str (format "%02X" (ubyte (aget data i)))
                 (format "%02X" (ubyte (aget data (+ i 1))))
                 (format "%02X" (ubyte (aget data (+ i 2))))
                 (format "%02X" (ubyte (aget data (+ i 3)))))]
    (if (= hex "00000000")
      nil
      (str (subs hex 0 4) "-" (subs hex 4 6) "-" (subs hex 6 8)))))

(defn parse-identification [data]
  (let [nation (ubyte (aget data 0))
        card-number (String. data 1 16 (Charset/forName "US-ASCII"))
        auth (read-name data 17)
        i (:next auth)
        issue (unix->date data i)
        begin (unix->date data (+ i 4))
        expiry (unix->date data (+ i 8))
        surname (read-name data (+ i 12))
        first-names (read-name data (:next surname))
        birth-at (:next first-names)
        birth (birth->date data birth-at)
        lang (String. data (+ birth-at 4) 2 (Charset/forName "US-ASCII"))]
    {:card-issuing-member-state nation
     :card-number card-number
     :card-issuing-authority (:text auth)
     :card-issue-date issue
     :card-validity-begin begin
     :card-expiry-date expiry
     :surname (:text surname)
     :first-names (:text first-names)
     :birth-date birth
     :preferred-language lang}))

;; TODO hardcoded for now
(def identification-tag 0x052000)

(defn read-driver-identification [path]
  ;; TODO: maybe we don't need to read full file at once or at least not by one thread
  (let [tlvs (parse-tlv (read-bytes path))
        id-tlv (first (filter (fn [t] (= (:tag t) identification-tag)) tlvs))]
    (when id-tlv
      (parse-identification (:value id-tlv)))))

(defn -main [& args]
  ;; please use your own .DDD file you downloaded from your own tacho card
  (let [path (if (first args) (first args) "example.DDD")
        info (read-driver-identification path)]
    (if info
      (doseq [[k v] info]
        (println (str (name k) ": " v)))
      (do
        (println "could not find identification")
        (System/exit 1)))))
