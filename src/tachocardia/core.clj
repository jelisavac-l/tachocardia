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

(defn u24-be
  "3 bytes big endian (used for odometer and tags)"
  [data i]
  (+ (* (ubyte (aget data i)) 65536)
     (* (ubyte (aget data (+ i 1))) 256)
     (ubyte (aget data (+ i 2)))))

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

(defn unix->datetime [data i]
  (let [t (u32-be data i)]
    (if (= t 0)
      nil
      (str (Instant/ofEpochSecond t)))))

(defn read-reg-number
  ;; vehicle registration = 1 byte nation + 1 byte code page + 13 bytes text
  [data i]
  (let [nation (ubyte (aget data i))
        code-page (ubyte (aget data (+ i 1)))
        raw (byte-array 13)]
    (dotimes [k 13]
      (aset raw k (aget data (+ i 2 k))))
    {:nation nation
     :registration (iso8859 code-page raw)}))

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
        card-number (str/trim (String. data 1 16 (Charset/forName "US-ASCII")))
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

(defn parse-licence [data]
  (let [auth (read-name data 0)
        nation (ubyte (aget data 36))
        number (str/trim (String. data 37 16 (Charset/forName "US-ASCII")))]
    {:driving-licence-issuing-authority (:text auth)
     :driving-licence-issuing-nation nation
     :driving-licence-number number}))

;; event and fault records have the exact same 24-byte layout
(defn parse-event-records [data]
  (let [n (alength data)
        result (atom [])]
    (loop [i 0]
      (if (> (+ i 24) n)
        @result
        (let [etype (ubyte (aget data i))
              begin (unix->datetime data (+ i 1))
              end (unix->datetime data (+ i 5))
              veh (read-reg-number data (+ i 9))]
          ;; skip empty slots in the circular buffer
          (when begin
            (swap! result conj {:type etype
                                :begin-time begin
                                :end-time end
                                :vehicle-nation (:nation veh)
                                :vehicle-registration (:registration veh)}))
          (recur (+ i 24)))))))

(defn parse-vehicles [data]
  ;; 2 bytes newest pointer, then 31-byte records
  (let [n (alength data)
        result (atom [])]
    (loop [i 2]
      (if (> (+ i 31) n)
        @result
        (let [odo-begin (u24-be data i)
              odo-end (u24-be data (+ i 3))
              first-use (unix->datetime data (+ i 6))
              last-use (unix->datetime data (+ i 10))
              veh (read-reg-number data (+ i 14))]
          (when first-use
            (swap! result conj {:first-use first-use
                                :last-use last-use
                                :odometer-begin-km odo-begin
                                :odometer-end-km odo-end
                                :vehicle-nation (:nation veh)
                                :vehicle-registration (:registration veh)}))
          (recur (+ i 31)))))))

(defn parse-places [data]
  ;; 1 byte newest pointer, then 10-byte records
  (let [n (alength data)
        result (atom [])]
    (loop [i 1]
      (if (> (+ i 10) n)
        @result
        (let [entry-time (unix->datetime data i)
              entry-type (ubyte (aget data (+ i 4)))
              country (ubyte (aget data (+ i 5)))
              region (ubyte (aget data (+ i 6)))
              odometer (u24-be data (+ i 7))]
          (when entry-time
            (swap! result conj {:entry-time entry-time
                                :entry-type entry-type
                                :country country
                                :region region
                                :odometer-km odometer}))
          (recur (+ i 10)))))))

;; TODO hardcoded tags from the EU spec
(def identification-tag 0x052000)
(def licence-tag        0x052100)
(def events-tag         0x050200)
(def faults-tag         0x050300)
(def vehicles-tag       0x050500)
(def places-tag         0x050600)

(defn find-tlv [tlvs tag]
  (:value (first (filter (fn [t] (= (:tag t) tag)) tlvs))))

(defn read-driver-identification [path]
  ;; TODO: maybe we don't need to read full file at once or at least not by one thread
  (let [tlvs (parse-tlv (read-bytes path))
        id-val (find-tlv tlvs identification-tag)]
    (when id-val
      (parse-identification id-val))))

(defn read-card [path]
  (let [tlvs (parse-tlv (read-bytes path))
        id-val (find-tlv tlvs identification-tag)
        lic-val (find-tlv tlvs licence-tag)
        ev-val (find-tlv tlvs events-tag)
        fl-val (find-tlv tlvs faults-tag)
        veh-val (find-tlv tlvs vehicles-tag)
        plc-val (find-tlv tlvs places-tag)]
    {:identification (when id-val (parse-identification id-val))
     :driving-licence (when lic-val (parse-licence lic-val))
     :events (if ev-val (parse-event-records ev-val) [])
     :faults (if fl-val (parse-event-records fl-val) [])
     :vehicles-used (if veh-val (parse-vehicles veh-val) [])
     :places (if plc-val (parse-places plc-val) [])}))

(defn -main [& args]
  ;; please use your own .DDD file you downloaded from your own tacho card
  (let [path (if (first args) (first args) "example.DDD")
        card (read-card path)]
    (if (:identification card)
      (println card)
      (do
        (println "could not find identification")
        (System/exit 1)))))
