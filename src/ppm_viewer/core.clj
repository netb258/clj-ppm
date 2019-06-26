(ns ppm-viewer.core
  (:require [quil.core :as q]
            [clojure.string :as s])
  (:gen-class))

;; --------------------------------------------------------------------------------------------
;; --------------------------------------- File Reading ---------------------------------------
;; --------------------------------------------------------------------------------------------

(defn file-to-bytes
  "Takes a string path to a file and
  returns the file as a seq of java.lang.Byte.
  Example:
  (file-to-bytes \"TEST_FILE.txt\") => '(97 98 99 13 10 100 101 102 13 10 103 ...)"
  [file-path]
  (let [f (java.io.File. file-path)
        ary (byte-array (.length f))]
    (with-open [stream (java.io.FileInputStream. f)]
      (.read stream ary)
      (seq ary))))

;; We will need this little helper function, as PPM files contain values between 0 and 255
;; but java.lang.Byte holds values between -128 and +128.
(defn byte-to-ubyte
  "Converts a signed byte value (-128 to +128) to an unsigned byte value (0 to 255).
  Example:
  (byte-to-ubyte -12) => 244"
  [byte]
  (Byte/toUnsignedInt byte))

;; --------------------------------------------------------------------------------------------
;; ----------------------------------- PPM Header functions -----------------------------------
;; --------------------------------------------------------------------------------------------

(defn get-ppm-header
  "Takes a string path to a PPM image file and
  returns the header lines as a seq.
  Example:
  (get-ppm-header \"250px-Statue_of_Liberty_7.ppm\") => '(\"P6\" \"250 345\" \"255\")"
  [file-path]
  (let [file-lines (s/split (slurp file-path) #"\n")
        header (take-while #(not (re-find #"[0-9]+ [0-9]+" %)) file-lines) ;; This shuld get everything up-to the width and height.
        header (take (+ (count header) 2) file-lines)] ;; Now this should get the whole header.
    header))

(defn get-ppm-header-size
  "Takes a string path to a PPM image file and
  returns the char count of the PPM header.
  Example:
  (get-ppm-header-size \"250px-Statue_of_Liberty_7.ppm\") => 91"
  [file-path]
  (inc (count (s/join "\n" (get-ppm-header file-path)))))

(defn get-header-width-and-height
  "Takes a PPM header as a seq of lines and returns the image width and height as a vector (touple).
  Example:
  (get-header-width-and-height '(\"P6\" \"250 345\" \"255\")) => [250 345]"
  [header]
  (as-> header res
       (filter #(not (re-find #"^#" %)) res) ;; PPM files allow comments (lines with #). We should ignore these.
       (second res) ;; The width and height should be the second item in the header.
       (s/split res #" ") ;; The width and height are encoded very simply (space between them): 640 480.
       (mapv #(Integer/parseInt %) res)))

;; --------------------------------------------------------------------------------------------
;; ------------------------------------ PPM Body functions ------------------------------------
;; --------------------------------------------------------------------------------------------

(defn get-ppm-body
  "Takes a string path to a PPM image file and returns the file as a seq of bytes WITHOUT the header.
  Example: (get-ppm-body \"250px-Statue_of_Liberty_7.ppm\") => '(53 111 18 ...)"
  [file-path]
  (->> (file-to-bytes file-path)
       (drop (get-ppm-header-size file-path) ,)))

(def file-name (first *command-line-args*)) ;; The image name is a command line argument.
(def header (get-ppm-header file-name))
(def image-body (get-ppm-body file-name))
(def img-size (get-header-width-and-height header))

;; We want to make a vector of vectors (matrix), with every pixel in the image.
;; It should look like this:
;;  [[(12 101 -8) (55 -11 14) ...]
;;   [(112 4 -1) (101 106 -28) ...]
;;   ...]
;; Note, that every pixel is represented with 3 bytes (r g b).
(def pixels
  (let [pixel-bytes 3]
    (into []
          (map #(->> % (partition pixel-bytes ,) (into [] ,))
               (partition (* (first img-size) pixel-bytes) image-body)))))

;; --------------------------------------------------------------------------------------------
;; ---------------------------------------- Draw Image ----------------------------------------
;; --------------------------------------------------------------------------------------------

(defn setup []
  (q/frame-rate 1)
  (q/background 200))

(defn draw []
  (let [width (first img-size)
        height (second img-size)
        im (q/create-image width height :rgb)] ;; Create empty image.
    (dotimes [x height] ;; Populate the empty image.
      (dotimes [y width]
        (let [pixel (get-in pixels [x y])
              red (byte-to-ubyte (first pixel))
              green (byte-to-ubyte (second pixel))
              blue (byte-to-ubyte (last pixel))]
          (q/set-pixel im y x (q/color red green blue)))))
    (q/image im 0 0))) ;; Actually draw the image.

(defn -main [& args]
  (q/defsketch ppm
  :title "PPM Image"
  :settings #(q/smooth 2)
  :features [:exit-on-close]
  :setup setup
  :draw draw
  :size img-size))
