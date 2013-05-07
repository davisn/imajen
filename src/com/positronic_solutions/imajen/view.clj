;; Copyright 2013 Positronic Solutions, LLC

;; This file is part of Imajen.

;; Imajen is free software:  you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Imajen is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with Imajen.  If not, see <http://www.gnu.org/licenses/>.

(ns com.positronic-solutions.imajen.view
  (:use seesaw.core
        seesaw.color
        seesaw.graphics)
  (:require [com.positronic-solutions.imajen :as imajen]))

(defn render-function [f ^long w ^long h]
  ;; Note:  The following type hint (BufferedImage) really boosts performance
  ;;        (over 5x for a simple, constant function)
  (let [^java.awt.image.BufferedImage img (buffered-image w h)]
    (let [dx (/ 1.0 w)
          dy (/ 1.0 h)]
      (doseq [^long x (range w)
              ^long y (range h)]
        (let [[^double rf ^double gf ^double bf] (f (* x dx) (* y dy))
              r (int (* 255 (imajen/clamp rf 0.0 1.0)))
              g (int (* 255 (imajen/clamp gf 0.0 1.0)))
              b (int (* 255 (imajen/clamp bf 0.0 1.0)))
              ;; Note:  It's important to use unchecked-int here
              ;;        in order to avoid an IllegalArgumentException
              ;;        (value out of range for int).
              ;;
              ;;        See here for details:
              ;;          http://stackoverflow.com/questions/8598124/unsigned-comparison-of-numbers-in-clojure
              c (unchecked-int (bit-or (bit-shift-left 255 24)
                                       (bit-shift-left r 16)
                                       (bit-shift-left g 8)
                                       b))]
          ;; We can probably improve performance by rendering to an array,
          ;; then doing a bulk copy
          (. img setRGB x y c))))
    img))

(defn function-canvas [f]
  (canvas :background :black
          :paint (fn [c g]
                   (time
                    (do
                      (println "Rendering " (. c getWidth) "x" (. c getHeight) " image")
                      (let [w (. c (getWidth))
                            h (. c (getHeight))
                            img (render-function f w h)]
                        (. g drawImage img 0 0 c)))))))

(defn show-function [f]
  (-> (frame :on-close :dispose
             :visible? true
             :content (function-canvas f))
      pack!
      show!))
