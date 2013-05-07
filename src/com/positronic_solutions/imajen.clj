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

(ns com.positronic-solutions.imajen
  (:require [incanter.core :as incanter]))

(defn floor [v]
  (bigint v))

(defn clamp [v lower-bound upper-bound]
  (-> v
      (max lower-bound)
      (min upper-bound)))

(defn clamper [lower-bound upper-bound]
  (fn [v]
    (clamp v lower-bound upper-bound)))

(defn scale
  ([s]
     (partial scale s))
  ([s v]
     (incanter/mult s v))
  ([s v & vs]
     (map (scale s) (concat [v] vs))))

(defn translate
  ([t]
     (partial translate t))
  ([t v]
     (incanter/plus t v))
  ([t v & vs]
     (map (translate t) (concat [v] vs))))

(defn magnitude [v]
  (->> (map incanter/sq v)
       (reduce +)
       (incanter/sqrt)))

(defn normalize [v]
  (incanter/div v (magnitude v)))

(defn linear-gradient [c1 c2]
  (fn [t]
    (let [w (- 1 t)]
      (incanter/plus (incanter/mult w c1)
                     (incanter/mult t c2)))))

(defn linear-interpolation [f]
  (fn 
    ([x]
       (let [x0 (floor x)
             x1 (inc x0)
             t (- x x0)
             w (- 1 t)]
         (incanter/plus (incanter/mult w (f x0))
                        (incanter/mult t (f x1)))))
    ([x & xs]
       (let [x0 (floor x)
             x1 (inc x0)
             f0 (linear-interpolation (partial f x0))
             f1 (linear-interpolation (partial f x1))
             t (- x x0)
             w (- 1 t)]
         (incanter/plus (incanter/mult w (apply f0 xs))
                        (incanter/mult t (apply f1 xs)))))))

(defn cosine-interpolation [f]
  (fn
    ([x]
       (let [x0 (floor x)
             x1 (inc x0)
             t (- x x0)
             mu (/ (- 1
                      (Math/cos (* Math/PI t)))
                   2)]
         (incanter/plus (incanter/mult (- 1 mu) (f x0))
                        (incanter/mult mu (f x1)))))
    ([x & xs]
       (let [x0 (floor x)
             x1 (inc x0)
             f0 (cosine-interpolation (partial f x0))
             f1 (cosine-interpolation (partial f x1))
             t (- x x0)
             mu (/ (- 1
                      (Math/cos (* Math/PI t)))
                   2)]
         (incanter/plus (incanter/mult (- 1 mu) (apply f0 xs))
                        (incanter/mult mu (apply f1 xs)))))))

(defn cubic-interpolation [f]
  (fn 
    ([x]
       (let [x0 (floor x)
             x1 (inc x0)
             x2 (inc x1)
             x-1 (dec x0)
             t (- x x0)
             t-squared (* t t)
             y-1 (f x-1)
             y0 (f x0)
             y1 (f x1)
             y2 (f x2)
             a0 (incanter/minus (incanter/plus y2 y0)
                                (incanter/plus y1 y-1))
             a1 (incanter/minus y-1 y0 a0)
             a2 (incanter/minus y1 y-1)
             a3 y0]
         (incanter/plus (incanter/mult a0 t t-squared)
                        (incanter/mult a1 t-squared)
                        (incanter/mult a2 t)
                        a3)))
    ([x & xs]
       (let [x0 (floor x)
             x1 (inc x0)
             x2 (inc x1)
             x-1 (dec x0)
             t (- x x0)
             t-squared (* t t)
             f-1 (cubic-interpolation (partial f x-1))
             f0 (cubic-interpolation (partial f x0))
             f1 (cubic-interpolation (partial f x1))
             f2 (cubic-interpolation (partial f x2))
             y-1 (apply f-1 xs)
             y0 (apply f0 xs)
             y1 (apply f1 xs)
             y2 (apply f2 xs)
             a0 (incanter/minus (incanter/plus y2 y0)
                                (incanter/plus y1 y-1))
             a1 (incanter/minus y-1 y0 a0)
             a2 (incanter/minus y1 y-1)
             a3 y0]
         (incanter/plus (incanter/mult a0 t t-squared)
                        (incanter/mult a1 t-squared)
                        (incanter/mult a2 t)
                        a3)))))

(defn uncurry [f]
  (fn
    ([]
       (f))
    ([x]
       (f x))
    ([x & xs]
       (apply (uncurry (f x)) xs))))

(defn tee [& fs]
  (fn [& args]
    (map apply fs (repeat args))))
