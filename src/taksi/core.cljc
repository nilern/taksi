(ns taksi.core
  "Async monad that is actually a monad and does not run implicitly."
  #?(:cljs (:require-macros [taksi.macros :refer [deftasktype]]))
  (:require [monnit.core :as m]
            #?(:clj [taksi.macros :refer [deftasktype]])))

(defprotocol Task
  (task? [self])
  (-fork [self reject resolve]))

(extend-protocol Task
  #?(:clj Object, :cljs default)
  (task? [_] false)
  (-fork [self _ _]
    (assert false (str "-fork called on non-Task value " self)))

  nil
  (task? [_] false)
  (-fork [self _] (assert false "-fork called on nil")))

(declare ->FMap1 ->FMap2 ->FMap3 ->FMap4 ->FMapN ->Bind)

(deftasktype Bind [mv f]
  Task
  (task? [_] true)
  (-fork [_ reject resolve]
    (-fork mv reject (fn [a] (-fork (f a) reject resolve)))))

(deftasktype FMap1 [f a]
  Task
  (task? [_] true)
  (-fork [_ reject resolve]
    (-fork a reject (fn [a] (resolve (f a))))))

(deftasktype FMap2 [f a b]
  Task
  (task? [_] true)
  (-fork [_ reject resolve]
    (-fork a reject
           (fn [a] (-fork b reject
                          (fn [b] (resolve (f a b))))))))

(deftasktype FMap3 [f a b c]
  Task
  (task? [_] true)
  (-fork [_ reject resolve]
    (-fork a reject
           (fn [a] (-fork b reject
                          (fn [b]
                            (-fork c reject
                                   (fn [c] (resolve (f a b c))))))))))

(deftasktype FMap4 [f a b c d]
  Task
  (task? [_] true)
  (-fork [_ reject resolve]
    (-fork a reject
           (fn [a] (-fork b reject
                          (fn [b]
                            (-fork c reject
                                   (fn [c]
                                     (-fork d reject
                                            (fn [d] (resolve (f a b c d))))))))))))

(deftasktype FMapN [f a b c d args]
  Task
  (task? [_] true)
  (-fork [_ reject resolve]
    (-fork a reject
           (fn [a] (-fork b reject
                          (fn [b]
                            (-fork c reject
                                   (fn [c]
                                     (-fork d reject
                                            (fn [d]
                                              (-fork (apply m/fmap
                                                            (fn [& args] (apply f a b c d args))
                                                            args)
                                                     reject resolve)))))))))))

(deftype Rejected [e]
  Task
  (task? [_] true)
  (-fork [_ reject _] (reject e))

  m/Functor
  (-fmap [self _] self)
  (-fmap [self _ _] self)
  (-fmap [self _ _ _] self)
  (-fmap [self _ _ _ _] self)
  (-fmap [self _ _ _ _ _] self)

  m/Monad
  (bind [self _] self))

(def rejected ->Rejected)

(deftype Resolved [v]
  Task
  (task? [_] true)
  (-fork [_ _ resolve] (resolve v))

  m/Functor
  (-fmap [_ f] (Resolved. (f v)))
  (-fmap [self f b] (->FMap2 f self b))
  (-fmap [self f b c] (->FMap3 f self b c))
  (-fmap [self f b c d] (->FMap4 f self b c d))
  (-fmap [self f b c d args] (->FMapN f self b c d args))

  m/Monad
  (bind [_ f] (f v)))

(def resolved ->Resolved)

(def pure resolved)

(defmethod m/pure Task [_ v] (Resolved. v))

(defn fork [reject resolve t] (-fork t reject resolve))

