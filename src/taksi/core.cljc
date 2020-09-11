(ns taksi.core
  "Async monad that is actually a monad and does not run implicitly."
  (:refer-clojure :exclude [map]))

(declare fork)

(defprotocol Fork
  (-fork [self reject resolve]))

(deftype Task [fork-self]
  Fork
  (-fork [_ reject resolve] (fork-self reject resolve)))

(def task
  "Create a new task from a `(fn [reject resolve] ... cancel-thunk)`.
  `cancel-thunk` must be an idempotent function of zero arguments."
  ->Task)

(defn resolved
  "Create a task that will immediately succeed with `v`."
  [v]
  (Task. (fn [_ resolve] (resolve v) (fn [] nil))))

(defn rejected
  "Create a task that will immediately fail with `err`."
  [err]
  (Task. (fn [reject _] (reject err) (fn [] nil))))

(defn map
  "Map `f` over the resolution value of the task `t`."
  [f t]
  (Task. (fn [reject resolve] (fork reject (comp resolve f) t))))

(defn then
  "Create a task that will fork `t`
  and then fork the result of calling `f` on its resolution value."
  [t f]
  (Task. (fn [reject resolve]
           (let [cancel (volatile! nil)
                 resolve* (fn [v] (vreset! cancel (fork reject resolve (f v))))]
             (vreset! cancel (fork reject resolve* t))
             (fn [] (@cancel))))))

(defn- cas!
  "Compare-and-swap; iff `@atom` is `old`, [[reset!]] it to `new`. Return `@atom`."
  [atom old new]
  (let [res (volatile! nil)]
    (swap! atom (fn [v]
                  (vreset! res v)
                  (if (identical? v old) new v)))
    @res))

;; FIXME: If we are to complect Error monad this should ignore rejections
;;        until the last one, the one that leaves no task that can succeed.
;; FIXME: Not sure if all the synchronization is correct (or necessary?).
;; OPTIMIZE: Quite inefficient, portability came first.
(defn select
  "Create a task that will succeed or fail as soon as one of the tasks `ts` does with
  the same value and cancel the other `ts` at that time."
  [ts]
  (Task. (fn [reject resolve]
           (let [cancels (volatile! nil)
                 cancel-others (fn [n]
                                 (transduce (comp (map-indexed vector)
                                                  (remove (fn [[i _]] (= i n))))
                                            (completing (fn [_ [_ cancel]] (cancel)))
                                            nil @cancels))
                                                     
                 rejector (atom reject)
                 resolver (atom resolve)
                 reject-nth (fn [i] (fn [err]
                                      (when-let [do-reject (cas! rejector reject nil)]
                                        (when (cas! resolver resolve nil)
                                          (cancel-others i)
                                          (do-reject err)))))
                 resolve-nth (fn [i] (fn [v]
                                       (when-let [do-resolve (cas! resolver resolve nil)]
                                         (when (cas! rejector reject nil)
                                           (cancel-others i)
                                           (do-resolve v)))))]
             (vreset! cancels (mapv (fn [i t] (fork (reject-nth i) (resolve-nth i) t))
                                    (range)
                                    ts))
             (fn [] (run! (fn [cancel] (cancel)) @cancels))))))

(defn fork
  "Run the task `t`, calling `resolve` on success and `reject` on failure.
  Returns a thunk that can be called with no arguments to cancel the execution."
  [reject resolve t]
  (-fork t reject resolve))

