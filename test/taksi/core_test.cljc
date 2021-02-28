(ns taksi.core-test
  (:require [clojure.test :refer [deftest testing is]]
            #?(:cljs [cljs.test :refer [async]])
            [taksi.core :as t]
            [monnit.core :as m]))

(defn- fork-resolving [t]
  (let [res (volatile! nil)]
    (t/fork nil (constantly nil) #(vreset! res %) t)
    @res))

(defn- fork-rejected [t]
  (let [res (volatile! nil)]
    (t/fork nil #(vreset! res %) (constantly nil) t)
    @res))

(deftest test-resolved
  (let [t (t/resolved 5)]
    (testing "resolved"
      (is (= true (t/task? t)))
      (is (= 5 (fork-resolving t))))

    (testing "fmap 1"
      (let [t (m/fmap inc t)]
        (is (= true (t/task? t)))
        (is (= 6 (fork-resolving t)))))

    (doseq [n (range 2 10)]
      (testing (str "fmap " n)
        (let [t (apply m/fmap + (repeat n t))]
          (is (= true (t/task? t)))
          (is (= (* n 5) (fork-resolving t)))

          (doseq [m (range 2 10)]
            (testing (str "nested fmap " m)
              (let [t (apply m/fmap + (repeat m t))]
                (is (= true (t/task? t)))
                (is (= (* n m 5) (fork-resolving t))))))

          (testing "nested bind"
            (let [t (m/bind t (comp t/resolved inc))]
              (is (= true (t/task? t)))
              (is (= (inc (* n 5)) (fork-resolving t))))))))

    (testing "bind"
      (let [t (m/bind t (comp t/resolved inc))]
        (is (= true (t/task? t)))
        (is (= 6 (fork-resolving t)))))))

(deftest test-rejected
  (let [t (t/rejected "NO.")]
    (testing "rejected"
      (is (= true (t/task? t)))
      (is (= "NO." (fork-rejected t))))

    (testing "fmap 1"
      (let [t (m/fmap inc t)]
        (is (= true (t/task? t)))
        (is (= "NO." (fork-rejected t)))))

    (doseq [n (range 2 10)]
      (testing (str "fmap " n)
        (let [t (apply m/fmap + (repeat n t))]
          (is (= true (t/task? t)))
          (is (= "NO." (fork-rejected t))))))

    (testing "bind"
      (let [t (m/bind t (comp t/resolved inc))]
        (is (= true (t/task? t)))
        (is (= "NO." (fork-rejected t)))))))

(deftest test-pure
  (let [t (t/pure 5)]
    (testing "t/pure"
      (is (= true (t/task? t)))
      (is (= 5 (fork-resolving t)))))

  (let [t (m/pure t/Task 5)]
    (testing "m/pure"
      (is (= true (t/task? t)))
      (is (= 5 (fork-resolving t))))))

(deftest test-non-tasks
  (is (= false (t/task? "foo")))
  (is (= false (t/task? nil))))

#?(:cljs
  (deftest test-from-promise
    (async done
      (let [get-promise (fn [] (.resolve js/Promise 5))
            t (m/bind (t/promise->task get-promise)
                      #(do (is (= % 5)) (done)))]
        (is (= true (t/task? t)))
        (fork-resolving t)))))

(deftest test-context
  (let [t t/get-context]
    (is (= true (t/task? t)))
    (is (= "con texto" (t/fork "con texto" (constantly nil) identity t)))

    (testing "fmap"
      (is (= 6 (t/fork 5 (constantly nil) identity (m/fmap inc t)))))))

