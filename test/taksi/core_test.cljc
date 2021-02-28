(ns taksi.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [taksi.core :as t]
            [monnit.core :as m]))

(defn- fork-resolving [t] (t/fork (constantly nil) identity t))

(defn- fork-rejected [t] (t/fork identity (constantly nil) t))

(deftest test-resolved
  (let [t (t/resolved 5)]
    (testing "resolved"
      (is (t/task? t))
      (is (= (fork-resolving t) 5)))

    (testing "fmap 1"
      (let [t (m/fmap inc t)]
        (is (t/task? t))
        (is (= (fork-resolving t) 6))))

    (doseq [n (range 2 10)]
      (testing (str "fmap " n)
        (let [t (apply m/fmap + (repeat n t))]
          (is (t/task? t))
          (is (= (fork-resolving t) (* n 5)))

          (doseq [m (range 2 10)]
            (testing (str "nested fmap " m)
              (let [t (apply m/fmap + (repeat m t))]
                (is (t/task? t))
                (is (= (fork-resolving t) (* n m 5))))))

          (testing "nested bind"
            (let [t (m/bind t (comp t/resolved inc))]
              (is (t/task? t))
              (is (= (fork-resolving t) (inc (* n 5)))))))))

    (testing "bind"
      (let [t (m/bind t (comp t/resolved inc))]
        (is (t/task? t))
        (is (= (fork-resolving t) 6))))))

(deftest test-rejected
  (let [t (t/rejected "NO.")]
    (testing "rejected"
      (is (t/task? t))
      (is (= (fork-rejected t) "NO.")))

    (testing "fmap 1"
      (let [t (m/fmap inc t)]
        (is (t/task? t))
        (is (= (fork-rejected t) "NO."))))

    (doseq [n (range 2 10)]
      (testing (str "fmap " n)
        (let [t (apply m/fmap + (repeat n t))]
          (is (t/task? t))
          (is (= (fork-rejected t) "NO.")))))

    (testing "bind"
      (let [t (m/bind t (comp t/resolved inc))]
        (is (t/task? t))
        (is (= (fork-rejected t) "NO."))))))

(deftest test-pure
  (let [t (t/pure 5)]
    (testing "t/pure"
      (is (t/task? t))
      (is (= (fork-resolving t) 5))))

  (let [t (m/pure t/Task 5)]
    (testing "m/pure"
      (is (t/task? t))
      (is (= (fork-resolving t) 5)))))

(deftest test-non-tasks
  (is (not (t/task? "foo")))
  (is (not (t/task? nil))))

