(use spork/test)
(use ../effuse/init)

(start-suite :core)

(do
  :simple-run

  (assert (= 123 (run 123))))

(do
  :incomplete-effect-definition

  (def {:get state/get :put state/put} (effect :state [[:get 0] [:put 1]]))

  (assert
   (=
    [false "handler must implement all operations for 'state'.  missing state/put"]
    (protect (handler (fn state/get []))))))

(do
  :handler-visibility

  (def {:foo foo} (effect :foo))
  (def captured @[])

  (run
   (with-in
     [(handler
       (fn foo [arg]
         (array/push captured :outer)
         (array/push captured arg)))]
     (array/push captured :before)
     (with-in
       [(handler
         (fn foo [arg]
           (array/push captured :inner)
           (foo arg)))]
       (foo :bar))))

  (assert (deep= @[:before :inner :outer :bar] captured)))

(do
  :handler-as-value

  (def captured @[])

  (def {:foo foo} (effect :foo))
  (def capture-handler (handler (fn foo [s] (array/push captured s))))

  (run
   (with-in
    [capture-handler]
    (foo :foo)))

  (assert (deep= @[:foo] captured)))

(do
  :handler-not-found

  (def {:foo foo} (effect :foo))
  (assert (= [false "no handler found for effect: foo"]
             (protect (run (foo))))))

(do
   :ctl-no-resume

   (def captured @[])
   (def {:skip skip} (effect :skip))

   (def result
     (run
      (with-in
        [(handler (ctl skip [] 1234))] # NOTE: no (resume)

        (array/push captured :alfa)
        (skip)
        (array/push captured :bravo))))
   (assert (= 1234 result))
   (assert (deep= @[:alfa] captured)))

(do
  :ctl-defer-no-resume

  (def captured @[])
  (def {:skip skip} (effect :skip))

  (def result
    (run
     (with-in
       [(handler (ctl skip [] 1234))] # NOTE: no (resume)
       (defer (array/push captured :alfa)
         (array/push captured :bravo)
         (skip)
         (array/push captured :charlie)))))
  (assert (= 1234 result))
  (assert (deep= @[:bravo :alfa] captured)))

(do
   :raw-ctl-abort

   (def captured @[])
   (def {:explode explode} (effect :explode))

   (assert
    (= [false :exploded]
       (protect
        (run
         (defer (array/push captured :alfa)
           (with-in
             [(handler (raw explode []
                            (error :exploded)
                            (array/push captured :delta)))]

             (array/push captured :bravo)
             (explode)
             (array/push captured :charlie)
             nil))
         :exploded))))
   (assert (deep= @[:bravo :alfa] captured)))

(do
  :raw-ctl-abort-nested

  (def captured @[])
  (def {:explode explode} (effect :explode))
  (def {:nested nested} (effect :nested))

  (assert
   (= [false :exploded]
      (protect
       (run
        (defer (array/push captured :alfa)
          (with-in
            [(handler (raw explode []
                           (error :exploded)))
             (handler (fn nested []
                        (defer (array/push captured :bravo)
                          (explode))))]

            (array/push captured :charlie)
            (nested)
            (array/push captured :delta)
            nil))))))
  (assert (deep= @[:charlie :bravo :alfa] captured)))

(do
  :tail-resume

  (def captured @[])

  (def {:foo foo} (effect :foo))

  (run
   (with-in
     [(handler
       (ctl foo []
            (resume :alfa)))]
     (array/push captured (foo))))

  (assert (deep= @[:alfa] captured)))

(do
  :post-resume

  (def captured @[])

  (def {:foo foo} (effect :foo))

  (run
   (with-in
     [(handler
       (ctl foo []
            (resume :alfa)
            (array/push captured :bravo)))]
     (array/push captured (foo))))

  (assert (deep= @[:alfa :bravo] captured)))

(do
  :mask

  (def captured @[])
  (def a-effect (effect :a))
  (def {:a a} a-effect)

  (run
   (with-in
     [(handler
       (fn a []
         (array/push captured :outer)))
      (handler
       (fn a []
         (array/push captured :inner)))]
     (a)
     (with-in
       [(mask a-effect)]
       (a))))

  (assert (deep= @[:inner :outer] captured)))

(do
  :mask-behind

  (def captured @[])
  (def a-effect (effect :a))
  (def {:a a} a-effect)
  (def {:b b} (effect :b))

  (run
   (with-in
    [(handler
      (fn a []
        (array/push captured :outer)))
     (handler
      (fn b []
        (a)))
     (handler
      (fn a []
        (array/push captured :inner)))
     (mask-behind a-effect)]
    (b)))

  (assert (deep= @[:inner] captured)))


(do
  :ctl-handler-errors-preserve-callstack

  (def {:foo foo} (effect :foo))
  (def {:bar bar} (effect :bar))

  (defn some-fn
    []
    (with-in
      [(handler (ctl foo [] (error :bonk)))
       (handler (ctl bar []
                     (foo)
                     # prevent TCO to preserve function names
                     nil))]
      (bar))
    # prevent TCO to preserve function names
    nil)


  (def fiber (fiber/new run* :e))
  (def err (resume fiber some-fn))

  (def buf @"")
  (with-dyns [*err* buf]
    (debug/stacktrace fiber err))

  (def stacktrace (string buf))
  (assert (string/find "in foo*handler*" stacktrace))
  (assert (string/find "in bar*handler*" stacktrace))
  (assert (string/find "in some-fn" stacktrace)))

(do
  :ret

  (assert
   (= 2
      (run
       (with-in
         [(handler (return [x] (inc x)))]
         1)))))

(end-suite)
