(use spork/test)
(use ../effuse)
(use ../effuse/async)

(start-suite :async)

(defn- hang []
  (ev/take (ev/chan)))

(do
  :straight-through

  (def captured @[])
  (run
   (with-in
     [goscope]

     (go
      (array/push captured :alfa))
     (array/push captured :bravo))
   (array/push captured :charlie))

  (assert (deep= @[:bravo :alfa :charlie] captured)))

(do
  :sleep-to-yield

  (def captured @[])
  (run
   (with-in
     [goscope]

     (go (array/push captured :alfa))
     (ev/sleep 0)
     (array/push captured :bravo))
   (array/push captured :charlie))

  (assert (deep= @[:alfa :bravo :charlie] captured)))

(do
  :go-nested

  (def captured @[])
  (run
   (with-in
     [goscope]

     (go (go (go (array/push captured :alfa))))))

  (assert (deep= @[:alfa] captured)))

(do
  :goscope-result

  (assert (= 123 (run (with-in [goscope] 123)))))

(do
  :goscope-nested

  (def captured @[])
  (run
   (with-in [goscope]
     (go
      (ev/sleep 0)
      (array/push captured :alfa))

     (with-in [goscope]
       (go (array/push captured :bravo)))

     (go (array/push captured :charlie))))

  (assert (deep= @[:bravo :alfa :charlie] captured)))

(do
  :cancel-multiple

  (def captured @[])
  (def ch (ev/chan))


  (assert
   (= [false "canceled"]
      (protect
       (run
        (with-in
          [goscope]

          (go (defer (array/push captured :alfa)) (ev/take ch))
          (go (defer (array/push captured :bravo)) (ev/take ch))

          (ev/sleep 0)
          (error "canceled"))))))

  (sort captured)
  (assert (deep= @[:alfa :bravo] captured)))


(do
  :cancellation

  (def captured @[])
  (assert
   (= [false "canceled"]
      (protect
       (run
        (with-in
          [goscope]

          (array/push captured :alfa)
          (go (array/push captured :bravo))
          (error "canceled")
          (array/push captured :charlie))))))

  (assert (deep= @[:alfa] captured)))

(do
 :cancel-no-wait

 (def captured @[])

 (assert
  (=
   [false "canceled"]
   (protect
    (run
     (with-in
       [goscope]
       (go (array/push captured :NO-alfa))
       (error "canceled"))))))
 (assert (deep= @[] captured)))

(do
  :cancellation-no-peers
  (def captured @[])

  (assert
   (= [false "canceled"]
      (protect
       (run
        (with-in
          [goscope]

          (error "canceled")
          (array/push captured :alfa))))))

  (assert (deep= @[] captured)))

(do
  :cancellation-nested

  (def captured @[])
  (assert
   (= [false "canceled"]
      (protect
       (run
        (defer (array/push captured :alfa)
          (with-in [goscope]
            (defer (array/push captured :bravo)
              (go
               (with-in [goscope]
                 (defer (array/push captured :charlie)
                   (hang)))))

            (ev/sleep 0)
            (error "canceled")))))))

  (assert (deep= @[:bravo :charlie :alfa] captured)))

(do
  :cancel-scope-effect

  (def captured @[])

  (def [_ {:eff-a eff-a}] (effect :eff-a))

  (assert
   (= [false "canceled"]
      (protect
       (run
        (with-in [goscope]
          (go
           (defer (array/push captured :bravo)
             (with-in
               [(handler
                 (fn eff-a []
                   (defer (array/push captured :alfa)
                     (hang))))]
               (array/push captured :charlie)
               (: eff-a)
               (array/push captured :delta))))

          (ev/sleep 0)
          (error "canceled"))))))

  (assert (deep= @[:charlie :alfa :bravo] captured)))

(do
   :cancel-higher-scope-effect

   (def captured @[])
   (def [_ {:eff-a eff-a}] (effect :eff-a))

   (protect
    (run
      (with-in
       [(handler
         (fn eff-a []
           (defer (array/push captured :alfa)
             (hang))))
        goscope]

       (go
        (defer (array/push captured :bravo)
          (array/push captured :charlie)
          (: eff-a)
          (array/push captured :delta)))

       (ev/sleep 0)
       (error "canceled"))))

   (assert (deep= @[:charlie :alfa :bravo] captured)))

(end-suite)
