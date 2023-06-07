(use ./init)

(def go-effect (effect :go))
(def {:go go*} go-effect)

(defmacro go
  [& body]
  ~(,go* (fn [] ,;body)))

(def canceled (gensym))

(defn- cancel-all [chan fibers]
  (each f fibers (ev/cancel f canceled))
  (while (not (empty? fibers))
    (def [_ fiber] (ev/take chan))
    (put fibers fiber nil)))

(defn- wait-for-fibers
  [chan fibers]
  (while (not (empty? fibers))

    (def [sig fiber] (try (ev/take chan)
                          ([err fib]
                           (cancel-all chan fibers)
                           (propagate err fib))))
    (put fibers fiber nil)
    (when (not= sig :ok)
      (cancel-all chan fibers)
      (def err (fiber/last-value fiber))
      (when (not= err canceled)
        (propagate err fiber)))))

(defn goscope
  [action]

  (def fibers @{})
  (def chan (ev/chan))

  # Declare go-handler with a name, so it can install itself in any (go) blocks
  # that are created.  That way, nested `go` calls all target the current
  # `goscope`.  Without this setup, things running inside `(go)` would search
  # for a `go` implementation higher up in the scope, which would be very
  # confusing for users.  To schedule work into a higher goscope, it will need
  # to be passed as a named handler.
  (var go-handler nil)
  (set go-handler
       (handler
        (fn go* [f]
          (def fiber (ev/go go-handler f chan))
          (put fibers fiber fiber))))

  (var result nil)
  (def fiber (ev/go go-handler |(set result (action)) chan))
  (put fibers fiber fiber)

  (wait-for-fibers chan fibers)
  result)
