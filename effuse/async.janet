(use ./init)

(def go-effect (effect :go))
(def {:go go*} go-effect)

(defmacro go
  [& body]
  ~(,go* (fn [] ,;body)))

(defn- cancel-all [chan fibers reason]
  (each f fibers (ev/cancel f reason))
  (while (not (empty? fibers))
    (def [_ fiber] (ev/take chan))
    (put fibers fiber nil)))

(defn- wait-for-fibers
  [chan fibers]
  (while (not (empty? fibers))
    (def [sig fiber] (ev/take chan))
    (if (= sig :ok)
      (put fibers fiber nil)
      (do
        (cancel-all chan fibers "sibling canceled")
        (propagate (fiber/last-value fiber) fiber)))))

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

  (with-in
    [go-handler]

    (defer (cancel-all chan fibers "parent canceled")
      (def result (action))
      (wait-for-fibers chan fibers)
      result)))
