(import effuse/internal :as internal)

(defdyn *evv*
  :private)

(defn- require-evv
  []
  (let [evv (dyn *evv*)]
    (unless evv (error "not in an effuse context"))
    evv))

# FIXME: better name.  this is a stack of operation fibers that resumed, but
# still need to finish running their body.
(defdyn *pending*
  :private)

(defn- require-pending
  []
  (let [pending (dyn *pending*)]
    (unless pending (error "not under any handler"))
    pending))

(defn op/effect
  "Get the effect an operation is part of."
  [op]
  (internal/op/effect op))

(defn op/name
  "Get the name of an operation."
  [op]
  (internal/op/name op))

(defn effect/name
  "Get the name of an effect."
  [eff]
  (internal/effect/name eff))

(defn- rfind-index
  [pred ind &opt start]

  (default start (dec (length ind)))

  (var ret nil)
  (var found? false)
  (loop [i :down-to [start 0] :until found?]
    (when (pred (in ind i))
      (set found? true)
      (set ret i)))
  ret)

(defn- get-effect-handler-index
  {:ty (fn [Effect] [EffectHandler int])}
  [eff &opt op]

  (default op (first (values eff)))

  (let [evv (require-evv)]
    (def index (rfind-index |(in $ op) evv))
    (unless index
      (errorf "no handler found for effect: %s" (effect/name eff)))
    index))

(defn- get-op-handler
  {:ty (fn [Operation] [[HandlerKind Function] int])}
  [op]

  (let [index (get-effect-handler-index (op/effect op) op)
        evv (require-evv)
        eff-hnd (in evv index)]
    [eff-hnd (in eff-hnd op) index]))

(defn- invoke
  [op & args]

  (internal/op/check-arity op (length args))

  (let [[eff-hnd [kind hnd] index] (get-op-handler op)]
    (if (= kind :fun)
      (with-dyns [*evv* (tuple/slice (require-evv) 0 index)]
        (hnd ;args))
      (let [evv (require-evv)]
        (match (yield [:effuse/perform kind hnd (tuple/slice evv 0 index) args])
          [:effuse/abort fib err] (propagate err fib)
          [:effuse/finalize _ value] (signal 0 [eff-hnd index value])
          [:effuse/resume fib value]
          (do
            # ctl/raw handlers may have (resume) in tail position, but there is
            # no way to know that, so unconditionally push fib into *pending*.
            (array/push (require-pending) fib)
            value)
          x (errorf "internal error: malformed operation result: %q" x))))))

(defn effect
  [name &opt opspec]

  (default opspec [[name -1 -1]])

  (internal/effect name
                   (->> opspec
                        (map (fn [[name min-arity max-arity]]
                               (default min-arity -1)
                               (default max-arity min-arity)

                               [name min-arity max-arity]))
                        (tuple/slice))
                   invoke))

(defn handler*
  [spec]

  # TODO: Validate that the values of `spec` are [HandlerKind, Function] tuples.

  (def effects @{})
  (eachk k spec
    (match k
      :ret nil
      op
      (if-let [eff (op/effect op)]
        (put effects eff eff)
        (errorf "handler key must be effect operation or :ret.  got %q" k))))

  (when (> (length effects) 1)
    (let [effect-names (map effect/name (values effects))]
      (sort effect-names)
      (errorf "handler must only implement operations from a single effect. found %s"
              (string/join effect-names ", "))))

  (def eff (first (keys effects)))
  (when eff
    (def missing-ops @[])

    (each op eff
      (unless (in spec op)
        (array/push missing-ops op)))

    (unless (empty? missing-ops)
      (let [missing-names (map string missing-ops)]
        (errorf "handler must implement all operations for '%s'.  missing %s"
                (effect/name eff)
                (string/join missing-names ", ")))))

  (fn hnd [action]
    # This is a fused version of the functionality present in with-dyns,
    # defer, and prompt.

    (def evv (require-evv))
    (def pending @[])

    (defn thunk []
      (setdyn *evv* [;evv spec])
      (setdyn *pending* pending)

      (def ret (in spec :ret identity))
      (ret (action)))

    (def fiber (fiber/new thunk :pt))
    (def result (resume fiber))

    (while (not (empty? pending))
      (let [fib (array/pop pending)]
        (while true
          (resume fib)
          (if (= (fiber/status fib) :dead)
            (break)
            (propagate (fiber/last-value fib) fib)))))

    (case (fiber/status fiber)
      :dead result
      :user0 (match result
               [(@ spec) (length evv) payload] payload
               _ (propagate result fiber))
      (propagate result fiber))))

(def- errs/effect-handler-format "invalid effect handler format")

(defn- rewrite-handler-fn [spec]
  (unless (and (tuple? spec)
               (= (tuple/type spec) :parens))
    (with-dyns [*macro-form* spec]
      (maclintf :error errs/effect-handler-format)))

  (defn check-arglist [args]
    (unless (and (tuple? args)
                 (= (tuple/type args)
                    :brackets))
      (with-dyns [*macro-form* args]
        (maclintf :error errs/effect-handler-format)))
    args)

  (defn handler-name
    [effect-form]
    (if (symbol? effect-form)
      (symbol (string/format "%q*handler*" effect-form))
      (symbol "anonymous handler")))

  (match spec
    ['return args & body]
    ~[:ret (fn return [,;(check-arglist args)] ,;body)]

    ['raw effect args & body]
    ~[,effect [:raw (fn ,(handler-name effect) [,'ctl ,;(check-arglist args)] ,;body)]]

    ['ctl effect args & body]
    ~[,effect [:ctl (fn ,(handler-name effect) [,'resume ,;(check-arglist args)] ,;body)]]

    ['fn effect args & body]
    ~[,effect [:fun (fn ,(handler-name effect) [,;(check-arglist args)] ,;body)]]

    _ (with-dyns [*macro-form* spec]
        (maclintf :error errs/effect-handler-format))))

(defn- create-handler [spec]
  (let [h @{}]
    (each f spec
      (let [[k v] (rewrite-handler-fn f)]
        (put h k v)))
    (table/to-struct h)))

(defmacro handler [& spec]
  (assert (not (empty? spec))
          "must define at least one handler")
  ~(handler* ,(create-handler spec)))

(defn mask [eff]
  (fn mask* [action]
    (def index (get-effect-handler-index eff))
    (def evv (require-evv))
    (with-dyns [*evv* [;(tuple/slice evv 0 index)
                       ;(tuple/slice evv (inc index))]]
      (action))))

(defn mask-behind [eff]
  (fn mask-behind* [action]

    (def evv (require-evv))
    (def orig-index (get-effect-handler-index eff))
    (def anyop (first (values eff)))
    (def mask-index (rfind-index |(in $ anyop) evv (dec orig-index)))
    (def dest-index (inc mask-index))

    (with-dyns [*evv* [;(tuple/slice evv 0 dest-index)
                       (in evv orig-index)
                       ;(tuple/slice evv dest-index orig-index)
                       ;(tuple/slice evv orig-index)]]
      (action))))

(defn- run-core
  [fib resume-with ancestors]

  (match (resume fib resume-with)
    [:effuse/perform kind hnd evv args]
    (do
      (defn run-hnd [args]
        (setdyn *evv* evv)

        (var ctl-called? false)

        (defn ctl [action arg]
          (when ctl-called? (error "ctl must be called exactly once"))
          (set ctl-called? true)
          (case action
            :resume (yield [:effuse/resume arg])
            :finalize (yield [:effuse/finalize arg])
            (errorf "unknown ctl action: %q" action)))

        (case kind
          :raw (let [value (hnd ctl ;args)]
                 (unless ctl-called?
                   (error "raw handlers must resume, abort, or finalize before returning"))
                 value)
          :ctl (let [resume* (fn resume [value] (ctl :resume value))
                     result (hnd resume* ;args)]
                 (unless ctl-called?
                   (ctl :finalize result))
                 result)
          (errorf "internal error: unhandled invoke kind: %q" kind)))

      (run-core (fiber/new run-hnd :iye) args (array/push ancestors fib)))

    ([tag next-val] (or (= tag :effuse/resume)
                        (= tag :effuse/finalize)))

    (run-core (array/pop ancestors) [tag fib next-val] ancestors)

    value (if (= (fiber/status fib) :error)
            (run-core (array/pop ancestors) [:effuse/abort fib value] ancestors)
            (propagate value fib))))

(defn run*
  [action]
  (with-dyns [*evv* []]
    (run-core (fiber/new action :iy) nil @[])))


(defmacro run
  [& body]
  ~(run*
     (fn [] ,;body)))


# TODO: Figure out what to call `with-in`, and whether it should be kept.  It
# was handy in Algae (named `with`), because Fennel is so spartan.  But maybe
# in Janet, the need is not as great?  At the very least, we would need
# `with-handler(s)`, but `(with-handlers [(handler ...]` stutters, and making
# `with-handlers` do the handle spec rewriting would mean non-inline handlers
# would be harder to apply.
(defmacro with-in
  [wrappers & body]

  (reduce (fn [wrapped wrapper] ~(,wrapper (fn [] ,wrapped)))
          ~(do ,;body)
          (reverse wrappers)))
