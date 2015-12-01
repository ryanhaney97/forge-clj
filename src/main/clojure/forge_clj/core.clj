(ns forge-clj.core
  "Includes all of the core functionality for forge-clj, such as defmod,
  and general-purpose macros used by other namespaces in forge-clj."
  (:require
   [clojure.string :as string]
   [clojure.set :as cset]
   [clojure.tools.nrepl.server :refer [start-server]])
  (:import
   [net.minecraft.block Block]
   [net.minecraft.item Item]
   [net.minecraftforge.common MinecraftForge]
   [cpw.mods.fml.common.registry GameRegistry]
   [cpw.mods.fml.common Mod Mod$EventHandler FMLCommonHandler IWorldGenerator]
   [cpw.mods.fml.common.event FMLPreInitializationEvent FMLInitializationEvent FMLPostInitializationEvent]))

(declare client?)

(defn gen-method
  "Given a key word, returns a java method as a symbol by capitalizing all but the first word."
  [k]
  (let [key-name (name k)
        words (string/split key-name #"-")
        method-name (apply str (first words) (map string/capitalize (rest words)))]
    (symbol method-name)))

(defn gen-setter
  "Given a key word, returns a setter java method as a symbol by adding a prefix,
  and capitalizing the remaining words."
  [k]
  (symbol (str "." (gen-method (str "set-" (name k))))))

(defn gen-classname
  "Given a symbol, returns a symbol representing a class name for java by capitalizing all words."
  [s]
  (let [s (str s)
        words (string/split s #"-")
        class-name (apply str (map string/capitalize words))]
    (symbol class-name)))

(defn get-fullname [name-ns class-name]
  (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name))))

(defmacro with-prefix [prefix & defs]
  (let [per-def (fn [possible-def]
                  (if (or (= (first possible-def) 'def) (= (first possible-def) 'defn) (= (first possible-def) 'def-) (= (first possible-def) 'defn-) (= (first possible-def) `def) (= (first possible-def) `defn) (= (first possible-def) `def-) (= (first possible-def) `defn-))
                    (let [first-val (first possible-def)
                          def-name (second possible-def)
                          def-name (symbol (str prefix def-name))
                          def-statement (cons first-val (cons def-name (rest (rest possible-def))))]
                      def-statement)
                    possible-def))
        def-statements (cons `do (map per-def defs))]
    def-statements))

(defmacro defmod
  "MACRO: Takes the current user namespace, the name of the mod, the version, and a rest argument evaled as a map.
  Using these things, constructs a class for the mod, with the proper annotations and such.
  Proxies can optionally be included via the rest argument, with :common and :client.
  When including a proxy, DO NOT REQUIRE THE NAMESPACE, and instead use the FULL NAMESPACE NAME.
  This will allow forge-clj to include the client namespaces only if run on an integrated client."
  [name-ns mod-name version & proxies]
  (let [proxies (apply hash-map proxies)
        commonproxy (get proxies :common {})
        clientproxy (get proxies :client {})
        repl (get proxies :repl)
        repl (if (true? repl)
               7888
               repl)
        client-ns (when clientproxy
                    (get clientproxy :pre-init (get clientproxy :init (get clientproxy :post-init nil))))
        client-ns (if client-ns (first (string/split (str client-ns) #"/")) nil)
        common-ns (when commonproxy
                    (get commonproxy :pre-init (get commonproxy :init (get commonproxy :post-init nil))))
        common-ns (if common-ns (first (string/split (str common-ns) #"/")) nil)
        prefix (str mod-name "-")
        fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname mod-name)))]
    `(do
       (gen-class
        :name ~(with-meta fullname `{Mod {:name ~(str (gen-classname mod-name)) :modid ~(str mod-name) :version ~(str version)}})
        :prefix ~(symbol prefix)
        :methods [[~(with-meta 'preInit `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLPreInitializationEvent] ~'void]
                  [~(with-meta 'init `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLInitializationEvent] ~'void]
                  [~(with-meta 'postInit `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLPostInitializationEvent] ~'void]])
       (with-prefix ~prefix
         (defn ~'preInit [~'this ~'event]
           ~(when repl
              `(defonce ~'repl-server (start-server :port ~repl)))
           ~(when common-ns
              `(require (symbol ~common-ns)))
           ~(when (:pre-init commonproxy)
              `(~(:pre-init commonproxy) ~'this ~'event))
           (if client?
             (do
               ~(when client-ns
                  `(require (symbol ~client-ns)))
               ~(when (:pre-init clientproxy)
                  `(~(:pre-init clientproxy) ~'this ~'event)))))

         (defn ~'init [~'this ~'event]
           ~(when (:init commonproxy)
              `(~(:init commonproxy) ~'this ~'event))
           (if client?
             ~(when (:init clientproxy)
                `(~(:init clientproxy) ~'this ~'event))))

         (defn ~'postInit [~'this ~'event]
           ~(when (:post-init commonproxy)
              `(~(:post-init commonproxy) ~'this ~'event))
           (if client?
             ~(when (:post-init clientproxy)
                `(~(:post-init clientproxy) ~'this ~'event))))))))

(defmacro genobj
  "MACRO: General purpose macro used to extend objects. Takes the superclass, constructor arguments (as a vector), the name,
  and the data (as a map), to create an instance of an anonymous class that extends the provided superclass.
  The data given will be converted to a java setter that will be called on the resulting object
  (for example, :block-name \"name\" becomes .setBlockName(\"name\")).
  Multiple arguments can be specified using a vector.

  This occurs unless one of the following special keywords is used:

  :override - should contain a map of method-names (as keywords, such as :set-block, which results in .setBlock) and functions to override them.
  :interfaces - makes the object implement the provided interfaces, contained in a vector. Interface methods must be overriden via :override.
  :calls - map of calls on the final object. Differs from normal since you can specify methods without the word set in front of them."
  [superclass constructor-args objdata]
  (let [overrides (:override objdata)
        override-methods (when overrides (map gen-method (keys overrides)))
        override-calls (if overrides (map #(list apply %1 'args) (vals overrides)))
        override-calls (if overrides (map #(list %1 ['& 'args] %2) override-methods override-calls))
        interfaces (:interfaces objdata)
        method-calls (:calls objdata)
        objdata (dissoc objdata :override :interfaces :calls)
        setters (map gen-setter (keys objdata))
        calls (map #(if (vector? %2)
                      (apply list %1 %2)
                      (list %1 %2)) setters (vals objdata))
        method-calls (map #(if (vector? (val %1))
                             (apply list (symbol (str "." (gen-method (key %1)))) (val %1))
                             (list (symbol (str "." (gen-method (key %1)))) (val %1))) method-calls)
        super-vector (if interfaces (concat [superclass] interfaces) [superclass])]
    (if overrides
      `(doto (proxy ~super-vector ~constructor-args
               ~@override-calls)
         ~@calls
         ~@method-calls)
      `(doto (proxy ~super-vector ~constructor-args)
         ~@calls
         ~@method-calls))))

(defmacro defobj
  "MACRO: same as genobj, but takes a name (as the third argument), and stores the resulting instance in a def with that name.
  Other macros using this will have DEFOBJ: in their docs, instead of MACRO:.
  Realize that those using defobj are indeed macros as well."
  [superclass constructor-args obj-name objdata]
  `(def ~obj-name (genobj ~superclass ~constructor-args ~objdata)))

(defmulti register
  "Multimethod consisting of a series of register functions. Handles the following arguments:
  (block-object, name): registers block with specified name.
  (item-object, name): registers item with specified name.
  (block-object, name, blockitem): registers block with specified name and blockitem object.
  (world-generator): registers a world generator.
  (world-generator, priority): registers a world generator with the specified priority."
  (fn [element & args] [(count args) (type element)]))
(defmethod register [1 Block] [element forge-name]
  (GameRegistry/registerBlock element forge-name)
  element)
(defmethod register [1 Item] [element forge-name]
  (GameRegistry/registerItem element forge-name)
  element)
(defmethod register [2 Block] [element forge-name blockitem]
  (GameRegistry/registerBlock element nil forge-name (object-array []))
  (GameRegistry/registerItem blockitem forge-name)
  element)
(defmethod register [0 IWorldGenerator] [generator]
  (register generator 0))
(defmethod register [1 IWorldGenerator] [generator mod-priority]
  (GameRegistry/registerWorldGenerator generator mod-priority))

(defn register-tile-entity
  "Registers a Tile Entity with the specified id"
  [entity id]
  (GameRegistry/registerTileEntity entity id))

(defn register-events
  "Registers an Event Handler."
  [handler]
  (.register (.bus (FMLCommonHandler/instance)) handler)
  (.register MinecraftForge/EVENT_BUS handler))

(defmacro defclass
  ([superclass name-ns class-name classdata]
   (let [classdata (cset/rename-keys classdata {:expose-fields :exposes
                                                :expose :exposes-methods
                                                :expose-methods :exposes-methods
                                                :interfaces :implements})
         classdata (select-keys classdata [:implements :exposes-methods :constructors :exposes :init :state :post-init :methods :factory :impl-ns :load-impl-ns])
         classdata (reduce concat [] (into [] classdata))
         prefix (str class-name "-")
         fullname (get-fullname name-ns class-name)]
     `(do
        (gen-class
         :name ~fullname
         :prefix ~prefix
         :extends ~superclass
         ~@classdata)
        (def ~class-name ~fullname)
        (import ~fullname))))
  ([name-ns class-name classdata]
   `(defclass nil ~name-ns ~class-name ~classdata)))

(defmacro defassocclass
  "DEFCLASS: Creates a class with the specified superclass (optional), namespace name, class name, and classdata.
  This class implements ITransientAssociative, and as such classes created with this can be treated similarly to a hash-map.
  For example, data can be obtained via (:keyword class-instance), or (get class-instance :keyword).
  Data can also be changed via the assoc! function. Remember to include the exclaimation point!
  Other macros using this will have DEFASSOCCLASS: in their docs instead of MACRO:.
  This also means that those labeled as using this macro are also macros."
  ([superclass name-ns class-name classdata]
   (let [classdata (assoc classdata :interfaces (conj (get classdata :interfaces []) `clojure.lang.ITransientAssociative)
                     :init 'initialize
                     :state 'data)
         fields (get classdata :fields {})
         prefix (str class-name "-")
         fullname (get-fullname name-ns class-name)
         this-sym (with-meta 'this {:tag fullname})]
     `(do
        (defclass ~superclass ~name-ns ~class-name ~classdata)
        (with-prefix ~prefix
          (defn ~'initialize []
            [[] (atom ~fields)])
          (defn ~'assoc [~'this ~'obj-key ~'obj-val]
            (swap! (~'.-data ~this-sym) assoc ~'obj-key ~'obj-val)
            ~'this)
          (defn ~'conj [~'this ~'obj]
            (swap! (~'.-data ~this-sym) conj ~'obj)
            ~'this)
          (defn ~'persistent [~'this]
            (deref (~'.-data ~this-sym)))
          (defn ~'valAt
            ([~'this ~'obj]
             (get (deref (~'.-data ~this-sym)) ~'obj))
            ([~'this ~'obj ~'notfound]
             (get (deref (~'.-data ~this-sym)) ~'obj ~'notfound)))))))
  ([name-ns class-name classdata]
   `(defassocclass nil ~name-ns ~class-name ~classdata)))

;Mod declaration by forge-clj
;----------------------------

(gen-class
 :name ^{Mod {:name "ForgeClj" :modid "forge-clj" :version "0.5.0"}} forge_clj.core.ForgeClj
 :prefix "forge-clj-"
 :methods [[^{Mod$EventHandler []} preInit [cpw.mods.fml.common.event.FMLPreInitializationEvent] void]
           [^{Mod$EventHandler []} init [cpw.mods.fml.common.event.FMLInitializationEvent] void]
           [^{Mod$EventHandler []} postInit [cpw.mods.fml.common.event.FMLPostInitializationEvent] void]])

(with-prefix forge-clj-
  (defn preInit [this event]
    (def client? (.isClient (.getSide (FMLCommonHandler/instance)))))
  (defn init [this event])
  (defn postInit [this event]))
