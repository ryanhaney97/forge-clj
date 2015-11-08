;Declares the namespace and all imports.
(ns forge-clj.core
  (:require
   [clojure.string :as string])
  (:import
   [net.minecraft.block Block]
   [net.minecraft.item Item]
   [net.minecraftforge.common MinecraftForge]
   [cpw.mods.fml.common.registry GameRegistry]
   [cpw.mods.fml.common Mod Mod$EventHandler FMLCommonHandler IWorldGenerator]
   [cpw.mods.fml.common.event FMLPreInitializationEvent FMLInitializationEvent FMLPostInitializationEvent]))

;Declares a final global symbol, that will be set later on as true if client or false if dedicated server.
(declare client?)

;Given a key word, returns a java method as a symbol by capitalizing all but the first word.
(defn gen-method [k]
  (let [key-name (name k)
        words (string/split key-name #"-")
        method-name (apply str (first words) (map string/capitalize (rest words)))]
    (symbol method-name)))

;Given a key word, returns a setter java method as a symbol by adding a prefix,
;and capitalizing the remaining words.
(defn gen-setter [k]
  (symbol (str "." (gen-method (str "set-" (name k))))))

;Given a symbol, returns a symbol representing a class name for java by capitalizing all words.
(defn gen-classname [s]
  (let [s (str s)
        words (string/split s #"-")
        class-name (apply str (map string/capitalize words))]
    (symbol class-name)))

;Takes the current user namespace, the name of the mod, the version, and a rest argument evaled as a map.
;Using these things, constructs a class for the mod, with the proper annotations and such.
;Proxies can optionally be included via the rest argument, with :common and :client.
(defmacro defmod [name-ns mod-name version & proxies]
  (let [proxies (apply hash-map proxies)
        commonproxy (if (:common proxies) (:common proxies) {})
        clientproxy (if (:client proxies) (:client proxies) {})
        prefix (str mod-name "-")
        fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname mod-name)))]
    `(do
       (gen-class
        :name ~(with-meta fullname `{Mod {:name ~(str (gen-classname mod-name)) :modid ~(str mod-name) :version ~(str version)}})
        :prefix ~(symbol prefix)
        :methods [[~(with-meta 'preInit `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLPreInitializationEvent] ~'void]
                  [~(with-meta 'init `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLInitializationEvent] ~'void]
                  [~(with-meta 'postInit `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLPostInitializationEvent] ~'void]])
       (defn ~(symbol (str prefix "preInit")) [~'this ~'event]
         ~(when (:pre-init commonproxy)
            `(~(:pre-init commonproxy) ~'this ~'event))
         (if client?
           ~(when (:pre-init clientproxy)
              `(~(:pre-init clientproxy) ~'this ~'event))))

       (defn ~(symbol (str prefix "init")) [~'this ~'event]
         ~(when (:init commonproxy)
            `(~(:init commonproxy) ~'this ~'event))
         (if client?
           ~(when (:init clientproxy)
              `(~(:init clientproxy) ~'this ~'event))))

       (defn ~(symbol (str prefix "postInit")) [~'this ~'event]
         ~(when (:post-init commonproxy)
            `(~(:post-init commonproxy) ~'this ~'event))
         (if client?
           ~(when (:post-init clientproxy)
              `(~(:post-init clientproxy) ~'this ~'event)))))))

;General purpose macro used to extend objects. Takes the superclass, constructor arguments (as a vector), the name,
;and the data (as a map), to create an instance of an anonymous class that extends the provided superclass.
(defmacro genobj [superclass constructor-args objdata]
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

(defmacro defobj [superclass constructor-args obj-name objdata]
  `(def ~obj-name (genobj ~superclass ~constructor-args ~objdata)))

;General purpose macro used to extend objects.
;Similar to defobj at first glance, except that this generates an actual class instead of an anonymous instance.
(defmacro defclass
  ([superclass name-ns class-name classdata]
   (let [super-methods (:expose classdata)
         interfaces (:interfaces classdata)
         prefix (str class-name "-")
         fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name)))]
     `(do
        (gen-class
         :name ~fullname
         :prefix ~prefix
         :extends ~superclass
         :exposes-methods ~super-methods
         :implements ~interfaces)
        (def ~class-name ~fullname))))
  ([name-ns class-name classdata]
   (let [interfaces (:interfaces classdata)
         prefix (str class-name "-")
         fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name)))]
     `(do
        (gen-class
         :name ~fullname
         :prefix ~prefix
         :implements ~interfaces)
        (def ~class-name ~fullname)))))

;A series of register functions. When calling "register", the proper function will be called underneath.
(defmulti register (fn [element & args] [(count args) (type element)]))
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

;Registers a Tile Entity.
(defn register-tile-entity [entity id]
  (GameRegistry/registerTileEntity entity id))

;Registers an Event Handler.
(defn register-events [handler]
  (.register (.bus (FMLCommonHandler/instance)) handler)
  (.register MinecraftForge/EVENT_BUS handler))

;Creates class similarly to defclass. However, it implements the ITransientAssociative interface, allowing its data
;to be stored and retrieved like a hash-map. When assoc-ing one of these classes, use assoc! instead of assoc, since
;this changes state.
(defmacro defassocclass
  ([superclass name-ns class-name classdata]
   (let [super-methods (:expose classdata)
         interfaces (conj (get classdata :interfaces []) `clojure.lang.ITransientAssociative)
         fields (:fields classdata)
         prefix (str class-name "-")
         fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name)))
         this-sym (with-meta 'this {:tag fullname})]
     `(do
        (gen-class
         :name ~fullname
         :prefix ~prefix
         :extends ~superclass
         :exposes-methods ~super-methods
         :state ~'data
         :init ~'initialize
         :implements ~interfaces)
        (def ~class-name ~fullname)

        (defn ~(symbol (str prefix "initialize")) []
          [[] (atom ~fields)])
        (defn ~(symbol (str prefix "assoc")) [~'this ~'obj-key ~'obj-val]
          (swap! (~'.-data ~this-sym) assoc ~'obj-key ~'obj-val)
          ~'this)
        (defn ~(symbol (str prefix "conj")) [~'this ~'obj]
          (swap! (~'.-data ~this-sym) conj ~'obj)
          ~'this)
        (defn ~(symbol (str prefix "persistent")) [~'this]
          (deref (~'.-data ~this-sym)))
        (defn ~(symbol (str prefix "conj")) [~'this ~'obj]
          (swap! (~'.-data ~this-sym) conj ~'obj))
        (defn ~(symbol (str prefix "valAt"))
          ([~'this ~'obj]
           (get (deref (~'.-data ~this-sym)) ~'obj))
          ([~'this ~'obj ~'notfound]
           (get (deref (~'.-data ~this-sym)) ~'obj ~'notfound))))))
  ([name-ns class-name classdata]
   (let [interfaces (conj (get classdata :interfaces []) `clojure.lang.ITransientAssociative)
         fields (get classdata :fields {})
         prefix (str class-name "-")
         fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name)))
         this-sym (with-meta 'this {:tag fullname})]
     `(do
        (do
          (gen-class
           :name ~fullname
           :prefix ~prefix
           :state ~'data
           :init ~'initialize
           :implements ~interfaces)
          (def ~class-name ~fullname)

          (defn ~(symbol (str prefix "initialize")) []
            [[] (atom ~fields)])
          (defn ~(symbol (str prefix "assoc")) [~'this ~'obj-key ~'obj-val]
            (swap! (~'.-data ~this-sym) assoc ~'obj-key ~'obj-val)
            ~'this)
          (defn ~(symbol (str prefix "conj")) [~'this ~'obj]
            (swap! (~'.-data ~this-sym) conj ~'obj)
            ~'this)
          (defn ~(symbol (str prefix "persistent")) [~'this]
            (deref (~'.-data ~this-sym)))
          (defn ~(symbol (str prefix "conj")) [~'this ~'obj]
            (swap! (~'.-data ~this-sym) conj ~'obj))
          (defn ~(symbol (str prefix "valAt"))
            ([~'this ~'obj]
             (get (deref (~'.-data ~this-sym)) ~'obj))
            ([~'this ~'obj ~'notfound]
             (get (deref (~'.-data ~this-sym)) ~'obj ~'notfound))))))))

;Generates the mod file for forge-clj itself, with respective name, id, etc.
(gen-class
 :name ^{Mod {:name "ForgeClj" :modid "forge-clj" :version "0.4.0"}} forge_clj.core.ForgeClj
 :prefix "forge-clj-"
 :methods [[^{Mod$EventHandler []} preInit [cpw.mods.fml.common.event.FMLPreInitializationEvent] void]
           [^{Mod$EventHandler []} init [cpw.mods.fml.common.event.FMLInitializationEvent] void]
           [^{Mod$EventHandler []} postInit [cpw.mods.fml.common.event.FMLPostInitializationEvent] void]])

;Event functions, init and postInit do nothing. The preInit function detects if this is the client or server, and sets
;the client? symbol as true if this is on an integrated client, or false if this is on a dedicated server.
;Used in defmod for proxy specification.
(defn forge-clj-preInit [this event]
  (def client? (.isClient (.getSide (FMLCommonHandler/instance)))))
(defn forge-clj-init [this event])
(defn forge-clj-postInit [this event])
