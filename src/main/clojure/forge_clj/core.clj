(ns forge-clj.core
  "Includes all of the core functionality for forge-clj, such as defmod,
  and general-purpose macros used by other namespaces in forge-clj."
  (:require
   [clojure.string :as string]
   [clojure.set :as cset]
   [forge-clj.util :refer [gen-method gen-setter gen-classname get-fullname with-prefix]]
   [clojure.tools.nrepl.server :refer [start-server]])
  (:import
   [java.lang.reflect Field]
   [cpw.mods.fml.common Mod Mod$EventHandler FMLCommonHandler]
   [cpw.mods.fml.common.event FMLPreInitializationEvent FMLInitializationEvent FMLPostInitializationEvent]))

(declare client?)

(defmacro defmod
  "MACRO: Takes the current user namespace, the name of the mod, the version, and a rest argument evaled as a map.
  Using these things, constructs a class for the mod, with the proper annotations and such.
  Proxies can optionally be included via the rest argument, with :common and :client.
  When including a proxy, DO NOT REQUIRE THE NAMESPACE, and instead use the FULL NAMESPACE NAME.
  This will allow forge-clj to include the client namespaces only if run on an integrated client.
  The :repl keyword can also be used to start a nrepl. If :repl is 'true', it'll use the default value
  of 7888 as the port. Otherwise, if :repl is a number, it'll use that number as the port instead."
  [mod-name version & options]
  (let [options (apply hash-map options)
        name-ns (get options :ns *ns*)
        commonproxy (get options :common {})
        clientproxy (get options :client {})
        repl (get options :repl)
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
        fullname (get-fullname name-ns mod-name)]
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

(defmacro defclass
  "MACRO: Given a superclass (optional), a class name,
  and a map of classdata, creates a Java class using Clojure's gen-class function.
  This new class will have a name as specified by gen-classname, and a full package name as specified by get-fullname.
  The full package name will be stored in a def with a name equal to the class name provided.
  The new class will also be imported,
  so you can reference it by its classname from gen-class in your code.
  Macros using this within forge-clj will be labeled with DEFCLASS: in their documentation rather than MACRO:.
  They are still macros unless specified otherwise.

  The following keywords are valid classdata (most of these are explained in detail in Clojure's gen-class documentation):

  :implements/:interfaces - should have a vector containing interfaces for this class to implement.
  :exposes-methods/:expose/:expose-methods - should have a map with the key being the desired method, and the value being the new name for a method that can be called to execute the super method.
  :expose-fields/:exposes - creates a public access field in this class that redirects to another field. Can be used to make private/protected values public.
  :constructors - map of constructors. The key is a vector of types for the constructor to recieve. The value is an associated super constructor if applicable (if not, just use an empty vector).
  :init - name of an initializing function called with arguements to each created constructor.
  :post-init - similar to :init, but specifies a function that is called after the object is already constructed and is passed an instance of the newly-constructed object.
  :methods - creates methods using a vector of vectors. Each vector contains a name, a vector of arguement types, and a return type (or void). Not needed if this method is defined in a superclass/interface, such methods are already made.
  :state - name of a public final field unique to each object. While final, it CAN be an atom if you want.
  :factory - see Clojure's documentation for gen-class.
  :impl-ns - see Clojure's documentation for gen-class.
  :load-impl-ns - see Clojure's documentation for gen-class."
  ([superclass class-name classdata]
   (let [name-ns (get classdata :ns *ns*)
         classdata (cset/rename-keys classdata {:expose-fields :exposes
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
  ([class-name classdata]
   `(defclass nil ~class-name ~classdata)))

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
        fields (:fields objdata)
        objdata (dissoc objdata :override :interfaces :calls :fields)
        setters (map gen-setter (keys objdata))
        calls (map #(if (vector? %2)
                      (apply list %1 %2)
                      (list %1 %2)) setters (vals objdata))
        method-calls (map #(if (vector? (val %1))
                             (apply list (symbol (str "." (gen-method (key %1)))) (val %1))
                             (list (symbol (str "." (gen-method (key %1)))) (val %1))) method-calls)
        field-calls (map (fn [field]
                           `(fn [~'obj]
                              (set! (~(symbol (str ".-" (gen-method (key field)))) ~(with-meta 'obj `{:tag ~superclass})) ~(val field)))) fields)
        super-vector (if interfaces (concat [superclass] interfaces) [superclass])]
    (if overrides
      `(->>
        (doto (proxy ~super-vector ~constructor-args
                ~@override-calls)
          ~@calls
          ~@method-calls
          ~@field-calls))
      `(doto (proxy ~super-vector ~constructor-args)
         ~@calls
         ~@method-calls
         ~@field-calls))))

(defmacro defobj
  "MACRO: same as genobj, but takes a name (as the third argument), and stores the resulting instance in a def with that name.
  Other macros using this will have DEFOBJ: in their docs, instead of MACRO:.
  Realize that other things using defobj in forge-clj are macros as well unless specified otherwise."
  [superclass constructor-args obj-name objdata]
  `(def ~obj-name (genobj ~superclass ~constructor-args ~objdata)))

(defmacro defassocclass
  "DEFCLASS: Creates a class with the specified superclass (optional), class name, and classdata.
  This class implements ITransientAssociative, and as such classes created with this can be treated similarly to a hash-map.
  For example, data can be obtained via (:keyword class-instance), or (get class-instance :keyword).
  Data can also be changed via the assoc! function. Remember to include the exclaimation point!
  Other macros using this will have DEFASSOCCLASS: in their docs instead of MACRO:.
  This also means that those labeled as using this macro are also macros unless specified otherwise."
  ([superclass class-name classdata]
   (let [name-ns (get classdata :ns *ns*)
         classdata (assoc classdata :interfaces (conj (get classdata :interfaces []) `clojure.lang.ITransientAssociative)
                     :init 'initialize
                     :state 'data)
         fields (get classdata :fields {})
         prefix (str class-name "-")
         fullname (get-fullname name-ns class-name)
         this-sym (with-meta 'this {:tag fullname})]
     `(do
        (defclass ~superclass ~class-name ~classdata)
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
  ([class-name classdata]
   `(defassocclass nil ~class-name ~classdata)))

;Mod declaration by forge-clj
;----------------------------

(gen-class
 :name ^{Mod {:name "ForgeClj" :modid "forge-clj" :version "0.5.1"}} forge_clj.core.ForgeClj
 :prefix "forge-clj-"
 :methods [[^{Mod$EventHandler []} preInit [cpw.mods.fml.common.event.FMLPreInitializationEvent] void]
           [^{Mod$EventHandler []} init [cpw.mods.fml.common.event.FMLInitializationEvent] void]
           [^{Mod$EventHandler []} postInit [cpw.mods.fml.common.event.FMLPostInitializationEvent] void]])

(with-prefix forge-clj-
  (defn preInit [this event]
    (def client? (.isClient (.getSide (FMLCommonHandler/instance)))))
  (defn init [this event])
  (defn postInit [this event]))
