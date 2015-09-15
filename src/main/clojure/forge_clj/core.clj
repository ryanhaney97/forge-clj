(ns forge-clj.core
  (:require
   [clojure.string :as string])
  (:import
   [net.minecraft.block Block]
   [net.minecraft.item Item ItemStack]
   [net.minecraft.block.material Material]
   [net.minecraft.creativetab CreativeTabs]
   [cpw.mods.fml.common.registry GameRegistry]
   [cpw.mods.fml.common ILanguageAdapter Mod Mod$EventHandler]
   [cpw.mods.fml.common.event FMLPreInitializationEvent FMLInitializationEvent FMLPostInitializationEvent]))

(gen-class
 :name forge_clj.core.ClojureAdapter
 :implements [cpw.mods.fml.common.ILanguageAdapter]
 :constructors {[] []}
 :prefix "adapter-")

(defn adapter-getNewInstance [this container cljclass classloader method]
  (if method
    (.invoke method nil)
    (.newInstance cljclass)))

(defn adapter-supportsStatics [this]
  true)

(defn adapter-setProxy [this target proxy-target proxy-obj])

(defn adapter-setInternalProxies [this modcontainer side loader]
  (let [proxy-target (.getClass (.getMod modcontainer))
        mod-ns (apply str (rest (string/split (str proxy-target) #"\s")))
        mod-name (last (string/split mod-ns #"\."))
        target-ns (reduce #(str %1 "." %2) (butlast (string/split mod-ns #"\.")))
        target-ns-other (symbol (string/replace target-ns "_" "-"))
        required-ns [(symbol target-ns-other)]
        _ (require required-ns)
        target-atom (load-string (str target-ns-other "/modproxy"))
        imported-ns (symbol (str target-ns " CommonProxy ClientProxy"))
        _ (load-string (str "(import '(" imported-ns "))"))
        proxy-obj (if (.isClient side) (load-string (str "(new " target-ns ".ClientProxy)")) (load-string (str "(new " target-ns ".CommonProxy)")))]
    (reset! target-atom proxy-obj)))

(defn gen-setter [k]
  (let [key-name (reduce str (rest (str k)))
        words (string/split key-name #"-")
        method-name (apply str ".set" (map string/capitalize words))]
    (symbol method-name)))

(defn gen-method [k]
  (let [key-name (reduce str (rest (str k)))
        words (string/split key-name #"-")
        method-name (apply str (first words) (map string/capitalize (rest words)))]
    (symbol method-name)))

(defn gen-classname [s]
  (let [s (str s)
        words (string/split s #"-")
        class-name (apply str (map string/capitalize words))]
    (symbol class-name)))

(defmacro defcommonproxy [name-ns & funcs]
  (let [prefix (symbol (str (gensym "common") "-"))
        funcs (apply hash-map funcs)
        pre-init-func (if (:pre-init funcs) `(~(:pre-init funcs) ~'event) nil)
        init-func (if (:init funcs) `(~(:init funcs) ~'event) nil)
        post-init-func (if (:post-init funcs) `(~(:post-init funcs) ~'event) nil)
        commonname (symbol (str (string/replace name-ns #"-" "_") ".CommonProxy"))]
    `(do
       (gen-class
        :name ~commonname
        :prefix ~prefix
        :methods [[~'preInit [cpw.mods.fml.common.event.FMLPreInitializationEvent] ~'void]
                  [~'init [cpw.mods.fml.common.event.FMLInitializationEvent] ~'void]
                  [~'postInit [cpw.mods.fml.common.event.FMLPostInitializationEvent] ~'void]])
       (defn ~(symbol (str prefix "preInit")) [~'this ~'event]
         ~pre-init-func)
       (defn ~(symbol (str prefix "init")) [~'this ~'event]
         ~init-func)
       (defn ~(symbol (str prefix "postInit")) [~'this ~'event]
         ~post-init-func))))

(defmacro defclientproxy [name-ns & funcs]
  (let [prefix (symbol (str (gensym "client") "-"))
        funcs (apply hash-map funcs)
        name-ns-other (symbol (string/replace (str name-ns) "-" "_"))
        pre-init-func (if (:pre-init funcs) `(~(:pre-init funcs) ~'event) nil)
        init-func (if (:init funcs) `(~(:init funcs) ~'event) nil)
        post-init-func (if (:post-init funcs) `(~(:post-init funcs) ~'event) nil)
        clientname (symbol (str (string/replace name-ns #"-" "_") ".ClientProxy"))]
    `(do
       (gen-class
        :name ~clientname
        :prefix ~prefix
        :methods [[~'preInit [cpw.mods.fml.common.event.FMLPreInitializationEvent] ~'void]
                  [~'init [cpw.mods.fml.common.event.FMLInitializationEvent] ~'void]
                  [~'postInit [cpw.mods.fml.common.event.FMLPostInitializationEvent] ~'void]])
       (defn ~(symbol (str prefix "preInit")) [~'this ~'event]
         (.preInit (new ~(symbol (str name-ns-other ".CommonProxy"))) ~'event)
         ~pre-init-func)
       (defn ~(symbol (str prefix "init")) [~'this ~'event]
         (.init (new ~(symbol (str name-ns-other ".CommonProxy"))) ~'event)
         ~init-func)
       (defn ~(symbol (str prefix "postInit")) [~'this ~'event]
         (.postInit (new ~(symbol (str name-ns-other ".CommonProxy"))) ~'event)
         ~post-init-func))))

(defmacro defmod [name-ns mod-name version]
  (let [prefix (str mod-name "-")
        fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname mod-name)))]
    `(do
       (gen-class
        :name ~(with-meta fullname `{Mod {:name ~(str (gen-classname mod-name)) :modid ~(str mod-name) :version ~(str version) :modLanguageAdapter "forge_clj.core.ClojureAdapter"}})
        :prefix ~(symbol prefix)
        :methods [[~(with-meta 'preInit `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLPreInitializationEvent] ~(symbol "void")]
                  [~(with-meta 'init `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLInitializationEvent] ~(symbol "void")]
                  [~(with-meta 'postInit `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLPostInitializationEvent] ~(symbol "void")]])
       (def ~'modproxy (atom nil))
       (defn ~(symbol (str prefix "preInit")) [~'this ~'event]
         ~'(.preInit @modproxy event))

       (defn ~(symbol (str prefix "init")) [~'this ~'event]
         ~'(.init @modproxy event))

       (defn ~(symbol (str prefix "postInit")) [~'this ~'event]
         ~'(.postInit @modproxy event)))))

(defmacro defblock [block-name & args]
  (let [blockdata (apply hash-map args)
        material (if (:material blockdata) (:material blockdata) `Material/rock)
        overrides (:override blockdata)
        override-methods (when overrides (map gen-method (keys overrides)))
        override-calls (if overrides (map #(list apply %1 'args) (vals overrides)))
        override-calls (if overrides (map #(list %1 ['& 'args] %2) override-methods override-calls))
        blockdata (dissoc blockdata :material :override)
        setters (map gen-setter (keys blockdata))
        calls (map #(list %1 %2) setters (vals blockdata))]
    (if overrides
      `(def ~block-name (doto (proxy [Block] [~material]
                                ~@override-calls)
                          (.setBlockName ~(str block-name))
                          ~@calls))
      `(def ~block-name (doto (proxy [Block] [~material])
                          (.setBlockName ~(str block-name))
                          ~@calls)))))

(defmacro defitem [item-name & args]
  (let [itemdata (apply hash-map args)
        overrides (:override itemdata)
        override-methods (when overrides (map gen-method (keys overrides)))
        override-calls (if overrides (map #(list apply %1 'args) (vals overrides)))
        override-calls (if overrides (map #(list %1 ['& 'args] %2) override-methods override-calls))
        itemdata (dissoc itemdata :override)
        setters (map gen-setter (keys itemdata))
        calls (map #(list %1 %2) setters (vals itemdata))]
    (if overrides
      `(def ~item-name (doto (proxy [Item] []
                               ~@override-calls)
                         (.setUnlocalizedName ~(str item-name))
                         ~@calls))
      `(def ~item-name (doto (proxy [Item] [])
                         (.setUnlocalizedName ~(str item-name))
                         ~@calls)))))

(defn convert-recipe [recipe]
  (let [layout (:layout recipe)
        layout (string/replace layout #"_" " ")
        layout (string/split layout #"\n")
        layout (mapv string/trim layout)
        bindings (:bindings recipe)
        bindings (into [] bindings)
        bindings (flatten bindings)]
    (object-array (concat layout bindings))))

(defn addrecipe [result recipe & options]
  (let [options (apply hash-map options)
        shapeless? (:shapeless? options)
        quantity (if (:quantity options) (:quantity options) 1)
        result-itemstack (ItemStack. result quantity)
        recipe-array (if shapeless? recipe (convert-recipe recipe))]
    (if shapeless?
      (GameRegistry/addShapelessRecipe result-itemstack recipe-array)
      (GameRegistry/addRecipe result-itemstack recipe-array))))

(defmulti register (fn [element forge-name] (type element)))

(defmethod register Block [element forge-name]
  (GameRegistry/registerBlock element forge-name))

(defmethod register Item [element forge-name]
  (GameRegistry/registerItem element forge-name))

(defmacro step-sound [k]
  (let [base "net.minecraft.block.material.Material/"
        soundtype (string/capitalize (str (gen-method k)))]
    (read-string (str base soundtype))))

(defmacro creative-tab [k]
  (let [base "net.minecraft.creativetab.CreativeTabs/tab"
        soundtype (string/capitalize (str (gen-method k)))]
    (read-string (str base soundtype))))

(defmacro step-sound [k]
  (let [base "net.minecraft.block.Block/soundType"
        soundtype (string/capitalize (str (gen-method k)))]
    (read-string (str base soundtype))))
