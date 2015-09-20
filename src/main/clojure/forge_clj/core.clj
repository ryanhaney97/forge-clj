(ns forge-clj.core
  (:require
   [clojure.string :as string])
  (:import
   [java.lang NoSuchFieldError]
   [net.minecraft.block Block]
   [net.minecraft.item Item ItemStack]
   [net.minecraft.block.material Material]
   [net.minecraft.creativetab CreativeTabs]
   [cpw.mods.fml.common.registry GameRegistry]
   [cpw.mods.fml.common ILanguageAdapter Mod Mod$EventHandler FMLCommonHandler]
   [cpw.mods.fml.common.event FMLPreInitializationEvent FMLInitializationEvent FMLPostInitializationEvent]))

(declare client?)

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
        :methods [[~(with-meta 'preInit `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLPreInitializationEvent] ~(symbol "void")]
                  [~(with-meta 'init `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLInitializationEvent] ~(symbol "void")]
                  [~(with-meta 'postInit `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLPostInitializationEvent] ~(symbol "void")]])
       (defn ~(symbol (str prefix "preInit")) [~'this ~'event]
         ~(when (:pre-init commonproxy)
            `(~(:pre-init commonproxy) ~'event))
         (if client?
           ~(when (:pre-init clientproxy)
              `(~(:pre-init clientproxy) ~'event))))

       (defn ~(symbol (str prefix "init")) [~'this ~'event]
         ~(when (:init commonproxy)
            `(~(:init commonproxy) ~'event))
         (if client?
           ~(when (:init clientproxy)
              `(~(:init clientproxy) ~'event))))

       (defn ~(symbol (str prefix "postInit")) [~'this ~'event]
         ~(when (:post-init commonproxy)
            `(~(:post-init commonproxy) ~'event))
         (if client?
           ~(when (:post-init clientproxy)
              `(~(:post-init clientproxy) ~'event)))))))

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
                          ~@calls))
      `(def ~block-name (doto (proxy [Block] [~material])
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
                         ~@calls))
      `(def ~item-name (doto (proxy [Item] [])
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

(defn material-deploy [k]
  (condp = k
    :air net.minecraft.block.material.Material/air
    :grass net.minecraft.block.material.Material/grass
    :ground net.minecraft.block.material.Material/ground
    :wood net.minecraft.block.material.Material/wood
    :rock net.minecraft.block.material.Material/rock
    :iron net.minecraft.block.material.Material/iron
    :anvil net.minecraft.block.material.Material/anvil
    :water net.minecraft.block.material.Material/water
    :lava net.minecraft.block.material.Material/lava
    :leaves net.minecraft.block.material.Material/leaves
    :plants net.minecraft.block.material.Material/plants
    :vine net.minecraft.block.material.Material/vine
    :sponge net.minecraft.block.material.Material/sponge
    :cloth net.minecraft.block.material.Material/cloth
    :fire net.minecraft.block.material.Material/fire
    :sand net.minecraft.block.material.Material/sand
    :circuits net.minecraft.block.material.Material/circuits
    :carpet net.minecraft.block.material.Material/carpet
    :glass net.minecraft.block.material.Material/glass
    :redstone-light net.minecraft.block.material.Material/redstoneLight
    :tnt net.minecraft.block.material.Material/tnt
    :coral net.minecraft.block.material.Material/coral
    :ice net.minecraft.block.material.Material/ice
    :packed-ice net.minecraft.block.material.Material/packedIce
    :snow net.minecraft.block.material.Material/snow
    :crafted-snow net.minecraft.block.material.Material/craftedSnow
    :cactus net.minecraft.block.material.Material/cactus
    :clay net.minecraft.block.material.Material/clay
    :gourd net.minecraft.block.material.Material/gourd
    :dragon-egg net.minecraft.block.material.Material/dragonEgg
    :portal net.minecraft.block.material.Material/portal
    :cake net.minecraft.block.material.Material/cake
    :web net.minecraft.block.material.Material/web))

(defn creative-tab-deploy [k]
  (condp = k
    :block net.minecraft.creativetab.CreativeTabs/tabBlock
    :decorations net.minecraft.creativetab.CreativeTabs/tabDecorations
    :redstone net.minecraft.creativetab.CreativeTabs/tabRedstone
    :transport net.minecraft.creativetab.CreativeTabs/tabTransport
    :misc net.minecraft.creativetab.CreativeTabs/tabMisc
    :food net.minecraft.creativetab.CreativeTabs/tabFood
    :tools net.minecraft.creativetab.CreativeTabs/tabTools
    :combat net.minecraft.creativetab.CreativeTabs/tabCombat
    :brewing net.minecraft.creativetab.CreativeTabs/tabBrewing
    :materials net.minecraft.creativetab.CreativeTabs/tabMaterials))

(defn step-sound-deploy [k]
  (condp = k
    :stone net.minecraft.block.Block/soundTypeStone
    :wood net.minecraft.block.Block/soundTypeWood
    :gravel net.minecraft.block.Block/soundTypeGravel
    :grass net.minecraft.block.Block/soundTypeGrass
    :piston net.minecraft.block.Block/soundTypePiston
    :metal net.minecraft.block.Block/soundTypeMetal
    :glass net.minecraft.block.Block/soundTypeGlass))

(defmacro material-dev [k]
  (let [base "net.minecraft.block.material.Material/"
        soundtype (string/capitalize (str (gen-method k)))]
    (read-string (str base soundtype))))

(defmacro creative-tab-dev [k]
  (let [base "net.minecraft.creativetab.CreativeTabs/tab"
        soundtype (string/capitalize (str (gen-method k)))]
    (read-string (str base soundtype))))

(defmacro step-sound-dev [k]
  (let [base "net.minecraft.block.Block/soundType"
        soundtype (string/capitalize (str (gen-method k)))]
    (read-string (str base soundtype))))

(defmacro material [k]
  `(try
     (material-deploy ~k)
     (catch NoSuchFieldError ~'e
       (material-dev ~k))))

(defmacro creative-tab [k]
  `(try
     (creative-tab-deploy ~k)
     (catch NoSuchFieldError ~'e
       (creative-tab-dev ~k))))

(defmacro step-sound [k]
  `(try
     (step-sound-deploy ~k)
     (catch NoSuchFieldError ~'e
       (step-sound-dev ~k))))

(gen-class
 :name ^{Mod {:name "ForgeClj" :modid "forge-clj" :version "0.1.0"}} forge_clj.core.ForgeClj
 :prefix "forge-clj-"
 :methods [[^{Mod$EventHandler []} preInit [cpw.mods.fml.common.event.FMLPreInitializationEvent] void]
           [^{Mod$EventHandler []} init [cpw.mods.fml.common.event.FMLInitializationEvent] void]
           [^{Mod$EventHandler []} postInit [cpw.mods.fml.common.event.FMLPostInitializationEvent] void]])

(defn forge-clj-preInit [this event]
  (def client? (.isClient (.getSide (FMLCommonHandler/instance)))))
(defn forge-clj-init [this event])
(defn forge-clj-postInit [this event])
