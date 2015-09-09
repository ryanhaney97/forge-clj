(ns forge-clj.core
  (:require
   [clojure.string :as string])
  (:import
   [net.minecraft.block Block]
   [net.minecraft.item Item]
   [net.minecraft.block.material Material]
   [net.minecraft.creativetab CreativeTabs]
   [cpw.mods.fml.common.registry GameRegistry]
   [cpw.mods.fml.common Mod Mod$EventHandler]
   [cpw.mods.fml.common.event FMLInitializationEvent]))

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

(defmacro defmod [name-ns mod-name version init]
  (let [prefix (str mod-name "-")
        fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname mod-name)))]
    `(do
       (gen-class
        :name ~(with-meta fullname `{Mod {:modid ~(str mod-name) :version ~(str version)}})
        :prefix ~(symbol prefix)
        :methods [[~(with-meta 'init `{Mod$EventHandler []}) [cpw.mods.fml.common.event.FMLInitializationEvent] ~(symbol "void")]])
       (defn ~(symbol (str prefix "init")) [~(symbol "this") ~(symbol "event")]
         (~init ~(symbol "this") ~(symbol "event"))))))

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

(defmulti register (fn [element forge-name] (type element)))

(defmethod register Block [element forge-name]
  (GameRegistry/registerBlock element forge-name))

(defmethod register Item [element forge-name]
  (GameRegistry/registerItem element forge-name))

(defn material [k]
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

(defn creative-tab [k]
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

(defn step-sound [k]
  (condp = k
    :stone net.minecraft.block.Block/soundTypeStone
    :wood net.minecraft.block.Block/soundTypeWood
    :gravel net.minecraft.block.Block/soundTypeGravel
    :grass net.minecraft.block.Block/soundTypeGrass
    :piston net.minecraft.block.Block/soundTypePiston
    :metal net.minecraft.block.Block/soundTypeMetal
    :glass net.minecraft.block.Block/soundTypeGlass))
