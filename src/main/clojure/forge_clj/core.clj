;Declares the namespace and all imports.
(ns forge-clj.core
  (:require
   [clojure.string :as string]
   [clojure.set :as cset])
  (:import
   [java.util Random]
   [net.minecraft.block Block]
   [net.minecraft.item Item ItemStack ItemBlock ItemBlockWithMetadata ItemArmor ItemFood ItemSword ItemPickaxe ItemAxe ItemSpade ItemHoe]
   [net.minecraft.block.material Material]
   [net.minecraft.creativetab CreativeTabs]
   [net.minecraft.world.gen.feature WorldGenerator]
   [net.minecraft.world World]
   [net.minecraftforge.common.util EnumHelper]
   [cpw.mods.fml.common.registry GameRegistry]
   [cpw.mods.fml.common IWorldGenerator Mod Mod$EventHandler FMLCommonHandler]
   [cpw.mods.fml.common.event FMLPreInitializationEvent FMLInitializationEvent FMLPostInitializationEvent]))

;Declares a final global symbol, that will be set later on as true if client or false if dedicated server.
(declare client?)

;Makes an ItemStack. For convenience.
(defn itemstack
  ([item]
   (itemstack item 1))
  ([item amount]
   (itemstack item amount 0))
  ([item amount metadata]
   (ItemStack. item amount metadata)))

;Given a key word, returns a setter java method as a symbol by adding set as a prefix,
;and capitalizing the remaining words.
(defn gen-setter [k]
  (let [key-name (reduce str (rest (str k)))
        words (string/split key-name #"-")
        method-name (apply str ".set" (map string/capitalize words))]
    (symbol method-name)))

;Given a key word, returns a java method as a symbol by capitalizing all but the first word.
(defn gen-method [k]
  (let [key-name (reduce str (rest (str k)))
        words (string/split key-name #"-")
        method-name (apply str (first words) (map string/capitalize (rest words)))]
    (symbol method-name)))

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

;Given the name of a block, and a series of keywords and values representing the properties of the block,
;generates a Block object with the specified properties. Methods can be overriden using the :override keyword.
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

;Given the name of an item, and a series of keywords and values representing the properties of the item,
;generates an Item object with the specified properties. Methods can be overriden using the :override keyword.
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

;Given the respective arguments, creates a tool material.
(defmacro deftoolmaterial [material-name harvest-level durability mining-speed damage enchantability]
  `(def ~material-name (EnumHelper/addToolMaterial ~(str material-name) ~harvest-level ~durability ~(float mining-speed) ~(float damage) ~enchantability)))

;Given the resepctive arguments, creates a tool. Requires a previously defined tool material,
;as well as the type of the tool.
(defmacro deftool [item-name material tooltype & args]
  (let [itemdata (apply hash-map args)
        overrides (:override itemdata)
        override-methods (when overrides (map gen-method (keys overrides)))
        override-calls (if overrides (map #(list apply %1 'args) (vals overrides)))
        override-calls (if overrides (map #(list %1 ['& 'args] %2) override-methods override-calls))
        itemdata (dissoc itemdata :override)
        setters (map gen-setter (keys itemdata))
        calls (map #(list %1 %2) setters (vals itemdata))
        tool (condp = tooltype
               :sword `ItemSword
               :pickaxe `ItemAxe
               :axe `ItemAxe
               :spade `ItemSpade
               :shovel `ItemSpade
               :hoe `ItemHoe
               tooltype)]
    (if overrides
      `(def ~item-name (doto (proxy [~tool] [~material]
                               ~@override-calls)
                         ~@calls))
      `(def ~item-name (doto (proxy [~tool] [~material])
                         ~@calls)))))

;Given the respective arguments, creates an armor material.
(defmacro defarmormaterial [material-name durability damage-reduction enchantability]
  (let [damage-reduction (if (map? damage-reduction) [(:helmet damage-reduction)
                                                      (:chestplate damage-reduction)
                                                      (:leggings damage-reduction)
                                                      (:boots damage-reduction)] damage-reduction)]
    `(def ~material-name (EnumHelper/addArmorMaterial ~(str material-name) ~durability (int-array ~damage-reduction) ~enchantability))))

;Given the respective arguments, creates a piece of armor. Requires a previously defined armor material,
;as well as the piece of armor.
(defmacro defarmor [item-name material armortype & args]
  (let [itemdata (apply hash-map args)
        renderindex (if (:renderindex itemdata) (:renderindex itemdata) 0)
        overrides (:override itemdata)
        override-methods (when overrides (map gen-method (keys overrides)))
        override-calls (if overrides (map #(list apply %1 'args) (vals overrides)))
        override-calls (if overrides (map #(list %1 ['& 'args] %2) override-methods override-calls))
        itemdata (dissoc itemdata :override :renderindex)
        setters (map gen-setter (keys itemdata))
        calls (map #(list %1 %2) setters (vals itemdata))
        armor (condp = armortype
                :helmet 0
                :chestplate 1
                :leggings 2
                :boots 3
                armortype)]
    (if overrides
      `(def ~item-name (doto (proxy [ItemArmor] [~material ~renderindex ~armor]
                               ~@override-calls)
                         ~@calls))
      `(def ~item-name (doto (proxy [ItemArmor] [~material ~renderindex ~armor])
                         ~@calls)))))

;Given the respective arguments, creates food.
(defmacro deffood [item-name heal-amount saturation-modifier & args]
  (let [itemdata (apply hash-map args)
        wolves-favorite? (some? (:wolves-favorite? itemdata))
        overrides (:override itemdata)
        override-methods (when overrides (map gen-method (keys overrides)))
        override-calls (if overrides (map #(list apply %1 'args) (vals overrides)))
        override-calls (if overrides (map #(list %1 ['& 'args] %2) override-methods override-calls))
        itemdata (dissoc itemdata :override :wolves-favorite?)
        setters (map gen-setter (keys itemdata))
        calls (map #(list %1 %2) setters (vals itemdata))]
    (if overrides
      `(def ~item-name (doto (proxy [ItemFood] [~heal-amount ~(float saturation-modifier) ~wolves-favorite?]
                               ~@override-calls)
                         ~@calls))
      `(def ~item-name (doto (proxy [ItemFood] [~heal-amount ~(float saturation-modifier) ~wolves-favorite?])
                         ~@calls)))))

;Given a previously defined block, creates an item for that block (aka an ItemBlock).
;Register this together with the block referenced.
(defmacro defblockitem [item-name block & args]
  (let [itemdata (apply hash-map args)
        overrides (:override itemdata)
        override-methods (when overrides (map gen-method (keys overrides)))
        override-calls (if overrides (map #(list apply %1 'args) (vals overrides)))
        override-calls (if overrides (map #(list %1 ['& 'args] %2) override-methods override-calls))
        meta? (:metadata? itemdata)
        itemdata (dissoc itemdata :override :metadata?)
        setters (map gen-setter (keys itemdata))
        calls (map #(list %1 %2) setters (vals itemdata))]
    (if overrides
      `(def ~item-name (doto (proxy [~(if meta? `ItemBlockWithMetadata `ItemBlock)] ~(if meta? [block block] [block])
                               ~@override-calls)
                         ~@calls))
      `(def ~item-name (doto (proxy [~(if meta? `ItemBlockWithMetadata `ItemBlock)] ~(if meta? [block block] [block]))
                         ~@calls)))))

;A simple map allowing easy conversion between dimensions and their ids.
(def dimension {:overworld 0
                :nether -1
                :end 1})

;Utility function. Given a map and a function, applies that function to all values in the map.
(defn update-map-vals [func m]
  (into {} (map #(vector (key %1) (func (val %1))) m)))

;Utility function. Given a map and a function, applies that function to all keys in the map.
(defn update-map-keys [func m]
  (cset/map-invert (update-map-vals func (cset/map-invert m))))

;Utility function. Takes a map of all the generate functions, finds the respective dimension needed,
;and runs the correct generation function for that dimension.
(defn clj-generate [random chunk-x chunk-z ^World world generate-fns]
  (let [dimension-id (.-dimensionId (.-provider world))
        func (get generate-fns dimension-id)]
    (when func
      (func world random chunk-x chunk-z))))

;Given a name and a series of dimension-generator pairs, creates a generator that runs the correct generatior function.
(defmacro defgenerate [generator-name & generate-fns]
  (let [generate-fns (apply hash-map generate-fns)
        get-dimension #(get dimension %1 %1)
        generate-fns (update-map-keys get-dimension generate-fns)]
    `(def ~generator-name (reify IWorldGenerator
                            (~'generate [~'this ~'random ~'chunk-x ~'chunk-z ~'world ~'chunk-generator ~'chunk-provider]
                                        (clj-generate ~'random ~'chunk-x ~'chunk-z ~'world ~generate-fns))))))

;Converts a shaped recipe specified in the addrecipe function, into an object array to be passed to the registry.
(defn convert-recipe [recipe]
  (let [layout (:layout recipe)
        layout (string/replace layout #"_" " ")
        layout (string/split layout #"\n")
        layout (mapv string/trim layout)
        bindings (:bindings recipe)
        bindings (into [] bindings)
        bindings (flatten bindings)]
    (object-array (concat layout bindings))))

;Converts a shapeless recipe specified in the addrecipe function, into an object array to be passed to the registry.
(defn convert-shapeless-recipe [recipe]
  (let [items (:items recipe)
        per-item (fn [imap]
                   (itemstack (:item imap) (if (:quantity imap) (:quantity imap) 1) (if (:metadata imap) (:metadata imap) 0)))
        items (mapv per-item items)]
    (object-array items)))

;Given a resulting item or block, as well as a map containing the details of the recipe,
;will register a recipe for the specified item or block. The methodology used to determine the recipe
;is based on whether or not it is shapeless, which is provided via the :shapeless keyword.
(defn addrecipe [result recipe]
  (let [shapeless? (:shapeless recipe)
        quantity (if (:quantity recipe) (:quantity recipe) 1)
        metadata (if (:metadata recipe) (:metadata recipe) 0)
        result-itemstack (itemstack result quantity metadata)
        recipe-array (if shapeless? (convert-shapeless-recipe recipe) (convert-recipe recipe))]
    (if shapeless?
      (GameRegistry/addShapelessRecipe result-itemstack recipe-array)
      (GameRegistry/addRecipe result-itemstack recipe-array))))

;Given an input item or block, an output item or block, and the amount of experience gained from the smelt,
;registers a smelting recipe in the GameRegistry.
(defn addsmelting [input output exp]
  (GameRegistry/addSmelting input (ItemStack. output) (float exp)))

;A series of register functions. When calling "register", the proper function will be called underneath.
(defmulti register (fn [element forge-name & args] [(count args) (type element)]))
(defmethod register [0 Block] [element forge-name]
  (GameRegistry/registerBlock element forge-name)
  element)
(defmethod register [0 Item] [element forge-name]
  (GameRegistry/registerItem element forge-name)
  element)
(defmethod register [1 Block] [element forge-name blockitem]
  (GameRegistry/registerBlock element nil forge-name (object-array []))
  (GameRegistry/registerItem blockitem forge-name)
  element)

;Registers a generator. I hope to fit this with the previous register functions soon.
(defn register-generator
  ([generator]
   (register-generator generator 0))
  ([generator mod-priority]
   (GameRegistry/registerWorldGenerator generator mod-priority)))

;A series of field functions (material, creative-tab, and steps-sound).
;Still needs a refactor, but it's better now than before.
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

;Extremely basic function that returns the absolute value of a number. For convenience.
(defn abs [n]
  (if (< n 0)
    (* -1 n)
    n))

;Given a generator, world, Random object (must be an object!), a chunk x value, a chunk z value,
;the number of chances to spawn, and the heights, runs the respective generator at random locations in the chunk,
;according to the Random object.
(defmacro run-default-generator [^WorldGenerator generator ^World world ^Random rand-obj chunk-x chunk-z chances height1 height2]
  `(loop [~'chance ~chances]
     (when (< 0 ~'chance)
       (.generate ~generator ~world ~rand-obj (+ (* 16 ~chunk-x) (.nextInt ~rand-obj 16)) (+ ~height1 (.nextInt ~rand-obj (abs (- ~height1 ~height2)))) (+ (* 16 ~chunk-z) (.nextInt ~rand-obj 16)))
       (recur (dec ~'chance)))))

;Generates the mod file for forge-clj itself, with respective name, id, etc.
(gen-class
 :name ^{Mod {:name "ForgeClj" :modid "forge-clj" :version "0.2.0"}} forge_clj.core.ForgeClj
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
