;Declares the namespace and all imports.
(ns forge-clj.core
  (:require
   [clojure.string :as string]
   [clojure.set :as cset])
  (:import
   [java.util Random]
   [net.minecraft.block Block BlockContainer]
   [net.minecraft.block.material Material]
   [net.minecraft.item Item ItemStack ItemBlock ItemBlockWithMetadata ItemArmor ItemFood ItemSword ItemPickaxe ItemAxe ItemSpade ItemHoe]
   [net.minecraft.world.gen.feature WorldGenerator]
   [net.minecraft.world World]
   [net.minecraft.nbt NBTBase NBTTagCompound NBTTagByte NBTTagShort NBTTagInt NBTTagLong NBTTagFloat NBTTagDouble NBTTagByteArray NBTTagIntArray NBTTagString NBTTagList]
   [net.minecraft.tileentity TileEntity]
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

;General purpose macro used to extend objects. Takes the superclass, constructor arguments (as a vector), the name,
;and the data (as a map), to create an instance of an anonymous class that extends the provided superclass.
(defmacro defobj [superclass constructor-args obj-name objdata]
  (let [overrides (:override objdata)
        override-methods (when overrides (map gen-method (keys overrides)))
        override-calls (if overrides (map #(list apply %1 'args) (vals overrides)))
        override-calls (if overrides (map #(list %1 ['& 'args] %2) override-methods override-calls))
        interfaces (:interfaces objdata)
        objdata (dissoc objdata :override :interfaces)
        setters (map gen-setter (keys objdata))
        calls (map #(list %1 %2) setters (vals objdata))
        super-vector (if interfaces (concat [superclass] interfaces) [superclass])]
    (if overrides
      `(def ~obj-name (doto (proxy ~super-vector ~constructor-args
                              ~@override-calls)
                        ~@calls))
      `(def ~obj-name (doto (proxy ~super-vector ~constructor-args)
                        ~@calls)))))

;General purpose macro used to extend objects.
;Similar to defobj at first glance, except that this generates an actual class instead of an anonymous instance.
(defmacro defclass [superclass name-ns class-name classdata]
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

;Creates a Tile Entity class.
(defmacro deftileentity [name-ns class-name & args]
  (let [classdata (apply hash-map args)]
    `(defclass TileEntity ~name-ns ~class-name ~classdata)))

;Given the name of a block, and a series of keywords and values representing the properties of the block,
;generates a Block object with the specified properties. Methods can be overriden using the :override keyword.
(defmacro defblock [block-name & args]
  (let [blockdata (apply hash-map args)
        material (if (:material blockdata) (:material blockdata) `Material/rock)
        container? (:container? blockdata)
        blockdata (dissoc blockdata :material :container?)]
    `(defobj ~(if container? `BlockContainer `Block) [~material] ~block-name ~blockdata)))

;Given the name of an item, and a series of keywords and values representing the properties of the item,
;generates an Item object with the specified properties. Methods can be overriden using the :override keyword.
(defmacro defitem [item-name & args]
  (let [itemdata (apply hash-map args)]
    `(defobj Item [] ~item-name ~itemdata)))

;Given the respective arguments, creates a tool material.
(defmacro deftoolmaterial [material-name harvest-level durability mining-speed damage enchantability]
  `(def ~material-name (EnumHelper/addToolMaterial ~(str material-name) ~harvest-level ~durability ~(float mining-speed) ~(float damage) ~enchantability)))

;Given the resepctive arguments, creates a tool. Requires a previously defined tool material,
;as well as the type of the tool.
(defmacro deftool [item-name material tooltype & args]
  (let [itemdata (apply hash-map args)
        tool (condp = tooltype
               :sword `ItemSword
               :pickaxe `ItemAxe
               :axe `ItemAxe
               :spade `ItemSpade
               :shovel `ItemSpade
               :hoe `ItemHoe
               tooltype)]
    `(defobj ~tool [~material] ~item-name ~itemdata)))

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
        itemdata (dissoc itemdata :renderindex)
        armor (condp = armortype
                :helmet 0
                :chestplate 1
                :leggings 2
                :boots 3
                armortype)]
    `(defobj ItemArmor [~material ~renderindex ~armor] ~item-name ~itemdata)))

;Given the respective arguments, creates food.
(defmacro deffood [item-name heal-amount saturation-modifier & args]
  (let [itemdata (apply hash-map args)
        wolves-favorite? (some? (:wolves-favorite? itemdata))
        itemdata (dissoc itemdata :wolves-favorite?)]
    `(defobj ItemFood [~heal-amount ~(float saturation-modifier) ~wolves-favorite?] ~item-name ~itemdata)))

;Given a previously defined block, creates an item for that block (aka an ItemBlock).
;Register this together with the block referenced.
(defmacro defblockitem [item-name block & args]
  (let [itemdata (apply hash-map args)
        meta? (:metadata? itemdata)
        itemdata (dissoc itemdata :metadata?)]
    `(defobj ~(if meta? `ItemBlockWithMetadata `ItemBlock) ~(if meta? [block block] [block]) ~item-name ~itemdata)))

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

(defn register-tile-entity [entity id]
  (GameRegistry/registerTileEntity entity id))

(defn string-tag-handler [s]
  (condp = s
    "true" true
    "false" false
    "nil" nil
    s))

(defn nbt-key-val-pair [^NBTTagCompound nbt k]
  (let [tag (.getTag nbt k)
        value (condp instance? tag
                NBTTagByte (.getByte nbt k)
                NBTTagShort (.getShort nbt k)
                NBTTagInt (.getInteger nbt k)
                NBTTagLong (.getLong nbt k)
                NBTTagFloat (.getFloat nbt k)
                NBTTagDouble (.getDouble nbt k)
                NBTTagByteArray (into [] (.getByteArray nbt k))
                NBTTagIntArray (into [] (.getIntArray nbt k))
                NBTTagString (string-tag-handler (.getString nbt k))
                NBTTagCompound (.getCompoundTag nbt k)
                tag)]
    [(keyword k) value]))

(defn nbt->map [^NBTTagCompound nbt]
  (let [nbt-json (str nbt)
        removed-braces (apply str (butlast (rest nbt-json)))
        pairs (string/split removed-braces #"[^\\],")
        nbt-keys (map #(first (string/split %1 #":")) pairs)
        nbt-keys (filter (complement empty?) nbt-keys)
        nbt-pairs (mapv (partial nbt-key-val-pair nbt) nbt-keys)
        nbt-map (into {} nbt-pairs)]
    nbt-map))

(defn read-tag-data! [^NBTTagCompound nbt entity-atom]
  (let [data (nbt->map nbt)
        data-key [(:x data) (:y data) (:z data)]]
    (swap! entity-atom assoc data-key data)))

(defn get-data [^TileEntity entity entity-atom]
  (let [data-key [(.-xCoord entity) (.-yCoord entity) (.-zCoord entity)]]
    (get @entity-atom data-key)))

(defn swap-data! [^TileEntity entity entity-atom new-data]
  (let [data-key [(.-xCoord entity) (.-yCoord entity) (.-zCoord entity)]]
    (swap! entity-atom assoc data-key new-data)))

(def byte-array-type (type (byte-array [])))
(def int-array-type (type (int-array [])))

(defn handle-colls [k v ^NBTTagCompound nbt]
  (if (or (empty? v) (= (type (first v)) java.lang.Long) (= (type (first v)) java.lang.Integer))
    (.setIntArray nbt k (int-array v))
    (.setString nbt k (str v))))

(defn keyword->string [k]
  (let [string (str k)
        string (apply str (rest string))]
    string))

(defn add-to-tag [k v ^NBTTagCompound nbt]
  (if (instance? NBTBase v)
    (.setTag nbt k v)
    (condp = (type v)
      java.lang.Byte (.setByte nbt k v)
      java.lang.Short (.setShort nbt k v)
      java.lang.Integer (.setInteger nbt k v)
      java.lang.Long (.setLong nbt k v)
      java.lang.Float (.setFloat nbt k v)
      java.lang.Double (.setDouble nbt k v)
      java.lang.String (.setString nbt k v)
      byte-array-type (.setByteArray nbt k v)
      int-array-type (.setIntArray nbt k v)
      clojure.lang.PersistentVector (handle-colls k v nbt)
      clojure.lang.PersistentList (handle-colls k v nbt)
      (.setString nbt k (str v))))
  nbt)

(defn map->nbt [nbt-map ^NBTTagCompound nbt]
  (reduce #(add-to-tag (keyword->string (key %2)) (val %2) %1) nbt nbt-map))

(defn write-tag-data! [^TileEntity entity ^NBTTagCompound nbt entity-atom]
  (let [nbt-map (get-data entity entity-atom)]
    (map->nbt nbt-map nbt)))

;Generates the mod file for forge-clj itself, with respective name, id, etc.
(gen-class
 :name ^{Mod {:name "ForgeClj" :modid "forge-clj" :version "0.2.2"}} forge_clj.core.ForgeClj
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
