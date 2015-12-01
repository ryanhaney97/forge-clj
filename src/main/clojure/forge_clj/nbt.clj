(ns forge-clj.nbt
  "Contains various functions to handle conversion between clojure maps and nbt data.
  Not generally needed by the user, but used often by forge-clj."
  (:require
   [clojure.string :as string])
  (:import
   [net.minecraft.nbt NBTBase NBTTagCompound NBTTagByte NBTTagShort NBTTagInt NBTTagLong NBTTagFloat NBTTagDouble NBTTagByteArray NBTTagIntArray NBTTagString NBTTagList]
   [net.minecraft.item ItemStack]))

(defn string-tag-handler
  "Handles certain types of strings to handle true, false, and nil values."
  [s]
  (condp = (string/trim (string/trim-newline s))
    "true" true
    "false" false
    "nil" nil
    s))

(defn load-istack
  "Given a key and a compound tag, gets an itemstack from the tag."
  [k ^NBTTagCompound nbt]
  (let [istack-nbt (.getCompoundTag nbt k)]
    (ItemStack/loadItemStackFromNBT istack-nbt)))

(declare nbt->map)

(defn nbt-key-val-pair
  "Takes an NBTTagCompound and a key. Gets the tag associated with the key,
  and creates the raw value based on its type. If type cannot be resolved, simply returns the tag object.
  There are also special values that forge-clj handles with the \"§\" character, followed by ITEMSTACK, HASHMAP, VECTOR, or LIST, in order to handle those types."
  [^NBTTagCompound nbt k]
  (let [tag (.getTag nbt k)
        value (if (.contains (str k) "§")
                (let [tag-key-word (last (string/split k #"§"))]
                  (condp = tag-key-word
                    "ITEMSTACK" (load-istack k nbt)
                    "HASHMAP" (nbt->map (.getCompoundTag nbt k))
                    "VECTOR" (let [nbt-map (nbt->map (.getCompoundTag nbt k))]
                               (mapv (partial get nbt-map) (map (comp keyword str) (range (count nbt-map)))))
                    "LIST" (let [nbt-map (nbt->map (.getCompoundTag nbt k))]
                             (map (partial get nbt-map) (map (comp keyword str) (range (count nbt-map)))))
                    tag))
                (condp instance? tag
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
                  tag))]
    [(if (.contains (str k) "§") (keyword (first (string/split k #"§"))) (keyword k)) value]))

(defn nbt->map
  "Converts NBT data into a Clojure map.
  Note that the ',' and ':' characters need to be escaped out of when used in strings and similar data by using the traditional backslash."
  [^NBTTagCompound nbt]
  (let [nbt-json (str nbt)
        removed-braces (apply str (butlast (rest nbt-json)))
        pairs (string/split removed-braces #"[^\\],")
        nbt-keys (map #(first (string/split %1 #"[^\\]:")) pairs)
        nbt-keys (filter (complement empty?) nbt-keys)
        nbt-pairs (mapv (partial nbt-key-val-pair nbt) nbt-keys)
        nbt-map (into {} nbt-pairs)]
    nbt-map))

(def byte-array-type (type (byte-array [])))
(def int-array-type (type (int-array [])))

(declare map->nbt)

(defn handle-colls
  "Handles collections, creating int arrays when possible, and converting it into a tag that handles vectors and lists if not."
  [k v ^NBTTagCompound nbt]
  (if (or (empty? v) (= (type (first v)) java.lang.Long) (= (type (first v)) java.lang.Integer))
    (.setIntArray nbt k (int-array v))
    (.setTag nbt (str k "§" (if (vector? v) "VECTOR" "LIST")) (map->nbt (zipmap (map str (range (count v))) v) (NBTTagCompound.)))))

(defn save-istack
  "Saves an itemstack to the nbtdata."
  [k ^ItemStack istack ^NBTTagCompound nbt]
  (let [istack-nbt (NBTTagCompound.)]
    (.writeToNBT istack istack-nbt)
    (.setTag nbt (str k "§ITEMSTACK") istack-nbt)))

(defn add-to-tag
  "Adds the value at the respective key in the nbt tag based on type. If type cannot be resolved, converts to String
  via prn-str and uses that instead. Unresolvable values should use the load-string function to re-evaluate them or similar, assuming they are usable by prn-str."
  [k v ^NBTTagCompound nbt]
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
      clojure.lang.PersistentArrayMap (.setTag nbt (str k "§HASHMAP") (map->nbt v (NBTTagCompound.)))
      net.minecraft.item.ItemStack (save-istack k v nbt)
      (.setString nbt k (prn-str v))))
  nbt)

(defn map->nbt
  "Stores a Clojure hash-map in an nbt-tag. Might be somewhat lossy if an unsupported type is stored.
  Currently supports:
  Bytes, Shorts, Integers, Longs, Floats, Doubles, Strings, Byte Arrays, Int Arrays, clojure vectors, clojure lists, clojure hash-maps, and Minecraft ItemStacks.
  If a type is not recognized it will be converted into a string as specified by prn-str if possible."
  [nbt-map ^NBTTagCompound nbt]
  (reduce #(add-to-tag (name (key %2)) (val %2) %1) nbt nbt-map))

(defn read-tag-data!
  "Given an atom and a NBTTagCompound, converts the compound into a hash-map, filters out irrelevent fields, and
  replaces the atom with the read data. Fields must be specified in the atom before hand or they will be filtered out."
  [entity-atom ^NBTTagCompound nbt]
  (let [data (nbt->map nbt)
        fields @entity-atom
        field-keys (keys fields)
        data (select-keys data field-keys)
        per-field (fn [entity-data field-key]
                    (if (not (contains? entity-data field-key))
                      (assoc entity-data field-key (get fields field-key))
                      entity-data))
        data (reduce per-field data field-keys)]
    (reset! entity-atom data)))

(defn write-tag-data!
  "Given an atom and a NBTTagCompound, stores the contents of the atom in the nbt tag."
  [entity-atom ^NBTTagCompound nbt]
  (let [nbt-map (deref entity-atom)]
    (map->nbt nbt-map nbt)))
