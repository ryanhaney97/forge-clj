(ns forge-clj.entity
  (:require
   [forge-clj.nbt :refer [read-tag-data! write-tag-data!]]
   [forge-clj.core :refer [defassocclass gen-classname]]
   [clojure.string :as string])
  (:import
   [net.minecraft.entity Entity]
   [net.minecraftforge.common IExtendedEntityProperties]))

;Creates a class used to store extended properties.
(defmacro defextendedproperties [name-ns class-name & args]
  (let [classdata (apply hash-map args)
        classdata (assoc classdata :interfaces (conj (get classdata :interfaces []) `IExtendedEntityProperties))
        prefix (str class-name "-")
        fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name)))
        this-sym (with-meta 'this {:tag fullname})
        on-load (get classdata :on-load `(constantly nil))
        on-save (get classdata :on-save `(constantly nil))
        classdata (dissoc classdata :on-load :on-save)]
    `(do
       (defassocclass ~name-ns ~class-name ~classdata)
       (defn ~(symbol (str prefix "loadNBTData")) [~'this ~'compound]
         (read-tag-data! (~'.-data ~this-sym) ~'compound)
         (~on-load ~this-sym))
       (defn ~(symbol (str prefix "saveNBTData")) [~'this ~'compound]
         (write-tag-data! (~'.-data ~this-sym) ~'compound)
         (~on-save ~this-sym))
       (defn ~(symbol (str prefix "init")) [~'this ~'entity ~'world]
         nil))))

;Obtains the extended properties from an entity with the specified id.
(defn get-extended-properties [^Entity entity id]
  (.getExtendedProperties entity (str id)))

;Registers Extended Properties.
(defn register-extended-properties [^Entity entity id ^IExtendedEntityProperties properties]
  (.registerExtendedProperties entity (str id) properties))
