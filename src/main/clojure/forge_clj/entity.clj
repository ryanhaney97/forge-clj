(ns forge-clj.entity
  "Contains macros and functions related to entities and extended entity properties."
  (:require
   [forge-clj.nbt :refer [read-tag-data! write-tag-data!]]
   [forge-clj.core :refer [defassocclass get-fullname with-prefix]]
   [clojure.string :as string])
  (:import
   [net.minecraft.entity Entity]
   [net.minecraftforge.common IExtendedEntityProperties]))

;Creates a class used to store extended properties.
(defmacro defextendedproperties
  "DEFASSOCCLASS: Creates a class implementing IExtendedEntityProperties.

  The following keywords are treated specially:

  :on-load - called after loading nbt data, with an instance of this passed to it.
  :on-save - called before saving nbt data, with an instance of this passed to it."
  [name-ns class-name & args]
  (let [classdata (apply hash-map args)
        classdata (assoc classdata :interfaces (conj (get classdata :interfaces []) `IExtendedEntityProperties))
        prefix (str class-name "-")
        fullname (get-fullname name-ns class-name)
        this-sym (with-meta 'this {:tag fullname})
        on-load (get classdata :on-load `(constantly nil))
        on-save (get classdata :on-save `(constantly nil))
        classdata (dissoc classdata :on-load :on-save)]
    `(do
       (defassocclass ~name-ns ~class-name ~classdata)
       (with-prefix ~prefix
         (defn ~'loadNBTData [~'this ~'compound]
           (read-tag-data! (~'.-data ~this-sym) ~'compound)
           (~on-load ~this-sym))
         (defn ~'saveNBTData [~'this ~'compound]
           (~on-save ~this-sym)
           (write-tag-data! (~'.-data ~this-sym) ~'compound))
         (defn ~'init [~'this ~'entity ~'world]
           nil)))))

(defn get-extended-properties
  "Gets the extended entity properties from the specified entity with the provided string id."
  [^Entity entity id]
  (.getExtendedProperties entity (str id)))

(defn register-extended-properties
  "Takes and Entity and an id and registers extended entity properties on it."
  [^Entity entity id ^IExtendedEntityProperties properties]
  (.registerExtendedProperties entity (str id) properties))
