(ns forge-clj.event
  "Contains macros and functions for creating an event handler."
  (:require
   [forge-clj.util :refer [get-fullname gen-classname]]
   [clojure.string :as string])
  (:import
   [cpw.mods.fml.common.eventhandler SubscribeEvent]))

(def event-packages
  "Just a vector of packages to be searched for event classes."
  ["net.minecraftforge.event."
   "cpw.mods.fml.common.event."
   "net.minecraftforge.event.brewing."
   "net.minecraftforge.event.entity."
   "net.minecraftforge.event.entity.item."
   "net.minecraftforge.event.entity.living."
   "net.minecraftforge.event.entity.minecraft."
   "net.minecraftforge.event.entity.player."
   "net.minecraftforge.event.terraingen."
   "net.minecraftforge.event.world."
   "net.minecraftforge.client.event."
   "net.minecraftforge.client.event.sound."
   "cpw.mods.fml.client.event."])

(defn try-to-resolve-event
  "Given a list of event packages, will try to use Class/forName to resolve the given event as a Class.
  If it fails, recurs with the rest of the list until it runs out of names.
  When that happens, it'll just return the event untouched."
  [event-list event]
  (if (empty? event-list)
    event
    (try
      (Class/forName (str (first event-list) event))
      (catch Exception e
        (try-to-resolve-event (rest event-list) event)))))

(defn get-event-from-key
  "Given a keyword, uses try-to-resolve-event to try to resolve a
  classname generated from the keyword as a Class, using event-packages as the event-list."
  [k]
  (let [event (gen-classname (name k))]
    (try-to-resolve-event event-packages event)))

(defn gen-event-signiture
  "Creates a method signiture for the given map entry containing things pertaining to events.
  The key is the name of the method (to be defined by the user later),
  while the value is a map containing a :event, which is the event class being handled.
  If :event is a keyword, forge-clj will try to resolve the keyword as a class via the get-event-from-function.
  Otherwise it expects the full package name of the class along with the name to be provided.
  Optionally a :priority can be provided, which is the event priority."
  [map-entry]
  (let [method-name (symbol (apply str (rest (str (key map-entry)))))
        event-map (val map-entry)
        event (:event event-map)
        event (if (keyword? event)
                (get-event-from-key event)
                event)
        priority (:priority event-map)]
    `[~(with-meta method-name `{SubscribeEvent ~(if priority
                                                  `[~priority]
                                                  `[])}) [~event] ~'void]))

(defmacro gen-events
  "MACRO: Creates an event handler given the namespace, handler name, and a series of arguments representing the events
  to be handled."
  [name-ns handler-name & args]
  (let [fullname (get-fullname name-ns handler-name)
        events (apply hash-map args)
        signitures (mapv gen-event-signiture events)
        prefix (str handler-name "-")]
    `(do
       (gen-class
        :name ~fullname
        :prefix ~prefix
        :methods ~signitures)
       (def ~handler-name (new ~fullname)))))
