(ns forge-clj.event
  "Contains macros and functions for creating an event handler."
  (:require
   [forge-clj.core :refer [gen-classname]]
   [clojure.string :as string])
  (:import
   [cpw.mods.fml.common.eventhandler SubscribeEvent]))

(defn gen-event-signiture
  "Creates a method signiture for the given map entry containing things pertaining to events.
  The key is the name of the method (to be defined by the user later),
  while the value is a map containing a :event, which is the event class being handled, and optionally a :priority, which is the event priority."
  [map-entry]
  (let [method-name (symbol (apply str (rest (str (key map-entry)))))
        event-map (val map-entry)
        event (:event event-map)
        priority (:priority event-map)]
    `[~(with-meta method-name `{SubscribeEvent ~(if priority
                                                  `[~priority]
                                                  `[])}) [~event] ~'void]))

(defmacro gen-events
  "Creates an event handler given the namespace, handler name, and a series of arguments representing the events
  to be handled."
  [name-ns handler-name & args]
  (let [fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname handler-name)))
        events (apply hash-map args)
        signitures (mapv gen-event-signiture events)
        prefix (str handler-name "-")]
    `(do
       (gen-class
        :name ~fullname
        :prefix ~prefix
        :methods ~signitures)
       (def ~handler-name (new ~fullname)))))
