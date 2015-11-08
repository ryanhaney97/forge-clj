(ns forge-clj.event
  (:require
   [forge-clj.core :refer [gen-classname]]
   [clojure.string :as string])
  (:import
   [cpw.mods.fml.common.eventhandler SubscribeEvent]))

;Creates a method signiture for the given map entry containing things pertaining to events.
(defn gen-event-signiture [map-entry]
  (let [method-name (symbol (apply str (rest (str (key map-entry)))))
        event-map (val map-entry)
        event (:event event-map)
        priority (:priority event-map)]
    `[~(with-meta method-name `{SubscribeEvent ~(if priority
                                                  `[~priority]
                                                  `[])}) [~event] ~'void]))

;Creates an event handler given the namespace, handler name, and a series of arguments representing the events
;to be handled.
(defmacro gen-events [name-ns handler-name & args]
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
