(ns forge-clj.entity
  "Contains macros and functions related to entities and extended entity properties."
  (:require
    [forge-clj.nbt :refer [read-tag-data! write-tag-data!]]
    [forge-clj.core :refer [defassocclass]]
    [forge-clj.util :refer [get-fullname with-prefix remote?]]
    [forge-clj.network :refer [fc-network-send fc-network-receive]]
    [clojure.core.async :refer [>!! <! chan sub go]])
  (:import
    [net.minecraftforge.common IExtendedEntityProperties]))

;Creates a class used to store extended properties.
(defmacro defextendedproperties
  "DEFASSOCCLASS: Creates a class implementing IExtendedEntityProperties.

  The following keywords are treated specially:

  :on-load - called after loading nbt data, with an instance of this passed to it.
  :on-save - called before saving nbt data, with an instance of this passed to it."
  [class-name & args]
  (let [classdata (apply hash-map args)
        name-ns (get classdata :ns *ns*)
        classdata (assoc classdata :interfaces (conj (get classdata :interfaces []) `IExtendedEntityProperties))
        prefix (str class-name "-")
        fullname (get-fullname name-ns class-name)
        this-sym (with-meta 'this {:tag fullname})
        on-load (get classdata :on-load `(constantly nil))
        on-save (get classdata :on-save `(constantly nil))
        sync-data (:sync-data classdata [])
        dont-save (conj (:dont-save classdata []) :world :entity)
        classdata (dissoc classdata :on-load :on-save :sync-data :dont-save)
        client-sync-event (keyword (gensym "client-sync"))
        server-sync-event (keyword (gensym "server-sync"))
        on-change `(fn [~'this ~'obj-key ~'obj-val]
                     (when (some #{~'obj-key} ~sync-data)
                       (if (remote? (:world ~'this))
                         (>!! fc-network-send {:key ~'obj-key
                                               :val ~'obj-val
                                               :send :server
                                               :id ~server-sync-event})
                         (>!! fc-network-send {:key ~'obj-key
                                               :val ~'obj-val
                                               :send :all
                                               :id ~client-sync-event}))))
        classdata (if (and (not (:on-change classdata)) (not (empty? sync-data)))
                    (assoc classdata :on-change on-change)
                    classdata)]
    `(do
       (defassocclass ~class-name ~classdata)
       (with-prefix ~prefix
                    (defn ~'loadNBTData [~'this ~'compound]
                      (let [data# (~'.-data ~this-sym)
                            not-saved# (select-keys (deref data#) ~dont-save)]
                        (apply swap! data# dissoc ~dont-save)
                        (read-tag-data! data# ~'compound)
                        (swap! data# merge not-saved#))
                      (~on-load ~this-sym))
                    (defn ~'saveNBTData [~'this ~'compound]
                      (~on-save ~this-sym)
                      (let [data# (~'.-data ~this-sym)
                            not-saved# (select-keys (deref data#) ~dont-save)]
                        (apply swap! data# dissoc ~dont-save)
                        (write-tag-data! data# ~'compound)
                        (swap! data# merge not-saved#)))
                    (defn ~'init [~'this ~'entity ~'world]
                      (when (not (empty? ~sync-data))
                        (if (remote? ~'world)
                          (let [client-sub# (sub fc-network-receive ~client-sync-event (chan))]
                            (go
                              (while true
                                (let [nbt-data# (<! client-sub#)]
                                  (swap! (~'.-data ~this-sym) assoc (:key nbt-data#) (:val nbt-data#))))))
                          (let [server-sub# (sub fc-network-receive ~server-sync-event (chan))]
                            (go
                              (while true
                                (let [nbt-data# (<! server-sub#)]
                                  (swap! (~'.-data ~this-sym) assoc (:key nbt-data#) (:val nbt-data#))))))))
                      (swap! (~'.-data ~this-sym) assoc :world ~'world :entity ~'entity)
                      nil)))))

(defmacro defentity
  [class-name & args])