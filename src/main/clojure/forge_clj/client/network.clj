(ns forge-clj.client.network
  (:require
    [forge-clj.core :refer [init-client-chan]]
    [forge-clj.network :refer [register-message fc-network gen-packet-handler schedule-task]]
    [forge-clj.client.util :refer [minecraft-instance]]
    [clojure.core.async :refer [chan go >!! <!! <! >! pub sub] :as async])
  (:import
    [net.minecraft.client Minecraft]))

(defmacro defclientnetwork
  ([common-network-name common-network-send common-network-wrapper]
   (let [with-name (comp symbol (partial str (if (coll? common-network-name)
                                               (eval common-network-name)
                                               common-network-name)))]
     `(do
        (defn ~(with-name "-on-packet-from-server") [~'nbt-map ~'context]
          (schedule-task (Minecraft/getMinecraft)
                         (fn []
                           (let [~'nbt-map (assoc ~'nbt-map :minecraft (minecraft-instance))
                                 ~'nbt-map (assoc ~'nbt-map :player (.-thePlayer ~(with-meta `(:minecraft ~'nbt-map) `{:tag Minecraft}))
                                                            :world (.-worldObj (.-thePlayer ~(with-meta `(:minecraft ~'nbt-map) `{:tag Minecraft})))
                                                            :context ~'context)]
                             (>!! ~common-network-send ~'nbt-map))))
          nil)

        (gen-packet-handler ~(with-name "-client-packet-handler") ~(with-name "-on-packet-from-server"))

        (defn ~(with-name "-client-init") []
          (register-message ~common-network-wrapper ~(with-name "-client-packet-handler") 1 :client)))))
  ([common-network]
    `(defclientnetwork (:name ~common-network) (:send ~common-network) (deref (:wrapper ~common-network)))))

(defclientnetwork fc-network)

(go
  (<! init-client-chan)
  (fc-network-client-init))