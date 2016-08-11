(ns forge-clj.client.network
  (:require
    [forge-clj.core :refer [init-client-chan]]
    [forge-clj.network :refer [register-message fc-network-wrapper fc-network-send fc-network-receive gen-packet-handler schedule-task]]
    [forge-clj.client.util :refer [minecraft-instance]]
    [clojure.core.async :refer [chan go >!! <!! <! >! pub sub] :as async])
  (:import
    [net.minecraftforge.fml.common.network.simpleimpl MessageContext]
    [net.minecraft.client Minecraft]))

(defn on-packet-from-server [nbt-map ^MessageContext context]
  (schedule-task (Minecraft/getMinecraft)
                 (fn []
                   (let [nbt-map (assoc nbt-map :minecraft (minecraft-instance))
                         nbt-map (assoc nbt-map :player (.-thePlayer ^Minecraft (:minecraft nbt-map))
                                                :world (.-worldObj (.-thePlayer ^Minecraft (:minecraft nbt-map)))
                                                :context context)]
                     (>!! fc-network-send nbt-map))))
  nil)

(gen-packet-handler fc-client-packet-handler on-packet-from-server)

(go
  (<! init-client-chan)
  (register-message fc-network-wrapper fc-client-packet-handler 1 :client))