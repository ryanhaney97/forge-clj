(ns forge-clj.client.network
  (:require
    [forge-clj.core :refer [init-pub]]
    [forge-clj.network :refer [register-message fc-network-wrapper fc-network-send fc-network-receive gen-packet-handler]]
    [forge-clj.client.util :refer [minecraft-instance]]
    [clojure.core.async :refer [chan go >!! <!! <! >! pub sub] :as async])
  (:import
    [net.minecraftforge.fml.common.network.simpleimpl SimpleNetworkWrapper MessageContext]
    [net.minecraft.client Minecraft]
    [forge_clj.network NbtPacket]))

(defn send-to-server
  "Sends a message from the client to the server along the specified network.
  Message is in the form of a map."
  [^SimpleNetworkWrapper network nbt-map]
  (let [packet (NbtPacket. nbt-map)]
    (.sendToServer network packet)))

(defn on-packet-from-server [nbt-map ^MessageContext context]
  (let [nbt-map (assoc nbt-map :minecraft (minecraft-instance))
        nbt-map (assoc nbt-map :player (.-thePlayer ^Minecraft (:minecraft nbt-map))
                               :world (.-theWorld ^Minecraft (:minecraft nbt-map))
                               :context context
                               :receive :client)]
    (.addScheduledTask ^Minecraft (:minecraft nbt-map)
                       (reify Runnable
                         (run [_]
                           (>!! fc-network-send nbt-map))))))

(gen-packet-handler fc-client-packet-handler on-packet-from-server)

(let [init-sub (sub init-pub :client (chan))]
  (go
    (<! init-sub)
    (register-message fc-network-wrapper fc-client-packet-handler 1 :client)))

(let [net-sub (sub fc-network-receive :send-server (chan))]
  (go
    (while true
      (let [nbt-map (<! net-sub)]
        (send-to-server fc-network-wrapper (dissoc nbt-map :send))))))