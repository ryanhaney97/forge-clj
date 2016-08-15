(ns forge-clj.network
  "Contains macros, classes, and functions involving the SimpleNetworkWrapper,
  and related network implementations."
  (:require
    [forge-clj.nbt :refer [nbt->map map->nbt]]
    [forge-clj.core :refer [defclass init-chan]]
    [forge-clj.util :refer [get-fullname with-prefix]]
    [clojure.core.async :refer [chan go go-loop >!! <!! <! >! pub sub] :as async])
  (:import
    [net.minecraft.nbt NBTTagCompound]
    [net.minecraftforge.fml.relauncher Side]
    [net.minecraftforge.fml.common.network ByteBufUtils NetworkRegistry NetworkRegistry$TargetPoint]
    [net.minecraftforge.fml.common.network.simpleimpl IMessage IMessageHandler SimpleNetworkWrapper MessageContext]
    [io.netty.buffer ByteBuf]
    [net.minecraft.entity.player EntityPlayerMP]
    [net.minecraft.entity Entity]
    [net.minecraft.world WorldServer]))

;The following generates a class for forge-clj itself that serves as the default packet
;used by its networking functions.

;This packet stores and retrieves data via the nbt and hash-map converter in forge-clj.nbt.

;------------------------------------------------------------------------------------------

(defclass nbt-packet {:implements [net.minecraftforge.fml.common.network.simpleimpl.IMessage]
                      :constructors {[clojure.lang.PersistentArrayMap] []
                                     [] []}
                      :state data
                      :init init})

(with-prefix nbt-packet-
             (defn init
               ([]
                [[] (atom {})])
               ([nbt-map]
                [[] (atom nbt-map)]))

             (defn fromBytes [^NbtPacket this ^ByteBuf buf]
               (let [nbt-data (ByteBufUtils/readTag buf)
                     converted-data (nbt->map nbt-data)]
                 (reset! (.-data this) converted-data)))

             (defn toBytes [^NbtPacket this ^ByteBuf buf]
               (let [converted-data (deref (.-data this))
                     nbt-data (map->nbt converted-data (NBTTagCompound.))]
                 (ByteBufUtils/writeTag buf nbt-data))))

;------------------------------------------------------------------------------------------

(defmacro gen-packet-handler
  "DEFCLASS: Creates a packet handler given the handler name
  and the function to call upon receiving a message.

  Uses forge_clj.network.NbtPacket as the packet underneath.

  The function called upon receiving a message is called with 2 arguments.
  The first being the hashmap received, and the second being the MessageContext."
  [handler-name on-message]
  (let [prefix (str handler-name "-")]
    `(do
       (defclass ~handler-name {:implements [net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler]})
       (with-prefix ~prefix
                    (defn ~'onMessage [~'this ~'message ~'context]
                      (~on-message (deref (.-data ~(with-meta 'message `{:tag NbtPacket}))) ~'context))))))

(defn create-network
  "Creates a network (aka a SimpleNetworkWrapper) given the network name."
  [network-name]
  (.newSimpleChannel NetworkRegistry/INSTANCE network-name))

(defn register-message
  "Registers a network message given a network, a packet handler, an id (as a number),
  and a side (which can be either a keyword of :client or :server, or a Side)."
  [^SimpleNetworkWrapper network ^Class handler id side]
  (let [^Side network-side (if (= side :client)
                             Side/CLIENT
                             (if (= side :server)
                               Side/SERVER
                               side))]
    (.registerMessage network handler NbtPacket (int id) network-side)))

(defn send-to
  "Sends a message along the specified network from the server to the client side target.
  Message is in the form of a map"
  [^SimpleNetworkWrapper network nbt-map target]
  (let [packet (NbtPacket. nbt-map)]
    (.sendTo network packet target)))

(defn get-target-point [entity range]
  (if (instance? Entity entity)
    (NetworkRegistry$TargetPoint. (.-dimension ^Entity entity) (.-posX ^Entity entity) (.-posY ^Entity entity) (.-posZ ^Entity entity) range)
    entity))

(defn send-to-all-around
  "Sends a message along the specified network from the server to the clients around the specified target.
  Message is in the form of a map"
  ([^SimpleNetworkWrapper network nbt-map target range]
   (let [packet (NbtPacket. nbt-map)
         target (get-target-point target range)]
     (.sendToAllAround network packet target)))
  ([^SimpleNetworkWrapper network nbt-map target]
   (send-to-all-around network nbt-map target 100)))

(defn send-to-all
  "Sends a message along the specified network from the server to all clients.
  Message is in the form of a map"
  [^SimpleNetworkWrapper network nbt-map]
  (let [packet (NbtPacket. nbt-map)]
    (.sendToAll network packet)))

(defn send-to-server
  "Sends a message from the client to the server along the specified network.
  Message is in the form of a map."
  [^SimpleNetworkWrapper network nbt-map]
  (let [packet (NbtPacket. nbt-map)]
    (.sendToServer network packet)))

(defn partition-fc-network [send-map]
  (if (:send send-map)
    (keyword (str "send-" (name (:send send-map :all))))
    (:id send-map)))

(declare fc-network-wrapper)

(def fc-network-send (chan 100))
(def fc-network-receive (pub fc-network-send partition-fc-network))

(defn schedule-task [world f]
  (if (instance? WorldServer world)
    (.addScheduledTask ^WorldServer world
                       (reify Runnable
                         (run [_]
                           (f))))
    (.addScheduledTask ^net.minecraft.client.Minecraft world
                       (reify Runnable
                         (run [_]
                           (f))))))

(defn on-packet-from-client [nbt-map ^MessageContext context]
  (schedule-task (.getServerForPlayer (.-playerEntity (.getServerHandler context)))
                 (fn []
                   (let [nbt-map (assoc nbt-map :player (.-playerEntity (.getServerHandler context)))
                         nbt-map (assoc nbt-map :world (.getServerForPlayer ^EntityPlayerMP (:player nbt-map))
                                                :context context)]
                     (>!! fc-network-send nbt-map))))
  nil)

(defn net-listen [id fn]
  (let [net-sub (sub fc-network-receive id (chan))]
    (go-loop [data (<! net-sub)]
             (schedule-task (:minecraft data (:world data))
                            (partial fn data))
             (recur (<! net-sub)))))

(gen-packet-handler fc-common-packet-handler on-packet-from-client)

(go
  (<! init-chan)
  (def fc-network-wrapper (create-network "fc-network-wrapper"))
  (register-message fc-network-wrapper fc-common-packet-handler 0 :server))

(let [net-sub (sub fc-network-receive :send-to (chan))]
  (go
    (while true
      (let [nbt-map (<! net-sub)
            target (:target nbt-map)]
        (send-to fc-network-wrapper (dissoc nbt-map :send :target) target)))))

(let [net-sub (sub fc-network-receive :send-around (chan))]
  (go
    (while true
      (let [nbt-map (<! net-sub)
            target (:target nbt-map)
            range (:range nbt-map 100)]
        (send-to-all-around fc-network-wrapper (dissoc nbt-map :send :target :range) target range)))))

(let [net-sub (sub fc-network-receive :send-all (chan))]
  (go
    (while true
      (let [nbt-map (<! net-sub)]
        (send-to-all fc-network-wrapper (dissoc nbt-map :send))))))

(let [net-sub (sub fc-network-receive :send-server (chan))]
  (go
    (while true
      (let [nbt-map (<! net-sub)]
        (send-to-server fc-network-wrapper (dissoc nbt-map :send))))))