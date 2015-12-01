(ns forge-clj.network
  "Contains macros, classes, and functions involving the SimpleNetworkWrapper,
  and related network implementations."
  (:require
   [forge-clj.nbt :refer [nbt->map map->nbt]]
   [forge-clj.core :refer [get-fullname with-prefix defclass]]
   [clojure.string :as string])
  (:import
   [net.minecraft.nbt NBTTagCompound]
   [cpw.mods.fml.relauncher Side]
   [cpw.mods.fml.common.network ByteBufUtils NetworkRegistry]
   [cpw.mods.fml.common.network.simpleimpl IMessage IMessageHandler SimpleNetworkWrapper]
   [io.netty.buffer ByteBuf]))

;The following generates a class for forge-clj itself that serves as the default packet
;used by its networking functions.

;This packet stores and retrieves data via the nbt and hash-map converter in forge-clj.nbt.

;------------------------------------------------------------------------------------------

(defclass forge_clj.network nbt-packet {:implements [cpw.mods.fml.common.network.simpleimpl.IMessage]
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
  "DEFCLASS: Creates a packet handler given the namespace, handler name,
  and the function to call upon receiving a message.

  Uses forge_clj.network.NbtPacket as the packet underneath.

  The function called upon receiving a message is called with 2 arguments.
  The first being the hashmap received, and the second being the MessageContext."
  [name-ns handler-name on-message]
  (let [prefix (str handler-name "-")]
    `(do
       (defclass ~name-ns ~handler-name {:implements [cpw.mods.fml.common.network.simpleimpl.IMessageHandler]})
       (with-prefix ~prefix
         (defn ~'onMessage [~'this ~'message ~'context]
           (~on-message (deref (.-data ~(with-meta 'message `{:tag NbtPacket}))) ~'context))))))

(defn create-network
  "Creates a network given the network name."
  [network-name]
  (.newSimpleChannel NetworkRegistry/INSTANCE network-name))

(defn register-message
  "Registers a network message given a network, a packet handler, an id, and a side."
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

(defn send-to-all-around
  "Sends a message along the specified network from the server to the clients around the specified target.
  Message is in the form of a map"
  ([^SimpleNetworkWrapper network nbt-map target]
   (let [packet (NbtPacket. nbt-map)]
     (.sendToAllAround network packet target))))

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
