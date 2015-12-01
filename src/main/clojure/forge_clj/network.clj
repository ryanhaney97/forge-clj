(ns forge-clj.network
  "Contains macros, classes, and functions involving the SimpleNetworkWrapper,
  and related network implementations."
  (:require
   [forge-clj.nbt :refer [nbt->map map->nbt]]
   [forge-clj.core :refer [gen-classname]]
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

(gen-class
 :name forge_clj.network.NBTPacket
 :prefix "nbt-packet-"
 :state data
 :init init
 :constructors {[clojure.lang.PersistentArrayMap] []
                [] []}
 :implements [cpw.mods.fml.common.network.simpleimpl.IMessage])

(defn nbt-packet-init
  ([]
   [[] (atom {})])
  ([nbt-map]
   [[] (atom nbt-map)]))

(defn nbt-packet-fromBytes [^forge_clj.network.NBTPacket this ^ByteBuf buf]
  (let [nbt-data (ByteBufUtils/readTag buf)
        converted-data (nbt->map nbt-data)]
    (reset! (.-data this) converted-data)))

(defn nbt-packet-toBytes [^forge_clj.network.NBTPacket this ^ByteBuf buf]
  (let [converted-data (deref (.-data this))
        nbt-data (map->nbt converted-data (NBTTagCompound.))]
    (ByteBufUtils/writeTag buf nbt-data)))

;------------------------------------------------------------------------------------------

(defmacro gen-packet-handler
  "MACRO: Creates a packet handler given the namespace, handler name,
  and the function to call upon receiving a message.

  Uses forge_clj.network.NBTPacket as the packet underneath.

  The function called upon receiving a message is called with 2 arguments.
  The first being the hashmap received, and the second being the MessageContext."
  [name-ns handler-name on-message]
  (let [fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname handler-name)))
        prefix (str handler-name "-")]
    `(do
       (gen-class
        :name ~fullname
        :prefix ~prefix
        :implements [cpw.mods.fml.common.network.simpleimpl.IMessageHandler])
       (defn ~(symbol (str prefix "onMessage")) [~'this ~'message ~'context]
         (~on-message (deref (.-data ~(with-meta 'message {:tag 'forge_clj.network.NBTPacket}))) ~'context))
       (def ~handler-name ~fullname)
       (import ~fullname))))

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
    (.registerMessage network handler forge_clj.network.NBTPacket (int id) network-side)))

(defn send-to
  "Sends a message along the specified network from the server to the client side target.
  Message is in the form of a map"
  [^SimpleNetworkWrapper network nbt-map target]
  (let [packet (forge_clj.network.NBTPacket. nbt-map)]
    (.sendTo network packet target)))

(defn send-to-all-around
  "Sends a message along the specified network from the server to the clients around the specified target.
  Message is in the form of a map"
  ([^SimpleNetworkWrapper network nbt-map target]
   (let [packet (forge_clj.network.NBTPacket. nbt-map)]
     (.sendToAllAround network packet target))))

(defn send-to-all
  "Sends a message along the specified network from the server to all clients.
  Message is in the form of a map"
  [^SimpleNetworkWrapper network nbt-map]
  (let [packet (forge_clj.network.NBTPacket. nbt-map)]
    (.sendToAll network packet)))

(defn send-to-server
  "Sends a message from the client to the server along the specified network.
  Message is in the form of a map."
  [^SimpleNetworkWrapper network nbt-map]
  (let [packet (forge_clj.network.NBTPacket. nbt-map)]
    (.sendToServer network packet)))
