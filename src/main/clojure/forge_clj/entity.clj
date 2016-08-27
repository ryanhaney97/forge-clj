(ns forge-clj.entity
  "Contains macros and functions related to entities and extended entity properties."
  (:require
    [forge-clj.nbt :refer [read-tag-data! write-tag-data!]]
    [forge-clj.core :refer [defassocclass get-data genobj]]
    [forge-clj.util :refer [get-fullname with-prefix remote? get-extended-properties get-entity-by-id get-entity-id deep-merge construct sync-data]]
    [forge-clj.network :refer [fc-network]]
    [forge-clj.registry :refer [register-extended-properties]]
    [clojure.core.async :refer [>!! >! <! chan sub go timeout]]
    [clojure.string :as string])
  (:import
    [forge_clj.util IForgeCljSyncData]
    [net.minecraftforge.common IExtendedEntityProperties]
    [net.minecraftforge.event.entity EntityEvent$EntityConstructing]
    [net.minecraft.entity EntityLiving SharedMonsterAttributes]
    [net.minecraft.entity.ai.attributes IAttributeInstance]
    [net.minecraft.entity.ai EntityAIBase]
    [net.minecraftforge.event.entity.player PlayerEvent$StartTracking]
    [net.minecraftforge.fml.common.gameevent PlayerEvent$PlayerLoggedInEvent]))

(defn make-entity-map [package entities]
  (apply merge (map #(hash-map %1 (get-fullname (symbol package) (str "entity-" (name %1)))) entities)))

(def entity-map
  (merge
    (make-entity-map "net.minecraft.entity" [:entity :creature :ageable :flying :living])
    (make-entity-map "net.minecraft.entity.passive" [:animal :bat :chicken :cow :horse :mooshroom :ocelot :pig :rabbit :sheep :squid :wolf :water-mob :tameable :ambient-creature])
    (make-entity-map "net.minecraft.entity.monster" [:blaze :cave-spider :creeper :enderman :endermite :ghast :giant-zombie :golem :guardian :iron-golem :magma-cube :mob :pig-zombie :silverfish :skeleton :slime :snowman :spider :witch :zombie])
    (make-entity-map "net.minecraft.entity.boss" [:dragon :wither])
    (make-entity-map "net.minecraft.entity.player" [:player])))

;Creates a class used to store extended properties.
(defmacro defextendedproperties
  "DEFASSOCCLASS: Creates a class implementing IExtendedEntityProperties.

  The following keywords are treated specially:

  :on-load - called after loading nbt data, with an instance of this passed to it.
  :on-save - called before saving nbt data, with an instance of this passed to it."
  [class-name & args]
  (let [classdata (apply hash-map args)
        name-ns (:ns classdata *ns*)
        sync-data (:sync-data classdata [])
        classdata (assoc classdata :interfaces (apply conj (:interfaces classdata []) (if (not (empty? sync-data))
                                                                                        `[IExtendedEntityProperties IForgeCljSyncData]
                                                                                        `[IExtendedEntityProperties])))
        prefix (str class-name "-")
        fullname (get-fullname name-ns class-name)
        this-sym (with-meta 'this {:tag fullname})
        on-load (:on-load classdata `(constantly nil))
        on-save (:on-save classdata `(constantly nil))
        dont-save (conj (:dont-save classdata []) :world :entity)
        network (:network classdata `fc-network)
        classdata (dissoc classdata :on-load :on-save :sync-data :dont-save :network)
        event-base (str (string/replace (str name-ns) #"\." "-") "-" class-name "-")
        sync-event (keyword (str event-base "sync-event"))
        init-sync-event (keyword (str event-base "init-sync-event"))
        on-change `(fn [~'this ~'obj-key ~'obj-val]
                     (when (some #{~'obj-key} ~sync-data)
                       (>!! (:send ~network) (merge {:key ~'obj-key
                                                     :val ~'obj-val
                                                     :entity-id (get-entity-id (:entity ~'this))
                                                     :id ~sync-event} (if (remote? (:world ~'this))
                                                                        {:send :server}
                                                                        {:send :around
                                                                         :target (:entity ~'this)})))))
        classdata (if (and (not (:on-change classdata)) (not (empty? sync-data)))
                    (assoc classdata :on-change on-change)
                    classdata)]
    `(do
       (defassocclass ~class-name ~classdata)
       ~(when (not-empty sync-data)
          `(do
             ((:listen ~network) ~sync-event
               (fn [~'nbt-data]
                 (let [~'world (:world ~'nbt-data)
                       ~'entity (get-entity-by-id ~'world (:entity-id ~'nbt-data))
                       ~this-sym (get-extended-properties ~'entity ~(str class-name))]
                   (swap! (get-data ~this-sym) assoc (:key ~'nbt-data) (:val ~'nbt-data)))))
             ((:listen ~network) ~init-sync-event
               (fn [~'nbt-data]
                 (let [~'nbt-sync-data (select-keys ~'nbt-data ~sync-data)
                       ~'world (:world ~'nbt-data)
                       ~'entity (get-entity-by-id ~'world (:entity-id ~'nbt-data))
                       ~this-sym (get-extended-properties ~'entity ~(str class-name))]
                   (swap! (get-data ~this-sym) deep-merge ~'nbt-sync-data))))))
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
                      (swap! (~'.-data ~this-sym) assoc :world ~'world :entity ~'entity)
                      nil)
                    ~(when (not-empty sync-data)
                       `(defn ~'syncData
                          ([~'this]
                            (if (not (remote? (:world ~'this)))
                              (go
                                (>! (:send ~network) (assoc (select-keys (deref (get-data ~'this)) ~sync-data)
                                                       :send :all
                                                       :id ~init-sync-event
                                                       :entity-id (get-entity-id (:entity ~'this)))))))
                          ([~'this ~'player]
                            (if (not (remote? (:world ~'this)))
                              (go
                                (>! (:send ~network) (assoc (select-keys (deref (get-data ~'this)) ~sync-data)
                                                       :send :to
                                                       :target ~'player
                                                       :id ~init-sync-event
                                                       :entity-id (get-entity-id (:entity ~'this)))))))))))))

(defn add-extended-properties [^EntityEvent$EntityConstructing event applied-entities property-name properties]
  (let [entity (.-entity event)
        applied-entities (map #(get entity-map %1 %1) applied-entities)]
    (when (and (reduce #(if (instance? %2 entity)
                         (reduced true)
                         false) false (map #(if (symbol? %1) (resolve %1) %1) applied-entities)) (nil? (get-extended-properties entity property-name)))
      (register-extended-properties entity property-name (construct properties)))))

(defn init-sync
  ([^PlayerEvent$StartTracking event]
   (let [entity (.-target event)
         player (.-entityPlayer event)]
     (when (instance? IForgeCljSyncData entity)
       (sync-data entity player))))
  ([^PlayerEvent$StartTracking event applied-entities property-name]
   (let [entity (.-target event)
         player (.-entityPlayer event)
         applied-entities (map #(get entity-map %1 %1) applied-entities)]
     (when (reduce #(if (instance? %2 entity)
                     (reduced true)
                     false) false (map #(if (symbol? %1) (resolve %1) %1) applied-entities))
       (sync-data (get-extended-properties entity property-name) player))))
  ([^PlayerEvent$PlayerLoggedInEvent event property-name]
   (sync-data (get-extended-properties (.-player event) property-name) (.-player event))))

(def shared-monster-attributes-map
  {:max-health SharedMonsterAttributes/maxHealth
   :movement-speed SharedMonsterAttributes/movementSpeed
   :knockback-resistance SharedMonsterAttributes/knockbackResistance
   :follow-range SharedMonsterAttributes/followRange
   :attack-damage SharedMonsterAttributes/attackDamage})

(defn get-attribute [^EntityLiving entity attribute]
  (if (or (= :attack-damage attribute) (not (get shared-monster-attributes-map attribute)))
    (.registerAttribute (.getAttributeMap entity) (get shared-monster-attributes-map attribute attribute)))
  (.getEntityAttribute entity (get shared-monster-attributes-map attribute attribute)))

(defn set-attribute-base [^EntityLiving entity attribute base-val]
  (.setBaseValue ^IAttributeInstance (get-attribute entity attribute) (double base-val)))

(defn get-mutex [compatible]
  (if (= compatible :all)
    8
    (if (empty? compatible)
      7
      (if (some #{:swimming} compatible)
        (if (or (some #{:begging} compatible) (some #{:watch-closest} compatible))
          1
          3)
        5))))

(defn make-ai [entity {:keys [mutex compatible execute? start continue]
                       :or {compatible []}}]
  (let [mutex (int (if mutex
                     mutex
                     (get-mutex compatible)))
        execute? (partial execute? entity)
        start (partial start entity)
        continue (partial continue entity)]
    (genobj EntityAIBase []
            {:mutex-bits mutex
             :override {:should-execute execute?
                        :start-executing start
                        :continue-executing continue}})))

(defmacro defcreature
  [class-name & args]
  (let [classdata (apply hash-map args)
        name-ns (:ns classdata *ns*)
        prefix (str class-name "-")
        fullname (get-fullname name-ns class-name)
        this-sym (with-meta 'this {:tag fullname})
        on-load (:on-load classdata `(constantly nil))
        on-save (:on-save classdata `(constantly nil))
        sync-data (:sync-data classdata [])
        dont-save (:dont-save classdata [])
        attributes (:attributes classdata {})
        network (:network classdata `fc-network)
        entity-type (:type classdata :creature)
        entity-type (get entity-map entity-type entity-type)
        ai (:ai classdata [])
        target-ai (:target-ai classdata [])
        clean-ai? (:clean-ai? classdata)
        classdata (dissoc classdata :on-load :on-save :sync-data :dont-save :attributes :network :ai :type :clean-ai?)
        task-calls (map (fn [task]
                          (if (:type task)
                            `(~'.addTask (~'.-tasks ~this-sym) ~(:priority task 0) (apply construct ~(:type task) ~this-sym ~(:args task `[])))
                            `(~'.addTask (~'.-tasks ~this-sym) ~(:priority task 0) (make-ai ~this-sym ~task)))) ai)
        target-task-calls (map (fn [task]
                                 (if (:type task)
                                   `(~'.addTask (~'.-targetTasks ~this-sym) ~(:priority task 0) (apply construct ~(:type task) ~this-sym ~(:args task `[])))
                                   `(~'.addTask (~'.-targetTasks ~this-sym) ~(:priority task 0) (make-ai ~this-sym ~task)))) target-ai)
        event-base (str (string/replace (str name-ns) #"\." "-") "-" class-name "-")
        sync-event (keyword (str event-base "sync-event"))
        init-sync-event (keyword (str event-base "init-sync-event"))
        on-change `(fn [~'this ~'obj-key ~'obj-val]
                     (when (some #{~'obj-key} ~sync-data)
                       (>!! (:send ~network) (merge {:key ~'obj-key
                                                     :val ~'obj-val
                                                     :entity-id (get-entity-id ~'this)
                                                     :id ~sync-event} (if (remote? (.-worldObj ~this-sym))
                                                                        {:send :server}
                                                                        {:send :around
                                                                         :target ~'this})))))
        classdata (if (and (not (:on-change classdata)) (not-empty sync-data))
                    (assoc classdata :on-change on-change)
                    classdata)
        classdata (assoc classdata :expose '{readEntityFromNBT superReadEntityFromNBT
                                             writeEntityToNBT superWriteEntityToNBT
                                             applyEntityAttributes superApplyEntityAttributes})
        classdata (if (not-empty sync-data)
                    (assoc classdata :interfaces (conj (:interfaces classdata []) `IForgeCljSyncData))
                    classdata)
        classdata (if (or (not-empty ai) (not-empty target-ai))
                    (assoc classdata :post-init "post-init")
                    classdata)]
    `(do
       (defassocclass ~entity-type ~class-name ~classdata)
       ~(when (not (empty? sync-data))
          `(do
             ((:listen ~network) ~sync-event
               (fn [~'nbt-data]
                 (let [~'world (:world ~'nbt-data)
                       ~this-sym (get-entity-by-id ~'world (:entity-id ~'nbt-data))]
                   (swap! (get-data ~this-sym) assoc (:key ~'nbt-data) (:val ~'nbt-data)))))
             ((:listen ~network) ~init-sync-event
               (fn [~'nbt-data]
                 (let [~'nbt-sync-data (select-keys ~'nbt-data ~sync-data)
                       ~'world (:world ~'nbt-data)
                       ~this-sym (get-entity-by-id ~'world (:entity-id ~'nbt-data))]
                   (swap! (get-data ~this-sym) deep-merge ~'nbt-sync-data))))))
       (with-prefix ~prefix
                    (defn ~'readEntityFromNBT [~'this ~'compound]
                      (~'.superReadEntityFromNBT ~this-sym ~'compound)
                      (let [data# (~'.-data ~this-sym)
                            not-saved# (select-keys (deref data#) ~dont-save)]
                        (apply swap! data# dissoc ~dont-save)
                        (read-tag-data! data# ~'compound)
                        (swap! data# merge not-saved#))
                      (~on-load ~this-sym))
                    (defn ~'writeEntityToNBT [~'this ~'compound]
                      (~'.superWriteEntityToNBT ~this-sym ~'compound)
                      (~on-save ~this-sym)
                      (let [data# (~'.-data ~this-sym)
                            not-saved# (select-keys (deref data#) ~dont-save)]
                        (apply swap! data# dissoc ~dont-save)
                        (write-tag-data! data# ~'compound)
                        (swap! data# merge not-saved#)))
                    (defn ~'applyEntityAttributes [~'this]
                      (~'.superApplyEntityAttributes ~this-sym)
                      (dorun (map (fn [~'entry]
                                    (set-attribute-base ~this-sym (key ~'entry) (val ~'entry))) ~attributes)))
                    ~(when (not-empty sync-data)
                       `(defn ~'syncData
                          ([~'this]
                            (if (not (remote? (.-worldObj ~this-sym)))
                              (>!! (:send ~network) (assoc (select-keys (deref (get-data ~'this)) ~sync-data)
                                                      :send :all
                                                      :id ~init-sync-event
                                                      :entity-id (get-entity-id ~'this)))))
                          ([~'this ~'player]
                            (if (not (remote? (.-worldObj ~this-sym)))
                              (>!! (:send ~network) (assoc (select-keys (deref (get-data ~'this)) ~sync-data)
                                                      :send :to
                                                      :target ~'player
                                                      :id ~init-sync-event
                                                      :entity-id (get-entity-id ~'this)))))))
                    ~(when (or (not-empty ai) (not-empty target-ai))
                       `(defn ~'post-init [~'this ~'& ~'args]
                          ~(when clean-ai?
                             `(do
                                (.clear (.taskEntries (.-tasks ~this-sym)))
                                (.clear (.taskEntries (.-targetTasks ~this-sym)))))
                          ~@task-calls
                          ~@target-task-calls))))))