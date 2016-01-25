(ns forge-clj.world
  "Contains macros and functions related to world generation."
  (:require
   [forge-clj.core :refer [defobj defclass]]
   [forge-clj.util :refer [update-map-keys abs get-fullname with-prefix defmemo]])
  (:import
   [java.util Random]
   [net.minecraft.init Blocks]
   [net.minecraft.block Block BlockFalling]
   [net.minecraft.entity EnumCreatureType]
   [net.minecraft.world.gen.feature WorldGenerator WorldGenLakes WorldGenDungeons]
   [net.minecraft.world.gen.structure MapGenScatteredFeature]
   [net.minecraft.world.gen NoiseGeneratorOctaves NoiseGeneratorPerlin MapGenBase MapGenRavine MapGenCaves]
   [net.minecraft.world.biome BiomeGenBase]
   [net.minecraft.world.chunk IChunkProvider Chunk]
   [net.minecraft.world World WorldType SpawnerAnimals]
   [net.minecraft.util MathHelper]
   [cpw.mods.fml.common IWorldGenerator]
   [cpw.mods.fml.common.eventhandler Event$Result]
   [net.minecraftforge.event.terraingen ChunkProviderEvent$ReplaceBiomeBlocks InitMapGenEvent$EventType TerrainGen PopulateChunkEvent$Populate$EventType PopulateChunkEvent$Pre PopulateChunkEvent$Post]
   [net.minecraftforge.common MinecraftForge]))

(def dimension {:overworld 0
                :nether -1
                :end 1})

(defn clj-generate
  "Takes a map of all the generate functions, finds the respective dimension needed,
  and runs the correct generation function for that dimension."
  [random chunk-x chunk-z ^World world generate-fns]
  (let [dimension-id (.-dimensionId (.-provider world))
        func (get generate-fns dimension-id)]
    (when func
      (func world random chunk-x chunk-z))))

(defn run-default-generator
  "Given a generator, world, Random object (must be an object!), a chunk x value, a chunk z value,
  the number of chances to spawn, and the heights, runs the respective generator at random locations in the chunk,
  according to the Random object."
  [^WorldGenerator generator ^World world ^Random rand-obj chunk-x chunk-z chances height1 height2]
  (loop [chance chances]
    (when (< 0 chance)
      (.generate generator world rand-obj (int (+ (* 16 chunk-x) (.nextInt rand-obj 16))) (int (+ height1 (.nextInt rand-obj (abs (- height1 height2))))) (int (+ (* 16 chunk-z) (.nextInt rand-obj 16))))
      (recur (dec chance)))))

(defmacro defgenerate
  "Given a name and a series of dimension-generator pairs, creates a generator that runs the correct generator function."
  [generator-name & generate-fns]
  (let [generate-fns (apply hash-map generate-fns)
        get-dimension #(get dimension %1 %1)
        generate-fns (update-map-keys get-dimension generate-fns)]
    `(def ~generator-name (reify IWorldGenerator
                            (~'generate [~'this ~'random ~'chunk-x ~'chunk-z ~'world ~'chunk-generator ~'chunk-provider]
                                        (clj-generate ~'random ~'chunk-x ~'chunk-z ~'world ~generate-fns))))))

(defmacro defbiome [biome-name biome-id & options]
  (let [options (apply hash-map options)]
    `(defobj BiomeGenBase [~biome-id] ~biome-name ~options)))

(defn get-loop-number [loop-count outer-current inner-current]
  (+ (* loop-count outer-current) inner-current))

(defn make-parabolic-field []
  (reduce #(assoc %1 (first %2) (second %2)) (into [] (repeat 25 0)) (for [j (range -2 3)
                                                                           k (range -2 3)]
                                                                       [(+ (+ j 2) (* (+ k 2) 5)) (/ 10.0 (MathHelper/sqrt_float (+ (* j j) (* k k) 0.2)))])))

(defn gen-noise-octaves
  ([^NoiseGeneratorOctaves noise-generator par1 par2 par3 par4 par5 par6 par7]
   (.generateNoiseOctaves noise-generator nil (int par1) (int par2) (int par3) (int par4) (double par5) (double par6) (double par7)))
  ([^NoiseGeneratorOctaves noise-generator par1 par2 par3 par4 par5 par6 par7 par8 par9]
   (.generateNoiseOctaves noise-generator nil (int par1) (int par2) (int par3) (int par4) (int par5) (int par6) (double par7) (double par8) (double par9))))

(defn calc-biome-floats [^BiomeGenBase biome1 ^BiomeGenBase biome2 world-type parabolic-field-val]
  (let [root-height (.-rootHeight biome2)
        height-variation (.-heightVariation biome2)
        amplified? (and (= world-type WorldType/AMPLIFIED) (< 0.0 root-height))
        root-height (if amplified? (+ 1 (* 2.0 root-height)) root-height)
        height-variation (if amplified? (+ 1 (* 4.0 height-variation)) height-variation)
        parabolic-modifier (/ parabolic-field-val (+ 2.0 root-height))
        parabolic-modifier (if (< (.-rootHeight biome1) root-height) (/ parabolic-modifier 2.0) parabolic-modifier)]
    [(* height-variation parabolic-modifier) (* root-height parabolic-modifier) parabolic-modifier]))

(defn calc-biome-floats-for-iteration [biomes world-type parabolic-field j1 k1]
  (let [biome (get biomes (+ (* 10 (+ k1 2)) j1 2))
        iteration-coll (range -2 3)
        unmodified-coll (reduce #(map + %1 %2) [0 0 0] (for [first-val iteration-coll
                                                             second-val iteration-coll
                                                             :let [current-biome (get biomes (+ (* 10 (+ k1 2 second-val)) j1 2 first-val))]]
                                                         (calc-biome-floats biome current-biome world-type (get parabolic-field (+ (* 5 (+ second-val 2)) first-val 2)))))]
    [(+ (* (/ (first unmodified-coll) (last unmodified-coll)) 0.9) 0.1) (/ (- (* (/ (second unmodified-coll) (last unmodified-coll)) 4) 1) 8.0) (last unmodified-coll)]))

(defn calc-tweaked-noise [n]
  (let [n (/ n 8000.0)
        n (if (< n 0) (* n -0.3) n)
        n (- (* 3.0 n) 2)]
    (if (< n 0)
      (let [n (/ n 2.0)
            n (if (< n -1.0) -1.0 n)]
        (/ (/ n 1.4) 2.0))
      (if (> n 1.0)
        (/ 1.0 8.0)
        (/ n 8.0)))))

(defn get-noise-at-sub-iteration [noise1 noise2 noise3 tweaked-noise tweaked-variation iteration]
  (let [number (/ (/ (* (- iteration tweaked-noise) 12.0 128.0) 256.0) tweaked-variation)
        number (if (< number 0) (* number 4.0) number)
        noise1 (/ noise1 512.0)
        noise2 (/ noise2 512.0)
        noise3 (/ (+ 1.0 (/ noise3 10.0)) 2.0)
        noise-val (- (MathHelper/denormalizeClamp (double noise1) (double noise2) (double noise3)) number)
        noise-val (if (< 29 iteration) (+ (* noise-val (- 1 (/ (- iteration 29) 3))) (* -10.0 (/ (- iteration 29) 3))) noise-val)]
    noise-val))

(defn get-noises-at-iteration [noise1 noise2 noise3 noise4 biomes world-type parabolic-field j1 k1]
  (let [loop-number (get-loop-number 5 j1 k1)
        b-floats (calc-biome-floats-for-iteration biomes world-type parabolic-field j1 k1)
        t-noise (calc-tweaked-noise (nth noise4 loop-number))
        t-noise (+ (* (/ (* (+ (* t-noise 0.2) (second b-floats)) 8.5) 8.0) 4.0) 8.5)
        t-variation (first b-floats)
        base-sub-iteration (* 33 loop-number)]
    (mapv #(get-noise-at-sub-iteration (nth noise1 (+ loop-number %1)) (nth noise2 (+ loop-number %1)) (nth noise3 (+ loop-number %1)) t-noise t-variation %1) (range 33))))

(defn initialize-noise-field [noise-gen-1 noise-gen-2 noise-gen-3 noise-gen-4 biomes world-type par1 par2 par3]
  (let [noise1 (gen-noise-octaves noise-gen-1 par1 par2 par3 5 33 5 684.412 684.412 684.412)
        noise2 (gen-noise-octaves noise-gen-2 par1 par2 par3 5 33 5 684.412 684.412 684.412)
        noise3 (gen-noise-octaves noise-gen-3 par1 par2 par3 5 33 5 8.555150000000001 4.277575000000001 8.555150000000001)
        noise4 (gen-noise-octaves noise-gen-4 par1 par3 5 5 200 200 0.5)
        parabolic-field (make-parabolic-field)]
    (reduce concat [] (for [j1 (range 5)
                            k1 (range 5)]
                        (get-noises-at-iteration noise1 noise2 noise3 noise4 biomes world-type parabolic-field j1 k1)))))

;ATTEMPT #1: ATTEMPTED TO REPRODUCE USING RECURSION. TOOK A WHILE, CRASHED, AND IS HARD TO UNDERSTAND.

;(defn get-block-type [block-fill block-liquid k2 l2 d15]
;  (if (> d15 0)
;    block-fill
;    (if (< (+ (* k2 8) l2) 63)
;      block-liquid
;      nil)))

;(defn reduce-assoc-blocks [blocks indexes block-types]
;  (if (empty? indexes)
;    blocks
;    (do
;      (println (first indexes))
;      (recur (assoc blocks (first indexes) (first block-types)) (rest indexes) (rest block-types)))))

;(defn get-blocks-at-sub-iteration [terrain-block-fill terrain-block-liquid k j1 k2 l2 blocks i3 d10 d11]
;  (let [initial-index (- (bit-or (bit-shift-left (+ (* k 4) i3) 12) (bit-shift-left (* j1 4) 8) (+ (* k2 8) 12)) 256)
;        d16 (* (- d11 d10) 0.25)
;        d15 (- d10 d16)
;        d15-collection (take 4 (iterate (partial + d16) d15))
;        indexes (take 4 (iterate (partial + 256) initial-index))
;        block-types (mapv (partial get-block-type terrain-block-fill terrain-block-liquid k2 l2) d15-collection)]
;    (into [] (reduce-assoc-blocks blocks indexes block-types))))

;(defn reduce-blocks-at-iteration-2 [terrain-block-fill terrain-block-liquid k j1 j2 l2 blocks i3-collection d10-collection d11-collection]
;  (if (empty? i3-collection)
;    blocks
;    (recur terrain-block-fill terrain-block-liquid k j1 j2 l2 (get-blocks-at-sub-iteration terrain-block-fill terrain-block-liquid k j1 j2 l2 blocks (first i3-collection) (first d10-collection) (first d11-collection)) (rest i3-collection) (rest d10-collection) (rest d11-collection))))

;(defn get-blocks-at-iteration-2 [terrain-block-fill terrain-block-liquid k j1 j2 blocks l2 noise1 noise2 noise3 noise4]
;  (let [d12 (* (- noise3 noise1) 0.25)
;        d13 (* (- noise4 noise2) 0.25)
;        i3-collection (range 4)
;        d10-collection (take 4 (iterate (partial + d12) noise1))
;        d11-collection (take 4 (iterate (partial + d13) noise2))]
;    (into [] (reduce-blocks-at-iteration-2 terrain-block-fill terrain-block-liquid k j1 j2 l2 blocks i3-collection d10-collection d11-collection))))

;(defn reduce-noise-iterations [terrain-block-fill terrain-block-liquid k j1 j2 blocks l2-collection noise1-collection noise2-collection noise3-collection noise4-collection]
;  (if (empty? l2-collection)
;    blocks
;    (recur terrain-block-fill terrain-block-liquid k j1 j2
;           (get-blocks-at-iteration-2 terrain-block-fill terrain-block-liquid k j1 j2 blocks (first l2-collection) (first noise1-collection) (first noise2-collection) (first noise3-collection) (first noise4-collection))
;           (rest l2-collection) (rest noise1-collection) (rest noise2-collection) (rest noise3-collection) (rest noise4-collection))))

;(defn get-blocks-at-noise-iteration [terrain-block-fill terrain-block-liquid k k1 j1 j2 l1 i2 noise-field blocks k2]
;  (let [noise1 (nth noise-field (+ k1 k2))
;        noise2 (nth noise-field (+ l1 k2))
;        noise3 (nth noise-field (+ i2 k2))
;        noise4 (nth noise-field (+ j2 k2))
;        alt-noise1 (* (- (nth noise-field (+ k1 k2 1)) noise1) 0.125)
;        alt-noise2 (* (- (nth noise-field (+ l1 k2 1)) noise2) 0.125)
;        alt-noise3 (* (- (nth noise-field (+ i2 k2 1)) noise3) 0.125)
;        alt-noise4 (* (- (nth noise-field (+ j2 k2 1)) noise4) 0.125)
;        l2-collection (range 8)
;        noise1-collection (take 8 (iterate (partial + alt-noise1) noise1))
;        noise2-collection (take 8 (iterate (partial + alt-noise2) noise2))
;        noise3-collection (take 8 (iterate (partial + alt-noise3) noise3))
;        noise4-collection (take 8 (iterate (partial + alt-noise4) noise4))]
;    (into [] (reduce-noise-iterations terrain-block-fill terrain-block-liquid k j1 j2 blocks l2-collection noise1-collection noise2-collection noise3-collection noise4-collection))))

;(defn get-blocks-at-iteration-1 [noise-field terrain-block-fill terrain-block-liquid k blocks j1]
;  (let [l (* k 5)
;        i1 (* (inc k) 5)
;        k1 (* (+ l j1) 33)
;        l1 (* (+ l j1 1) 33)
;        i2 (* (+ i1 j1) 33)
;        j2 (* (+ i1 j1 1) 33)
;        k2-collection (range 32)]
;    (into [] (reduce (partial get-blocks-at-noise-iteration terrain-block-fill terrain-block-liquid k k1 j1 j2 l1 i2 noise-field) blocks k2-collection))))

;(defn get-blocks-at-iteration [noise-field terrain-block-fill terrain-block-liquid blocks k]
;  (let [j1-collection (range 4)]
;    (into [] (reduce (partial get-blocks-at-iteration-1 noise-field terrain-block-fill terrain-block-liquid k) blocks j1-collection))))

;(defn generate-terrain [blocks noise-field terrain-block-fill terrain-block-liquid]
;  (into [] (reduce (partial get-blocks-at-iteration noise-field terrain-block-fill terrain-block-liquid) blocks (range 4))))

;(defn get-outer-loop [index]
;  (int (/ index 16384)))

;(defn get-first-inner-loop [index]
;  (mod (int (/ (mod index 8192) 1024)) 4))

;(defn get-second-inner-loop [index]
;  (int (/ (mod index 256) 8)))

;(defn get-third-inner-loop [index]
;  (int (mod (mod index 32) 8)))

;(defn get-base-iteration [index]
;  (int (mod (/ index 4096) 4)))

;(defn get-iteration [index]
;  (int (mod (/ index 256) 4)))

;ATTEMPT #2: PERFORM CALCULATION PER INDEX. TOOK TOO LONG.

;(defn get-block-at-index [noise-field terrain-block-fill terrain-block-liquid index]
;(let [outer-loop (get-outer-loop index)
;      inner1 (get-first-inner-loop index)
;      inner2 (get-second-inner-loop index)
;      inner3 (get-third-inner-loop index)
;      base-noise-pos (+ (* 165 outer-loop) (* 33 inner1) inner2)
;      noise1 (nth noise-field base-noise-pos)
;      noise2 (nth noise-field (+ base-noise-pos 33))
;      noise3 (nth noise-field (+ base-noise-pos 165))
;      noise4 (nth noise-field (+ base-noise-pos 165 33))
;      base-mod-noise-pos (inc base-noise-pos)
;      mod-noise1 (* (- (nth noise-field base-mod-noise-pos) noise1) 0.125)
;      mod-noise2 (* (- (nth noise-field (+ base-mod-noise-pos 33)) noise2) 0.125)
;      mod-noise3 (* (- (nth noise-field (+ base-mod-noise-pos 165)) noise3) 0.125)
;      mod-noise4 (* (- (nth noise-field (+ base-mod-noise-pos 165 33)) noise4) 0.125)
;      noise3 (* (- noise3 noise1) 0.25)
;      noise4 (* (- noise4 noise2) 0.25)
;      base-iteration (get-base-iteration index)
;      iteration (get-iteration index)
;      noise1 (+ noise1 (* mod-noise1 inner3))
;      noise2 (+ noise2 (* mod-noise2 inner3))
;      noise3 (+ noise3 (* mod-noise3 inner3))
;      noise4 (+ noise4 (* mod-noise4 inner3))
;      noise1 (+ noise1 (* base-iteration noise3))
;      alt-noise1 (* (- noise2 noise1) 0.25)
;      alt-noise2 (- noise1 alt-noise1)
;      alt-noise2 (+ alt-noise2 (* alt-noise1 iteration))]
;  (if (> (+ alt-noise1 alt-noise2) 0)
;    terrain-block-fill
;    (if (< (+ inner3 (* inner2 8)) 63)
;      terrain-block-liquid
;      nil)))
;)

;(defn generate-terrain [blocks noise-field terrain-block-fill terrain-block-liquid]
;  (doall (map (partial get-block-at-index noise-field terrain-block-fill terrain-block-liquid) blocks)))

;ATTEMPT #3: TRIED SOMETHING DIFFERENT. BROKEN BUT COULD BE FIXED. STILL TOO SLOW SO ABANDONED.

;(defn get-terrain-block [terrain-block-fill terrain-block-liquid y noise1]
;  (if (> noise1 0)
;    terrain-block-fill
;    (if (< y 63)
;      terrain-block-liquid
;      nil)))

;(defn make-sub-row [terrain-block-fill terrain-block-liquid y noise2 noise1]
;  (let [interval (* (- noise2 noise1) 0.25)
;        sub-vals (take 4 (iterate (partial + interval) noise1))]
;    (map (partial get-terrain-block terrain-block-fill terrain-block-liquid y) sub-vals)))

;(defn make-row [terrain-block-fill terrain-block-liquid y noise1 noise2 noise3]
;  (let [noise1-interval (* (- noise3 noise1) 0.25)
;        sub-vals (take 4 (iterate (partial + noise1-interval) noise1))]
;    (map (partial make-sub-row terrain-block-fill terrain-block-liquid y noise2) sub-vals)))

;(defn make-sub-wall [terrain-block-fill terrain-block-liquid noise-field sub-layer row sub-wall]
;  (let [noise1-index (+ (* row 33) (* sub-layer 165) sub-wall)
;        noise2-index (+ noise1-index 33)
;        noise3-index (+ noise1-index 165)
;        noise1 (nth noise-field noise1-index)
;        noise2 (nth noise-field noise2-index)
;        noise3 (nth noise-field noise3-index)
;        noise1-increment (* (- (nth noise-field (inc noise1-index)) noise1) 0.125)
;        noise2-increment (* (- (nth noise-field (inc noise2-index)) noise2) 0.125)
;        noise3-increment (* (- (nth noise-field (inc noise3-index)) noise3) 0.125)
;        base-y (* sub-wall 8)]
;    (map #(make-row terrain-block-fill terrain-block-liquid (+ base-y %1) (+ noise1 (* noise1-increment %1)) (+ noise2 (* noise2-increment %1)) (+ noise3 (* noise3-increment %1))) (range 8))))

;(defn make-wall [terrain-block-fill terrain-block-liquid noise-field sub-layer row]
;  (map (partial make-sub-wall terrain-block-fill terrain-block-liquid noise-field sub-layer row) (range 32)))

;(defn make-walls [terrain-block-fill terrain-block-liquid noise-field sub-layer]
;  (map (partial make-wall terrain-block-fill terrain-block-liquid noise-field sub-layer) (range 4)))

;(defn generate-terrain [terrain-block-fill terrain-block-liquid noise-field]
;  (time (doall (flatten (map (partial make-walls terrain-block-fill terrain-block-liquid noise-field) (range 4))))))

(defn default-generate-terrain [terrain-block-fill terrain-block-liquid noise-field]
  (repeat 65536 nil))

(defn post-biome-event [^IChunkProvider chunk-provider par1 par2 blocks byte-collection biomes world-obj]
  (let [event (ChunkProviderEvent$ReplaceBiomeBlocks. chunk-provider par1 par2 blocks (into-array Byte/TYPE (map byte byte-collection)) (into-array BiomeGenBase (map (partial cast BiomeGenBase) biomes)) world-obj)]
    (.post MinecraftForge/EVENT_BUS event)
    (.getResult event)))

(defn replace-blocks-for-biome [chunk-provider world-obj rand-obj ^NoiseGeneratorPerlin noise-perl par1 par2 blocks byte-collection biomes]
  (let [block-array (into-array Block blocks)
        byte-collection (into-array Byte/TYPE byte-collection)
        event-result (post-biome-event chunk-provider par1 par2 block-array byte-collection biomes world-obj)]
    (if (= event-result Event$Result/DENY)
      {:blocks block-array
       :byte-collection byte-collection}
      (let [stone-noise (.func_151599_a noise-perl nil (* par1 16) (* par2 16) 16 16 0.0625 0.0625 1.0)]
        (doall (for [k (range 16)
                     l (range 16)]
                 (.genTerrainBlocks ^BiomeGenBase (get biomes (+ (* k 16) l)) world-obj rand-obj block-array byte-collection (+ (* par1 16) k) (+ (* par2 16) l) (get stone-noise (+ (* k 16) l)))))
        {:blocks block-array
         :byte-collection byte-collection}))))

(defn provide-chunk [this-data gen-terrain-fn par1 par2]
  (let [^IChunkProvider this (:this this-data)
        ^World world-obj (:world this-data)
        ^Random rand-obj (:rand this-data)
        byte-collection (into [] (repeat 65536 0))
        ^MapGenBase cave-generator (:cave-generator this-data)
        ^MapGenBase ravine-generator (:ravine-generator this-data)
        ^MapGenScatteredFeature scattered-feature-generator (if (:map-features? this-data)
                                                              (:scattered-feature-generator this-data))
        noise-gens (:noise-gens this-data)
        biome-atom (:biomes this-data)
        terrain-block-fill (:terrain-block-fill this-data)
        terrain-block-liquid (:terrain-block-liquid this-data)]
    (.setSeed rand-obj (unchecked-add (unchecked-multiply par1 341873128712) (unchecked-multiply par2 132897987541)))
    (swap! biome-atom (fn [biomes]
                        (.getBiomesForGeneration (.getWorldChunkManager world-obj) (into-array BiomeGenBase biomes) (- (* par1 4) 2) (- (* 4 par2) 2) 10 10)))
    (let [blocks (gen-terrain-fn terrain-block-fill terrain-block-liquid
                                 (initialize-noise-field (first noise-gens) (second noise-gens) (nth noise-gens 2) (nth noise-gens 5) @biome-atom (.getTerrainType (.getWorldInfo world-obj)) (* par1 4) 0 (* par2 4)))]
      (swap! biome-atom (fn [biomes]
                          (.loadBlockGeneratorData (.getWorldChunkManager world-obj) biomes (* par1 16) (* par2 16) 16 16)))
      (let [{:keys [blocks byte-collection]} (replace-blocks-for-biome this world-obj rand-obj (nth noise-gens 3) par1 par2 blocks byte-collection @biome-atom)]
        (.func_151539_a cave-generator this world-obj par1 par2 blocks)
        (.func_151539_a ravine-generator this world-obj par1 par2 blocks)
        (if scattered-feature-generator
          (.func_151539_a scattered-feature-generator this world-obj par1 par2 blocks))
        (let [chunk-obj (Chunk. world-obj blocks byte-collection par1 par2)
              biome-array (.getBiomeArray chunk-obj)
              array-index-collection (range (count biome-array))]
          (doall (map #(aset biome-array %1 (byte (.-biomeID ^BiomeGenBase %2))) array-index-collection @biome-atom))
          (.generateSkylightMap chunk-obj)
          chunk-obj)))))

(defn populate-structures [^MapGenScatteredFeature scattered-feature-generator world-obj rand-obj par2 par3]
  (.generateStructuresInChunk scattered-feature-generator world-obj rand-obj par2 par3))

(defn populate-water-lakes [chunk-provider world-obj ^Random rand-obj par2 par3 biome flag]
  (if (and (not= biome BiomeGenBase/desert) (not= biome BiomeGenBase/desertHills) (not flag) (= (.nextInt rand-obj 4) 0) (TerrainGen/populate chunk-provider world-obj rand-obj par2 par3 flag PopulateChunkEvent$Populate$EventType/LAKE))
    (.generate (WorldGenLakes. Blocks/water) world-obj rand-obj (+ (* par2 16) (.nextInt rand-obj 16) 8) (.nextInt rand-obj 256) (+ (* par3 16) (.nextInt rand-obj 16) 8))))

(defn populate-lava-lakes [chunk-provider world-obj ^Random rand-obj par2 par3 flag]
  (when (and (TerrainGen/populate chunk-provider world-obj rand-obj par2 par3 flag PopulateChunkEvent$Populate$EventType/LAVA) (not flag) (= (.nextInt rand-obj 8) 0))
    (let [k1 (+ (* par2 16) (.nextInt rand-obj 16) 8)
          l1 (.nextInt rand-obj (+ (.nextInt rand-obj 248) 8))
          i2 (+ (* par3 16) (.nextInt rand-obj 16) 8)]
      (if (or (< 63 l1) (= (.nextInt rand-obj 10) 0))
        (.generate (WorldGenLakes. Blocks/lava) world-obj rand-obj k1 l1 i2)))))

(defn populate-dungeons [chunk-provider world-obj ^Random rand-obj par2 par3 flag]
  (let [do-gen (TerrainGen/populate chunk-provider world-obj rand-obj par2 par3 flag PopulateChunkEvent$Populate$EventType/DUNGEON)]
    (if do-gen
      (dotimes [k1 8]
        (.generate (WorldGenDungeons.) world-obj rand-obj (+ (* par2 16) (.nextInt rand-obj 16) 8) (.nextInt rand-obj 256) (+ (* par3 16) (.nextInt rand-obj 16) 8))))))

(defn decorate-biome [^BiomeGenBase biome world-obj rand-obj par2 par3]
  (.decorate biome world-obj rand-obj (* par2 16) (* par3 16)))

(defn populate-animals [chunk-provider world-obj ^Random rand-obj par2 par3 biome flag]
  (if (TerrainGen/populate chunk-provider world-obj rand-obj par2 par3 flag PopulateChunkEvent$Populate$EventType/ANIMALS)
    (SpawnerAnimals/performWorldGenSpawning world-obj biome (+ (* par2 16) 8) (+ (* par3 16) 8) 16 16 rand-obj)))

(defn populate-snow-and-ice [chunk-provider ^World world-obj ^Random rand-obj par2 par3 flag]
  (if (TerrainGen/populate chunk-provider world-obj rand-obj par2 par3 flag PopulateChunkEvent$Populate$EventType/ICE)
    (doall (for [k1 (range 16)
                 l1 (range 16)]
             (let [precipitation-height (.getPrecipitationHeight world-obj (+ (* par2 16) 8 k1) (+ (* par3 16) 8 l1))]
               (if (.isBlockFreezable world-obj (+ (* par2 16) 8 k1) (dec precipitation-height) (+ (* par3 16) 8 l1))
                 (.setBlock world-obj (+ (* par2 16) 8 k1) (dec precipitation-height) (+ (* par3 16) 8 l1) Blocks/ice 0 2))
               (if (.func_147478_e world-obj (+ (* par2 16) 8 k1) precipitation-height (+ (* par3 16) 8 l1) true)
                 (.setBlock world-obj (+ (* par2 16) 8 k1) precipitation-height (+ (* par3 16) 8 l1) Blocks/snow_layer 0 2)))))))

(defn populate [this-data chunk-provider par2 par3]
  (set! BlockFalling/fallInstantly true)
  (let [^World world-obj (:world this-data)
        ^Random rand-obj (:rand this-data)
        biome (.getBiomeGenForCoords world-obj (+ (* par2 16) 16) (+ (* par3 16) 16))
        scattered-feature-generator (if (:map-features? this-data)
                                      (:scattered-feature-generator this-data))
        _ (.setSeed rand-obj (.getSeed world-obj))
        i1 (long (unchecked-add (unchecked-multiply (/ (.nextLong rand-obj) (long 2)) (long 2)) (long 1)))
        j1 (long (unchecked-add (unchecked-multiply (/ (.nextLong rand-obj) (long 2)) (long 2)) (long 1)))
        i1 (unchecked-multiply (long par2) i1)
        j1 (unchecked-multiply (long par3) j1)
        _ (.setSeed rand-obj (bit-xor (unchecked-multiply i1 j1) (.getSeed world-obj)))
        flag false]
    (.post MinecraftForge/EVENT_BUS (PopulateChunkEvent$Pre. chunk-provider world-obj rand-obj par2 par3 flag))
    (if scattered-feature-generator
      (populate-structures scattered-feature-generator world-obj rand-obj par2 par3))
    (populate-water-lakes chunk-provider world-obj rand-obj par2 par3 biome flag)
    (populate-lava-lakes chunk-provider world-obj rand-obj par2 par3 flag)
    (populate-dungeons chunk-provider world-obj rand-obj par2 par3 flag)
    (decorate-biome biome world-obj rand-obj par2 par3)
    (populate-animals chunk-provider world-obj rand-obj par2 par3 biome flag)
    (populate-snow-and-ice chunk-provider world-obj rand-obj par2 par3 flag)
    (.post MinecraftForge/EVENT_BUS (PopulateChunkEvent$Post. chunk-provider world-obj rand-obj par2 par3 flag)))
  (set! BlockFalling/fallInstantly false))

(defn get-possible-creatures [^World world ^MapGenScatteredFeature scattered-feature-generator creature-type par2 par3 par4]
  (let [^BiomeGenBase biome (.getBiomeGenForCoords world par2 par4)]
    (if (and (= creature-type EnumCreatureType/monster) scattered-feature-generator (.func_143030_a scattered-feature-generator par2 par3 par4))
      (.getScatteredFeatureSpawnList scattered-feature-generator)
      (.getSpawnableList biome creature-type))))

(defn initialize-chunk-provider
  ([]
   [[] {}])
  ([terrain-block-fill terrain-block-liquid world seed map-features?]
   (let [rand-obj (Random. seed)]
     [[] {:world world
          :rand rand-obj
          :map-features? map-features?
          :noise-gens [(NoiseGeneratorOctaves. rand-obj 16)
                       (NoiseGeneratorOctaves. rand-obj 16)
                       (NoiseGeneratorOctaves. rand-obj 8)
                       (NoiseGeneratorPerlin. rand-obj 4)
                       (NoiseGeneratorOctaves. rand-obj 10)
                       (NoiseGeneratorOctaves. rand-obj 16)
                       (NoiseGeneratorOctaves. rand-obj 16)
                       (NoiseGeneratorOctaves. rand-obj 8)]
          :cave-generator (TerrainGen/getModdedMapGen (MapGenCaves.) InitMapGenEvent$EventType/CAVE)
          :scattered-feature-generator (TerrainGen/getModdedMapGen (MapGenScatteredFeature.) InitMapGenEvent$EventType/SCATTERED_FEATURE)
          :ravine-generator (TerrainGen/getModdedMapGen (MapGenRavine.) InitMapGenEvent$EventType/RAVINE)
          :biomes (atom [])
          :terrain-block-fill terrain-block-fill
          :terrain-block-liquid terrain-block-liquid}])))

(defmacro defchunkprovider [class-name & args]
  (let [classdata (apply hash-map args)
        classdata (assoc classdata
                    :interfaces (conj (get classdata :interfaces []) `IChunkProvider)
                    :init 'initialize
                    :state 'data
                    :constructors {[World Long/TYPE Boolean/TYPE] []})
        terrain-block-fill (get classdata :terrain-block-fill `Blocks/stone)
        terrain-block-liquid (get classdata :terrain-block-liquid `Blocks/water)
        gen-terrain-fn (get classdata :generate-terrain `default-generate-terrain)
        prefix (str class-name "-")
        this-sym (with-meta 'this {:tag (get-fullname (get classdata :ns *ns*) class-name)})]
    `(do
       (defclass ~class-name ~classdata)
       (with-prefix ~prefix
         (def ~'initialize (partial initialize-chunk-provider ~terrain-block-fill ~terrain-block-liquid))
         (defn ~'provideChunk [~'this ~'arg1 ~'arg2]
           (provide-chunk (assoc (.-data ~this-sym) :this ~'this) ~gen-terrain-fn ~'arg1 ~'arg2))
         (defn ~'loadChunk [~'this ~'arg1 ~'arg2]
           (provide-chunk (assoc (.-data ~this-sym) :this ~'this) ~gen-terrain-fn ~'arg1 ~'arg2))
         (def ~'chunkExists (constantly true))
         (defn ~'populate [~'this ~'chunk-provider ~'arg2 ~'arg3]
           (populate (.-data ~this-sym) ~'chunk-provider ~'arg2 ~'arg3))
         (def ~'saveChunks (constantly true))
         (def ~'saveExtraData (constantly nil))
         (def ~'unloadQueuedChunks (constantly false))
         (def ~'canSave (constantly true))
         (defn ~'makeString [~'this]
           (str (.-data ~this-sym)))
         (defn ~'getPossibleCreatures [~'this ~'creature-type ~'par2 ~'par3 ~'par4]
           (get-possible-creatures (:world (.-data ~this-sym)) (:scattered-feature-generator (.-data ~this-sym)) ~'creature-type ~'par2 ~'par3 ~'par4))
         (def ~'getLoadedChunkCount (constantly 0))
         (def ~'recreateStructures (constantly nil))))))

(defmacro defworldtype [obj-name & args]
  (let [objdata (apply hash-map args)
        type-name (get objdata :type-name (str obj-name))
        objdata (dissoc objdata :type-name)]
    `(do
       (defobj WorldType [~type-name] ~obj-name ~objdata))))
