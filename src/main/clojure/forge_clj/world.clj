(ns forge-clj.world
  "Contains macros and functions related to world generation."
  (:require
   [forge-clj.util :refer [update-map-keys abs]])
  (:import
   [java.util Random]
   [net.minecraft.world.gen.feature WorldGenerator]
   [net.minecraft.world World]
   [cpw.mods.fml.common IWorldGenerator]))

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
