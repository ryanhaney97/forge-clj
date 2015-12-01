(ns forge-clj.client.renderer
  "Contains the functions and macros required in order to handle model rendering."
  (:require
   [forge-clj.core :refer [defobj]]
   [forge-clj.client.util :refer [bind-texture]])
  (:import
   [net.minecraft.tileentity TileEntity]
   [net.minecraft.entity Entity]
   [net.minecraft.client Minecraft]
   [net.minecraft.client.renderer.tileentity TileEntitySpecialRenderer]
   [net.minecraft.client.model ModelBase ModelRenderer]
   [org.lwjgl.opengl GL11]
   [cpw.mods.fml.client.registry ClientRegistry]))

(defn model-renderer
  "Function that takes a model-map and creates a ModelRenderer object. If memo-test? is set to true, will print to console every time it is called."
  [^ModelBase base property-map memo-test?]
  (if memo-test?
    (println "Renderer Called!"))
  (let [texture-offset (get property-map :texture-offset {})
        renderer (ModelRenderer. base (int (get texture-offset :x 0)) (int (get texture-offset :y 0)))
        box (get property-map :box {})
        rotation-point (get property-map :rotation-point {})
        texture-size (get property-map :texture-size {})
        mirror? (get property-map :mirror? false)
        rotation (get property-map :rotation {})
        renderer (doto renderer
                   (.addBox (float (get box :x 0)) (float (get box :y 0)) (float (get box :z 0)) (int (get box :width 1)) (int (get box :height 1)) (int (get box :depth 1)))
                   (.setRotationPoint (float (get rotation-point :x 0)) (float (get rotation-point :y 0)) (float (get rotation-point :z 0)))
                   (.setTextureSize (int (get texture-size :x 16)) (int (get texture-size :y 16)))
                   (#(set! (.-mirror ^ModelRenderer %1) mirror?))
                   (#(set! (.-rotateAngleX ^ModelRenderer %1) (float (get rotation :x 0))))
                   (#(set! (.-rotateAngleY ^ModelRenderer %1) (float (get rotation :y 0))))
                   (#(set! (.-rotateAngleZ ^ModelRenderer %1) (float (get rotation :z 0)))))]
    renderer))

(def memo-model-renderer (memoize model-renderer))

(defn render-renderer
  "Given a ModelRenderer object, the f needed by it, and the opacity of the renderer, renderers the renderer in game."
  [^ModelRenderer renderer f opacity]
  (if opacity
    (do
      (GL11/glEnable GL11/GL_BLEND)
      (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
      (GL11/glColor4f (float 1.0) (float 1.0) (float 1.0) (float opacity))
      (.render renderer f)
      (GL11/glDisable GL11/GL_BLEND))
    (.render renderer f)))

(defn render-tile-entity
  "Renders a Tile Entity given a model-base, update-function, tile entity, floats, and whether to use the memo? or memo-test? features."
  [^ModelBase model-base update-fn ^TileEntity tile-entity f f1 f2 f3 f4 f5 memo? memo-test?]
  (.render model-base nil f f1 f2 f3 f4 f5)
  (let [render-data (vals (update-fn tile-entity))]
    (doall (map #(render-renderer %1 f5 %2) (map #(if memo?
                                                    (memo-model-renderer model-base %1 memo-test?)
                                                    (model-renderer model-base %1 false)) render-data) (map :opacity render-data)))))

(defn render-entity
  "Renders an Entity given a model-base, update-function, entity, floats, and whether to use the memo? or memo-test? features."
  [^ModelBase model-base update-fn ^Entity entity f f1 f2 f3 f4 f5 memo? memo-test?]
  (.render model-base entity f f1 f2 f3 f4 f5)
  (let [render-data (vals (update-fn entity))]
    (doall (map #(render-renderer %1 f5 %2) (map #(if memo?
                                                    (memo-model-renderer model-base %1 memo-test?)
                                                    (model-renderer model-base %1 false)) render-data) (map :opacity render-data)))))

(defmacro deftilerenderer
  "DEFOBJ: Given a function to call upon updating along with the normal options,
  creates an anonymous instance of a TileEntitySpecialRenderer.

  The update function should take a single argument that is the current state of the tile-entity,
  and return map representing the model to be rendered.

  The following keys are treated specially:

  :texture - specifies a texture for the model, via a string representing the resource location it is located at.
  :memo? - If set to true, will use a memoized version of the main rendering function. Can be very performant so long as the data generated loops. False by default.
  :memo-test? - If set to true, will cause the main rendering function to print to the console every time it is called. Use with :memo? to see if your data loops properly, and can actually use memoization."
  [renderer-name update-model & options]
  (let [options (apply hash-map options)
        texture (:texture options)
        memo? (:memo? options)
        memo-test? (:memo-test? options)
        options (dissoc options :texture :memo? :memo-test?)
        render-tile-entity-at `(fn [~'entity ~'x ~'y ~'z ~'f]
                                 (GL11/glPushMatrix)
                                 (GL11/glTranslated ~'x ~'y ~'z)
                                 ~(if texture
                                    `(bind-texture ~texture)
                                    `(GL11/glDisable GL11/GL_TEXTURE_2D))
                                 (render-tile-entity ~(symbol (str renderer-name "-model-obj")) ~update-model ~'entity 0 0 0 0 0 0.0625 ~memo? ~memo-test?)
                                 ~(if (not texture)
                                    `(GL11/glEnable GL11/GL_TEXTURE_2D))
                                 (GL11/glPopMatrix))
        options (if (not (get-in options [:override :render-tile-entity-at]))
                  (assoc-in options [:override :render-tile-entity-at] render-tile-entity-at)
                  options)]
    `(do
       (def ~(symbol (str renderer-name "-model-obj")) (proxy [ModelBase] []))
       (defobj TileEntitySpecialRenderer [] ~renderer-name ~options))))

(defn bind-tile-renderer
  "Given a tile entity class and an instance of a TileEntitySpecialRenderer, binds the renderer to the tile entity."
  [^Class tile-entity-class ^TileEntitySpecialRenderer renderer]
  (ClientRegistry/bindTileEntitySpecialRenderer tile-entity-class renderer))
