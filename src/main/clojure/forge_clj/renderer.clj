(ns forge-clj.renderer
  (:require
   [forge-clj.core :refer [defobj]])
  (:import
   [net.minecraft.tileentity TileEntity]
   [net.minecraft.entity Entity]
   [net.minecraft.client Minecraft]
   [net.minecraft.client.renderer.tileentity TileEntitySpecialRenderer]
   [net.minecraft.client.model ModelBase ModelRenderer]
   [net.minecraft.util ResourceLocation]
   [org.lwjgl.opengl GL11]
   [cpw.mods.fml.client.registry ClientRegistry]))

(defn model-renderer [^ModelBase base property-map memo-test?]
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

(defn render-renderer [^ModelRenderer renderer f]
  (.render renderer f))

(defn render-tile-entity [^ModelBase model-base update-fn ^TileEntity tile-entity f f1 f2 f3 f4 f5 memo? memo-test?]
  (.render model-base nil f f1 f2 f3 f4 f5)
  (doall (map #(render-renderer %1 f5) (map #(if memo?
                                               (memo-model-renderer model-base %1 memo-test?)
                                               (model-renderer model-base %1 false)) (vals (update-fn tile-entity))))))

(defn render-entity [^ModelBase model-base update-fn ^Entity entity f f1 f2 f3 f4 f5 memo? memo-test?]
  (.render model-base entity f f1 f2 f3 f4 f5)
  (doall (map #(render-renderer %1 f5) (map #(if memo?
                                               (memo-model-renderer model-base %1 memo-test?)
                                               (model-renderer model-base %1 false)) (vals (update-fn entity))))))

(defn resource-location [location]
  (ResourceLocation. (str location)))

(defn bind-texture [texture-location]
  (let [resource (resource-location (str texture-location))]
    (.bindTexture ^net.minecraft.client.renderer.texture.TextureManager (.getTextureManager ^Minecraft (Minecraft/getMinecraft)) resource)))

(defmacro deftilerenderer [renderer-name update-model & options]
  (let [options (apply hash-map options)
        texture (:texture options)
        color (get options :color {})
        memo? (:memo? options)
        memo-test? (:memo-test? options)
        options (dissoc options :texture :render-args :color :update-model :memo? :memo-test?)
        render-tile-entity-at `(fn [~'entity ~'x ~'y ~'z ~'f]
                                 (GL11/glPushMatrix)
                                 (GL11/glTranslated ~'x ~'y ~'z)
                                 ~(if texture
                                    `(bind-texture ~texture)
                                    `(GL11/glDisable GL11/GL_TEXTURE_2D))
                                 (GL11/glColor3f (float (get ~color :r 1)) (float (get ~color :g 1)) (float (get ~color :b 1)))
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

(defn bind-tile-renderer [^Class tile-entity-class ^TileEntitySpecialRenderer renderer]
  (ClientRegistry/bindTileEntitySpecialRenderer tile-entity-class renderer))
