(ns forge-clj.client.util
  (:require
   [forge-clj.tileentity :refer [get-tile-entity-at]]
   [forge-clj.util :refer [server-worlds]])
  (:import
   [net.minecraft.client.renderer.texture TextureManager]
   [net.minecraft.client Minecraft]
   [net.minecraft.util ResourceLocation MovingObjectPosition]
   [org.lwjgl.opengl GL11]))

(defn minecraft-instance []
  (Minecraft/getMinecraft))

(defn client-world []
  (.-theWorld ^Minecraft (minecraft-instance)))

(defn get-tile-entity-looked-at []
  (let [^MovingObjectPosition mop (.rayTrace (.-renderViewEntity ^Minecraft (minecraft-instance)) 200 1.0)]
    (if mop
      (get-tile-entity-at (first (server-worlds)) (.-blockX mop) (.-blockY mop) (.-blockZ mop)))))

(defn resource-location [location]
  (ResourceLocation. (str location)))

(defn bind-texture [texture-location]
  (let [resource (resource-location texture-location)]
    (.bindTexture ^TextureManager (.getTextureManager ^Minecraft (minecraft-instance)) resource)))

(defn set-gl-color
  ([r g b]
   (GL11/glColor3f (float r) (float g) (float b)))
  ([r g b a]
   (GL11/glColor4f (float r) (float g) (float b) (float a))))
