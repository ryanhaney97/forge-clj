(ns forge-clj.client.util
  "Contains a series of useful functions. Client side only."
  (:require
   [forge-clj.tileentity :refer [get-tile-entity-at]]
   [forge-clj.util :refer [server-worlds]])
  (:import
   [net.minecraft.client.renderer.texture TextureManager]
   [net.minecraft.client Minecraft]
   [net.minecraft.util ResourceLocation MovingObjectPosition]
   [org.lwjgl.opengl GL11]))

(defn minecraft-instance
  "Gets the current minecraft instance."
  []
  (Minecraft/getMinecraft))

(defn client-world
  "Gets the client's current world."
  []
  (.-theWorld ^Minecraft (minecraft-instance)))

(defn get-tile-entity-looked-at
  "Gets the tile entity the player is looking at."
  []
  (let [^MovingObjectPosition mop (.rayTrace (.-renderViewEntity ^Minecraft (minecraft-instance)) 200 1.0)]
    (if mop
      (get-tile-entity-at (first (server-worlds)) (.-blockX mop) (.-blockY mop) (.-blockZ mop)))))

(defn resource-location
  "Given a string representing a ResourceLocation, creates a ResourceLocation for the provided string."
  [location]
  (ResourceLocation. (str location)))

(defn bind-texture
  "Given a string representing a ResourceLocation, binds a texture to the TextureManager."
  [texture-location]
  (let [resource (resource-location texture-location)]
    (.bindTexture ^TextureManager (.getTextureManager ^Minecraft (minecraft-instance)) resource)))

(defn set-gl-color
  "Given a rgb value, and optionally an alpha value, sets the GL11 color to the specified values."
  ([r g b]
   (GL11/glColor3f (float r) (float g) (float b)))
  ([r g b a]
   (GL11/glColor4f (float r) (float g) (float b) (float a))))
