(ns forge-clj.client.util
  "Contains a series of useful functions. Client side only."
  (:require
   [forge-clj.util :refer [server-worlds get-tile-entity-at]])
  (:import
   [net.minecraft.client.renderer.texture TextureManager]
   [net.minecraft.client Minecraft]
   [net.minecraft.util ResourceLocation MovingObjectPosition BlockPos]
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
  (let [^MovingObjectPosition mop (.rayTrace (.getRenderViewEntity ^Minecraft (minecraft-instance)) 200 1.0)
        ^BlockPos block-pos (.getBlockPos mop)]
    (if mop
      (get-tile-entity-at (client-world) (.getX block-pos) (.getY block-pos) (.getZ block-pos)))))

(defn resource-location
  "Given a string representing a ResourceLocation, creates a ResourceLocation for the provided string."
  [location]
  (ResourceLocation. (str location)))

(defn bind-texture
  "Given a string representing a ResourceLocation, binds a texture to the TextureManager."
  ([texture-location minecraft]
  (let [resource (resource-location texture-location)]
    (.bindTexture ^TextureManager (.getTextureManager ^Minecraft minecraft) resource)))
  ([texture-location]
    (bind-texture texture-location (minecraft-instance))))

(defn set-gl-color
  "Given a rgb value, and optionally an alpha value, sets the GL11 color to the specified values."
  ([r g b]
   (GL11/glColor3f (float r) (float g) (float b)))
  ([r g b a]
   (GL11/glColor4f (float r) (float g) (float b) (float a))))
