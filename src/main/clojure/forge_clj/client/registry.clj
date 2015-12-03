(ns forge-clj.client.registry
  "Contains all of the functions used to register things with Minecraft Forge on the Client side."
  (:import [net.minecraft.client.renderer.tileentity TileEntitySpecialRenderer]
           [cpw.mods.fml.client.registry ClientRegistry]))

(defn bind-tile-renderer
  "Given a tile entity class and an instance of a TileEntitySpecialRenderer, binds the renderer to the tile entity."
  [^Class tile-entity-class ^TileEntitySpecialRenderer renderer]
  (ClientRegistry/bindTileEntitySpecialRenderer tile-entity-class renderer))
