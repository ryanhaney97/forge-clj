(ns forge-clj.client.registry
  "Contains all of the functions used to register things with Minecraft Forge on the Client side."
  (:import [net.minecraft.client.renderer.tileentity TileEntitySpecialRenderer]
           [net.minecraftforge.fml.client.registry ClientRegistry]
           [net.minecraft.block Block]
           [net.minecraft.item Item]
           [net.minecraft.client.resources.model ModelResourceLocation ModelBakery]
           [net.minecraft.client Minecraft]))

(defn bind-tile-renderer
  "Given a tile entity class and an instance of a TileEntitySpecialRenderer, binds the renderer to the tile entity."
  [^Class tile-entity-class ^TileEntitySpecialRenderer renderer]
  (ClientRegistry/bindTileEntitySpecialRenderer tile-entity-class renderer))

(defn register-model
  ([item location]
    (register-model item 0 location))
  ([item meta location]
    (register-model item meta location "inventory"))
  ([item meta location variant]
  (let [^Item item (if (instance? Block item)
                     (Item/getItemFromBlock ^Block item)
                     item)
        location (ModelResourceLocation. (str location) (str variant))]
    (.register (.getItemModelMesher (.getRenderItem (Minecraft/getMinecraft))) item (int meta) location))))

(defn add-variants
  ([item variants]
    (ModelBakery/addVariantName ^Item item (into-array String variants))))