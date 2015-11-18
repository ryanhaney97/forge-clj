(ns forge-clj.client.ui
  (:require
   [forge-clj.core :refer [genobj]]
   [forge-clj.client.renderer :refer [bind-texture]])
  (:import
   [net.minecraft.client.gui.inventory GuiContainer]
   [net.minecraft.client.gui Gui]
   [org.lwjgl.opengl GL11]))

(defn draw-rect [^Gui gui rect-map]
  (.drawTexturedModalRect gui (:x rect-map) (:y rect-map) (:u rect-map) (:v rect-map) (:width rect-map) (:height rect-map)))

(defmacro make-gui-container [container background foreground & args]
  (let [objdata (apply hash-map args)
        texture (:texture objdata)
        draw-background `(fn [~'partial-ticks ~'mouse-x ~'mouse-y]
                           (GL11/glColor4f (float 1) (float 1) (float 1) (float 1))
                           ~(if texture
                             `(bind-texture ~texture))
                           (~background ~'this ~'partial-ticks ~'mouse-x ~'mouse-y))
        draw-foreground `(fn [~'mouse-x ~'mouse-y]
                           (~foreground ~'this ~'mouse-x ~'mouse-y))
        objdata (assoc-in objdata [:override :draw-gui-container-background-layer] draw-background)
        objdata (assoc-in objdata [:override :draw-gui-container-foreground-layer] draw-foreground)
        objdata (dissoc objdata :texture)]
    `(genobj GuiContainer [~container] ~objdata)))
