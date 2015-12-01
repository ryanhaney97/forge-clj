(ns forge-clj.client.ui
  "Contains the client side ui macros."
  (:require
   [forge-clj.core :refer [defclass]])
  (:import
   [net.minecraft.client.gui.inventory GuiContainer]))

(defmacro defguicontainer
  "DEFCLASS: Given a name space, class name, and classdata, creates a class extending GuiContainer.

  Remember to create implementations of drawGuiContainerBackgroundLayer and
  drawGuiContainerForegroundLayer or this will break!"
  [name-ns class-name & args]
  (let [classdata (apply hash-map args)]
    `(defclass GuiContainer ~name-ns ~class-name ~classdata)))
