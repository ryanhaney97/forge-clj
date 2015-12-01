(ns forge-clj.client.ui
  "Contains the client side ui macros."
  (:require
   [forge-clj.core :refer [gen-classname]]
   [clojure.string :as string])
  (:import
   [net.minecraft.client.gui.inventory GuiContainer]
   [net.minecraft.client.gui Gui]
   [net.minecraft.inventory Container]))

(defmacro defguicontainer
  "MACRO: Given a name space, class name, and classdata, creates a class extending GuiContainer.

  Remember to create implementations of drawGuiContainerBackgroundLayer and drawGuiContainerForegroundLayer or this will break!

  The following keywords are treated specially:

  :interfaces - vector of interfaces to implement.
  :expose-methods - map of methods to expose.
  :expose-fields - map of fields to expose. Useful for protected fields."
  [name-ns class-name & args]
  (let [classdata (apply hash-map args)
        prefix (str class-name "-")
        interfaces (get classdata :interfaces [])
        super-methods (:expose-methods classdata)
        exposed-fields (:expose-fields classdata)
        fullname (symbol (str (string/replace name-ns #"-" "_") "." (gen-classname class-name)))]
    `(do
       (gen-class
        :name ~fullname
        :prefix ~prefix
        :extends GuiContainer
        :exposes-methods ~super-methods
        :exposes ~exposed-fields
        :implements ~interfaces)
       (def ~class-name ~fullname)
       (import ~fullname))))
