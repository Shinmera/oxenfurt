#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem oxenfurt-drakma
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Drakma client backend for Oxenfurt."
  :homepage "https://github.com/Shinmera/oxenfurt"
  :serial T
  :components ((:file "drakma"))
  :depends-on (:drakma
               :oxenfurt-core))
