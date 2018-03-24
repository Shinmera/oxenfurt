#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem oxenfurt-dexador
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Dexador client backend for Oxenfurt."
  :homepage "https://github.com/Shinmera/oxenfurt"
  :serial T
  :components ((:file "dexador"))
  :depends-on (:dexador
               :oxenfurt-core))
