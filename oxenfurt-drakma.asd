#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem oxenfurt-drakma
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Drakma client backend for Oxenfurt."
  :homepage "https://Shinmera.github.io/oxenfurt/"
  :bug-tracker "https://github.com/Shinmera/oxenfurt/issues"
  :source-control (:git "https://github.com/Shinmera/oxenfurt.git")
  :serial T
  :components ((:file "drakma"))
  :depends-on (:drakma
               :oxenfurt-core))
