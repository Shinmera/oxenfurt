#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem oxenfurt
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A client for the Oxford dictionary API."
  :homepage "https://github.com/Shinmera/oxenfurt"
  :serial T
  :components ((:file "package")
               (:file "api")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :alexandria
               :drakma
               :yason))
