#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.oxenfurt)

(defun %request (url parameters key id)
  (let ((drakma:*text-content-types* `(("application" . "json"))))
    (drakma:http-request url
                         :accept "application/json;charset=UTF-8"
                         :additional-headers `(("Accept-Charset" . "UTF-8")
                                               ("app_key" . ,key)
                                               ("app_id" . ,id))
                         :external-format-in :utf-8
                         :external-format-out :utf-8
                         :want-stream T
                         :decode-content T
                         :parameters parameters)))
