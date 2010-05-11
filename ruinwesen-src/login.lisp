(in-package :ruinwesen)

(defclass admin-handler (admin-only-handler page-handler)
  ())

(defmethod handle ((handler admin-handler))
  (with-bknr-page ()
    "Please choose an administration activity from the menu above"))

(defclass login-handler (form-handler)
  ())

(defmethod handle-form ((handler login-handler) action)
  (if (and (eq action :login)
           (not (anonymous-p (bknr-session-user))))
      (redirect "/admin")
      (with-http-response (:content-type "text/html; charset=UTF-8")
        (with-http-body ()
          (html
           (:html
            (:head
             (:title "Please log in to the BKNR website CMS")
             ((:link :rel "stylesheet" :href "/static/styles.css")))
            ((:body :onload "var input = document.forms[0].elements[0]; input.focus(); input.select();")
             ((:img :src "/image/bknr-logo" :width "352" :height "124"))
             (:p "Please log in to the BKNR website CMS")
             ((:form :method "post")
              (:table
               (:tr (:td "Username")
                    (:td ((:input :name "__username"))))
               (:tr (:td "Password")
                    (:td ((:input :name "__password" :type "password"))))
               (when (eq action :login)
                 (html
                  (:tr (:td)
                       ((:td :class "login-failed")
                        "Login failed"))))
               (:tr ((:td :colspan "2")
                     ((:input :type "submit" :name "action" :value "login")))))))))))))

