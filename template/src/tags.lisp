(in-package :template.tags)

(cl-interpol:enable-interpol-syntax)

(define-bknr-tag head (&key title)
  (html (:head
         (:title (:princ-safe title))
         ((:link :rel "shortcut icon" :href "/static/images/favicon.ico" :type "image/x-icon"))
         ((:link :type "text/css" :rel "stylesheet" :media "all" :href "/static/css/reset.css"))
         ((:link :type "text/css" :rel "stylesheet" :media "all" :href "/static/css/style.css"))
         (:princ "
<!--[if IE 6]>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/css/ie6.css\"/>
<![endif]-->
")

       ((:script :type "text/javascript" :src "/static/js/jquery.js") " ")
       ((:script :type "text/javascript" :src "/static/js/supersleight.plugin.js") " ")
       ((:script :type "text/javascript" :src "/static/js/template.js") " ")
         

                 )))

(define-bknr-tag header ()
  (html ((:div :id "headcontainer")
         ((:div :id "head")
          (:h1 (:span) "Odendahl Systemprogrammierung")
          ((:ul :id "nav")
           (dolist (elt '(("About" . "/index")
                          ("Services" . "/services")
                          ("Projects" . ("/projects" "/project"))
                          ;;("Blog" . "/blog")
                          ("Contact" . "/contact")))
             (cond
               ((if (listp (cdr elt))
                    (member (hunchentoot:script-name*) (cdr elt) :test #'string=)
                    (string= (hunchentoot:script-name*) (cdr elt)))
                (html ((:li :class "selected") (:princ-safe (car elt)))))
               (t
                (html (:li ((:a :href (if (listp (cdr elt))
                                          (first (cdr elt))
                                          (cdr elt)))
                            (:princ-safe (car elt)))))))))))))

(define-bknr-tag footer ()
  (html ((:div :id "footer")
         ((:div :id "twitterbox")
          ((:img :src "/static/images/twitter-bird.png" :alt "Twitter bird")) (:span " ")
          (:p "Follow me on " (:br) ((:a :href "http://twitter.com/wesen" :title "wesen on twitter") "twitter") "!"))
         ((:div :id "bookbox")
          ((:img :src "/static/images/arduino-book.png" :alt "Arduino Book"))
          (:p "Buy our book on " ((:a :href "http://www.amazon.de/Arduino-Physical-Computing-Designer-oreillys/dp/3897218933" :title "Arduino Buch auf Amazon") "amazon") "!"))
         ((:p :id "copyright") "(c) 2010 Manuel Odendahl" (:br)
          ((:a :href "/legal" :title "Legal information") "legal / impressum")))))

;; project lists and stuff

(define-bknr-tag project (&key id tag)
  (unless id
    (setf id (query-param "id")))
  (unless tag
    (setf tag (query-param "tag")))
  (let ((project (if id
                     (find-store-object id :class 'template::portfolio-project :query-function #'template::get-project-with-title)
                     (random-elts (store-objects-with-class 'template::portfolio-project) 1))))
    (with-slots (template::title template::tags template::time template::description template::image template::image)
        project
      (html ((:div :id "project")
             ((:img :alt template::title :src (format nil "/image/~a" (store-object-id template::image))))
             ((:div :class "project-title")
              (:h2 (:princ-safe (string-upcase template::title))))
             ((:div :class "project-date") (:princ-safe (cybertiggyr-time:format-time nil "%Y" template::time)))
             ((:div :class "project-description")
              (:p (:princ template::description)))
             ((:div :class "project-tags")
              (:princ-safe "Tags: ")
              (dolist (tag template::tags)
                (html ((:a :href (format nil "/projects?tag=~a" (string-downcase (symbol-name tag))) :title (format nil "~a projects" tag))
                       (:princ-safe (string-downcase (symbol-name tag)))) " ")))

            (when (bknr.web::admin-p (bknr-session-user))
              (html (:hr) ((:a :href (format nil "/edit-project?id=~a" (template::portfolio-project-title project)))
                     "edit project")))
             )             
            (when tag
              (let* ((projects (template::get-projects-with-tag-sorted tag))
                     (prev-project (template::find-before projects project))
                     (next-project (template::find-after projects project)))
                (when prev-project
                  (html ((:a :href (format nil "/project?id=~a&tag=~a" (template::portfolio-project-title prev-project) tag)
                             :title (format nil "previous ~a project" tag) :id "prevlink")
                         (:princ-safe (format nil "previous ~a project" tag)))))
                (when next-project
                  (html ((:a :href (format nil "/project?id=~a&tag=~a" (template::portfolio-project-title next-project) tag)
                             :title (format nil "next ~a project" tag) :id "nextlink")
                         (:princ-safe (format nil "next ~a project" tag)))))))
            

             ))))

(define-bknr-tag project-box (&key id tag)
  (let ((project (if id
                     (find-store-object id :class 'template::portfolio-project :query-function #'template::get-project-with-title)
                     (random-elts (store-objects-with-class 'template::portfolio-project) 1))))
    (html ((:li :class "projectbox")
           ((:a :href (format nil "/project?id=~a&tag=~a" (template::portfolio-project-title project) (string-downcase tag))
                :title (template::portfolio-project-title project))
            ((:img :src (format nil "/image/~a" (store-object-id (template::portfolio-project-box-image project)))
                   :alt (template::portfolio-project-title project)))
            ((:span :class "legend")
             (:princ-safe (string-upcase (template::portfolio-project-title project)))))))))

(define-bknr-tag project-list (&key count tag random)
  (unless tag
    (setf tag (query-param "tag")))
  (setf count (when count (parse-integer count))
        tag (when tag (make-keyword-from-string tag)))
  (let ((projects
         (sort (copy-tree (if tag
                              (template::get-projects-with-tag tag)
                              (store-objects-with-class 'template::portfolio-project)))
               #'> :key #'template::portfolio-project-time)))
    (when random
      (setf projects (randomize-list projects)))
    (when count
      (setf projects (subseq projects 0 (min (length projects) count))))
    (html ((:ul :class "projectlist")
           (dolist (project projects)
             (project-box :id (template::portfolio-project-title project) :tag tag))))))

(define-bknr-tag projects (&key tag count random)
  (unless tag
    (setf tag (query-param "tag")))
  (unless count
    (setf count (query-param "count")))
  (unless tag
    (setf random "true" count "8"))
  (html ((:div :id "projects")
         (:h2 (:princ-safe (format nil "~@(~A~)" (or tag "random"))) " projects")
         (project-list :tag tag :count count :random random))))

(define-bknr-tag category-list ()
  (html ((:ul :id "categories")
         (dolist (category (template::project-tag-sorted-list))
           (let ((name (string-downcase (car category))))
             (html (:li ((:span :class "category")
                         ((:a :href (format nil "/projects?tag=~a" name)
                              :title (format nil "~a projects" name))
                          (:princ-safe name)))
                        ((:span :class "category-count")
                         (:princ-safe (cdr category))))))))))

;; blog lists and stuff

(define-bknr-tag blog-box (&key id)
  )

(define-bknr-tag blog-list (&key count start)
  )

(define-bknr-tag blog-tag-list ()
  )

;; add-project form

(defvar *add-project-form-errors* nil)

(define-bknr-tag required ()
  (html (:em ((:img :src "/static/images/required.png"
                    :alt "required")))))

(define-bknr-tag edit-project-form (&key id)
  (unless id
    (setf id (query-param "id")))
  (let ((project (when id (find-store-object id :class 'template::portfolio-project
                                             :query-function #'template::get-project-with-title))))
    (format t "edit-project-form ~A~%" project)
    (html
     ((:form :action "/edit-project" :method "post" :enctype "multipart/form-data")
      (if project
          (html ((:input :type "hidden" :name "project-id" :value (store-object-id project)))
                ((:input :type "hidden" :name "action-type" :value "edit")))
          (html ((:input :type "hidden" :name "action-type" :value "add"))))
      ((:div :class "left1col")
       ((:div :class "contactinfo")
        ((:div :class "formelt")
         ((:label :for "title")
          "Title: " (required))
         ((:input :type "text" :size "30" :id "title" :name "title"
                  :value (if project
                             (template::portfolio-project-title project)
                             ""))))
        ((:div :class "formelt")
         ((:label :for "tags")
          "Tags (space-separated): " (required))
         ((:input :type "text" :size "30" :id "tags" :name "tags"
                  :value (if project
                             (format nil "~{~A~^ ~}" (mapcar #'(lambda (x)
                                                                 (string-downcase (symbol-name x)))
                                                             (template::portfolio-project-tags project)))
                             ""))))
        ((:div :class "formelt")
         ((:label :for "time")
          "Time (YYYY-MM-DD): ")
         ((:input :type "text" :size "30" :id "time" :name "time"
                  :value (if project
                             (cybertiggyr-time:format-time nil "%Y-%m-%d"
                                                           (template::portfolio-project-time project))
                             ""))))
        ((:div :class "formelt")
         ((:label :for "file")
          "File: " (unless project (required)))
         ((:input :type "file" :size "30" :id "file" :name "file")))
        ((:div :class "formelt")
         ((:label :for "boxfile")
          "Box File: " (unless project (required)))
         ((:input :type "file" :size "30" :id "boxfile" :name "boxfile")))))
      ((:div :class "right2col")
       ((:div :class "contactmessage")
        ((:textarea :id "description" :name "description" :cols "60" :rows "20")
         (if project
             (html (:princ-safe (template::portfolio-project-description project)))
             (html (:princ-safe "  "))))
        (add-project-form-errors)
        ((:input :type "submit" :id "submit" :value (if project
                                                        "Edit the project!"
                                                        "Add the project!")
                 :name "submit"))))))))

(define-bknr-tag add-project-form ()
  (with-query-params (submit action-type project-id)
    (format t "submit ~a action-type ~a project-id ~A~%" submit action-type project-id)
    (if submit
        (if (and action-type (string= action-type "edit"))
            (handle-edit-project)
            (handle-add-project))
        (emit-tag-child 0))))

(defun handle-edit-project ()
  (with-query-params (title tags time file boxfile description project-id)
    (let ((project (find-store-object project-id :class 'template::portfolio-project
                                      :query-function #'template::get-project-with-title)))
      (when time
        (setf time (cybertiggyr-time:parse-time time)))
      (when (null project)
        (let ((*add-project-form-errors* (format nil "Could not find project with id ~A" project-id)))
          (emit-tag-child 0)
          (return-from handle-edit-project)))
      (when (or (null title) (null tags) (null description))
        (let ((*add-project-form-errors* "Please fill out the required fields!"))
          (emit-tag-child 0))
        (return-from handle-edit-project))
      (with-transaction ()
        (with-slots (template::title template::tags template::time template::description) project
          (setf template::title title
                template::tags (template::make-tags-from-string tags)
                template::time time
                template::description description)))
      (when boxfile
        (let ((boxfileobj (import-image (first boxfile) :name (second boxfile)
                                        :type (image-type-symbol (third boxfile)))))
          (with-transaction ()
            (with-slots (template::box-image) project
              (setf template::box-image boxfileobj)))))
      (when file
        (let ((fileobj (import-image (first file) :name (second file)
                                        :type (image-type-symbol (third file)))))
          (with-transaction ()
            (with-slots (template::image) project
              (setf template::image fileobj)))))
      (html ((:div :class "message")
             (:p "Project has been updated!"))
            (project :id (store-object-id project))))))
      
    

(defun handle-add-project ()
  (with-query-params (title tags time file boxfile description)
    (if (or (null title) (null tags) (null file) (null description) (null boxfile))
        (let ((*add-project-form-errors* "Please fill out the required fields!"))
          (emit-tag-child 0))
        (progn
          (let* ((taglist (template::make-tags-from-string tags))
                 (fileobj (import-image (first file) :name (second file) :type (image-type-symbol (third file))))
                 (boxfileobj (import-image (first boxfile) :name (second boxfile) :type (image-type-symbol (third file))))
                 (project (make-instance 'template::portfolio-project
                                         :title title
                                         :tags taglist
                                         :description description
                                         :image fileobj
                                         :box-image boxfileobj
                                         :time (if time (cybertiggyr-time:parse-time time) (get-universal-time)))))
            (format t "project ~A~%" project)
            (html ((:div :class "message") 
                   (:p "project has been added"))
                  (project :id (store-object-id project))))))))

(define-bknr-tag add-project-form-errors ()
  (when *add-project-form-errors*
    (html ((:p :class "error") (:princ-safe *add-project-form-errors*)))))

;; handle the contact form

(defvar *contact-form-errors* nil)

(define-bknr-tag contact-form ()
  (with-query-params (name email website message submit)
    (if submit
        (if (or (null name) (null email) (null message))
            (let ((*contact-form-errors* "Please fill out the required fields!"))
              (emit-tag-child 0))
            (progn
              (cl-smtp:send-email "localhost" email "manuel"
                                  (format nil "Portfolio: Message from ~A <~A> (~A)" name email website)
                                  message)
              (html ((:div :class "message") 
                     (:p "Thank you for your message! I will try to respond as fast as possible.")))))
        (emit-tag-child 0))))

(define-bknr-tag contact-form-errors ()
  (when *contact-form-errors*
    (html ((:p :class "error") (:princ-safe *contact-form-errors*)))))
  

;; login and admin page tags
(define-bknr-tag login-page ()
  (if (and (hunchentoot:session-value :login-redirect-uri)
           (not (bknr.user:anonymous-p (bknr-session-user))))
      (redirect (puri:render-uri (hunchentoot:session-value :login-redirect-uri) nil))
      (emit-tag-children)))


(define-bknr-tag admin-page ()
  (if (bknr.web::admin-p (bknr-session-user))
      (emit-tag-children)
      (progn
        (setf (hunchentoot:session-value :login-redirect-uri)
              (parse-uri (hunchentoot:script-name*)))
        (redirect "/login"))))