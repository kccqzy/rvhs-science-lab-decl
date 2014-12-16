;;; -*- mode: lisp; -*-
"use strict"

;; A few shortcuts to save typing. This is not real macros because it
;; performs replacement only, doesn't run code, i.e. effectively
;; everything is inside a quasiquote. And the syntax is weird. And
;; since it doesn't support quoting or symbol-name or stringification,
;; the displayName will be attached at runtime...
(macro fn (args rest...)
       (function ~args ~rest...))
(macro defun (name args rest...)
       (var ~name (fn ~args ~rest...)))
(macro obj (rest...)
       (object ~rest...))
(macro defvar (name value) (var ~name ~value))

($
 (fn ()

   ;; Aliases to help minification and macros to save typing.
   (defvar React_createElement React.createElement)
   (defvar React_createClass React.createClass)
   (macro e (name attrs rest...)
          (React_createElement ~name (obj ~@attrs) ~rest...))
   (macro defcomponent (name rest...)
          (var ~name (React_createClass (_.defaults (obj ~rest...)
                                                    (obj render (fn () false))
                                                    (_.invert (obj ~name "displayName"))))))

   ;; An APIConnection is a wrapper around WebSocket.
   (defun APIConnection (pathname)
     (defvar wsUrl
       (+ (+ (if (= window.location.protocol "https:") "wss://" "ws://")
             window.location.host)
          pathname))
     (defvar conn null)
     (defvar callback null)
     (defvar timeConnected null)
     (defun connect ()
       (if (> (- (Date.now) timeConnected) 2000)
         (do
           (set conn (new WebSocket wsUrl))
           (set conn.onopen (fn () (set timeConnected (Date.now))))
           (set conn.onmessage callback)
           (set conn.onerror (fn ()
                               (console.log "WS connection errored; how?")
                               (connect)))
           (set conn.onclose (fn ()
                               (console.log "WS connection closed; why?"))))
         (setTimeout connect (- (Date.now) timeConnected))))
     (connect)
     (-> ($ window) (.on "beforeunload" (fn () (conn.close))))
     (obj
      registerCallback (fn (func)
                         (set conn.onmessage func)
                         (set callback func))
      readyState (fn ()
                   (if conn conn.readyState -1))
      close (fn () (conn.close))))

   ;; An EntityRow is a row of data in the table. Each row also has
   ;; two action buttons.
   (defcomponent EntityRow
     render
     (fn ()
       (defvar that this)
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar firstCell (if (&& (! (null? dataSpec.categoryColumn))
                                 this.props.firstRowSpan)
                           (e "td" (rowSpan this.props.firstRowSpan)
                             (get (get 0 dataSpec.categoryColumn) this.props.entity))
                           null))
       (e "tr" ()
         firstCell
         (_.map dataSpec.columns
                (fn (spec idx)
                  (e "td" (key idx) (get (get 0 spec) that.props.entity))))
         (e "td" (className "text-right")
           (e "div" (className "btn-group" role "group" "aria-label" "Action Buttons")
             (e "button" (type "button" className "btn btn-default btn-xs" "aria-label" "Edit" "data-action" "edit" "data-entityid" this.props.entity.id)
               (e "span" (className "glyphicon glyphicon-pencil" "aria-hidden" "true")))
             (e "button" (type "button" className "btn btn-default btn-xs" "aria-label" "Delete" "data-action" "delete" "data-entityid" this.props.entity.id)
               (e "span" (className "glyphicon glyphicon-trash" "aria-hidden" "true"))))))))

   ;; An EntityCategory is a group of data shared under a category,
   ;; presented as a tbody with a single cell spanning all of them.
   (defcomponent EntityCategory
     render
     (fn ()
       (e "tbody" ()
         (_.map this.props.entities
                (fn (entity i entities)
                  (e EntityRow
                      (key entity.id entity entity firstRowSpan (if i 0 entities.length))))))))

   ;; An EntityTable is the entire table for holding the data. It
   ;; receives events from the two action buttons on each row.
   (defcomponent EntityTable

     ;; Assume no data for initial state.
     getInitialState
     (fn () (obj tableData (obj data (array))))

     ;; When mounted, add event handlers.
     componentDidMount
     (fn ()
       (defvar that this)
       (this.props.conn.registerCallback
        (fn (e)
          (that.setState (obj tableData (JSON.parse e.data)))))
       (-> ($ (this.getDOMNode))
           (.on "click" "button[data-action=\"edit\"]"
                (fn ()
                  (defvar entityid (-> ($ this) (.data "entityid")))
                  (defvar entity (_.find that.state.tableData.data (fn (d) (= d.id entityid))))
                  (React.render
                   (e that.props.entityEditor (entity entity))
                   (getModalWrapper))))
           (.on "click" "button[data-action=\"delete\"]"
                (fn ()
                  (React.render
                   (e that.props.deleteConfirmation (entityid (-> ($ this) (.data "entityid"))))
                   (getModalWrapper))))))

     ;; Close connection when unmounted. This is paranoia because
     ;; currently there is no event to unmount them.
     componentWillUnmount
     (fn () (this.props.conn.close))

     ;; Now we need to group the data according to the requirements
     ;; outlined in the dataSpec (a.k.a. massage) and then render
     ;; them. The massaging process has type [Object] -> [[Object]].
     ;; [[Object]].
     render
     (fn ()
       (defvar dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (defvar rawData this.state.tableData.data)
       (defvar rows (if (null? dataSpec.categoryColumn)
                      (do ; no category column, one row per entry without
                                        ; using EntityCategory
                        (defvar sortName (get 0 (get 0 dataSpec.columns)))
                        (defvar massagedData (_.sortBy rawData sortName))
                        (e "tbody" ()
                          (_.map massagedData
                                 (fn (entity)
                                   (e EntityRow (firstRowSpan 1 entity entity key entity.id))))))
                      (do ; there is a category column, so group by that
                                        ; first
                        (defvar categoryName (get 0 dataSpec.categoryColumn))
                        (defvar sortName (get 0 (get 0 dataSpec.columns)))

                        ;; This is dense, so a reminder of the types:
                        ;; groupBy :: (Eq b, Show b) => [a] -> (a -> b) -> {[a]}
                        ;; map :: {v} -> (v -> String -> a) -> [a]
                        ;; sortBy :: [{a}] -> String -> [{a}]
                        (defvar massagedData
                          (_.map (_.sortBy (_.map (_.groupBy rawData categoryName)
                                                  (fn (v k) (obj k k v (_.sortBy v sortName))))
                                           "k")
                                 (fn (d) (.v d))))
                        (_.map massagedData
                               (fn (entities)
                                 (defvar category (.category (get 0 entities)))
                                 (e EntityCategory (entities entities key category)))))))

       (defvar headers (_.map (-> (if (null? dataSpec.categoryColumn)
                                    (array)
                                    (array (get 2 dataSpec.categoryColumn)))
                                  (.concat (_.map dataSpec.columns (fn (v) (get 2 v)))))
                              (fn (label idx) (e "th" (key idx) label))))
       (e "div" (className "table-responsive")
         (e "table" (className "table")
           (e "thead" ()
             (e "tr" ()
               headers
               (e "th" ())))
           rows))))

   ;; The Modal dialog that takes control of input and needs to be
   ;; dealt with before the rest of the page is functional.
   (defcomponent Modal
     ;; Modal rendering.
     render
     (fn ()
       (defvar header
         (e "div" (className "modal-header")
           (if this.props.canClose
             (e "button" (type "button" className "close" id "modalClose" "data-dismiss" "modal")
               (e "span" ("aria-hidden" "true") "×")
               (e "span" (className "sr-only") "Close"))
             "")
           (e "h4" (className "modal-title") this.props.title)))
       (defvar footer
         (if this.props.buttons
           (e "div" (className "modal-footer") this.props.buttons)
           ""))
       (e "div" (id "modal" className "modal fade")
         (e "div" (className "modal-dialog")
           (e "div" (className "modal-content")
             header
             (e "div" (className "modal-body") this.props.children)
             footer))))

     ;; When it is mounted or updated, it needs to be shown.
     componentDidMount (fn ()
                         (-> ($ (this.getDOMNode))
                             (.modal (obj keyboard false backdrop "static"))))
     componentDidUpdate (fn ()
                          (-> ($ (this.getDOMNode))
                              (.modal (obj keyboard false backdrop "static")))))

   ;; The target for rendering the Modal.
   (defun getModalWrapper () (-> ($ "#modal-wrapper") (.get 0)))

   ;; An action modal, with custom content, a Cancel button and an
   ;; action button.
   (defcomponent ActionModal
     render
     (fn ()
       (defvar buttons
         (e "div" (id "actionModalButtons")
           (e "button" (type "button" className "btn btn-default" "data-dismiss" "modal") "Cancel")
           (e "button" (type (|| this.props.actionButtonType "button") className (+ "btn btn-" this.props.actionButtonStyle) id "actionButton") this.props.actionButtonLabel)))
       (e Modal (canClose 1 title this.props.title buttons buttons)
         this.props.children))

     ;; Register click handler.
     componentDidMount
     (fn ()
       (defvar that this)
       (-> ($ "#actionButton")
           (.on "click" (fn (e)
                          (e.preventDefault)
                          (-> ($ "#actionModalButtons")
                              (.empty)
                              (.append "<img width=16 height=16 src=/static/res/loading.gif />"))
                          (-> ($ "#modalClose")
                              (.remove))
                          (that.props.next (fn () (-> ($ "#modal") (.modal "hide")))))))))

   ;; The CCA editor control.
   (defcomponent CcaEditor
     render
     (fn ()
       (defvar title (if this.props.entity "Edit CCA" "Add a new CCA"))
       (defvar actionButtonLabel (if this.props.entity "Edit" "Add"))
       (defvar endpoint (if this.props.entity (+ "/api/ccas/" this.props.entity.id) "/api/ccas"))
       (defvar method (if this.props.entity "PUT" "POST"))
       (defun next (hideModal)
         ($.ajax endpoint (obj type method
                               data (-> ($ "#ccaEditorForm") (.serialize))
                               complete hideModal)))
       (e "form" (id "ccaEditorForm" role "form")
         (e ActionModal (title title actionButtonLabel actionButtonLabel actionButtonStyle "primary" actionButtonType "submit" next next)
           (e "div" (className "form-group")
             (e "label" (htmlFor "name") "Name")
             (e "input" (type "text" className "form-control" name "name" placeholder "CCA Name" defaultValue (if this.props.entity this.props.entity.name ""))))
           (e "div" (className "form-group")
             (e "label" (htmlFor "category") "Category")
             (e "input" (type "text" className "form-control" name "category" placeholder "CCA Category" defaultValue (if this.props.entity this.props.entity.category ""))))))))

   ;; The Admin console homepage.
   (defcomponent AdminHomeR
     render
     (fn ()
       (e "div" (className "row")
         (e "div" (className "col-sm-11 col-md-8 col-lg-7")
           (e "h2" () "Welcome")
           (e "p" () "Welcome to the admin console for RVHS Science Lab Undertaking Project. XXX Be verbose.")
           (e "h2" () "Quick Guide")
           (e "p" () "TODO")
           (e "h2" () "API Documentation")
           (e "p" () "If you know some basics of programming, you can use it to add or remove things automatically via the HTTP JSON API.")))))

   ;; The admin console CCA page.
   (defcomponent AdminCcasR
     render
     (fn ()
       (defcomponent CcaDeleteConfirmation
         render
         (fn ()
           (defvar that this)
           (defun next (hideModal)
             ($.ajax (+ "/api/ccas/" that.props.entityid) (obj type "DELETE" complete hideModal)))
           (e ActionModal
               (title "Delete This CCA" actionButtonLabel "Delete It" actionButtonStyle "danger" next next)
             (e "p" () "Are you sure want to delete this CCA from the database?"))))
       (e "div" ()
         (e "div" (className "pull-right btn-group" role "toolbar" "aria-label" "Action Buttons")
           (e "button" (id "addButton" type "button" className "btn btn-default") "Add New")
           (e "button" (id "removeAllButton" type "button" className "btn btn-default") "Remove All"))
         (e "h2" () "All CCAs")
         (e EntityTable (conn (APIConnection "/api/ccas") entityEditor CcaEditor deleteConfirmation CcaDeleteConfirmation))))

     componentDidMount
     (fn ()
       (-> ($ "#addButton")
           (.on "click"
                (fn ()
                  (React.render
                   (e CcaEditor ())
                   (getModalWrapper)))))
       (-> ($ "#removeAllButton")
           (.on "click"
                (fn ()
                  (React.render
                   (e ActionModal
                       (title "Deleting All CCAs" actionButtonLabel "Yes, Delete All" actionButtonStyle "danger" next (fn (hideModal) ($.ajax "/api/ccas" (obj type "DELETE" complete hideModal))))
                     (e "p" () "Are you sure you want to delete all CCAs currently stored in the database? This will also delete all students’ CCA information."))
                   (getModalWrapper)))))
       ))

   (defcomponent AdminSubjectsR)

   (defcomponent AdminTeachersR)

   (defcomponent AdminStudentsR)

   (defvar pageSpec (obj "/admin" (obj pageName "Home"
                                       component AdminHomeR
                                       dataSpec null)
                         "/admin/ccas" (obj pageName "Manage CCAs"
                                            component AdminCcasR
                                            dataSpec (obj categoryColumn (array "category" _.identity "CCA Category")
                                                          columns (array (array "name" _.identity "CCA Name"))))
                         "/admin/subjects" (obj pageName "Manage Subjects"
                                                component AdminSubjectsR
                                                dataSpec (obj categoryColumn null
                                                              columns (array (array "code" _.identity "Subject Code")
                                                                             (array "level" _.identity "Applies To")
                                                                             (array "is_science" _.identity "Science Subject?")
                                                                             (array "name" _.identity "Subject Name"))))
                         "/admin/teachers" (obj pageName "Manage Teachers"
                                                component AdminTeachersR
                                                dataSpec null)
                         "/admin/students" (obj pageName "Manage Students"
                                                component AdminStudentsR
                                                dataSpec null)))

   (defcomponent Page
     render
     (fn ()
       (defvar pathname window.location.pathname)
       (defvar tabs
         (_.map pageSpec
                (fn (page route)
                  (e "li" (key route role "presentation" className (if (= route pathname) "active" ""))
                    (e "a" (href (if (= route pathname) "#" route))
                      (.pageName page))))))
       (e "div" (id "content-wrapper")
         (e "div" (id "modal-wrapper"))
         (e "div" (className "container")
           (e "div" (className "page-header")
             (e "h1" () "RVHS Science Lab Undertaking — For Teachers and Administrators"))
           (e "p" () "You are logged in as xxx.")
           (e "div" (role "tabpanel")
             (e "ul" (className "nav nav-tabs")
               tabs))
           (e "div" (id "main-content")))))
     componentDidMount
     (fn ()
       (defvar pathname window.location.pathname)
       (if (undefined? window.WebSocket)
         (React.render
          (e Modal (canClose 0 title "Browser Unsupported")
            (e "p" ()
              "Your browser is too old to use this website. This website requires at least Internet Explorer version 10, Apple Safari version 7, Google Chrome version 16, or Mozilla Firefox version 11. Regardless of which broswer you are using, it is always recommended that you use the latest version available."))
          (getModalWrapper))
         (React.render
          (e (.component (get pathname pageSpec)) ())
          (-> ($ "#main-content") (.get 0))))))

   (React.render (e Page ()) document.body)))


;;; Local variables:
;;; eval: (put 'fn 'lisp-indent-function 'defun)
;;; eval: (put 'e 'lisp-indent-function 2)
;;; eval: (put 'if 'lisp-indent-function 1)
;;; eval: (put 'do 'lisp-indent-function 0)
;;; eval: (add-hook 'after-save-hook (lambda () (shell-command "lispy admin.ls") (shell-command "/Library/Internet\\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java -jar ../../scratch/closure-compiler/compiler.jar --compilation_level SIMPLE_OPTIMIZATIONS --language_in ECMASCRIPT5_STRICT --js admin.js > admin.min.js")) nil t)
;;; End:
