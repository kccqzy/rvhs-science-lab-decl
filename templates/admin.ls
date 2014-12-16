;;; -*- mode: lisp; -*-
"use strict"

;; A few shortcuts to save typing. This is not real macros because it
;; performs replacement only, doesn't run code, i.e. effectively
;; everything is inside a quasiquote. And the syntax is weird. And
;; since it doesn't support quoting or symbol-name or stringification,
;; the displayName will be attached at runtime...
(macro fn (args rest...)
       (function ~args ~rest...))
(macro defn (name args rest...)
       (var ~name (fn ~args ~rest...)))
(macro obj (rest...)
       (object ~rest...))
(macro e (name attrs rest...)
       (React.createElement ~name (obj ~@attrs) ~rest...))
(macro defcomponent (name rest...)
       (var ~name (React.createClass (_.defaults (obj ~rest...) (obj render (fn () false)) (_.invert (obj ~name "displayName"))))))

($
 (fn ()

   ;; An APIConnection is a wrapper around WebSocket.
   (defn APIConnection (pathname)
     (var wsUrl
          (+ (+ (if (= window.location.protocol "https:") "wss://" "ws://")
                window.location.host)
             pathname)
          connAttempts 0
          conn null
          callback null)
     (defn retry ()
       (when (< ++connAttempts 10)
         (set conn (new WebSocket wsUrl))
         (set conn.onmessage callback)
         (set conn.onerror (fn ()
                             (console.log "WS connection errored; how?")
                             (retry)))
         (set conn.onclose (fn ()
                             (console.log "WS connection closed; why?")))))
     (retry)
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
       (var that this)
       (var dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (var firstCell (if (&& (! (null? dataSpec.categoryColumn))
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
   ;; presented as a rowspan.
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
     getInitialState
     (fn () (obj tableData (obj data (array))))
     componentDidMount
     (fn ()
       (var that this)
       (this.props.conn.registerCallback
        (fn (e)
          (that.setState (obj tableData (JSON.parse e.data)))))
       (-> ($ (this.getDOMNode))
           (.on "click" "button[data-action=\"edit\"]"
                (fn ()
                  (var entityid (-> ($ this) (.data "entityid")))
                  (var entity (_.find that.state.tableData.data (fn (d) (= d.id entityid))))
                  (React.render
                   (e that.props.entityEditor (editing entity))
                   (getModalWrapper))))
           (.on "click" "button[data-action=\"delete\"]"
                (fn ()
                  (React.render
                   (e that.props.deleteConfirmation (deleting (-> ($ this) (.data "entityid"))))
                   (getModalWrapper))))))
     componentWillUnmount
     (fn () (this.props.conn.close))
     render
     (fn ()
       ;; Now we need to group the data according to the requirements
       ;; outlined in the dataSpec. The process has type [Object] ->
       ;; [[Object]].
       (var dataSpec (.dataSpec (get window.location.pathname pageSpec)))
       (var rawData this.state.tableData.data)
       (var rows (if (null? dataSpec.categoryColumn)
                   (do ; no category column, one row per entry without using EntityCategory
                       (var sortName (get 0 (get 0 dataSpec.columns)))
                       (var massagedData (_.sortBy rawData sortName))
                     (e "tbody" ()
                       (_.map massagedData
                              (fn (entity)
                                (e EntityRow (firstRowSpan 1 entity entity key entity.id))))))
                   (do ; there is a category column, so group by that first
                       (var categoryName (get 0 dataSpec.categoryColumn))
                       (var sortName (get 0 (get 0 dataSpec.columns)))
                     ;; groupBy :: (Eq b, Show b) => [a] -> (a -> b) -> {[a]}
                     ;; map :: {v} -> (v -> String -> a) -> [a]
                     ;; sortBy :: [{a}] -> String -> [{a}]
                     (var massagedData (_.map (_.sortBy (_.map (_.groupBy rawData categoryName)
                                                               (fn (v k) (obj k k v (_.sortBy v sortName))))
                                                        "k")
                                              (fn (d) (.v d))))
                     (_.map massagedData
                            (fn (entities)
                              (e EntityCategory (entities entities key (.category (get 0 entities)))))))))

       (var headers (_.map (-> (if (null? dataSpec.categoryColumn) (array) (array (get 2 dataSpec.categoryColumn)))
                               (.concat (_.map dataSpec.columns (fn (v) (get 2 v)))))
                           (fn (label idx) (e "th" (key idx) label))))
       (e "div" (className "table-responsive")
         (e "table" (className "table")
           (e "thead" ()
             (e "tr" ()
               headers
               (e "th" ())))
           rows))))

   (defcomponent Modal
     render
     (fn ()
       (var header
            (e "div" (className "modal-header")
              (if this.props.canClose
                (e "button" (type "button" className "close" "data-dismiss" "modal")
                  (e "span" ("aria-hidden" "true") "×")
                  (e "span" (className "sr-only") "Close"))
                "")
              (e "h4" (className "modal-title") this.props.title)))
       (var footer
            (if this.props.buttons
              (e "div" (className "modal-footer") this.props.buttons)
              ""))
       (e "div" (id "modal" className "modal fade")
         (e "div" (className "modal-dialog")
           (e "div" (className "modal-content")
             header
             (e "div" (className "modal-body") this.props.children)
             footer))))
     componentDidMount
     (fn ()
       (-> ($ (this.getDOMNode))
           (.modal (obj keyboard false backdrop "static"))))
     componentDidUpdate
     (fn ()
       (-> ($ (this.getDOMNode))
           (.modal (obj keyboard false backdrop "static")))))

   (defn getModalWrapper () (-> ($ "#modal-wrapper") (.get 0)))

   (defcomponent ActionModal
     render
     (fn ()
       (e Modal (canClose 1 title this.props.title buttons (e "div" ()
                                                             (e "button" (type "button" className "btn btn-default" "data-dismiss" "modal") "Cancel")
                                                             (e "button" (type (|| this.props.actionButtonType "button") className (+ "btn btn-" this.props.actionButtonStyle) id "actionButton") this.props.actionButtonLabel)))
         this.props.children))
     componentDidMount
     (fn ()
       (var that this)
       (-> ($ "#actionButton")
           (.on "click" (fn (e)
                          (e.preventDefault)
                          (that.props.next (fn () (-> ($ "#modal") (.modal "hide")))))))))

   (defcomponent CcaEditor
     render
     (fn ()
       (var title (if this.props.editing "Edit CCA" "Add a new CCA"))
       (var actionButtonLabel (if this.props.editing "Edit" "Add"))
       (var endpoint (if this.props.editing (+ "/api/ccas/" this.props.editing.id) "/api/ccas"))
       (var method (if this.props.editing "PUT" "POST"))
       (e "form" (id "ccaEditorForm" role "form")
         (e ActionModal (title title actionButtonLabel actionButtonLabel actionButtonStyle "primary" actionButtonType "submit"
                               next (fn (hideModal)
                                      ($.ajax endpoint (obj type method
                                                               data (-> ($ "#ccaEditorForm") (.serialize))
                                                               complete hideModal))))
           (e "div" (className "form-group")
             (e "label" (htmlFor "name") "Name")
             (e "input" (type "text" className "form-control" id "name" name "name" placeholder "CCA Name" defaultValue (if this.props.editing this.props.editing.name ""))))
           (e "div" (className "form-group")
             (e "label" (htmlFor "category") "Category")
             (e "input" (type "text" className "form-control" id "category" name "category" placeholder "CCA Category" defaultValue (if this.props.editing this.props.editing.category ""))))))))

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

   (defcomponent AdminCcasR
     render
     (fn ()
       (defcomponent CcaDeleteConfirmation
         render
         (fn ()
           (var that this)
           (e ActionModal
             (title "Delete This CCA" actionButtonLabel "Delete It" actionButtonStyle "danger"
              next (fn (hideModal) ($.ajax (+ "/api/ccas/" that.props.deleting) (obj type "DELETE" complete hideModal))))
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
           (.on "click" (fn ()
                          (React.render
                           (e CcaEditor ())
                           (getModalWrapper)))))
       (-> ($ "#removeAllButton")
           (.on "click" (fn ()
                          (React.render
                           (e ActionModal
                             (title "Deleting All CCAs" actionButtonLabel "Yes, Delete All" actionButtonStyle "danger" next (fn (hideModal) ($.ajax "/api/ccas" (obj type "DELETE" complete hideModal))))
                             (e "p" () "Are you sure you want to delete all CCAs currently stored in the database? This will also delete all students’ CCA information."))
                           (getModalWrapper)))))
       ))

   (defcomponent AdminSubjectsR)

   (defcomponent AdminTeachersR)

   (defcomponent AdminStudentsR)

   (var pageSpec (obj "/admin" (obj pageName "Home"
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
       (var pathname window.location.pathname)
       (var tabs (_.map pageSpec (fn (page route)
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
       (var pathname window.location.pathname)
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
;;; eval: (add-hook 'after-save-hook (lambda () (shell-command "lispy admin.ls")))
;;; End:
